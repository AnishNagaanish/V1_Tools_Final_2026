
"""
Model exported as python (patched).
Name : model
With QGIS : 33414
"""

from qgis.core import (
    QgsProcessing,
    QgsProcessingAlgorithm,
    QgsProcessingMultiStepFeedback,
    QgsProcessingParameterVectorLayer,
    QgsProcessingParameterFeatureSink,
    QgsProcessingParameterField,
    QgsProcessingParameterString,
    QgsProcessingParameterCrs,
    QgsVectorLayerSimpleLabeling,
    QgsPalLayerSettings,
    QgsProject,
    QgsWkbTypes,
    QgsGeometry
)
import processing
import os


class Model(QgsProcessingAlgorithm):

    def initAlgorithm(self, config=None):
        # Input layer
        self.addParameter(
            QgsProcessingParameterVectorLayer(
                'hexatronics_addresses',
                'Hexatronics Addresses',
                defaultValue=None
            )
        )

        # Optional filter
        self.addParameter(
            QgsProcessingParameterField(
                'project_filter_field',
                'Filter Field (optional)',
                parentLayerParameterName='hexatronics_addresses',
                type=QgsProcessingParameterField.String,
                optional=True
            )
        )
        self.addParameter(
            QgsProcessingParameterString(
                'project_filter_value',
                'Filter Value (optional)',
                defaultValue=None,
                optional=True
            )
        )

        # Optional CRS selection
        self.addParameter(
            QgsProcessingParameterCrs(
                'target_crs',
                'Target CRS (optional)',
                optional=True
            )
        )

        # Vector sink (optional). If left blank, model uses TEMPORARY_OUTPUT.
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                'Cd_details',
                'CD_Details (optional)',
                type=QgsProcessing.TypeVectorAnyGeometry,
                createByDefault=True,
                supportsAppend=True,
                defaultValue=None
            )
        )

    def _resolve_project_dir(self, parameters, context):
        """Destination folder:
        1) project directory; 2) input layer directory; 3) user home
        """
        proj_path = QgsProject.instance().fileName()
        if proj_path:
            return os.path.dirname(proj_path)

        in_layer = self.parameterAsVectorLayer(parameters, 'hexatronics_addresses', context)
        if in_layer:
            src = in_layer.dataProvider().dataSourceUri()
            if src:
                src_file = src.split('|')[0]
                if os.path.isabs(src_file) and os.path.exists(src_file):
                    return os.path.dirname(src_file)

        return os.path.expanduser('~')

    def _point_for_insertion(self, geom: QgsGeometry) -> QgsGeometry:
        gt = QgsWkbTypes.geometryType(geom.wkbType())
        if gt == QgsWkbTypes.PointGeometry:
            return geom
        return geom.centroid()

    def processAlgorithm(self, parameters, context, model_feedback):
        feedback = QgsProcessingMultiStepFeedback(6, model_feedback)
        results = {}
        outputs = {}

        # Step 0: Fix geometries
        alg_params = {
            'INPUT': parameters['hexatronics_addresses'],
            'METHOD': 1,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['FixGeometries'] = processing.run(
            'native:fixgeometries', alg_params,
            context=context, feedback=feedback, is_child_algorithm=True
        )

        feedback.setCurrentStep(1)
        if feedback.isCanceled():
            return {}

        # Step 1: Optional filter (now hardened)
        source_for_next = outputs['FixGeometries']['OUTPUT']
        in_layer = self.parameterAsVectorLayer(parameters, 'hexatronics_addresses', context)

        fld = parameters.get('project_filter_field', None)
        val = parameters.get('project_filter_value', None)

        def _field_exists(layer, field_name):
            if not layer or not field_name:
                return False
            return field_name in [f.name() for f in layer.fields()]

        if fld and val is not None and str(val).strip() != '' and _field_exists(in_layer, fld):
            alg_params = {
                'FIELD': fld,
                'INPUT': source_for_next,
                'OPERATOR': 0,  # equals
                'VALUE': val,
                'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT  # ✅ FIX: provide OUTPUT
            }
            outputs['ExtractByAttribute'] = processing.run(
                'native:extractbyattribute', alg_params,
                context=context, feedback=feedback, is_child_algorithm=True
            )
            source_for_next = outputs['ExtractByAttribute']['OUTPUT']
        else:
            # If field missing / empty value, skip filter gracefully
            feedback.pushInfo('Filter step skipped (field missing or value empty).')

        feedback.setCurrentStep(2)
        if feedback.isCanceled():
            return {}

        # Step 2: Optional reprojection
        target_crs = parameters.get('target_crs', None)
        if target_crs:
            alg_params = {
                'INPUT': source_for_next,
                'TARGET_CRS': target_crs,
                'OPERATION': 0,
                'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
            }
            outputs['ReprojectLayer'] = processing.run(
                'native:reprojectlayer', alg_params,
                context=context, feedback=feedback, is_child_algorithm=True
            )
            source_for_next = outputs['ReprojectLayer']['OUTPUT']

        feedback.setCurrentStep(3)
        if feedback.isCanceled():
            return {}

        # Step 3: Refactor fields to produce CD_Details (emit literal \P breaks)
        fields_mapping = [
            {'alias': '', 'comment': '', 'expression': '"T_N"', 'length': 254, 'name': 'T_N', 'precision': 0, 'sub_type': 0, 'type': 10, 'type_name': 'text'},
            {'alias': '', 'comment': '', 'expression': '"address_nu"', 'length': 254, 'name': 'address_nu', 'precision': 0, 'sub_type': 0, 'type': 10, 'type_name': 'text'},

            # '\\\\P' -> '\P' inside the expression string (AutoCAD paragraph break)
            {'alias': '',
             'comment': '',
             'expression': 'concat("address_nu", \'\\\\P\', \'ROUTE#: \', regexp_replace(regexp_substr("T_N", \'^R\\\\d+\'), \'^R\', \'\'), \'\\\\P\', \'DUCT#: \', regexp_substr("T_N", \'-(\\\\d+)$\'))',
             'length': 254,
             'name': 'CD_Details',
             'precision': 0,
             'sub_type': 0,
             'type': 10,
             'type_name': 'text'}
        ]

        sink_param = parameters.get('Cd_details', None)
        refactor_out = sink_param if sink_param else QgsProcessing.TEMPORARY_OUTPUT

        alg_params = {
            'FIELDS_MAPPING': fields_mapping,
            'INPUT': source_for_next,
            'OUTPUT': refactor_out
        }
        outputs['RefactorFields'] = processing.run(
            'native:refactorfields', alg_params,
            context=context, feedback=feedback, is_child_algorithm=True
        )

        out_id = outputs['RefactorFields']['OUTPUT']
        out_layer = context.getMapLayer(out_id)
        if out_layer is not None:
            out_layer.setName('CD_Details')
            try:
                out_layer.setTitle('CD_Details')
            except Exception:
                pass
            try:
                pal = QgsPalLayerSettings()
                pal.enabled = True
                pal.fieldName = 'CD_Details'
                pal.placement = QgsPalLayerSettings.OverPoint
                labeling = QgsVectorLayerSimpleLabeling(pal)
                out_layer.setLabeling(labeling)
                out_layer.setLabelsEnabled(True)
                out_layer.triggerRepaint()
            except Exception:
                pass

        results['Cd_details'] = outputs['RefactorFields']['OUTPUT']

        feedback.setCurrentStep(4)
        if feedback.isCanceled():
            return results

        # Step 4: Prepare final CSV fields (keep geometry for X,Y)
        alg_params = {
            'FIELDS_MAPPING': [
                {'alias': '', 'comment': '', 'expression': '"CD_Details"', 'length': 254,
                 'name': 'CD_Details', 'precision': 0, 'sub_type': 0, 'type': 10, 'type_name': 'text'},

                {'alias': '', 'comment': '',
                 'expression': "x( centroid( transform( $geometry, @layer_crs, 'EPSG:4326' ) ) )",
                 'length': 20, 'name': 'Longitude_4326', 'precision': 10,
                 'sub_type': 0, 'type': 6, 'type_name': 'double precision'},

                {'alias': '', 'comment': '',
                 'expression': "y( centroid( transform( $geometry, @layer_crs, 'EPSG:4326' ) ) )",
                 'length': 20, 'name': 'Latitude_4326', 'precision': 10,
                 'sub_type': 0, 'type': 6, 'type_name': 'double precision'}
            ],
            'INPUT': out_layer,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }
        outputs['PrepareForCSV'] = processing.run(
            'native:refactorfields', alg_params,
            context=context, feedback=feedback, is_child_algorithm=True
        )
        layer_for_csv = outputs['PrepareForCSV']['OUTPUT']

        # Paths next to project
        out_dir = self._resolve_project_dir(parameters, context)
        csv_path = os.path.join(out_dir, 'CD_Details.csv')
        scr_path = os.path.join(out_dir, 'CD_Details.scr')

        # Save CSV with GDAL layer options: AS_XY (writes X,Y), COMMA separator
        save_params = {
            'INPUT': layer_for_csv,
            'OUTPUT': csv_path,
            'LAYER_NAME': 'CD_Details',
            'DATASOURCE_OPTIONS': '',
            'LAYER_OPTIONS': 'GEOMETRY=AS_XY;SEPARATOR=COMMA',
            'ACTION_ON_EXISTING_FILE': 0
        }
        outputs['SaveCSV'] = processing.run(
            'native:savefeatures', save_params,
            context=context, feedback=feedback, is_child_algorithm=True
        )

        feedback.setCurrentStep(5)
        if feedback.isCanceled():
            results['csv_output'] = csv_path
            return results

        # Step 5: Generate AutoCAD .scr script (robust preamble + -MTEXT placement)
        csv_layer = context.getMapLayer(layer_for_csv)
        if csv_layer is not None:
            try:
                with open(scr_path, 'w', encoding='utf-8') as fh:
                    fh.write("_.UCS _World \n")
                    fh.write("_.SETVAR OSMODE 0 \n")
                    fh.write("_.-LAYER _Make CD_Details \n")

                    for f in csv_layer.getFeatures():
                        txt = f['CD_Details'] if 'CD_Details' in f.fields().names() else ''
                        if txt is None:
                            txt = ''
                        txt_mtext = str(txt).replace('\r\n', '\n').replace('\r', '\n').replace('\n', '\\P')

                        geom = f.geometry()
                        ins_pt = self._point_for_insertion(geom)
                        pt = ins_pt.asPoint()
                        x = pt.x(); y = pt.y()

                        fh.write("_.-MTEXT \n")
                        fh.write(f"{x},{y} \n")
                        fh.write("_W \n")
                        fh.write("150.0 \n")
                        fh.write(f"{txt_mtext} \n")
                        fh.write("\n")

                    fh.write("_.ZOOM _E \n")
                    fh.write("\n")
                feedback.pushInfo(f"AutoCAD script written to: {scr_path}")
            except Exception as e:
                feedback.reportError(f"Could not write SCR file: {e}")

        feedback.pushInfo(f'CSV written to: {csv_path}')
        results['csv_output'] = csv_path
        results['scr_output'] = scr_path
        return results

    def name(self):
        return 'model'

    def displayName(self):
        return 'model'

    def group(self):
        return ''

    def groupId(self):
        return ''

    def createInstance(self):
        return Model()
