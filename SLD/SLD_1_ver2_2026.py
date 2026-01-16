"""
================================================================================
SLD CREATION WIZARD - PRODUCTION VERSION (FIXED)
================================================================================
Complete tool for creating Single Line Diagrams from Hexatronics data

Features:
- Part 1: Create Equipment, Fiber_Cable, Grid layers from source data
- Part 2: Generate Tree Layout SLD diagram with PROPER BUS-BASED ROUTING
- Style file browser options for BOTH tools + default styles
- Daisy Chain calculation
- Professional styling and labeling
- No overlaps in tree layout

Run in QGIS Python Console
================================================================================
"""

from qgis.PyQt import QtWidgets, QtGui
from qgis.PyQt.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QProgressBar, QGroupBox, QTextEdit, QCheckBox, QMessageBox, 
    QApplication, QFileDialog, QComboBox, QSpinBox, QDoubleSpinBox,
    QTabWidget, QWidget, QGridLayout, QLineEdit
)
from qgis.PyQt.QtCore import Qt, QVariant
from qgis.core import (
    QgsProject, QgsProcessing, QgsProcessingFeedback,
    QgsProcessingContext, QgsVectorLayer, QgsFeatureRequest,
    QgsField, QgsFeature, QgsGeometry, QgsPointXY,
    QgsWkbTypes, QgsSymbol, QgsSimpleLineSymbolLayer,
    QgsTextFormat, QgsPalLayerSettings, QgsVectorLayerSimpleLabeling,
    QgsSingleSymbolRenderer, QgsMarkerSymbol, QgsCategorizedSymbolRenderer,
    QgsRendererCategory, QgsSimpleMarkerSymbolLayer, QgsTextBufferSettings,
    QgsUnitTypes, QgsSimpleFillSymbolLayer, QgsFillSymbol
)
from qgis.gui import QgsMapLayerComboBox
from qgis.core import QgsMapLayerProxyModel
from collections import defaultdict, deque
import processing


# =============================================================================
# STYLING FUNCTIONS
# =============================================================================

class LayerStyler:
    """Apply professional styles to layers"""
    
    @staticmethod
    def style_equipment_layer(layer):
        """Categorized styling for Equipment layer based on TYPE"""
        categories = [
            ('OLT-FX4', '#e74c3c', 'star', 8),
            ('FDH-96F', '#3498db', 'square', 6),
            ('FDH-32F', '#2980b9', 'square', 5),
            ('Cabinet', '#9b59b6', 'diamond', 7),
            ('Splice Closure', '#27ae60', 'circle', 5),
            ('HSW', '#f39c12', 'triangle', 5),
        ]
        
        category_list = []
        for type_val, color, shape, size in categories:
            symbol = QgsMarkerSymbol.createSimple({
                'name': shape, 'color': color, 'size': str(size),
                'outline_color': '#2c3e50', 'outline_width': '0.5'
            })
            category_list.append(QgsRendererCategory(type_val, symbol, type_val))
        
        default_symbol = QgsMarkerSymbol.createSimple({
            'name': 'circle', 'color': '#95a5a6', 'size': '4',
            'outline_color': '#7f8c8d', 'outline_width': '0.4'
        })
        category_list.append(QgsRendererCategory('', default_symbol, 'Other'))
        
        renderer = QgsCategorizedSymbolRenderer('TYPE', category_list)
        layer.setRenderer(renderer)
        LayerStyler._apply_label(layer, 'Callouts', 8, bold=True, 
                                  buffer_color='#ffffff', buffer_size=1.0)
    
    @staticmethod
    def style_fiber_layer(layer):
        """Style Fiber_Cable layer"""
        symbol = QgsSymbol.defaultSymbol(layer.geometryType())
        line_style = QgsSimpleLineSymbolLayer()
        line_style.setColor(QtGui.QColor('#2c3e50'))
        line_style.setWidth(1.5)
        symbol.changeSymbolLayer(0, line_style)
        layer.setRenderer(QgsSingleSymbolRenderer(symbol))
        LayerStyler._apply_label(layer, 'F_Call', 7, bold=False, 
                                  placement='curved', buffer_color='#ffffff', buffer_size=0.8)
    
    @staticmethod
    def style_grid_layer(layer):
        """Style Grid layer"""
        symbol = QgsFillSymbol.createSimple({
            'color': 'transparent', 'outline_color': '#bdc3c7',
            'outline_width': '0.3', 'outline_style': 'solid'
        })
        layer.setRenderer(QgsSingleSymbolRenderer(symbol))
    
    @staticmethod
    def style_sld_equipment(layer):
        """Categorized styling for SLD Equipment"""
        categories = [
            ('OLT-FX4', '#e74c3c', 'star', 10),
            ('FDH-96F', '#3498db', 'square', 8),
            ('FDH-32F', '#2980b9', 'square', 7),
            ('Cabinet', '#9b59b6', 'diamond', 9),
            ('Splice Closure', '#27ae60', 'circle', 6),
            ('HSW', '#f39c12', 'triangle', 6),
        ]
        
        category_list = []
        for type_val, color, shape, size in categories:
            symbol = QgsMarkerSymbol.createSimple({
                'name': shape, 'color': color, 'size': str(size),
                'outline_color': '#1a252f', 'outline_width': '0.8'
            })
            category_list.append(QgsRendererCategory(type_val, symbol, type_val))
        
        default_symbol = QgsMarkerSymbol.createSimple({
            'name': 'circle', 'color': '#95a5a6', 'size': '5',
            'outline_color': '#7f8c8d', 'outline_width': '0.5'
        })
        category_list.append(QgsRendererCategory('', default_symbol, 'Other'))
        
        renderer = QgsCategorizedSymbolRenderer('TYPE', category_list)
        layer.setRenderer(renderer)
        LayerStyler._apply_label(layer, 'Callouts', 9, bold=True,
                                  buffer_color='#ffffff', buffer_size=1.2)
    
    @staticmethod
    def style_sld_fiber(layer):
        """Style SLD Fiber lines"""
        symbol = QgsSymbol.defaultSymbol(layer.geometryType())
        line_style = QgsSimpleLineSymbolLayer()
        line_style.setColor(QtGui.QColor('#34495e'))
        line_style.setWidth(2.0)
        symbol.changeSymbolLayer(0, line_style)
        layer.setRenderer(QgsSingleSymbolRenderer(symbol))
        LayerStyler._apply_label(layer, 'F_Call', 8, bold=False,
                                  placement='line', buffer_color='#ffffff', buffer_size=1.0)
    
    @staticmethod
    def style_sld_grid(layer):
        """Style SLD Grid"""
        symbol = QgsSymbol.defaultSymbol(layer.geometryType())
        line_style = QgsSimpleLineSymbolLayer()
        line_style.setColor(QtGui.QColor('#ecf0f1'))
        line_style.setWidth(0.5)
        symbol.changeSymbolLayer(0, line_style)
        layer.setRenderer(QgsSingleSymbolRenderer(symbol))
    
    @staticmethod
    def _apply_label(layer, field_name, font_size, bold=False, 
                     placement='over', buffer_color=None, buffer_size=0):
        """Apply labeling to a layer"""
        settings = QgsPalLayerSettings()
        settings.enabled = True
        settings.fieldName = field_name
        
        if placement == 'curved':
            settings.placement = QgsPalLayerSettings.Curved
        elif placement == 'line':
            settings.placement = QgsPalLayerSettings.Line
        else:
            settings.placement = QgsPalLayerSettings.OverPoint
        
        text_format = QgsTextFormat()
        font = QtGui.QFont('Arial', font_size)
        font.setBold(bold)
        text_format.setFont(font)
        text_format.setSize(font_size)
        text_format.setSizeUnit(QgsUnitTypes.RenderPoints)
        text_format.setColor(QtGui.QColor('#2c3e50'))
        
        if buffer_color and buffer_size > 0:
            buffer_settings = QgsTextBufferSettings()
            buffer_settings.setEnabled(True)
            buffer_settings.setSize(buffer_size)
            buffer_settings.setSizeUnit(QgsUnitTypes.RenderMillimeters)
            buffer_settings.setColor(QtGui.QColor(buffer_color))
            text_format.setBuffer(buffer_settings)
        
        settings.setFormat(text_format)
        layer.setLabelsEnabled(True)
        layer.setLabeling(QgsVectorLayerSimpleLabeling(settings))


# =============================================================================
# STYLE FILE BROWSER WIDGET
# =============================================================================

class StyleFileBrowser(QWidget):
    """Widget for browsing QML style files with enable/disable option"""
    
    def __init__(self, label_text, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        
        self.checkbox = QCheckBox(f"{label_text}:")
        self.checkbox.setChecked(False)
        self.checkbox.toggled.connect(self._on_toggle)
        
        self.line_edit = QLineEdit()
        self.line_edit.setPlaceholderText("Use default style")
        self.line_edit.setEnabled(False)
        
        self.browse_btn = QPushButton("Browse...")
        self.browse_btn.setEnabled(False)
        self.browse_btn.clicked.connect(self._browse)
        
        layout.addWidget(self.checkbox)
        layout.addWidget(self.line_edit, 1)
        layout.addWidget(self.browse_btn)
    
    def _on_toggle(self, checked):
        self.line_edit.setEnabled(checked)
        self.browse_btn.setEnabled(checked)
        if not checked:
            self.line_edit.clear()
            self.line_edit.setPlaceholderText("Use default style")
    
    def _browse(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Select Style File", "",
            "QGIS Style Files (*.qml);;All Files (*)"
        )
        if file_path:
            self.line_edit.setText(file_path)
    
    def get_style_path(self):
        """Returns the style file path or None if using default"""
        if self.checkbox.isChecked() and self.line_edit.text().strip():
            return self.line_edit.text().strip()
        return None


# =============================================================================
# PART 1: SLD DATA CREATION
# =============================================================================

def get_splitter_count(house_count):
    """Calculate splitter count from HOUSE COUN"""
    try:
        count = int(house_count) if house_count else 0
    except (ValueError, TypeError):
        count = 0
    return 1 if count < 51 else 2


class SLDDataCreator:
    """Creates Equipment, Fiber_Cable, Grid layers from source data"""
    
    def __init__(self, log_callback=None):
        self.log = log_callback or print
        self.outputs = {}
    
    def run(self, params):
        """Main processing function"""
        context = QgsProcessingContext()
        feedback = QgsProcessingFeedback()
        
        cabinet = params['cabinet']
        access_structures = params['access_structures']
        closures = params['closures']
        feeder_fiber = params['feeder_fiber']
        fdh = params['fdh']
        hc_field = params['house_count_field']
        
        # Step 1: Create FDH with splitter count
        self.log("▶ Step 1: Calculating splitter count...")
        fdh_sized = self._create_fdh_with_splitter(fdh, hc_field)
        self.log(f"  ✓ Calculated for {fdh_sized.featureCount()} FDH features")
        
        # Step 2: Snap feeder to FDH
        self.log("▶ Step 2: Snapping feeder to FDH...")
        self.outputs['snap1'] = processing.run('native:snapgeometries', {
            'BEHAVIOR': 0, 'INPUT': feeder_fiber, 'REFERENCE_LAYER': fdh_sized,
            'TOLERANCE': 10, 'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 3: Create Grid
        self.log("▶ Step 3: Creating grid...")
        self.outputs['grid'] = processing.run('native:creategrid', {
            'CRS': 'ProjectCrs', 'EXTENT': feeder_fiber, 'HOVERLAY': 0,
            'HSPACING': 750, 'TYPE': 2, 'VOVERLAY': 0, 'VSPACING': 750,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 4: Snap to closures
        self.log("▶ Step 4: Snapping to closures...")
        self.outputs['snap2'] = processing.run('native:snapgeometries', {
            'BEHAVIOR': 0, 'INPUT': self.outputs['snap1']['OUTPUT'],
            'REFERENCE_LAYER': closures, 'TOLERANCE': 10,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 5: Refactor FDH fields
        self.log("▶ Step 5: Refactoring FDH fields...")
        self.outputs['spc_fdh'] = processing.run('native:refactorfields', {
            'FIELDS_MAPPING': [
                {'expression': '"TYPE"', 'length': 254, 'name': 'TYPE', 'type': 10, 'type_name': 'text'},
                {'expression': '"IDENTIFIER"', 'length': 254, 'name': 'IDENTIFIER', 'type': 10, 'type_name': 'text'},
                {'expression': '"ELEMENTID"', 'length': 254, 'name': 'ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"NO_SP_1_32"', 'length': 10, 'name': 'NO_SP_1_32', 'type': 4, 'type_name': 'int8'}
            ],
            'INPUT': fdh_sized, 'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 6: Snap to access structures
        self.log("▶ Step 6: Snapping to access structures...")
        self.outputs['snap3'] = processing.run('native:snapgeometries', {
            'BEHAVIOR': 0, 'INPUT': self.outputs['snap2']['OUTPUT'],
            'REFERENCE_LAYER': access_structures, 'TOLERANCE': 10,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 7: Merge vector layers
        self.log("▶ Step 7: Merging layers...")
        self.outputs['merged'] = processing.run('native:mergevectorlayers', {
            'CRS': 'ProjectCrs',
            'LAYERS': [cabinet, closures, self.outputs['spc_fdh']['OUTPUT']],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 8: Simplify geometries
        self.log("▶ Step 8: Simplifying geometries...")
        self.outputs['simplified'] = processing.run('native:simplifygeometries', {
            'INPUT': self.outputs['snap3']['OUTPUT'], 'METHOD': 0,
            'TOLERANCE': 3280.84, 'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 9: Join with access structures
        self.log("▶ Step 9: Joining with access structures...")
        self.outputs['joined_hh'] = processing.run('native:joinattributesbylocation', {
            'DISCARD_NONMATCHING': False, 'INPUT': self.outputs['merged']['OUTPUT'],
            'JOIN': access_structures, 'JOIN_FIELDS': ['ELEMENTID'],
            'METHOD': 0, 'PREDICATE': [0], 'PREFIX': 'HH_',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 10: Refactor HH fields
        self.log("▶ Step 10: Refactoring fields...")
        self.outputs['hh_refac'] = processing.run('native:refactorfields', {
            'FIELDS_MAPPING': [
                {'expression': '"TYPE"', 'length': 254, 'name': 'TYPE', 'type': 10, 'type_name': 'text'},
                {'expression': '"IDENTIFIER"', 'length': 254, 'name': 'IDENTIFIER', 'type': 10, 'type_name': 'text'},
                {'expression': '"ELEMENTID"', 'length': 254, 'name': 'ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"NO_SP_1_32"', 'length': 10, 'name': 'NO_SP_1_32', 'type': 4, 'type_name': 'int8'}
            ],
            'INPUT': self.outputs['joined_hh']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 11: Reproject fiber
        self.log("▶ Step 11: Reprojecting fiber...")
        self.outputs['fiber_reproj'] = processing.run('native:reprojectlayer', {
            'CONVERT_CURVED_GEOMETRIES': False,
            'INPUT': self.outputs['simplified']['OUTPUT'],
            'OPERATION': '', 'TARGET_CRS': 'ProjectCrs',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 12: Create fiber labels
        self.log("▶ Step 12: Creating fiber labels...")
        fiber_label_expr = '''concat(if(trim(array_get(string_to_array("IDENTIFIER", ','), 2)) ='12F',concat(trim(array_get(string_to_array("IDENTIFIER", ','), 2)),' STINGRAY (unk'')','\\n','PLACE IN SPARE DUCT 2' ),concat(trim(array_get(string_to_array("IDENTIFIER", ','), 2)),' FIBER (unk'')' )),concat('\\n',array_get(string_to_array("LABEL", ','), 0),',',array_get(string_to_array("LABEL", ','), 1),'\\n',array_get(string_to_array("LABEL", ','), 2),',',array_get(string_to_array("LABEL", ','), 3)))'''
        
        self.outputs['fiber_label'] = processing.run('native:refactorfields', {
            'FIELDS_MAPPING': [
                {'expression': '"TYPE"', 'length': 254, 'name': 'TYPE', 'type': 10, 'type_name': 'text'},
                {'expression': '"IDENTIFIER"', 'length': 254, 'name': 'IDENTIFIER', 'type': 10, 'type_name': 'text'},
                {'expression': '"FROM"', 'length': 254, 'name': 'FROM', 'type': 10, 'type_name': 'text'},
                {'expression': '"TO"', 'length': 254, 'name': 'TO', 'type': 10, 'type_name': 'text'},
                {'expression': '"LABEL"', 'length': 254, 'name': 'LABEL', 'type': 10, 'type_name': 'text'},
                {'expression': '"LOOP"', 'length': 11, 'name': 'LOOP', 'type': 4, 'type_name': 'int8'},
                {'expression': '"ELEMENTID"', 'length': 254, 'name': 'ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"LENGTH"', 'length': 10, 'name': 'LENGTH', 'precision': 2, 'type': 6, 'type_name': 'double precision'},
                {'expression': '"LENGTH"+10', 'length': 10, 'name': 'LENGTH_10', 'precision': 2, 'type': 6, 'type_name': 'double precision'},
                {'expression': fiber_label_expr, 'length': 254, 'name': 'F_Call', 'type': 10, 'type_name': 'text'},
                {'expression': 'x(centroid($geometry))', 'length': 20, 'name': 'x', 'precision': 6, 'type': 6, 'type_name': 'double precision'},
                {'expression': 'y(centroid($geometry))', 'length': 20, 'name': 'y', 'precision': 6, 'type': 6, 'type_name': 'double precision'}
            ],
            'INPUT': self.outputs['fiber_reproj']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 13: Fix fiber geometries
        self.log("▶ Step 13: Fixing fiber geometries...")
        self.outputs['fiber_fixed'] = processing.run('native:fixgeometries', {
            'INPUT': self.outputs['fiber_label']['OUTPUT'], 'METHOD': 1,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 14: Join labels
        self.log("▶ Step 14: Joining labels...")
        self.outputs['pt_line'] = processing.run('native:joinattributestable', {
            'DISCARD_NONMATCHING': False, 'FIELD': 'ELEMENTID',
            'FIELDS_TO_COPY': ['LABEL'], 'FIELD_2': 'TO',
            'INPUT': self.outputs['hh_refac']['OUTPUT'],
            'INPUT_2': self.outputs['simplified']['OUTPUT'],
            'METHOD': 1, 'PREFIX': '',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 15: Reproject equipment
        self.log("▶ Step 15: Reprojecting equipment...")
        self.outputs['equip_reproj'] = processing.run('native:reprojectlayer', {
            'CONVERT_CURVED_GEOMETRIES': False,
            'INPUT': self.outputs['pt_line']['OUTPUT'],
            'OPERATION': '', 'TARGET_CRS': 'ProjectCrs',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 16: Equipment formulas
        self.log("▶ Step 16: Applying equipment formulas...")
        live_pairs_expr = '''if("TYPE"='FDH-96F',concat(array_get(string_to_array("LABEL", ','), 0),',',(trim(if((to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0)))+to_real(trim("NO_SP_1_32"))-1)=to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 1))),(array_get(string_to_array("LABEL", ','), 1)),concat(to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0))),'-',to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0)))+to_real(trim("NO_SP_1_32"))-1))))),'')'''
        
        callouts_expr = '''if("TYPE"='FDH-96F',concat('D-',"ELEMENTID",'\\n',if("TYPE"='FDH-96F',concat(array_get(string_to_array("LABEL", ','), 0),',',(trim(if((to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0)))+to_real(trim("NO_SP_1_32"))-1)=to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 1))),(array_get(string_to_array("LABEL", ','), 1)),concat(to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0))),'-',to_real(trim(array_get(string_to_array(trim(array_get(string_to_array("LABEL", ','), 1)), '-'), 0)))+to_real(trim("NO_SP_1_32"))-1))))),''),'\\n',"DaisyChain"),"ELEMENTID")'''
        
        self.outputs['equip_formula'] = processing.run('native:refactorfields', {
            'FIELDS_MAPPING': [
                {'expression': '"TYPE"', 'length': 254, 'name': 'TYPE', 'type': 10, 'type_name': 'text'},
                {'expression': '"IDENTIFIER"', 'length': 254, 'name': 'IDENTIFIER', 'type': 10, 'type_name': 'text'},
                {'expression': '"ELEMENTID"', 'length': 254, 'name': 'ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"NO_SP_1_32"', 'length': 10, 'name': 'NO_SP_1_32', 'type': 4, 'type_name': 'int8'},
                {'expression': '"LABEL"', 'length': 254, 'name': 'LABEL', 'type': 10, 'type_name': 'text'},
                {'expression': live_pairs_expr, 'length': 100, 'name': 'Live_Pairs', 'type': 10, 'type_name': 'text'},
                {'expression': "''", 'length': 100, 'name': 'DaisyChain', 'type': 10, 'type_name': 'text'},
                {'expression': callouts_expr, 'length': 254, 'name': 'Callouts', 'type': 10, 'type_name': 'text'}
            ],
            'INPUT': self.outputs['equip_reproj']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 17: Join by nearest
        self.log("▶ Step 17: Joining by nearest...")
        self.outputs['equip_nearest'] = processing.run('native:joinbynearest', {
            'DISCARD_NONMATCHING': False, 'FIELDS_TO_COPY': ['ELEMENTID'],
            'INPUT': self.outputs['equip_formula']['OUTPUT'],
            'INPUT_2': access_structures, 'MAX_DISTANCE': None,
            'NEIGHBORS': 1, 'PREFIX': 'HH_',
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 18: Final equipment formulas
        self.log("▶ Step 18: Final equipment formulas...")
        final_callouts_expr = '''trim(if(TYPE='FDH-96F', "Callouts" ,concat( "ELEMENTID" ,'\\n', "HH_ELEMENTID" )))'''
        
        self.outputs['equip_final'] = processing.run('native:refactorfields', {
            'FIELDS_MAPPING': [
                {'expression': '"TYPE"', 'length': 254, 'name': 'TYPE', 'type': 10, 'type_name': 'text'},
                {'expression': '"IDENTIFIER"', 'length': 254, 'name': 'IDENTIFIER', 'type': 10, 'type_name': 'text'},
                {'expression': '"ELEMENTID"', 'length': 254, 'name': 'ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"HH_ELEMENTID"', 'length': 254, 'name': 'HH_ELEMENTID', 'type': 10, 'type_name': 'text'},
                {'expression': '"NO_SP_1_32"', 'length': 10, 'name': 'NO_SP_1_32', 'type': 4, 'type_name': 'int8'},
                {'expression': '"LABEL"', 'length': 254, 'name': 'LABEL', 'type': 10, 'type_name': 'text'},
                {'expression': '"Live_Pairs"', 'length': 100, 'name': 'Live_Pairs', 'type': 10, 'type_name': 'text'},
                {'expression': '"DaisyChain"', 'length': 100, 'name': 'DaisyChain', 'type': 10, 'type_name': 'text'},
                {'expression': final_callouts_expr, 'length': 254, 'name': 'Callouts', 'type': 10, 'type_name': 'text'}
            ],
            'INPUT': self.outputs['equip_nearest']['OUTPUT'],
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        # Step 19: Fix equipment geometries
        self.log("▶ Step 19: Fixing geometries...")
        self.outputs['equip_fixed'] = processing.run('native:fixgeometries', {
            'INPUT': self.outputs['equip_final']['OUTPUT'], 'METHOD': 1,
            'OUTPUT': QgsProcessing.TEMPORARY_OUTPUT
        }, context=context, feedback=feedback)
        self.log("  ✓ Done")
        
        return {
            'grid': self.outputs['grid']['OUTPUT'],
            'fiber': self.outputs['fiber_fixed']['OUTPUT'],
            'equipment': self.outputs['equip_fixed']['OUTPUT']
        }
    
    def _create_fdh_with_splitter(self, fdh_layer, hc_field):
        """Create FDH layer with NO_SP_1_32 from HOUSE COUN"""
        crs = fdh_layer.crs()
        geom_type = fdh_layer.geometryType()
        
        uri = f"Point?crs={crs.authid()}" if geom_type == 0 else f"Polygon?crs={crs.authid()}"
        mem_layer = QgsVectorLayer(uri, "FDH_Sized", "memory")
        provider = mem_layer.dataProvider()
        
        existing_fields = fdh_layer.fields().toList()
        provider.addAttributes(existing_fields)
        
        if 'NO_SP_1_32' not in [f.name() for f in existing_fields]:
            provider.addAttributes([QgsField("NO_SP_1_32", QVariant.Int)])
        
        mem_layer.updateFields()
        
        new_features = []
        for feat in fdh_layer.getFeatures():
            new_feat = QgsFeature(mem_layer.fields())
            new_feat.setGeometry(feat.geometry())
            
            for field in existing_fields:
                try:
                    new_feat[field.name()] = feat[field.name()]
                except:
                    pass
            
            house_count = feat[hc_field]
            new_feat['NO_SP_1_32'] = get_splitter_count(house_count)
            
            if 'TYPE' in [f.name() for f in mem_layer.fields()]:
                new_feat['TYPE'] = 'FDH-96F'
            
            new_features.append(new_feat)
        
        provider.addFeatures(new_features)
        mem_layer.updateExtents()
        return mem_layer


# =============================================================================
# PART 2: DAISY CHAIN CALCULATION
# =============================================================================

class DaisyChainCalculator:
    """Calculate Daisy Chain values and update Callouts"""
    
    def __init__(self, log_callback=None):
        self.log = log_callback or print
        self.live_12 = 0
        self.con_live = 0
    
    def run(self, equipment_layer, fiber_layer):
        """Calculate Daisy Chain"""
        self.log("\n▶ Calculating Daisy Chain...")
        
        import re
        eq_fnames = [f.name() for f in equipment_layer.fields()]
        ln_fnames = [f.name() for f in fiber_layer.fields()]
        
        def traverse(from_id):
            if not from_id:
                return
            
            connections = []
            for feat in fiber_layer.getFeatures():
                from_val = feat['FROM'] if 'FROM' in ln_fnames else None
                if from_val == from_id:
                    connections.append(feat)
            
            def sort_key(feature):
                label = feature['LABEL'] if 'LABEL' in ln_fnames and feature['LABEL'] else ''
                parts = label.split("DEAD", 1)
                if len(parts) == 2:
                    nums = re.findall(r'\d+', parts[0].strip()[:-1] if parts[0].strip() else '')
                    return int(nums[-1]) if nums else -1
                return -1
            
            connections.sort(key=sort_key)
            
            for conn in connections:
                identifier = conn['IDENTIFIER'] if 'IDENTIFIER' in ln_fnames else ''
                identifier = identifier or ''
                parts = identifier.split(',')
                num = parts[-1][:-1].strip() if parts and len(parts[-1]) > 0 else ''
                
                to_field = conn['TO'] if 'TO' in ln_fnames else ''
                to_field = to_field or ''
                
                if num in ('48', '12') and to_field.startswith('HSW'):
                    if num == '48':
                        self.live_12 += 1
                        self.con_live = 1
                    else:
                        self.con_live += 1
                    
                    daisy_value = f'{self.live_12}.{self.con_live}'
                    
                    req = QgsFeatureRequest().setFilterExpression(f"\"ELEMENTID\" = '{conn['TO']}'")
                    equipment_layer.startEditing()
                    for feat in equipment_layer.getFeatures(req):
                        feat['DaisyChain'] = daisy_value
                        equipment_layer.updateFeature(feat)
                    equipment_layer.commitChanges()
                
                traverse(conn['TO'])
        
        olt_count = 0
        for feat in equipment_layer.getFeatures():
            feat_type = feat['TYPE'] if 'TYPE' in eq_fnames else ''
            if feat_type and 'OLT' in str(feat_type).upper():
                element_id = feat['ELEMENTID'] if 'ELEMENTID' in eq_fnames else None
                if element_id:
                    olt_count += 1
                    self.log(f"  Found OLT: {element_id}")
                    traverse(element_id)
        
        if olt_count == 0:
            self.log("  ⚠ No OLT found")
        
        self.log("  ✓ Daisy Chain calculated")
        self._update_callouts(equipment_layer)
    
    def _update_callouts(self, equipment_layer):
        """Update Callouts with Daisy Chain"""
        self.log("▶ Updating Callouts...")
        
        eq_fnames = [f.name() for f in equipment_layer.fields()]
        equipment_layer.startEditing()
        
        for feat in equipment_layer.getFeatures():
            feat_type = feat['TYPE'] if 'TYPE' in eq_fnames else ''
            element_id = feat['ELEMENTID'] if 'ELEMENTID' in eq_fnames else ''
            hh_element_id = feat['HH_ELEMENTID'] if 'HH_ELEMENTID' in eq_fnames else ''
            live_pairs = feat['Live_Pairs'] if 'Live_Pairs' in eq_fnames else ''
            daisy_chain = feat['DaisyChain'] if 'DaisyChain' in eq_fnames else ''
            
            if feat_type == 'FDH-96F':
                parts = [f'D-{element_id}']
                if live_pairs:
                    parts.append(str(live_pairs))
                if daisy_chain:
                    parts.append(f'Daisy Chain: {daisy_chain}')
            else:
                parts = []
                if element_id:
                    parts.append(str(element_id))
                if hh_element_id:
                    parts.append(str(hh_element_id))
                if daisy_chain:
                    parts.append(f'Daisy Chain: {daisy_chain}')
            
            feat['Callouts'] = '\n'.join(parts)
            equipment_layer.updateFeature(feat)
        
        equipment_layer.commitChanges()
        self.log("  ✓ Callouts updated")


# =============================================================================
# PART 3: SLD TREE LAYOUT GENERATOR - FIXED BUS ROUTING
# =============================================================================

class TreeLayoutEngine:
    """Proper tree layout with no overlaps"""
    
    def __init__(self, x_spacing=300, y_spacing=250):
        self.x_spacing = x_spacing
        self.y_spacing = y_spacing
        self.connections = {}
        self.all_nodes = set()
        self.roots = []
        self.positions = {}
        self.node_levels = {}
        self.node_width = {}
    
    def build_graph(self, fiber_layer, from_field, to_field):
        """Build directed graph"""
        self.connections = defaultdict(list)
        from_nodes = set()
        to_nodes = set()
        
        for feat in fiber_layer.getFeatures():
            frm = str(feat[from_field]).strip() if feat[from_field] else ''
            to = str(feat[to_field]).strip() if feat[to_field] else ''
            
            if frm and to:
                self.all_nodes.add(frm)
                self.all_nodes.add(to)
                from_nodes.add(frm)
                to_nodes.add(to)
                if to not in self.connections[frm]:
                    self.connections[frm].append(to)
        
        self.roots = list(from_nodes - to_nodes)
        if not self.roots and self.all_nodes:
            self.roots = [list(self.all_nodes)[0]]
        
        return len(self.all_nodes), len(self.roots)
    
    def calculate_layout(self):
        """Calculate positions"""
        if not self.roots:
            return {}
        
        self._assign_levels()
        
        for root in self.roots:
            self._calculate_subtree_width(root)
        
        current_x = 0
        for root in self.roots:
            subtree_width = self.node_width.get(root, 1) * self.x_spacing
            self._assign_x_positions(root, current_x, current_x + subtree_width)
            current_x += subtree_width + self.x_spacing * 2
        
        return self.positions
    
    def _assign_levels(self):
        visited = set()
        queue = deque()
        
        for root in self.roots:
            queue.append((root, 0))
            visited.add(root)
            self.node_levels[root] = 0
        
        while queue:
            node, level = queue.popleft()
            self.node_levels[node] = level
            
            for child in self.connections.get(node, []):
                if child not in visited:
                    visited.add(child)
                    queue.append((child, level + 1))
    
    def _calculate_subtree_width(self, node):
        children = self.connections.get(node, [])
        if not children:
            self.node_width[node] = 1
            return 1
        
        total = sum(self._calculate_subtree_width(c) for c in children)
        self.node_width[node] = total
        return total
    
    def _assign_x_positions(self, node, left, right):
        center_x = (left + right) / 2
        level = self.node_levels.get(node, 0)
        y = -level * self.y_spacing
        
        self.positions[node] = (center_x, y)
        
        children = self.connections.get(node, [])
        if not children:
            return
        
        total_width = sum(self.node_width.get(c, 1) for c in children)
        width_per_unit = (right - left) / total_width if total_width > 0 else (right - left)
        
        current_left = left
        for child in children:
            child_width = self.node_width.get(child, 1) * width_per_unit
            self._assign_x_positions(child, current_left, current_left + child_width)
            current_left += child_width


class SLDLayoutGenerator:
    """Generate SLD diagram layers with PROPER BUS-BASED ROUTING"""
    
    def __init__(self, log_callback=None):
        self.log = log_callback or print
    
    def generate(self, fiber_layer, equip_layer=None,
                 from_field='FROM', to_field='TO',
                 eq_key_field='ELEMENTID', eq_label_field='Callouts',
                 fb_label_field='F_Call',
                 x_spacing=300, y_spacing=250, draw_grid=True,
                 point_style_file=None, line_style_file=None, grid_style_file=None):
        """Generate SLD layers with PROPER bus-based line routing"""
        
        self.log("\n▶ Building network graph...")
        
        layout = TreeLayoutEngine(x_spacing, y_spacing)
        num_nodes, num_roots = layout.build_graph(fiber_layer, from_field, to_field)
        
        self.log(f"  Nodes: {num_nodes}, Roots: {num_roots}")
        
        if num_nodes == 0:
            self.log("  ⚠ No nodes found!")
            return None, None, None
        
        self.log("▶ Calculating tree layout...")
        positions = layout.calculate_layout()
        self.log(f"  Positioned {len(positions)} nodes")
        
        crs = fiber_layer.crs().authid() if fiber_layer.crs().isValid() else "EPSG:3857"
        
        # Create layers
        point_layer = QgsVectorLayer(f"Point?crs={crs}", "SLD_Equipment", "memory")
        line_layer = QgsVectorLayer(f"LineString?crs={crs}", "SLD_Fiber", "memory")
        grid_layer = QgsVectorLayer(f"LineString?crs={crs}", "SLD_Grid", "memory")
        
        point_pr = point_layer.dataProvider()
        line_pr = line_layer.dataProvider()
        grid_pr = grid_layer.dataProvider()
        
        # Add fields for points
        if equip_layer:
            point_pr.addAttributes(equip_layer.fields().toList())
        else:
            point_pr.addAttributes([QgsField("NODE_ID", QVariant.String)])
        point_layer.updateFields()
        
        # Add fields for lines - simple structure
        line_fields = [
            QgsField("FROM_NODE", QVariant.String),
            QgsField("TO_NODE", QVariant.String),
            QgsField("SEGMENT_TYPE", QVariant.String),
            QgsField("F_Call", QVariant.String),
        ]
        line_pr.addAttributes(line_fields)
        line_layer.updateFields()
        
        # Build lookups
        equip_attrs = {}
        if equip_layer and eq_key_field:
            for feat in equip_layer.getFeatures():
                key = str(feat[eq_key_field]).strip() if feat[eq_key_field] else ''
                if key:
                    equip_attrs[key] = feat.attributes()
        
        # Build fiber attributes lookup
        fiber_fcall = {}
        for feat in fiber_layer.getFeatures():
            frm = str(feat[from_field]).strip() if feat[from_field] else ''
            to = str(feat[to_field]).strip() if feat[to_field] else ''
            if frm and to and fb_label_field:
                try:
                    fcall_val = feat[fb_label_field]
                    if fcall_val:
                        fiber_fcall[(frm, to)] = str(fcall_val)
                except:
                    pass
        
        # Create equipment points
        self.log("▶ Creating equipment nodes...")
        for node_id, (x, y) in positions.items():
            feat = QgsFeature(point_layer.fields())
            feat.setGeometry(QgsGeometry.fromPointXY(QgsPointXY(x, y)))
            
            if equip_layer and node_id in equip_attrs:
                feat.setAttributes(equip_attrs[node_id])
            elif not equip_layer:
                feat.setAttributes([str(node_id)])
            
            point_pr.addFeature(feat)
        
        # PROPER BUS-BASED ROUTING
        self.log("▶ Creating fiber connections (PROPER bus-based routing)...")
        
        total_segments = 0
        
        for parent, children in layout.connections.items():
            if parent not in positions or not children:
                continue
            
            px, py = positions[parent]
            
            # Get valid children with positions
            valid_children = []
            child_positions_list = []
            for child in children:
                if child in positions:
                    valid_children.append(child)
                    child_positions_list.append(positions[child])
            
            if not valid_children:
                continue
            
            # Calculate bus Y level
            child_ys = [cp[1] for cp in child_positions_list]
            min_child_y = min(child_ys)
            bus_y = py + (min_child_y - py) * 0.5
            
            child_xs = [cp[0] for cp in child_positions_list]
            
            # SEGMENT 1: VERTICAL from parent DOWN to bus
            feat = QgsFeature(line_layer.fields())
            feat.setGeometry(QgsGeometry.fromPolylineXY([
                QgsPointXY(px, py),
                QgsPointXY(px, bus_y)
            ]))
            first_conn = (parent, valid_children[0])
            f_call = fiber_fcall.get(first_conn, '')
            feat.setAttributes([parent, '', 'VERTICAL_DOWN', f_call])
            line_pr.addFeature(feat)
            total_segments += 1
            
            # SEGMENT 2: HORIZONTAL BUS
            min_x = min(min(child_xs), px)
            max_x = max(max(child_xs), px)
            
            if min_x != max_x:
                feat = QgsFeature(line_layer.fields())
                feat.setGeometry(QgsGeometry.fromPolylineXY([
                    QgsPointXY(min_x, bus_y),
                    QgsPointXY(max_x, bus_y)
                ]))
                feat.setAttributes(['', '', 'HORIZONTAL_BUS', ''])
                line_pr.addFeature(feat)
                total_segments += 1
            
            # SEGMENT 3: VERTICAL lines from bus DOWN to each child
            for i, child in enumerate(valid_children):
                cx, cy = child_positions_list[i]
                
                feat = QgsFeature(line_layer.fields())
                feat.setGeometry(QgsGeometry.fromPolylineXY([
                    QgsPointXY(cx, bus_y),
                    QgsPointXY(cx, cy)
                ]))
                conn_key = (parent, child)
                f_call = fiber_fcall.get(conn_key, '')
                feat.setAttributes([parent, child, 'VERTICAL_UP', f_call])
                line_pr.addFeature(feat)
                total_segments += 1
        
        self.log(f"  Created {total_segments} line segments")
        
        # Create grid
        if draw_grid and positions:
            self.log("▶ Creating grid...")
            xs = [p[0] for p in positions.values()]
            ys = [p[1] for p in positions.values()]
            
            min_x, max_x = min(xs) - x_spacing, max(xs) + x_spacing
            min_y, max_y = min(ys) - y_spacing, max(ys) + y_spacing
            
            x = min_x
            while x <= max_x:
                feat = QgsFeature()
                feat.setGeometry(QgsGeometry.fromPolylineXY([
                    QgsPointXY(x, min_y), QgsPointXY(x, max_y)
                ]))
                grid_pr.addFeature(feat)
                x += x_spacing
            
            y = min_y
            while y <= max_y:
                feat = QgsFeature()
                feat.setGeometry(QgsGeometry.fromPolylineXY([
                    QgsPointXY(min_x, y), QgsPointXY(max_x, y)
                ]))
                grid_pr.addFeature(feat)
                y += y_spacing
        
        point_layer.updateExtents()
        line_layer.updateExtents()
        grid_layer.updateExtents()
        
        # Apply styles
        self.log("▶ Applying styles...")
        
        if point_style_file and point_style_file.strip():
            point_layer.loadNamedStyle(point_style_file)
            self.log(f"  ✓ Equipment style loaded from file")
        else:
            LayerStyler.style_sld_equipment(point_layer)
            self.log("  ✓ Equipment: Default style applied")
        
        if line_style_file and line_style_file.strip():
            line_layer.loadNamedStyle(line_style_file)
            self.log(f"  ✓ Fiber style loaded from file")
        else:
            LayerStyler.style_sld_fiber(line_layer)
            self.log("  ✓ Fiber: Default style applied")
        
        if grid_style_file and grid_style_file.strip():
            grid_layer.loadNamedStyle(grid_style_file)
            self.log(f"  ✓ Grid style loaded from file")
        else:
            LayerStyler.style_sld_grid(grid_layer)
            self.log("  ✓ Grid: Default style applied")
        
        self.log(f"\n  ✓ Equipment: {point_layer.featureCount()} nodes")
        self.log(f"  ✓ Fiber: {line_layer.featureCount()} line segments")
        
        return point_layer, line_layer, grid_layer


# =============================================================================
# MAIN DIALOG
# =============================================================================

class SLDWizardDialog(QDialog):
    """Main dialog with tabs for data creation and SLD layout"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("SLD Creation Wizard - Production (Fixed Bus Routing)")
        self.setMinimumSize(900, 1000)
        self._setup_ui()
        self._auto_populate()
    
    def _setup_ui(self):
        layout = QVBoxLayout(self)
        
        # Header
        header = QLabel("🔌 SLD Creation Wizard")
        header.setStyleSheet("""
            font-size: 20px; font-weight: bold; color: #2c3e50;
            padding: 15px; background: #ecf0f1; border-radius: 5px;
        """)
        header.setAlignment(Qt.AlignCenter)
        layout.addWidget(header)
        
        # Tabs
        tabs = QTabWidget()
        tabs.setStyleSheet("QTabBar::tab { padding: 10px 20px; font-weight: bold; }")
        
        # === TAB 1: Data Creation ===
        tab1 = QWidget()
        tab1_layout = QVBoxLayout(tab1)
        
        # Input layers group
        input_group = QGroupBox("📁 Input Layers")
        input_layout = QGridLayout(input_group)
        
        self.combos = {}
        layers = [
            ('cabinet', 'Cabinet'),
            ('splitters', 'Hexatronics Splitters'),
            ('access_structures', 'Access Structures'),
            ('closures', 'Project Closures'),
            ('feeder_fiber', 'Feeder Fiber'),
            ('fdh', 'FDH (with HOUSE COUN)'),
        ]
        
        for i, (key, label) in enumerate(layers):
            lbl = QLabel(f"{label}:")
            combo = QgsMapLayerComboBox()
            combo.setFilters(QgsMapLayerProxyModel.VectorLayer)
            combo.setAllowEmptyLayer(True)
            self.combos[key] = combo
            input_layout.addWidget(lbl, i, 0)
            input_layout.addWidget(combo, i, 1)
        
        tab1_layout.addWidget(input_group)
        
        # Data Creation Options
        opt_group = QGroupBox("⚙️ Options")
        opt_layout = QVBoxLayout(opt_group)
        self.chk_daisy = QCheckBox("Calculate Daisy Chain and update Callouts")
        self.chk_daisy.setChecked(True)
        self.chk_style = QCheckBox("Apply professional styling (or use custom styles below)")
        self.chk_style.setChecked(True)
        opt_layout.addWidget(self.chk_daisy)
        opt_layout.addWidget(self.chk_style)
        tab1_layout.addWidget(opt_group)
        
        # === STYLE FILES GROUP FOR TAB 1 ===
        style_group1 = QGroupBox("🎨 Custom Style Files (QML) - Data Layers")
        style_layout1 = QVBoxLayout(style_group1)
        
        style_info1 = QLabel("Leave unchecked to use default styles, or browse for custom QML files:")
        style_info1.setStyleSheet("color: #7f8c8d; font-style: italic;")
        style_layout1.addWidget(style_info1)
        
        self.style_data_equipment = StyleFileBrowser("Equipment Style")
        self.style_data_fiber = StyleFileBrowser("Fiber_Cable Style")
        self.style_data_grid = StyleFileBrowser("Grid Style")
        
        style_layout1.addWidget(self.style_data_equipment)
        style_layout1.addWidget(self.style_data_fiber)
        style_layout1.addWidget(self.style_data_grid)
        
        tab1_layout.addWidget(style_group1)
        
        tabs.addTab(tab1, "1️⃣ Create Data Layers")
        
        # === TAB 2: SLD Layout ===
        tab2 = QWidget()
        tab2_layout = QVBoxLayout(tab2)
        
        sld_input_group = QGroupBox("📁 Input Layers")
        sld_input_layout = QGridLayout(sld_input_group)
        
        sld_input_layout.addWidget(QLabel("Equipment Layer:"), 0, 0)
        self.cbo_equip = QgsMapLayerComboBox()
        self.cbo_equip.setFilters(QgsMapLayerProxyModel.PointLayer)
        self.cbo_equip.setAllowEmptyLayer(True)
        sld_input_layout.addWidget(self.cbo_equip, 0, 1)
        
        sld_input_layout.addWidget(QLabel("Fiber Layer:"), 1, 0)
        self.cbo_fiber = QgsMapLayerComboBox()
        self.cbo_fiber.setFilters(QgsMapLayerProxyModel.LineLayer)
        self.cbo_fiber.setAllowEmptyLayer(True)
        sld_input_layout.addWidget(self.cbo_fiber, 1, 1)
        
        tab2_layout.addWidget(sld_input_group)
        
        # SLD Layout Settings
        sld_settings_group = QGroupBox("📐 Layout Settings")
        sld_settings_layout = QGridLayout(sld_settings_group)
        
        sld_settings_layout.addWidget(QLabel("X Spacing:"), 0, 0)
        self.spn_x = QSpinBox()
        self.spn_x.setRange(100, 2000)
        self.spn_x.setValue(350)
        sld_settings_layout.addWidget(self.spn_x, 0, 1)
        
        sld_settings_layout.addWidget(QLabel("Y Spacing:"), 0, 2)
        self.spn_y = QSpinBox()
        self.spn_y.setRange(100, 2000)
        self.spn_y.setValue(300)
        sld_settings_layout.addWidget(self.spn_y, 0, 3)
        
        self.chk_grid = QCheckBox("Draw background grid")
        self.chk_grid.setChecked(True)
        sld_settings_layout.addWidget(self.chk_grid, 1, 0, 1, 4)
        
        tab2_layout.addWidget(sld_settings_group)
        
        # === STYLE FILES GROUP FOR TAB 2 ===
        style_group2 = QGroupBox("🎨 Custom Style Files (QML) - SLD Layers")
        style_layout2 = QVBoxLayout(style_group2)
        
        style_info2 = QLabel("Leave unchecked to use default styles, or browse for custom QML files:")
        style_info2.setStyleSheet("color: #7f8c8d; font-style: italic;")
        style_layout2.addWidget(style_info2)
        
        self.style_sld_equipment = StyleFileBrowser("SLD Equipment Style")
        self.style_sld_fiber = StyleFileBrowser("SLD Fiber Style")
        self.style_sld_grid = StyleFileBrowser("SLD Grid Style")
        
        style_layout2.addWidget(self.style_sld_equipment)
        style_layout2.addWidget(self.style_sld_fiber)
        style_layout2.addWidget(self.style_sld_grid)
        
        tab2_layout.addWidget(style_group2)
        
        tabs.addTab(tab2, "2️⃣ Generate SLD Layout")
        
        layout.addWidget(tabs)
        
        # Progress section
        prog_group = QGroupBox("📊 Progress")
        prog_layout = QVBoxLayout(prog_group)
        
        self.progress = QProgressBar()
        self.progress.setStyleSheet("""
            QProgressBar { border: 2px solid #bdc3c7; border-radius: 5px; text-align: center; }
            QProgressBar::chunk { background: #3498db; }
        """)
        
        self.status = QLabel("Ready")
        self.status.setAlignment(Qt.AlignCenter)
        self.status.setStyleSheet("font-weight: bold; font-size: 12px;")
        
        self.log = QTextEdit()
        self.log.setReadOnly(True)
        self.log.setMaximumHeight(150)
        self.log.setStyleSheet("""
            background: #1a1a2e; color: #00ff00; 
            font-family: 'Consolas', monospace; font-size: 11px;
            border-radius: 5px;
        """)
        
        prog_layout.addWidget(self.progress)
        prog_layout.addWidget(self.status)
        prog_layout.addWidget(self.log)
        layout.addWidget(prog_group)
        
        # Buttons
        btn_layout = QHBoxLayout()
        
        self.btn_create = QPushButton("🔧 Create Data Layers")
        self.btn_create.setStyleSheet("""
            QPushButton { background: #27ae60; color: white; padding: 12px 25px; 
                          font-weight: bold; border-radius: 5px; font-size: 13px; }
            QPushButton:hover { background: #2ecc71; }
            QPushButton:disabled { background: #95a5a6; }
        """)
        self.btn_create.clicked.connect(self._run_data_creation)
        
        self.btn_sld = QPushButton("📊 Generate SLD Layout")
        self.btn_sld.setStyleSheet("""
            QPushButton { background: #3498db; color: white; padding: 12px 25px;
                          font-weight: bold; border-radius: 5px; font-size: 13px; }
            QPushButton:hover { background: #5dade2; }
            QPushButton:disabled { background: #95a5a6; }
        """)
        self.btn_sld.clicked.connect(self._run_sld_layout)
        
        self.btn_close = QPushButton("Close")
        self.btn_close.setStyleSheet("padding: 12px 20px;")
        self.btn_close.clicked.connect(self.reject)
        
        btn_layout.addWidget(self.btn_create)
        btn_layout.addWidget(self.btn_sld)
        btn_layout.addStretch()
        btn_layout.addWidget(self.btn_close)
        layout.addLayout(btn_layout)
    
    def _auto_populate(self):
        """Auto-populate layer combos"""
        layers = QgsProject.instance().mapLayers().values()
        patterns = {
            'cabinet': ['cabinet'],
            'splitters': ['splitter'],
            'access_structures': ['access', 'structure'],
            'closures': ['closure'],
            'feeder_fiber': ['feeder'],
            'fdh': ['fdh'],
        }
        
        for key, keywords in patterns.items():
            for layer in layers:
                name = layer.name().lower()
                if any(kw in name for kw in keywords):
                    self.combos[key].setLayer(layer)
                    break
    
    def _log(self, msg):
        self.log.append(msg)
        QApplication.processEvents()
    
    def _find_house_count_field(self, layer):
        field_names = [f.name() for f in layer.fields()]
        for name in ['HOUSE COUN', 'HOUSE_COUN', 'HOUSECOUN']:
            if name in field_names:
                return name
        for name in field_names:
            if 'house' in name.lower() and 'coun' in name.lower():
                return name
        return None
    
    def _apply_style_or_default(self, layer, style_browser, default_styler):
        """Apply custom style from browser or use default"""
        style_path = style_browser.get_style_path()
        if style_path:
            layer.loadNamedStyle(style_path)
            self._log(f"  ✓ Custom style loaded: {style_path}")
        else:
            default_styler(layer)
            self._log(f"  ✓ Default style applied")
    
    def _run_data_creation(self):
        """Run data creation"""
        missing = [k for k, v in self.combos.items() if not v.currentLayer()]
        if missing:
            QMessageBox.warning(self, "Missing Layers", f"Please select: {', '.join(missing)}")
            return
        
        fdh = self.combos['fdh'].currentLayer()
        hc_field = self._find_house_count_field(fdh)
        if not hc_field:
            QMessageBox.warning(self, "Error", "Cannot find HOUSE COUN field in FDH layer")
            return
        
        self.btn_create.setEnabled(False)
        self.btn_sld.setEnabled(False)
        self.log.clear()
        self.progress.setValue(0)
        self.status.setText("Creating data layers...")
        
        try:
            self._log(f"✓ Found HOUSE COUN field: '{hc_field}'")
            self._log("="*50)
            
            params = {
                'cabinet': self.combos['cabinet'].currentLayer(),
                'splitters': self.combos['splitters'].currentLayer(),
                'access_structures': self.combos['access_structures'].currentLayer(),
                'closures': self.combos['closures'].currentLayer(),
                'feeder_fiber': self.combos['feeder_fiber'].currentLayer(),
                'fdh': fdh,
                'house_count_field': hc_field
            }
            
            creator = SLDDataCreator(self._log)
            results = creator.run(params)
            
            self.progress.setValue(70)
            self._log("\n▶ Adding layers to project...")
            
            project_crs = QgsProject.instance().crs()
            
            def add_layer(output, name):
                if isinstance(output, QgsVectorLayer):
                    layer = output
                else:
                    layer = QgsVectorLayer(str(output), name, "ogr")
                if not layer.crs().isValid():
                    layer.setCrs(project_crs)
                layer.setName(name)
                return layer
            
            grid_layer = add_layer(results['grid'], "Grid")
            fiber_layer = add_layer(results['fiber'], "Fiber_Cable")
            equip_layer = add_layer(results['equipment'], "Equipment")
            
            # Apply styles
            if self.chk_style.isChecked():
                self._log("▶ Applying styles...")
                self._apply_style_or_default(grid_layer, self.style_data_grid, LayerStyler.style_grid_layer)
                self._apply_style_or_default(fiber_layer, self.style_data_fiber, LayerStyler.style_fiber_layer)
                self._apply_style_or_default(equip_layer, self.style_data_equipment, LayerStyler.style_equipment_layer)
            
            QgsProject.instance().addMapLayer(grid_layer)
            QgsProject.instance().addMapLayer(fiber_layer)
            QgsProject.instance().addMapLayer(equip_layer)
            
            self._log("  ✓ Grid layer added")
            self._log("  ✓ Fiber_Cable layer added")
            self._log("  ✓ Equipment layer added")
            
            self.progress.setValue(80)
            
            # Daisy Chain
            if self.chk_daisy.isChecked():
                calc = DaisyChainCalculator(self._log)
                calc.run(equip_layer, fiber_layer)
            
            self.progress.setValue(100)
            self.status.setText("✅ Data Creation Complete!")
            
            self._log("\n" + "="*50)
            self._log("✅ DATA CREATION COMPLETE!")
            self._log("="*50)
            
            QMessageBox.information(self, "Success", 
                "Data layers created successfully!\n\n"
                "• Grid\n• Fiber_Cable\n• Equipment\n\n"
                "Now you can generate SLD Layout in Tab 2.")
            
        except Exception as e:
            import traceback
            self._log(f"\n❌ ERROR: {e}")
            self._log(traceback.format_exc())
            self.status.setText("❌ Error occurred")
            QMessageBox.critical(self, "Error", str(e))
        
        finally:
            self.btn_create.setEnabled(True)
            self.btn_sld.setEnabled(True)
    
    def _run_sld_layout(self):
        """Run SLD layout generation"""
        fiber = self.cbo_fiber.currentLayer()
        equip = self.cbo_equip.currentLayer()
        
        if not fiber:
            QMessageBox.warning(self, "Error", "Please select Fiber layer (Fiber_Cable)")
            return
        
        self.btn_create.setEnabled(False)
        self.btn_sld.setEnabled(False)
        self.progress.setValue(0)
        self.status.setText("Generating SLD Layout...")
        self.log.clear()
        
        try:
            self._log("="*50)
            self._log("SLD LAYOUT GENERATION (Fixed Bus Routing)")
            self._log("="*50)
            
            generator = SLDLayoutGenerator(self._log)
            
            # Get style file paths
            point_style = self.style_sld_equipment.get_style_path()
            line_style = self.style_sld_fiber.get_style_path()
            grid_style = self.style_sld_grid.get_style_path()
            
            point_layer, line_layer, grid_layer = generator.generate(
                fiber_layer=fiber,
                equip_layer=equip,
                from_field='FROM',
                to_field='TO',
                eq_key_field='ELEMENTID' if equip else None,
                eq_label_field='Callouts' if equip else None,
                fb_label_field='F_Call',
                x_spacing=self.spn_x.value(),
                y_spacing=self.spn_y.value(),
                draw_grid=self.chk_grid.isChecked(),
                point_style_file=point_style,
                line_style_file=line_style,
                grid_style_file=grid_style
            )
            
            if point_layer:
                QgsProject.instance().addMapLayer(grid_layer)
                QgsProject.instance().addMapLayer(line_layer)
                QgsProject.instance().addMapLayer(point_layer)
                
                self.progress.setValue(100)
                self.status.setText("✅ SLD Layout Complete!")
                
                self._log("\n" + "="*50)
                self._log("✅ SLD LAYOUT COMPLETE!")
                self._log("="*50)
                self._log("\nRouting Method: PROPER BUS-BASED")
                self._log("  • Vertical lines from parent to bus")
                self._log("  • Horizontal bus connecting children")
                self._log("  • Vertical lines from bus to children")
                self._log("  • NO diagonal lines or bends!")
                
                QMessageBox.information(self, "Success",
                    f"SLD Layout created successfully!\n\n"
                    f"• SLD_Equipment: {point_layer.featureCount()} nodes\n"
                    f"• SLD_Fiber: {line_layer.featureCount()} segments\n"
                    f"• SLD_Grid\n\n"
                    f"Routing: Proper bus-based (no bends)")
            else:
                QMessageBox.warning(self, "Error", "Failed to generate SLD layout")
        
        except Exception as e:
            import traceback
            self._log(f"\n❌ ERROR: {e}")
            self._log(traceback.format_exc())
            self.status.setText("❌ Error occurred")
            QMessageBox.critical(self, "Error", str(e))
        
        finally:
            self.btn_create.setEnabled(True)
            self.btn_sld.setEnabled(True)


# =============================================================================
# RUN APPLICATION
# =============================================================================

dialog = SLDWizardDialog(iface.mainWindow())
dialog.show()