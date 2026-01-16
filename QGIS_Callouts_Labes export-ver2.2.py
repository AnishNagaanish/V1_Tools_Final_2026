"""
FTTH CD Processor - v3.1 STABLE
+ Better error handling to prevent crashes
+ Memory management improvements
+ Drop conduits inherit RT prefix from distribution at DAP
+ Case-insensitive TYPE/ELEMENTID field handling
----------------------------------------------------------------
Paste in QGIS Python Console and press Enter.
"""
from qgis.core import (
    QgsProcessing,
    QgsProcessingAlgorithm,
    QgsProcessingParameterVectorLayer,
    QgsProcessingParameterFileDestination,
    QgsProcessingParameterCrs,
    QgsCoordinateReferenceSystem,
    QgsCoordinateTransform,
    QgsSpatialIndex,
    QgsProject,
    QgsPointXY,
    QgsGeometry,
    QgsFeature
)
import processing
import csv
import os
import re
import math
from datetime import datetime

class FTTHProcessorV3Stable(QgsProcessingAlgorithm):
    DAP = 'DAP'
    TOBY = 'TOBY'
    HANDHOLE = 'HANDHOLE'
    DIST_CONDUIT = 'DIST_CONDUIT'
    DROP_CONDUIT = 'DROP_CONDUIT'
    ADDRESSES = 'ADDRESSES'
    HOUSE_DROPS = 'HOUSE_DROPS'
    CRS = 'CRS'
    OUTPUT = 'OUTPUT'

    def initAlgorithm(self, config=None):
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.DAP, 'DAP Layer (Duct Access Points)',
            [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.TOBY, 'TOBY Layer',
            [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.HANDHOLE, 'Handhole Layer',
            [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.DIST_CONDUIT, 'Distribution Conduit Layer',
            [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.DROP_CONDUIT, 'Drop Conduit Layer',
            [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.ADDRESSES, 'Hexatronics Addresses (points)',
            [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.HOUSE_DROPS, 'Hexatronics House Drops (lines)',
            [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterCrs(
            self.CRS, 'Output Coordinate System', 'EPSG:2236'))
        downloads_folder = os.path.join(os.path.expanduser('~'), 'Downloads')
        default_output = os.path.join(downloads_folder, 'FTTH_Annotations.csv')
        self.addParameter(QgsProcessingParameterFileDestination(
            self.OUTPUT, 'Output CSV File',
            'CSV Files (*.csv)', defaultValue=default_output))

    def reproject_layer(self, layer, target_crs, context, feedback):
        if layer is None:
            return None
        try:
            if layer.crs() == target_crs:
                return layer
            result = processing.run("native:reprojectlayer", {
                'INPUT': layer, 'TARGET_CRS': target_crs, 'OUTPUT': 'memory:'
            }, context=context, feedback=feedback)
            return result['OUTPUT']
        except Exception as e:
            feedback.pushInfo(f" WARNING: Reproject failed: {str(e)}")
            return layer

    def feet_to_crs_units(self, output_crs, dist_ft, feedback=None):
        try:
            from qgis.core import QgsUnitTypes
            units = output_crs.mapUnits()
            if units == QgsUnitTypes.DistanceFeet:
                return dist_ft
            elif units == QgsUnitTypes.DistanceMeters:
                return dist_ft * 0.3048
        except:
            pass
        return dist_ft

    def build_text(self, lines):
        return "\\P".join(lines)

    def get_field_value(self, feat, field_name):
        """Get field value - case insensitive, with error handling."""
        try:
            names = feat.fields().names()
            for f in names:
                if f.upper() == field_name.upper():
                    val = feat[f]
                    if val is not None and str(val).strip() != '':
                        return str(val).strip()
            return ''
        except:
            return ''

    def extract_way_count(self, feat):
        """Extract way count from TYPE field."""
        try:
            feat_type = self.get_field_value(feat, 'TYPE')
            if feat_type:
                m = re.search(r'(\d+)\s*-?\s*W(?:AY)?', feat_type, re.IGNORECASE)
                if m:
                    return int(m.group(1))
                m2 = re.search(r'(\d+)', feat_type)
                if m2:
                    return int(m2.group(1))
        except:
            pass
        return None

    def get_element_prefix(self, feat):
        """Get RTx prefix from ELEMENTID."""
        try:
            elem_id = self.get_field_value(feat, 'ELEMENTID')
            if not elem_id:
                elem_id = self.get_field_value(feat, 'ELEMENT_ID')
            if elem_id and len(elem_id) >= 3:
                return elem_id[:3]
        except:
            pass
        return 'RT1'

    def get_line_endpoints(self, geom):
        """Safely get endpoints of a line geometry."""
        try:
            if geom is None or geom.isEmpty():
                return []
            if geom.isMultipart():
                pts = geom.asMultiPolyline()
                endpoints = []
                for part in pts:
                    if part and len(part) >= 2:
                        endpoints.append(QgsPointXY(part[0]))
                        endpoints.append(QgsPointXY(part[-1]))
                return endpoints
            else:
                pts = geom.asPolyline()
                if pts and len(pts) >= 2:
                    return [QgsPointXY(pts[0]), QgsPointXY(pts[-1])]
        except:
            pass
        return []

    def build_dap_dist_map(self, dap_layer, dist_layer, snap_tol, feedback):
        """Build mapping: DAP ID -> {prefix, way} from distribution conduits."""
        dap_dist_map = {}
        if not dap_layer or not dist_layer:
            return dap_dist_map
        
        try:
            feedback.pushInfo(" Building DAP -> Distribution mapping...")
            
            # Cache DAP points
            dap_cache = {}
            for f in dap_layer.getFeatures():
                try:
                    g = f.geometry()
                    if g and not g.isEmpty():
                        dap_cache[f.id()] = g.asPoint()
                except:
                    continue
            
            # Check each distribution conduit
            for dist_f in dist_layer.getFeatures():
                try:
                    g = dist_f.geometry()
                    if g is None or g.isEmpty():
                        continue
                    
                    way = self.extract_way_count(dist_f)
                    prefix = self.get_element_prefix(dist_f)
                    
                    if not way:
                        continue
                    
                    endpoints = self.get_line_endpoints(g)
                    
                    for ep in endpoints:
                        for dap_id, dap_pt in dap_cache.items():
                            try:
                                dist = math.hypot(ep.x() - dap_pt.x(), ep.y() - dap_pt.y())
                                if dist <= snap_tol:
                                    if dap_id not in dap_dist_map or way > dap_dist_map[dap_id]['way']:
                                        dap_dist_map[dap_id] = {'way': way, 'prefix': prefix}
                            except:
                                continue
                except:
                    continue
            
            feedback.pushInfo(f"   Found {len(dap_dist_map)} DAP(s) with distribution connections")
        except Exception as e:
            feedback.pushInfo(f" WARNING: DAP mapping error: {str(e)}")
        
        return dap_dist_map

    def find_nearest_dap_id(self, point, dap_cache, snap_tol):
        """Find nearest DAP ID within snap tolerance."""
        try:
            nearest_id = None
            nearest_dist = snap_tol + 1
            for dap_id, dap_pt in dap_cache.items():
                d = math.hypot(point.x() - dap_pt.x(), point.y() - dap_pt.y())
                if d < nearest_dist:
                    nearest_dist = d
                    nearest_id = dap_id
            return nearest_id if nearest_dist <= snap_tol else None
        except:
            return None

    def get_conduit_label(self, feat, dap_cache, dap_dist_map, snap_tol, is_drop=False):
        """Build RTx-HEX-<way>-WAY label. Drops inherit prefix from distribution."""
        try:
            own_prefix = self.get_element_prefix(feat)
            own_way = self.extract_way_count(feat)
            
            if is_drop and dap_cache and dap_dist_map:
                g = feat.geometry()
                endpoints = self.get_line_endpoints(g)
                
                for ep in endpoints:
                    dap_id = self.find_nearest_dap_id(ep, dap_cache, snap_tol)
                    if dap_id and dap_id in dap_dist_map:
                        inherited_prefix = dap_dist_map[dap_id]['prefix']
                        drop_way = own_way if own_way else 1
                        return f"{inherited_prefix}-HEX-{drop_way}-WAY"
            
            if own_way:
                return f"{own_prefix}-HEX-{own_way}-WAY"
            return f"{own_prefix}-HEX-CONDUIT"
        except:
            return "RT1-HEX-CONDUIT"

    def toby_address_map(self, toby_layer, hd_with_addr, feedback):
        """Build TOBY ID -> address mapping."""
        m = {}
        if not toby_layer or not hd_with_addr:
            return m
        try:
            idx = QgsSpatialIndex(hd_with_addr.getFeatures())
            cache = {f.id(): f for f in hd_with_addr.getFeatures()}
            
            for t in toby_layer.getFeatures():
                try:
                    tg = t.geometry()
                    if tg is None or tg.isEmpty():
                        continue
                    cand = idx.intersects(tg.boundingBox())
                    nums = []
                    street = None
                    seen = set()
                    
                    for cid in cand:
                        hf = cache.get(cid)
                        if not hf:
                            continue
                        hg = hf.geometry()
                        if not hg or hg.isEmpty() or not hg.intersects(tg):
                            continue
                        a_num = self.get_field_value(hf, 'address_nu')
                        s_name = self.get_field_value(hf, 'street_nam')
                        if a_num and a_num not in seen:
                            nums.append(a_num)
                            seen.add(a_num)
                        if street is None and s_name:
                            street = s_name
                    
                    if nums and street:
                        addr = f"{' & '.join(nums)} {street}"
                    elif street:
                        addr = street
                    elif nums:
                        addr = ' & '.join(nums)
                    else:
                        addr = ""
                    m[t.id()] = addr.upper() if addr else "ADDRESS"
                except:
                    continue
            feedback.pushInfo(f" Built addresses for {len(m)} TOBY(s)")
        except Exception as e:
            feedback.pushInfo(f" WARNING: Address mapping error: {str(e)}")
        return m

    def segment_length_feet(self, output_crs, geom):
        """Compute segment length in feet."""
        try:
            from qgis.core import QgsUnitTypes
            length = geom.length()
            units = output_crs.mapUnits()
            if units == QgsUnitTypes.DistanceFeet:
                return int(round(length))
            elif units == QgsUnitTypes.DistanceMeters:
                return int(round(length * 3.28084))
        except:
            pass
        return int(round(geom.length()))

    def safe_refactor(self, layer, context, feedback):
        """Refactor layer to standard fields."""
        if not layer:
            return None
        try:
            names = [f.name() for f in layer.fields()]
            
            def find_field(target):
                for n in names:
                    if n.upper() == target.upper():
                        return n
                return None

            type_fld = find_field('TYPE')
            elem_fld = find_field('ELEMENTID') or find_field('ELEMENT_ID')

            expr_type = f'coalesce("{type_fld}", \'\')' if type_fld else '\'\''
            expr_elem = f'coalesce("{elem_fld}", \'\')' if elem_fld else '\'\''

            mapping = [
                {'name': 'TYPE', 'type': 10, 'length': 254, 'precision': 0, 'expression': expr_type},
                {'name': 'ELEMENTID', 'type': 10, 'length': 254, 'precision': 0, 'expression': expr_elem},
            ]
            out = processing.run('native:refactorfields', {
                'INPUT': layer, 'FIELDS_MAPPING': mapping, 'OUTPUT': 'memory:'
            }, context=context, feedback=feedback)['OUTPUT']
            return out
        except Exception as e:
            feedback.pushInfo(f" WARNING: Refactor failed: {str(e)}")
            return layer

    def add_source_field(self, layer, source_value, context, feedback):
        """Add SOURCE field to identify DIST vs DROP."""
        if not layer:
            return None
        try:
            out = processing.run('native:fieldcalculator', {
                'INPUT': layer,
                'FIELD_NAME': 'SOURCE',
                'FIELD_TYPE': 2,
                'FIELD_LENGTH': 10,
                'FORMULA': f"'{source_value}'",
                'OUTPUT': 'memory:'
            }, context=context, feedback=feedback)['OUTPUT']
            return out
        except Exception as e:
            feedback.pushInfo(f" WARNING: Add source field failed: {str(e)}")
            return layer

    def processAlgorithm(self, parameters, context, feedback):
        results = []
        
        try:
            # Get parameters
            dap_orig = self.parameterAsVectorLayer(parameters, self.DAP, context)
            toby_orig = self.parameterAsVectorLayer(parameters, self.TOBY, context)
            hh_orig = self.parameterAsVectorLayer(parameters, self.HANDHOLE, context)
            dist_orig = self.parameterAsVectorLayer(parameters, self.DIST_CONDUIT, context)
            drop_orig = self.parameterAsVectorLayer(parameters, self.DROP_CONDUIT, context)
            addr_orig = self.parameterAsVectorLayer(parameters, self.ADDRESSES, context)
            hdrops_orig = self.parameterAsVectorLayer(parameters, self.HOUSE_DROPS, context)

            output_crs = self.parameterAsCrs(parameters, self.CRS, context)
            output_file = self.parameterAsString(parameters, self.OUTPUT, context)

            if output_file:
                base, ext = os.path.splitext(output_file)
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                output_file = f"{base}_{timestamp}{ext}"

            feedback.pushInfo("=" * 60)
            feedback.pushInfo(" FTTH CD Processor v3.1 STABLE")
            feedback.pushInfo(" - Drop inherits RT prefix from Distribution at DAP")
            feedback.pushInfo(" - Example: RT1-24WAY dist + 2WAY drop = RT1-HEX-2-WAY")
            feedback.pushInfo("=" * 60)

            # Check for cancellation
            if feedback.isCanceled():
                return {self.OUTPUT: None}

            wgs84 = QgsCoordinateReferenceSystem('EPSG:4326')
            to_wgs84 = QgsCoordinateTransform(output_crs, wgs84, QgsProject.instance())

            snap_tol = self.feet_to_crs_units(output_crs, 10.0, feedback)
            min_gap = self.feet_to_crs_units(output_crs, 5.0, feedback)
            cross_len = self.feet_to_crs_units(output_crs, 5.0, feedback)

            # Reproject layers
            feedback.pushInfo(" Reprojecting layers...")
            dap = self.reproject_layer(dap_orig, output_crs, context, feedback)
            toby = self.reproject_layer(toby_orig, output_crs, context, feedback)
            hh = self.reproject_layer(hh_orig, output_crs, context, feedback)
            dist = self.reproject_layer(dist_orig, output_crs, context, feedback)
            drop = self.reproject_layer(drop_orig, output_crs, context, feedback)
            addr = self.reproject_layer(addr_orig, output_crs, context, feedback)
            hdl = self.reproject_layer(hdrops_orig, output_crs, context, feedback)

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # Build DAP cache
            dap_cache = {}
            if dap:
                for f in dap.getFeatures():
                    try:
                        g = f.geometry()
                        if g and not g.isEmpty():
                            dap_cache[f.id()] = g.asPoint()
                    except:
                        continue

            # Build DAP -> Distribution mapping
            dap_dist_map = self.build_dap_dist_map(dap, dist, snap_tol, feedback)

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # TOBY Address mapping
            hd_with_addr = None
            add_addr_map = {}
            if addr and hdl and toby:
                try:
                    feedback.pushInfo(" Joining House Drops to Addresses...")
                    jb = processing.run('native:joinbynearest', {
                        'DISCARD_NONMATCHING': False,
                        'FIELDS_TO_COPY': ['FullAddr', 'address_nu', 'street_nam'],
                        'INPUT': hdl, 'INPUT_2': addr,
                        'MAX_DISTANCE': None, 'NEIGHBORS': 1, 'PREFIX': '',
                        'OUTPUT': 'memory:'
                    }, context=context, feedback=feedback)
                    hd_with_addr = jb['OUTPUT']
                    add_addr_map = self.toby_address_map(toby, hd_with_addr, feedback)
                except Exception as e:
                    feedback.pushInfo(f" WARNING: Address join failed: {str(e)}")

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # DAP labels
            dap_label_pts = []
            if dap and dap.featureCount() > 0:
                feedback.pushInfo(f" Processing {dap.featureCount()} DAP features...")
                for f in dap.getFeatures():
                    try:
                        g = f.geometry()
                        if g is None or g.isEmpty():
                            continue
                        pt = g.asPoint()
                        elem_id = self.get_field_value(f, 'ELEMENTID')
                        if not elem_id:
                            elem_id = self.get_field_value(f, 'ELEMENT_ID')
                        suffix = elem_id[-6:].replace('-', '') if elem_id else 'XXXXXX'
                        text = self.build_text(["DIG 4'X4' PIT", f"{suffix}-2-WAY", "STA."])
                        x, y = pt.x(), pt.y()
                        dap_label_pts.append((x, y))
                        results.append({
                            'Type': 'DAP', 'ElementID': elem_id,
                            'X': round(x, 2), 'Y': round(y, 2),
                            'Lat': None, 'Lon': None,
                            'Text': text, 'Layer': 'DAP_LABEL'
                        })
                    except:
                        continue

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # Build cross lines for splitting
            def make_cross_lines(points_layer):
                if not points_layer or points_layer.featureCount() == 0:
                    return None
                try:
                    expr = f"make_line(project($geometry, {cross_len}, radians(45)), project($geometry, {cross_len}, radians(225)))"
                    out = processing.run('native:geometrybyexpression', {
                        'EXPRESSION': expr, 'INPUT': points_layer,
                        'OUTPUT_GEOMETRY': 1, 'WITH_M': False, 'WITH_Z': False,
                        'OUTPUT': 'memory:'
                    }, context=context, feedback=feedback)['OUTPUT']
                    return out
                except:
                    return None

            split_layers = []
            dap_cross = make_cross_lines(dap)
            toby_cross = make_cross_lines(toby)
            hh_cross = make_cross_lines(hh)
            if dap_cross:
                split_layers.append(dap_cross)
            if toby_cross:
                split_layers.append(toby_cross)
            if hh_cross:
                split_layers.append(hh_cross)

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # Prepare conduits
            merged_conduits = None
            if dist or drop:
                feedback.pushInfo(" Processing conduits...")
                layers_to_merge = []
                
                if dist:
                    try:
                        dist_safe = self.safe_refactor(dist, context, feedback)
                        dist_safe = self.add_source_field(dist_safe, 'DIST', context, feedback)
                        fix_dc = processing.run('native:fixgeometries', {
                            'INPUT': dist_safe, 'METHOD': 0, 'OUTPUT': 'memory:'
                        }, context=context, feedback=feedback)['OUTPUT']
                        layers_to_merge.append(fix_dc)
                    except Exception as e:
                        feedback.pushInfo(f" WARNING: Dist processing error: {str(e)}")
                
                if drop:
                    try:
                        drop_safe = self.safe_refactor(drop, context, feedback)
                        drop_safe = self.add_source_field(drop_safe, 'DROP', context, feedback)
                        fix_dp = processing.run('native:fixgeometries', {
                            'INPUT': drop_safe, 'METHOD': 0, 'OUTPUT': 'memory:'
                        }, context=context, feedback=feedback)['OUTPUT']
                        layers_to_merge.append(fix_dp)
                    except Exception as e:
                        feedback.pushInfo(f" WARNING: Drop processing error: {str(e)}")

                if layers_to_merge:
                    try:
                        merged_conduits = processing.run('native:mergevectorlayers', {
                            'CRS': output_crs, 'LAYERS': layers_to_merge, 'OUTPUT': 'memory:'
                        }, context=context, feedback=feedback)['OUTPUT']
                    except Exception as e:
                        feedback.pushInfo(f" WARNING: Merge error: {str(e)}")

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # Split conduits at nodes
            final_segments = None
            if merged_conduits and split_layers:
                try:
                    feedback.pushInfo(" Splitting conduits at nodes...")
                    merged_splits = processing.run('native:mergevectorlayers', {
                        'CRS': output_crs, 'LAYERS': split_layers, 'OUTPUT': 'memory:'
                    }, context=context, feedback=feedback)['OUTPUT']
                    
                    split_result = processing.run('native:splitwithlines', {
                        'INPUT': merged_conduits, 'LINES': merged_splits, 'OUTPUT': 'memory:'
                    }, context=context, feedback=feedback)['OUTPUT']
                    
                    seg_min = self.feet_to_crs_units(output_crs, 2.0, feedback)
                    final_segments = processing.run('native:extractbyexpression', {
                        'INPUT': split_result, 'EXPRESSION': f"length($geometry) > {seg_min}",
                        'OUTPUT': 'memory:'
                    }, context=context, feedback=feedback)['OUTPUT']
                except Exception as e:
                    feedback.pushInfo(f" WARNING: Split error: {str(e)}")
                    final_segments = merged_conduits

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # Build segment callouts
            if final_segments:
                feedback.pushInfo(" Building segment callouts...")
                
                def key_xy(x, y):
                    return (round(x, 2), round(y, 2))

                groups = {}
                count = 0
                
                for f in final_segments.getFeatures():
                    try:
                        g = f.geometry()
                        if g is None or g.isEmpty():
                            continue
                        
                        seg_len = self.segment_length_feet(output_crs, g)
                        mp = g.interpolate(g.length() / 2.0)
                        if mp.isEmpty():
                            continue
                        pt = mp.asPoint()
                        
                        source = self.get_field_value(f, 'SOURCE')
                        is_drop = (source.upper() == 'DROP')
                        
                        cd_line = self.get_conduit_label(f, dap_cache, dap_dist_map, snap_tol, is_drop)
                        
                        k = key_xy(pt.x(), pt.y())
                        if k not in groups:
                            try:
                                wpt = to_wgs84.transform(pt)
                                lat, lon = round(wpt.y(), 6), round(wpt.x(), 6)
                            except:
                                lat, lon = None, None
                            groups[k] = {
                                'x': pt.x(), 'y': pt.y(),
                                'lat': lat, 'lon': lon,
                                'length_ft': seg_len,
                                'labels': set([cd_line])
                            }
                        else:
                            groups[k]['labels'].add(cd_line)
                            if seg_len > groups[k]['length_ft']:
                                groups[k]['length_ft'] = seg_len
                        
                        count += 1
                    except:
                        continue
                
                feedback.pushInfo(f"   Processed {count} segments into {len(groups)} groups")
                
                for k, grp in groups.items():
                    label_lines = sorted(list(grp['labels']))
                    text_lines = [f"BORE & PLACE ({grp['length_ft']}')"] + label_lines
                    text = self.build_text(text_lines)
                    results.append({
                        'Type': 'CONDUIT_SEG', 'ElementID': '',
                        'X': round(grp['x'], 2), 'Y': round(grp['y'], 2),
                        'Lat': grp['lat'], 'Lon': grp['lon'],
                        'Text': text, 'Layer': 'BORE_LABEL'
                    })

            if feedback.isCanceled():
                return {self.OUTPUT: None}

            # TOBY labels
            if toby and toby.featureCount() > 0:
                feedback.pushInfo(f" Processing {toby.featureCount()} TOBY features...")
                
                hh_cache = {}
                if hh:
                    for f in hh.getFeatures():
                        try:
                            g = f.geometry()
                            if g and not g.isEmpty():
                                hh_cache[f.id()] = g.asPoint()
                        except:
                            continue
                
                for f in toby.getFeatures():
                    try:
                        g = f.geometry()
                        if g is None or g.isEmpty():
                            continue
                        pt = g.asPoint()
                        elem_id = self.get_field_value(f, 'ELEMENTID')
                        if not elem_id:
                            elem_id = self.get_field_value(f, 'ELEMENT_ID')
                        add_addr = add_addr_map.get(f.id(), "ADDRESS")

                        # Check if near DAP or HH
                        include_dig = True
                        for dap_id, dap_pt in dap_cache.items():
                            d = math.hypot(pt.x() - dap_pt.x(), pt.y() - dap_pt.y())
                            if d <= snap_tol:
                                include_dig = False
                                break
                        if include_dig:
                            for hh_id, hh_pt in hh_cache.items():
                                d = math.hypot(pt.x() - hh_pt.x(), pt.y() - hh_pt.y())
                                if d <= snap_tol:
                                    include_dig = False
                                    break

                        lines = []
                        if include_dig:
                            lines.append("DIG 4'X4' PIT")
                        lines.extend([
                            "PL.NEW TOBY BOX IN",
                            "4'X4' PIT SERVES",
                            add_addr,
                            "GPS:",
                            "STA."
                        ])
                        text = self.build_text(lines)

                        x, y = pt.x(), pt.y()
                        
                        # Push away from nearest DAP label
                        if dap_label_pts:
                            nearest_d = 1e12
                            nx = ny = None
                            for (dx, dy) in dap_label_pts:
                                d = math.hypot(x - dx, y - dy)
                                if d < nearest_d:
                                    nearest_d, nx, ny = d, dx, dy
                            if nearest_d < min_gap and nearest_d > 0:
                                dirx = (x - nx) / nearest_d
                                diry = (y - ny) / nearest_d
                                push = min_gap - nearest_d
                                x += dirx * push
                                y += diry * push

                        results.append({
                            'Type': 'TOBY', 'ElementID': elem_id,
                            'X': round(x, 2), 'Y': round(y, 2),
                            'Lat': None, 'Lon': None,
                            'Text': text, 'Layer': 'TOBY_LABEL'
                        })
                    except:
                        continue

            # Write CSV
            feedback.pushInfo("\n Writing CSV...")
            try:
                with open(output_file, 'w', newline='', encoding='utf-8') as f:
                    writer = csv.DictWriter(f, fieldnames=['Type', 'ElementID', 'X', 'Y', 'Lat', 'Lon', 'Text', 'Layer'])
                    writer.writeheader()
                    writer.writerows(results)
                feedback.pushInfo(f" Saved: {output_file}")
            except Exception as e:
                feedback.pushInfo(f" ERROR writing CSV: {str(e)}")

            # Summary
            feedback.pushInfo("\n" + "=" * 60)
            feedback.pushInfo("COMPLETE!")
            feedback.pushInfo("=" * 60)
            feedback.pushInfo(f" DAP:          {len([r for r in results if r['Type'] == 'DAP'])}")
            feedback.pushInfo(f" TOBY:         {len([r for r in results if r['Type'] == 'TOBY'])}")
            feedback.pushInfo(f" CONDUIT SEGS: {len([r for r in results if r['Type'] == 'CONDUIT_SEG'])}")
            feedback.pushInfo(f" TOTAL:        {len(results)}")
            feedback.pushInfo("=" * 60)

        except Exception as e:
            feedback.pushInfo(f"\n CRITICAL ERROR: {str(e)}")
            import traceback
            feedback.pushInfo(traceback.format_exc())

        return {self.OUTPUT: output_file}

    def name(self):
        return 'ftth_processor_v3_stable'

    def displayName(self):
        return 'FTTH Processor v3.1 (STABLE)'

    def group(self):
        return 'FTTH Tools'

    def groupId(self):
        return 'ftth_tools'

    def shortHelpString(self):
        return ('v3.1 STABLE: Drop conduits inherit RT PREFIX from distribution at DAP. '
                'Better error handling to prevent crashes. Case-insensitive fields.')

    def createInstance(self):
        return FTTHProcessorV3Stable()


from qgis.core import QgsApplication
QgsApplication.processingRegistry().addAlgorithmAlias('ftth_processor_v3_stable', 'ftth_processor_v3_stable')
print("\n" + "=" * 60)
print(" FTTH Processor v3.1 STABLE LOADED!")
print(" - Better error handling to prevent crashes")
print(" - Drop conduits inherit RT PREFIX from Distribution")
print(" - Example: RT1-24WAY dist + 2WAY drop = RT1-HEX-2-WAY")
print(" Processing > Toolbox > FTTH Tools")
print("=" * 60)