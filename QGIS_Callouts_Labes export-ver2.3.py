"""
FTTH CD Processor - v3.2 NO CRASH
+ Stable processing (no startEditing/commitChanges)
+ Drop conduits (1-WAY & 2-WAY) inherit RT prefix from distribution at DAP
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
    QgsPointXY
)
import processing
import csv
import os
import re
import math
from datetime import datetime

class FTTHProcessorV32(QgsProcessingAlgorithm):
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
            self.DAP, 'DAP Layer', [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.TOBY, 'TOBY Layer', [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.HANDHOLE, 'Handhole Layer', [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.DIST_CONDUIT, 'Distribution Conduit Layer', [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.DROP_CONDUIT, 'Drop Conduit Layer', [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.ADDRESSES, 'Hexatronics Addresses', [QgsProcessing.TypeVectorPoint], optional=True))
        self.addParameter(QgsProcessingParameterVectorLayer(
            self.HOUSE_DROPS, 'Hexatronics House Drops', [QgsProcessing.TypeVectorLine], optional=True))
        self.addParameter(QgsProcessingParameterCrs(self.CRS, 'Output CRS', 'EPSG:2236'))
        downloads = os.path.join(os.path.expanduser('~'), 'Downloads')
        self.addParameter(QgsProcessingParameterFileDestination(
            self.OUTPUT, 'Output CSV', 'CSV Files (*.csv)', 
            defaultValue=os.path.join(downloads, 'FTTH_Annotations.csv')))

    def reproject(self, layer, crs, context, feedback):
        if not layer:
            return None
        if layer.crs() == crs:
            return layer
        try:
            return processing.run("native:reprojectlayer", {
                'INPUT': layer, 'TARGET_CRS': crs, 'OUTPUT': 'memory:'
            }, context=context, feedback=feedback)['OUTPUT']
        except:
            return layer

    def feet_to_units(self, crs, ft):
        try:
            from qgis.core import QgsUnitTypes
            if crs.mapUnits() == QgsUnitTypes.DistanceFeet:
                return ft
            elif crs.mapUnits() == QgsUnitTypes.DistanceMeters:
                return ft * 0.3048
        except:
            pass
        return ft

    def get_field(self, feat, name):
        try:
            for f in feat.fields().names():
                if f.upper() == name.upper():
                    v = feat[f]
                    return str(v).strip() if v not in (None, '') else ''
        except:
            pass
        return ''

    def get_way(self, feat):
        t = self.get_field(feat, 'TYPE')
        if t:
            t = t.upper()
            if '1-WAY' in t or '1WAY' in t or '1 WAY' in t:
                return 1
            if '2-WAY' in t or '2WAY' in t or '2 WAY' in t:
                return 2
            m = re.search(r'(\d+)', t)
            if m:
                return int(m.group(1))
        return None

    def get_prefix(self, feat):
        e = self.get_field(feat, 'ELEMENTID') or self.get_field(feat, 'ELEMENT_ID')
        return e[:3] if e and len(e) >= 3 else 'RT1'

    def get_endpoints(self, geom):
        try:
            if geom.isMultipart():
                pts = geom.asMultiPolyline()
                eps = []
                for p in pts:
                    if p and len(p) >= 2:
                        eps.extend([QgsPointXY(p[0]), QgsPointXY(p[-1])])
                return eps
            else:
                pts = geom.asPolyline()
                if pts and len(pts) >= 2:
                    return [QgsPointXY(pts[0]), QgsPointXY(pts[-1])]
        except:
            pass
        return []

    def seg_len_ft(self, crs, geom):
        try:
            from qgis.core import QgsUnitTypes
            l = geom.length()
            if crs.mapUnits() == QgsUnitTypes.DistanceFeet:
                return int(round(l))
            elif crs.mapUnits() == QgsUnitTypes.DistanceMeters:
                return int(round(l * 3.28084))
        except:
            pass
        return int(round(geom.length()))

    def processAlgorithm(self, parameters, context, feedback):
        results = []
        
        # Get inputs
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
            output_file = f"{base}_{datetime.now().strftime('%Y%m%d_%H%M%S')}{ext}"

        feedback.pushInfo("=" * 50)
        feedback.pushInfo(" FTTH Processor v3.2 - NO CRASH")
        feedback.pushInfo("=" * 50)

        wgs84 = QgsCoordinateReferenceSystem('EPSG:4326')
        to_wgs84 = QgsCoordinateTransform(output_crs, wgs84, QgsProject.instance())
        
        snap_tol = self.feet_to_units(output_crs, 50.0)  # 50 feet snap
        cross_len = self.feet_to_units(output_crs, 5.0)

        # Reproject
        feedback.pushInfo(" Reprojecting...")
        dap = self.reproject(dap_orig, output_crs, context, feedback)
        toby = self.reproject(toby_orig, output_crs, context, feedback)
        hh = self.reproject(hh_orig, output_crs, context, feedback)
        dist = self.reproject(dist_orig, output_crs, context, feedback)
        drop = self.reproject(drop_orig, output_crs, context, feedback)
        addr = self.reproject(addr_orig, output_crs, context, feedback)
        hdl = self.reproject(hdrops_orig, output_crs, context, feedback)

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # Build DAP cache
        dap_cache = {}
        if dap:
            for f in dap.getFeatures():
                g = f.geometry()
                if g and not g.isEmpty():
                    dap_cache[f.id()] = {'pt': g.asPoint(), 'feat': f}
        feedback.pushInfo(f" DAPs: {len(dap_cache)}")

        # Build DAP -> Distribution prefix mapping
        dap_prefix_map = {}
        if dist and dap_cache:
            feedback.pushInfo(" Mapping distributions to DAPs...")
            for df in dist.getFeatures():
                g = df.geometry()
                if not g or g.isEmpty():
                    continue
                way = self.get_way(df)
                prefix = self.get_prefix(df)
                if not way:
                    continue
                eps = self.get_endpoints(g)
                for ep in eps:
                    for did, dinfo in dap_cache.items():
                        d = math.hypot(ep.x() - dinfo['pt'].x(), ep.y() - dinfo['pt'].y())
                        if d <= snap_tol:
                            if did not in dap_prefix_map or way > dap_prefix_map[did]['way']:
                                dap_prefix_map[did] = {'prefix': prefix, 'way': way}
            feedback.pushInfo(f"   DAPs with distribution: {len(dap_prefix_map)}")

        # Build DROP -> inherited prefix mapping (BEFORE splitting)
        drop_prefix_map = {}  # drop feature id -> prefix
        if drop and dap_cache and dap_prefix_map:
            feedback.pushInfo(" Mapping drops to distributions...")
            tagged = 0
            for df in drop.getFeatures():
                g = df.geometry()
                if not g or g.isEmpty():
                    continue
                eps = self.get_endpoints(g)
                for ep in eps:
                    for did, dinfo in dap_cache.items():
                        d = math.hypot(ep.x() - dinfo['pt'].x(), ep.y() - dinfo['pt'].y())
                        if d <= snap_tol and did in dap_prefix_map:
                            drop_prefix_map[df.id()] = dap_prefix_map[did]['prefix']
                            tagged += 1
                            break
                    if df.id() in drop_prefix_map:
                        break
            feedback.pushInfo(f"   Drops tagged: {tagged}")

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # TOBY Address mapping
        add_addr_map = {}
        if addr and hdl and toby:
            try:
                feedback.pushInfo(" Building TOBY addresses...")
                jb = processing.run('native:joinbynearest', {
                    'DISCARD_NONMATCHING': False,
                    'FIELDS_TO_COPY': ['FullAddr', 'address_nu', 'street_nam'],
                    'INPUT': hdl, 'INPUT_2': addr,
                    'MAX_DISTANCE': None, 'NEIGHBORS': 1, 'PREFIX': '',
                    'OUTPUT': 'memory:'
                }, context=context, feedback=feedback)
                hd_joined = jb['OUTPUT']
                
                idx = QgsSpatialIndex(hd_joined.getFeatures())
                hd_cache = {f.id(): f for f in hd_joined.getFeatures()}
                
                for tf in toby.getFeatures():
                    tg = tf.geometry()
                    if not tg or tg.isEmpty():
                        continue
                    cands = idx.intersects(tg.boundingBox())
                    nums, street = [], None
                    for cid in cands:
                        hf = hd_cache.get(cid)
                        if not hf:
                            continue
                        hg = hf.geometry()
                        if not hg or not hg.intersects(tg):
                            continue
                        an = self.get_field(hf, 'address_nu')
                        sn = self.get_field(hf, 'street_nam')
                        if an and an not in nums:
                            nums.append(an)
                        if not street and sn:
                            street = sn
                    if nums and street:
                        add_addr_map[tf.id()] = f"{' & '.join(nums)} {street}".upper()
                    elif street:
                        add_addr_map[tf.id()] = street.upper()
                    elif nums:
                        add_addr_map[tf.id()] = ' & '.join(nums).upper()
                feedback.pushInfo(f"   TOBY addresses: {len(add_addr_map)}")
            except Exception as e:
                feedback.pushInfo(f"   Address error: {str(e)}")

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # DAP Labels
        dap_pts = []
        if dap:
            feedback.pushInfo(f" Processing DAPs...")
            for f in dap.getFeatures():
                g = f.geometry()
                if not g or g.isEmpty():
                    continue
                pt = g.asPoint()
                eid = self.get_field(f, 'ELEMENTID') or self.get_field(f, 'ELEMENT_ID')
                suffix = eid[-6:].replace('-', '') if eid else 'XXXXXX'
                text = "\\P".join(["DIG 4'X4' PIT", f"{suffix}-2-WAY", "STA."])
                dap_pts.append((pt.x(), pt.y()))
                results.append({
                    'Type': 'DAP', 'ElementID': eid,
                    'X': round(pt.x(), 2), 'Y': round(pt.y(), 2),
                    'Lat': None, 'Lon': None,
                    'Text': text, 'Layer': 'DAP_LABEL'
                })

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # Build cross lines for splitting
        def make_cross(layer):
            if not layer or layer.featureCount() == 0:
                return None
            try:
                expr = f"make_line(project($geometry,{cross_len},radians(45)),project($geometry,{cross_len},radians(225)))"
                return processing.run('native:geometrybyexpression', {
                    'EXPRESSION': expr, 'INPUT': layer,
                    'OUTPUT_GEOMETRY': 1, 'WITH_M': False, 'WITH_Z': False,
                    'OUTPUT': 'memory:'
                }, context=context, feedback=feedback)['OUTPUT']
            except:
                return None

        splits = [l for l in [make_cross(dap), make_cross(toby), make_cross(hh)] if l]

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # Process conduits
        if dist or drop:
            feedback.pushInfo(" Processing conduits...")
            
            # Store original drop info before merging
            drop_info = {}  # Will map by geometry centroid
            if drop:
                for df in drop.getFeatures():
                    g = df.geometry()
                    if g and not g.isEmpty():
                        # Use centroid as key
                        c = g.centroid().asPoint()
                        key = (round(c.x(), 1), round(c.y(), 1))
                        way = self.get_way(df)
                        prefix = drop_prefix_map.get(df.id(), self.get_prefix(df))
                        drop_info[key] = {'way': way, 'prefix': prefix}

            # Helper to refactor fields (remove DEPLOYMENT and other problematic columns)
            def safe_refactor(layer, source_val, ctx, fb):
                if not layer:
                    return None
                try:
                    names = [f.name().upper() for f in layer.fields()]
                    
                    # Find fields case-insensitive
                    def find_f(n):
                        for f in layer.fields():
                            if f.name().upper() == n.upper():
                                return f.name()
                        return None
                    
                    type_f = find_f('TYPE')
                    elem_f = find_f('ELEMENTID') or find_f('ELEMENT_ID')
                    
                    # Build minimal field mapping - only keep TYPE, ELEMENTID, SOURCE
                    mapping = [
                        {'name': 'TYPE', 'type': 10, 'length': 100, 'precision': 0,
                         'expression': f'"{type_f}"' if type_f else "''"},
                        {'name': 'ELEMENTID', 'type': 10, 'length': 100, 'precision': 0,
                         'expression': f'"{elem_f}"' if elem_f else "''"},
                        {'name': 'SOURCE', 'type': 10, 'length': 10, 'precision': 0,
                         'expression': f"'{source_val}'"}
                    ]
                    
                    result = processing.run('native:refactorfields', {
                        'INPUT': layer, 'FIELDS_MAPPING': mapping, 'OUTPUT': 'memory:'
                    }, context=ctx, feedback=fb)['OUTPUT']
                    
                    result = processing.run('native:fixgeometries', {
                        'INPUT': result, 'METHOD': 0, 'OUTPUT': 'memory:'
                    }, context=ctx, feedback=fb)['OUTPUT']
                    
                    return result
                except Exception as e:
                    fb.pushInfo(f"   Refactor error: {str(e)}")
                    return None

            # Merge and split conduits
            layers = []
            if dist:
                fd = safe_refactor(dist, 'DIST', context, feedback)
                if fd:
                    layers.append(fd)
            
            if drop:
                fd = safe_refactor(drop, 'DROP', context, feedback)
                if fd:
                    layers.append(fd)

            if layers:
                merged = processing.run('native:mergevectorlayers', {
                    'CRS': output_crs, 'LAYERS': layers, 'OUTPUT': 'memory:'
                }, context=context, feedback=feedback)['OUTPUT']

                # Split at nodes
                final_segs = merged
                if splits:
                    try:
                        msplits = processing.run('native:mergevectorlayers', {
                            'CRS': output_crs, 'LAYERS': splits, 'OUTPUT': 'memory:'
                        }, context=context, feedback=feedback)['OUTPUT']
                        
                        final_segs = processing.run('native:splitwithlines', {
                            'INPUT': merged, 'LINES': msplits, 'OUTPUT': 'memory:'
                        }, context=context, feedback=feedback)['OUTPUT']
                    except:
                        pass

                if feedback.isCanceled():
                    return {self.OUTPUT: None}

                # Build segment labels
                feedback.pushInfo(" Building segment labels...")
                groups = {}
                
                for f in final_segs.getFeatures():
                    g = f.geometry()
                    if not g or g.isEmpty() or g.length() < 2:
                        continue
                    
                    mp = g.interpolate(g.length() / 2.0)
                    if mp.isEmpty():
                        continue
                    pt = mp.asPoint()
                    seg_len = self.seg_len_ft(output_crs, g)
                    
                    # Determine label
                    source = self.get_field(f, 'SOURCE').upper()
                    own_way = self.get_way(f)
                    own_prefix = self.get_prefix(f)
                    
                    if source == 'DROP':
                        # Find inherited prefix from original drop
                        c = g.centroid().asPoint()
                        best_match = None
                        best_dist = 9999
                        for key, info in drop_info.items():
                            d = math.hypot(c.x() - key[0], c.y() - key[1])
                            if d < best_dist:
                                best_dist = d
                                best_match = info
                        
                        if best_match:
                            prefix = best_match['prefix']
                            way = best_match['way'] if best_match['way'] else (own_way if own_way else 1)
                        else:
                            prefix = own_prefix
                            way = own_way if own_way else 1
                        
                        label = f"{prefix}-HEX-{way}-WAY"
                    else:
                        # Distribution
                        if own_way:
                            label = f"{own_prefix}-HEX-{own_way}-WAY"
                        else:
                            label = f"{own_prefix}-HEX-CONDUIT"
                    
                    # Group by location
                    key = (round(pt.x(), 1), round(pt.y(), 1))
                    if key not in groups:
                        try:
                            wpt = to_wgs84.transform(pt)
                            lat, lon = round(wpt.y(), 6), round(wpt.x(), 6)
                        except:
                            lat, lon = None, None
                        groups[key] = {
                            'x': pt.x(), 'y': pt.y(),
                            'lat': lat, 'lon': lon,
                            'len': seg_len,
                            'labels': set([label])
                        }
                    else:
                        groups[key]['labels'].add(label)
                        if seg_len > groups[key]['len']:
                            groups[key]['len'] = seg_len

                # Output labels
                for k, grp in groups.items():
                    lbls = sorted(list(grp['labels']))
                    txt = "\\P".join([f"BORE & PLACE ({grp['len']}')"] + lbls)
                    results.append({
                        'Type': 'CONDUIT_SEG', 'ElementID': '',
                        'X': round(grp['x'], 2), 'Y': round(grp['y'], 2),
                        'Lat': grp['lat'], 'Lon': grp['lon'],
                        'Text': txt, 'Layer': 'BORE_LABEL'
                    })
                
                feedback.pushInfo(f"   Segment groups: {len(groups)}")

        if feedback.isCanceled():
            return {self.OUTPUT: None}

        # TOBY Labels
        if toby:
            feedback.pushInfo(f" Processing TOBYs...")
            hh_pts = []
            if hh:
                for f in hh.getFeatures():
                    g = f.geometry()
                    if g and not g.isEmpty():
                        hh_pts.append(g.asPoint())
            
            min_gap = self.feet_to_units(output_crs, 5.0)
            
            for f in toby.getFeatures():
                g = f.geometry()
                if not g or g.isEmpty():
                    continue
                pt = g.asPoint()
                eid = self.get_field(f, 'ELEMENTID') or self.get_field(f, 'ELEMENT_ID')
                addr = add_addr_map.get(f.id(), "ADDRESS")
                
                # Check if near DAP or HH
                dig = True
                for did, dinfo in dap_cache.items():
                    if math.hypot(pt.x() - dinfo['pt'].x(), pt.y() - dinfo['pt'].y()) <= snap_tol:
                        dig = False
                        break
                if dig:
                    for hp in hh_pts:
                        if math.hypot(pt.x() - hp.x(), pt.y() - hp.y()) <= snap_tol:
                            dig = False
                            break
                
                lines = []
                if dig:
                    lines.append("DIG 4'X4' PIT")
                lines.extend(["PL.NEW TOBY BOX IN", "4'X4' PIT SERVES", addr, "GPS:", "STA."])
                
                x, y = pt.x(), pt.y()
                if dap_pts:
                    nd = min((math.hypot(x-dx, y-dy), dx, dy) for dx, dy in dap_pts)
                    if nd[0] < min_gap and nd[0] > 0:
                        dx = (x - nd[1]) / nd[0]
                        dy = (y - nd[2]) / nd[0]
                        x += dx * (min_gap - nd[0])
                        y += dy * (min_gap - nd[0])
                
                results.append({
                    'Type': 'TOBY', 'ElementID': eid,
                    'X': round(x, 2), 'Y': round(y, 2),
                    'Lat': None, 'Lon': None,
                    'Text': "\\P".join(lines), 'Layer': 'TOBY_LABEL'
                })

        # Write CSV
        feedback.pushInfo(" Writing CSV...")
        try:
            with open(output_file, 'w', newline='', encoding='utf-8') as f:
                w = csv.DictWriter(f, fieldnames=['Type', 'ElementID', 'X', 'Y', 'Lat', 'Lon', 'Text', 'Layer'])
                w.writeheader()
                w.writerows(results)
            feedback.pushInfo(f" Saved: {output_file}")
        except Exception as e:
            feedback.pushInfo(f" CSV Error: {str(e)}")

        # Summary
        feedback.pushInfo("\n" + "=" * 50)
        feedback.pushInfo(f" DAP: {len([r for r in results if r['Type']=='DAP'])}")
        feedback.pushInfo(f" TOBY: {len([r for r in results if r['Type']=='TOBY'])}")
        feedback.pushInfo(f" CONDUIT: {len([r for r in results if r['Type']=='CONDUIT_SEG'])}")
        feedback.pushInfo(f" TOTAL: {len(results)}")
        feedback.pushInfo("=" * 50)

        return {self.OUTPUT: output_file}

    def name(self):
        return 'ftth_v32'
    def displayName(self):
        return 'FTTH Processor v3.2 (No Crash)'
    def group(self):
        return 'FTTH Tools'
    def groupId(self):
        return 'ftth_tools'
    def shortHelpString(self):
        return 'v3.2: Stable version. 1-WAY and 2-WAY drops inherit RT prefix from distribution at DAP.'
    def createInstance(self):
        return FTTHProcessorV32()

from qgis.core import QgsApplication
QgsApplication.processingRegistry().addAlgorithmAlias('ftth_v32', 'ftth_v32')
print("\n" + "=" * 50)
print(" FTTH Processor v3.2 LOADED - NO CRASH VERSION")
print(" 1-WAY and 2-WAY drops inherit prefix from distribution")
print(" Processing > Toolbox > FTTH Tools")
print("=" * 50)