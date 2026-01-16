import os
import traceback
import tempfile
import time

from qgis.PyQt.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton,
    QFileDialog, QLineEdit, QCheckBox, QMessageBox, QProgressDialog
)
from qgis.PyQt.QtCore import Qt
from qgis.core import (
    QgsProject, QgsVectorLayer, QgsWkbTypes, QgsCoordinateReferenceSystem,
    QgsVectorFileWriter
)
from qgis.gui import QgsProjectionSelectionWidget  # CRS picker widget

import processing

# AutoCAD (if available)
try:
    from pyautocad import Autocad
    HAS_PYAUTOCAD = True
except Exception:
    HAS_PYAUTOCAD = False


# --------------------- Utilities --------------------- #

def polygon_layers():
    """Return all polygon vector layers currently loaded in the project."""
    lst = []
    for lyr in QgsProject.instance().mapLayers().values():
        if isinstance(lyr, QgsVectorLayer) and lyr.geometryType() == QgsWkbTypes.PolygonGeometry:
            lst.append(lyr)
    return lst

def ensure_same_crs(layer_to_match: QgsVectorLayer, layer_to_reproject: QgsVectorLayer):
    """Reproject layer_to_reproject to the CRS of layer_to_match, if needed. Returns a layer (original or reprojection in memory)."""
    try:
        if not layer_to_match.crs().isValid() or not layer_to_reproject.crs().isValid():
            return layer_to_reproject
        if layer_to_match.crs() == layer_to_reproject.crs():
            return layer_to_reproject
        return processing.run(
            "native:reprojectlayer",
            {"INPUT": layer_to_reproject, "TARGET_CRS": layer_to_match.crs(), "OUTPUT": "memory:"}
        )["OUTPUT"]
    except Exception:
        # If processing isn't available or something odd happens, fall back
        return layer_to_reproject

def reproject_to(layer: QgsVectorLayer, target_crs: QgsCoordinateReferenceSystem):
    """Reproject a layer to target CRS if different."""
    if not target_crs or not target_crs.isValid():
        return layer
    if layer.crs() == target_crs:
        return layer
    return processing.run(
        "native:reprojectlayer",
        {"INPUT": layer, "TARGET_CRS": target_crs, "OUTPUT": "memory:"}
    )["OUTPUT"]

def export_to_shp(layer: QgsVectorLayer, folder: str, base_name: str) -> str:
    """Export layer to a shapefile path (creates .shp/.shx/.dbf/.prj)."""
    os.makedirs(folder, exist_ok=True)
    safe_name = "".join(ch if ch.isalnum() or ch in ("_", "-", " ") else "_" for ch in base_name).strip()
    shp_path = os.path.join(folder, f"{safe_name}.shp")
    res = processing.run("native:savefeatures", {"INPUT": layer, "OUTPUT": shp_path})
    return res["OUTPUT"]

def export_to_dxf(layer: QgsVectorLayer, dxf_path: str) -> str:
    """Export layer to DXF using the core writer."""
    tctx = QgsProject.instance().transformContext()
    opts = QgsVectorFileWriter.SaveVectorOptions()
    opts.driverName = "DXF"
    opts.fileEncoding = "UTF-8"
    err, msg, _ = QgsVectorFileWriter.writeAsVectorFormatV3(layer, dxf_path, tctx, opts)
    if err != QgsVectorFileWriter.NoError:
        raise RuntimeError(f"DXF export failed: {msg}")
    return dxf_path

def cad_quiet_on(acad):
    """Silence AutoCAD command dialogs for batch import."""
    original = {}
    for var in ("FILEDIA", "CMDECHO", "REGENMODE"):
        try:
            original[var] = acad.doc.GetVariable(var)
            acad.doc.SetVariable(var, 0)
        except Exception:
            pass
    return original

def cad_quiet_restore(acad, original):
    """Restore AutoCAD variables."""
    for var, val in (original or {}).items():
        try:
            acad.doc.SetVariable(var, val)
        except Exception:
            pass
def mapimport_polygon(acad, shp_path: str):
    """Import polygon shapefile via MAPIMPORT."""
    shp_path = shp_path.replace("\\", "/")
    cmd = f'(command "_-MapImport" "SHP" "{shp_path}" "N" "I" "Y" "L" "A" "D" "C" "P" "P")\n'
    acad.doc.SendCommand(cmd)

def move_imported_to_layer(acad, source_layer_name: str, target_layer_name: str):
    """Move all entities on source layer to target layer (creates target if missing)."""
    if not target_layer_name:
        return
    try:
        try:
            existing = [ly.Name for ly in acad.doc.Layers]
            if target_layer_name not in existing:
                acad.doc.Layers.Add(target_layer_name)
        except Exception:
            pass
        lisp = f'((command "_.CHANGE" (ssget "X" \'((8 . "{source_layer_name}"))) "" "P" "LA" "{target_layer_name}" ""))\n\n'
        acad.doc.SendCommand(lisp)
    except Exception:
        pass


# --------------------- Dialog --------------------- #

class ClipToCadDialog(QDialog):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Clip Polygons → Send to AutoCAD")
        self.setMinimumWidth(620)

        self.input_combo = QComboBox()
        self.overlay_combo = QComboBox()
        self.populate_layer_combos()

        # Output CRS selector (version-safe)
        self.crs_widget = QgsProjectionSelectionWidget(self)
        # Only call optional methods if they exist in this QGIS build
        for method_name in ("setLayerCrsVisible", "setShowCrsSelectorButton"):
            if hasattr(self.crs_widget, method_name):
                try:
                    getattr(self.crs_widget, method_name)(True)
                except Exception:
                    pass

        # Default Output CRS = current clipper's CRS (if available)
        ov = self._get_layer_by_index(self.overlay_combo.currentIndex())
        if ov and ov.crs().isValid():
            try:
                self.crs_widget.setCrs(ov.crs())
            except Exception:
                pass

        self.btn_use_clipper_crs = QPushButton("Use clipper CRS")
        self.btn_use_clipper_crs.setToolTip("Set Output CRS same as the selected clipper layer")
        self.btn_use_clipper_crs.clicked.connect(self.set_output_crs_to_clipper)

        # When clipper layer changes, refresh default Output CRS (non-destructive, you can override anytime)
        self.overlay_combo.currentIndexChanged.connect(self.on_clipper_changed)

        self.output_name = QLineEdit()
        self.output_name.setPlaceholderText("Output base name (e.g., Clipped_Parcels)")

        self.output_folder = QLineEdit()
        self.output_folder.setPlaceholderText("Choose output folder for SHP/DXF…")
        self.btn_browse = QPushButton("Browse…")
        self.btn_browse.clicked.connect(self.choose_folder)

        self.chk_add_to_qgis = QCheckBox("Add clipped result to QGIS")
        self.chk_add_to_qgis.setChecked(True)

        self.chk_send_to_cad = QCheckBox("Send to AutoCAD via MAPIMPORT (requires Map 3D/Civil 3D)")
        self.chk_send_to_cad.setChecked(True)

        self.cad_target_layer = QLineEdit()
        self.cad_target_layer.setPlaceholderText("Optional: target AutoCAD layer name (e.g., 'CLIPPED_AREAS')")

        self.chk_fallback_dxf = QCheckBox("Also export DXF (fallback/open if MAPIMPORT not available)")
        self.chk_fallback_dxf.setChecked(False)

        self.btn_run = QPushButton("Run")
        self.btn_run.clicked.connect(self.run_task)
        self.btn_cancel = QPushButton("Cancel")
        self.btn_cancel.clicked.connect(self.reject)

        # Layout
        layout = QVBoxLayout()
        row1 = QHBoxLayout(); row1.addWidget(QLabel("Clip this layer:")); row1.addWidget(self.input_combo)
        row2 = QHBoxLayout(); row2.addWidget(QLabel("By this layer (clipper):")); row2.addWidget(self.overlay_combo)
        layout.addLayout(row1); layout.addLayout(row2)

        # Output CRS row
        row_crs = QHBoxLayout()
        row_crs.addWidget(QLabel("Output CRS:"))
        row_crs.addWidget(self.crs_widget, stretch=1)
        row_crs.addWidget(self.btn_use_clipper_crs)
        layout.addLayout(row_crs)

        row3 = QHBoxLayout(); row3.addWidget(QLabel("Output name:")); row3.addWidget(self.output_name)
        row4 = QHBoxLayout(); row4.addWidget(QLabel("Output folder:")); row4.addWidget(self.output_folder); row4.addWidget(self.btn_browse)
        layout.addLayout(row3); layout.addLayout(row4)

        layout.addWidget(self.chk_add_to_qgis)
        layout.addWidget(self.chk_send_to_cad)
        row5 = QHBoxLayout(); row5.addWidget(QLabel("AutoCAD target layer:")); row5.addWidget(self.cad_target_layer)
        layout.addLayout(row5)
        layout.addWidget(self.chk_fallback_dxf)

        row_btn = QHBoxLayout(); row_btn.addStretch(1); row_btn.addWidget(self.btn_run); row_btn.addWidget(self.btn_cancel)
        layout.addLayout(row_btn)

        self.setLayout(layout)

        # Progress dialog
        self.progress = QProgressDialog("Ready…", "Cancel", 0, 100, self)
        self.progress.setWindowModality(Qt.WindowModal)
        self.progress.setAutoClose(True)
        self.progress.setAutoReset(True)

    def _get_layer_by_index(self, idx):
        if idx < 0:
            return None
        return self.layers[idx] if 0 <= idx < len(self.layers) else None

    def populate_layer_combos(self):
        self.input_combo.clear()
        self.overlay_combo.clear()
        self.layers = polygon_layers()
        names = [lyr.name() for lyr in self.layers]
        self.input_combo.addItems(names)
        self.overlay_combo.addItems(names)
        # Select the first two distinct layers if available
        if len(names) >= 2:
            self.input_combo.setCurrentIndex(0)
            self.overlay_combo.setCurrentIndex(1)

    def set_output_crs_to_clipper(self):
        ov = self._get_layer_by_index(self.overlay_combo.currentIndex())
        if ov and ov.crs().isValid():
            try:
                self.crs_widget.setCrs(ov.crs())
            except Exception:
                pass

    def on_clipper_changed(self, _idx):
        # Set output CRS to the clipper CRS as a helpful default (user can change it any time)
        self.set_output_crs_to_clipper()

    def choose_folder(self):
        folder = QFileDialog.getExistingDirectory(self, "Select Output Folder")
        if folder:
            self.output_folder.setText(folder)

    def update_progress(self, value, text):
        self.progress.setValue(value)
        self.progress.setLabelText(text)
        self.progress.setWindowTitle(f"Clip → CAD ({value}%)")
        self.progress.repaint()
        # Keep UI responsive
        from qgis.PyQt.QtWidgets import QApplication as _QApp
        _QApp.processEvents()

    def run_task(self):
        try:
            if self.input_combo.currentIndex() < 0 or self.overlay_combo.currentIndex() < 0:
                QMessageBox.warning(self, "Select Layers", "Please select both input and clipper polygon layers.")
                return
            if self.input_combo.currentIndex() == self.overlay_combo.currentIndex():
                QMessageBox.warning(self, "Invalid Selection", "Choose two different polygon layers.")
                return

            # CRS picked by user
            try:
                out_crs: QgsCoordinateReferenceSystem = self.crs_widget.crs()
            except Exception:
                out_crs = None
            if not out_crs or not out_crs.isValid():
                QMessageBox.warning(self, "Output CRS", "Please select a valid Output CRS.")
                return

            in_layer = self.layers[self.input_combo.currentIndex()]
            ov_layer = self.layers[self.overlay_combo.currentIndex()]

            # Validate polygon geometry
            if in_layer.geometryType() != QgsWkbTypes.PolygonGeometry or ov_layer.geometryType() != QgsWkbTypes.PolygonGeometry:
                QMessageBox.critical(self, "Geometry Error", "Both selected layers must be polygon layers.")
                return

            # Output folder/name
            out_folder = self.output_folder.text().strip() or tempfile.mkdtemp(prefix="clip_to_cad_")
            out_name = self.output_name.text().strip() or f"Clipped_{in_layer.name()}"

            # Reproject input to CLIPPER CRS if needed (for the overlay/clip operation)
            self.update_progress(8, "Reprojecting input to clipper CRS (if needed)…")
            in_for_clip = ensure_same_crs(ov_layer, in_layer)

            # Clip
            self.update_progress(28, "Clipping polygons…")
            clip_res = processing.run("native:clip", {"INPUT": in_for_clip, "OVERLAY": ov_layer, "OUTPUT": "memory:"})
            clipped = clip_res["OUTPUT"]

            feat_count = clipped.featureCount()
            if feat_count == 0:
                QMessageBox.information(self, "Clip Result Empty", "The clip output has 0 features. Nothing will be exported or sent to AutoCAD.")
                if self.chk_add_to_qgis.isChecked():
                    QgsProject.instance().addMapLayer(clipped)
                self.accept()
                return

            # Reproject CLIPPED result to USER-SELECTED Output CRS
            self.update_progress(45, f"Reprojecting clip result to Output CRS: {out_crs.authid()} …")
            clipped_out = reproject_to(clipped, out_crs)

            # Export to shapefile in the Output CRS
            self.update_progress(60, "Exporting shapefile (Output CRS)…")
            shp_path = export_to_shp(clipped_out, out_folder, out_name)
            base_name = os.path.splitext(os.path.basename(shp_path))[0]

            # Optionally add to QGIS
            if self.chk_add_to_qgis.isChecked():
                self.update_progress(65, "Adding result to QGIS…")
                QgsProject.instance().addMapLayer(clipped_out)

            # Send to AutoCAD via MAPIMPORT
            did_send = False
            notes = []
            if self.chk_send_to_cad.isChecked():
                if not HAS_PYAUTOCAD:
                    notes.append("pyautocad not available; cannot MAPIMPORT.")
                else:
                    self.update_progress(75, "Connecting to AutoCAD…")
                    try:
                        acad = Autocad(create_if_not_exists=True)
                        original_vars = cad_quiet_on(acad)
                        try:
                            self.update_progress(85, "MAPIMPORT (polygons)…")
                            mapimport_polygon(acad, shp_path)
                            time.sleep(0.4)  # allow AutoCAD to complete the import
                            # Move to target layer if provided
                            tgt_layer = self.cad_target_layer.text().strip()
                            if tgt_layer:
                                self.update_progress(92, f"Moving entities to AutoCAD layer '{tgt_layer}'…")
                                move_imported_to_layer(acad, base_name, tgt_layer)
                            did_send = True
                        finally:
                            cad_quiet_restore(acad, original_vars)
                    except Exception as e:
                        notes.append(f"AutoCAD import error: {e}")

            # Fallback: DXF
            if (not did_send) and self.chk_fallback_dxf.isChecked():
                try:
                    self.update_progress(94, "Exporting DXF (fallback)…")
                    dxf_path = os.path.join(out_folder, f"{out_name}.dxf")
                    export_to_dxf(clipped_out, dxf_path)
                    # Try to open with default CAD app
                    self.update_progress(98, "Opening DXF in default CAD…")
                    try:
                        os.startfile(dxf_path)  # Windows only
                    except Exception:
                        pass
                    notes.append(f"DXF exported: {dxf_path}")
                except Exception as e:
                    notes.append(f"DXF export failed: {e}")

            self.update_progress(100, "Done")
            msg = (
                f"Clip completed.\n"
                f"Features: {feat_count}\n"
                f"Output CRS: {out_crs.authid()}\n"
                f"Output SHP: {shp_path}"
            )
            if notes:
                msg += "\n\nNotes:\n- " + "\n- ".join(notes)
            QMessageBox.information(self, "Success", msg)
            self.accept()
        except Exception as e:
            tb = traceback.format_exc()
            QMessageBox.critical(self, "Error", f"{e}\n\n{tb}")
            self.reject()


# --------------------- Run Dialog --------------------- #

def run():
    dlg = ClipToCadDialog()
    if not polygon_layers():
        QMessageBox.information(None, "No Polygon Layers", "Load at least two polygon layers into the project and try again.")
        return
    dlg.exec_()

# Execute immediately
run()
