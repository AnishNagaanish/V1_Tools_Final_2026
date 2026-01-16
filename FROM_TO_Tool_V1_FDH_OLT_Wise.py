from qgis.PyQt.QtWidgets import QDialog, QVBoxLayout, QDialogButtonBox, QLabel, QListWidget

class LayerSelectionDialog(QDialog):
    def __init__(self, parent=None, title='', layer_type=QgsWkbTypes.PointGeometry):
        super(LayerSelectionDialog, self).__init__(parent)
        
        self.setWindowTitle(title)
        
        self.layout = QVBoxLayout()
        
        self.label = QLabel("Select Point Layers")
        self.layout.addWidget(self.label)
        
        self.list_widget = QListWidget()
        self.list_widget.setSelectionMode(QListWidget.MultiSelection)
        self.layout.addWidget(self.list_widget)
        
        for layer in QgsProject.instance().mapLayers().values():
            if isinstance(layer, QgsVectorLayer) and layer.geometryType() == layer_type:
                self.list_widget.addItem(layer.name())
        
        self.button_box = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        self.button_box.accepted.connect(self.accept)
        self.button_box.rejected.connect(self.reject)
        self.layout.addWidget(self.button_box)
        
        self.setLayout(self.layout)
        self.selected_layer_names = []
    
    def accept(self):
        self.selected_layer_names = [item.text() for item in self.list_widget.selectedItems()]
        super(LayerSelectionDialog, self).accept()

    def reject(self):
        self.selected_layer_names = []
        super(LayerSelectionDialog, self).reject()

try:
    # Get the active polygon layer through an input dialog box
    layer_list = QgsProject.instance().mapLayers().values()
    polygon_layer_names = [layer.name() for layer in layer_list if isinstance(layer, QgsVectorLayer) and layer.geometryType() == QgsWkbTypes.PolygonGeometry]
    polygon_layer_name, ok = QInputDialog.getItem(None, "Select Polygon Layer", "Choose the polygon layer:", polygon_layer_names, 0, False)

    if not ok:
        raise Exception("No polygon layer selected.")
    polygon_layer = QgsProject.instance().mapLayersByName(polygon_layer_name)[0]

    # Hardcoded attribute name for filtering polygons
    filter_attribute_name = 'LABEL'
    # User-defined attribute value for filtering polygons
    filter_attribute_value, ok = QInputDialog.getText(None, "Input", "Enter the LABEL of FDH:")
    if not ok:
        raise Exception("Attribute value not provided")

    # Filter polygons based on attribute value
    selected_polygons = [poly for poly in polygon_layer.getFeatures() if poly[filter_attribute_name] == filter_attribute_value]
    if not selected_polygons:
        raise Exception("No polygons found with the provided attribute value")

    # Get the active layer through an input dialog box
    line_layer_names = [layer.name() for layer in layer_list if isinstance(layer, QgsVectorLayer) and layer.geometryType() == QgsWkbTypes.LineGeometry]
    layer_name, ok = QInputDialog.getItem(None, "Select Active Layer", "Choose the active layer:", line_layer_names, 0, False)
    if not ok:
        raise Exception("No layer selected.")
    active_layer = QgsProject.instance().mapLayersByName(layer_name)[0]

    # Create and display the point layer selection dialog
    dialog = LayerSelectionDialog(title='Select Point Layers', layer_type=QgsWkbTypes.PointGeometry)
    if dialog.exec_() == QDialog.Accepted and dialog.selected_layer_names:
        point_layer_names = dialog.selected_layer_names
        point_layers = [QgsProject.instance().mapLayersByName(name)[0] for name in point_layer_names]
    else:
        raise Exception("No point layers selected.")

    # Create spatial indices for the selected point layers
    indices = {name: QgsSpatialIndex(layer.getFeatures()) for name, layer in zip(point_layer_names, point_layers)}

    # Start editing the active line layer
    active_layer.startEditing()
    # Batch process feature geometries and their attributes within selected polygons
    line_features = list(active_layer.getFeatures())
    point_features = {name: {feat.id(): feat for feat in layer.getFeatures()} for name, layer in zip(point_layer_names, point_layers)}

    # Iterate through line features
    for line_feature in line_features:
        line_geometry = line_feature.geometry()

        # Check if line feature is within selected polygons
        if not any(poly.geometry().intersects(line_geometry) for poly in selected_polygons):
            continue

        # Handle MultiLineString geometry
        if line_geometry.isMultipart():
            parts = line_geometry.asMultiPolyline()
        elif line_geometry.type() == QgsWkbTypes.LineGeometry:
            parts = [line_geometry.asPolyline()]
        else:
            print(f"Invalid geometry type for feature {line_feature.id()}. Skipping.")
            continue

        start_attribute = ''
        end_attribute = ''
        for part in parts:
            start_point = part[0]
            end_point = part[-1]
            start_geom = QgsGeometry.fromPointXY(QgsPointXY(start_point))
            end_geom = QgsGeometry.fromPointXY(QgsPointXY(end_point))
            try:
                for name in point_layer_names:
                    nearest_start_id = indices[name].nearestNeighbor(start_geom.asPoint(), 1)
                    nearest_end_id = indices[name].nearestNeighbor(end_geom.asPoint(), 1)
                    if nearest_start_id and not start_attribute:
                        start_point_feature = point_features[name][nearest_start_id[0]]
                        if start_point_feature.geometry().asPoint() == start_geom.asPoint():
                            start_attribute = start_point_feature['ELEMENTID']
                            print(f"Start attribute from {name}: {start_attribute}")
                    if nearest_end_id and not end_attribute:
                        end_point_feature = point_features[name][nearest_end_id[0]]
                        if end_point_feature.geometry().asPoint() == end_geom.asPoint():
                            end_attribute = end_point_feature['ELEMENTID']
                            print(f"End attribute from {name}: {end_attribute}")
            except Exception as e:
                print(f"Error processing part {part}: {e}")
                continue

        # Update the existing feature
        if start_attribute and end_attribute:
            line_feature['from'] = start_attribute
            line_feature['to'] = end_attribute
            active_layer.updateFeature(line_feature)
            print(f"Updated feature {line_feature.id()} with From: {start_attribute} and To: {end_attribute}")

    # Commit changes
    active_layer.commitChanges()
    QMessageBox.information(None, "Task Completed", "The selected layer has been updated successfully.")
    print("Task completed successfully. The selected layer has been updated.")
except Exception as e:
    QMessageBox.critical(None, "Error", f"An error occurred: {e}")
    print(f"An error occurred: {e}. Please try running the script again.")
