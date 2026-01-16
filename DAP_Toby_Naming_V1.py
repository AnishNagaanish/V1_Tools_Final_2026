from qgis.PyQt.QtWidgets import QInputDialog, QMessageBox, QDialog, QVBoxLayout, QLabel, QLineEdit, QPushButton, QFormLayout
from qgis.core import QgsFeature, QgsGeometry, QgsProject, QgsVectorLayer, QgsField
from PyQt5.QtCore import QVariant

class InputDialog(QDialog):
    def __init__(self):
        super().__init__()
        self.setWindowTitle('Input Parameters')
        self.layout = QFormLayout()
        
        self.point_layer_input = QLineEdit()
        self.filter_attribute_value_input = QLineEdit()
        self.middle_string_input = QLineEdit()
        
        self.layout.addRow('Enter Hexatronics Duct Access Points Layer or Hexatronics Toby Box Layer:', self.point_layer_input)
        self.layout.addRow('Enter the FDH_LABEL:', self.filter_attribute_value_input)
        self.layout.addRow('Enter the text "DAP" for Duct Access Point or "HH" for TOBY Box:', self.middle_string_input)
        
        self.submit_button = QPushButton('Submit')
        self.submit_button.clicked.connect(self.accept)
        self.layout.addWidget(self.submit_button)
        
        self.setLayout(self.layout)

def sequence_points_within_polygon(point_layer, polygon_layer, point_1_layer, filter_attribute_name, filter_attribute_value, concat_attribute_name, middle_string):
    if point_layer.fields().indexOf("ELEMENTID") == -1:
        point_layer.startEditing()
        point_layer.dataProvider().addAttributes([QgsField("ELEMENTID", QVariant.String)])
        point_layer.updateFields()
    point_layer.startEditing()  # Start editing block here

    selected_polygons = [poly for poly in polygon_layer.getFeatures() if poly[filter_attribute_name] == filter_attribute_value]
    for polygon in selected_polygons:
        nearest_distance = float('inf')
        start_point_feature = None
        
        for point_1_feature in point_1_layer.getFeatures():
            point_1_geom = point_1_feature.geometry()
            
            for point_feature in point_layer.getFeatures():
                if polygon.geometry().contains(point_feature.geometry()):
                    distance = point_feature.geometry().distance(point_1_geom)
                    if distance < nearest_distance:
                        nearest_distance = distance
                        start_point_feature = point_feature
        if start_point_feature is None:
            print("No starting point found in polygon!")
            continue

        branch_features = [start_point_feature]
        remaining_features = [
            f for f in point_layer.getFeatures()
            if polygon.geometry().contains(f.geometry()) and f.id() != start_point_feature.id()
        ]

        current_feature = start_point_feature
        while remaining_features:
            next_feature = min(remaining_features, key=lambda f: current_feature.geometry().distance(f.geometry()))
            branch_features.append(next_feature)
            remaining_features.remove(next_feature)
            current_feature = next_feature

        sequence_number = 1
        for feature in branch_features:
            concat_attribute_value = polygon[concat_attribute_name]  # Getting the ELEMENTID attribute value from the polygon
            feature["ELEMENTID"] = f"{concat_attribute_value}-{middle_string}-{sequence_number:02}"
            point_layer.updateFeature(feature)
            print(f"Feature {feature.id()} assigned sequence: {feature['ELEMENTID']}")
            sequence_number += 1
    
    point_layer.commitChanges()  # End editing block here
    print("Sequencing complete.")

def get_layer_by_name(layer_name):
    layers = QgsProject.instance().mapLayersByName(layer_name)
    if not layers:
        raise ValueError(f"Layer '{layer_name}' not found")
    return layers[0]

try:
    # Hardcoded values for polygon and point_1 layers
    polygon_layer_name = 'Hexatronics Distribution Areas'
    point_1_layer_name = 'FDH'

    input_dialog = InputDialog()
    if input_dialog.exec_() == QDialog.Accepted:
        point_layer_name = input_dialog.point_layer_input.text()
        filter_attribute_value = input_dialog.filter_attribute_value_input.text()
        middle_string = input_dialog.middle_string_input.text()
        
        point_layer = get_layer_by_name(point_layer_name)
        polygon_layer = get_layer_by_name(polygon_layer_name)
        point_1_layer = get_layer_by_name(point_1_layer_name)

    else:
        raise Exception("Input not provided")

    filter_attribute_name = 'LABEL'
    concat_attribute_name = 'ELEMENTID'

    sequence_points_within_polygon(point_layer, polygon_layer, point_1_layer, filter_attribute_name, filter_attribute_value, concat_attribute_name, middle_string)

except ValueError as e:
    print(e)
    raise

except Exception as e:
    QMessageBox.critical(None, "Error", f"An error occurred: {e}")
    print(f"An error occurred: {e}. Please try running the script again.")
