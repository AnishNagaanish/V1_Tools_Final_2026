from qgis.core import (
    QgsProject, QgsField, QgsVectorLayer, 
    QgsExpression, QgsExpressionContext, QgsFeature, QgsWkbTypes
)
from PyQt5.QtWidgets import (
    QDialog, QListWidget, QVBoxLayout, QPushButton, QLineEdit, QLabel, QMessageBox
)
from PyQt5.QtCore import QVariant

class AddColumnsDialog(QDialog):
    def __init__(self):
        """Initialize the Dialog."""
        super().__init__()

        self.setWindowTitle("Update Lat/Long Columns in Point Layers")
        self.layout = QVBoxLayout()

        # Layer selection
        self.layer_list = QListWidget()
        self.layer_list.setSelectionMode(QListWidget.MultiSelection)
        self.load_layers()
        self.layout.addWidget(QLabel("Select Point Layers:"))
        self.layout.addWidget(self.layer_list)

        # Formula inputs for Lat & Long
        self.layout.addWidget(QLabel("Formula for Latitude (Optional, Default: y($geometry))"))
        self.lat_formula = QLineEdit()
        self.layout.addWidget(self.lat_formula)

        self.layout.addWidget(QLabel("Formula for Longitude (Optional, Default: x($geometry))"))
        self.long_formula = QLineEdit()
        self.layout.addWidget(self.long_formula)

        # Optional custom column
        self.layout.addWidget(QLabel("Custom Column Name (Optional):"))
        self.column_name = QLineEdit()
        self.layout.addWidget(self.column_name)

        self.layout.addWidget(QLabel("Default Formula/Value for Custom Column (Optional):"))
        self.default_value = QLineEdit()
        self.layout.addWidget(self.default_value)

        # Button to apply changes
        self.add_button = QPushButton("Update Columns")
        self.add_button.clicked.connect(self.update_columns)
        self.layout.addWidget(self.add_button)

        self.setLayout(self.layout)

    def load_layers(self):
        """Load only vector layers with point geometry."""
        for layer in QgsProject.instance().mapLayers().values():
            if isinstance(layer, QgsVectorLayer) and layer.wkbType() == QgsWkbTypes.Point:
                self.layer_list.addItem(layer.name())

    def update_columns(self):
        """Update Lat, Long, and optional custom columns in selected layers."""
        selected_layers = [item.text() for item in self.layer_list.selectedItems()]
        if not selected_layers:
            QMessageBox.warning(self, "Warning", "Please select at least one layer.")
            return

        for layer_name in selected_layers:
            layer = QgsProject.instance().mapLayersByName(layer_name)[0]
            layer.startEditing()

            # Get existing field names
            existing_fields = [field.name() for field in layer.fields()]

            # Define fields only if they don't exist
            fields_to_add = []
            fields_to_add = []
            if "Lat" not in existing_fields:
                fields_to_add.append(QgsField("Lat", QVariant.Double, "Latitude", 10, 5))  # Precision set to 5
            if "Long" not in existing_fields:
                fields_to_add.append(QgsField("Long", QVariant.Double, "Longitude", 10, 5))  # Precision set to 5
            if self.column_name.text() and self.column_name.text() not in existing_fields:
                fields_to_add.append(QgsField(self.column_name.text(), QVariant.Double, "CustomField", 10, 5))  # Precision set to 5


            # Add new fields if needed
            if fields_to_add:
                layer.dataProvider().addAttributes(fields_to_add)
                layer.updateFields()

            for feature in layer.getFeatures():
                try:
                    # Create an expression context
                    context = QgsExpressionContext()
                    context.setFeature(feature)

                    # Evaluate formulas for Lat and Long
                    lat_expr = QgsExpression(self.lat_formula.text()) if self.lat_formula.text() else QgsExpression("y($geometry)")
                    long_expr = QgsExpression(self.long_formula.text()) if self.long_formula.text() else QgsExpression("x($geometry)")

                    lat_value = lat_expr.evaluate(context)
                    long_value = long_expr.evaluate(context)

                    # Ensure values are within precision limits
                    lat_value = float("{:.5f}".format(lat_value))
                    long_value = float("{:.5f}".format(long_value))

                    feature.setAttribute("Lat", lat_value)
                    feature.setAttribute("Long", long_value)

                    # Check for errors in formula evaluation
                    if lat_expr.hasParserError() or long_expr.hasParserError():
                        QMessageBox.warning(self, "Formula Error", "Invalid formula in Lat/Long fields.")

                    # Evaluate formula for custom column (if provided)
                    if self.column_name.text() and self.default_value.text():
                        custom_expr = QgsExpression(self.default_value.text())
                        custom_value = custom_expr.evaluate(context)
                        custom_value = round(custom_value, 5)  # Precision enforcement

                        feature.setAttribute(self.column_name.text(), custom_value)

                        if custom_expr.hasParserError():
                            QMessageBox.warning(self, "Formula Error", "Invalid formula in custom column.")

                    layer.updateFeature(feature)  # Ensure feature is updated

                except Exception as e:
                    QMessageBox.warning(self, "Error", f"Formula evaluation failed: {e}")

        layer.commitChanges()

        QMessageBox.information(self, "Success", "Columns updated successfully!")

# Show the dialog in QGIS
dialog = AddColumnsDialog()
dialog.exec_()
