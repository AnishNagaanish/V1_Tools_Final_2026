from qgis.PyQt import QtWidgets, QtCore
from qgis.core import QgsProject, QgsVectorLayer

class FieldDeletionDialog(QtWidgets.QDialog):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Delete Fields from Selected Layers")
        self.setMinimumWidth(400)

        layout = QtWidgets.QVBoxLayout()

        # Layer selection
        self.layer_list = QtWidgets.QListWidget()
        self.layer_list.setSelectionMode(QtWidgets.QAbstractItemView.MultiSelection)
        for layer in QgsProject.instance().mapLayers().values():
            if isinstance(layer, QgsVectorLayer):
                self.layer_list.addItem(layer.name())
        layout.addWidget(QtWidgets.QLabel("Select layers:"))
        layout.addWidget(self.layer_list)

        # Field input
        self.field_input = QtWidgets.QLineEdit()
        layout.addWidget(QtWidgets.QLabel("Enter field names to delete (comma-separated):"))
        layout.addWidget(self.field_input)

        # Progress bar
        self.progress_bar = QtWidgets.QProgressBar()
        self.progress_bar.setMinimum(0)
        layout.addWidget(QtWidgets.QLabel("Progress:"))
        layout.addWidget(self.progress_bar)

        # Buttons
        button_box = QtWidgets.QDialogButtonBox(QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel)
        button_box.accepted.connect(self.run_deletion)
        button_box.rejected.connect(self.reject)
        layout.addWidget(button_box)

        self.setLayout(layout)

    def get_selected_layers(self):
        selected_names = [item.text() for item in self.layer_list.selectedItems()]
        return [layer for layer in QgsProject.instance().mapLayers().values()
                if layer.name() in selected_names]

    def get_fields_to_delete(self):
        return [field.strip().lower() for field in self.field_input.text().split(",") if field.strip()]

    def run_deletion(self):
        layers = self.get_selected_layers()
        fields_to_delete = self.get_fields_to_delete()

        self.progress_bar.setMaximum(len(layers))
        self.progress_bar.setValue(0)

        for i, layer in enumerate(layers):
            layer.startEditing()
            field_names = [field.name() for field in layer.fields()]
            for field_name in fields_to_delete:
                for actual_field in field_names:
                    if actual_field.lower() == field_name:
                        layer.deleteAttribute(layer.fields().indexOf(actual_field))
            layer.commitChanges()

            self.progress_bar.setValue(i + 1)
            QtWidgets.QApplication.processEvents()  # Keeps UI responsive

        QtWidgets.QMessageBox.information(self, "Success", "Fields deleted (case-insensitive) from selected layers.")
        self.accept()

# Run the dialog
dialog = FieldDeletionDialog()
dialog.exec_()
