from qgis.core import (
    QgsApplication,
    QgsProject,
    QgsMapLayer,
    QgsVectorLayer,
    QgsCoordinateReferenceSystem
)
from qgis.analysis import QgsNativeAlgorithms
import processing
import os

# Initialize QGIS Application (Running without GUI)
QgsApplication.setPrefixPath("/usr", True)
qgs = QgsApplication([], False)
qgs.initQgis()

# Register native algorithms
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Get the currently opened QGIS project
project = QgsProject.instance()

# Define target CRS
target_crs = QgsCoordinateReferenceSystem('EPSG:4326')

# Ensure the project CRS is set to EPSG:4326
project.setCrs(target_crs)

# Process layers without using temporary memory and replace them in their original file paths
layers_to_process = list(project.mapLayers().values())[:]

for layer in layers_to_process:
    if layer.type() == QgsMapLayer.VectorLayer and layer.crs().authid() != target_crs.authid():
        layer_path = layer.source()  # Get original file path

        if layer_path.startswith('memory:'):  # Skip temporary layers
            continue

        params = {
            'INPUT': layer,
            'TARGET_CRS': target_crs,
            'OUTPUT': layer_path  # Overwriting the original file
        }
        result = processing.run("native:reprojectlayer", params)

        reprojected_layer = QgsVectorLayer(layer_path, layer.name(), layer.providerType())

        # Preserve original properties
        reprojected_layer.loadNamedStyle(layer.styleURI())  # Apply original layer style
        reprojected_layer.setCustomProperties(layer.customProperties())  # Preserve custom properties

        project.removeMapLayer(layer)  # Remove old layer
        project.addMapLayer(reprojected_layer)  # Add updated layer

# Save changes to the project
project.write(project.fileName())

print("All layers successfully reprojected to EPSG:4326 and replaced in their original file paths. QGIS will now close.")

# Clean exit
QgsApplication.quit()
