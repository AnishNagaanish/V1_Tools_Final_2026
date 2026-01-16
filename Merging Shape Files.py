from qgis import processing
from qgis.core import QgsVectorLayer, QgsProcessingFeedback
import os, shutil, datetime
from PyQt5.QtWidgets import QFileDialog
from PyQt5.QtGui import QDesktopServices
from PyQt5.QtCore import QUrl

# --- Step 1: Select multiple input folders ---
dlg = QFileDialog()
dlg.setFileMode(QFileDialog.Directory)
dlg.setOption(QFileDialog.DontUseNativeDialog, True)  # enables multi-select
dlg.setOption(QFileDialog.ShowDirsOnly, True)

folders = []
if dlg.exec_():
    folders = dlg.selectedFiles()  # list of directories

if not folders:
    raise Exception("No input folders selected!")

# --- Step 2: Select output folder ---
out_folder = QFileDialog.getExistingDirectory(None, "Select Output Folder")
if not out_folder:
    raise Exception("No output folder selected!")

# --- Step 3: Collect shapefiles grouped by filename ---
shapefile_groups = {}
for folder in folders:
    for root, dirs, files in os.walk(folder):
        for f in files:
            if f.lower().endswith(".shp"):
                name = f  # group by filename
                path = os.path.join(root, f)
                shapefile_groups.setdefault(name, []).append(path)

# --- Prepare log containers ---
log_success, log_copy, log_skip_mixed, log_invalid, log_failure = [], [], [], [], []

# --- Utility: remove existing shapefile set ---
def clean_existing_shp(out_path):
    base = out_path[:-4] if out_path.lower().endswith(".shp") else out_path
    for ext in [".shp", ".shx", ".dbf", ".prj", ".cpg"]:
        try:
            os.remove(base + ext)
        except:
            pass

# --- Step 4: Merge or copy each group ---
total_groups = len(shapefile_groups)
for idx, (name, files) in enumerate(shapefile_groups.items(), start=1):
    print(f"[{idx}/{total_groups}] Processing group: {name} ({len(files)} files)")

    # Validate layers, collect geometry types
    valid_files, geom_types, invalids = [], set(), []

    for f in files:
        vlayer = QgsVectorLayer(f, "", "ogr")
        if not vlayer.isValid():
            # Diagnose why invalid
            reason = []
            if not os.path.exists(f.replace(".shp", ".dbf")):
                reason.append("Missing DBF")
            if not os.path.exists(f.replace(".shp", ".shx")):
                reason.append("Missing SHX")
            if not os.path.exists(f.replace(".shp", ".prj")):
                reason.append("Missing PRJ")
            if not reason:
                reason = ["Unknown / corrupt shapefile"]
            log_invalid.append({"group": name, "file": f, "reason": ", ".join(reason)})
            invalids.append(f)
        else:
            valid_files.append(f)
            geom_types.add(vlayer.geometryType())

    if not valid_files:
        log_failure.append({"group": name, "files": files, "error": "No valid layers"})
        continue

    if len(geom_types) > 1:
        log_skip_mixed.append({"group": name, "files": valid_files, "geom_types": list(geom_types)})
        continue

    if len(valid_files) == 1:
        src, dst = valid_files[0], os.path.join(out_folder, name)
        try:
            clean_existing_shp(dst)
            shutil.copy(src, dst)
            log_copy.append({"group": name, "source": src, "dest": dst})
        except Exception as e:
            log_failure.append({"group": name, "files": [src], "error": f"Copy failed: {e}"})
        continue

    # Merge multiple valid files
    out_path = os.path.join(out_folder, name)
    clean_existing_shp(out_path)
    try:
        feedback = QgsProcessingFeedback()
        processing.run("native:mergevectorlayers", {
            'LAYERS': valid_files,
            'CRS': None,
            'OUTPUT': out_path
        }, feedback=feedback)
        log_success.append({"group": name, "count": len(valid_files), "output": out_path, "files": valid_files})
    except Exception as e:
        log_failure.append({"group": name, "files": valid_files, "error": str(e)})

# --- Step 5: Build HTML report ---
timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
report_path = os.path.join(out_folder, "merge_report.html")

def html_escape(s):
    return (s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))

html = []
html.append("""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>QGIS Merge Report</title>
<style>
  body { font-family: Segoe UI, Roboto, Arial, sans-serif; margin: 24px; background: #0f172a; color: #e5e7eb; }
  h1 { color: #93c5fd; margin-bottom: 4px; }
  .subtitle { color: #cbd5e1; margin-top: 0; font-size: 0.95em; }
  .summary { display: grid; grid-template-columns: repeat(5, 1fr); gap: 12px; margin: 20px 0 30px; }
  .card { padding: 14px; border-radius: 10px; }
  .ok { background: #064e3b; color: #a7f3d0; }
  .copy { background: #1f2937; color: #d1d5db; }
  .skip { background: #3f1d1d; color: #fecaca; }
  .invalid { background: #3b0764; color: #e9d5ff; }
  .fail { background: #7f1d1d; color: #fecaca; }
  table { width: 100%; border-collapse: collapse; margin-bottom: 28px; }
  th, td { padding: 10px 12px; border-bottom: 1px solid #334155; vertical-align: top; }
  th { text-align: left; background: #1f2937; color: #93c5fd; }
  .section-title { margin: 30px 0 10px; color: #c7d2fe; }
  .badge { display: inline-block; padding: 2px 8px; border-radius: 999px; font-size: 12px; margin-left: 8px; background: #0ea5e9; color: #022c22; }
  .path { font-family: Consolas, Menlo, monospace; color: #f8fafc; }
  .group { color: #f59e0b; font-weight: 600; }
  .footer { margin-top: 26px; color: #9ca3af; font-size: 0.9em; }
</style>
</head>
<body>
<h1>QGIS shapefile merge report</h1>
<p class="subtitle">Generated at """ + html_escape(timestamp) + """</p>
""")

# Summary
html.append('<div class="summary">')
html.append(f'<div class="card ok"><strong>Merged:</strong> {len(log_success)}</div>')
html.append(f'<div class="card copy"><strong>Copied:</strong> {len(log_copy)}</div>')
html.append(f'<div class="card skip"><strong>Skipped (mixed geometry):</strong> {len(log_skip_mixed)}</div>')
html.append(f'<div class="card invalid"><strong>Invalid layers:</strong> {len(log_invalid)}</div>')
html.append(f'<div class="card fail"><strong>Failures:</strong> {len(log_failure)}</div>')
html.append('</div>')

# Sections
def make_table(title, headers, rows):
    html.append(f'<h2 class="section-title">{title}</h2>')
    html.append('<table><thead><tr>' + ''.join(f'<th>{h}</th>' for h in headers) + '</tr></thead><tbody>')
    for row in rows:
        html.append('<tr>' + ''.join(f'<td>{c}</td>' for c in row) + '</tr>')
    html.append('</tbody></table>')

if log_success:
    rows = [[f'<span class="group">{html_escape(i["group"])}</span>',
             str(i["count"]),
             f'<span class="path">{html_escape(i["output"])}</span>',
             "<br>".join(f'<span class="path">{html_escape(p)}</span>' for p in i["files"])]
            for i in log_success]
    make_table("Merged Groups", ["Group", "Count", "Output", "Files"], rows)

if log_copy:
    rows = [[f'<span class="group">{html_escape(i["group"])}</span>',
             f'<span class="path">{html_escape(i["source"])}</span>',
             f'<span class="path">{html_escape(i["dest"])}</span>']
            for i in log_copy]
    make_table("Copied Single Files", ["Group", "Source", "Destination"], rows)

if log_skip_mixed:
    rows = [[f'<span class="group">{html_escape(i["group"])}</span>',
             ", ".join(map(str, i["geom_types"])),
             "<br>".join(f'<span class="path">{html_escape(p)}</span>' for p in i["files"])]
            for i in log_skip_mixed]
    make_table("Skipped (Mixed Geometry)", ["Group", "Geometry Types", "Files"], rows)

if log_invalid:
    rows = [[f'<span class="group">{html_escape(i["group"])}</span>',
             f'<span class="path">{html_escape(i["file"])}</span>',
             html_escape(i["reason"])]
            for i in log_invalid]
    make_table("Invalid Layers", ["Group", "File", "Reason"], rows)

if log_failure:
    rows = [[f'<span class="group">{html_escape(i["group"])}</span>',
             html_escape(i["error"]),
             "<br>".join(f'<span class="path">{html_escape(p)}</span>' for p in i["files"])]
            for i in log_failure]
    make_table("Failures", ["Group", "Error", "Files"], rows)

html.append(f'<p>Processed {total_groups} filename groups from {len(folders)} input folders.</p>')
html.append('</body></html>')

# --- Write report file ---
with open(report_path, "w", encoding="utf-8") as f:
    f.write("".join(html))

# --- Step 6: Completion message and open report ---
print("--------------------------------------------------")
print(f"✅ Completed processing {total_groups} groups")
print(f"   Merged: {len(log_success)}")
print(f"   Copied: {len(log_copy)}")
print(f"   Skipped (mixed geometry): {len(log_skip_mixed)}")
print(f"   Invalid: {len(log_invalid)}")
print(f"   Failures: {len(log_failure)}")
print(f"📄 Report saved to: {report_path}")
print("--------------------------------------------------")

try:
    QDesktopServices.openUrl(QUrl.fromLocalFile(report_path))
except Exception as e:
    print(f"Could not open report automatically: {e}")
