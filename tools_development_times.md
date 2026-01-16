# Estimated Development & Testing Time (ballpark)

Repository: AnishNagaanish/V1_Tools_Final_2026

Notes:
- These are approximate development and testing time estimates based on file type, size, and typical complexity.
- "Dev" = development time. "Test" = testing + bugfixing + small iterations. Times are given in hours (h) or days (d) where 8h = 1d.
- Results may be incomplete if the repo has more files than the API listing used. Full repo contents: https://github.com/AnishNagaanish/V1_Tools_Final_2026/contents

Top-5 tools by total dev+test time (summary)
1. FTTH Planner Pro 1.zip — Dev 24-40h, Test 8-24h, Total ~32-64h (4-8d)
2. ftth_automation 1.zip — Dev 24-40h, Test 8-16h, Total ~32-56h (4-7d)
3. Callouts_Placement_Auto.lsp — Dev 16-32h, Test 4-8h, Total ~20-40h (3-5d)
4. QGIS_Callouts_Labes export-ver2.2.py — Dev 16-32h, Test 6-12h, Total ~22-44h (3-6d)
5. QGIS_Callouts_Labes export-ver2.3.py — Dev 16-32h, Test 6-12h, Total ~22-44h (3-6d)

Full per-file estimates (from repo contents returned by the API listing):

| filename | type | dev (estimate) | test (estimate) | total | notes |
|---|---:|---:|---:|---:|---|
| Add Lat_Long.py | Python | 4h | 2h | 6h | Geocoding/attribute addition scripts are small-medium complexity. |
| AlignAnchor2Pole.lsp | AutoLISP | 2h | 1h | 3h | Simple alignment helper. |
| BLK_MT.lsp | AutoLISP | 8h | 3h | 11h | Block + MText automation needs placement logic and formatting. |
| BOM2CSV.lsp | AutoLISP | 12h | 4h | 16h | BOM extraction involves parsing entities and attribute mapping. |
| BoreLengths.lsp | AutoLISP | 3h | 1h | 4h | Length calculation automation. |
| Buildings to Autocad.py | Python | 24h | 8h | 32h | Conversion of building data to DWG can be involved (geometry, attributes). |
| CD_details_ver1.py | Python | 16h | 6h | 22h | Detail generation and formatting for CAD/PDF output. |
| CSV2MTEXT.lsp | AutoLISP | 10h | 3h | 13h | Converting CSV rows to MText with formatting and positioning. |
| Callouts_Placement_Auto.lsp | AutoLISP | 24h | 8h | 32h | Complex multi-sheet callout placement and rules. |
| CleanPrintLayoutsV1.1.lsp | AutoLISP | 8h | 2h | 10h | Layout cleanup across many sheets. |
| CleanPrintLayoutsv1.lsp | AutoLISP | 8h | 2h | 10h | Similar to v1.1. |
| Cleartext.lsp | AutoLISP | 1.5h | 0.5h | 2h | Simple text cleanup script. |
| Connect_Labels_v2.lsp | AutoLISP | 16h | 6h | 22h | Label connection, routing, leader logic, lots of edge cases. |
| DAP_Toby_Naming_V1.py | Python | 6h | 2h | 8h | Naming/renaming automation. |
| DHATCHLAYER.lsp | AutoLISP | 1.5h | 0.5h | 2h | Simple hatch/layer handling. |
| DIRECTION -ARROW(FlowFromFDH).lsp | AutoLISP | 6h | 2h | 8h | Direction arrow placement logic. |
| DSSCLR.lsp | AutoLISP | 0.5h | 0.5h | 1h | Very small utility. |
| DWCLR.lsp | AutoLISP | 0.5h | 0.5h | 1h | Very small utility. |
| DW_MTEXT.lsp | AutoLISP | 10h | 3h | 13h | MText creation/formatting complexity. |
| DapCreator 1.zip | Zip/tool | 6h | 2h | 8h | Small packaged utility. |
| Delete Fields_QGIS.py | Python | 2h | 1h | 3h | Simple field removal and validation. |
| FROM_TO_Tool_V1_FDH_OLT_Wise.py | Python | 12h | 4h | 16h | Mapping/transformation logic. |
| FTTH Planner Pro 1.zip | Zip/app | 32h | 16h | 48h | Large planning tool/package; likely multiple modules. |
| FTTH_CSV_Import.lsp | AutoLISP | 12h | 4h | 16h | CSV parsing, placement and attribute mapping. |
| GP_Convert.lsp | AutoLISP | 1h | 0.5h | 1.5h | Small conversion helper. |
| GenerateAndExportCoordinates.lsp | AutoLISP | 3h | 1h | 4h | Coordinate extraction + export. |
| Line_Poly.lsp | AutoLISP | 1h | 0.5h | 1.5h | Small helper. |
| Merging Shape Files.py | Python | 6h | 2h | 8h | Shapefile I/O and attribute merging, depends on complexity. |
| OSP_CleanupTools.lsp | AutoLISP | 12h | 4h | 16h | Collection of cleanup utilities; domain-specific rules. |
| PDF Merger (dir) | Utility | 4h | 2h | 6h | PDF merge batching and CLI/UI. |
| PRINTPATHAUTO_ver1.lsp | AutoLISP | 12h | 4h | 16h | Automating print/plot setups across sheets. |
| PlaceWaterMainLine_CWM.lsp | AutoLISP | 4h | 1h | 5h | Specialized placement logic. |
| QGIS_Callouts_Labes export-ver2.2.py | Python | 24h | 8h | 32h | Complex QGIS→CAD label/export processing. |
| QGIS_Callouts_Labes export-ver2.3.py | Python | 24h | 8h | 32h | Updated version; similar effort. |
| Reproject_Layers to EPSG-4326.py | Python | 2h | 1h | 3h | Batch reproject script. |
| SLD (dir) | Styles | 4h | 2h | 6h | SLD creation and testing across layers. |
| SNAP_DROP.LSP | AutoLISP | 16h | 6h | 22h | Robust snapping/dropping logic with many cases. |
| STREET_NAMES.lsp | AutoLISP | 4h | 1h | 5h | Street name placement/formatting. |
| ST_Convert.lsp | AutoLISP | 1h | 0.5h | 1.5h | Small conversion tool. |
| SelectSewerLayers.lsp | AutoLISP | 1h | 0.5h | 1.5h | Layer selection helper. |
| SelectSewerLayers_OSCEOLA.lsp | AutoLISP | 1h | 0.5h | 1.5h | Variant for specific dataset. |
| SelectwaterLayers.lsp | AutoLISP | 1h | 0.5h | 1.5h | Layer selection helper. |
| TB_ALIGN(WMA).LSP | AutoLISP | 8h | 2h | 10h | Alignment tool with domain rules. |
| Toby_CD (dir) | Collection | 8h | 4h | 12h | Collection of CD tools; aggregate effort. |
| TransferGPS_STA.lsp | AutoLISP | 3h | 1h | 4h | Transfers GPS stations to drawing attributes. |
| UpdateLength.lsp | AutoLISP | 2h | 1h | 3h | Auto-updating length attributes. |
| WATER_METER_ALIGN(WMA).LSP | AutoLISP | 8h | 2h | 10h | Water meter alignment tool. |
| Water Meter_Rotation_DFT.lsp | AutoLISP | 3h | 1h | 4h | Rotation/placement helper. |
| ftth_automation 1.zip | Zip/tool | 32h | 16h | 48h | Large automation package for FTTH tasks. |
| updatesheetnumbers.lsp | AutoLISP | 3h | 1h | 4h | Batch sheet-number updates. |
| updatesheetnumbers_ver1.lsp | AutoLISP | 4h | 1h | 5h | Variant/version with more logic. |

Caveats & how I estimated
- Estimations are based on file size, filename, and typical complexity for similar utilities (parsing, CAD placements, file I/O, UI prompts, edge-case handling).
- Zipped packages and larger Python tools were assumed multi-module and tested across sample datasets, hence much larger time.
- Testing time includes manual testing across representative datasets, edge-case fixes, and small iterations. It does not assume full QA cycles or integration testing with large CI setups.

Next steps
- I can also create an equivalent CSV file if you'd prefer that format for automation or sorting.
- If you want more accurate estimations, give me 2–3 representative examples per tool (dataset sizes, expected runtime, number of unique cases) and I will refine numbers.

-----

(End of file)