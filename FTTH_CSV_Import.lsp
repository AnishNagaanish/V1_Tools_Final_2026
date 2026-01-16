;;; ============================================================
;;; FTTH TEXT IMPORTER - FIXED VERSION
;;; ============================================================
;;; Imports MText labels from FTTH CSV
;;; Type: FTTH to run
;;; 
;;; FIXES:
;;; - Added DROP_LABEL layer (was missing)
;;; - Better coordinate validation
;;; - Debug output option
;;; ============================================================

(vl-load-com)

(defun c:FTTH (/ csvFile fp line data x y text layer count startTime endTime minX minY maxX maxY firstCoord)
  
  (princ "\n============================================")
  (princ "\n   FTTH TEXT IMPORTER - FIXED")
  (princ "\n============================================")
  (princ "\n   IMPORTANT: Make sure your drawing units")
  (princ "\n   are set to US Survey Feet for EPSG:2236")
  (princ "\n============================================")
  
  ;; Select CSV file
  (setq csvFile (getfiled "Select FTTH CSV File" "" "csv" 0))
  
  (if (not csvFile)
    (princ "\n*** Cancelled ***")
    (progn
      (setq fp (open csvFile "r"))
      
      (if (not fp)
        (princ "\n*** ERROR: Cannot open file ***")
        (progn
          (setq startTime (getvar "MILLISECS"))
          (setvar "CMDECHO" 0)
          
          (princ (strcat "\nReading: " csvFile))
          
          ;; Create ALL layers including DROP_LABEL
          (FTTH-MakeLayer "DAP_LABEL" 1)      ; Red
          (FTTH-MakeLayer "TOBY_LABEL" 3)     ; Green
          (FTTH-MakeLayer "BORE_LABEL" 5)     ; Blue
          (FTTH-MakeLayer "DROP_LABEL" 4)     ; Cyan - THIS WAS MISSING!
          (FTTH-MakeLayer "PIT_LABEL" 6)      ; Magenta
          
          ;; Skip header line
          (read-line fp)
          
          (princ "\nImporting text...")
          (setq count 0)
          (setq firstCoord T)
          (setq minX nil minY nil maxX nil maxY nil)
          
          ;; Process each line
          (while (setq line (read-line fp))
            (if (> (strlen line) 10)
              (progn
                (setq data (FTTH-ParseLine line))
                
                (if (and data (>= (length data) 8))
                  (progn
                    ;; Get coordinates and text
                    (setq x     (atof (nth 2 data)))
                    (setq y     (atof (nth 3 data)))
                    (setq text  (nth 6 data))
                    (setq layer (nth 7 data))
                    
                    ;; Validate coordinates (FL State Plane East typically has large values)
                    (if (and (> x 100000) (> y 100000))
                      (progn
                        ;; Track extents for zoom
                        (if firstCoord
                          (progn
                            (setq minX x maxX x minY y maxY y)
                            (setq firstCoord nil)
                            ;; Show first coordinate for debugging
                            (princ (strcat "\n  First point: X=" (rtos x 2 2) " Y=" (rtos y 2 2)))
                          )
                          (progn
                            (if (< x minX) (setq minX x))
                            (if (> x maxX) (setq maxX x))
                            (if (< y minY) (setq minY y))
                            (if (> y maxY) (setq maxY y))
                          )
                        )
                        
                        ;; Clean the text
                        (setq text (FTTH-CleanText text))
                        
                        ;; Create MText at location
                        (if (> (strlen text) 0)
                          (progn
                            (FTTH-CreateMText x y text layer 2.5)
                            (setq count (1+ count))
                          )
                        )
                      )
                      ;; Debug: show invalid coordinates
                      (if (or (> x 0) (> y 0))
                        (princ (strcat "\n  WARNING: Small coords X=" (rtos x 2 2) " Y=" (rtos y 2 2) " - may be wrong CRS"))
                      )
                    )
                  )
                )
              )
            )
          )
          
          (close fp)
          
          ;; Zoom to extents if we have valid data
          (if (and minX maxX minY maxY (> count 0))
            (progn
              (princ (strcat "\n\n  Coordinate Range:"))
              (princ (strcat "\n    X: " (rtos minX 2 2) " to " (rtos maxX 2 2)))
              (princ (strcat "\n    Y: " (rtos minY 2 2) " to " (rtos maxY 2 2)))
              ;; Zoom with padding
              (command "_.ZOOM" "W" 
                (list (- minX 100) (- minY 100)) 
                (list (+ maxX 100) (+ maxY 100)))
            )
            (command "_.ZOOM" "E")
          )
          
          (setq endTime (getvar "MILLISECS"))
          
          ;; Summary
          (princ "\n")
          (princ "\n============================================")
          (princ "\n   COMPLETE!")
          (princ "\n============================================")
          (princ (strcat "\n   Text labels imported: " (itoa count)))
          (princ (strcat "\n   Time: " (rtos (/ (- endTime startTime) 1000.0) 2 2) " seconds"))
          (princ "\n")
          (princ "\n   If labels are not visible or in wrong location:")
          (princ "\n   1. Check that QGIS output CRS is EPSG:2236")
          (princ "\n   2. Check AutoCAD drawing units (should be Feet)")
          (princ "\n   3. Type ZOOM E to zoom to extents")
          (princ "\n============================================")
        )
      )
    )
  )
  (princ)
)

;;; ============================================================
;;; DEBUG COMMAND - Show first 5 lines of CSV
;;; ============================================================
(defun c:FTTH-DEBUG (/ csvFile fp line count)
  (princ "\n=== CSV DEBUG ===")
  (setq csvFile (getfiled "Select FTTH CSV File" "" "csv" 0))
  (if csvFile
    (progn
      (setq fp (open csvFile "r"))
      (setq count 0)
      (while (and (setq line (read-line fp)) (< count 6))
        (princ (strcat "\nLine " (itoa count) ": " line))
        (if (> count 0)
          (progn
            (setq data (FTTH-ParseLine line))
            (princ (strcat "\n  X=" (nth 2 data) " Y=" (nth 3 data) " Layer=" (nth 7 data)))
          )
        )
        (setq count (1+ count))
      )
      (close fp)
    )
  )
  (princ)
)

;;; ============================================================
;;; CREATE LAYER
;;; ============================================================
(defun FTTH-MakeLayer (name color)
  (if (not (tblsearch "LAYER" name))
    (progn
      (entmake 
        (list
          '(0 . "LAYER")
          '(100 . "AcDbSymbolTableRecord")
          '(100 . "AcDbLayerTableRecord")
          (cons 2 name)
          (cons 62 color)
          '(70 . 0)
        )
      )
      (princ (strcat "\n  Created layer: " name))
    )
  )
)

;;; ============================================================
;;; CREATE MTEXT
;;; ============================================================
(defun FTTH-CreateMText (x y text layer height)
  (entmake
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbMText")
      (cons 10 (list x y 0.0))
      (cons 40 height)
      (cons 41 0.0)
      '(71 . 1)
      '(72 . 1)
      (cons 1 text)
    )
  )
)

;;; ============================================================
;;; PARSE CSV LINE - Handle quoted fields properly
;;; ============================================================
(defun FTTH-ParseLine (line / result current i char inQuote)
  (setq result '())
  (setq current "")
  (setq inQuote nil)
  (setq i 1)
  
  (while (<= i (strlen line))
    (setq char (substr line i 1))
    
    (cond
      ((= char "\"")
       (setq inQuote (not inQuote)))
      
      ((and (= char ",") (not inQuote))
       (setq result (append result (list current)))
       (setq current ""))
      
      ((and (= char "\t") (not inQuote))
       (setq result (append result (list current)))
       (setq current ""))
      
      (T
       (setq current (strcat current char)))
    )
    (setq i (1+ i))
  )
  
  (append result (list current))
)

;;; ============================================================
;;; CLEAN TEXT
;;; ============================================================
(defun FTTH-CleanText (text / result)
  (setq result text)
  ;; \P is already correct MText line break format
  result
)

;;; ============================================================
;;; LOAD MESSAGE
;;; ============================================================
(princ "\n")
(princ "\n============================================")
(princ "\n   FTTH TEXT IMPORTER - FIXED VERSION")
(princ "\n============================================")
(princ "\n   Commands:")
(princ "\n   • FTTH       - Import CSV labels")
(princ "\n   • FTTH-DEBUG - Show CSV contents")
(princ "\n")
(princ "\n   Make sure your drawing is set to")
(princ "\n   FL State Plane East (Feet) - EPSG:2236")
(princ "\n============================================")
(princ)