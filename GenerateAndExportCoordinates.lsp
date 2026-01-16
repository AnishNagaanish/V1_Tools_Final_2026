(defun c:GenerateAndExportCoordinates ()
  ;; Ask user where to save the CSV file
  (setq filePath (getfiled "Save CSV File As" "C:/coordinates.csv" "csv" 1))
  (if filePath
    (progn
      ;; Open CSV file for writing
      (setq file (open filePath "w"))  
      (write-line "Text, X, Y" file)  ; Header row

      ;; Select an object to determine the target layer
      (prompt "\nSelect an object on the desired layer: ")
      (setq obj (car (entsel)))
      (if obj
        (progn
          (setq layerName (cdr (assoc 8 (entget obj))))  ; Get layer name
          (setq points (ssget "X" (list (cons 0 "POINT") (cons 8 layerName))))
          (setq blocks (ssget "X" (list (cons 0 "INSERT") (cons 8 layerName))))
          
          ;; Process Points and Blocks
          (if (or points blocks)
            (progn
              (if points
                (repeat (sslength points)
                  (setq point (ssname points 0))
                  (setq coords (cdr (assoc 10 (entget point))))
                  (setq text (strcat "GPS: " (rtos (car coords) 2 6) "," (rtos (cadr coords) 2 6)))
                  
                  ;; Create text label
                  (entmake (list (cons 0 "TEXT") (cons 10 coords) (cons 40 2.5) (cons 1 text) (cons 7 "Standard")))

                  ;; Write to CSV file
                  (write-line (strcat text "," (rtos (car coords) 2 6) "," (rtos (cadr coords) 2 6)) file)
                  
                  (setq points (ssdel point points))
                )
              )
              (if blocks
                (repeat (sslength blocks)
                  (setq block (ssname blocks 0))
                  (setq coords (cdr (assoc 10 (entget block))))
                  (setq text (strcat "GPS: " (rtos (car coords) 2 6) "," (rtos (cadr coords) 2 6)))
                  
                  ;; Create text label
                  (entmake (list (cons 0 "TEXT") (cons 10 coords) (cons 40 2.5) (cons 1 text) (cons 7 "Standard")))

                  ;; Write to CSV file
                  (write-line (strcat text "," (rtos (car coords) 2 6) "," (rtos (cadr coords) 2 6)) file)
                  
                  (setq blocks (ssdel block blocks))
                )
              )
              (prompt "\nCoordinates placed and exported successfully!")
            )
            (prompt "\nNo points or blocks found on the selected layer.")
          )
        )
        (prompt "\nNo object selected.")
      )
      (close file)
    )
    (princ "\nExport cancelled.")
  )
  (princ)
)
