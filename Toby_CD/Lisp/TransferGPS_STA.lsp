(defun c:TransferGPS_STA ()
  (setq selGPS (entsel "\nSelect GPS label: "))  ; Select GPS label
  (setq selTarget (entsel "\nSelect target label: "))  ; Select target label
  (setq selSTA (entsel "\nSelect STA label: "))  ; Select STA label
  (if (and selGPS selTarget selSTA)
    (progn
      ;; Get GPS text
      (setq gpsData (cdr (assoc 1 (entget (car selGPS)))))  
      ;; Extract coordinates from "GPS: lat, lon"
      (if (wcmatch gpsData "GPS: *")
        (setq gpsCoords (substr gpsData 6))  ; Extract lat/lon but keep "GPS:"
        (princ "\nInvalid GPS label format!")  ; Error handling
      )
      ;; Get STA text
      (setq staData (cdr (assoc 1 (entget (car selSTA)))))  
      ;; Get target label text
      (setq targetText (cdr (assoc 1 (entget (car selTarget)))))  
      
      ;; Preserve original text and only update GPS and STA values
      (setq newText (vl-string-subst (strcat "GPS: " gpsCoords) "GPS:" targetText))
      (setq newText (vl-string-subst (strcat "STA.  " staData) "STA." newText))

      ;; Update target label with new text
      (setq newEnt (subst (cons 1 newText) (assoc 1 (entget (car selTarget))) (entget (car selTarget))))
      (entmod newEnt)
      (princ "\nUpdated target label successfully!")
    )
  )
  (princ)
)
