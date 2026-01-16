(defun c:DHATCHLAYER (/ selObj layerName ss i ent)
  (setq selObj (car (entsel "\nSelect a polygon on the target layer: ")))
  (if selObj
    (progn
      (setq layerName (cdr (assoc 8 (entget selObj)))) ; Get layer name
      (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 8 layerName)))) ; Select all polylines on that layer
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (command "_.-HATCH"
                     "_P" ; Predefined
                     "AR-CONC" ; Pattern name
                     "1" ; Scale
                     "0" ; Angle
                     "_S" ; Select objects
                     ent
                     ""
                     ""
            )
            (setq i (1+ i))
          )
          (princ "\n Hatching completed for all polygons on the selected layer.")
        )
        (princ "\n No closed polylines found on the selected layer.")
      )
    )
    (princ "\n No object selected.")
  )
  (princ)
)