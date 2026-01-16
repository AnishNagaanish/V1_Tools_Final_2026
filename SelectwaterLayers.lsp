(defun c:SelectwaterLayers ( / ss sewerSS layerName ent i )
  (setq sewerSS (ssadd)) ; Create an empty selection set

  (setq ss (ssget "X")) ; Get all entities in the drawing
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq layerName (cdr (assoc 8 (entget ent)))) ; Get layer name
        (if (and layerName (wcmatch (strcase layerName) "*WATER*"))
          (ssadd ent sewerSS)
        )
        (setq i (1+ i))
      )
      (if (> (sslength sewerSS) 0)
        (progn
          (sssetfirst nil sewerSS) ; Select the filtered entities
          (princ (strcat "\nSelected " (itoa (sslength sewerSS)) " objects on 'sewer' layers."))
        )
        (princ "\nNo objects found on layers containing 'sewer'.")
      )
    )
    (princ "\nNo entities found in drawing.")
  )
  (princ)
)
