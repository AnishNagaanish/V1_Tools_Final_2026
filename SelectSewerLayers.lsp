(defun c:SelectSewerLayers ( / ss keywordList sewerSS layerName ent i matchFound )
  ;; Define the keywords you want to match
  (setq keywordList '("SEWER" "LOW_PRESSURE" "STEP_TANK")) ; Add or change keywords here

  (setq sewerSS (ssadd)) ; Create an empty selection set
  (setq ss (ssget "X")) ; Get all entities in the drawing

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq layerName (strcase (cdr (assoc 8 (entget ent))))) ; Get layer name in uppercase
        (setq matchFound nil)

        ;; Check if layer name contains any of the keywords
        (foreach kw keywordList
          (if (wcmatch layerName (strcat "*" kw "*"))
            (setq matchFound T)
          )
        )

        ;; If match found, add entity to selection set
        (if matchFound
          (ssadd ent sewerSS)
        )

        (setq i (1+ i))
      )

      ;; Final selection
      (if (> (sslength sewerSS) 0)
        (progn
          (sssetfirst nil sewerSS)
          (princ (strcat "\nSelected " (itoa (sslength sewerSS)) " objects on matching layers."))
        )
        (princ "\nNo matching objects found.")
      )
    )
    (princ "\nNo entities found in drawing.")
  )
  (princ)
)
