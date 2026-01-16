(defun c:GP_Convert ( / ss i ent entData layerName insPt scale)
  (setq scale 1.0) ; Set desired block scale

  ;; Select all entities in the drawing
  (setq ss (ssget "X"))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq layerName (cdr (assoc 8 entData))) ; Get layer name

        ;; Check if layer name ends with "STEP_TANK"
        (if (wcmatch layerName "*GRINDER_TANK")
          (progn
            ;; Try to get insertion point or first vertex
            (cond
              ((assoc 10 entData) (setq insPt (cdr (assoc 10 entData)))) ; For blocks/text
              ((assoc 11 entData) (setq insPt (cdr (assoc 11 entData)))) ; For lines
              (T (setq insPt '(0 0 0))) ; Default if no point found
            )
            (entdel ent) ; Delete original entity
            (command "_.-INSERT" "GP" insPt scale scale 0) ; Insert block
          )
        )
        (setq i (1+ i))
      )
    )
    (prompt "\nNo entities found.")
  )
  (princ)
)
