(defun c:Line_Poly ( / ent lay ss i obj)
  (prompt "\nSelect one object from the target layer...")
  (setq ent (entsel))
  (if ent
    (progn
      (setq obj (entget (car ent)))
      (setq lay (cdr (assoc 8 obj))) ; Get layer name
      (prompt (strcat "\nConverting all LINEs on layer: " lay))
      (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 lay)))) ; Select all LINEs on that layer
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (command "._PEDIT" ent "Y" "Join" ent "" "")
            (setq i (1+ i))
          )
          (prompt "\nConversion complete.")
        )
        (prompt "\nNo LINEs found on that layer.")
      )
    )
    (prompt "\nNo object selected.")
  )
  (princ)
)
