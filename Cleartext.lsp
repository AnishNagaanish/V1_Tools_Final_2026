(defun c:CleanText ( / ss i ent obj fullText routePos ductPos trimPos trimmedText)
  (setq ss (ssget "X" '((0 . "TEXT")))) ; Select all TEXT entities
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq fullText (vlax-get obj 'TextString))

        ;; Find positions of ROUTE#: and DUCT#:
        (setq routePos (vl-string-search "ROUTE#:" fullText))
        (setq ductPos (vl-string-search "DUCT#:" fullText))

        ;; Determine earliest position to trim
        (cond
          ((and routePos ductPos)
           (setq trimPos (min routePos ductPos)))
          (routePos
           (setq trimPos routePos))
          (ductPos
           (setq trimPos ductPos))
          (t
           (setq trimPos nil)) ; Neither found
        )

        ;; If either keyword found, trim text
        (if trimPos
          (progn
            (setq trimmedText (substr fullText 1 trimPos))
            ;; Update TEXT
            (vlax-put obj 'TextString trimmedText)
          )
        )
        (setq i (1+ i))
      )
    )
    (prompt "\nNo TEXT found.")
  )
  (princ)
)
