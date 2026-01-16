;; Align every "ANCHOR" block to the nearest "EXISTING POLE" block by base point.
;; Optional: set MATCH_ROTATION to T to copy pole rotation to anchor.
;;
;; Usage: type ALIGNANCHOR2POLE in the command line.

(defun c:AlignAnchor2Pole ( / blkRef blkMov ssRef ssMov
                             i j nRef nMov entMov entRef
                             pMov pRef bestRef bestDist d
                             oldCmd oldSnap matchRot rotRef eData eNew)

  (vl-load-com)

  ;; ----------------------------
  ;; Settings (edit as needed)
  ;; ----------------------------
  (setq blkRef "EXISTING POLE") ; reference block name (target)
  (setq blkMov "ANCHOR")        ; block to move (source)
  (setq matchRot nil)           ; set to T if you want to match rotation

  ;; ----------------------------
  ;; Helpers
  ;; ----------------------------
  (defun _inspt (e) (cdr (assoc 10 (entget e))))
  (defun _rotation (e)
    (cond ((cdr (assoc 50 (entget e))))
          (0.0)))
  (defun _dist (p1 p2) (distance p1 p2))

  ;; ----------------------------
  ;; Select inserts (Model space)
  ;; ----------------------------
  (setq ssRef (ssget "X"
                     (list (cons 0 "INSERT")
                           (cons 2 blkRef)
                           (cons 410 "Model"))))
  (setq ssMov (ssget "X"
                     (list (cons 0 "INSERT")
                           (cons 2 blkMov)
                           (cons 410 "Model"))))

  (cond
    ((null ssRef) (princ (strcat "\nNo inserts found for reference block: " blkRef)))
    ((null ssMov) (princ (strcat "\nNo inserts found for moving block: " blkMov)))
    (T
      (setq oldCmd  (getvar 'CMDECHO))
      (setq oldSnap (getvar 'OSMODE))
      (setvar 'CMDECHO 0)
      (setvar 'OSMODE 0)
      (command "_.UNDO" "_Begin")

      (setq nRef (sslength ssRef))
      (setq nMov (sslength ssMov))

      (princ (strcat "\nFound " (itoa nMov) " \"" blkMov "\" and "
                      (itoa nRef) " \"" blkRef "\"."))

      (setq i 0)
      (while (< i nMov)
        (setq entMov (ssname ssMov i))
        (setq pMov   (_inspt entMov))

        ;; find nearest reference pole
        (setq j 0
              bestRef nil
              bestDist nil)
        (while (< j nRef)
          (setq entRef (ssname ssRef j))
          (setq pRef   (_inspt entRef))
          (setq d     (_dist pMov pRef))
          (if (or (null bestDist) (< d bestDist))
            (progn (setq bestDist d)
                   (setq bestRef  entRef)))
          (setq j (1+ j))
        )

        (if bestRef
          (progn
            (setq pRef (_inspt bestRef))
            ;; Move anchor from its base point to the pole base point
            (command "_.MOVE" entMov "" pMov pRef)

            ;; Optionally match rotation
            (if matchRot
              (progn
                (setq rotRef (_rotation bestRef))
                (setq eData (entget entMov))
                (if (assoc 50 eData)
                  (setq eNew (subst (cons 50 rotRef) (assoc 50 eData) eData))
                  (setq eNew (append eData (list (cons 50 rotRef)))))
                (entmod eNew) (entupd entMov)
              )
            )
          )
        )

        (setq i (1+ i))
      )

      (command "_.UNDO" "_End")
      (setvar 'OSMODE oldSnap)
      (setvar 'CMDECHO oldCmd)
      (princ "\nDone: Anchors aligned to nearest poles.")
    )
  )
  (princ)
)