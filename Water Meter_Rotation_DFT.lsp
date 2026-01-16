(defun c:DFT ( / blkEnt blkObj blkName plEnt layName
                ssBlocks ssPlines plList
                en br pt nearest nearestPt cp vec ang
                i d distMin effOK)

  (vl-load-com)

  ;; Step 1: Select one block and get its EffectiveName
  (setq blkEnt (car (entsel "\nSelect one block: ")))
  (if (not blkEnt) (progn (princ "\nNo block selected.") (exit)))
  (setq blkObj (vlax-ename->vla-object blkEnt))
  (setq effOK (vlax-property-available-p blkObj 'EffectiveName))
  (setq blkName (if effOK (vla-get-EffectiveName blkObj)
                  (cdr (assoc 2 (entget blkEnt)))))

  ;; Step 2: Select one polyline and get its layer
  (setq plEnt (car (entsel "\nSelect one polyline: ")))
  (if (not plEnt) (progn (princ "\nNo polyline selected.") (exit)))
  (setq layName (cdr (assoc 8 (entget plEnt))))

  ;; Collect all blocks of that name
  (setq ssBlocks (ssget "X" (list (cons 0 "INSERT") (cons 2 blkName))))
  (if (not ssBlocks) (progn (princ (strcat "\nNo blocks found: " blkName)) (exit)))

  ;; Collect all polylines on that layer
  (setq ssPlines (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 8 layName))))
  (if (not ssPlines) (progn (princ (strcat "\nNo polylines on layer: " layName)) (exit)))

  ;; Build list of polyline objects
  (setq plList nil)
  (repeat (setq i (sslength ssPlines))
    (setq plList (cons (vlax-ename->vla-object (ssname ssPlines (setq i (1- i)))) plList))
  )

  ;; Step 3: Process each block of that name
  (repeat (setq i (sslength ssBlocks))
    (setq en (ssname ssBlocks (setq i (1- i))))
    (setq br (vlax-ename->vla-object en))
    (setq pt (cdr (assoc 10 (entget en)))) ; insertion point

    ;; Find nearest polyline
    (setq nearest nil distMin 1e99 nearestPt nil)
    (foreach pl plList
      (setq cp (vlax-curve-getClosestPointTo pl pt))
      (setq d (distance pt cp))
      (if (< d distMin)
        (setq distMin d nearest pl nearestPt cp)
      )
    )

    ;; If nearest polyline found, rotate block to face it
    (if nearest
      (progn
        ;; Vector from block → nearest point
        (setq vec (mapcar '- nearestPt pt))

        ;; Angle of that vector
        (setq ang (angle '(0.0 0.0) vec))

        ;; Because block’s 0° = North (+Y), add 90° offset
        (setq ang (+ ang (/ pi 2)))

        ;; If it looks reversed, flip 180°:
        ;; (setq ang (+ ang pi))

        ;; Normalize
        (setq ang (rem ang (* 2.0 pi)))
        (if (< ang 0.0) (setq ang (+ ang (* 2.0 pi))))

        ;; Apply rotation
        (vla-put-rotation br ang)
      )
    )
  )

  (princ "\nAll blocks of that type rotated to face nearest polyline.")
  (princ)
)
