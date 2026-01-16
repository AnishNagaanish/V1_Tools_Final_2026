(defun c:CWM ( / ssBlocks ssPolylines i blk blkpt closestPt minDist ent testPt dist)
  (prompt "\nSelect WATER METER blocks:")
  (setq ssBlocks (ssget '((0 . "INSERT")))) ; User selects blocks

  (prompt "\nSelect WATER MAIN polylines:")
  (setq ssPolylines (ssget '((0 . "LWPOLYLINE")))) ; User selects polylines

  (if (and ssBlocks ssPolylines)
    (progn
      (setq i 0)
      (while (< i (sslength ssBlocks))
        (setq blk (ssname ssBlocks i))
        (setq blkpt (cdr (assoc 10 (entget blk)))) ; Get block insertion point

        ;; Initialize minimum distance
        (setq minDist nil)
        (setq closestPt nil)

        ;; Loop through all selected polylines
        (setq j 0)
        (while (< j (sslength ssPolylines))
          (setq ent (ssname ssPolylines j))
          (setq testPt (vlax-curve-getClosestPointTo ent blkpt))
          (setq dist (distance blkpt testPt))

          ;; Update if this is the closest so far
          (if (or (not minDist) (< dist minDist))
            (progn
              (setq minDist dist)
              (setq closestPt testPt)
            )
          )
          (setq j (1+ j))
        )

        ;; Draw polyline from block to closest point
        (if closestPt
          (progn
            (command "_.LAYER" "_Make" "WATER MAIN" "")
            (command "_.PLINE" blkpt closestPt "")
          )
        )

        (setq i (1+ i))
      )
    )
    (prompt "\nSelection failed. Please try again.")
  )
  (princ)
)