
;;; ============================================================
;;; FTTH AUTO CALLOUTS LISP - BORE > BLOCK > BORE SEQUENCE
;;; With HANDHOLE Auto-Detection + Nearest-ROW Lock + Avoid Layers
;;; ============================================================

(vl-load-com)

(if (not counter) (setq counter 1))

;;; ============================================================
;;; CALLOUT ORDER - CHANGE AS NEEDED
;;; ============================================================
(setq *CALLOUT-ORDER* (list
  "BORE"                    ; 1st
  "TOBY BOX"               ; 2nd
  "DAP (HAND DIG)"         ; 3rd
  "BORE PIT"               ; 4th
  "PROPOSED HH WITH FDH"   ; 5th - Handhole
  "GROUND ROD"             ; 6th
))

;;; ============================================================
;;; SETTINGS
;;; ============================================================
(setq *CALLOUT-HEX-RADIUS* 3.8)
(setq *CALLOUT-TEXT-HEIGHT* 3.0)
(setq *CALLOUT-ARROW-SIZE* 2.5)
(setq *CALLOUT-OFFSET* 20.0)
(setq *BLOCK-NEAR-DIST* 15.0)
(setq *BORE-CONNECT-DIST* 5.0)
(setq *DAP-BOREPIT-TOLERANCE* 2.0)
(setq *HANDHOLE-TOLERANCE* 15.0)

;;; Arrow connection option: T = connect to SIDE, nil = connect to CENTER
(setq *CONNECT-TO-SIDE* nil)
(setq *BLOCK-SIDE-OFFSET* 3.0)  ; Distance from center to side of block

;;; Layer names
(setq *BORE-LAYER* "BORE")
(setq *DROP-LAYER* "Drop")

;;; NEW: Lock to picked ROW side + Avoid layers
(setq *LOCK-TO-PICKED-ROW* T)    ; honor the ROW the user clicked for the selected ROW
(setq *AVOID-LAYERS* '("Right Of Way" "EDGE OF PAVEMENT" "DITCH LINE" "2D_Centre Line_lines" "Property Line" "Station Values"))
(setq *AVOID-DIST* 6.0)          ; buffer distance from avoid layers
(setq *AVOID-CURVES* '())

;;; Global storage
(setq *ROW-SIDES* '())           ; list of (rowEnt . sidePickPoint)
(setq *PLACED-CALLOUTS* '())
(setq *DROP-POLYLINES* '())
(setq *PROCESSED-BORES* '())
(setq *PROCESSED-BLOCKS* '())

;;; ============================================================
;;; UTILITY FUNCTIONS
;;; ============================================================

(defun GetBoundingBox (ent / obj minPt maxPt)
  (setq obj (vlax-ename->vla-object ent))
  (if (not (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPt 'maxPt))))
    (list (vlax-safearray->list minPt) (vlax-safearray->list maxPt))
    nil
  )
)

(defun PointInsideBBox (pt bbox / minPt maxPt)
  (if (and pt bbox (car bbox) (cadr bbox))
    (progn
      (setq minPt (car bbox))
      (setq maxPt (cadr bbox))
      (and (>= (car pt) (car minPt))
           (<= (car pt) (car maxPt))
           (>= (cadr pt) (cadr minPt))
           (<= (cadr pt) (cadr maxPt)))
    )
    nil
  )
)

(defun BBoxOverlap (bbox1 bbox2 / min1 max1 min2 max2)
  (if (and bbox1 bbox2)
    (progn
      (setq min1 (car bbox1) max1 (cadr bbox1))
      (setq min2 (car bbox2) max2 (cadr bbox2))
      (and (<= (car min1) (car max2))
           (>= (car max1) (car min2))
           (<= (cadr min1) (cadr max2))
           (>= (cadr max1) (cadr min2)))
    )
    nil
  )
)

(defun GetBlockCenter (blkEnt / obj insPt)
  (if blkEnt
    (progn
      (setq obj (vlax-ename->vla-object blkEnt))
      (setq insPt (vl-catch-all-apply 'vlax-get-property (list obj 'InsertionPoint)))
      (if (and insPt (not (vl-catch-all-error-p insPt)))
        (progn
          (if (= (type insPt) 'variant)
            (setq insPt (vlax-safearray->list (vlax-variant-value insPt)))
          )
          (list (car insPt) (cadr insPt))
        )
        nil
      )
    )
    nil
  )
)

(defun GetPlineMidpoint (plineEnt / len pt)
  (setq len (vlax-curve-getDistAtParam plineEnt (vlax-curve-getEndParam plineEnt)))
  (setq pt (vlax-curve-getPointAtDist plineEnt (/ len 2.0)))
  (if pt (list (car pt) (cadr pt)) nil)
)

(defun GetPlineStartPt (plineEnt / pt)
  (setq pt (vlax-curve-getStartPoint plineEnt))
  (if pt (list (car pt) (cadr pt)) nil)
)

(defun GetPlineEndPt (plineEnt / pt)
  (setq pt (vlax-curve-getEndPoint plineEnt))
  (if pt (list (car pt) (cadr pt)) nil)
)

;;; ============================================================
;;; GET BORE MIDPOINT INSIDE GRID
;;; ============================================================
(defun GetBoreMidpointInGrid (boreEnt gridBBox / len step i pt firstDist lastDist midDist midPt)
  (setq len (vlax-curve-getDistAtParam boreEnt (vlax-curve-getEndParam boreEnt)))
  (setq step (/ len 20.0))
  (setq firstDist nil)
  (setq lastDist nil)
  (setq i 0.0)
  
  (while (<= i len)
    (setq pt (vlax-curve-getPointAtDist boreEnt i))
    (if (and pt (PointInsideBBox (list (car pt) (cadr pt)) gridBBox))
      (progn
        (if (null firstDist) (setq firstDist i))
        (setq lastDist i)
      )
    )
    (setq i (+ i step))
  )
  
  (if (and firstDist lastDist)
    (progn
      (setq midDist (/ (+ firstDist lastDist) 2.0))
      (setq midPt (vlax-curve-getPointAtDist boreEnt midDist))
      (if midPt
        (list (car midPt) (cadr midPt))
        (GetPlineMidpoint boreEnt)
      )
    )
    (GetPlineMidpoint boreEnt)
  )
)

;;; ============================================================
;;; CHECK IF BORE TOUCHES GRID
;;; ============================================================
(defun BoreTouchesGrid (boreEnt gridBBox / startPt endPt midPt boreBBox len step i pt result)
  (setq result nil)
  (setq startPt (GetPlineStartPt boreEnt))
  (setq endPt (GetPlineEndPt boreEnt))
  (setq midPt (GetPlineMidpoint boreEnt))
  (setq boreBBox (GetBoundingBox boreEnt))
  
  (if (or (and startPt (PointInsideBBox startPt gridBBox))
          (and endPt (PointInsideBBox endPt gridBBox))
          (and midPt (PointInsideBBox midPt gridBBox))
          (BBoxOverlap boreBBox gridBBox))
    (setq result T)
    (progn
      (setq len (vlax-curve-getDistAtParam boreEnt (vlax-curve-getEndParam boreEnt)))
      (setq step (/ len 10.0))
      (setq i 0.0)
      (while (and (<= i len) (not result))
        (setq pt (vlax-curve-getPointAtDist boreEnt i))
        (if (and pt (PointInsideBBox (list (car pt) (cadr pt)) gridBBox))
          (setq result T)
        )
        (setq i (+ i step))
      )
    )
  )
  result
)

;;; ============================================================
;;; ROW SIDE FUNCTIONS
;;; ============================================================

;;; Nearest ROW by curve (and return its associated picked side point)
(defun GetClosestROWSide (pt / closestEnt closestDist closestSide ent dist pair closestPt)
  (setq closestEnt nil closestDist 1e20 closestSide nil)
  (foreach pair *ROW-SIDES*
    (setq ent (car pair))
    (setq closestPt (vlax-curve-getClosestPointTo ent pt))
    (if closestPt
      (progn
        (setq dist (distance pt closestPt))
        (if (< dist closestDist)
          (setq closestDist dist closestEnt ent closestSide (cdr pair))
        )
      )
    )
  )
  (cons closestEnt closestSide)
)

;;; (keep) choose ROW by the PICKED side point proximity (not used now, but kept for reference)
(defun GetRowSideByPick (basePt / closestPair closestDist pair sidePt d)
  (setq closestPair nil
        closestDist 1e20)
  (foreach pair *ROW-SIDES*
    (setq sidePt (cdr pair))
    (if sidePt
      (progn
        (setq d (distance basePt sidePt))
        (if (< d closestDist)
          (setq closestDist d
                closestPair pair)
        )
      )
    )
  )
  closestPair
)

(defun GetPerpAngle (objPt rowEnt / closestPt param deriv)
  (setq closestPt (vlax-curve-getClosestPointTo rowEnt objPt))
  (if closestPt
    (progn
      (setq param (vlax-curve-getParamAtPoint rowEnt closestPt))
      (setq deriv (vlax-curve-getFirstDeriv rowEnt param))
      (+ (angle '(0 0) deriv) (/ pi 2.0))
    )
    0.0
  )
)

;;; Perpendicular toward the user's picked side
(defun PerpTowardSide (basePt rowEnt sidePt / cp param deriv baseAng angA angB)
  (if (and basePt rowEnt)
    (progn
      (setq cp    (vlax-curve-getClosestPointTo rowEnt basePt))
      (if (not cp) (setq cp (vlax-curve-getClosestPointTo rowEnt sidePt)))
      (if cp
        (progn
          (setq param (vlax-curve-getParamAtPoint rowEnt cp))
          (setq deriv (vlax-curve-getFirstDeriv rowEnt param))
          (setq baseAng (+ (angle '(0 0) deriv) (/ pi 2.0)))
          ;; pick the perpendicular that is closer to sidePt
          (setq angA baseAng)
          (setq angB (+ baseAng pi))
          (if (and sidePt
                   (> (distance (polar basePt angA 10.0) sidePt)
                      (distance (polar basePt angB 10.0) sidePt)))
            (setq baseAng angB)
          )
          baseAng
        )
        0.0
      )
    )
    0.0
  )
)

;;; STRICT: testPt must be on the same signed side of ROW tangent as sidePt
(defun IsPointOnPickedSide (rowEnt sidePt testPt / cp param deriv tan vecSide vecTest signSide signTest)
  (if (and rowEnt sidePt testPt)
    (progn
      (setq cp    (vlax-curve-getClosestPointTo rowEnt testPt))
      (if (not cp) (setq cp (vlax-curve-getClosestPointTo rowEnt sidePt)))
      (if cp
        (progn
          (setq param (vlax-curve-getParamAtPoint rowEnt cp))
          (setq deriv (vlax-curve-getFirstDeriv rowEnt param))
          (setq tan (list (car deriv) (cadr deriv)))
          (setq vecSide (list (- (car sidePt) (car cp))
                              (- (cadr sidePt) (cadr cp))))
          (setq vecTest (list (- (car testPt) (car cp))
                              (- (cadr testPt) (cadr cp))))
          ;; 2D cross product z = a_x*b_y - a_y*b_x
          (setq signSide (- (* (car tan) (cadr vecSide))
                            (* (cadr tan) (car vecSide))))
          (setq signTest (- (* (car tan) (cadr vecTest))
                            (* (cadr tan) (car vecTest))))
          ;; STRICT: reject near-zero signs to avoid ambiguity
          (if (and (> (abs signSide) 1e-4) (> (abs signTest) 1e-4))
            (if (> (* signSide signTest) 0.0) T nil)
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

;;; ============================================================
;;; OVERLAP CHECKING
;;; ============================================================

(defun CheckOverlap (pt minDist / overlap existPt)
  (setq overlap nil)
  (foreach existPt *PLACED-CALLOUTS*
    (if (and pt existPt (< (distance pt existPt) minDist))
      (setq overlap T)
    )
  )
  overlap
)

(defun CheckDropOverlap (pt minDist / overlap dropEnt closestPt)
  (setq overlap nil)
  (foreach dropEnt *DROP-POLYLINES*
    (if (and (not overlap) pt)
      (progn
        (setq closestPt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list dropEnt pt)))
        (if (and closestPt (not (vl-catch-all-error-p closestPt)))
          (if (< (distance pt closestPt) minDist)
            (setq overlap T)
          )
        )
      )
    )
  )
  overlap
)

;;; Avoid-layers loader and checker
(defun LoadAvoidCurves (/ ss i e)
  (setq *AVOID-CURVES* '())
  (if *AVOID-LAYERS*
    (foreach lay *AVOID-LAYERS*
      ;; NOTE: ssget filter accepts wildcards; this pattern covers common curve types
      (setq ss (ssget "X" (list (cons 8 lay) (cons 0 "LINE,ARC,LWPOLYLINE,POLYLINE,SPLINE"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq e (ssname ss i))
            (setq *AVOID-CURVES* (cons e *AVOID-CURVES*))
            (setq i (1+ i))
          )
        )
      )
    )
  )
)

(defun CheckAvoidOverlap (pt / hit curve cpt)
  (setq hit nil)
  (foreach curve *AVOID-CURVES*
    (if (and (not hit) pt)
      (progn
        (setq cpt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list curve pt)))
        (if (and cpt (not (vl-catch-all-error-p cpt)))
          (if (< (distance pt cpt) *AVOID-DIST*)
            (setq hit T)
          )
        )
      )
    )
  )
  hit
)

;;; ============================================================
;;; FIND NON-OVERLAP POSITION (CORRECT SIDE) - SAFE VERSION
;;; ============================================================

(defun ValidCandidate (p gridBox minDist dropMinDist rowEnt sidePt)
  (and p
       (PointInsideBBox p gridBox)
       (not (CheckOverlap p minDist))
       (not (CheckDropOverlap p dropMinDist))
       (not (CheckAvoidOverlap p))
       (or (not *LOCK-TO-PICKED-ROW*)
           (and rowEnt sidePt (IsPointOnPickedSide rowEnt sidePt p)))))

;;; Safe placement:
;;;  - Chooses ROW by nearest CURVE (GetClosestROWSide)
;;;  - Honors the user's clicked side for THAT ROW (half-plane constraint)
;;;  - Avoids Drop and configured avoid-layers
;;;  - Robust fallback only along the allowed side ray
(defun FindNonOverlapPosition (basePt gridEnt offset minDist / 
  rowPair rowEnt sidePt perpAng testPt gridBox dropMinDist
  baseAng maxAngleDev tryDist i j found ok)
  
  (if (null basePt) (setq basePt '(0 0)))
  
  (setq gridBox     (GetBoundingBox gridEnt))
  (setq dropMinDist (* *CALLOUT-HEX-RADIUS* 1.5))
  (setq maxAngleDev (/ pi 2.0))
  (setq found       nil)
  (setq ok          nil)
  
  ;; >>> Choose ROW by NEAREST CURVE to the anchor point
  (setq rowPair (GetClosestROWSide basePt))    ;; returns (rowEnt . sidePt)
  (if rowPair
    (progn
      (setq rowEnt (car rowPair))
      (setq sidePt (cdr rowPair))
      (setq perpAng (PerpTowardSide basePt rowEnt sidePt))
      (setq baseAng perpAng)
    )
    (progn
      (setq baseAng 0.0)
      (setq perpAng 0.0)
    )
  )
  
  ;; 1) Angle sweep with expanding radius (wider search)
  (setq j 1)
  (while (and (not found) (<= j 18))    ;; more rings for robustness
    (setq tryDist (* offset j 0.7))
    (setq i 0)
    (while (and (not found) (<= i 28))  ;; more samples per ring
      (if (= (rem i 2) 0)
        (setq testPt (polar basePt (+ baseAng (* (/ i 2) (/ pi 14.0))) tryDist))
        (setq testPt (polar basePt (- baseAng (* (/ (1+ i) 2) (/ pi 14.0))) tryDist))
      )
      (if (ValidCandidate testPt gridBox minDist dropMinDist rowEnt sidePt)
        (setq found testPt))
      (setq i (1+ i))
    )
    (setq j (1+ j))
  )
  
  ;; 2) If still not found, walk only along baseAng ray toward picked side
  (if (not found)
    (progn
      (setq i 1)
      (while (and (not found) (<= i 40)) ;; extend up to ~28*offset distance
        (setq tryDist (* offset i 0.5))
        (setq testPt (polar basePt baseAng tryDist))
        (if (ValidCandidate testPt gridBox minDist dropMinDist rowEnt sidePt)
          (setq found testPt))
        (setq i (1+ i))
      )
    )
  )
  
  ;; 3) Final fallback: nudge minimal distance toward side; never violate constraints
  (if (not found)
    (progn
      (setq i 1 ok nil)
      (while (and (not ok) (<= i 15))
        (setq testPt (polar basePt baseAng (* 3.0 i))) ;; small steps
        (if (ValidCandidate testPt gridBox minDist dropMinDist rowEnt sidePt)
          (progn
            (setq found testPt)
            (setq ok T)
          )
        )
        (setq i (1+ i))
      )
    )
  )
  
  ;; As a last resort (rare), if nothing passes, keep a tiny offset on side ray
  (if (not found)
    (setq found (polar basePt baseAng 5.0)))
  
  found
)

;;; ============================================================
;;; GET ARROW CONNECTION POINT (SIDE OR CENTER)
;;; ============================================================

(defun GetArrowPoint (blockPt calloutPt / ang sidePt)
  (if (and *CONNECT-TO-SIDE* blockPt calloutPt)
    (progn
      ;; Calculate angle from block to callout
      (setq ang (angle blockPt calloutPt))
      ;; Get point on side of block (toward callout)
      (setq sidePt (polar blockPt ang *BLOCK-SIDE-OFFSET*))
      sidePt
    )
    ;; Return center if option disabled or points invalid
    blockPt
  )
)

;;; ============================================================
;;; GET ARROW CONNECTION POINT FOR BORE (always center/midpoint)
;;; ============================================================

(defun GetBoreArrowPoint (boreMidPt calloutPt)
  ;; For BORE, always connect to midpoint (no side offset)
  boreMidPt
)

;;; ============================================================
;;; FIND HANDHOLE GROUP IN GRID (AUTO-DETECT)
;;; ============================================================

(defun FindHandholeGroup (gridBBox / ss ent hhPt hhEnt i borePitEnt groundRodEnt borePitPt groundRodPt result tmpPt)
  (setq result nil)
  (setq hhEnt nil)
  (setq hhPt nil)
  
  ;; Find PROPOSED HH WITH FDH in grid
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "PROPOSED HH WITH FDH"))))
  (if ss
    (progn
      (princ (strcat "\n  Found " (itoa (sslength ss)) " PROPOSED HH WITH FDH in drawing"))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq tmpPt (GetBlockCenter ent))
        (if (and tmpPt (PointInsideBBox tmpPt gridBBox))
          (progn
            (setq hhEnt ent)
            (setq hhPt tmpPt)
            (princ "\n  >>> HANDHOLE found INSIDE grid!")
          )
        )
        (setq i (1+ i))
      )
    )
  )
  
  ;; If HANDHOLE found in grid
  (if (and hhEnt hhPt)
    (progn
      ;; Find BORE PIT nearby
      (setq borePitEnt nil)
      (setq borePitPt nil)
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "BORE PIT"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq tmpPt (GetBlockCenter ent))
            (if (and tmpPt 
                     (< (distance hhPt tmpPt) *HANDHOLE-TOLERANCE*)
                     (PointInsideBBox tmpPt gridBBox))
              (progn
                (setq borePitEnt ent)
                (setq borePitPt tmpPt)
                (princ "\n  >>> BORE PIT found near HANDHOLE!")
              )
            )
            (setq i (1+ i))
          )
        )
      )
      
      ;; Find GROUND ROD nearby
      (setq groundRodEnt nil)
      (setq groundRodPt nil)
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "GROUND ROD"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq tmpPt (GetBlockCenter ent))
            (if (and tmpPt 
                     (< (distance hhPt tmpPt) *HANDHOLE-TOLERANCE*)
                     (PointInsideBBox tmpPt gridBBox))
              (progn
                (setq groundRodEnt ent)
                (setq groundRodPt tmpPt)
                (princ "\n  >>> GROUND ROD found near HANDHOLE!")
              )
            )
            (setq i (1+ i))
          )
        )
      )
      
      ;; Build result
      (setq result (list
        (if (and borePitEnt borePitPt) (list borePitEnt borePitPt "BORE PIT") nil)
        (list hhEnt hhPt "PROPOSED HH WITH FDH")
        (if (and groundRodEnt groundRodPt) (list groundRodEnt groundRodPt "GROUND ROD") nil)
      ))
    )
  )
  result
)

;;; ============================================================
;;; CHECK IF BLOCKS ARE AT SAME LOCATION
;;; ============================================================

(defun FindMatchingBorePit (checkPt gridBBox / ss ent blkPt i result)
  (setq result nil)
  (if checkPt
    (progn
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "BORE PIT"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BLOCKS*))
              (progn
                (setq blkPt (GetBlockCenter ent))
                (if (and blkPt 
                         (< (distance checkPt blkPt) *DAP-BOREPIT-TOLERANCE*)
                         (PointInsideBBox blkPt gridBBox))
                  (setq result ent)
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

(defun FindMatchingDAP (borePitPt gridBBox / ss ent blkPt i result)
  (setq result nil)
  (if borePitPt
    (progn
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "DAP (HAND DIG)"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BLOCKS*))
              (progn
                (setq blkPt (GetBlockCenter ent))
                (if (and blkPt 
                         (< (distance borePitPt blkPt) *DAP-BOREPIT-TOLERANCE*)
                         (PointInsideBBox blkPt gridBBox))
                  (setq result ent)
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

(defun FindMatchingToby (borePitPt gridBBox / ss ent blkPt i result)
  (setq result nil)
  (if borePitPt
    (progn
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "TOBY BOX"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BLOCKS*))
              (progn
                (setq blkPt (GetBlockCenter ent))
                (if (and blkPt 
                         (< (distance borePitPt blkPt) *DAP-BOREPIT-TOLERANCE*)
                         (PointInsideBBox blkPt gridBBox))
                  (setq result ent)
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

(defun FindMatchingGroundRod (hhPt gridBBox / ss ent blkPt i result)
  (setq result nil)
  (if hhPt
    (progn
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "GROUND ROD"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BLOCKS*))
              (progn
                (setq blkPt (GetBlockCenter ent))
                (if (and blkPt 
                         (< (distance hhPt blkPt) *HANDHOLE-TOLERANCE*)
                         (PointInsideBBox blkPt gridBBox))
                  (setq result ent)
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

(defun FindMatchingHandhole (checkPt gridBBox / ss ent blkPt i result)
  (setq result nil)
  (if checkPt
    (progn
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 "PROPOSED HH WITH FDH"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BLOCKS*))
              (progn
                (setq blkPt (GetBlockCenter ent))
                (if (and blkPt 
                         (< (distance checkPt blkPt) *HANDHOLE-TOLERANCE*)
                         (PointInsideBBox blkPt gridBBox))
                  (setq result ent)
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

;;; ============================================================
;;; FIND BLOCKS NEAR POINT (IN ORDER)
;;; ============================================================

(defun FindBlocksNearPointOrdered (pt dist gridBBox / result ss ent blkPt i blkName matchingEnt matchingGR)
  (setq result '())
  
  (if pt
    (foreach blkName *CALLOUT-ORDER*
      (if (/= (strcase blkName) "BORE")
        (progn
          (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blkName))))
          (if ss
            (progn
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i))
                (if (not (member ent *PROCESSED-BLOCKS*))
                  (progn
                    (setq blkPt (GetBlockCenter ent))
                    (if (and blkPt 
                             (< (distance pt blkPt) dist)
                             (PointInsideBBox blkPt gridBBox))
                      (progn
                        (cond
                          ;; TOBY BOX + BORE PIT combo
                          ((= (strcase blkName) "TOBY BOX")
                           (setq matchingEnt (FindMatchingBorePit blkPt gridBBox))
                           (if matchingEnt
                             (setq result (append result (list (list ent blkPt "TOBY BOX + BORE PIT" matchingEnt))))
                             (setq result (append result (list (list ent blkPt blkName))))
                           )
                          )
                          ;; DAP + BORE PIT combo
                          ((= (strcase blkName) "DAP (HAND DIG)")
                           (setq matchingEnt (FindMatchingBorePit blkPt gridBBox))
                           (if matchingEnt
                             (setq result (append result (list (list ent blkPt "DAP + BORE PIT" matchingEnt))))
                             (setq result (append result (list (list ent blkPt blkName))))
                           )
                          )
                          ;; BORE PIT - check if already handled
                          ((= (strcase blkName) "BORE PIT")
                           (setq matchingEnt (FindMatchingDAP blkPt gridBBox))
                           (if (not matchingEnt)
                             (setq matchingEnt (FindMatchingToby blkPt gridBBox))
                           )
                           (if (not matchingEnt)
                             (setq result (append result (list (list ent blkPt blkName))))
                           )
                          )
                          ;; HANDHOLE + GROUND ROD
                          ((= (strcase blkName) "PROPOSED HH WITH FDH")
                           (setq result (append result (list (list ent blkPt blkName))))
                           (setq matchingGR (FindMatchingGroundRod blkPt gridBBox))
                           (if matchingGR
                             (setq result (append result (list (list matchingGR (GetBlockCenter matchingGR) "GROUND ROD"))))
                           )
                          )
                          ;; GROUND ROD - check if already handled
                          ((= (strcase blkName) "GROUND ROD")
                           (setq matchingEnt (FindMatchingHandhole blkPt gridBBox))
                           (if (not matchingEnt)
                             (setq result (append result (list (list ent blkPt blkName))))
                           )
                          )
                          ;; All others
                          (T
                           (setq result (append result (list (list ent blkPt blkName))))
                          )
                        )
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )
  result
)

;;; ============================================================
;;; FIND CONNECTED BORES
;;; ============================================================

(defun FindConnectedBores (pt dist / result ss ent startPt endPt i)
  (setq result '())
  (if pt
    (progn
      (setq ss (ssget "X" (list (cons 0 "*POLYLINE") (cons 8 *BORE-LAYER*))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (if (not (member ent *PROCESSED-BORES*))
              (progn
                (setq startPt (GetPlineStartPt ent))
                (setq endPt (GetPlineEndPt ent))
                (if (or (and startPt (< (distance pt startPt) dist))
                        (and endPt (< (distance pt endPt) dist)))
                  (setq result (append result (list ent)))
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
    )
  )
  result
)

;;; ============================================================
;;; CREATE CALLOUT
;;; ============================================================

(defun CreateCallout (pt1 pt2 num / blockName polyEnt textEnt wipeEnt ss)
  (if (and pt1 pt2)
    (progn
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)
      
      (setq blockName (strcat "hexa_" (itoa num)))
      
      (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blockName))))
      (if ss (command "_.ERASE" ss ""))
      
      (setvar "CECOLOR" "BYLAYER")
      (command "_.POLYGON" "6" pt2 "I" *CALLOUT-HEX-RADIUS*)
      (setq polyEnt (entlast))
      
      (setvar "WIPEOUTFRAME" 0)
      (command "_.WIPEOUT" "P" polyEnt "Y")
      (setq wipeEnt (entlast))
      
      (setvar "CECOLOR" "BYLAYER")
      (command "_.POLYGON" "6" pt2 "I" *CALLOUT-HEX-RADIUS*)
      (setq polyEnt (entlast))
      
      (setvar "CECOLOR" "0")
      (command "_.TEXT" "MC" pt2 *CALLOUT-TEXT-HEIGHT* "0" (itoa num))
      (setq textEnt (entlast))
      (setvar "CECOLOR" "BYLAYER")
      
      (if (tblsearch "BLOCK" blockName)
        (command "_.BLOCK" blockName "Y" pt2 wipeEnt polyEnt textEnt "")
        (command "_.BLOCK" blockName pt2 wipeEnt polyEnt textEnt "")
      )
      
      (command "_.MLEADER" "O" "C" "B" blockName "A" "Y" (rtos *CALLOUT-ARROW-SIZE* 2 2) "X" "H" pt1 pt2)
      
      (setq *PLACED-CALLOUTS* (append *PLACED-CALLOUTS* (list pt2)))
    )
  )
)

;;; ============================================================
;;; MAIN AUTO CALLOUT COMMAND
;;; ============================================================

(defun c:AUTOCALLOUT (/ gridEnt rowEnt sidePt ss ent i 
                        minDist oldEcho oldOsmode addMore gridBBox
                        calloutNum boreEnt boreMidPt boreEndPt boreStartPt calloutPt
                        blocksAtEnd blkData blkEnt blkPt blkName currentPt
                        connectedBores otherEndBores totalConnected
                        continueLoop autoLoop lastEndPt matchingBorePit
                        handholeGroup hhData)
  
  (setq minDist (* *CALLOUT-HEX-RADIUS* 2.5))
  (setq *PLACED-CALLOUTS* '())
  (setq *ROW-SIDES* '())
  (setq *DROP-POLYLINES* '())
  (setq *PROCESSED-BORES* '())
  (setq *PROCESSED-BLOCKS* '())
  (setq calloutNum 1)
  
  (setq oldEcho (getvar "CMDECHO"))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  
  ;; Load DROP polylines
  (setq ss (ssget "X" (list (cons 0 "*POLYLINE") (cons 8 *DROP-LAYER*))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq *DROP-POLYLINES* (append *DROP-POLYLINES* (list (ssname ss i))))
        (setq i (1+ i))
      )
    )
  )

  ;; Load avoid curves from configured layers
  (LoadAvoidCurves)
  (princ (strcat "\nLoaded " (itoa (length *AVOID-CURVES*)) " avoid curves from layers: "))
  (foreach lay *AVOID-LAYERS* (princ (strcat lay " ")))
  
  (princ "\n\n========================================")
  (princ "\n  AUTO CALLOUT - BORE > BLOCK > BORE")
  (princ "\n  HANDHOLE AUTO-DETECT ENABLED")
  (princ "\n  Side-Lock: ")
  (princ (if *LOCK-TO-PICKED-ROW* "ON" "OFF"))
  (princ (strcat " | Avoid dist: " (rtos *AVOID-DIST* 2 2)))
  (princ "\n========================================")
  
  ;; Step 1: Select GRID
  (princ "\n\nStep 1: Select GRID...")
  (setq gridEnt (car (entsel "\nSelect GRID polyline: ")))
  
  (if (not gridEnt)
    (princ "\n*** Cancelled ***")
    (progn
      (setq gridBBox (GetBoundingBox gridEnt))
      
      ;; Step 2: Select ROW lines
      (princ "\n\nStep 2: Select ROW lines...")
      (setq addMore T)
      (while addMore
        (setq rowEnt (car (entsel "\nSelect ROW (ENTER when done): ")))
        (if rowEnt
          (progn
            (setq sidePt (getpoint "\nPick callout side: "))
            (if sidePt
              (progn
                (setq *ROW-SIDES* (append *ROW-SIDES* (list (cons rowEnt sidePt))))
                (princ (strcat "  ROW #" (itoa (length *ROW-SIDES*)) " added"))
              )
            )
          )
          (setq addMore nil)
        )
      )
      
      (if (= (length *ROW-SIDES*) 0)
        (princ "\n*** No ROW selected ***")
        (progn
          (princ (strcat "\n\nLoaded " (itoa (length *DROP-POLYLINES*)) " drop lines"))
          
          ;; ==========================================
          ;; AUTO-DETECT HANDHOLE GROUP
          ;; ==========================================
          (princ "\n\nChecking for HANDHOLE in grid...")
          (setq handholeGroup (FindHandholeGroup gridBBox))
          
          (if handholeGroup
            (progn
              (princ "\n\n========================================")
              (princ "\n>>> CREATING HANDHOLE CALLOUTS...")
              (princ "\n========================================")
              
              ;; #1 - BORE PIT
              (setq hhData (car handholeGroup))
              (if hhData
                (progn
                  (setq blkEnt (car hhData))
                  (setq blkPt (cadr hhData))
                  (setq blkName (caddr hhData))
                  (if (and blkEnt blkPt)
                    (progn
                      (setq *PROCESSED-BLOCKS* (append *PROCESSED-BLOCKS* (list blkEnt)))
                      (setq calloutPt (FindNonOverlapPosition blkPt gridEnt *CALLOUT-OFFSET* minDist))
                      (if calloutPt
                        (progn
                          (CreateCallout (GetArrowPoint blkPt calloutPt) calloutPt calloutNum)
                          (princ (strcat "\n  #" (itoa calloutNum) " " blkName))
                          (setq calloutNum (1+ calloutNum))
                        )
                      )
                    )
                  )
                )
              )
              
              ;; #2 and #3 - HANDHOLE (two callouts)
              (setq hhData (cadr handholeGroup))
              (if hhData
                (progn
                  (setq blkEnt (car hhData))
                  (setq blkPt (cadr hhData))
                  (setq blkName (caddr hhData))
                  (if (and blkEnt blkPt)
                    (progn
                      (setq *PROCESSED-BLOCKS* (append *PROCESSED-BLOCKS* (list blkEnt)))
                      ;; First HH callout
                      (setq calloutPt (FindNonOverlapPosition blkPt gridEnt *CALLOUT-OFFSET* minDist))
                      (if calloutPt
                        (progn
                          (CreateCallout (GetArrowPoint blkPt calloutPt) calloutPt calloutNum)
                          (princ (strcat "\n  #" (itoa calloutNum) " " blkName " (1)"))
                          (setq calloutNum (1+ calloutNum))
                        )
                      )
                      ;; Second HH callout
                      (setq calloutPt (FindNonOverlapPosition blkPt gridEnt *CALLOUT-OFFSET* minDist))
                      (if calloutPt
                        (progn
                          (CreateCallout (GetArrowPoint blkPt calloutPt) calloutPt calloutNum)
                          (princ (strcat "\n  #" (itoa calloutNum) " " blkName " (2)"))
                          (setq calloutNum (1+ calloutNum))
                        )
                      )
                    )
                  )
                )
              )
              
              ;; #4 - GROUND ROD
              (setq hhData (caddr handholeGroup))
              (if hhData
                (progn
                  (setq blkEnt (car hhData))
                  (setq blkPt (cadr hhData))
                  (setq blkName (caddr hhData))
                  (if (and blkEnt blkPt)
                    (progn
                      (setq *PROCESSED-BLOCKS* (append *PROCESSED-BLOCKS* (list blkEnt)))
                      (setq calloutPt (FindNonOverlapPosition blkPt gridEnt *CALLOUT-OFFSET* minDist))
                      (if calloutPt
                        (progn
                          (CreateCallout (GetArrowPoint blkPt calloutPt) calloutPt calloutNum)
                          (princ (strcat "\n  #" (itoa calloutNum) " " blkName))
                          (setq calloutNum (1+ calloutNum))
                        )
                      )
                    )
                  )
                )
              )
              
              (princ "\n========================================")
              (princ "\n>>> Handhole callouts complete!")
              (princ "\n========================================")
            )
            (princ "\n  (No HANDHOLE found in this grid)")
          )
          
          ;; Step 3: BORE selection
          (princ "\n\n========================================")
          (princ "\nStep 3: Select BORE to continue")
          (princ "\n  Press ENTER to finish")
          (princ "\n========================================")
          
          (setq continueLoop T)
          
          (while continueLoop
            (princ "\n")
            (setq boreEnt (car (entsel "\nSelect BORE (ENTER to finish): ")))
            
            (if (not boreEnt)
              (setq continueLoop nil)
              
              (if (not (BoreTouchesGrid boreEnt gridBBox))
                (princ "  *** Bore outside GRID - skipped ***")
                
                (progn
                  (setq autoLoop T)
                  (setq lastEndPt nil)
                  
                  (while autoLoop
                    (if (member boreEnt *PROCESSED-BORES*)
                      (progn
                        (princ "  Already processed")
                        (setq autoLoop nil)
                      )
                      (progn
                        (setq *PROCESSED-BORES* (append *PROCESSED-BORES* (list boreEnt)))
                        
                        (setq boreStartPt (GetPlineStartPt boreEnt))
                        (setq boreEndPt (GetPlineEndPt boreEnt))
                        
                        (if lastEndPt
                          (if (< (distance lastEndPt boreStartPt) (distance lastEndPt boreEndPt))
                            (setq currentPt boreEndPt)
                            (setq currentPt boreStartPt)
                          )
                          (if (PointInsideBBox boreEndPt gridBBox)
                            (setq currentPt boreEndPt)
                            (setq currentPt boreStartPt)
                          )
                        )
                        
                        (setq boreMidPt (GetBoreMidpointInGrid boreEnt gridBBox))
                        
                        (if boreMidPt
                          (progn
                            (setq calloutPt (FindNonOverlapPosition boreMidPt gridEnt *CALLOUT-OFFSET* minDist))
                            (if calloutPt
                              (progn
                                (CreateCallout (GetBoreArrowPoint boreMidPt calloutPt) calloutPt calloutNum)
                                (princ (strcat "\n  #" (itoa calloutNum) " BORE"))
                                (setq calloutNum (1+ calloutNum))
                              )
                            )
                          )
                        )
                        
                        ;; Find blocks at ends
                        (setq blocksAtEnd (FindBlocksNearPointOrdered currentPt *BLOCK-NEAR-DIST* gridBBox))
                        (if (equal currentPt boreEndPt)
                          (setq blocksAtEnd (append blocksAtEnd 
                            (FindBlocksNearPointOrdered boreStartPt *BLOCK-NEAR-DIST* gridBBox)))
                          (setq blocksAtEnd (append blocksAtEnd 
                            (FindBlocksNearPointOrdered boreEndPt *BLOCK-NEAR-DIST* gridBBox)))
                        )
                        
                        ;; Create callouts for blocks
                        (foreach blkData blocksAtEnd
                          (setq blkEnt (car blkData))
                          (setq blkPt (cadr blkData))
                          (setq blkName (caddr blkData))
                          
                          (if (and blkEnt blkPt (not (member blkEnt *PROCESSED-BLOCKS*)))
                            (progn
                              (setq *PROCESSED-BLOCKS* (append *PROCESSED-BLOCKS* (list blkEnt)))
                              
                              ;; Mark combo blocks as processed
                              (if (or (= blkName "DAP + BORE PIT") (= blkName "TOBY BOX + BORE PIT"))
                                (progn
                                  (setq matchingBorePit (cadddr blkData))
                                  (if matchingBorePit
                                    (setq *PROCESSED-BLOCKS* (append *PROCESSED-BLOCKS* (list matchingBorePit)))
                                  )
                                )
                              )
                              
                              (setq calloutPt (FindNonOverlapPosition blkPt gridEnt *CALLOUT-OFFSET* minDist))
                              (if calloutPt
                                (progn
                                  ;; Use GetArrowPoint for BLOCKS (optional SIDE connect)
                                  (CreateCallout (GetArrowPoint blkPt calloutPt) calloutPt calloutNum)
                                  (princ (strcat "\n  #" (itoa calloutNum) " " blkName))
                                  (setq calloutNum (1+ calloutNum))
                                )
                              )
                            )
                          )
                        )
                        
                        ;; Find connected bores
                        (setq connectedBores (FindConnectedBores currentPt *BORE-CONNECT-DIST*))
                        
                        (if (equal currentPt boreEndPt)
                          (setq otherEndBores (FindConnectedBores boreStartPt *BORE-CONNECT-DIST*))
                          (setq otherEndBores (FindConnectedBores boreEndPt *BORE-CONNECT-DIST*))
                        )
                        
                        (setq connectedBores (vl-remove-if 
                          '(lambda (x) 
                             (or (member x *PROCESSED-BORES*)
                                 (not (BoreTouchesGrid x gridBBox))))
                          connectedBores))
                        
                        (setq otherEndBores (vl-remove-if 
                          '(lambda (x) 
                             (or (member x *PROCESSED-BORES*)
                                 (not (BoreTouchesGrid x gridBBox))))
                          otherEndBores))
                        
                        (setq totalConnected (+ (length connectedBores) (length otherEndBores)))
                        
                        (cond
                          ((= totalConnected 0)
                           (princ "\n  >>> END OF PATH")
                           (setq autoLoop nil)
                          )
                          ((and (= (length connectedBores) 1) (= (length otherEndBores) 0))
                           (princ "\n  >>> Continuing...")
                           (setq lastEndPt currentPt)
                           (setq boreEnt (car connectedBores))
                          )
                          (T
                           (princ (strcat "\n  >>> JUNCTION: " (itoa totalConnected) " bore(s)"))
                           (setq autoLoop nil)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          
          (princ "\n\n========================================")
          (princ (strcat "\n  DONE! Created " (itoa (1- calloutNum)) " callouts"))
          (princ "\n========================================")
        )
      )
    )
  )
  
  (setvar "CMDECHO" oldEcho)
  (setvar "OSMODE" oldOsmode)
  (princ)
)

(defun c:AC () (c:AUTOCALLOUT))

;;; ============================================================
;;; MANUAL CALLOUT
;;; ============================================================

(defun c:CALLOUTS (/ pt1 pt2 blockName polyEnt textEnt wipeEnt ss)
  (setq pt1 (getpoint "\nArrow START point: "))
  (if pt1
    (progn
      (setq pt2 (getpoint pt1 "\nHexagon location: "))
      (if pt2
        (progn
          (setvar "CMDECHO" 0)
          (setvar "OSMODE" 0)
          (setq blockName (strcat "hexa_" (itoa counter)))
          (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blockName))))
          (if ss (command "_.ERASE" ss ""))
          (setvar "CECOLOR" "BYLAYER")
          (command "_.POLYGON" "6" pt2 "I" *CALLOUT-HEX-RADIUS*)
          (setq polyEnt (entlast))
          (setvar "WIPEOUTFRAME" 0)
          (command "_.WIPEOUT" "P" polyEnt "Y")
          (setq wipeEnt (entlast))
          (setvar "CECOLOR" "BYLAYER")
          (command "_.POLYGON" "6" pt2 "I" *CALLOUT-HEX-RADIUS*)
          (setq polyEnt (entlast))
          (setvar "CECOLOR" "0")
          (command "_.TEXT" "MC" pt2 *CALLOUT-TEXT-HEIGHT* "0" (itoa counter))
          (setq textEnt (entlast))
          (setvar "CECOLOR" "BYLAYER")
          (if (tblsearch "BLOCK" blockName)
            (command "_.BLOCK" blockName "Y" pt2 wipeEnt polyEnt textEnt "")
            (command "_.BLOCK" blockName pt2 wipeEnt polyEnt textEnt "")
          )
          (command "_.MLEADER" "O" "C" "B" blockName "A" "Y" (rtos *CALLOUT-ARROW-SIZE* 2 2) "X" "H" pt1 pt2)
          (princ (strcat "\nCallout " (itoa counter) " created"))
          (setq counter (1+ counter))
          (c:CALLOUTS)
        )
      )
    )
  )
  (princ)
)

(defun c:CO () (c:CALLOUTS))

;;; Counter commands
(defun c:SC (/ n)
  (setq n (getint "\nSet counter to: "))
  (if n (setq counter n))
  (princ (strcat "\nCounter: " (itoa counter)))
  (princ)
)

(defun c:RC ()
  (setq counter 1)
  (princ "\nCounter reset to 1")
  (princ)
)

(defun c:CC ()
  (princ (strcat "\nCounter: " (itoa counter)))
  (princ)
)

;;; View order
(defun c:ACORDER ()
  (princ "\n\nCALLOUT ORDER:")
  (setq i 1)
  (foreach item *CALLOUT-ORDER*
    (princ (strcat "\n  " (itoa i) ". " item))
    (setq i (1+ i))
  )
  (princ "\n")
  (princ)
)

;;; Set tolerances
(defun c:ACSETTOL (/ newTol)
  (princ (strcat "\n\nCurrent DAP/BORE PIT tolerance: " (rtos *DAP-BOREPIT-TOLERANCE* 2 2)))
  (setq newTol (getreal "\nEnter new tolerance (or ENTER to keep): "))
  (if newTol
    (setq *DAP-BOREPIT-TOLERANCE* newTol)
  )
  (princ)
)

(defun c:ACSETHH (/ newTol)
  (princ (strcat "\n\nCurrent HANDHOLE tolerance: " (rtos *HANDHOLE-TOLERANCE* 2 2)))
  (setq newTol (getreal "\nEnter new tolerance (or ENTER to keep): "))
  (if newTol
    (setq *HANDHOLE-TOLERANCE* newTol)
  )
  (princ)
)

(defun c:ACADDLAYER (/ layName)
  (princ (strcat "\n\nCurrent BORE layer: " *BORE-LAYER*))
  (princ (strcat "\nCurrent DROP layer: " *DROP-LAYER*))
  (setq layName (getstring T "\n\nNew BORE layer (ENTER to keep): "))
  (if (and layName (/= layName ""))
    (setq *BORE-LAYER* layName)
  )
  (setq layName (getstring T "\nNew DROP layer (ENTER to keep): "))
  (if (and layName (/= layName ""))
    (setq *DROP-LAYER* layName)
  )
  (princ)
)

;;; Toggle SIDE or CENTER connection for blocks
(defun c:ACSIDE ()
  (if *CONNECT-TO-SIDE*
    (progn
      (setq *CONNECT-TO-SIDE* nil)
      (princ "\n  Arrow connects to BLOCK CENTER")
    )
    (progn
      (setq *CONNECT-TO-SIDE* T)
      (princ "\n  Arrow connects to BLOCK SIDE")
    )
  )
  (princ)
)

;;; Set side offset distance
(defun c:ACSETSIDE (/ newVal)
  (princ (strcat "\n\nCurrent side offset: " (rtos *BLOCK-SIDE-OFFSET* 2 2)))
  (princ (strcat "\nConnect to side: " (if *CONNECT-TO-SIDE* "YES" "NO")))
  (setq newVal (getreal "\nEnter new side offset (or ENTER to keep): "))
  (if newVal
    (progn
      (setq *BLOCK-SIDE-OFFSET* newVal)
      (princ (strcat "\n  Side offset set to: " (rtos newVal 2 2)))
    )
  )
  (princ)
)

;;; ============================================================
;;; RENUMBER CALLOUTS - Increase or decrease all callout numbers
;;; ============================================================

(defun c:ACRENUM (/ ss i ent blkName oldNum newNum offset oldBlockName newBlockName
                    textEnt textData oldText newText obj)
  
  (princ "\n\n========================================")
  (princ "\n  RENUMBER CALLOUTS")
  (princ "\n========================================")
  (princ "\n  Positive number = increase (2,3,4 -> 3,4,5)")
  (princ "\n  Negative number = decrease (3,4,5 -> 2,3,4)")
  
  (setq offset (getint "\n\nEnter offset (+1, -1, +2, etc.): "))
  
  (if offset
    (progn
      ;; Find all hexa_ blocks (MULTILEADERs)
      (setq ss (ssget "X" '((0 . "MULTILEADER"))))
      
      (if ss
        (progn
          (princ (strcat "\n  Found " (itoa (sslength ss)) " callouts"))
          (princ "\n  Renumbering...")
          
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))
            
            ;; Get the block name from MULTILEADER
            (if (vlax-property-available-p obj 'ContentBlockName)
              (progn
                (setq blkName (vlax-get-property obj 'ContentBlockName))
                
                ;; Check if it's a hexa_ block
                (if (and blkName (wcmatch blkName "hexa_*"))
                  (progn
                    ;; Extract old number
                    (setq oldNum (atoi (substr blkName 6)))
                    (setq newNum (+ oldNum offset))
                    
                    ;; Only process if new number is positive
                    (if (> newNum 0)
                      (progn
                        (setq newBlockName (strcat "hexa_" (itoa newNum)))
                        
                        ;; Create new block if doesn't exist
                        (if (not (tblsearch "BLOCK" newBlockName))
                          (CreateHexBlock newNum)
                        )
                        
                        ;; Update MULTILEADER to use new block
                        (vlax-put-property obj 'ContentBlockName newBlockName)
                      )
                      (princ (strcat "\n  Skipped: " blkName " (result would be < 1)"))
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
          
          (princ "\n\n  DONE! Callouts renumbered.")
          (princ (strcat "\n  Offset applied: " (if (> offset 0) "+" "") (itoa offset)))
          
          ;; Update counter
          (setq counter (+ counter offset))
          (if (< counter 1) (setq counter 1))
          (princ (strcat "\n  Counter now: " (itoa counter)))
        )
        (princ "\n  No callouts found!")
      )
    )
    (princ "\n  Cancelled")
  )
  (princ "\n========================================")
  (princ)
)

;;; Create a hexagon block for renumbering
(defun CreateHexBlock (num / blockName pt polyEnt textEnt wipeEnt ss)
  (setq blockName (strcat "hexa_" (itoa num)))
  (setq pt '(0 0))
  
  ;; Delete if exists
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blockName))))
  (if ss (command "_.ERASE" ss ""))
  
  (setvar "CMDECHO" 0)
  (setvar "CECOLOR" "BYLAYER")
  (command "_.POLYGON" "6" pt "I" *CALLOUT-HEX-RADIUS*)
  (setq polyEnt (entlast))
  
  (setvar "WIPEOUTFRAME" 0)
  (command "_.WIPEOUT" "P" polyEnt "Y")
  (setq wipeEnt (entlast))
  
  (setvar "CECOLOR" "BYLAYER")
  (command "_.POLYGON" "6" pt "I" *CALLOUT-HEX-RADIUS*)
  (setq polyEnt (entlast))
  
  (setvar "CECOLOR" "0")
  (command "_.TEXT" "MC" pt *CALLOUT-TEXT-HEIGHT* "0" (itoa num))
  (setq textEnt (entlast))
  (setvar "CECOLOR" "BYLAYER")
  
  (if (tblsearch "BLOCK" blockName)
    (command "_.BLOCK" blockName "Y" pt wipeEnt polyEnt textEnt "")
    (command "_.BLOCK" blockName pt wipeEnt polyEnt textEnt "")
  )
)

;;; Renumber selected callouts only
(defun c:ACRENUMSEL (/ ss i ent blkName oldNum newNum offset obj)
  
  (princ "\n\nSelect callouts to renumber...")
  (setq ss (ssget '((0 . "MULTILEADER"))))
  
  (if ss
    (progn
      (princ (strcat "\n  Selected " (itoa (sslength ss)) " callouts"))
      (princ "\n  Positive = increase, Negative = decrease")
      (setq offset (getint "\n  Enter offset (+1, -1, +2, etc.): "))
      
      (if offset
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))
            
            (if (vlax-property-available-p obj 'ContentBlockName)
              (progn
                (setq blkName (vlax-get-property obj 'ContentBlockName))
                
                (if (and blkName (wcmatch blkName "hexa_*"))
                  (progn
                    (setq oldNum (atoi (substr blkName 6)))
                    (setq newNum (+ oldNum offset))
                    
                    (if (> newNum 0)
                      (progn
                        (if (not (tblsearch "BLOCK" (strcat "hexa_" (itoa newNum))))
                          (CreateHexBlock newNum)
                        )
                        (vlax-put-property obj 'ContentBlockName (strcat "hexa_" (itoa newNum)))
                        (princ (strcat "\n  " (itoa oldNum) " -> " (itoa newNum)))
                      )
                    )
                  )
                )
              )
            )
            (setq i (1+ i))
          )
          (princ "\n  DONE!")
        )
        (princ "\n  Cancelled")
      )
    )
    (princ "\n  No callouts selected")
  )
  (princ)
)

;;; ============================================
;;; COMMANDS TO MANAGE AVOID LAYERS/DISTANCE + SIDE LOCK
;;; ============================================

(defun c:ACAVOID (/ s done lst)
  (princ "\nEnter layer names to avoid (type DONE to finish):")
  (setq done nil lst '())
  (while (not done)
    (setq s (getstring T "\nLayer: "))
    (cond
      ((or (null s) (= (strcase s) "DONE")) (setq done T))
      ((/= s "") (setq lst (cons s lst)))
    )
  )
  (setq *AVOID-LAYERS* (reverse lst))
  (LoadAvoidCurves)
  (princ (strcat "\nAvoid layers set: "))
  (foreach lay *AVOID-LAYERS* (princ (strcat lay " ")))
  (princ)
)

(defun c:ACSETAVOIDDIST (/ v)
  (princ (strcat "\nCurrent avoid distance: " (rtos *AVOID-DIST* 2 2)))
  (setq v (getreal "\nEnter new avoid distance (ENTER to keep): "))
  (if v (setq *AVOID-DIST* v))
  (princ)
)

(defun c:ACLOCKSIDE ()
  (if *LOCK-TO-PICKED-ROW*
    (progn (setq *LOCK-TO-PICKED-ROW* nil) (princ "\nLock-to-picked-ROW: OFF"))
    (progn (setq *LOCK-TO-PICKED-ROW* T)   (princ "\nLock-to-picked-ROW: ON"))
  )
  (princ)
)

;;; ============================================================
;;; DEBUG: Which ROW is being honored?
;;; ============================================================
(defun c:ACDEBUG (/ pt rowPair rowEnt sidePt lay cp dL)
  (princ "\nPick an anchor point (e.g., BORE midpoint)...")
  (setq pt (getpoint "\nAnchor point: "))
  (if pt
    (progn
      (setq rowPair (GetClosestROWSide pt))
      (if rowPair
        (progn
          (setq rowEnt (car rowPair))
          (setq sidePt (cdr rowPair))
          (setq lay (vlax-get-property (vlax-ename->vla-object rowEnt) 'Layer))
          (setq cp (vlax-curve-getClosestPointTo rowEnt pt))
          (setq dL (if cp (distance pt cp) nil))
          (princ (strcat "\nNearest ROW layer: " lay))
          (if dL (princ (strcat " | dist: " (rtos dL 2 2))))
          (if sidePt (princ (strcat " | sidePt: " (rtos (car sidePt) 2 2) ", " (rtos (cadr sidePt) 2 2))))
        )
        (princ "\n(No ROW found)")
      )
    )
  )
  (princ)
)

;;; Load message
(princ "\n")
(princ "\n========================================")
(princ "\n  FTTH AUTO CALLOUTS LOADED")
(princ "\n  HANDHOLE AUTO-DETECT ENABLED")
(princ "\n  Nearest-ROW lock enabled")
(princ (strcat "\n  Avoid distance: " (rtos *AVOID-DIST* 2 2)))
(princ "\n========================================")
(princ "\n  AC  - Auto callout")
(princ "\n  CO  - Manual callout")
(princ "\n  SC/RC/CC - Counter controls")
(princ "\n  ACORDER - View order")
(princ "\n  ACSETTOL - DAP/BORE PIT tolerance")
(princ "\n  ACSETHH  - HANDHOLE tolerance")
(princ "\n")
(princ "\n  ARROW CONNECTION (Blocks only):")
(princ "\n  ACSIDE    - Toggle SIDE/CENTER")
(princ "\n  ACSETSIDE - Set side offset distance")
(princ (strcat "\n  Current: " (if *CONNECT-TO-SIDE* "SIDE" "CENTER")))
(princ (strcat " (offset: " (rtos *BLOCK-SIDE-OFFSET* 2 1) ")"))
(princ "\n")
(princ "\n  AVOID LAYERS:")
(princ "\n  ACAVOID        - Set avoid layers list")
(princ "\n  ACSETAVOIDDIST - Set avoid buffer distance")
(princ "\n  ACLOCKSIDE     - Toggle lock-to-picked-ROW")
(princ "\n  ACDEBUG        - Report nearest ROW to a point")
(princ "\n")
(princ "\n  RENUMBER CALLOUTS:")
(princ "\n  ACRENUM    - Renumber ALL callouts")
(princ "\n  ACRENUMSEL - Renumber SELECTED only")
(princ "\n========================================")
(princ)
