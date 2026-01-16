(vl-load-com)

; ==============================================================================
; Configuration
; ==============================================================================

(setq *CFG-FDH-LAYER* "PROPOSED HH WITH FDH")   ; FDH layer for fallback/source detection
(setq *CFG-FDH-BLOCK* "PROPOSED HH WITH FDH")                    ; FDH block name for graph source nodes
(setq *CFG-BORE-LAYER* "BORE")                  ; Layer containing route polylines
(setq *CFG-TOL* 0.1)                            ; Snap tolerance (feet)
(setq *CFG-ARROW-BLOCK* "Direction Arrow")      ; Arrow block name
(setq *CFG-ARROW-SCALE* 1.0)                    ; Arrow scale
(setq *CFG-ARROW-OFFSET-DEG* -90.0)             ; Arrow base rotation offset

; ==============================================================================
; Utilities
; ==============================================================================

(defun _deg->rad (a) (* pi (/ a 180.0)))

(defun _as-pt (p / try coords)
  (cond
    ((listp p)
     (cond ((= (length p) 2) (mapcar 'float (append p '(0.0))))
           ((= (length p) 3) (mapcar 'float p))
           (T (list 0.0 0.0 0.0))))
    ((= (type p) 'VLA-OBJECT)
     (cond
       ((vlax-property-available-p p 'Coordinates)
        (setq coords (vlax-get p 'Coordinates))
        (_as-pt (vlax-safearray->list coords)))
       ((and (vlax-property-available-p p 'X)
             (vlax-property-available-p p 'Y))
        (list (float (vlax-get p 'X))
              (float (vlax-get p 'Y))
              (if (vlax-property-available-p p 'Z) (float (vlax-get p 'Z)) 0.0)))
       (T (list 0.0 0.0 0.0))))
    (T
     (setq try (vl-catch-all-apply '(lambda () (vlax-safearray->list (vlax-variant-value p)))))
     (cond
       ((not (vl-catch-all-error-p try)) (_as-pt try))
       (T
        (setq try (vl-catch-all-apply '(lambda () (vlax-safearray->list p))))
        (if (vl-catch-all-error-p try)
          (list 0.0 0.0 0.0)
          (_as-pt try))))))
)

(defun _snap-pt (pt tol)
  (mapcar '(lambda (x) (* tol (fix (/ x tol)))) (list (car pt) (cadr pt)))
)

(defun _remove-duplicates (lst / out)
  (setq out '())
  (foreach x lst
    (if (not (member x out))
      (setq out (cons x out))
    )
  )
  (reverse out)
)

(defun _ensure-block (blkname / doc blks ok path)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        blks (vla-get-Blocks doc)
        ok T)
  (if (vl-catch-all-error-p (vl-catch-all-apply '(lambda () (vla-Item blks blkname))))
    (progn
      (prompt (strcat "\nBlock \"" blkname "\" not found. Select a DWG to import it..."))
      (setq path (getfiled "Select block file" "" "dwg" 0))
      (if (and path (findfile path))
        (progn
          (command "_.UNDO" "_BEGIN")
          (vl-catch-all-apply
            '(lambda ()
               (vla-InsertBlock
                 (vla-get-ModelSpace doc)
                 (vlax-3d-point (list 0.0 0.0 0.0))
                 path 1.0 1.0 1.0 0.0)))
          (command "_.U")
          (command "_.UNDO" "_END"))
        (progn (prompt "\nNo block provided. Command cancelled.") (setq ok nil))))
    T)
  ok)
; ==============================================================================
; Selection helpers
; ==============================================================================

(defun _ss-bore ()
  ;; Select all BORE polylines
  (ssget "_X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 *CFG-BORE-LAYER*)))
)

(defun _ss-fdh-block ()
  ;; Select FDH blocks by block name
  (ssget "_X" (list '(0 . "INSERT") (cons 2 *CFG-FDH-BLOCK*)))
)

(defun _ss-fdh-layer ()
  ;; Select FDH inserts by layer
  (ssget "_X" (list '(0 . "INSERT") (cons 8 *CFG-FDH-LAYER*)))
)

(defun _collect-fdh-nodes (/ ss1 ss2 pts i ent ins)
  ;; Collect FDH insertion points from block name and layer
  (setq pts '())
  (setq ss1 (_ss-fdh-block))
  (if ss1
    (progn
      (setq i 0)
      (repeat (sslength ss1)
        (setq ent (ssname ss1 i)
              ins (_as-pt (cdr (assoc 10 (entget ent)))))
        (setq pts (cons (_snap-pt ins *CFG-TOL*) pts))
        (setq i (1+ i))
      )
    )
  )
  (setq ss2 (_ss-fdh-layer))
  (if ss2
    (progn
      (setq i 0)
      (repeat (sslength ss2)
        (setq ent (ssname ss2 i)
              ins (_as-pt (cdr (assoc 10 (entget ent)))))
        (setq pts (cons (_snap-pt ins *CFG-TOL*) pts))
        (setq i (1+ i))
      )
    )
  )
  (_remove-duplicates pts)
)
; ==============================================================================
; Graph build and traversal
; ==============================================================================

(defun _edge-record (obj ent tol / sp ep)
  ;; Build an edge record: (start-node end-node ename)
  (setq sp (_snap-pt (vlax-curve-getStartPoint obj) tol))
  (setq ep (_snap-pt (vlax-curve-getEndPoint   obj) tol))
  (list sp ep ent)
)

(defun _build-graph (ss tol / idx ent obj graph)
  ;; Build graph from selection set of polylines
  (setq graph '())
  (if ss
    (progn
      (setq idx 0)
      (repeat (sslength ss)
        (setq ent (ssname ss idx)
              obj (vlax-ename->vla-object ent))
        (setq graph (cons (_edge-record obj ent tol) graph))
        (setq idx (1+ idx))
      )
    )
  )
  graph
)

(defun _neighbors (node graph / out edge ent obj sp ep p proj d)
  ;; Return all edges connected to NODE, either by endpoint or by projection within tolerance
  (setq out '())
  (foreach edge graph
    (setq sp (car edge) ep (cadr edge) ent (caddr edge))
    (if (or (equal node sp *CFG-TOL*) (equal node ep *CFG-TOL*))
      (setq out (cons edge out))
      (progn
        ;; Endpoint-to-curve connection check
        (setq obj (vlax-ename->vla-object ent))
        (setq p (append node (list 0.0)))
        (setq proj (_as-pt (vlax-curve-getClosestPointTo obj p)))
        (setq d (distance p proj))
        (if (<= d *CFG-TOL*)
          (setq out (cons edge out))
        )
      )
    )
  )
  out
)

(defun _other-node (edge node)
  ;; Return the opposite node of EDGE given NODE
  (if (equal node (car edge) *CFG-TOL*)
    (cadr edge)
    (car edge))
)

(defun _reverse-edge (ent / obj)
  ;; Reverse polyline safely
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-method-applicable-p obj 'Reverse)
    (vla-Reverse obj)
    (command "_.PEDIT" ent "R" "")
  )
)

(defun _orient-network (graph sources / queue visited depthMap node lvl edge sp ep ent spLvl epLvl other)
  ;; Initialize queue with (node . depth)
  (setq queue (mapcar '(lambda (n) (list n 0)) sources))
  (setq visited '()
        depthMap '())

  (while queue
    (setq node (caar queue)
          lvl  (cadar queue)
          queue (cdr queue))

    ;; Record BFS depth if not already set
    (if (not (assoc node depthMap))
      (setq depthMap (cons (cons node lvl) depthMap)))

    ;; Explore neighbors
    (foreach edge (_neighbors node graph)
      (if (not (member edge visited))
        (progn
          (setq sp (car edge)
                ep (cadr edge)
                ent (caddr edge))

          ;; Look up BFS levels of endpoints
          (setq spLvl (cdr (assoc sp depthMap)))
          (setq epLvl (cdr (assoc ep depthMap)))

          ;; If both endpoints have levels, orient from lower to higher
          (cond
            ((and spLvl epLvl)
             (if (< spLvl epLvl)
               ;; sp -> ep is correct
               nil
               (_reverse-edge ent)))
            ;; If only current node has a level, orient away from it
            ((equal node sp *CFG-TOL*)
             ;; node is start, so orient sp -> ep
             nil)
            ((equal node ep *CFG-TOL*)
             ;; node is end, so reverse to orient ep -> sp
             (_reverse-edge ent))
          )

          ;; Enqueue the other node with lvl+1
          (setq other (_other-node edge node))
          (setq queue (cons (list other (1+ lvl)) queue))

          ;; Mark edge visited
          (setq visited (cons edge visited))
        )
      )
    )
  )
  visited
)



(defun c:DANF ( / ms ss blkName scale rotOffsetDeg rotOffsetRad cnt e vlaObj sp ep midP insPt deriv tanVec ang blkRef)
  (setq blkName      *CFG-ARROW-BLOCK*)
  (setq scale        *CFG-ARROW-SCALE*)
  (setq rotOffsetDeg *CFG-ARROW-OFFSET-DEG*)
  (setq rotOffsetRad (_deg->rad rotOffsetDeg))
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))

  (if (not (_ensure-block blkName))
    (progn (prompt "\nCannot proceed without block definition.") (princ))
    (progn
      (setq ss (_ss-bore))
      (if (not ss)
        (prompt "\nNo polylines found on BORE.")
        (progn
          (setq cnt 0)
          (foreach e (vl-remove-if 'null (mapcar 'cadr (ssnamex ss)))
            (setq vlaObj (vlax-ename->vla-object e))
            ;; Midpoint param
            (setq sp   (vlax-curve-getStartParam vlaObj))
            (setq ep   (vlax-curve-getEndParam   vlaObj))
            (setq midP (/ (+ sp ep) 2.0))
            (setq insPt (_as-pt (vlax-curve-getPointAtParam vlaObj midP)))
            ;; Tangent vector at midpoint
            (setq deriv (vlax-curve-getFirstDeriv vlaObj midP))
            (setq tanVec (_as-pt deriv))
            ;; Angle from tangent only (trust BFS orientation)
            (setq ang (angle '(0 0 0) tanVec))
            (setq ang (+ ang pi))
            (setq ang (+ ang rotOffsetRad))
            ;; Insert arrow
            (setq blkRef
              (vla-InsertBlock
                ms
                (vlax-3d-point insPt)
                blkName
                scale scale scale
                ang))
            (vla-put-Layer blkRef *CFG-BORE-LAYER*)
            (setq cnt (1+ cnt)))
          (prompt (strcat "\nInserted " (itoa cnt) " arrows at midpoints of BORE routes."))
        )
      )
    )
  )
  (princ)
)
