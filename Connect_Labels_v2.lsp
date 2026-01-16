;;; ============================================================
;;; BLOCK TO LABEL CONNECTOR - WITH DROP SUPPORT
;;; ============================================================
;;; Connects blocks/polylines to their nearest matching labels
;;; Highlights ambiguous connections in RED
;;; 
;;; Supports: TOBY, DAP, BORE, DROP, HANDHOLE
;;; ============================================================

(vl-load-com)

;;; ============================================================
;;; SETTINGS - EDIT THESE TO MATCH YOUR DRAWING
;;; ============================================================

;; Block names (exact names in your drawing)
(setq *TOBY-BLOCK* "TOBY BOX")
(setq *DAP-BLOCK* "DAP (HAND DIG)")
(setq *HANDHOLE-BLOCK* "PROPOSED HANDHOLE")

;; Polyline layer for BORE and DROP (both on same layer)
(setq *BORE-LAYER-NAME* "BORE")      ; Layer where BORE & DROP polylines are

;; Label search text (what text the label contains)
(setq *TOBY-LABEL-TEXT* "TOBY BOX")
(setq *DAP-LABEL-TEXT* "DAP")
(setq *BORE-LABEL-TEXT* "BORE & PLACE")
(setq *DROP-LABEL-TEXT* "DROP (")         ; Matches "DROP (125')" etc.
(setq *HANDHOLE-LABEL-TEXT* "HANDHOLE")

;; Combined search for BORE/DROP (searches for both label types)
(setq *BORE-DROP-COMBINED* T)  ; Set to T to combine BORE+DROP matching

;; Layers for connector lines - SEPARATE FOR EACH TYPE
(setq *CONNECTOR-LAYER-BORE* "maplines_bore")     ; BORE/DROP polylines
(setq *CONNECTOR-LAYER-TOBY* "maplines_toby")     ; TOBY blocks
(setq *CONNECTOR-LAYER-DAP* "maplines")           ; DAP blocks
(setq *CONNECTOR-LAYER-HANDHOLE* "Mapline_hh")    ; HANDHOLE blocks

(setq *PROBLEM-LAYER* "LABEL_PROBLEMS")

;; Maximum search distance for labels (in drawing units)
(setq *MAX-SEARCH-DIST* 50.0)  ; 50 feet - adjust as needed

;; Minimum distance difference to consider "clear winner"
(setq *AMBIGUOUS-THRESHOLD* 5.0)

;;; ============================================================
;;; UTILITY FUNCTIONS
;;; ============================================================

(defun MakeLayer (name color / )
  (if (not (tblsearch "LAYER" name))
    (entmake 
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 name)
        (cons 62 color)
        '(70 . 0)
      )
    )
  )
)

(defun GetBlockInsertPt (ent / obj insPt)
  (setq obj (vlax-ename->vla-object ent))
  (setq insPt (vlax-get-property obj 'InsertionPoint))
  (if (= (type insPt) 'variant)
    (setq insPt (vlax-safearray->list (vlax-variant-value insPt)))
  )
  (list (car insPt) (cadr insPt))
)

(defun GetMTextInsertPt (ent / eData pt)
  (setq eData (entget ent))
  (setq pt (cdr (assoc 10 eData)))
  (if pt (list (car pt) (cadr pt)) nil)
)

(defun GetMTextContent (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'TextString)
    (vlax-get-property obj 'TextString)
    ""
  )
)

(defun TextContains (text searchStr / upperText upperSearch)
  (setq upperText (strcase text))
  (setq upperSearch (strcase searchStr))
  (vl-string-search upperSearch upperText)
)

(defun DrawLine (pt1 pt2 layer color / )
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      (cons 62 color)
      '(100 . "AcDbPolyline")
      '(90 . 2)
      '(70 . 0)
      (cons 10 (list (car pt1) (cadr pt1)))
      (cons 10 (list (car pt2) (cadr pt2)))
    )
  )
)

(defun DrawCircle (pt radius layer color / )
  (entmake
    (list
      '(0 . "CIRCLE")
      (cons 8 layer)
      (cons 62 color)
      (cons 10 (list (car pt) (cadr pt) 0.0))
      (cons 40 radius)
    )
  )
)

;;; ============================================================
;;; COLLECT ALL POLYLINES ON A LAYER
;;; ============================================================

(defun CollectPolylines (layerName / ss i ent polylines midPt)
  (setq polylines '())
  (setq ss (ssget "X" (list (cons 0 "*POLYLINE") (cons 8 layerName))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq midPt (GetPlineMidpoint ent))
        (if midPt
          (setq polylines (cons (list ent midPt) polylines))
        )
        (setq i (1+ i))
      )
    )
  )
  polylines
)

(defun GetPlineMidpoint (ent / len pt)
  (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))
  (setq pt (vlax-curve-getPointAtDist ent (/ len 2.0)))
  (if pt (list (car pt) (cadr pt)) nil)
)

;;; ============================================================
;;; COLLECT ALL MTEXT LABELS
;;; ============================================================

(defun CollectLabels (searchText / ss i ent labels content pt)
  (setq labels '())
  (setq ss (ssget "X" '((0 . "MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq content (GetMTextContent ent))
        (if (TextContains content searchText)
          (progn
            (setq pt (GetMTextInsertPt ent))
            (if pt
              (setq labels (cons (list ent pt content) labels))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  labels
)

;;; ============================================================
;;; COLLECT ALL BLOCKS OF A TYPE
;;; ============================================================

(defun CollectBlocks (blockName / ss i ent blocks pt)
  (setq blocks '())
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blockName))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq pt (GetBlockInsertPt ent))
        (if pt
          (setq blocks (cons (list ent pt) blocks))
        )
        (setq i (1+ i))
      )
    )
  )
  blocks
)

;;; ============================================================
;;; FIND NEAREST LABEL FOR A POINT
;;; ============================================================

(defun FindNearestLabel (blockPt labels / nearest nearestDist secondDist label labelPt dist)
  (setq nearest nil)
  (setq nearestDist 1e20)
  (setq secondDist 1e20)
  
  (foreach label labels
    (setq labelPt (cadr label))
    (setq dist (distance blockPt labelPt))
    
    (if (< dist nearestDist)
      (progn
        (setq secondDist nearestDist)
        (setq nearestDist dist)
        (setq nearest label)
      )
      (if (< dist secondDist)
        (setq secondDist dist)
      )
    )
  )
  
  (if (and nearest (< nearestDist *MAX-SEARCH-DIST*))
    (list nearest nearestDist secondDist)
    nil
  )
)

;;; ============================================================
;;; COLLECT LABELS WITH MULTIPLE SEARCH TERMS
;;; ============================================================

(defun CollectLabelsMultiple (searchTextList / ss i ent labels content pt found)
  (setq labels '())
  (setq ss (ssget "X" '((0 . "MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq content (GetMTextContent ent))
        (setq found nil)
        ;; Check if content matches ANY of the search terms
        (foreach searchText searchTextList
          (if (and (not found) (TextContains content searchText))
            (setq found T)
          )
        )
        (if found
          (progn
            (setq pt (GetMTextInsertPt ent))
            (if pt
              (setq labels (cons (list ent pt content) labels))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  labels
)

;;; ============================================================
;;; CONNECT BORE+DROP POLYLINES TO BORE+DROP LABELS (COMBINED)
;;; ============================================================

(defun ConnectBoreDropCombined (layerName / 
                                 polylines labels pline plinePt result
                                 labelData label labelPt labelContent nearDist secondDist isAmbiguous
                                 connectedCount ambiguousCount noLabelCount usedLabels availableLabels
                                 labelType)
  
  (princ (strcat "\n\nProcessing BORE+DROP Combined: Layer \"" layerName "\""))
  (princ "\n  Matching polylines to BORE or DROP labels...")
  
  ;; Collect polylines from BORE layer
  (setq polylines (CollectPolylines layerName))
  
  ;; Collect BOTH BORE and DROP labels
  (setq labels (CollectLabelsMultiple (list *BORE-LABEL-TEXT* *DROP-LABEL-TEXT*)))
  
  (princ (strcat "\n  Found " (itoa (length polylines)) " polylines on \"" layerName "\""))
  (princ (strcat "\n  Found " (itoa (length labels)) " labels (BORE + DROP)"))
  (princ (strcat "\n  Search radius: " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
  (princ (strcat "\n  Output layer: " *CONNECTOR-LAYER-BORE*))
  
  (if (= (length polylines) 0)
    (progn
      (princ (strcat "\n  *** No polylines on layer \"" layerName "\"! ***"))
      (list 0 0)
    )
    (if (= (length labels) 0)
      (progn
        (princ "\n  *** No BORE or DROP labels found! ***")
        (list 0 0)
      )
      (progn
        (setq connectedCount 0)
        (setq ambiguousCount 0)
        (setq noLabelCount 0)
        (setq usedLabels '())
        
        (foreach pline polylines
          (setq plinePt (cadr pline))
          
          (setq availableLabels (vl-remove-if
            '(lambda (x) (member (car x) usedLabels))
            labels))
          
          (setq result (FindNearestLabel plinePt availableLabels))
          
          (if result
            (progn
              (setq labelData (car result))
              (setq label (car labelData))
              (setq labelPt (cadr labelData))
              (setq labelContent (caddr labelData))
              (setq nearDist (cadr result))
              (setq secondDist (caddr result))
              
              ;; Determine label type for reporting
              (if (TextContains labelContent *BORE-LABEL-TEXT*)
                (setq labelType "BORE")
                (setq labelType "DROP")
              )
              
              (setq isAmbiguous (and (< secondDist 1e19) 
                                     (< (- secondDist nearDist) *AMBIGUOUS-THRESHOLD*)))
              
              (if isAmbiguous
                (progn
                  (DrawLine plinePt labelPt *PROBLEM-LAYER* 1)
                  (DrawCircle plinePt 10.0 *PROBLEM-LAYER* 1)
                  (setq ambiguousCount (1+ ambiguousCount))
                )
                (progn
                  (DrawLine plinePt labelPt *CONNECTOR-LAYER-BORE* 256)
                  (setq usedLabels (cons label usedLabels))
                  (setq connectedCount (1+ connectedCount))
                )
              )
            )
            (progn
              (DrawCircle plinePt 15.0 *PROBLEM-LAYER* 1)
              (setq noLabelCount (1+ noLabelCount))
            )
          )
        )
        
        (princ (strcat "\n  Connected: " (itoa connectedCount)))
        (if (> ambiguousCount 0)
          (princ (strcat "\n  Ambiguous: " (itoa ambiguousCount) " (RED)"))
        )
        (if (> noLabelCount 0)
          (princ (strcat "\n  No label found: " (itoa noLabelCount) " (RED)"))
        )
        
        (list connectedCount (+ ambiguousCount noLabelCount))
      )
    )
  )
)

;;; ============================================================
;;; GENERIC: CONNECT POLYLINES TO LABELS (with custom layer)
;;; ============================================================

(defun ConnectPolylinesToLabels (layerName labelSearchText displayName connectorLayer / 
                                  polylines labels pline plinePt result
                                  labelData label labelPt nearDist secondDist isAmbiguous
                                  connectedCount ambiguousCount noLabelCount usedLabels availableLabels)
  
  (princ (strcat "\n\nProcessing " displayName ": Layer \"" layerName "\" -> \"" labelSearchText "\""))
  
  (setq polylines (CollectPolylines layerName))
  (setq labels (CollectLabels labelSearchText))
  
  (princ (strcat "\n  Found " (itoa (length polylines)) " polylines"))
  (princ (strcat "\n  Found " (itoa (length labels)) " labels"))
  (princ (strcat "\n  Search radius: " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
  (princ (strcat "\n  Output layer: " connectorLayer))
  
  (if (= (length polylines) 0)
    (progn
      (princ (strcat "\n  *** No polylines on layer \"" layerName "\"! ***"))
      (list 0 0)
    )
    (if (= (length labels) 0)
      (progn
        (princ (strcat "\n  *** No labels containing \"" labelSearchText "\"! ***"))
        (list 0 0)
      )
      (progn
        (setq connectedCount 0)
        (setq ambiguousCount 0)
        (setq noLabelCount 0)
        (setq usedLabels '())
        
        (foreach pline polylines
          (setq plinePt (cadr pline))
          
          (setq availableLabels (vl-remove-if
            '(lambda (x) (member (car x) usedLabels))
            labels))
          
          (setq result (FindNearestLabel plinePt availableLabels))
          
          (if result
            (progn
              (setq labelData (car result))
              (setq label (car labelData))
              (setq labelPt (cadr labelData))
              (setq nearDist (cadr result))
              (setq secondDist (caddr result))
              
              (setq isAmbiguous (and (< secondDist 1e19) 
                                     (< (- secondDist nearDist) *AMBIGUOUS-THRESHOLD*)))
              
              (if isAmbiguous
                (progn
                  (DrawLine plinePt labelPt *PROBLEM-LAYER* 1)
                  (DrawCircle plinePt 10.0 *PROBLEM-LAYER* 1)
                  (setq ambiguousCount (1+ ambiguousCount))
                )
                (progn
                  (DrawLine plinePt labelPt connectorLayer 256)
                  (setq usedLabels (cons label usedLabels))
                  (setq connectedCount (1+ connectedCount))
                )
              )
            )
            (progn
              (DrawCircle plinePt 15.0 *PROBLEM-LAYER* 1)
              (setq noLabelCount (1+ noLabelCount))
            )
          )
        )
        
        (princ (strcat "\n  Connected: " (itoa connectedCount)))
        (if (> ambiguousCount 0)
          (princ (strcat "\n  Ambiguous: " (itoa ambiguousCount) " (RED)"))
        )
        (if (> noLabelCount 0)
          (princ (strcat "\n  No label found: " (itoa noLabelCount) " (RED)"))
        )
        
        (list connectedCount (+ ambiguousCount noLabelCount))
      )
    )
  )
)

;;; ============================================================
;;; GENERIC: CONNECT BLOCKS TO LABELS (with custom layer)
;;; ============================================================

(defun ConnectBlocksToLabels (blockName labelSearchText connectorLayer / blocks labels block blockPt result
                               labelData label labelPt nearDist secondDist isAmbiguous
                               connectedCount ambiguousCount noLabelCount usedLabels availableLabels)
  
  (princ (strcat "\n\nProcessing: " blockName " -> \"" labelSearchText "\""))
  
  (setq blocks (CollectBlocks blockName))
  (setq labels (CollectLabels labelSearchText))
  
  (princ (strcat "\n  Found " (itoa (length blocks)) " blocks"))
  (princ (strcat "\n  Found " (itoa (length labels)) " labels"))
  (princ (strcat "\n  Search radius: " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
  (princ (strcat "\n  Output layer: " connectorLayer))
  
  (if (= (length blocks) 0)
    (progn
      (princ (strcat "\n  *** No blocks named \"" blockName "\"! ***"))
      (list 0 0)
    )
    (if (= (length labels) 0)
      (progn
        (princ (strcat "\n  *** No labels containing \"" labelSearchText "\"! ***"))
        (list 0 0)
      )
      (progn
        (setq connectedCount 0)
        (setq ambiguousCount 0)
        (setq noLabelCount 0)
        (setq usedLabels '())
        
        (foreach block blocks
          (setq blockPt (cadr block))
          
          (setq availableLabels (vl-remove-if
            '(lambda (x) (member (car x) usedLabels))
            labels))
          
          (setq result (FindNearestLabel blockPt availableLabels))
          
          (if result
            (progn
              (setq labelData (car result))
              (setq label (car labelData))
              (setq labelPt (cadr labelData))
              (setq nearDist (cadr result))
              (setq secondDist (caddr result))
              
              (setq isAmbiguous (and (< secondDist 1e19) 
                                     (< (- secondDist nearDist) *AMBIGUOUS-THRESHOLD*)))
              
              (if isAmbiguous
                (progn
                  (DrawLine blockPt labelPt *PROBLEM-LAYER* 1)
                  (DrawCircle blockPt 10.0 *PROBLEM-LAYER* 1)
                  (setq ambiguousCount (1+ ambiguousCount))
                )
                (progn
                  (DrawLine blockPt labelPt connectorLayer 256)
                  (setq usedLabels (cons label usedLabels))
                  (setq connectedCount (1+ connectedCount))
                )
              )
            )
            (progn
              (DrawCircle blockPt 15.0 *PROBLEM-LAYER* 1)
              (setq noLabelCount (1+ noLabelCount))
            )
          )
        )
        
        (princ (strcat "\n  Connected: " (itoa connectedCount)))
        (if (> ambiguousCount 0)
          (princ (strcat "\n  Ambiguous: " (itoa ambiguousCount) " (RED)"))
        )
        (if (> noLabelCount 0)
          (princ (strcat "\n  No label found: " (itoa noLabelCount) " (RED)"))
        )
        
        (list connectedCount (+ ambiguousCount noLabelCount))
      )
    )
  )
)

;;; ============================================================
;;; MAIN COMMAND - CONNECT ALL
;;; ============================================================

(defun c:CONNECTLABELS (/ totalConn totalProb result oldCmd oldOs)
  
  (setq oldCmd (getvar "CMDECHO"))
  (setq oldOs (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  (princ "\n")
  (princ "\n============================================================")
  (princ "\n     BLOCK/POLYLINE TO LABEL CONNECTOR")
  (princ "\n============================================================")
  
  ;; Create all connector layers
  (MakeLayer *CONNECTOR-LAYER-BORE* 8)
  (MakeLayer *CONNECTOR-LAYER-TOBY* 8)
  (MakeLayer *CONNECTOR-LAYER-DAP* 8)
  (MakeLayer *CONNECTOR-LAYER-HANDHOLE* 8)
  (MakeLayer *PROBLEM-LAYER* 1)
  
  (setq totalConn 0)
  (setq totalProb 0)
  
  ;; Connect TOBY blocks -> maplines_toby
  (setq result (ConnectBlocksToLabels *TOBY-BLOCK* *TOBY-LABEL-TEXT* *CONNECTOR-LAYER-TOBY*))
  (if result
    (progn
      (setq totalConn (+ totalConn (car result)))
      (setq totalProb (+ totalProb (cadr result)))
    )
  )
  
  ;; Connect DAP blocks -> maplines
  (setq result (ConnectBlocksToLabels *DAP-BLOCK* *DAP-LABEL-TEXT* *CONNECTOR-LAYER-DAP*))
  (if result
    (progn
      (setq totalConn (+ totalConn (car result)))
      (setq totalProb (+ totalProb (cadr result)))
    )
  )
  
  ;; Connect BORE + DROP polylines -> maplines_bore
  (setq result (ConnectBoreDropCombined *BORE-LAYER-NAME*))
  (if result
    (progn
      (setq totalConn (+ totalConn (car result)))
      (setq totalProb (+ totalProb (cadr result)))
    )
  )
  
  ;; Connect HANDHOLE blocks -> Mapline_hh
  (setq result (ConnectBlocksToLabels *HANDHOLE-BLOCK* *HANDHOLE-LABEL-TEXT* *CONNECTOR-LAYER-HANDHOLE*))
  (if result
    (progn
      (setq totalConn (+ totalConn (car result)))
      (setq totalProb (+ totalProb (cadr result)))
    )
  )
  
  ;; Summary
  (princ "\n")
  (princ "\n============================================================")
  (princ "\n                    COMPLETE!")
  (princ "\n============================================================")
  (princ (strcat "\n  Total Connected: " (itoa totalConn)))
  (princ (strcat "\n  Total Problems:  " (itoa totalProb)))
  (princ "\n")
  (princ "\n  CONNECTOR LAYERS:")
  (princ (strcat "\n    BORE/DROP:  " *CONNECTOR-LAYER-BORE*))
  (princ (strcat "\n    TOBY:       " *CONNECTOR-LAYER-TOBY*))
  (princ (strcat "\n    DAP:        " *CONNECTOR-LAYER-DAP*))
  (princ (strcat "\n    HANDHOLE:   " *CONNECTOR-LAYER-HANDHOLE*))
  (princ (strcat "\n  Problems:     " *PROBLEM-LAYER* " (RED)"))
  (if (> totalProb 0)
    (progn
      (princ "\n")
      (princ "\n  *** RED CIRCLES = Manual review needed ***")
      (princ "\n  Type ZOOMPROBLEMS to find them")
    )
  )
  (princ "\n============================================================")
  
  (setvar "CMDECHO" oldCmd)
  (setvar "OSMODE" oldOs)
  (princ)
)

;;; ============================================================
;;; INDIVIDUAL CONNECT COMMANDS
;;; ============================================================

(defun c:CONNECTTOBY ()
  (MakeLayer *CONNECTOR-LAYER-TOBY* 8)
  (MakeLayer *PROBLEM-LAYER* 1)
  (ConnectBlocksToLabels *TOBY-BLOCK* *TOBY-LABEL-TEXT* *CONNECTOR-LAYER-TOBY*)
  (princ)
)

(defun c:CONNECTDAP ()
  (MakeLayer *CONNECTOR-LAYER-DAP* 8)
  (MakeLayer *PROBLEM-LAYER* 1)
  (ConnectBlocksToLabels *DAP-BLOCK* *DAP-LABEL-TEXT* *CONNECTOR-LAYER-DAP*)
  (princ)
)

(defun c:CONNECTBORE ()
  (MakeLayer *CONNECTOR-LAYER-BORE* 8)
  (MakeLayer *PROBLEM-LAYER* 1)
  (ConnectBoreDropCombined *BORE-LAYER-NAME*)
  (princ)
)

;; Alias - CONNECTDROP does the same as CONNECTBORE now
(defun c:CONNECTDROP ()
  (princ "\n  NOTE: BORE and DROP are combined - running combined match...")
  (c:CONNECTBORE)
)

(defun c:CONNECTHH ()
  (MakeLayer *CONNECTOR-LAYER-HANDHOLE* 8)
  (MakeLayer *PROBLEM-LAYER* 1)
  (ConnectBlocksToLabels *HANDHOLE-BLOCK* *HANDHOLE-LABEL-TEXT* *CONNECTOR-LAYER-HANDHOLE*)
  (princ)
)

;;; ============================================================
;;; ZOOM TO PROBLEMS
;;; ============================================================

(defun c:ZOOMPROBLEMS (/ ss)
  (setq ss (ssget "X" (list (cons 8 *PROBLEM-LAYER*))))
  (if ss
    (progn
      (command "_.ZOOM" "O" ss "")
      (princ (strcat "\n  Found " (itoa (sslength ss)) " problem objects"))
      (princ "\n  Use ZOOM Previous (ZP) to go back")
    )
    (princ "\n  No problems found - all good!")
  )
  (princ)
)

;;; ============================================================
;;; CLEAR CONNECTORS
;;; ============================================================

(defun c:CLEARCONNECTORS (/ ss count)
  (setq count 0)
  
  ;; Clear BORE connector layer
  (setq ss (ssget "X" (list (cons 8 *CONNECTOR-LAYER-BORE*))))
  (if ss (progn (setq count (+ count (sslength ss))) (command "_.ERASE" ss "")))
  
  ;; Clear TOBY connector layer
  (setq ss (ssget "X" (list (cons 8 *CONNECTOR-LAYER-TOBY*))))
  (if ss (progn (setq count (+ count (sslength ss))) (command "_.ERASE" ss "")))
  
  ;; Clear DAP connector layer
  (setq ss (ssget "X" (list (cons 8 *CONNECTOR-LAYER-DAP*))))
  (if ss (progn (setq count (+ count (sslength ss))) (command "_.ERASE" ss "")))
  
  ;; Clear HANDHOLE connector layer
  (setq ss (ssget "X" (list (cons 8 *CONNECTOR-LAYER-HANDHOLE*))))
  (if ss (progn (setq count (+ count (sslength ss))) (command "_.ERASE" ss "")))
  
  ;; Clear problems layer
  (setq ss (ssget "X" (list (cons 8 *PROBLEM-LAYER*))))
  (if ss (progn (setq count (+ count (sslength ss))) (command "_.ERASE" ss "")))
  
  (princ (strcat "\n  Cleared " (itoa count) " connector objects"))
  (princ)
)

;;; ============================================================
;;; SETTINGS COMMAND
;;; ============================================================

(defun c:CONNECTSETTINGS ()
  (princ "\n")
  (princ "\n============================================================")
  (princ "\n                 CONNECTOR SETTINGS")
  (princ "\n============================================================")
  (princ "\n")
  (princ "\n  BLOCKS:")
  (princ (strcat "\n    TOBY:      \"" *TOBY-BLOCK* "\""))
  (princ (strcat "\n    DAP:       \"" *DAP-BLOCK* "\""))
  (princ (strcat "\n    HANDHOLE:  \"" *HANDHOLE-BLOCK* "\""))
  (princ "\n")
  (princ "\n  POLYLINES (BORE + DROP combined):")
  (princ (strcat "\n    Layer:     \"" *BORE-LAYER-NAME* "\""))
  (princ "\n    Matches:   BORE labels AND DROP labels")
  (princ "\n")
  (princ "\n  LABEL SEARCH TEXT:")
  (princ (strcat "\n    TOBY:      \"" *TOBY-LABEL-TEXT* "\""))
  (princ (strcat "\n    DAP:       \"" *DAP-LABEL-TEXT* "\""))
  (princ (strcat "\n    BORE:      \"" *BORE-LABEL-TEXT* "\""))
  (princ (strcat "\n    DROP:      \"" *DROP-LABEL-TEXT* "\""))
  (princ (strcat "\n    HANDHOLE:  \"" *HANDHOLE-LABEL-TEXT* "\""))
  (princ "\n")
  (princ (strcat "\n  Max Search Distance:  " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
  (princ (strcat "\n  Ambiguous Threshold:  " (rtos *AMBIGUOUS-THRESHOLD* 2 1) " units"))
  (princ "\n")
  (princ "\n  OUTPUT LAYERS (Separate for each type):")
  (princ (strcat "\n    BORE/DROP: \"" *CONNECTOR-LAYER-BORE* "\""))
  (princ (strcat "\n    TOBY:      \"" *CONNECTOR-LAYER-TOBY* "\""))
  (princ (strcat "\n    DAP:       \"" *CONNECTOR-LAYER-DAP* "\""))
  (princ (strcat "\n    HANDHOLE:  \"" *CONNECTOR-LAYER-HANDHOLE* "\""))
  (princ (strcat "\n    Problems:  \"" *PROBLEM-LAYER* "\" (RED)"))
  (princ "\n============================================================")
  (princ)
)

;;; ============================================================
;;; SET SEARCH DISTANCE
;;; ============================================================

(defun c:SETDIST (/ newDist)
  (princ (strcat "\n  Current search distance: " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
  (setq newDist (getreal "\n  Enter new search distance: "))
  (if newDist
    (progn
      (setq *MAX-SEARCH-DIST* newDist)
      (princ (strcat "\n  Search distance set to: " (rtos *MAX-SEARCH-DIST* 2 1) " units"))
    )
    (princ "\n  Cancelled")
  )
  (princ)
)

;;; ============================================================
;;; SET LAYER NAMES (INTERACTIVE)
;;; ============================================================

(defun c:SETLAYERS (/ input)
  (princ "\n")
  (princ "\n  Current polyline layer: " )
  (princ *BORE-LAYER-NAME*)
  (setq input (getstring T "\n  New layer name (Enter to keep): "))
  (if (> (strlen input) 0) (setq *BORE-LAYER-NAME* input))
  
  (princ (strcat "\n  Polyline layer: " *BORE-LAYER-NAME*))
  (princ "\n  (BORE + DROP polylines should be on this layer)")
  (princ)
)

;;; ============================================================
;;; QUICK STATS - COUNT OBJECTS
;;; ============================================================

(defun c:CONNECTSTATS (/ ss boreLabels dropLabels)
  (princ "\n")
  (princ "\n============================================================")
  (princ "\n                 OBJECT COUNT")
  (princ "\n============================================================")
  
  ;; Count blocks
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 *TOBY-BLOCK*))))
  (princ (strcat "\n  TOBY blocks:        " (if ss (itoa (sslength ss)) "0")))
  
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 *DAP-BLOCK*))))
  (princ (strcat "\n  DAP blocks:         " (if ss (itoa (sslength ss)) "0")))
  
  (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 *HANDHOLE-BLOCK*))))
  (princ (strcat "\n  HANDHOLE blocks:    " (if ss (itoa (sslength ss)) "0")))
  
  ;; Count polylines (BORE layer has both BORE and DROP)
  (setq ss (ssget "X" (list (cons 0 "*POLYLINE") (cons 8 *BORE-LAYER-NAME*))))
  (princ (strcat "\n  BORE/DROP polylines: " (if ss (itoa (sslength ss)) "0")))
  (princ (strcat "  (on layer \"" *BORE-LAYER-NAME* "\")"))
  
  ;; Count labels
  (princ "\n")
  (princ (strcat "\n  TOBY labels:        " (itoa (length (CollectLabels *TOBY-LABEL-TEXT*)))))
  (princ (strcat "\n  DAP labels:         " (itoa (length (CollectLabels *DAP-LABEL-TEXT*)))))
  
  (setq boreLabels (length (CollectLabels *BORE-LABEL-TEXT*)))
  (setq dropLabels (length (CollectLabels *DROP-LABEL-TEXT*)))
  (princ (strcat "\n  BORE labels:        " (itoa boreLabels)))
  (princ (strcat "\n  DROP labels:        " (itoa dropLabels)))
  (princ (strcat "\n  BORE+DROP total:    " (itoa (+ boreLabels dropLabels))))
  
  (princ (strcat "\n  HANDHOLE labels:    " (itoa (length (CollectLabels *HANDHOLE-LABEL-TEXT*)))))
  
  (princ "\n============================================================")
  (princ)
)

;;; ============================================================
;;; LOAD MESSAGE
;;; ============================================================

(princ "\n")
(princ "\n============================================================")
(princ "\n     BLOCK/POLYLINE TO LABEL CONNECTOR")
(princ "\n     BORE + DROP combined on same layer!")
(princ "\n============================================================")
(princ "\n")
(princ "\n  MAIN COMMANDS:")
(princ "\n    CONNECTLABELS   - Connect ALL types")
(princ "\n    CONNECTSTATS    - Count objects before connecting")
(princ "\n")
(princ "\n  INDIVIDUAL:")
(princ "\n    CONNECTTOBY     - TOBY blocks only")
(princ "\n    CONNECTDAP      - DAP blocks only")
(princ "\n    CONNECTBORE     - BORE+DROP polylines")
(princ "\n    CONNECTHH       - Handhole blocks only")
(princ "\n")
(princ "\n  UTILITIES:")
(princ "\n    SETDIST         - Change search distance")
(princ "\n    SETLAYERS       - Change polyline layer name")
(princ "\n    ZOOMPROBLEMS    - Zoom to red problems")
(princ "\n    CLEARCONNECTORS - Erase all connector lines")
(princ "\n    CONNECTSETTINGS - View all settings")
(princ "\n")
(princ "\n  OUTPUT LAYERS:")
(princ (strcat "\n    BORE/DROP -> " *CONNECTOR-LAYER-BORE*))
(princ (strcat "\n    TOBY      -> " *CONNECTOR-LAYER-TOBY*))
(princ (strcat "\n    DAP       -> " *CONNECTOR-LAYER-DAP*))
(princ (strcat "\n    HANDHOLE  -> " *CONNECTOR-LAYER-HANDHOLE*))
(princ "\n")
(princ (strcat "\n  Polyline Layer: \"" *BORE-LAYER-NAME* "\""))
(princ (strcat "\n  Search Distance: " (rtos *MAX-SEARCH-DIST* 2 0) " units"))
(princ "\n  RED circles = Manual review needed")
(princ "\n============================================================")
(princ)