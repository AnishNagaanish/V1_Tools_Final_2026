;;; ============================================================
;;; OSP_CleanupTools.lsp
;;; Author: (for Nagaanish)
;;; Commands:
;;;   - DELSLACK     : Delete MLeaders containing "slack loop" (case-insensitive)
;;;   - TrimMText    : Trim MTEXT before "ROUTE#:" if both ROUTE#: and DUCT#: exist
;;;   - CleanDrawing : Delete objects on target layers + block-content MLeaders on DIMS
;;;   - CleanDrawingDEBUG : Report counts only (no deletion)
;;;   - CLEANALL     : Runs TrimMText -> DELSLACK -> CleanDrawing
;;;
;;; Notes:
;;;   - Scans Model space and all Paper space layouts (not Xrefs).
;;;   - Each command is wrapped in an UNDO mark (one U undoes the operation).
;;;   - If items live inside Xrefs or nested blocks, see notes at end.
;;; ============================================================

(vl-load-com)

;; ------------ Utility Helpers ---------------------------------------

(defun _replace-all (s from to / pos)
  "Replace all occurrences of FROM with TO in string S."
  (while (and s (setq pos (vl-string-search from s)))
    (setq s (strcat (substr s 1 pos) to (substr s (+ pos (strlen from) 1))))
  )
  s
)

(defun _clean-mtext-for-match (s)
  "Reduce common MTEXT formatting for robust text matching."
  (if s
    (progn
      (setq s (_replace-all s "\\P" " "))
      (setq s (_replace-all s "\\p" " "))clean
      (vl-string-translate "{}\\;\r\n\t" "       " s)
    )
  )
)

(defun _safe-strcase (s) (if s (strcase s) ""))

(defun _start-undo (doc) (if doc (vla-StartUndoMark doc)))
(defun _end-undo   (doc) (if doc (vla-EndUndoMark   doc)))

(defun _same (a b) (= (strcase a) (strcase b)))

(defun _layer-item (doc name / lyr)
  "Return VLA Layer object by name (case-insensitive) or NIL."
  (setq lyr nil)
  (vlax-for L (vla-get-Layers doc)
    (if (_same (vla-get-Name L) name) (setq lyr L))
  )
  lyr
)

(defun _ensure-layer-ready (doc name / L)
  "Thaw/Unlock/TurnOn layer NAME if it exists. Returns T if found."
  (if (setq L (_layer-item doc name))
    (progn
      (if (= :vlax-true  (vla-get-Freeze  L)) (vla-put-Freeze  L :vlax-false))
      (if (= :vlax-true  (vla-get-Lock    L)) (vla-put-Lock    L :vlax-false))
      (if (= :vlax-false (vla-get-LayerOn L)) (vla-put-LayerOn L :vlax-true))
      T
    )
    nil
  )
)

(defun _make-layer-or-filter (layers)
  "Build an ssget OR filter for a list of layer names."
  (append
    (list (cons -4 "<OR"))
    (apply 'append (mapcar '(lambda (n) (list (cons 8 n))) layers))
    (list (cons -4 "OR>"))
  )
)

(defun _safe-prop (getter obj)
  "Call (getter obj) and return NIL if it errors."
  (if (vl-catch-all-error-p (vl-catch-all-apply getter (list obj)))
    nil
    (vl-catch-all-apply getter (list obj))
  )
)

(defun _mleader-has-block-content? (ml / ct bn txt)
  "Robustly determine if an MLeader uses BLOCK content."
  ;; Preferred: ContentType (0=MText, 1=Block)
  (setq ct (_safe-prop 'vla-get-ContentType ml))
  (cond
    ((and ct (numberp ct))
     (= ct 1)) ;; 1 = Block content
    (T
     ;; Fallback: ContentBlockName (non-empty & not 'Annotative' -> block)
     (setq bn (_safe-prop 'vla-get-ContentBlockName ml))
     (if (and bn (/= bn ""))
       (not (_same bn "Annotative"))
       ;; Last fallback: If TextString is empty/missing, likely block content
       (progn
         (setq txt (_safe-prop 'vla-get-TextString ml))
         (or (not txt) (= txt ""))
       )
     )
    )
  )
)

;; ============================================================
;; 1) DELSLACK — delete MLeaders whose text contains "slack loop"
;;    Case-insensitive; matches even with formatting/spacing in between.
;; ============================================================

(defun c:DELSLACK (/ *error* doc pat wc n txt)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (_end-undo doc) (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (_start-undo doc)

  ;; Prompt for phrase; default "Slack Loop"
  (princ "\nText to match (case-insensitive). Default = Slack Loop")
  (initget 128)
  (setq pat (getstring T "\nEnter text [press Enter for default]: "))
  (if (= pat "") (setq pat "Slack Loop"))

  (setq wc (strcat "*" (vl-string-translate " " "*" (strcase pat)) "*"))
  (setq n 0)

  (vlax-for blk (vla-get-Blocks doc)
    (if (= :vlax-true (vla-get-IsLayout blk))
      (vlax-for e blk
        (if (and (eq (vla-get-ObjectName e) "AcDbMLeader")
                 (not (vlax-erased-p e)))
          (progn
            (setq txt
              (cond
                ((not (vl-catch-all-error-p
                       (vl-catch-all-apply 'vla-get-TextString (list e))))
                 (_clean-mtext-for-match (vla-get-TextString e)))
                (T nil)
              )
            )
            (if (and txt (wcmatch (strcase txt) wc))
              (progn (vla-Delete e) (setq n (1+ n))))
          )
        )
      )
    )
  )

  (_end-undo doc)
  (princ (strcat "\nDeleted " (itoa n)
                 " MLeader(s) containing \"" (strcase pat) "\"."))
  (princ)
)

;; ============================================================
;; 2) TrimMText — keep text before 'ROUTE#:' if both ROUTE#: and DUCT#: exist
;;    Cleans \P to space before saving.
;; ============================================================

(defun c:TrimMText (/ *error* doc changed full routePos new)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (_end-undo doc) (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (_start-undo doc)
  (setq changed 0)

  (vlax-for blk (vla-get-Blocks doc)
    (if (= :vlax-true (vla-get-IsLayout blk))
      (vlax-for e blk
        (if (and (eq (vla-get-ObjectName e) "AcDbMText")
                 (not (vlax-erased-p e)))
          (progn
            (setq full (vla-get-TextString e))
            (if (and (vl-string-search "ROUTE#:" full)
                     (vl-string-search "DUCT#:"  full))
              (progn
                (setq routePos (vl-string-search "ROUTE#:" full))
                ;; Take content BEFORE ROUTE#:
                (setq new (substr full 1 routePos))
                ;; Clean \P breaks to spaces
                (setq new (_replace-all new "\\P" " "))
                (setq new (_replace-all new "\\p" " "))
                ;; Trim trailing spaces
                (while (and (> (strlen new) 0)
                            (= (substr new (strlen new) 1) " "))
                  (setq new (substr new 1 (1- (strlen new))))
                )
                (vla-put-TextString e new)
                (setq changed (1+ changed))
              )
            )
          )
        )
      )
    )
  )

  (_end-undo doc)
  (princ (strcat "\nTrimmed " (itoa changed) " MTEXT object(s)."))
  (princ)
)

;; ============================================================
;; 3) CleanDrawing — delete objects on target layers,
;;    and block-content MLeaders on layer DIMS.
;;    - Unlocks/thaws/turns on layers first.
;;    - Works in Model & Paper space.
;; ============================================================

(defun c:CleanDrawing (/ *error* doc targetLayers delCount blkDelCount ss i e obj
                         dimsReady layFilter)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg)))
    (_end-undo doc) (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (_start-undo doc)

  ;; Edit this list if needed:
  (setq targetLayers '("Add" "Drop" "Direction Arrow" "FIBER LINE"))

  ;; Ensure layers are modifiable
  (foreach L targetLayers (_ensure-layer-ready doc L))
  (setq dimsReady (_ensure-layer-ready doc "DIMS"))

  (setq delCount 0 blkDelCount 0)

  ;; Pass A — Delete anything on targetLayers
  (setq layFilter (_make-layer-or-filter targetLayers))
  (setq ss (ssget "X" layFilter))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (if (entdel e) (setq delCount (1+ delCount)))
        (setq i (1+ i))
      )
    )
  )

  ;; Pass B — Delete MLeaders on DIMS with BLOCK content
  (if dimsReady
    (progn
      (setq ss (ssget "X" '((0 . "MULTILEADER") (8 . "CALLOUTS"))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq e (ssname ss i)
                  obj (vlax-ename->vla-object e))
            (if (_mleader-has-block-content? obj)
              (progn (vla-Delete obj) (setq blkDelCount (1+ blkDelCount))))
            (setq i (1+ i))
          )
        )
      )
    )
  )

  (_end-undo doc)

  (princ (strcat
    "\nCleanDrawing summary:"
    "\n - Deleted " (itoa delCount) " object(s) on layers: "
    (vl-princ-to-string targetLayers)
    "\n - Deleted " (itoa blkDelCount) " block-content MLeader(s) on layer DIMS."
  ))
  (princ)
)

;; ============================================================
;; 4) CleanDrawingDEBUG — report what WOULD be deleted (no deletions)
;; ============================================================

(defun c:CleanDrawingDEBUG (/ doc targetLayers layFilter ss dimsSS counts i e layName
                              dimsCount obj)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (setq targetLayers '("Addresses" "Add" "Drop" "Direction Arrow"))

  ;; Count objects per target layer
  (setq counts (mapcar '(lambda (x) (cons x 0)) targetLayers))
  (setq layFilter (_make-layer-or-filter targetLayers))
  (setq ss (ssget "X" layFilter))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i)
              layName (cdr (assoc 8 (entget e))))
        (if layName
          (progn
            (foreach pair counts
              (if (_same (car pair) layName)
                (setcdr (assoc (car pair) counts) (1+ (cdr (assoc (car pair) counts)))))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (princ "\n[DEBUG] Objects found on target layers:")
  (foreach pair counts
    (princ (strcat "\n - " (car pair) ": " (itoa (cdr (assoc (car pair) counts)))))
  )

  ;; MLeaders on DIMS with block content
  (setq dimsSS (ssget "X" '((0 . "MULTILEADER") (8 . "CALLOUTS"))))
  (setq dimsCount 0)
  (if dimsSS
    (progn
      (setq i 0)
      (while (< i (sslength dimsSS))
        (setq e (ssname dimsSS i)
              obj (vlax-ename->vla-object e))
        (if (_mleader-has-block-content? obj)
          (setq dimsCount (1+ dimsCount)))
        (setq i (1+ i))
      )
    )
  )

  (princ (strcat "\n[DEBUG] Block-content MLeaders on DIMS: " (itoa dimsCount)))
  (princ "\n(No deletions performed in DEBUG.)")
  (princ)
)

;; ============================================================
;; 5) Convenience: run the 3 main actions in sequence
;; ============================================================

(defun c:CLEANALL ( / )
  (princ "\nRunning TrimMText...")      (c:TrimMText)
  (princ "\nRunning DELSLACK...")       (c:DELSLACK)
  (princ "\nRunning CleanDrawing...")   (c:CleanDrawing)
  (princ "\nCLEANALL complete.")        (princ)
)

(prompt
 "\nLoaded: Commands available - DELSLACK, TrimMText, CleanDrawing, CleanDrawingDEBUG, CLEANALL.")
(princ)
