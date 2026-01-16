;;; ------------------------------------------------------------
;;; BLK_MTEXT (Pick & Apply) - Pure AutoLISP (no COM, no (command), no unwind-protect)
;;; 1) BLK_PICK_PRINT  - Pick insertion point for PRINT layouts
;;; 2) BLK_PICK_BORE   - Pick insertion point for BORE  layouts
;;; 3) BLK_APPLY       - Inserts across all matching layouts
;;;    PRINT* -> P_Mtext,   BORE* -> Bo_Mtext
;;; - Removes existing matching blocks near the point (with ssget + entdel)
;;; - Uses CTAB to switch tabs; uses entmake to create the INSERT entity
;;; Author: Ankam Nagaanish
;;; ------------------------------------------------------------

;; ------------------ SETTINGS ------------------
(setq *printPattern* "PRINT*")
(setq *borePattern*  "BORE*")
(setq *printBlock*   "P_Mtext")
(setq *boreBlock*    "Bo_Mtext1")
(setq *deleteTol*    0.50)   ; half-window size around point; bump up if your drawing units are mm
(setq *insertLayer*  "0")    ; layer to place inserts on
;; ----------------------------------------------

(defun _up (s) (if s (strcase s) ""))
(defun _ps-wcs (pt) (if pt (trans pt 1 0))) ; normalize to WCS (Paper Space)
(defun _block-exists-p (name) (and name (tblsearch "BLOCK" name)))

(defun _layouts-matching (patt / lst)
  (setq lst (layoutlist))
  (vl-remove-if-not
    '(lambda (nm) (and (/= (_up nm) "MODEL")
                       (wcmatch (_up nm) (_up patt))))
    lst))

(defun _erase-block-near (blkName pt tol / p1 p2 ss i en ed)
  ;; Pure AutoLISP: no (command)
  (if (and blkName pt)
    (progn
      (setq p1 (list (- (car pt) tol) (- (cadr pt) tol))
            p2 (list (+ (car pt) tol) (+ (cadr pt) tol)))
      (setq ss (ssget "_C" p1 p2 (list '(0 . "INSERT") (cons 2 blkName))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq en (ssname ss i))
            (if en (entdel en))
            (setq i (1+ i))
          )
        )
      )
    )
  )
  (princ)
)

(defun _entmake-insert (blkName pt / data)
  ;; Pure entmake INSERT in Paper Space of *current* layout (CTAB)
  ;; Minimal safe DXF set:
  ;;  (0 . "INSERT")
  ;;  (100 . "AcDbEntity")
  ;;  (67 . 1)                ; Paper Space flag
  ;;  (410 . (getvar 'CTAB))  ; Layout name
  ;;  (8 . *insertLayer*)
  ;;  (100 . "AcDbBlockReference")
  ;;  (2 . blkName)
  ;;  (10 x y 0.0)
  ;;  (41 . 1.0) (42 . 1.0) (43 . 1.0)
  ;;  (50 . 0.0)
  (setq data
    (list
      (cons 0  "INSERT")
      (cons 100 "AcDbEntity")
      (cons 67  1)
      (cons 410 (getvar 'CTAB))
      (cons 8  *insertLayer*)
      (cons 100 "AcDbBlockReference")
      (cons 2  blkName)
      (cons 10 (list (car pt) (cadr pt) 0.0))
      (cons 41 1.0)
      (cons 42 1.0)
      (cons 43 1.0)
      (cons 50 0.0)
    )
  )
  (entmakex data)
)

;; Session-stored pick points (Paper Space WCS)
(setq *ptPrint* nil)
(setq *ptBore*  nil)

(defun c:BLK_PICK_PRINT ( / pt )
  (princ "\nActivate any PRINT* layout tab and stay in Paper Space, then pick:")
  (setq pt (_ps-wcs (getpoint "\nPick PRINT point: ")))
  (if pt (setq *ptPrint* pt))
  (princ (strcat "\nPRINT point: " (vl-princ-to-string *ptPrint*)))
  (princ))

(defun c:BLK_PICK_BORE ( / pt )
  (princ "\nActivate any BORE* layout tab and stay in Paper Space, then pick:")
  (setq pt (_ps-wcs (getpoint "\nPick BORE point: ")))
  (if pt (setq *ptBore* pt))
  (princ (strcat "\nBORE point: " (vl-princ-to-string *ptBore*)))
  (princ))

(defun c:BLK_APPLY ( / cntP cntB L)
  (setq cntP 0 cntB 0)

  (if (and *ptPrint* (not (_block-exists-p *printBlock*)))
    (princ (strcat "\nWarning: Block '" *printBlock* "' not found in this DWG.")))
  (if (and *ptBore* (not (_block-exists-p *boreBlock*)))
    (princ (strcat "\nWarning: Block '" *boreBlock* "' not found in this DWG.")))

  ;; PRINT* batch
  (if *ptPrint*
    (foreach L (_layouts-matching *printPattern*)
      (setvar 'CTAB L)
      ;; Remove existing near then insert
      (_erase-block-near *printBlock* *ptPrint* *deleteTol*)
      (if (_block-exists-p *printBlock*)
        (if (_entmake-insert *printBlock* *ptPrint*)
          (setq cntP (1+ cntP))
        )
      )
    )
  )

  ;; BORE* batch
  (if *ptBore*
    (foreach L (_layouts-matching *borePattern*)
      (setvar 'CTAB L)
      (_erase-block-near *boreBlock* *ptBore* *deleteTol*)
      (if (_block-exists-p *boreBlock*)
        (if (_entmake-insert *boreBlock* *ptBore*)
          (setq cntB (1+ cntB))
        )
      )
    )
  )

  (princ
    (strcat
      (if *ptPrint* (strcat "\nInserted '" *printBlock* "' into " (itoa cntP) " PRINT layouts.") "")
      (if *ptBore*  (strcat "\nInserted '" *boreBlock*  "' into " (itoa cntB) " BORE layouts.")  "")
      (if (and (null *ptPrint*) (null *ptBore*)) "\nNothing to insert." "")
      "\n--- Done ---"
    )
  )
  (princ)
)

;; Helper to clear stored points
(defun c:BLK_CLEAR_PTS () (setq *ptPrint* nil *ptBore* nil) (princ "\nCleared stored points.") (princ))

;; Simple guide
(defun c:BLK_MTEXT ( / )
  (princ "\n1) Activate any PRINT* layout and run BLK_PICK_PRINT.")
  (princ "\n2) Activate any BORE*  layout and run BLK_PICK_BORE.")
  (princ "\n3) Run BLK_APPLY to insert across all matching layouts.")
  (princ))
;; Alias
(defun c:Blk_Mtext () (c:BLK_MTEXT))
