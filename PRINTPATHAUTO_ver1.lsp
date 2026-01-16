
;;; ------------------------------------------------------------
;;; PRINTPATHAUTO.lsp
;;; Scan every layout whose name contains "print" and:
;;;  - Read FCU table -> sum D-BORE and HAND DIG
;;;  - Update OVERALL PATH PLACEMENT -> DIRECTIONAL BORE and TRENCH
;;;  - NEW: Update EXCAVATION DATA -> DIRT based on PROPOSED UTILITY
;;; No user selection required.
;;; ------------------------------------------------------------

(defun _rows (tbl) (vla-get-Rows tbl))
(defun _cols (tbl) (vla-get-Columns tbl))

(defun _getCellText (tbl r c / txt)
  (setq txt "")
  (vl-catch-all-apply '(lambda () (setq txt (vla-GetText tbl r c))))
  (if txt (vl-string-trim " " (vl-princ-to-string txt)) "")
)

(defun _setCellText (tbl r c s)
  (vl-catch-all-apply
    '(lambda () (vla-SetText tbl r c (vl-princ-to-string s)))
  )
)

(defun _stripNonDigits (s / lst)
  (setq lst (vl-string->list (vl-princ-to-string s)))
  (setq lst (vl-remove-if-not
              '(lambda (ch) (and (>= ch 48) (<= ch 57))) ; '0'..'9'
              lst))
  (vl-list->string lst)
)

(defun _parseInt (s)
  (setq s (vl-string-trim " " (vl-princ-to-string s)))
  (setq s (_stripNonDigits s))
  (if (and s (> (strlen s) 0)) (atoi s) 0)
)

(defun _findColByHeader (tbl header / rows cols r c hdr found idx limit)
  ;; Look for header text in the first two rows
  (setq rows (_rows tbl) cols (_cols tbl))
  (setq found nil idx nil)
  (setq limit (if (> rows 2) 2 rows))
  (setq r 0)
  (while (and (not found) (< r limit))
    (setq c 0)
    (while (and (not found) (< c cols))
      (setq hdr (_getCellText tbl r c))
      (if (and hdr (wcmatch (strcase hdr) (strcat "*" (strcase header) "*")))
        (progn (setq idx c) (setq found T))
      )
      (setq c (1+ c))
    )
    (setq r (1+ r))
  )
  idx
)

(defun _isHeaderRow (tbl r colA colB)
  (or (and colA (wcmatch (strcase (_getCellText tbl r colA)) "*ITEM*"))
      (and colB (wcmatch (strcase (_getCellText tbl r colB)) "*QUANTITY*")))
)

(defun _tableHasTitle (tbl titlePatterns / rows cols r c txt found)
  ;; Check first 3 rows for any title pattern
  (setq rows (_rows tbl) cols (_cols tbl))
  (setq found nil)
  (setq r 0)
  (while (and (not found) (< r (min 3 rows)))
    (setq c 0)
    (while (and (not found) (< c cols))
      (setq txt (_getCellText tbl r c))
      (if txt
        (foreach pat titlePatterns
          (if (wcmatch (strcase txt) (strcase pat))
            (setq found T)
          )
        )
      )
      (setq c (1+ c))
    )
    (setq r (1+ r))
  )
  found
)

(defun _sumByItemPatterns (tbl patList / rows cols itemCol qtyCol startRow r itemTxt sum)
  (setq rows (_rows tbl) cols (_cols tbl))
  (setq itemCol (_findColByHeader tbl "ITEM"))
  (setq qtyCol  (_findColByHeader tbl "QUANTITY"))
  (if (and (not (null itemCol)) (not (null qtyCol)))
    (progn
      (setq startRow 0)
      (if (_isHeaderRow tbl 0 itemCol qtyCol) (setq startRow 1))
      (if (and (< 1 rows) (_isHeaderRow tbl 1 itemCol qtyCol)) (setq startRow 2))
      (setq sum 0)
      (setq r startRow)
      (while (< r rows)
        (setq itemTxt (_getCellText tbl r itemCol))
        (if itemTxt
          (foreach pat patList
            (if (wcmatch (strcase itemTxt) (strcase pat))
              (setq sum (+ sum (_parseInt (_getCellText tbl r qtyCol))))
            )
          )
        )
        (setq r (1+ r))
      )
      sum
    )
    nil
  )
)

(defun _updateMethodQty (tbl methodPatterns value / rows cols methodCol qtyCol startRow r methodTxt done)
  (setq rows (_rows tbl) cols (_cols tbl))
  (setq methodCol (_findColByHeader tbl "METHOD"))
  (setq qtyCol    (_findColByHeader tbl "QUANTITY"))
  (if (null methodCol) (setq methodCol 0))
  (if (null qtyCol)    (setq qtyCol (if (> cols 1) 1 0)))
  (setq startRow 0)
  (if (or (wcmatch (strcase (_getCellText tbl 0 methodCol)) "*METHOD*")
          (wcmatch (strcase (_getCellText tbl 0 qtyCol))    "*QUANTITY*"))
    (setq startRow 1))
  (if (and (< 1 rows)
           (or (wcmatch (strcase (_getCellText tbl 1 methodCol)) "*METHOD*")
               (wcmatch (strcase (_getCellText tbl 1 qtyCol))    "*QUANTITY*")))
    (setq startRow 2))
  (setq r startRow done nil)
  (while (and (not done) (< r rows))
    (setq methodTxt (_getCellText tbl r methodCol))
    (if methodTxt
      (foreach pat methodPatterns
        (if (wcmatch (strcase methodTxt) (strcase pat))
          (progn (_setCellText tbl r qtyCol (strcat (itoa value) " FT.")) (setq done T))
        )
      )
    )
    (setq r (1+ r))
  )
  done
)

;;; ---------- Guarded: prevents incorrect object to bind ----------
(defun _findTablesInLayout (lay / blk srcTbl dstTbl)
  ;; Returns a dotted pair (srcTbl . dstTbl)
  (setq blk (vla-get-Block lay))
  (setq srcTbl nil dstTbl nil)
  (if (and blk (eq (type blk) 'VLA-OBJECT))
    (vlax-for obj blk
      (if (and (eq (strcase (vla-get-ObjectName obj)) "ACDBTABLE")
               (not (and srcTbl dstTbl)))
        (progn
          (if (and (not srcTbl)
                   (_tableHasTitle obj '("*FIBER*CORE*UNIT*" "*FCU*")))
            (setq srcTbl obj))
          (if (and (not dstTbl)
                   (_tableHasTitle obj '("*OVERALL*PATH*PLACEMENT*")))
            (setq dstTbl obj))
        )
      )
    )
  )
  (cons srcTbl dstTbl)
)

;;; ------------------------------------------------------------
;;; NEW HELPERS: EXCAVATION DATA → DIRT based on PROPOSED UTILITY
;;; ------------------------------------------------------------

(defun _findExcavationTable (lay / blk excTbl)
  (setq blk (vla-get-Block lay))
  (setq excTbl nil)
  (if (and blk (eq (type blk) 'VLA-OBJECT))
    (vlax-for obj blk
      (if (and (eq (strcase (vla-get-ObjectName obj)) "ACDBTABLE")
               (not excTbl))
        (if (_tableHasTitle obj '("*EXCAVATION*DATA*"))
          (setq excTbl obj)
        )
      )
    )
  )
  excTbl
)

(defun _removeParensNumber (s / p q)
  ;; Remove leading "(n)" and any following space: "(6) 4'X4' PIT" -> "4'X4' PIT"
  (setq s (vl-princ-to-string s))
  (setq p (vl-string-search "(" s))
  (setq q (vl-string-search ")" s))
  (if (and p q (> q p))
    (substr s (+ q 2)) ; skips ") " if present
    s
  )
)

(defun _parseCountFromLeadingParens (s / p q cnt)
  ;; Parses "(n)" as count; defaults to 1 if not found
  (setq s (vl-princ-to-string s))
  (setq p (vl-string-search "(" s))
  (setq q (vl-string-search ")" s))
  (if (and p q (> q p))
    (setq cnt (_parseInt (substr s (+ p 2) (- q p 1))))
    (setq cnt 1)
  )
  cnt
)

(defun _parseDimensionsToFeet (s / chars i ch digits num val vals)
  ;; Parses numbers and units:
  ;; ' (39) => feet, " (34) => inches, none => assume feet.
  ;; Returns list of dims in feet: (len-ft wid-ft depth-ft ...)
  (setq s (vl-princ-to-string s))
  (setq chars (vl-string->list s))
  (setq i 0 vals '() digits '())
  (while (< i (length chars))
    (setq ch (nth i chars))
    (cond
      ((and (>= ch 48) (<= ch 57))
       (setq digits (cons ch digits)))
      (T
       (if digits
         (progn
           (setq num (atoi (vl-list->string (reverse digits))))
           (cond
             ((= ch 39) (setq val (* 1.0 num)))   ; feet '
             ((= ch 34) (setq val (/ num 12.0)))  ; inches "
             (T         (setq val (* 1.0 num)))   ; default feet
           )
           (setq vals (append vals (list val)))
           (setq digits '())
         )
       )
      )
    )
    (setq i (1+ i))
  )
  ;; Trailing number without unit -> assume feet
  (if digits
    (setq vals (append vals (list (atof (vl-list->string (reverse digits))))))
  )
  vals
)

(defun _updateExcavationDIRT (tbl / rows cols propCol dirtCol startRow r txt cnt dims area didAny d1 d2)
  (setq rows (_rows tbl) cols (_cols tbl))
  (setq propCol (_findColByHeader tbl "PROPOSED UTILITY"))
  (setq dirtCol (_findColByHeader tbl "DIRT"))
  (if (and (not (null propCol)) (not (null dirtCol)))
    (progn
      ;; Determine data start row (skip header rows if present)
      (setq startRow 0)
      (if (or (wcmatch (strcase (_getCellText tbl 0 propCol)) "*PROPOSED*UTILITY*")
              (wcmatch (strcase (_getCellText tbl 0 dirtCol))    "*DIRT*"))
        (setq startRow 1))
      (if (and (< 1 rows)
               (or (wcmatch (strcase (_getCellText tbl 1 propCol)) "*PROPOSED*UTILITY*")
                   (wcmatch (strcase (_getCellText tbl 1 dirtCol))    "*DIRT*")))
        (setq startRow 2))

      (setq r startRow didAny nil)
      (while (< r rows)
        (setq txt (_getCellText tbl r propCol))
        (if (and txt (> (strlen (vl-princ-to-string txt)) 0))
          (progn
            (setq cnt  (_parseCountFromLeadingParens txt))
            (setq dims (_parseDimensionsToFeet (_removeParensNumber txt)))

            ;; ---- Handling missing dimension values ----
            (cond
              ;; Case A: at least two dimensions → use first two
              ((>= (length dims) 2)
               (setq d1 (nth 0 dims))
               (setq d2 (nth 1 dims))
               (setq area (fix (* cnt d1 d2)))
               (_setCellText tbl r dirtCol (strcat (itoa area) " SQ. FT."))
               (setq didAny T)
              )

              ;; Case B: exactly one dimension → assume square (d × d)
              ((= (length dims) 1)
               (setq d1 (nth 0 dims))
               (setq area (fix (* cnt d1 d1)))
               (_setCellText tbl r dirtCol (strcat (itoa area) " SQ. FT."))
               (setq didAny T)
              )

              ;; Case C: no dimensions found → skip (leave cell unchanged)
              (T
               ;; OPTIONAL: pattern-based defaults (uncomment to use):
               ;; (cond
               ;;   ((wcmatch (strcase txt) "*PIT*")
               ;;    (setq d1 4.0 d2 4.0) ; default PIT size 4'x4'
               ;;    (setq area (fix (* cnt d1 d2)))
               ;;    (_setCellText tbl r dirtCol (strcat (itoa area) " SQ. FT."))
               ;;    (setq didAny T)
               ;;   )
               ;;   ((wcmatch (strcase txt) "*HH*")
               ;;    (setq d1 (/ 24.0 12.0) d2 (/ 36.0 12.0)) ; default HH 24"x36"
               ;;    (setq area (fix (* cnt d1 d2)))
               ;;    (_setCellText tbl r dirtCol (strcat (itoa area) " SQ. FT."))
               ;;    (setq didAny T)
               ;;   )
               ;; )
               ;; Else: leave unchanged or write "N/A" to flag:
               ;; (_setCellText tbl r dirtCol "N/A")
              )
            )
          )
        )
        (setq r (1+ r))
      )
      didAny
    )
    nil
  )
)

;;; ------------------------------------------------------------
;;; INTEGRATED COMMAND
;;; ------------------------------------------------------------

(defun c:PRINTPATHAUTO (/ acad doc lays curTab layName pair srcTbl dstTbl sumDB sumHD didDB didHD processed excTbl didExc)
  (vl-load-com)
  (setq acad (vlax-get-acad-object))
  (setq doc  (vla-get-ActiveDocument acad))
  (setq lays (vla-get-Layouts doc))
  (setq curTab (getvar "CTAB"))
  (setq processed 0)

  ;; Guard the collection to avoid binding errors
  (if (and lays (eq (type lays) 'VLA-OBJECT))
    (vlax-for lay lays
      (setq layName (vla-get-Name lay))
      (if (and (not (equal (strcase layName) "MODEL"))
               (wcmatch (strcase layName) "*PRINT*"))
        (progn
          (setvar "CTAB" layName) ;; activate (optional but safe)
          (setq pair   (_findTablesInLayout lay))
          (setq srcTbl (car pair))
          (setq dstTbl (cdr pair))
          (if (and srcTbl dstTbl)
            (progn
              ;; Read totals from FCU
              (setq sumDB (_sumByItemPatterns srcTbl '("*D-BORE*")))
              (setq sumHD (_sumByItemPatterns srcTbl '("*HAND*DIG*" "*HAND-DIG*")))
              (prompt (strcat "\n[" layName "] FCU totals → D-BORE=" (itoa (if sumDB sumDB 0)) " FT., HAND DIG=" (itoa (if sumHD sumHD 0)) " FT."))

              ;; Update destination table
              (setq didDB (_updateMethodQty dstTbl '("*DIRECTIONAL*BORE*" "*DIR*BORE*" "*D-BORE*") (if sumDB sumDB 0)))
              (setq didHD (_updateMethodQty dstTbl '("*TRENCH*") (if sumHD sumHD 0)))

              (prompt (strcat "\n[" layName "] OVERALL PATH PLACEMENT updated  "
                              (if didDB "DIRECTIONAL BORE ✓ " "DIRECTIONAL BORE ✗ ")
                              (if didHD "TRENCH ✓"            "TRENCH ✗")))

              ;; ------------------------------------------------------------
              ;; NEW: Update EXCAVATION DATA → DIRT based on PROPOSED UTILITY
              ;; Rule: DIRT (SQ. FT.) = Count × Length(ft) × Width(ft)
              ;; Uses first two dimensions only; ignores depth if present.
              ;; ------------------------------------------------------------
              (setq excTbl (_findExcavationTable lay))
              (if excTbl
                (progn
                  (setq didExc (_updateExcavationDIRT excTbl))
                  (prompt (strcat "\n[" layName "] EXCAVATION DATA → "
                                  (if didExc "DIRT updated ✓" "No updatable rows ✗")))
                )
                (prompt (strcat "\n[" layName "] Could not locate EXCAVATION DATA table."))
              )

              (setq processed (1+ processed))
            )
            (prompt (strcat "\n[" layName "] Could not locate FCU and/or OVERALL PATH PLACEMENT tables."))
          )
        )
      )
    )
  )

  (setvar "CTAB" curTab) ;; restore
  (prompt (strcat "\nFinished. Layouts processed: " (itoa processed)))
  (princ)
)
