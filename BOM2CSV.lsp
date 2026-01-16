;;; BOM_Export_FIXED.lsp
;;; CSV export for Bill of Materials from AutoCAD table

(vl-load-com)

;; -------- Configuration --------
(setq *BOM_DEFAULT_LAYOUT* "BILL OF MATERIALS")
(setq *BOM_MAIN_HEADER*    "Bill of Materials - Component Breakdown")
(setq *BOM_SECTIONS*       '("Network Elements" "Underground Infrastructure" "Route Summary" "ROUTE Summary" "Demand"))

;; -------- DEFAULT ITEMS --------
;; Format: (ITEM DESCRIPTION UOM MATCH-KEY)
;; MATCH-KEY is what to look for in AutoCAD Item column

(setq *DEFAULT_NETWORK*
  '(
    ("MPB302781UV8" "Microduct,7/3,5mm 1-way, Direct Buried, TW" "FT" "MPB302781UV8")
    ("MPB302782UV8" "Microduct,7/3,5mm 2-way, Direct Buried, TW" "FT" "MPB302782UV8")
    ("MPB3027812UV8" "Microduct,7/3,5mm 12-way, Direct Buried, TW" "FT" "MPB3027812UV8")
    ("MPB3027824UV8" "Microduct,7/3,5mm 24-way, Direct Buried, TW" "FT" "MPB3027824UV8")
    ("T1 FIBER_CABLE 12F" "FIBER_CABLE 12F" "FT" "12F")
  )
)

(setq *DEFAULT_UNDERGROUND*
  '(
    ("LV" "Large Vault (24 in x 36 in x 24 in)" "Count" "LV")
    ("DAP" "DUCT ACCESS POINTS" "Count" "DAP")
    ("TB" "TOBY BOXES" "Count" "TB")
    ("FDH" "FDH" "Count" "FDH")
    ("GROUND ROD" "GROUND ROD" "Count" "GROUND ROD")
  )
)

(setq *DEFAULT_ROUTE_SUMMARY*
  '(
    ("PATH UG" "PATH PLACEMENT UG (BORE)" "Length" "BORE")
    ("PATH UG" "PATH PLACEMENT UG (TRENCH)" "Length" "TRENCH")
  )
)

(setq *DEFAULT_DEMAND*
  '(
    ("DEMAND" "TOTAL SA COUNT" "Count" "DEMAND")
  )
)

;; -------- Utilities --------
(defun _str-trim (s)
  (if s (vl-string-trim " \t\r\n" s) "")
)

(defun _join (lst sep / s)
  (cond
    ((null lst) "")
    ((null (cdr lst)) (car lst))
    (t
      (setq s (car lst) lst (cdr lst))
      (while lst (setq s (strcat s sep (car lst)) lst (cdr lst)))
      s
    )
  )
)

(defun _csvq (s / txt)
  (setq txt (vl-princ-to-string (if s s "")))
  (setq txt (vl-string-subst "" "\r" txt))
  (setq txt (vl-string-subst "" "\n" txt))
  (if (or (vl-string-search "," txt)
          (vl-string-search "\"" txt)
          (vl-string-search " " txt))
    (strcat "\"" (vl-string-subst "\"\"" "\"" txt) "\"")
    txt
  )
)

(defun _csv-line (fields)
  (_join (mapcar '_csvq fields) ",")
)

;; -------- MText format stripper --------
(defun _mtx-strip (s / i n out ch nxt k depth c j inner)
  (setq s (if s s "") i 1 n (strlen s) out "")
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((= ch "{")
        (setq j (1+ i) depth 1)
        (while (and (<= j n) (> depth 0))
          (setq c (substr s j 1))
          (cond ((= c "{") (setq depth (1+ depth)))
                ((= c "}") (setq depth (1- depth))))
          (setq j (1+ j))
        )
        (setq inner (substr s (1+ i) (- j i 2)))
        (setq out (strcat out (_mtx-strip inner)))
        (setq i j)
      )
      ((= ch "\\")
        (setq nxt (substr s (1+ i) 1))
        (cond
          ((= nxt "{") (setq out (strcat out "{")) (setq i (+ i 2)))
          ((= nxt "}") (setq out (strcat out "}")) (setq i (+ i 2)))
          ((= nxt "\\") (setq out (strcat out "\\")) (setq i (+ i 2)))
          ((= nxt "~") (setq out (strcat out " ")) (setq i (+ i 2)))
          ((wcmatch (strcase nxt) "[ACFHLKOPQTWASP]")
            (setq k (+ i 1))
            (while (and (<= k n) (/= (substr s k 1) ";")) (setq k (1+ k)))
            (setq i (if (<= k n) (1+ k) (1+ i))))
          (t (setq out (strcat out ch)) (setq i (1+ i)))
        )
      )
      (t (setq out (strcat out ch)) (setq i (1+ i)))
    )
  )
  (_str-trim out)
)

(defun _plain (s) (_mtx-strip s))

(defun _to-num (s / cleaned)
  (if (or (null s) (= (_str-trim (vl-princ-to-string s)) ""))
    0.0
    (progn
      (setq cleaned (_str-trim (vl-princ-to-string s)))
      (setq cleaned (vl-string-subst "" "'" cleaned))
      (setq cleaned (vl-string-subst "" "," cleaned))
      (setq cleaned (vl-string-subst "" "'" cleaned))
      (atof cleaned)
    )
  )
)

(defun _format-val (val)
  (if (= val 0.0) "0.00" (rtos val 2 2))
)

;; -------- TABLE helpers --------
(defun _tables-in-layout (lay / blk out)
  (setq blk (vla-get-Block lay) out '())
  (vlax-for e blk
    (if (= (vla-get-ObjectName e) "AcDbTable") (setq out (cons e out))))
  (reverse out)
)

(defun _row-cells (tb r / cols c lst)
  (setq cols (vla-get-Columns tb) c 0 lst '())
  (while (< c cols)
    (setq lst (cons (_plain (vla-GetText tb r c)) lst))
    (setq c (1+ c)))
  (reverse lst)
)

(defun _all-empty (cells / empty idx)
  (setq empty T idx 0)
  (while (and empty (< idx (length cells)))
    (if (/= (_str-trim (nth idx cells)) "") (setq empty nil))
    (setq idx (1+ idx)))
  empty
)

(defun _nonempty-count (cells / cnt idx)
  (setq cnt 0 idx 0)
  (while (< idx (length cells))
    (if (/= (_str-trim (nth idx cells)) "") (setq cnt (1+ cnt)))
    (setq idx (1+ idx)))
  cnt
)

(defun _first-nonempty (cells / idx result)
  (setq idx 0 result "")
  (while (and (< idx (length cells)) (= result ""))
    (if (/= (_str-trim (nth idx cells)) "") (setq result (_str-trim (nth idx cells))))
    (setq idx (1+ idx)))
  result
)

(defun is_header_row (cells)
  (and (>= (length cells) 2)
       (or (wcmatch (strcase (_str-trim (nth 0 cells))) "*ITEM*")
           (wcmatch (strcase (_str-trim (nth 1 cells))) "*DESCRIPTION*")))
)

(defun is_section_row (cells / txt found)
  (setq txt (strcase (_first-nonempty cells)) found nil)
  (foreach sec *BOM_SECTIONS*
    (if (wcmatch txt (strcat "*" (strcase sec) "*")) (setq found T)))
  (and (<= (_nonempty-count cells) 2) found)
)

(defun is_title_row (cells / txt)
  (setq txt (strcase (_first-nonempty cells)))
  (or (wcmatch txt "*BILL OF MATERIALS*") (wcmatch txt "*COMPONENT BREAKDOWN*"))
)

(defun get_section_type (cells / txt)
  (setq txt (strcase (_first-nonempty cells)))
  (cond
    ((wcmatch txt "*NETWORK*") "NETWORK")
    ((wcmatch txt "*UNDERGROUND*") "UNDERGROUND")
    ((wcmatch txt "*ROUTE*") "ROUTE")
    ((wcmatch txt "*DEMAND*") "DEMAND")
    (t "OTHER"))
)

;; -------- Find value from data using match key --------
(defun find-value-by-key (dataList matchKey / found val entry acadItem)
  (setq found nil val 0.0)
  (foreach entry dataList
    (if (not found)
      (progn
        (setq acadItem (strcase (nth 0 entry)))
        ;; Check if AutoCAD item matches the key
        (cond
          ;; Exact match
          ((= acadItem (strcase matchKey))
            (setq found T val (nth 3 entry)))
          ;; AutoCAD item contains the key
          ((wcmatch acadItem (strcat "*" (strcase matchKey) "*"))
            (setq found T val (nth 3 entry)))
          ;; Key contains the AutoCAD item
          ((wcmatch (strcase matchKey) (strcat "*" acadItem "*"))
            (setq found T val (nth 3 entry)))
        )
      )
    )
  )
  val
)

;; Special finder for Route items (BORE vs TRENCH)
(defun find-route-value (dataList matchKey / found val entry acadDesc)
  (setq found nil val 0.0)
  (foreach entry dataList
    (if (not found)
      (progn
        (setq acadDesc (strcase (nth 1 entry)))
        ;; Check description for BORE or TRENCH
        (if (wcmatch acadDesc (strcat "*" (strcase matchKey) "*"))
          (setq found T val (nth 3 entry))
        )
      )
    )
  )
  val
)

;; -------- Export core --------
(defun _export-layout-to-csv (doc lay / tabs outdir oname path fhandle tb r rows cells 
                                      item desc uom val currentSection
                                      networkData undergroundData routeData demandData
                                      defItem defDesc defUom defKey)
  (setq tabs (_tables-in-layout lay))
  (setq outdir (getvar 'dwgprefix))
  (setq oname (substr (getvar 'dwgname) 1 (- (strlen (getvar 'dwgname)) 4)))
  (setq path (strcat outdir oname "_BOM.csv"))
  
  ;; Initialize data storage
  (setq networkData '() undergroundData '() routeData '() demandData '() currentSection nil)
  
  ;; ======== PASS 1: Collect all data from AutoCAD ========
  (princ "\n\n=== READING AUTOCAD DATA ===")
  (foreach tb tabs
    (setq r 0 rows (vla-get-Rows tb))
    (princ (strcat "\nTable: " (itoa rows) " rows x " (itoa (vla-get-Columns tb)) " columns"))
    
    (while (< r rows)
      (setq cells (_row-cells tb r))
      (cond
        ((_all-empty cells) nil)
        ((is_title_row cells) nil)
        ((is_header_row cells) nil)
        ((is_section_row cells)
          (setq currentSection (get_section_type cells))
          (princ (strcat "\n  Section: " currentSection)))
        (t
          (if (>= (length cells) 4)
            (progn
              (setq item (_str-trim (nth 0 cells)))
              (setq desc (_str-trim (nth 1 cells)))
              (setq uom  (_str-trim (nth 2 cells)))
              (setq val  (_to-num (nth 3 cells)))
              (if (/= item "")
                (progn
                  (princ (strcat "\n    [" item "] = " (rtos val 2 2)))
                  (cond
                    ((= currentSection "NETWORK")
                      (setq networkData (cons (list item desc uom val) networkData)))
                    ((= currentSection "UNDERGROUND")
                      (setq undergroundData (cons (list item desc uom val) undergroundData)))
                    ((= currentSection "ROUTE")
                      (setq routeData (cons (list item desc uom val) routeData)))
                    ((= currentSection "DEMAND")
                      (setq demandData (cons (list item desc uom val) demandData)))
                  )
                )
              )
            )
          )
        )
      )
      (setq r (1+ r))
    )
  )
  
  ;; Debug output
  (princ "\n\n=== COLLECTED DATA ===")
  (princ (strcat "\n  Network: " (itoa (length networkData)) " items"))
  (princ (strcat "\n  Underground: " (itoa (length undergroundData)) " items"))
  (princ (strcat "\n  Route: " (itoa (length routeData)) " items"))
  (princ (strcat "\n  Demand: " (itoa (length demandData)) " items"))
  
  ;; ======== PASS 2: Write CSV ========
  (princ "\n\n=== WRITING CSV ===")
  
  ;; Open file
  (setq fhandle (open path "w"))
  (if (null fhandle)
    (progn
      (princ (strcat "\n*** ERROR: Cannot create file: " path))
      (exit)
    )
  )
  
  ;; Write main header
  (write-line (_csvq *BOM_MAIN_HEADER*) fhandle)
  
  ;; ---- NETWORK ELEMENTS ----
  (princ "\n  Network Elements...")
  (write-line "" fhandle)
  (write-line (_csvq "Network Elements") fhandle)
  (write-line "Item,Description,UOM,UnitCost,Value,Cost,Actual Material" fhandle)
  (foreach defrow *DEFAULT_NETWORK*
    (setq defItem (nth 0 defrow))
    (setq defDesc (nth 1 defrow))
    (setq defUom  (nth 2 defrow))
    (setq defKey  (nth 3 defrow))
    (setq val (find-value-by-key networkData defKey))
    (write-line (_csv-line (list defItem defDesc defUom "0.00" (_format-val val) "0.00" (_format-val val))) fhandle)
    (princ (strcat "\n    " defItem " = " (_format-val val)))
  )
  
  ;; ---- UNDERGROUND INFRASTRUCTURE ----
  (princ "\n  Underground Infrastructure...")
  (write-line "" fhandle)
  (write-line (_csvq "Underground Infrastructure") fhandle)
  (write-line "Item,Description,UOM,UnitCost,Value,Cost,Actual Material" fhandle)
  (foreach defrow *DEFAULT_UNDERGROUND*
    (setq defItem (nth 0 defrow))
    (setq defDesc (nth 1 defrow))
    (setq defUom  (nth 2 defrow))
    (setq defKey  (nth 3 defrow))
    (setq val (find-value-by-key undergroundData defKey))
    (write-line (_csv-line (list defItem defDesc defUom "0.00" (_format-val val) "0.00" (_format-val val))) fhandle)
    (princ (strcat "\n    " defItem " = " (_format-val val)))
  )
  
  ;; ---- ROUTE SUMMARY ----
  (princ "\n  Route Summary...")
  (write-line "" fhandle)
  (write-line (_csvq "Route Summary") fhandle)
  (write-line "Item,Description,UOM,UnitCost,Value,Cost,Actual Material" fhandle)
  (foreach defrow *DEFAULT_ROUTE_SUMMARY*
    (setq defItem (nth 0 defrow))
    (setq defDesc (nth 1 defrow))
    (setq defUom  (nth 2 defrow))
    (setq defKey  (nth 3 defrow))
    (setq val (find-route-value routeData defKey))
    (write-line (_csv-line (list defItem defDesc defUom "0.00" (_format-val val) "0.00" (_format-val val))) fhandle)
    (princ (strcat "\n    " defDesc " = " (_format-val val)))
  )
  
  ;; ---- DEMAND ----
  (princ "\n  Demand...")
  (write-line "" fhandle)
  (write-line (_csvq "Demand") fhandle)
  (write-line "Item,Description,UOM,UnitCost,Value,Cost,Actual Material" fhandle)
  (foreach defrow *DEFAULT_DEMAND*
    (setq defItem (nth 0 defrow))
    (setq defDesc (nth 1 defrow))
    (setq defUom  (nth 2 defrow))
    (setq defKey  (nth 3 defrow))
    (setq val (find-value-by-key demandData defKey))
    (write-line (_csv-line (list defItem defDesc defUom "0.00" (_format-val val) "0.00" (_format-val val))) fhandle)
    (princ (strcat "\n    " defItem " = " (_format-val val)))
  )
  
  ;; Close file
  (close fhandle)
  (princ "\n\n=== DONE ===")
  
  path
)

;; -------- Layout finder --------
(defun _layout-by-name-like (doc nameLike / lays nm hit)
  (setq lays (vla-get-Layouts doc) hit nil)
  (vlax-for L lays
    (setq nm (vla-get-Name L))
    (if (wcmatch (strcase nm) (strcat "*" (strcase nameLike) "*")) (setq hit L)))
  hit
)

;; -------- Commands --------
(defun c:BOM2CSV ( / doc util arg lay path )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq util (vla-get-Utility doc))
  (setq arg (vla-GetString util 1 (strcat "\nLayout name [" *BOM_DEFAULT_LAYOUT* "]: ")))
  (if (or (null arg) (= (_str-trim arg) "")) (setq arg *BOM_DEFAULT_LAYOUT*))
  (setq lay (_layout-by-name-like doc arg))
  (cond
    ((null lay) (prompt (strcat "\nLayout not found: " arg)))
    (t
      (setq path (_export-layout-to-csv doc lay))
      (prompt (strcat "\n\n*** CSV CREATED: " path " ***\n"))
    )
  )
  (princ)
)

(defun c:BOM2CSV_NOW ( / doc lay path )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lay (_layout-by-name-like doc *BOM_DEFAULT_LAYOUT*))
  (cond
    ((null lay) (prompt (strcat "\nLayout not found: " *BOM_DEFAULT_LAYOUT*)))
    (t
      (setq path (_export-layout-to-csv doc lay))
      (prompt (strcat "\n\n*** CSV CREATED: " path " ***\n"))
    )
  )
  (princ)
)

(defun c:BOM_DEBUG ( / doc lay tabs tb r rows cells)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lay (_layout-by-name-like doc *BOM_DEFAULT_LAYOUT*))
  (if lay
    (progn
      (setq tabs (_tables-in-layout lay))
      (princ (strcat "\nFound " (itoa (length tabs)) " table(s) in layout"))
      (foreach tb tabs
        (setq rows (vla-get-Rows tb))
        (princ (strcat "\n\nTable: " (itoa rows) " rows x " (itoa (vla-get-Columns tb)) " columns"))
        (setq r 0)
        (while (< r (min rows 30))
          (setq cells (_row-cells tb r))
          (princ (strcat "\nRow " (itoa r) ": "))
          (foreach c cells (princ (strcat "[" c "] ")))
          (setq r (1+ r)))))
    (princ "\nLayout not found"))
  (princ)
)

(princ "\n")
(princ "\n============================================")
(princ "\n  BOM EXPORT - LOADED")
(princ "\n============================================")
(princ "\n  Commands:")
(princ "\n    BOM2CSV     - Export with layout prompt")
(princ "\n    BOM2CSV_NOW - Export default layout")
(princ "\n    BOM_DEBUG   - Debug table structure")
(princ "\n============================================")
(princ "\n")
(princ)