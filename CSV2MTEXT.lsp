
;; CSV2MTEXT_Add.lsp — Fast CSV -> MTEXT (ActiveX) with wrap, style, height, bottom-left justify
;; Expects CSV columns: X,Y,CD_Details,Longitude_4326,Latitude_4326
;; CD_Details should contain '\P' paragraph breaks (from QGIS), but we normalize any \n, \r, \U+000A as well.

;; --- helpers -------------------------------------------------------------

(defun parsecsvq (s / i c inq buf out) ; quote-aware CSV split
  (setq i 0 inq nil buf "" out '())
  (while (< i (strlen s))
    (setq c (substr s (+ i 1) 1))
    (cond
      ((= c "\"") (setq inq (not inq)) (setq buf (strcat buf c)))
      ((and (not inq) (= c ",")) (setq out (append out (list buf))) (setq buf ""))
      (t (setq buf (strcat buf c)))
    )
    (setq i (1+ i))
  )
  (if (> (strlen buf) 0) (setq out (append out (list buf))))
  out
)

(defun trim (str) (vl-string-trim " \t" (if str str "")))

;; Ensure a text style exists and is set to a TrueType font with Bold/Italic flags.
;; Uses ActiveX TextStyle.SetFont(typeface, Bold, Italic, CharSet, PitchAndFamily).
;; CharSet = ANSI (0); PitchAndFamily = VARIABLE_PITCH(2) + FF_SWISS(32) + TMPF_TRUETYPE(4) = 38.
(defun ensure-ttf-style (doc styName typeface bold italic / styles st ok)
  (vl-load-com)
  (setq styles (vla-get-TextStyles doc))
  (setq ok (vl-catch-all-apply 'vla-item (list styles styName)))
  (setq st (if (vl-catch-all-error-p ok) (vla-add styles styName) ok))
  ;; Set typeface + bold/italic; (SetFont) is documented in ActiveX reference.
  ;;  ANSI_CHARSET = 0 , Pitch+Family = 38 (VARIABLE_PITCH + FF_SWISS + TMPF_TRUETYPE)
  (vla-SetFont st typeface (if bold :vlax-true :vlax-false) (if italic :vlax-true :vlax-false) 0 38)
  styName
)

;; --- main ---------------------------------------------------------------

(defun c:CSV2MTEXT_Add (/ fpath fh line parts x y txt n width ht sty lay doc ms mt oldvars apBL)

  (vl-load-com)

  ;; >>> Tune these defaults <<<
  (setq width  30.0)   ; MTEXT wrap box width (drawing units) — adjust to match your desired box width
  (setq ht      4.0)   ; MTEXT text height
  (setq sty    "PDF Arial Bold") ; text style name you requested
  (setq lay    "CD_Details")     ; target layer for the labels

  ;; save & minimize visual overhead to speed up
  (setq oldvars
    (list
      (cons "OSMODE"           (getvar "OSMODE"))
      (cons "CMDECHO"          (getvar "CMDECHO"))
      (cons "SELECTIONPREVIEW" (getvar "SELECTIONPREVIEW"))
      (cons "HIGHLIGHT"        (getvar "HIGHLIGHT"))
      (cons "TEXTFILL"         (getvar "TEXTFILL"))
    )
  )
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "SELECTIONPREVIEW" 0)
  (setvar "HIGHLIGHT" 0)
  (setvar "TEXTFILL" 0)

  ;; ActiveX objects
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (vla-put-ActiveSpace doc acModelSpace)
  (setq ms (vla-get-ModelSpace doc))

  ;; ensure layer
  (if (not (tblsearch "layer" lay))
    (command "_.-LAYER" "_Make" lay "_Color" "7" "" "")
    (command "_.-LAYER" "_Make" lay "")
  )

  ;; ensure style = "PDF Arial Bold" with Arial bold
  (ensure-ttf-style doc sty "Arial" T nil)  ;; Bold=True, Italic=False  [1](https://help.autodesk.com/cloudhelp/2018/ENU/AutoCAD-ActiveX-Reference/files/GUID-DB668114-2395-43C6-858C-2F2514C4BF46.htm)[6](https://documentation.help/AutoCAD-ActiveX-AAG/WS1a9193826455f5ff1a32d8d10ebc6b7ccc-6b97.htm)

  ;; numeric fallback for Bottom-Left if symbol is not bound
  (setq apBL (if (boundp 'acAttachmentPointBottomLeft) acAttachmentPointBottomLeft 7))  ;; 7 per enumeration  [5](https://help.autodesk.com/cloudhelp/2024/ENU/AutoCAD-LT-ActiveX-Reference/files/GUID-FD7EDA56-7FA0-4616-A746-9B97AE0C6456.htm)

  ;; pick CSV
  (setq fpath (getfiled "Select CD_Details.csv (X,Y,CD_Details,...)" "" "csv" 0))
  (if (and fpath (setq fh (open fpath "r")))
    (progn
      ;; skip header if present
      (setq line (read-line fh))
      (if line
        (progn
          (setq parts (parsecsvq line))
          (if (and parts (> (length parts) 0))
            (if (not (numberp (read (trim (car parts))))) ; header?
              (setq line (read-line fh))
            )
          )
        )
      )

      (setq n 0)
      (while (setq line (read-line fh))
        (setq parts (parsecsvq line))
        (if (>= (length parts) 3)
          (progn
            (setq x   (atof (trim (nth 0 parts))))
            (setq y   (atof (trim (nth 1 parts))))
            (setq txt (trim (nth 2 parts)))

            ;; normalize any breaks -> MTEXT paragraph breaks
            (setq txt (vl-string-subst "\\P" "\\U+000A" txt)) ; unicode line-break to paragraph  [7](https://opengislab.com/blog/2022/6/16/georeferencing-cad-dxf-with-qgis)
            (setq txt (vl-string-subst "\\P" "\r\n"     txt))
            (setq txt (vl-string-subst "\\P" "\n"       txt))
            (setq txt (vl-string-subst "\\P" "\r"       txt))

            (if (and x y)
              (progn
                ;; create MTEXT using ActiveX (fast)
                ;; AddMText takes insertion point + width; width enables word-wrap  [8](https://documentation.help/AutoCAD-ActiveX-AAG/WS1a9193826455f5ff1a32d8d10ebc6b7ccc-6b84.htm)
                (setq mt (vla-AddMText ms (vlax-3d-point (list x y 0.0)) width txt))

                ;; set style, height, layer, wrap width (explicit), and bottom-left attachment
                (vla-put-StyleName mt sty)                ;; apply our “PDF Arial Bold” style  [9](https://documentation.help/AutoCAD-ActiveX-AAG/WS1a9193826455f5ff1a32d8d10ebc6b7ccc-6b82.htm)
                (vla-put-Height    mt ht)                 ;; height = 4.0
                (vla-put-Layer     mt lay)
                (vla-put-Width     mt width)              ;; wrap boundary width  [3](https://help.autodesk.com/cloudhelp/2022/ENU/AutoCAD-ActiveX-Reference/files/GUID-023FE356-8A2C-4E3F-ADFB-85BCFCCCA6EA.htm)[4](https://help.autodesk.com/cloudhelp/2024/ENU/OARX-ManagedRefGuide/files/OARX-ManagedRefGuide-Autodesk_AutoCAD_DatabaseServices_MText_Width.html)
                (vla-put-AttachmentPoint mt apBL)         ;; bottom-left justify/spill up  [5](https://help.autodesk.com/cloudhelp/2024/ENU/AutoCAD-LT-ActiveX-Reference/files/GUID-FD7EDA56-7FA0-4616-A746-9B97AE0C6456.htm)

                (setq n (1+ n))
                (if (= (rem n 500) 0) (prompt (strcat "\nPlaced " (itoa n) " MTEXTs...")))
              )
            )
          )
        )
      )
      (close fh)
      (prompt (strcat "\nCSV2MTEXT_Add: Placed " (itoa n) " MTEXT(s)."))
    )
    (prompt "\nCSV2MTEXT_Add: Could not open CSV file.")
  )

  ;; zoom extents for visibility
  (command "_.ZOOM" "_E")

  ;; restore sysvars
  (foreach pair oldvars (setvar (car pair) (cdr pair)))

  (vla-endundomark doc)
  (princ)
)
