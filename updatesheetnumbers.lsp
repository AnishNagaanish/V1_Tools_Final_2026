(defun c:updatesheetnumbers
  (/ acadApp acadDoc layouts layoutList layoutCount sheetNum updatedLayouts
     blkTblRec blockFound attributesUpdated attVar attSA attList
     entity tag effName
     blockName tagSheet tagTotal)

  (vl-load-com)

  ;; --- CONFIG ---
  (setq blockName "TB-PLAN-TITLE_FULL") ; change if needed
  (setq tagSheet  "SHEET_NO")
  (setq tagTotal  "NO_OF_SHEETS")
  ;; --------------

  (defun _pad2 (n) (if (< n 10) (strcat "0" (itoa n)) (itoa n)))

  ;; Get app + doc
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq layouts (vla-get-Layouts acadDoc))

  ;; Build list of (layoutObj . tabOrder), skipping Model
  (setq layoutList '())
  (vlax-for lo layouts
    (if (/= (strcase (vla-get-Name lo)) "MODEL")
      (setq layoutList (cons (cons lo (vla-get-TabOrder lo)) layoutList))
    )
  )

  ;; Sort by tab order
  (setq layoutList (vl-sort layoutList (function (lambda (a b) (< (cdr a) (cdr b))))))

  ;; Count layouts
  (setq layoutCount (length layoutList))
  (setq sheetNum 1
        updatedLayouts 0)

  ;; Process in tab order
  (foreach pair layoutList
    (setq layout (car pair))
    (vla-put-ActiveLayout acadDoc layout)
    (setq blkTblRec (vla-get-Block layout))
    (setq blockFound nil
          attributesUpdated nil)

    ;; Search for title block
    (vlax-for entity blkTblRec
      (if (= (vla-get-ObjectName entity) "AcDbBlockReference")
        (progn
          (setq effName (strcase (vla-get-EffectiveName entity)))
          (if (= effName (strcase blockName))
            (progn
              (setq blockFound T)
              (if (and (vlax-property-available-p entity 'HasAttributes)
                       (vla-get-HasAttributes entity))
                (progn
                  (setq attVar (vla-GetAttributes entity))
                  (setq attSA  (vlax-variant-value attVar))
                  (setq attList (vlax-safearray->list attSA))
                  (foreach att attList
                    (setq tag (strcase (vla-get-TagString att)))
                    (cond
                      ((= tag (strcase tagSheet))
                       (vla-put-TextString att (_pad2 sheetNum))
                       (setq attributesUpdated T))
                      ((= tag (strcase tagTotal))
                       (vla-put-TextString att (itoa layoutCount))
                       (setq attributesUpdated T))
                    )
                  )
                  (vla-Update entity)
                )
              )
            )
          )
        )
      )
    )

    (if (and blockFound attributesUpdated)
      (progn
        (setq updatedLayouts (1+ updatedLayouts))
        (princ (strcat "\nUpdated layout: " (vla-get-Name layout)))
      )
      (princ (strcat "\nWarning: No '" blockName "' block or attributes found in layout '" (vla-get-Name layout) "'."))
    )

    (setq sheetNum (1+ sheetNum))
  )

  ;; Final feedback
  (if (> layoutCount 0)
    (princ (strcat "\nProcessed " (itoa layoutCount) " layout(s). Updated "
                   (itoa updatedLayouts) " title block(s)."))
    (princ "\nNo layouts found to process (excluding Model).")
  )

  (princ)
)
