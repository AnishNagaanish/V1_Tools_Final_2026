;; Helper function for zero-padding
(defun _pad2 (n)
  (if (< n 10)
    (strcat "0" (itoa n))
    (itoa n)
  )
)

(defun c:updatesheetnumbers
  (/ acadApp acadDoc layouts layoutList layoutCount sheetNum updatedLayouts
     blkTblRec blockFound attributesUpdated attVar attSA attList
     entity tag effName layout lo pair att
     blockNames tagSheet tagTotal)

  (vl-load-com)

  ;; --- CONFIG (MULTIPLE BLOCK NAMES) ---
  (setq blockNames (list
    "TF-TITLE BLOCK"
    "TB-PLAN-TITLE_FULL"
    ;; Add more block names here if needed
  ))
  (setq tagSheet  "SHEET_NO")
  (setq tagTotal  "NO_OF_SHEETS")
  ;; -------------------------------------

  ;; Error handler
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; Get app + doc
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq layouts (vla-get-Layouts acadDoc))

  ;; Build list: (layoutObj tabOrder layoutName)
  (setq layoutList '())
  (vlax-for lo layouts
    (if (/= (strcase (vla-get-Name lo)) "MODEL")
      (setq layoutList
        (cons
          (list lo (vla-get-TabOrder lo) (vla-get-Name lo))
          layoutList
        )
      )
    )
  )

  ;; Sort by Tab Order (left to right as shown in AutoCAD)
  (setq layoutList
    (vl-sort layoutList
      (function (lambda (a b) (< (cadr a) (cadr b))))
    )
  )

  ;; Count layouts
  (setq layoutCount (length layoutList))
  (setq sheetNum 1)
  (setq updatedLayouts 0)

  (if (= layoutCount 0)
    (progn
      (princ "\nNo layouts found to process (excluding Model).")
      (exit)
    )
  )

  ;; Show info
  (princ "\n\nLooking for these title blocks:")
  (foreach bn blockNames
    (princ (strcat "\n  - " bn))
  )

  ;; Show preview
  (princ (strcat "\n\nFound " (itoa layoutCount) " layouts. Order:"))
  (setq sheetNum 1)
  (foreach pair layoutList
    (princ (strcat "\n  Sheet " (_pad2 sheetNum) " -> " (caddr pair)))
    (setq sheetNum (1+ sheetNum))
  )

  ;; Reset and process
  (setq sheetNum 1)

  (princ "\n\nUpdating...")

  ;; Process in tab order
  (foreach pair layoutList
    (setq layout (car pair))
    (vla-put-ActiveLayout acadDoc layout)
    (setq blkTblRec (vla-get-Block layout))
    (setq blockFound nil)
    (setq attributesUpdated nil)

    ;; Search for title block
    (vlax-for entity blkTblRec
      (if (and
            (= (vla-get-ObjectName entity) "AcDbBlockReference")
            (not blockFound)  ; stop after first match
          )
        (progn
          ;; Get block name (handle dynamic blocks too)
          (setq effName
            (vl-catch-all-apply 'vla-get-EffectiveName (list entity))
          )
          (if (vl-catch-all-error-p effName)
            (setq effName (vla-get-Name entity))
          )
          
          ;; Check if this block matches ANY of our title block names
          (if (member (strcase effName) (mapcar 'strcase blockNames))
            (progn
              (setq blockFound T)
              (if (and
                    (vlax-property-available-p entity 'HasAttributes)
                    (= (vla-get-HasAttributes entity) :vlax-true)
                  )
                (progn
                  (setq attVar (vla-GetAttributes entity))
                  (setq attSA (vlax-variant-value attVar))
                  (setq attList (vlax-safearray->list attSA))
                  
                  (foreach att attList
                    (setq tag (strcase (vla-get-TagString att)))
                    (cond
                      ((= tag (strcase tagSheet))
                       (vla-put-TextString att (_pad2 sheetNum))
                       (setq attributesUpdated T)
                      )
                      ((= tag (strcase tagTotal))
                       (vla-put-TextString att (itoa layoutCount))
                       (setq attributesUpdated T)
                      )
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
        (princ (strcat "\n  " (caddr pair) " -> " (_pad2 sheetNum) " of " (itoa layoutCount)))
      )
      (princ (strcat "\n  WARNING: No title block found in '" (caddr pair) "'"))
    )

    (setq sheetNum (1+ sheetNum))
  )

  ;; Final feedback
  (princ (strcat "\n\n=== COMPLETE ==="))
  (princ (strcat "\nUpdated " (itoa updatedLayouts) " of " (itoa layoutCount) " layouts."))
  (princ "\n\nRun REGENALL to refresh the display.")
  (princ)
)

(princ "\nType UPDATESHEETNUMBERS to run.")
(princ)