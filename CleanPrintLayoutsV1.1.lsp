
;;; ------------------------------------------------------------
;;; CleanPrints.lsp
;;; Robust cleanup of *PRINT* layouts in Paper Space:
;;; - Deletes TEXT/MTEXT/ATTRIB that contain target phrases
;;; - Deletes AcDbTable tables if any cell matches target phrases
;;; - Deletes polylines on layer 'Bore'
;;; - Handles locked layers (unlock/restore)
;;; - Preview and Delete modes
;;; Author: Ankam Nagaanish
;;; ------------------------------------------------------------

(vl-load-com)

;; ------------------ SETTINGS ------------------
(setq *layoutMask* "*PRINT*") ; match any layout containing PRINT (case-insensitive)

(setq *textKillList*
  '("dig 4'x4' pit"
    "bore & place"
    "sta."
    "s12f"
    "gps"
    "hex-1"
    "hex-2"
    "hex-12"
    "hex-24"
    "new swing arm"
    "pl. new"
    "toby box")
)

(setq *tableKillList*
  '("fiber core unit (fcu)" "method" "METHOD"
    "excavation data")
)
;; ----------------------------------------------

(defun _str-lower (s) (if s (strcase s T) ""))

(defun _normalize-mtext (s)
  (if s
    (vl-string-trim " 	
"
      (vl-string-subst " "
        "\P"
        (vl-string-subst " "
          "\p"
          (vl-string-subst " "
            "
"
            (vl-string-subst " "
              "
"
              (vl-string-subst "" "{" (vl-string-subst "" "}" s))
            )
          )
        )
      )
    )
    ""
  )
)

(defun _contains-any (s lst / found)
  (if (and s lst)
    (progn
      (setq found nil)
      (while (and lst (not found))
        (if (vl-string-search (car lst) s)
          (setq found T)
        )
        (setq lst (cdr lst))
      )
      found
    )
    nil
  )
)

(defun _safe-get-textstring (vlaObj / onm)
  (setq onm (vlax-get vlaObj 'ObjectName))
  (cond
    ((wcmatch onm "*MText*") (_str-lower (_normalize-mtext (vlax-get vlaObj 'TextString))))
    ((wcmatch onm "*Text*")  (_str-lower (_normalize-mtext (vlax-get vlaObj 'TextString))))
    ((wcmatch onm "*Attribute*") (_str-lower (_normalize-mtext (vlax-get vlaObj 'TextString))))
    (T ""))
)

(defun _table-has-keywords-safe (vlaTable kw / rows cols r c txt got res)
  (if (not (wcmatch (vlax-get vlaTable 'ObjectName) "*AcDbTable*"))
    nil
    (progn
      (setq rows (vla-get-Rows vlaTable))
      (setq cols (vla-get-Columns vlaTable))
      (setq r 0 got nil)
      (while (and (< r rows) (not got))
        (setq c 0)
        (while (and (< c cols) (not got))
          (setq res (vl-catch-all-apply 'vla-GetText (list vlaTable r c)))
          (if (not (vl-catch-all-error-p res))
            (progn
              (setq txt (_str-lower (_normalize-mtext res)))
              (if (_contains-any txt kw) (setq got T))
            )
          )
          (setq c (1+ c))
        )
        (setq r (1+ r))
      )
      got
    )
  )
)

(defun _layer-ensure-unlocked (doc layName locks / layers layObj st)
  (setq layers (vla-get-Layers doc))
  (setq layObj (vl-catch-all-apply 'vla-Item (list layers layName)))
  (if (vl-catch-all-error-p layObj)
    locks
    (progn
      (if (= (vla-get-Lock layObj) :vlax-true)
        (progn
          (setq st (cons (cons layName T) locks))
          (vla-put-Lock layObj :vlax-false)
          st
        )
        (setq st (cons (cons layName nil) locks))
      )
    )
  )
)

(defun _layers-restore (doc locks / layers it lo)
  (setq layers (vla-get-Layers doc))
  (foreach it locks
    (setq lo (vl-catch-all-apply 'vla-Item (list layers (car it))))
    (if (not (vl-catch-all-error-p lo))
      (vla-put-Lock lo (if (cdr it) :vlax-true :vlax-false))
    )
  )
)

(defun _delete-entity-safe (doc vlaObj locks / layName)
  (setq layName (vlax-get vlaObj 'Layer))
  (setq locks (_layer-ensure-unlocked doc layName locks))
  (vl-catch-all-apply 'vla-Delete (list vlaObj))
  locks
)

(defun _scan-one-layout (doc vlaLayout doDelete / blk oname nText nTab kills locks)
  (setq nText 0 nTab 0 kills 0 locks '())
  (setq blk (vlax-get vlaLayout 'Block))
  (vlax-for ent blk
    (setq oname (vlax-get ent 'ObjectName))
    (cond
      ((or (wcmatch oname "*MText*")
           (wcmatch oname "*Text*")
           (wcmatch oname "*Attribute*"))
       (if (_contains-any (_safe-get-textstring ent) *textKillList*)
         (progn
           (setq nText (1+ nText))
           (if doDelete (setq locks (_delete-entity-safe doc ent locks)))
         )
       )
      )
      ((wcmatch oname "*Table*")
       (if (_table-has-keywords-safe ent *tableKillList*)
         (progn
           (setq nTab (1+ nTab))
           (if doDelete (setq locks (_delete-entity-safe doc ent locks)))
         )
       )
      )
      ((and (wcmatch oname "*Polyline*")
            (eq (strcase (vlax-get ent 'Layer)) "BORE"))
       (progn
         (setq nText (1+ nText))
         (if doDelete (setq locks (_delete-entity-safe doc ent locks)))
       )
      )
    )
  )
  (princ
    (strcat
      "
  " (vla-get-Name vlaLayout)
      " : texts=" (itoa nText)
      ", tables=" (itoa nTab)
      (if doDelete "" " (preview only)")
    )
  )
  (_layers-restore doc locks)
  (list nText nTab)
)

(defun c:CLEANPRINTS (/ acad doc lays lay loName doDelete mode totalT totalTb res)
  (vl-load-com)
  (setq acad (vlax-get-acad-object))
  (setq doc  (vla-get-ActiveDocument acad))
  (setq lays (vla-get-Layouts doc))
  (setq totalT 0 totalTb 0)

  (initget "Preview Delete")
  (setq mode (getkword "
Mode [Preview/Delete] <Preview>: "))
  (if (= mode "Delete") (setq doDelete T) (setq doDelete nil))

  (princ
    (strcat
      "
--- Clean '*PRINT*' Layouts ---"
      "
Mask: " *layoutMask*
      (if doDelete "
Mode: DELETE" "
Mode: PREVIEW (no deletion)")
      "
Text keys: " (vl-princ-to-string *textKillList*)
      "
Table keys: " (vl-princ-to-string *tableKillList*)
      "
--------------------------------"
    )
  )

  (vla-StartUndoMark doc)
  (vlax-for lay lays
    (setq loName (vla-get-Name lay))
    (if (and (not (eq (strcase loName) "MODEL"))
             (wcmatch (strcase loName) (strcase *layoutMask*)))
      (progn
        (setq res (_scan-one-layout doc lay doDelete))
        (setq totalT (+ totalT (car res)))
        (setq totalTb (+ totalTb (cadr res)))
      )
    )
  )
  (vla-EndUndoMark doc)

  (princ
    (strcat
      "
--------------------------------"
      "
TOTAL matches -> texts=" (itoa totalT)
      ", tables=" (itoa totalTb)
      (if doDelete "
Deletion complete." "
No entities were deleted (preview).")
      "
--------------------------------
"
    )
  )
  (princ)
)

(princ "
Type CLEANPRINTS to run. ")
(princ)
