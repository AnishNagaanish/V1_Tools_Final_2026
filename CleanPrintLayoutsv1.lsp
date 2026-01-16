
;;; ------------------------------------------------------------
;;; CleanPrints_Combined.lsp
;;; Combined cleanup for *PRINT* layouts:
;;; - Deletes TEXT/MTEXT/ATTRIB matching keywords
;;; - Deletes AcDbTable tables if any cell matches keywords
;;; - Deletes polylines on layer 'BORE'
;;; - Deletes all entities on layer 'BORE NOTES'
;;; - Deletes block references named "CONSTRUCTION NOTES" or "CONSTRUCTION NOTES 2"
;;; Handles locked layers and supports Preview/Delete modes
;;; Author: Ankam Nagaanish
;;; ------------------------------------------------------------

(vl-load-com)

;; ------------------ SETTINGS ------------------
(setq *layoutMask* "*PRINT*") ; match layouts containing PRINT
(setq *textKillList*
  '("dig 4'x4' pit"
    "bore &amp; place"
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
  '("*fiber core unit*"
    "*excavation data*"
    "*overall path placement*") ; Added wildcard support
)
(setq *targetLayer* "maplines_bore") ; layer to delete
(setq *blockKillList* (list "CONSTRUCTION NOTES" "CONSTRUCTION NOTES 2")) ; block names to delete
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
        (if (wcmatch s (car lst)) (setq found T))
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
          (if (and (numberp r) (numberp c))
            (progn
              (setq res (vl-catch-all-apply 'vla-GetText (list vlaTable r c)))
              (if (not (vl-catch-all-error-p res))
                (progn
                  (setq txt (_str-lower (_normalize-mtext res)))
                  (if (_contains-any txt kw) (setq got T))
                )
              )
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

(defun _delete-bore-notes-entities (doc blk doDelete / ent layName locks count)
  (setq locks '() count 0)
  (vlax-for ent blk
    (setq layName (_str-lower (vlax-get ent 'Layer)))
    (if (= layName (_str-lower *targetLayer*))
      (progn
        (setq count (1+ count))
        (if doDelete (setq locks (_delete-entity-safe doc ent locks)))
      )
    )
  )
  (_layers-restore doc locks)
  count
)

(defun _delete-blocks-by-name (doc blk doDelete / ent oname blkName locks count)
  (setq locks '() count 0)
  (vlax-for ent blk
    (setq oname (vlax-get ent 'ObjectName))
    (if (wcmatch oname "*BlockReference*")
      (progn
        (setq blkName (_str-lower (vlax-get ent 'EffectiveName)))
        (if (member blkName (mapcar '_str-lower *blockKillList*))
          (progn
            (setq count (1+ count))
            (if doDelete (setq locks (_delete-entity-safe doc ent locks)))
          )
        )
      )
    )
  )
  (_layers-restore doc locks)
  count
)

(defun _scan-one-layout (doc vlaLayout doDelete / blk oname nText nTab nBoreNotes nBlocks locks)
  (setq nText 0 nTab 0 nBoreNotes 0 nBlocks 0 locks '())
  (setq blk (vlax-get vlaLayout 'Block))
  ;; Text and Table cleanup
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
  ;; Additional cleanup
  (setq nBoreNotes (_delete-bore-notes-entities doc blk doDelete))
  (setq nBlocks (_delete-blocks-by-name doc blk doDelete))

  (princ
    (strcat
      "\n  " (vla-get-Name vlaLayout)
      " : texts=" (itoa nText)
      ", tables=" (itoa nTab)
      ", bore notes=" (itoa nBoreNotes)
      ", blocks=" (itoa nBlocks)
      (if doDelete "" " (preview only)")
    )
  )
  (_layers-restore doc locks)
  (list nText nTab nBoreNotes nBlocks)
)

(defun c:CLEANPRINTS_COMBINED (/ acad doc lays lay loName doDelete mode totalT totalTb totalBN totalBlk res)
  (vl-load-com)
  (setq acad (vlax-get-acad-object))
  (setq doc  (vla-get-ActiveDocument acad))
  (setq lays (vla-get-Layouts doc))
  (setq totalT 0 totalTb 0 totalBN 0 totalBlk 0)

  (initget "Preview Delete")
  (setq mode (getkword "\nMode [Preview/Delete] <Preview>: "))
  (if (= mode "Delete") (setq doDelete T) (setq doDelete nil))

  (princ
    (strcat
      "\n--- Clean '*PRINT*' Layouts ---"
      "\nText keys: " (vl-princ-to-string *textKillList*)
      "\nTable keys: " (vl-princ-to-string *tableKillList*)
      "\nTarget Layer: " *targetLayer*
      "\nBlock Names: " (vl-princ-to-string *blockKillList*)
      (if doDelete "\nMode: DELETE" "\nMode: PREVIEW (no deletion)")
      "\n--------------------------------"
    )
  )

  (vla-StartUndoMark doc)
  (vlax-for lay lays
    (setq loName (vla-get-Name lay))
    (if (and (not (eq (strcase loName) "MODEL"))
             (wcmatch (strcase loName) (strcase *layoutMask*)))
      (progn
        (setq res (_scan-one-layout doc lay doDelete))
        (setq totalT (+ totalT (nth 0 res)))
        (setq totalTb (+ totalTb (nth 1 res)))
        (setq totalBN (+ totalBN (nth 2 res)))
        (setq totalBlk (+ totalBlk (nth 3 res)))
      )
    )
  )
  (vla-EndUndoMark doc)

  (princ
    (strcat
      "\n--------------------------------"
      "\nTOTAL matches -> texts=" (itoa totalT)
      ", tables=" (itoa totalTb)
      ", bore notes=" (itoa totalBN)
      ", blocks=" (itoa totalBlk)
      (if doDelete "\nDeletion complete." "\nNo entities were deleted (preview).")
      "\n--------------------------------\n"
    )
  )
  (princ)
)

(princ "\nType CLEANPRINTS_COMBINED to run. ")
(princ)
