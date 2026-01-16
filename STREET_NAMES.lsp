(defun c:STREET_NAMES (/ ss i ent obj oldtxt newtxt count replace-list)
  ;; Define your find/replace pairs here
  ;; Format: ("FIND" "REPLACE")
  (setq replace-list '(
    ("STREET" "ST")
    ("DRIVE" "DR")
    ("AVENUE" "AVE")
    ("BOULEVARD" "BLVD")
    ("ROAD" "RD")
    ("LANE" "LN")
    ("COURT" "CT")
    ("PLACE" "PL")
    ("CIRCLE" "CIR")
    ("PARKWAY" "PKWY")
    ("TERRACE" "TER")
  ))
  
  (princ "\nMTEXT Find and Replace Multiple Strings...")
  (princ "\n=========================================")
  
  ;; Select all MTEXT objects in the drawing
  (setq ss (ssget "X" '((0 . "MTEXT"))))
  
  (if ss
    (progn
      (setq count 0)
      (setq i 0)
      
      ;; Loop through all MTEXT objects
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq oldtxt (vla-get-TextString obj))
        (setq newtxt oldtxt)
        (setq modified nil)
        
        ;; Apply all replacements to this MTEXT
        (foreach pair replace-list
          (setq find-str (car pair))
          (setq replace-str (cadr pair))
          
          ;; Case-insensitive replacement
          (while (vl-string-search (strcase find-str) (strcase newtxt))
            (setq pos (vl-string-search (strcase find-str) (strcase newtxt)))
            (setq newtxt 
              (strcat 
                (substr newtxt 1 pos)
                replace-str
                (substr newtxt (+ pos (strlen find-str) 1))
              )
            )
            (setq modified T)
          )
        )
        
        ;; Update the MTEXT if it was modified
        (if modified
          (progn
            (vla-put-TextString obj newtxt)
            (setq count (1+ count))
          )
        )
        
        (setq i (1+ i))
      )
      
      (princ (strcat "\n" (itoa count) " MTEXT object(s) modified."))
      (princ (strcat "\nTotal MTEXT objects processed: " (itoa (sslength ss))))
    )
    (princ "\nNo MTEXT objects found in the drawing.")
  )
  
  (princ)
)

(princ "\nType MTREPLACE to run the command.")
(princ)