(defun c:BoreLengths (/ boreList totalLen polyEnt polyObj len boreVal csvFile file grandTotal userInput)
  (setq boreList '() totalLen 0)
  (setq csvFile (strcat (getenv "USERPROFILE") "\\Downloads\\BoreLengths.csv"))

  (prompt "
Select polylines. Type X to finish.
")

  (while (progn
           (setq polyEnt (entsel "
Select polyline or type X to finish: "))
           (if (and polyEnt (/= (car polyEnt) "X"))
             T
             nil
           )
         )
    (setq polyObj (vlax-ename->vla-object (car polyEnt)))
    (setq len (vla-get-length polyObj))
    ;; Custom rounding
    (setq boreVal (fix len))
    (if (> (- len boreVal) 0.5)
      (setq boreVal (1+ boreVal))
    )
    ;; Ask user to override
    (setq userInput (getstring (strcat "Calculated Bore Length: " (itoa boreVal) " | Enter new value or press Enter: ")))
    (if (/= userInput "")
      (setq boreVal (atoi userInput))
    )
    ;; Update total and list
    (setq totalLen (+ totalLen boreVal))
    (setq boreList (append boreList (list (list boreVal totalLen))))
    (prompt (strcat "
Added Bore Length: " (itoa boreVal) " | Running Total: " (itoa totalLen) "
"))
  )

  ;; Export CSV
  (setq grandTotal totalLen)
  (setq file (open csvFile "w"))
  (write-line "Total Bore Length,Bore Length,Bore_len_with_previous" file)
  (foreach item boreList
    (write-line (strcat (itoa grandTotal) "," (itoa (car item)) "," (itoa (cadr item))) file)
  )
  (close file)
  (prompt (strcat "
Export complete! Total Bore Length: " (itoa grandTotal) "
File saved at: " csvFile "
"))
  (princ)
)