(defun c:DSSCLR ()
  ;; Get two points for the aligned dimension
  (setq pt1 (getpoint "\nSelect first point: "))
  (setq pt2 (getpoint "\nSelect second point: "))
  
  ;; Create the aligned dimension
  (command "_DIMALIGNED" pt1 pt2 pause) ; pause for dimension line location

  ;; Get the last created entity (the dimension)
  (setq dimObj (entlast))

  ;; Prompt for the number input
  (initget 0)
  (setq userVal (getstring "\nEnter dimension value (default is 5): "))
  (if (= userVal "") (setq userVal "5"))

  ;; Build the custom dimension text
  (setq dimText (strcat userVal "' SS CLR"))

  ;; Modify the dimension entity directly
  (setq dimData (entget dimObj))
  (setq dimData (subst (cons 1 dimText) (assoc 1 dimData) dimData)) ; Replace text
  (entmod dimData)
  (entupd dimObj) ; Update the entity

  ;; Feedback
  (princ (strcat "\nAligned dimension with '" dimText "' text added."))
  (princ)
)
