(defun c:UpdateLength ()
  (setq lineObj (entsel "\nSelect Polyline: ")) ;; Select polyline first
  (setq textObj (entsel "\nSelect MText: ")) ;; Select MText next

  (if (and lineObj textObj)
    (progn
      (setq lineEnt (vlax-ename->vla-object (car lineObj))) ;; Convert selection to VLA object
      (setq textEnt (car textObj))

      ;; Get Polyline length and round it up
      (setq lineLength (vla-get-Length lineEnt))
      (setq roundedLength (fix (+ lineLength 0.5))) ;; Round to nearest whole number
      (setq lineLengthStr (strcat  (rtos roundedLength 2 0) "'")) ;; Corrected format with single brackets


      ;; Get the current MText value
      (setq textData (entget textEnt))
      (setq textString (cdr (assoc 1 textData)))

      ;; Find existing length inside brackets and replace it correctly
      (setq openBracket (vl-string-search "(" textString))
      (setq closeBracket (vl-string-search ")" textString))

      (if (and openBracket closeBracket)
        (progn
          ;; Extract old length inside brackets
          (setq oldLength (substr textString (+ openBracket 2) (- closeBracket openBracket 1)))

          ;; Replace old length with new rounded polyline length
          (setq updatedText (vl-string-subst lineLengthStr oldLength textString))
        )
        (setq updatedText textString) ;; No change if brackets don't exist
      )

      ;; Apply changes to MText
      (setq newTextData (subst (cons 1 updatedText) (assoc 1 textData) textData))
      (entmod newTextData)
      (princ "\nMText updated successfully!")
    )
    (princ "\nInvalid selection. Please select a POLYLINE first, then MText.")
  )
  (princ)
)
