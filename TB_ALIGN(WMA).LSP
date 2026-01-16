(defun c:WMA (/ ss i blk pt rowEnt rowObj nearest dist count skipped old-cmdecho 
                   old-osmode offset-dist min-dist max-dist user-input target-pt direction-vec
                   mode-input vec-length param tangent)
  
  ;; ========== USER SETTINGS - CAN BE CHANGED ==========
  (setq default-offset 2.0)          ; Default offset distance in feet
  (setq min-search-dist 0.0)         ; Minimum search distance (0 = can be ON the ROW)
  (setq max-search-dist 25.0)        ; Maximum search distance to look for blocks
  ;; ====================================================
  
  ;; Store current settings
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  (princ "\nSnap Toby Box to Right Of Way...")
  (princ "\n=====================================")
  
  ;; Ask user for mode: Auto or Manual
  (initget "Auto Manual A M")
  (setq mode-input (getkword "\nSelect mode [Auto/Manual] <Auto>: "))
  
  (if (not mode-input)
    (setq mode-input "Auto")
  )
  
  ;; Normalize input
  (if (or (= mode-input "A") (= mode-input "a"))
    (setq mode-input "Auto")
  )
  (if (or (= mode-input "M") (= mode-input "m"))
    (setq mode-input "Manual")
  )
  
  ;; Prompt user to select the Right Of Way polyline
  (setq rowEnt (entsel "\nSelect Right Of Way polyline: "))
  
  (if (and rowEnt (setq rowObj (vlax-ename->vla-object (car rowEnt))))
    (progn
      (princ "\nRight Of Way polyline selected.")
      
      ;; Ask user for offset distance
      (setq user-input (getdist (strcat "\nEnter offset distance from Right Of Way <" 
                                       (rtos default-offset 2 2) ">: ")))
      
      (if (not user-input)
        (setq offset-dist default-offset)
        (setq offset-dist user-input)
      )
      
      (princ (strcat "\nOffset distance set to: " (rtos offset-dist 2 2) " feet"))
      
      ;; Select Toby Box blocks based on mode
      (if (= mode-input "Auto")
        (progn
          (princ "\nAuto mode: Processing all 'Toby Box' blocks...")
          (setq ss (ssget "X" '((0 . "INSERT") (2 . "Toby Box"))))
        )
        (progn
          (princ "\nManual mode: Select Toby Box blocks...")
          (setq ss (ssget '((0 . "INSERT") (2 . "Toby Box"))))
        )
      )
      
      (if ss
        (progn
          (princ (strcat "\nFound " (itoa (sslength ss)) " Toby Box blocks"))
          (princ "\nProcessing...")
          
          (setq i 0)
          (setq count 0)
          (setq skipped 0)
          
          (while (< i (sslength ss))
            (setq blk (ssname ss i))
            (setq pt (cdr (assoc 10 (entget blk)))) ; Get insertion point
            
            ;; Get closest point on ROW polyline using vlax-curve function
            (setq nearest (vlax-curve-getClosestPointTo (car rowEnt) pt))
            
            (setq dist (distance pt nearest))
            
            ;; Only move if distance is within search range
            (if (<= dist max-search-dist)
              (progn
                ;; Calculate direction vector from ROW to current position
                (setq direction-vec (list
                                      (- (car pt) (car nearest))
                                      (- (cadr pt) (cadr nearest))
                                      0.0))
                
                ;; Normalize the direction vector
                (setq vec-length (distance '(0 0 0) direction-vec))
                
                ;; If block is ON the ROW (distance = 0), place it perpendicular
                (if (< vec-length 0.001)
                  (progn
                    ;; Get parameter at nearest point
                    (setq param (vlax-curve-getParamAtPoint (car rowEnt) nearest))
                    ;; Get derivative (tangent) at that point
                    (setq tangent (vlax-curve-getFirstDeriv (car rowEnt) param))
                    ;; Calculate perpendicular vector (rotate 90 degrees)
                    (setq direction-vec (list
                                          (- (cadr tangent))  ; Perpendicular: swap X,Y and negate one
                                          (car tangent)
                                          0.0))
                    (setq vec-length (distance '(0 0 0) direction-vec))
                  )
                )
                
                ;; Normalize direction vector
                (if (> vec-length 0.001)
                  (progn
                    (setq direction-vec (list
                                          (/ (car direction-vec) vec-length)
                                          (/ (cadr direction-vec) vec-length)
                                          0.0))
                    
                    ;; Calculate target point at specified offset distance
                    (setq target-pt (list
                                      (+ (car nearest) (* (car direction-vec) offset-dist))
                                      (+ (cadr nearest) (* (cadr direction-vec) offset-dist))
                                      0.0))
                    
                    ;; Move block to target point
                    (command "._MOVE" blk "" pt target-pt)
                    (setq count (1+ count))
                    
                    ;; Progress indicator
                    (if (= (rem count 10) 0)
                      (princ (strcat "\rMoved " (itoa count) " blocks..."))
                    )
                  )
                  ;; If still can't determine direction, skip
                  (setq skipped (1+ skipped))
                )
              )
              (setq skipped (1+ skipped))
            )
            
            (setq i (1+ i))
          )
          
          (princ (strcat "\n\nCompleted!"))
          (princ (strcat "\n  Mode: " mode-input))
          (princ (strcat "\n  " (itoa count) " Toby Box blocks moved to " 
                        (rtos offset-dist 2 2) " feet from Right Of Way"))
          (princ (strcat "\n  " (itoa skipped) " blocks skipped (distance > " 
                        (rtos max-search-dist 2 0) " feet)"))
        )
        (progn
          (if (= mode-input "Manual")
            (princ "\n\nNo Toby Box blocks selected.")
            (princ "\n\nNo 'Toby Box' blocks found in drawing.")
          )
        )
      )
    )
    (princ "\n\nInvalid selection. Please select a valid polyline.")
  )
  
  ;; Restore settings
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  
  (princ)
)

(princ "\nSWMR command loaded - Snap Toby Boxs to Right of Way")
(princ "\nType SWMR to run")
(princ "\n  [A] Auto mode - Process ALL Toby Boxs")
(princ "\n  [M] Manual mode - Select specific Toby Boxs")
(princ "\n  Default offset: 2.0 feet")
(princ)