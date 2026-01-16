(defun c:DW_MTEXT (/ dw-layer row-layer txt-layer txt-style txt-height max-dw-dist ss-dw ss-row 
                     i j ent1 ent2 pt1 pt2 insertpt row-ent closest-pt dist 
                     min-dist row-angle param deriv rotation dw-dist 
                     old-layer old-cmdecho old-osmode old-highlight count
                     search-radius fence-pts ss-nearby param1 param2 k total-dw processed-pairs mtextobj)
  
  ;; ========== USER SETTINGS ==========
  (setq dw-layer "driveways")       ; Layer name for driveway polylines (CASE SENSITIVE!)
  (setq row-layer "Right Of Way")   ; Layer name for Right of Way polylines (CASE SENSITIVE!)
  (setq txt-layer "Driveway")        ; Layer name for D/W MTEXT (will be created if doesn't exist)
  (setq txt-style "Standard")       ; Text style name
  (setq txt-height 3.0)             ; Text height in feet
  (setq max-dw-dist 30.0)           ; Maximum distance between driveways to be considered a pair (in feet)
  ;; ===================================
  
  ;; Store current settings and disable for speed
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setq old-layer (getvar "CLAYER"))
  (setq old-highlight (getvar "HIGHLIGHT"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "HIGHLIGHT" 0)
  (command "._UNDO" "_Begin")
  
  (princ "\nInsert D/W MTEXT Between Driveways...")
  (princ "\n======================================")
  
  ;; Create text layer if it doesn't exist
  (if (not (tblsearch "LAYER" txt-layer))
    (progn
      (command "._LAYER" "_M" txt-layer "_C" "7" txt-layer "")
      (princ (strcat "\nCreated new layer: " txt-layer))
    )
  )
  
  ;; Set current layer to text layer
  (setvar "CLAYER" txt-layer)
  
  ;; Get all driveway polylines
  (setq ss-dw (ssget "X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 dw-layer))))
  
  ;; Get all Right of Way polylines
  (setq ss-row (ssget "X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 row-layer))))
  
  (if (and ss-dw ss-row)
    (progn
      (setq count 0)
      (setq total-dw (sslength ss-dw))
      
      (princ (strcat "\nFound " (itoa total-dw) " driveway polylines"))
      (princ (strcat "\nSearching for adjacent pairs within " (rtos max-dw-dist 2 2) " units..."))
      
      (setq i 0)
      (setq processed-pairs '())
      
      ;; Loop through each driveway
      (repeat total-dw
        (setq ent1 (ssname ss-dw i))
        
        ;; Get midpoint of current driveway
        (setq param1 (/ (vlax-curve-getEndParam ent1) 2.0))
        (setq pt1 (vlax-curve-getPointAtParam ent1 param1))
        
        ;; Create circular search points around this driveway's midpoint
        (setq search-radius (* max-dw-dist 1.2))
        (setq fence-pts (list
                         pt1
                         (polar pt1 0 search-radius)
                         (polar pt1 (/ pi 2) search-radius)
                         (polar pt1 pi search-radius)
                         (polar pt1 (* 1.5 pi) search-radius)
                         pt1
                        ))
        
        ;; Get only nearby driveways using fence
        (setq ss-nearby (ssget "_F" fence-pts 
                               (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 dw-layer))))
        
        (if ss-nearby
          (progn
            ;; Check each nearby driveway
            (setq j 0)
            (repeat (sslength ss-nearby)
              (setq ent2 (ssname ss-nearby j))
              
              ;; Only process if different entities and not already processed
              (if (and (/= ent1 ent2)
                       (not (equal ent1 ent2))
                       (not (member (list ent1 ent2) processed-pairs))
                       (not (member (list ent2 ent1) processed-pairs)))
                (progn
                  ;; Get midpoint of second driveway
                  (setq param2 (/ (vlax-curve-getEndParam ent2) 2.0))
                  (setq pt2 (vlax-curve-getPointAtParam ent2 param2))
                  
                  ;; Check actual distance
                  (setq dw-dist (distance pt1 pt2))
                  
                  ;; Ensure distance is greater than 0 (not the same line) and within max distance
                  (if (and (> dw-dist 0.1) (<= dw-dist max-dw-dist))
                    (progn
                      ;; Mark as processed
                      (setq processed-pairs (cons (list ent1 ent2) processed-pairs))
                      
                      ;; Calculate insertion point
                      (setq insertpt (list
                                      (/ (+ (car pt1) (car pt2)) 2.0)
                                      (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                                      0.0
                                     ))
                      
                      ;; Find closest Right of Way polyline
                      (setq min-dist 1e99)
                      (setq row-angle 0.0)
                      (setq k 0)
                      
                      (repeat (sslength ss-row)
                        (setq row-ent (ssname ss-row k))
                        (setq closest-pt (vlax-curve-getClosestPointTo row-ent insertpt))
                        (setq dist (distance insertpt closest-pt))
                        
                        (if (< dist min-dist)
                          (progn
                            (setq min-dist dist)
                            (setq param (vlax-curve-getParamAtPoint row-ent closest-pt))
                            (setq deriv (vlax-curve-getFirstDeriv row-ent param))
                            (setq row-angle (atan (cadr deriv) (car deriv)))
                          )
                        )
                        (setq k (1+ k))
                      )
                      
                      ;; Make text readable using quadrant rule
                      (setq rotation (rem row-angle (* 2 pi)))
                      (if (< rotation 0)
                        (setq rotation (+ rotation (* 2 pi)))
                      )
                      
                      (if (and (>= rotation (/ pi 2)) (< rotation (* 3 (/ pi 2))))
                        (setq rotation (+ rotation pi))
                      )
                      
                      (if (>= rotation (* 2 pi))
                        (setq rotation (- rotation (* 2 pi)))
                      )
                      
                      ;; Insert MTEXT using VLA objects (no prompts)
                      (setq mtextobj (vla-addMText 
                                       (vla-get-ModelSpace 
                                         (vla-get-ActiveDocument (vlax-get-acad-object)))
                                       (vlax-3d-point insertpt)
                                       0.0
                                       "D/W"))
                      (vla-put-Height mtextobj txt-height)
                      (vla-put-StyleName mtextobj txt-style)
                      (vla-put-Rotation mtextobj rotation)
                      (vla-put-AttachmentPoint mtextobj acAttachmentPointMiddleCenter)
                      (vla-put-InsertionPoint mtextobj (vlax-3d-point insertpt))
                      
                      (setq count (1+ count))
                      
                      ;; Progress indicator every 10 insertions
                      (if (= (rem count 10) 0)
                        (princ (strcat "\r" (itoa count) " texts inserted..."))
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )
          )
        )
        
        ;; Progress indicator every 100 driveways
        (if (= (rem i 100) 0)
          (princ (strcat "\rProcessing driveway " (itoa i) " of " (itoa total-dw) "..."))
        )
        
        (setq i (1+ i))
      )
      
      (princ (strcat "\n\nCompleted! " (itoa count) " D/W MTEXT inserted on layer: " txt-layer))
      (if (= count 0)
        (princ (strcat "\nTIP: Try increasing max-dw-dist (currently " (rtos max-dw-dist 2 1) " units)"))
      )
    )
    (progn
      (if (not ss-dw)
        (princ (strcat "\nERROR: No polylines found on layer: " dw-layer))
      )
      (if (not ss-row)
        (princ (strcat "\nERROR: No polylines found on layer: " row-layer))
      )
    )
  )
  
  ;; Restore settings
  (command "._UNDO" "_End")
  (setvar "CLAYER" old-layer)
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (setvar "HIGHLIGHT" old-highlight)
  
  (princ)
)

;; Alternative command for manual selection
(defun c:DWTEXT2 (/ dw1 dw2 row-layer txt-layer txt-style txt-height pt1 pt2 insertpt 
                     ss-row j row-ent closest-pt dist min-dist row-angle 
                     param deriv rotation old-layer old-cmdecho old-osmode old-highlight
                     param1 param2 mtextobj)
  
  ;; ========== USER SETTINGS ==========
  (setq row-layer "Right of Way")   ; Layer name for Right of Way polylines (CASE SENSITIVE!)
  (setq txt-layer "DW-TEXT")        ; Layer name for D/W MTEXT
  (setq txt-style "Standard")       ; Text style name
  (setq txt-height 3.0)             ; Text height in feet
  ;; ===================================
  
  ;; Store current settings
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setq old-layer (getvar "CLAYER"))
  (setq old-highlight (getvar "HIGHLIGHT"))
  (setvar "CMDECHO" 0)
  (setvar "HIGHLIGHT" 0)
  
  ;; Create text layer if it doesn't exist
  (if (not (tblsearch "LAYER" txt-layer))
    (progn
      (command "._LAYER" "_M" txt-layer "_C" "7" txt-layer "")
      (princ (strcat "\nCreated new layer: " txt-layer))
    )
  )
  
  ;; Set current layer to text layer
  (setvar "CLAYER" txt-layer)
  
  (princ "\nSelect first driveway polyline: ")
  (setq dw1 (car (entsel)))
  
  (if dw1
    (progn
      (princ "\nSelect second driveway polyline: ")
      (setq dw2 (car (entsel)))
      
      (if dw2
        (progn
          ;; Get midpoints
          (setq param1 (/ (vlax-curve-getEndParam dw1) 2.0))
          (setq pt1 (vlax-curve-getPointAtParam dw1 param1))
          
          (setq param2 (/ (vlax-curve-getEndParam dw2) 2.0))
          (setq pt2 (vlax-curve-getPointAtParam dw2 param2))
          
          ;; Calculate insertion point
          (setq insertpt (list
                          (/ (+ (car pt1) (car pt2)) 2.0)
                          (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                          0.0
                         ))
          
          ;; Get Right of Way polylines
          (setq ss-row (ssget "X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 row-layer))))
          
          (if ss-row
            (progn
              ;; Find closest ROW polyline
              (setq min-dist 1e99)
              (setq row-angle 0.0)
              (setq j 0)
              
              (repeat (sslength ss-row)
                (setq row-ent (ssname ss-row j))
                (setq closest-pt (vlax-curve-getClosestPointTo row-ent insertpt))
                (setq dist (distance insertpt closest-pt))
                
                (if (< dist min-dist)
                  (progn
                    (setq min-dist dist)
                    (setq param (vlax-curve-getParamAtPoint row-ent closest-pt))
                    (setq deriv (vlax-curve-getFirstDeriv row-ent param))
                    (setq row-angle (atan (cadr deriv) (car deriv)))
                  )
                )
                (setq j (1+ j))
              )
              
              ;; Make text readable using quadrant rule
              (setq rotation (rem row-angle (* 2 pi)))
              (if (< rotation 0)
                (setq rotation (+ rotation (* 2 pi)))
              )
              
              (if (and (>= rotation (/ pi 2)) (< rotation (* 3 (/ pi 2))))
                (setq rotation (+ rotation pi))
              )
              
              (if (>= rotation (* 2 pi))
                (setq rotation (- rotation (* 2 pi)))
              )
              
              ;; Insert MTEXT using VLA objects (no prompts)
              (setq mtextobj (vla-addMText 
                               (vla-get-ModelSpace 
                                 (vla-get-ActiveDocument (vlax-get-acad-object)))
                               (vlax-3d-point insertpt)
                               0.0
                               "D/W"))
              (vla-put-Height mtextobj txt-height)
              (vla-put-StyleName mtextobj txt-style)
              (vla-put-Rotation mtextobj rotation)
              (vla-put-AttachmentPoint mtextobj acAttachmentPointMiddleCenter)
              (vla-put-InsertionPoint mtextobj (vlax-3d-point insertpt))
              
              (princ (strcat "\nD/W MTEXT inserted successfully on layer: " txt-layer))
            )
            (princ (strcat "\nERROR: No polylines found on layer: " row-layer))
          )
        )
        (princ "\nNo second polyline selected.")
      )
    )
    (princ "\nNo first polyline selected.")
  )
  
  ;; Restore settings
  (setvar "CLAYER" old-layer)
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  (setvar "HIGHLIGHT" old-highlight)
  
  (princ)
)

(princ "\nDriveway MTEXT Commands Loaded!")
(princ "\nType DWTEXT - Auto insert D/W MTEXT between adjacent driveway pairs")
(princ "\nType DWTEXT2 - Manually select two driveway polylines")
(princ "\n")
(princ "\nNote: MTEXT will be created on 'DW-TEXT' layer (customizable in code)")
(princ)