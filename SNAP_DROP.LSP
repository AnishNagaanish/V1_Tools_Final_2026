(defun c:SNAP_DROP (/ drop-layer block1-name block2-name ss-drops ss-blocks1 ss-blocks2
                       i drop-ent drop-pt closest-block min-dist block-ent block-pt
                       dist old-cmdecho old-osmode count total-drops max-tolerance
                       j exclude-block ss-exclude exclude-ent exclude-data exclude-pt k 
                       connection-tolerance skipped-connected skipped-tolerance start-pt 
                       end-pt start-connected end-connected debug-count min-start-dist 
                       min-end-dist start-dist end-dist snap-start-or-end found-block-name 
                       count-dap count-toby debug-blocks ss-all-blocks temp-ent temp-data 
                       temp-name temp-count temp-pt ss-all similar-found n check-ent 
                       check-data check-name vertex-index num-vertices closest-vertex-dist 
                       closest-vertex-pt closest-vertex-index v vertex-pt min-vertex-to-block 
                       dist-vb ent-data ent-list vertex-count new-ent-list item found-dap-dist 
                       found-toby-dist dap-found-within-tolerance toby-variants variant 
                       block2-layer use-layer-for-toby temp-layer check-layer info-str)
  
  ;; ========== USER SETTINGS ==========
  (setq drop-layer "Drop")              ; Layer name for drop polylines
  (setq block1-name "TOBY BOX")   ; Primary block to snap to
  (setq block2-name "TOBY BOX")         ; Secondary block name (if using name search)
  (setq block2-layer "Hexatronics_Toby_Box")            ; Secondary block LAYER (if using layer search)
  (setq use-layer-for-toby T)           ; Set to T to search by layer, nil for block name
  (setq exclude-block "Add")            ; Skip drops already connected to this block
  (setq max-tolerance 25.0)             ; Maximum distance to snap (in feet)
  (setq connection-tolerance 2.0)       ; Distance to consider as "connected" (in feet)
  (setq snap-start-or-end "NEAREST")    ; Which end to snap: "START", "END", or "NEAREST"
  (setq debug-blocks T)                 ; Set to T to see debug info
  ;; ===================================
  
  ;; Store current settings and disable for speed
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  (princ "\nSnap Drop Polylines to Nearest Block...")
  (princ "\n========================================")
  
  ;; Debug: List all block names in drawing if enabled
  (if debug-blocks
    (progn
      (princ "\n\nDEBUG: Searching for all blocks in drawing...")
      (setq ss-all-blocks (ssget "X" '((0 . "INSERT"))))
      (if ss-all-blocks
        (princ (strcat "\nFound " (itoa (sslength ss-all-blocks)) " total blocks"))
        (princ "\nNo blocks found in drawing!")
      )
    )
  )
  
  ;; Get all drop polylines
  (setq ss-drops (ssget "X" (list '(0 . "LWPOLYLINE,POLYLINE") (cons 8 drop-layer))))
  
  ;; Get primary blocks (DAP HAND DIG)
  (setq ss-blocks1 (ssget "X" (list '(0 . "INSERT") (cons 2 block1-name))))
  
  (if ss-blocks1
    (progn
      (princ (strcat "\n✓ Found " (itoa (sslength ss-blocks1)) " '" block1-name "' blocks"))
      (if debug-blocks
        (progn
          (princ "\n  First 3 DAP locations:")
          (setq temp-count 0)
          (repeat (min 3 (sslength ss-blocks1))
            (setq temp-ent (ssname ss-blocks1 temp-count))
            (setq temp-data (entget temp-ent))
            (setq temp-pt (cdr (assoc 10 temp-data)))
            (setq temp-name (cdr (assoc 2 temp-data)))
            (princ (strcat "\n    Block name: \"" temp-name "\" at (" 
                          (rtos (car temp-pt) 2 2) ", " (rtos (cadr temp-pt) 2 2) ")"))
            (setq temp-count (1+ temp-count))
          )
        )
      )
    )
    (princ (strcat "\n✗ No '" block1-name "' blocks found - check spelling!"))
  )
  
  ;; Get secondary blocks (TOBY) - try by layer if enabled, otherwise by name
  (princ "\n\nSearching for TOBY BOX blocks...")
  
  (if use-layer-for-toby
    (progn
      (princ (strcat "\nSearching by LAYER: \"" block2-layer "\""))
      (setq ss-blocks2 (ssget "X" (list '(0 . "INSERT") (cons 8 block2-layer))))
    )
    (progn
      (princ (strcat "\nSearching by BLOCK NAME: \"" block2-name "\""))
      (setq ss-blocks2 (ssget "X" (list '(0 . "INSERT") (cons 2 block2-name))))
    )
  )
  
  (if ss-blocks2
    (progn
      (princ (strcat "\n✓ Found " (itoa (sslength ss-blocks2)) " TOBY blocks"))
      (if debug-blocks
        (progn
          (princ "\n  First 3 TOBY locations:")
          (setq temp-count 0)
          (repeat (min 3 (sslength ss-blocks2))
            (setq temp-ent (ssname ss-blocks2 temp-count))
            (setq temp-data (entget temp-ent))
            (setq temp-pt (cdr (assoc 10 temp-data)))
            (setq temp-name (cdr (assoc 2 temp-data)))
            (setq temp-layer (cdr (assoc 8 temp-data)))
            (princ (strcat "\n    Block: \"" temp-name "\" Layer: \"" temp-layer "\" at (" 
                          (rtos (car temp-pt) 2 2) ", " (rtos (cadr temp-pt) 2 2) ")"))
            (setq temp-count (1+ temp-count))
          )
        )
      )
    )
    (progn
      (if use-layer-for-toby
        (princ (strcat "\n✗ No blocks found on layer: \"" block2-layer "\""))
        (princ (strcat "\n✗ No blocks found with name: \"" block2-name "\""))
      )
      (princ "\n\nSearching for similar blocks/layers with 'TOB' or 'TB' in the name...")
      (setq ss-all (ssget "X" '((0 . "INSERT"))))
      (if ss-all
        (progn
          (setq similar-found nil)
          (setq toby-variants '())
          (setq n 0)
          (repeat (sslength ss-all)
            (setq check-ent (ssname ss-all n))
            (setq check-data (entget check-ent))
            (setq check-name (cdr (assoc 2 check-data)))
            (setq check-layer (cdr (assoc 8 check-data)))
            (if (or (vl-string-search "TOB" (strcase check-name))
                    (vl-string-search "TOB" (strcase check-layer))
                    (vl-string-search "TB" (strcase check-layer)))
              (progn
                (setq info-str (strcat "Block:\"" check-name "\" Layer:\"" check-layer "\""))
                (if (not (member info-str toby-variants))
                  (setq toby-variants (cons info-str toby-variants))
                )
                (setq similar-found T)
              )
            )
            (setq n (1+ n))
          )
          (if similar-found
            (progn
              (princ "\nFound blocks/layers containing 'TOB' or 'TB':")
              (foreach variant toby-variants
                (princ (strcat "\n  - " variant))
              )
              (princ "\n\nUpdate the code with the correct name:")
              (princ "\n  (setq use-layer-for-toby T)")
              (princ "\n  (setq block2-layer \"EXACT_LAYER_NAME\")")
              (princ "\nOR")
              (princ "\n  (setq use-layer-for-toby nil)")
              (princ "\n  (setq block2-name \"EXACT_BLOCK_NAME\")")
            )
            (princ "\nNo blocks found with 'TOB' or 'TB' in the name or layer!")
          )
        )
      )
    )
  )
  
  ;; Get all "Add" blocks to check for existing connections
  (setq ss-exclude (ssget "X" (list '(0 . "INSERT") (cons 2 exclude-block))))
  
  (if ss-exclude
    (princ (strcat "\nFound " (itoa (sslength ss-exclude)) " '" exclude-block "' blocks"))
    (princ (strcat "\nWARNING: No '" exclude-block "' blocks found in drawing!"))
  )
  
  (if (or ss-blocks1 ss-blocks2)
    (progn
      (setq count 0)
      (setq count-dap 0)
      (setq count-toby 0)
      (setq skipped-connected 0)
      (setq skipped-tolerance 0)
      (setq total-drops (sslength ss-drops))
      
      (princ (strcat "\n\nFound " (itoa total-drops) " drop polylines"))
      (princ (strcat "\nConnection tolerance: " (rtos connection-tolerance 2 2) " feet"))
      (princ (strcat "\nSnap tolerance: " (rtos max-tolerance 2 2) " feet"))
      (princ "\n\nProcessing...")
      
      (setq i 0)
      (setq debug-count 0)
      
      ;; Loop through each drop polyline
      (repeat total-drops
        (setq drop-ent (ssname ss-drops i))
        
        ;; Get both endpoints
        (setq start-pt (vlax-curve-getStartPoint drop-ent))
        (setq end-pt (vlax-curve-getEndPoint drop-ent))
        
        ;; Check if EITHER end is connected to "Add" block
        (setq start-connected nil)
        (setq end-connected nil)
        (setq min-start-dist 1e99)
        (setq min-end-dist 1e99)
        
        (if ss-exclude
          (progn
            (setq k 0)
            (repeat (sslength ss-exclude)
              (setq exclude-ent (ssname ss-exclude k))
              (setq exclude-data (entget exclude-ent))
              (setq exclude-pt (cdr (assoc 10 exclude-data)))
              
              ;; Check start point
              (setq start-dist (distance start-pt exclude-pt))
              (if (< start-dist min-start-dist)
                (setq min-start-dist start-dist)
              )
              (if (<= start-dist connection-tolerance)
                (setq start-connected T)
              )
              
              ;; Check end point
              (setq end-dist (distance end-pt exclude-pt))
              (if (< end-dist min-end-dist)
                (setq min-end-dist end-dist)
              )
              (if (<= end-dist connection-tolerance)
                (setq end-connected T)
              )
              
              (setq k (1+ k))
            )
          )
        )
        
        ;; Skip if ANY end is connected to Add block
        (if (or start-connected end-connected)
          (progn
            (setq skipped-connected (1+ skipped-connected))
          )
          (progn
            ;; NOT connected to Add - proceed to snap
            
            ;; Find nearest VERTEX based on mode
            (if (= (strcase snap-start-or-end) "NEAREST")
              (progn
                ;; Get all vertices of the polyline
                (setq num-vertices (+ (fix (vlax-curve-getEndParam drop-ent)) 1))
                (setq closest-vertex-dist 1e99)
                (setq closest-vertex-pt nil)
                (setq closest-vertex-index 0)
                
                ;; Check each vertex against all target blocks
                (setq v 0)
                (repeat num-vertices
                  (setq vertex-pt (vlax-curve-getPointAtParam drop-ent (float v)))
                  
                  ;; Find minimum distance from this vertex to any target block
                  (setq min-vertex-to-block 1e99)
                  
                  ;; Check against DAP blocks
                  (if ss-blocks1
                    (progn
                      (setq j 0)
                      (repeat (sslength ss-blocks1)
                        (setq block-ent (ssname ss-blocks1 j))
                        (setq block-data (entget block-ent))
                        (setq block-pt (cdr (assoc 10 block-data)))
                        (setq dist-vb (distance vertex-pt block-pt))
                        (if (< dist-vb min-vertex-to-block)
                          (setq min-vertex-to-block dist-vb)
                        )
                        (setq j (1+ j))
                      )
                    )
                  )
                  
                  ;; Check against TOBY blocks
                  (if ss-blocks2
                    (progn
                      (setq j 0)
                      (repeat (sslength ss-blocks2)
                        (setq block-ent (ssname ss-blocks2 j))
                        (setq block-data (entget block-ent))
                        (setq block-pt (cdr (assoc 10 block-data)))
                        (setq dist-vb (distance vertex-pt block-pt))
                        (if (< dist-vb min-vertex-to-block)
                          (setq min-vertex-to-block dist-vb)
                        )
                        (setq j (1+ j))
                      )
                    )
                  )
                  
                  ;; Track the closest vertex overall
                  (if (< min-vertex-to-block closest-vertex-dist)
                    (progn
                      (setq closest-vertex-dist min-vertex-to-block)
                      (setq closest-vertex-pt vertex-pt)
                      (setq closest-vertex-index v)
                    )
                  )
                  
                  (setq v (1+ v))
                )
                
                (setq drop-pt closest-vertex-pt)
                (setq vertex-index closest-vertex-index)
              )
              (progn
                ;; Use START or END based on setting
                (if (= (strcase snap-start-or-end) "START")
                  (progn
                    (setq drop-pt start-pt)
                    (setq vertex-index 0)
                  )
                  (progn
                    (setq drop-pt end-pt)
                    (setq vertex-index (fix (vlax-curve-getEndParam drop-ent)))
                  )
                )
              )
            )
            
            ;; PRIORITY: Check DAP first, then TOBY as fallback
            (setq min-dist 1e99)
            (setq closest-block nil)
            (setq found-block-name "")
            (setq found-dap-dist 1e99)
            (setq found-toby-dist 1e99)
            (setq dap-found-within-tolerance nil)
            
            ;; First priority: Check DAP blocks
            (if ss-blocks1
              (progn
                (setq j 0)
                (repeat (sslength ss-blocks1)
                  (setq block-ent (ssname ss-blocks1 j))
                  (setq block-data (entget block-ent))
                  (setq block-pt (cdr (assoc 10 block-data)))
                  
                  (setq dist (distance drop-pt block-pt))
                  
                  ;; Track closest DAP
                  (if (< dist found-dap-dist)
                    (setq found-dap-dist dist)
                  )
                  
                  ;; If DAP is within tolerance, use it
                  (if (and (<= dist max-tolerance) (< dist min-dist))
                    (progn
                      (setq min-dist dist)
                      (setq closest-block block-pt)
                      (setq found-block-name block1-name)
                      (setq dap-found-within-tolerance T)
                    )
                  )
                  
                  (setq j (1+ j))
                )
              )
            )
            
            ;; Second priority: Check TOBY blocks ONLY if no DAP found within tolerance
            (if (and ss-blocks2 (not dap-found-within-tolerance))
              (progn
                (setq j 0)
                (repeat (sslength ss-blocks2)
                  (setq block-ent (ssname ss-blocks2 j))
                  (setq block-data (entget block-ent))
                  (setq block-pt (cdr (assoc 10 block-data)))
                  
                  (setq dist (distance drop-pt block-pt))
                  
                  ;; Track closest TOBY
                  (if (< dist found-toby-dist)
                    (setq found-toby-dist dist)
                  )
                  
                  ;; Use TOBY only if within tolerance
                  (if (and (<= dist max-tolerance) (< dist min-dist))
                    (progn
                      (setq min-dist dist)
                      (setq closest-block block-pt)
                      (setq found-block-name block2-name)
                    )
                  )
                  
                  (setq j (1+ j))
                )
              )
            )
            
            ;; Debug output for first few drops
            (if (< debug-count 5)
              (progn
                (if closest-block
                  (princ (strcat "\n  Drop #" (itoa (1+ i)) ": Using " found-block-name 
                                " at " (rtos min-dist 2 2) " ft"))
                  (princ (strcat "\n  Drop #" (itoa (1+ i)) ": No blocks within tolerance"))
                )
                (if (and ss-blocks1 ss-blocks2)
                  (princ (strcat " (Nearest DAP:" (rtos found-dap-dist 2 2) 
                                " TOBY:" (rtos found-toby-dist 2 2) ")"))
                )
                (setq debug-count (1+ debug-count))
              )
            )
            
            ;; Snap the vertex to nearest block
            (if (and closest-block (<= min-dist max-tolerance))
              (progn
                ;; Move only the specific vertex using entmod
                (setq ent-data (entget drop-ent))
                (setq ent-list ent-data)
                (setq vertex-count 0)
                
                ;; For LWPOLYLINE, modify the vertex coordinates
                (if (= (cdr (assoc 0 ent-data)) "LWPOLYLINE")
                  (progn
                    (setq new-ent-list '())
                    (foreach item ent-list
                      (if (= (car item) 10)
                        (progn
                          (if (= vertex-count vertex-index)
                            ;; Replace this vertex with new coordinates
                            (setq new-ent-list (append new-ent-list 
                                  (list (cons 10 (list (car closest-block) (cadr closest-block))))))
                            ;; Keep original vertex
                            (setq new-ent-list (append new-ent-list (list item)))
                          )
                          (setq vertex-count (1+ vertex-count))
                        )
                        ;; Keep non-vertex data
                        (setq new-ent-list (append new-ent-list (list item)))
                      )
                    )
                    (entmod new-ent-list)
                    (entupd drop-ent)
                  )
                  ;; For old POLYLINE, use stretch command
                  (command "._STRETCH" (ssadd drop-ent) "" drop-pt closest-block)
                )
                
                (setq count (1+ count))
                
                ;; Track which block type was used
                (if (= found-block-name block1-name)
                  (setq count-dap (1+ count-dap))
                  (setq count-toby (1+ count-toby))
                )
                
                ;; Progress indicator
                (if (= (rem count 10) 0)
                  (princ (strcat "\rSnapped " (itoa count) "..."))
                )
              )
              (progn
                ;; Skip if too far
                (if closest-block
                  (setq skipped-tolerance (1+ skipped-tolerance))
                )
              )
            )
          )
        )
        
        (setq i (1+ i))
      )
      
      (princ (strcat "\n\nCompleted! " (itoa count) " drop polylines snapped"))
      (if (> count-dap 0)
        (princ (strcat "\n  " (itoa count-dap) " snapped to '" block1-name "'"))
      )
      (if (> count-toby 0)
        (princ (strcat "\n  " (itoa count-toby) " snapped to TOBY blocks"))
      )
      (if (> skipped-connected 0)
        (princ (strcat "\n" (itoa skipped-connected) 
                      " polylines skipped (already connected to '" exclude-block "' block)"))
      )
      (if (> skipped-tolerance 0)
        (princ (strcat "\n" (itoa skipped-tolerance) 
                      " polylines skipped (no blocks within " (rtos max-tolerance 2 0) " ft)"))
      )
    )
    (progn
      (if (not ss-blocks1)
        (princ (strcat "\nWARNING: No '" block1-name "' blocks found"))
      )
      (if (not ss-blocks2)
        (princ "\nWARNING: No TOBY blocks found")
      )
    )
  )
  
  ;; Restore settings
  (setvar "CMDECHO" old-cmdecho)
  (setvar "OSMODE" old-osmode)
  
  (princ)
)

(princ "\nSnap Drop Commands Loaded!")
(princ "\nType SNAPDROP - Snap drop polylines to nearest DAP/TOBY blocks")
(princ "\n")
(princ "\nSettings:")
(princ "\n  Snap mode: NEAREST vertex")
(princ "\n  TOBY search: By LAYER 'TB_B'")
(princ "\n  Priority: DAP first, then TOBY")
(princ)