; Event Handler
PRO WIDGET_SORT_EVENT, event

; Get structure stored in top-level widget
; as well as the user-generated event
WIDGET_CONTROL, event.top, GET_UVALUE=sorts
WIDGET_CONTROL, event.id, GET_UVALUE=action

; Catch any possible errors
CATCH, theError
IF theError NE 0 THEN BEGIN
  CATCH, /CANCEL
  PRINT, !ERROR_STATE.MSG
  RETURN
ENDIF

CASE action OF

  'accept': BEGIN
              ; Send changes to sorting routine and
              ; destroy widget
              order=sorts.order
              sorts.respectable->MULTISORT, order
              WIDGET_CONTROL, event.top, /DESTROY
              RETURN
            END
            
  'cancel': BEGIN
              ; Destroy widget and return
              WIDGET_CONTROL, event.top, /DESTROY
              RETURN
            END
            
  '1':  BEGIN  ; Checkbox event
          
          ; Find which button generated the event and
          ; change sensitivity status of input accordingly 
          index=WHERE(sorts.boxIDs EQ event.id)
          CASE event.select OF
            1: WIDGET_CONTROL, (sorts.orderIDs)[index], SENSITIVE=1
            0: WIDGET_CONTROL, (sorts.orderIDs)[index], SENSITIVE=0, SET_VALUE=''
          ENDCASE
        END
        
  ELSE: BEGIN  ; Order input event
  
          ; Find which input box generated the event and
          ; change/save input in structure
          index=WHERE(sorts.orderIDs EQ event.id)
          WIDGET_CONTROL, event.id, GET_VALUE=order
          IF WHERE(ABS(sorts.order) EQ ABS(order)) NE -1 THEN BREAK 
          sorts.order[index]=order
          WIDGET_CONTROL, event.top, SET_UVALUE=sorts
        END

ENDCASE

END

;------------------------------------------------------------------------------------------------

; Build Widget
PRO WIDGET_SORT, showcols, respectable

; Create top-level base
tlb=WIDGET_BASE(TITLE='Columns Sort', /BASE_ALIGN_CENTER, ROW=4, SCR_XSIZE=250)

; Create static information labels
info=WIDGET_LABEL(tlb, VALUE='Rank in order of sort relevance (1-9)', /FRAME, /ALIGN_CENTER)
infotext='Positive numbers sort in ascending order' + STRING(10B) + $
         'Negative numbers sort in descending order' 
info=WIDGET_LABEL(tlb, VALUE=infotext, /FRAME, /ALIGN_CENTER)

mlb=WIDGET_BASE(tlb, /BASE_ALIGN_CENTER, COLUMN=4)

numdisp=N_ELEMENTS(WHERE(showcols EQ 1))-1
colbase=LONARR(4)

colbase[0]=WIDGET_BASE(mlb, /FRAME, ROW=10)

IF numdisp GT 10 THEN BEGIN
  WIDGET_CONTROL, tlb, SCR_XSIZE=325
  colbase[1]=WIDGET_BASE(mlb, /FRAME, ROW=10)
ENDIF

IF numdisp GT 20 THEN BEGIN
  WIDGET_CONTROL, tlb, SCR_XSIZE=500
  colbase[2]=WIDGET_BASE(mlb, /FRAME, ROW=10)
ENDIF

IF numdisp GT 30 THEN BEGIN
  WIDGET_CONTROL, tlb, SCR_XSIZE=650
  colbase[3]=WIDGET_BASE(mlb, /FRAME, ROW=10)
ENDIF

columns=['Detectors','GRB Name','Data','Dt','Model','Amplitude','Epeak','Break E','Break E1',$
         'Break E2','Alpha','Beta','Index(PL/Comp)','Index1','Index2','Index3','Electron E',$
         'Op. Depth','Op. Depth2','Geom. Factor','kT','Cutoff E','E Folding','Param. Ratio','Cosmol. z',$
         'Metalicity','Mu', 'Sigma','Centroid','FWHM','FWHM slope','Line Width','Electron Cool',$
         'nHe/NH','H Column Dens.','Chi2/DOF','Red. Chi2','Likely/DOF', 'C-Stat/DOF', 'Ph. Flux',$
         'En. Flux']
numcols=N_ELEMENTS(columns)
RowIDs=LONARR(numcols)
ButtIDs=LONARR(numcols)
BoxIDs=LONARR(numcols)
OrderIDs=LONARR(numcols)

row=0
FOR i=0, numcols-1 DO BEGIN
  IF showcols[i] EQ 1 THEN BEGIN
    IF row LE 9  THEN dispcol=colbase[0]
    IF row GT 9  THEN dispcol=colbase[1]
    IF row GT 19 THEN dispcol=colbase[2]
    IF row GT 29 THEN dispcol=colbase[3]
    
    RowIDs[i]=WIDGET_BASE(dispcol, COLUMN=2)
    ButtIDs[i]=WIDGET_BASE(RowIDs[i], /NONEXCLUSIVE)
    BoxIDs[i]=WIDGET_BUTTON(ButtIDs[i], VALUE=columns[i], XSIZE=100, UVALUE='1')
    OrderIDs[i]=WIDGET_TEXT(RowIDs[i], VALUE='', XSIZE=2, /EDITABLE, $
                            SENSITIVE=0, /ALL_EVENTS, UVALUE='2')
  row=row+1  
  ENDIF ELSE BEGIN
    BoxIDs[i]=0
    OrderIDs[i]=0
  ENDELSE
ENDFOR

; Create the 'Accept' and 'Cancel' buttons
bottom=WIDGET_BASE(tlb, COLUMN=2, /BASE_ALIGN_CENTER)
accepted=WIDGET_BUTTON(bottom, VALUE='Accept', UVALUE='accept')
canceled=WIDGET_BUTTON(bottom, VALUE='Cancel', UVALUE='cancel')

; Realize widget
WIDGET_CONTROL, tlb, /REALIZE


; Create structure and store in top-level widget
sorts={sorts, boxIDs:boxIDs, orderIDs:orderIDs, order:LONARR(numcols), respectable:respectable}
WIDGET_CONTROl, tlb, SET_UVALUE=sorts

XMANAGER, 'WIDGET_SORT', tlb, /NO_BLOCK

END