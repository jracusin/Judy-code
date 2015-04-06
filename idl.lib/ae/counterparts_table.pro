;;; $Id: counterparts_table.pro,v 1.5 2006/07/05 19:30:05 patb Exp $
;;; IDL batch file to make counterparts table.
PRO counterparts_table, aj2mg_cat

null_field = '\nodata'

  NUM_SOURCES = n_elements(aj2mg_cat.ID)
  
  Av_min       = replicate( !VALUES.F_INFINITY,num_sources)
  Av_max       = replicate(-!VALUES.F_INFINITY,num_sources)

  NIR_match_id = replicate(null_field,num_sources)
  match_offset = replicate( !VALUES.F_NAN,num_sources)
  
  ; JHK magnitudes and errors (most from Jiang, some from 2MASS).
  JCIT = replicate( !VALUES.F_NAN,num_sources)
  HCIT = replicate( !VALUES.F_NAN,num_sources)
  KCIT = replicate( !VALUES.F_NAN,num_sources)
  
  EJ   = replicate( !VALUES.F_NAN,num_sources)
  EH   = replicate( !VALUES.F_NAN,num_sources)
  EK   = replicate( !VALUES.F_NAN,num_sources)

  JHKqual = replicate('\nodata',num_sources) ; PH_QUAL from 2MASS.
  FLAG_2MASS   = replicate('',  num_sources) ; CC_FLG from 2MASS
  
  ; Glimpse magnitudes and errors.
  mag3_6 = replicate( !VALUES.F_NAN,num_sources)
  mag4_5 = replicate( !VALUES.F_NAN,num_sources)
  mag5_8 = replicate( !VALUES.F_NAN,num_sources)           
  mag8_0 = replicate( !VALUES.F_NAN,num_sources)

  d3_6m  = replicate( !VALUES.F_NAN,num_sources)
  d4_5m  = replicate( !VALUES.F_NAN,num_sources)
  d5_8m  = replicate( !VALUES.F_NAN,num_sources)           
  d8_0m  = replicate( !VALUES.F_NAN,num_sources)

  
  ID_2MASS     = replicate(null_field,num_sources)
  ID_JIANG     = replicate(null_field,num_sources)
  ID_GLIMPSE   = replicate(null_field,num_sources)
                                                                        
  SM_2MASS     = replicate('-',num_sources)
  SM_JIANG     = replicate('-',num_sources)
  SM_GLIMPSE   = replicate('-',num_sources)             
  
  
  for jj=1,2 do begin
	data = (jj EQ 1) ? aj2mg_cat.jiang : aj2mg_cat.twomass
	IsNull   = data.IsNull 
	
;	;; Find min & max Av values.  
;	;; We process Jiang first and use need_av below to ensure that 2MASS Av values are used only when Jian is not available.
;	
;	need_av = Av_min EQ !VALUES.F_INFINITY
;	
;	; If we need to consider this catalog then look at all four Av values.
;	for ii=1,4 do begin
;	  case ii of  
;	   1: Av = data.Av_1
;	   2: Av = data.Av_2
;	   3: Av = data.Av_3
;	   4: Av = data.Av_4
;	  endcase
;
;	  s = where(need_av AND (IsNull EQ 0) AND (Av GT 0), count )
;	  if (count GT 0) then begin
;		Av_min[s] = Av_min[s] < Av[s]
;		Av_max[s] = Av_max[s] > Av[s]
;	  endif
;	endfor
	
	
NEED BUG FIX:  accept only successful match types since we are now storing failed match types in catalog.

	;; Note the matches & secondary match flag.
	s = where(IsNull EQ 0)
	if (jj EQ 1) then begin  
          ; Jiang
          ID_JIANG[s] = string(data[s].ID,F='(I5)')
          
	  s = where((data.num_SM GT 0) AND (IsNull EQ 0), count )
	  if (count GT 0) then SM_JIANG[s] = 's'
        endif else begin
          ; 2MASS
;         FLAG_2MASS[s] = repchr(data[s].cc_flg, '0','-')
          FLAG_2MASS[s] = data[s].cc_flg
          ID_2MASS[s]   = data[s].DESIGNATION
          
	  s = where((data.num_SM GT 0) AND (IsNull EQ 0), count)
	  if (count GT 0) then SM_2MASS[s] = 's'
        endelse

	                                                  
	;; Record properties of the best IR match. Preference is given to Jiang (jj=1) over 2MASS (jj=2).
	s = where((finite(match_offset) EQ 0) AND (IsNull EQ 0), count )
	if (count GT 0) then begin
	  NIR_match_id[s]     = (jj EQ 1) ? 'Jiang '+string(data[s].ID,F='(I5)') : '2MASS '+data[s].DESIGNATION
          match_offset[s] = sqrt(data[s].DELTAX^2 + data[s].DELTAY^2) * 0.492
	  
	  JCIT[s] = data[s].JCIT
	  HCIT[s] = data[s].HCIT
	  KCIT[s] = data[s].KCIT
          if (jj EQ 1) then begin  
            ; Jiang
            EJ[s] = data[s].EJ
            EH[s] = data[s].EH
            EK[s] = data[s].EK
          endif else begin
            ; 2MASS
            EJ[s] = data[s].J_CMSIG
            EH[s] = data[s].H_CMSIG
            EK[s] = data[s].K_CMSIG
            JHKqual[s] = data[s].ph_qual
          endelse

	endif	
  endfor ;jj
  
  
  ;; Null out any JHK values that are negative.
  s = where(JCIT LT 0, count)
  if (count GT 0) then begin
    JCIT[s] = !VALUES.F_NAN
    EJ  [s] = !VALUES.F_NAN
  endif

  s = where(HCIT LT 0, count)
  if (count GT 0) then begin
    HCIT[s] = !VALUES.F_NAN
    EH  [s] = !VALUES.F_NAN
  endif

  s = where(KCIT LT 0, count)
  if (count GT 0) then begin
    KCIT[s] = !VALUES.F_NAN
    EK  [s] = !VALUES.F_NAN
  endif

  s = where(EJ LT 0, count)
  if (count GT 0) then begin
    EJ  [s] = !VALUES.F_NAN
  endif

  s = where(EH LT 0, count)
  if (count GT 0) then begin
    EH  [s] = !VALUES.F_NAN
  endif

  s = where(EK LT 0, count)
  if (count GT 0) then begin
    EK  [s] = !VALUES.F_NAN
  endif
 
  
  ;; Extract GLIMPSE matches.
  data = aj2mg_cat.glimpse
  IsNull   = data.IsNull 
  s = where(IsNull EQ 0)
  ID_GLIMPSE[s] = data[s].DESIGNATION            
  mag3_6[s] = data[s].mag3_6
  mag4_5[s] = data[s].mag4_5
  mag5_8[s] = data[s].mag5_8
  mag8_0[s] = data[s].mag8_0

  d3_6m[s] = data[s].d3_6m
  d4_5m[s] = data[s].d4_5m
  d5_8m[s] = data[s].d5_8m
  d8_0m[s] = data[s].d8_0m
    
  s = where((data.num_SM GT 0) AND (IsNull EQ 0), count)
  if (count GT 0) then SM_GLIMPSE[s] = 's'
          
  ;; Null out flagged values.
  s = where(mag3_6 GT 99, count)
  if (count GT 0) then begin
    mag3_6[s] = !VALUES.F_NAN
    d3_6m [s] = !VALUES.F_NAN
  endif

  s = where(mag4_5 GT 99, count)
  if (count GT 0) then begin
    mag4_5[s] = !VALUES.F_NAN
    d4_5m [s] = !VALUES.F_NAN
  endif
  
  s = where(mag5_8 GT 99, count)
  if (count GT 0) then begin
    mag5_8[s] = !VALUES.F_NAN
    d5_8m [s] = !VALUES.F_NAN
  endif
  
  s = where(mag8_0 GT 99, count)
  if (count GT 0) then begin
    mag8_0[s] = !VALUES.F_NAN
    d8_0m [s] = !VALUES.F_NAN
  endif
  
  s = where(d3_6m GT 99, count)
  if (count GT 0) then begin
    d3_6m [s] = !VALUES.F_NAN
  endif

  s = where(d4_5m GT 99, count)
  if (count GT 0) then begin
    d4_5m [s] = !VALUES.F_NAN
  endif
  
  s = where(d5_8m GT 99, count)
  if (count GT 0) then begin
    d5_8m [s] = !VALUES.F_NAN
  endif
  
  s = where(d8_0m GT 99, count)
  if (count GT 0) then begin
    d8_0m [s] = !VALUES.F_NAN
  endif
   
  
  ;; Print out entries needing hand annotation.
  colJ   = string(JCIT, F='(%"%5.2f")')
  colH   = string(HCIT, F='(%"%5.2f")')
  colK   = string(KCIT, F='(%"%5.2f")')
  col3_6 = string(mag3_6, F='(%"%5.2f")')
  col4_5 = string(mag4_5, F='(%"%5.2f")')
  col5_8 = string(mag5_8, F='(%"%5.2f")')
  col8_0 = string(mag8_0, F='(%"%5.2f")')
  
  from_2mass = strmatch(NIR_match_id, '2MASS*') 

  ; 2MASS supplied JHK and U flag.
  print, '$>$ annotation on 2MASS J'
  s = where(from_2mass AND strmatch(JHKqual, 'U??'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], JCIT[s], F='(%"%4d  %s  %5.1f")'
  colJ[s] = '$>$'+colJ[s]

  ; 2MASS supplied JHK and U flag.
  print, '$>$ annotation on 2MASS H'
  s = where(from_2mass AND strmatch(JHKqual, '?U?'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], HCIT[s], F='(%"%4d  %s  %5.1f")'
  colH[s] = '$>$'+colH[s]
  
  ; 2MASS supplied JHK and U flag.
  print, '$>$ annotation on 2MASS K'
  s = where(from_2mass AND strmatch(JHKqual, '??U'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], KCIT[s], F='(%"%4d  %s  %5.1f")'
  colK[s] = '$>$'+colK[s]
  
  
;  print, ': annotation on 2MASS JHK'
;  s = where(from_2mass AND (strmatch(JHKqual, '*C*') OR strmatch(FLAG_2MASS, '*D*')))
;  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], JCIT[s], HCIT[s], KCIT[s]
  
  ; JHK supplied and (some error is missing or some error is large)
  TH = 0.1
  print, ': annotation on J'
  s = where((NIR_match_id NE null_field) AND finite(JCIT) AND ((EJ GT TH) OR NOT finite(EJ)))
  forprint, (aj2mg_cat.ACIS.ID)[s], JCIT[s], EJ[s], F='(%"%4d  %6.2f  %6.2f")'
  colJ[s] = colJ[s]+':'
  
  print, ': annotation on H'
  s = where((NIR_match_id NE null_field) AND finite(HCIT) AND ((EH GT TH) OR NOT finite(EH)))
  forprint, (aj2mg_cat.ACIS.ID)[s], HCIT[s], EH[s], F='(%"%4d  %6.2f  %6.2f")'
  colH[s] = colH[s]+':'
  
  print, ': annotation on K'
  s = where((NIR_match_id NE null_field) AND finite(KCIT) AND ((EK GT TH) OR NOT finite(EK)))
  forprint, (aj2mg_cat.ACIS.ID)[s], KCIT[s], EK[s], F='(%"%4d  %6.2f  %6.2f")'
  colK[s] = colK[s]+':'

;  print, ': annotation on mag3_6'
;  s = where((NIR_match_id NE null_field) AND finite(mag3_6) AND ((d3_6m GT TH) OR NOT finite(d3_6m)))
;  forprint, (aj2mg_cat.ACIS.ID)[s], mag3_6[s], d3_6m[s], F='(%"%4d  %6.2f  %6.2f")'
;  col3_6[s] = col3_6[s]+':'
;
;  print, ': annotation on mag4_5'
;  s = where((NIR_match_id NE null_field) AND finite(mag4_5) AND ((d4_5m GT TH) OR NOT finite(d4_5m)))
;  forprint, (aj2mg_cat.ACIS.ID)[s], mag4_5[s], d4_5m[s], F='(%"%4d  %6.2f  %6.2f")'
;  col4_5[s] = col4_5[s]+':'
;
;  print, ': annotation on mag5_8'
;  s = where((NIR_match_id NE null_field) AND finite(mag5_8) AND ((d5_8m GT TH) OR NOT finite(d5_8m)))
;  forprint, (aj2mg_cat.ACIS.ID)[s], mag5_8[s], d5_8m[s], F='(%"%4d  %6.2f  %6.2f")'
;  col5_8[s] = col5_8[s]+':'
;
;  print, ': annotation on mag8_0'
;  s = where((NIR_match_id NE null_field) AND finite(mag8_0) AND ((d8_0m GT TH) OR NOT finite(d8_0m)))
;  forprint, (aj2mg_cat.ACIS.ID)[s], mag8_0[s], d8_0m[s], F='(%"%4d  %6.2f  %6.2f")'
;  col8_0[s] = col8_0[s]+':'

print, 'BUG TO FIX: Bright SIRIUS sources are saturated; we should use 2MASS instead.  The criteria we used for hand adjusting photometry was J<9.5 (#854, 701, 720, 366).'

print, 'BUG TO FIX: The photometry for the O4+O4 is bad.  Could estimate as 0.5 measured value.'


print, 'BUG TO FIX: do not put \nodata in col? text fields; put NaN in numeric vectors so info is included in save file!'

  ; 2MASS supplied JHK and E flag.
  print, 'E phometric flag on 2MASS J'
  s = where(from_2mass AND strmatch(JHKqual, 'E??'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], JCIT[s], F='(%"%4d  %s  %5.1f")'
  colJ[s] = '\nodata'

  ; 2MASS supplied JHK and E flag.
  print, 'E phometric flag on 2MASS H'
  s = where(from_2mass AND strmatch(JHKqual, '?E?'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], HCIT[s], F='(%"%4d  %s  %5.1f")'
  colH[s] = '\nodata'
  
  ; 2MASS supplied JHK and E flag.
  print, 'E phometric flag on 2MASS K'
  s = where(from_2mass AND strmatch(JHKqual, '??E'))
  forprint, (aj2mg_cat.ACIS.ID)[s], JHKqual[s], KCIT[s], F='(%"%4d  %s  %5.1f")'
  colK[s] = '\nodata'

  
  ;; Make ASCII counterparts table.
  ID_ACIS = aj2mg_cat.ACIS.OBJECT_FINAL
  flags = SM_2MASS+SM_JIANG+SM_GLIMPSE
  tempfilename = 'temp.txt'
  forprint, TEXTOUT=tempfilename, /NoCOMMENT, aj2mg_cat.ACIS.ID, ID_ACIS, ID_2MASS+' '+FLAG_2MASS, ID_JIANG, ID_GLIMPSE, flags, colJ, colH, colK, col3_6,col4_5,col5_8,col8_0, JHKqual, F='(%"%3d , %s , , , %23s , %8s , %20s, %s ,  %s , %s , %s , %s , %s , %s , %s , %s ")'

  spawn, string(tempfilename, 'counterparts.txt', F='(%"sed ''s/NaN/\\\\nodata/g'' %s > %s")')

  seq = aj2mg_cat.ACIS.ID
  RA  = aj2mg_cat.ACIS.RA
  DEC = aj2mg_cat.ACIS.DEC
  
  save, FILE='counterparts.sav', seq, ID_ACIS, RA, DEC, ID_2MASS, FLAG_2MASS, ID_JIANG, ID_GLIMPSE, flags, JCIT, HCIT, KCIT, EJ, EH, EK, mag3_6, mag4_5, mag5_8, mag8_0, d3_6m, d4_5m, d5_8m, d8_0m, JHKqual, colJ, colH, colK, col3_6,col4_5,col5_8,col8_0
  
  dataset_1d, id1, color='white', EJ
  dataset_1d, id1, color='white', EH
  dataset_1d, id1, color='white', EK
  dataset_1d, id1, color='red', d3_6m
  dataset_1d, id1, color='red', d4_5m
  dataset_1d, id1, color='red', d5_8m
  dataset_1d, id1, color='red', d8_0m

  ;; Create Latex counterparts table.
  ; Use Latex math mode in object names.
  for ii=0,num_sources-1 do begin
	name = strjoin( strsplit(ID_ACIS[ii],    /EXTRACT, '+'), '$+$')
	name = strjoin( strsplit(name,           /EXTRACT, '-'), '$-$')
	ID_ACIS[ii] = name

        null = where(ID_2MASS EQ null_field)
	name = strjoin( strsplit(ID_2MASS[ii],   /EXTRACT, '+'), '$+$')
	name = strjoin( strsplit(name,           /EXTRACT, '-'), '$-$')
	ID_2MASS[ii] = name
        ID_2MASS[null] = null_field

        null = where(ID_GLIMPSE EQ null_field)
	name = strjoin( strsplit(ID_GLIMPSE[ii], /EXTRACT, '+'), '$+$')
	name = strjoin( strsplit(name,           /EXTRACT, '-'), '$-$')
	ID_GLIMPSE[ii] = name
        ID_GLIMPSE[null] = null_field
	        
  endfor  
  
  tempfilename = 'temp.txt'                    
  forprint, TEXTOUT=tempfilename, /NoCOMMENT, aj2mg_cat.ACIS.ID, ID_ACIS, ID_2MASS+' '+FLAG_2MASS, ID_JIANG, ID_GLIMPSE, flags, colJ, colH, colK, col3_6,col4_5,col5_8,col8_0, JHKqual, F='(%"%3d & %s && & & & %23s & %8s & %20s & %s && %9s & %9s & %9s & %6s & %6s & %6s & %6s \\\\ %% %s")'

  spawn, string(tempfilename, 'counterparts.tex', F='(%"sed ''s/NaN/\\\\nodata/g'' %s > %s")')
  
  

  
return
end
