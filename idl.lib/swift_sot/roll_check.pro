pro roll_check,ra_ppst,dec_ppst,roll_ppst


;Add the star tracker field - Spacecraft roll to the roll angles
;There is a 90degrees+17degres offset between spacecraft 
;coord and star tracker coord
  
  if n_params() lt 3 then begin
     print,'Please enter correct parameters: roll_check,ra,dec,roll'
     return
  endif
  
roll_ppst=253.0-roll_ppst

;Read the star catalog
readcol,'/home/pagani/XDS/star_track/test_shuffle/fl_cat.txt',ra_cat,dec_cat,mag_cat,id_cat,format='(f,f,f,a)',skip=1,/silent

;DENSITY
radec6deg=0
radecdens=0
border=0

;Check for stars in FoV 

for inccat=0l,n_elements(ra_cat)-1 do begin
    res = sphdist(ra_cat(inccat), dec_cat(inccat), ra_ppst, dec_ppst,/degrees) ;angles in DEGREES
    if res lt 6.0 then begin ; if star closer than 6.0 degrees check if it's in the 8x8 camera field of view
        radec6deg=radec6deg+1
        rotate_roll,ra_ppst,dec_ppst,ra_cat(inccat),dec_cat(inccat),roll_ppst*!dtor,ra_cat_new,dec_cat_new
        ;stop
        ;Get Star Tracker H and V coordinates after rotation
        
                                ;Here I do a simple check if there is
                                ;a situation like this: RA_PPST+359.8
                                ;and RA_CAT=0.5, then the two sources
                                ;are very close but H=359!!! not real!!
        if (abs(ra_cat_new-ra_ppst) gt 340.) then begin
            if ra_cat_new gt ra_ppst then ra_cat_new=ra_cat_new-360. else ra_cat_new=ra_cat_new+360
        endif
        H=(ra_cat_new-ra_ppst)*cos(dec_ppst*!dtor) ;Star Tracker H,V stars coord
        V=dec_cat_new-dec_ppst

        ;Checks if star in FoV and if it's at the border
        
        if abs(H) le 4.0 and abs(V) le 4.0 then begin
           prvar='H= '+strmid(strtrim(string(H),2),0,5)+'  V= '+strmid(strtrim(string(V),2),0,5)+'   Mag='+strmid(strtrim(string(mag_cat(inccat)),2),0,5)+'    ID: '+strtrim(string(id_cat(inccat)),2)
           print,prvar
           radecdens=radecdens+1 ; That is if the star is in the field of view
           if  abs(H) gt 3.75 or abs(V) gt 3.75 then border=border+1
        endif
        
        ;if abs(H) ge 4.0 or abs(V) ge 4.0 then begin
        ;    print,'!!!Star just outside field of view!!!   H= ',H,'  V=',V,'   Mag=',mag_cat(inccat),'    ID:',id_cat(inccat)
        ;    print,ra_cat(inccat),dec_cat(inccat)
        ;endif

    endif
endfor

;If problems, print warning

if (radecdens-border le 2) then print,'WARNING!!!  Stars at border causing problems!!! Not enough stars in FoV'
print,'Number of stars within pointing 6 degrees / in star tracker Field of View:  ',radec6deg,radecdens

if (radecdens-border le 4) then begin
   read,doy,prompt='Enter day of the year: '
   spawn,'find ~swift/stkproducts/STK_EPH_2007* > ephfile.txt'
   readcol,'ephfile.txt',nomeeph,format='(a)',/silent
   ephpick=nomeeph(n_elements(nomeeph)-1)
   spawn,'rm ephfile.txt'
   res=strsplit(strtrim(string(doy),2),'.',/extract)
   print,'Running roll_range.py....'
   rollcomando='roll_range.py '+ephpick+' 2007 '+res[0]+' '+strtrim(string(ra_ppst),2)+' '+strtrim(string(dec_ppst),2)+' > roll_log.txt'
   spawn,rollcomando
   readcol,'roll_log.txt',roll_allowed,format='(f)',/silent
   spawn,'rm roll_log.txt'
   minroll=min(roll_allowed[0:n_elements(roll_allowed)-2])
   maxroll=max(roll_allowed[0:n_elements(roll_allowed)-2])
   rolluse=253.0-minroll
   roll_stars=intarr(round(maxroll-minroll)+1)
   roll_array=fltarr(round(maxroll-minroll)+1)
   for cont_roll=0,round(maxroll-minroll) do begin
      radec6deg=0
      radecdens=0
      border=0
      for inccat=0l,n_elements(ra_cat)-1 do begin
         res = sphdist(ra_cat(inccat), dec_cat(inccat), ra_ppst, dec_ppst,/degrees) ;angles in DEGREES
         if res lt 6.0 then begin ; if star closer than 6.0 degrees check if it's in the 8x8 camera field of view
            radec6deg=radec6deg+1
            rotate_roll,ra_ppst,dec_ppst,ra_cat(inccat),dec_cat(inccat),rolluse*!dtor,ra_cat_new,dec_cat_new
                                ;stop
                                ;Get Star Tracker H and V coordinates after rotation
            
                                ;Here I do a simple check if there is
                                ;a situation like this: RA_PPST+359.8
                                ;and RA_CAT=0.5, then the two sources
                                ;are very close but H=359!!! not real!!
            if (abs(ra_cat_new-ra_ppst) gt 340.) then begin
               if ra_cat_new gt ra_ppst then ra_cat_new=ra_cat_new-360. else ra_cat_new=ra_cat_new+360
            endif
            H=(ra_cat_new-ra_ppst)*cos(dec_ppst*!dtor) ;Star Tracker H,V stars coord
            V=dec_cat_new-dec_ppst
            
        ;Checks if star in FoV and if it's at the border
            
            if abs(H) le 4.0 and abs(V) le 4.0 then begin
               prvar='H= '+strmid(strtrim(string(H),2),0,5)+'  V= '+strmid(strtrim(string(V),2),0,5)+'   Mag='+strmid(strtrim(string(mag_cat(inccat)),2),0,5)+'    ID: '+strtrim(string(id_cat(inccat)),2)
               print,prvar
               radecdens=radecdens+1 ; That is if the star is in the field of view
               if  abs(H) gt 3.75 or abs(V) gt 3.75 then border=border+1
            endif
            
                                ;if abs(H) ge 4.0 or abs(V) ge 4.0 then begin
        ;    print,'!!!Star just outside field of view!!!   H= ',H,'  V=',V,'   Mag=',mag_cat(inccat),'    ID:',id_cat(inccat)
        ;    print,ra_cat(inccat),dec_cat(inccat)
        ;endif

         endif
      endfor
      print,'Roll: ',+strtrim(string(minroll),2)+' Stars in FoV : ',strtrim(string(radecdens-border),2) 
      rolluse=rolluse+1.
      roll_array(cont_roll)=minroll
      minroll=minroll+1
      roll_stars(cont_roll)=radecdens-border
   endfor 
   
   maxstars=max(roll_stars)
   indexma=where(roll_stars eq maxstars)
   rangemin=min(roll_array(indexma))
   rangemax=max(roll_array(indexma))
   print,'Best Roll Angle range: '+strtrim(string(rangemin),2)+'- ',strtrim(string(rangemax),2)
   
endif

end

