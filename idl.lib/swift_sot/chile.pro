pro chile,file,realslew=realslew,afst=afst,onlyas=onlyas,lite=lite,antisun=antisun, $ 
          onlyat=onlyat,everyslew=everyslew,ephfile=ephfile,$
          help=help,ephem=eph,safepoint=safepoint,nogetafst=nogetafst,$
          check_momentum_off=check_momentum_off,onlymoon=onlymoon
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CHILE - Constraint Helper Interface for Limiting Entrapment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  if keyword_set(help) then begin
;     print,'syntax - chile,[/realslew,/afst,/onlyas,/lite,/antisun,/onlyat,/addat,/safepoint,/help]'
     print,'syntax - chile,[/realslew,/afst,/onlyas,/safepoint,/help]'
     return
  endif 
  
  doskip=0
  simpctable
  ;;need to write new program to read in light PPST or modify read_ppst
  
  if not keyword_set(afst) then begin 
     read_ppst,file,ppst,/grab
     afst=0
  endif else begin
     if not keyword_set(nogetafst) then begin 
        dfile='datefile.dat'
        olddate='e.g. 2005353'
        if exist(dfile) then begin 
           readcol,dfile,olddate,format='(a)' 
           date=olddate
        endif else date=''
        read,'What date ('+olddate+')? ',date
        if date eq '' then date=olddate
        if date ne olddate then writecol,dfile,date
        if keyword_set(realslew) then partial=1
        get_afst,date,file,/only,partial=partial
     endif 
     read_afst,file,ppst,/includeslew
  endelse 
  
  ;;chile lite!
  if keyword_set(lite) then begin 
     jump:
     find_eph,file,ephfile
     print,ephfile
     add=''
     donethat=0
     if n_elements(str_sep(file,'.out')) ne 2 and not afst then begin 
;       goto,skipadd
        read,add,prompt='Add new AT (y/n)? '
        if add eq 'y' then begin
           atfile='atfile.fits'
           if exist(atfile) then at=mrdfits(atfile,1) else $
              at=create_struct('ra',0d,'dec',0d,'tstart','','dur',60000L,'merit',100)
           begdate=met2date(round(date2met(ppst[0].begdate)-120))
           enddate=ppst[n_elements(ppst)-1].enddate
           if n_elements(eph) eq 0 and strpos(ephfile,'interp') eq -1 then begin
              print,'Interpolating STK file to 10 second resolution'
              
              interp_eph,ephfile,begdate,enddate,6,stkfile,ieph=eph
           endif else stkfile=ephfile

           print
           print,'What are the properties of the New AT?'
           input,'RA',ra,at.ra
           input,'Dec',dec,at.dec
           input,'Tstart',tstart,at.tstart
           input,'Duration',dur,at.dur
           input,'Merit',mer,at.merit
           
           at.ra=ra
           at.dec=dec
           at.tstart=tstart
           at.dur=dur
           at.merit=mer
           
           com='ppst_overlay.py '+stkfile+' '+file+' '+ntostr(ra)+' '+ntostr(dec)+' '+$
              tstart+' '+ntostr(dur)
           print,com
           spawn,com
           print
           
           addat=1
           file=file+'.out'
           print,file
           read_ppst,file,ppst
           ephfile=stkfile
           donethat=1
           mwrfits,at,atfile,/create
        endif else addat=0
        skipadd:
     endif 
     if n_elements(str_sep(file,'.out')) eq 2 and not donethat or afst then begin
        begdate=met2date(date2met(ppst[0].begdate)-120)
        enddate=ppst[n_elements(ppst)-1].enddate
        print,'Interpolating STK file to 10 second resolution'
        interp_eph,ephfile,begdate,enddate,6,stkfile,ieph=eph
        if ephfile eq stkfile then begin
           spawn,'rm '+ephfile
           goto,jump
        endif 
        ephfile=stkfile
        
     endif
     ans=''
     read,ans,$
        prompt='Run in most reduced mode (only AT slews, discard anti-sun slews, only traps) y/n? '
     if ans eq 'n' then everyslew=1 else begin
        w=where(ppst.targname eq 'TOO/AT',nw)
        if nw gt 0 then onlyat=1
     endelse 
     
     chile_light,file,ppst,antisun=antisun,onlyat=onlyat,everyslew=everyslew,afst=afst,$
        realslew=realslew,ephfile=ephfile,ephem=eph
  endif else begin              ;chile not lite
     ppst2radec,file,outlines=outlines, outpts=outpts,afst=afst,includeslew=includeslew,/grablast
     map_viewer,/auto, linefile=outlines, pointname=outpts,doskip=doskip,ppst=ppst,$ 
                realslew=realslew,onlyas=onlyas,safepoint=safepoint,$
                check_momentum_off=check_momentum_off,onlymo=onlymoon
     spawn,'rm '+outlines
     spawn,'rm '+outpts
;     ppst_slewdist,file
  endelse 
  
  
  return
end 
