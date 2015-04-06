pro write_qdp_lc,file=file,name=name,comment=comment
  
  if n_elements(name) eq 0 then name=''
  if n_elements(comment) eq 0 then comment=''
  
  lc=lcout2fits(file)
  openw,lunout,'plot.qdp',/get_lun
  printf,lunout,'cpd /xs'
  printf,lunout,'READ SERR 1 2'
  printf,lunout,'!WT'
  wwt=where(lc.type eq 0 and lc.src_rate_err gt 0.,nwt)
  wpc=where(lc.type eq 1 and lc.src_rate_err gt 0.,npc)
  wul=where(lc.src_rate_err lt 0.,nul)
  if nul gt 0 then begin 
     lc[wul].src_rate_err=lc[wul].src_rate*0.99
  endif 
  
;  xrange=prange(lc.time,(lc.tstop-lc.tstart)/2.)
;  yrange=prange(lc.src_rate,lc.src_rate_err)
  
  xrange=[min(lc.tstart),max(lc.tstop)]
  logxr=alog10(xrange)
  xrange=[10.^(round(logxr[0]-0.5)),10.^(round(logxr[1]+0.5))]
  
  yrange=[min(lc.src_rate-lc.src_rate_err),max(lc.src_rate+lc.src_rate_err)]
  logyr=alog10(yrange)
  yrange=[10.^(round(logyr[0]-0.5)),10.^(round(logyr[1]+0.5))]
  if yrange[0] lt 1e-4 then yrange[0]=1e-4
  
  time=ntostr(lc.time)
  timewidth=ntostr((lc.tstop-lc.tstart)/2.)
  ctrate=ntostr(lc.src_rate)
  ctrate_err=ntostr(lc.src_rate_err)
  
  if nwt gt 0 then begin 
     printf,lunout,time[wwt]+' '+timewidth[wwt]+' '+ctrate[wwt]+' '+ctrate_err[wwt]
     printf,lunout,'no no'
  endif
     
  printf,lunout,'!PC'
  if npc gt 0 then printf,lunout,time[wpc]+' '+timewidth[wpc]+' '+ctrate[wpc]+' '+ctrate_err[wpc]
  printf,lunout,'no no'
  
  printf,lunout,'!UPPER LIMIT'
  if nul gt 0 then printf,lunout,time[wul]+' '+timewidth[wul]+' '+ctrate[wul]+' '+ntostr(0.);+ctrate_err[wul]
  
  printf,lunout,'skip on'
  printf,lunout,'cpd /xs'
  printf,lunout,'scr white'
  printf,lunout,'log'
  printf,lunout,'la x time since BAT trigger (s)'
  printf,lunout,'la y Count Rate (0.3-10.0 keV) (s\u-1\d)'
  printf,lunout,'la f  blue WT - red PC'
  printf,lunout,'la t '+name
  printf,lunout,'r y '+ntostr(yrange[0])+' '+ntostr(yrange[1])
  printf,lunout,'r x '+ntostr(xrange[0])+' '+ntostr(xrange[1])
  printf,lunout,'la 1 ""'+comment+'"" vp 0.8 0.018 cs 1'
  printf,lunout,'time off'
  printf,lunout,'ma 17 on 1'
  if nwt gt 0 then begin 
     printf,lunout,'ma 17 on 2'
  
     printf,lunout,'ma 31 on 3   !arrow symbol on eventual upper limit'
     printf,lunout,'ma size 3 on 3'

     printf,lunout,'col 11 on 1   !blue on WT data'
     printf,lunout,'col 2 on 2   !red on PC data'
  endif else begin 
     printf,lunout,'ma 31 on 2   !arrow symbol on eventual upper limit'
     printf,lunout,'ma size 4 on 2'
     printf,lunout,'col 2 on 2 1   !red on PC data'
  endelse 
  printf,lunout,'col 2 on 3   !red on eventual upper limit'
  printf,lunout,'la par off'
  printf,lunout,'log'
  printf,lunout,'csize 1.3'
  printf,lunout,'lw 5'
  printf,lunout,'hardcopy lc_plot.ps/cps'
  printf,lunout,'p'  
    
  
  free_lun,lunout
  
  
  return
end
