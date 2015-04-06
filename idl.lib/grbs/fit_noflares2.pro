pro user_gtis,w,tstarts,tstops,_extra=_extra,part=part,flares=flares,noplot=noplot,zoom=zoom,lc=lc,flux=flux
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise

  clr=!red;!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  if not keyword_set(noplot) then begin
;     if not keyword_set(zoom) then plot_base_lc,_extra=_extra else $
;        plot_base_lc
     if not keyword_set(zoom) then plot_like_qdp,lc=lc,_extra=_extra,flux=flux else $
        plot_like_qdp,lc=lc,_extra=_extra,flux=flux
  endif 
  
  plotsym,0,1.0,/fill
  x=0. & y=0. & i=1 & tstarts=0. & tstops=0.
  !mouse.button=0

  while (!MOUSE.button NE 4) DO BEGIN 
     if keyword_set(flares) then begin 
        if i mod 4 eq 1 or i mod 4 eq 2 then begin 
           rise=1
           fall=0
           insert='rise of ' 
        endif else begin 
           insert='decay of '
           rise=0
           fall=1
        endelse 
     endif else insert=''
     if (i lt 3 and not keyword_set(flares)) or (keyword_set(flares) and i lt 5) then begin 
;        print,i
        if i mod 2 eq 1 then print,'LEFT click on START of '+insert+'GTI for '+part
        if i mod 2 eq 0 then print,'LEFT click on STOP of '+insert+'GTI for '+part
     endif else $
        print,'Repeat on other GTIs or RIGHT click when completed'
     cursor,xx,yy,/wait,/change

     if !MOUSE.button NE 4 then begin 
        if i mod 2 eq 1 then color=!magenta else color=!yellow
        
        oplot,[xx,xx],[1e-20,1e4],color=color
        print,'***click*** ',xx,yy
;        printf,llun,'***click*** ',xx,yy
        x=[x,xx]
        y=[y,yy]

        if i mod 2 eq 0 or (keyword_set(flares) and i mod 4 eq 0) then begin 
           if i eq 2 then wsmt='time gt '+ntostr(x[i-1])+' and time lt '+ntostr(x[i]) else begin
              wsmt=wsmt+' or time gt '+ntostr(x[i-1])+' and time lt '+ntostr(x[i])
           endelse 
           print,wsmt
           com='w=where('+wsmt+')'
           tmp=execute(com)
;           stop
;           if not keyword_set(flares) then begin
;              oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=clr,errcolor=clr,/nohat 
;              wpc=where(type[w] eq 1,nwpc)
;              if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=clr
;           endif else begin 
                            
;              if rise then begin 
;                 oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!blue,errcolor=!blue,/nohat
;                 nw=n_elements(w)
;                 wpc=where(type[w] eq 1,nwpc)
;                 if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!blue
;              endif 
;              
;              if fall then begin 
;                 wq=w[nw:*]
;                 oploterror,time[wq],cts[wq],timerr[wq],err[wq],psym=3,color=!green,errcolor=!green,/nohat
;                 wpc=where(type[wq] eq 1,nwpc)
;                 if nwpc gt 0 then oplot,time[wq[wpc]],cts[wq[wpc]],psym=8,color=!green
;              endif 
;           endelse 
           tstops=[tstops,xx]
        endif else tstarts=[tstarts,xx]
     endif 
     if keyword_set(flares) and i eq 4 then !mouse.button=4
     i=i+1
  endwhile
  x=x[1:*]
  y=y[1:*]
  tstarts=tstarts[1:*]
  tstops=tstops[1:*]
  
  return
end

pro plot_base_lc,w,_extra=_extra,ps=ps
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill

  if n_elements(xrange) eq 0 then xrange=[min(time-timerr),max(time+timerr)]
  if n_elements(yrange) eq 0 then begin 
      w=where(finite(err))
      yrange=[min(cts[w]-err[w]),max(cts[w]+err[w])]
     if yrange[0] lt 1e-5 and n_elements(flux) eq 0 then yrange[0]=1e-5
  endif   
;  if n_elements(yrange) eq 0 then yrange=[min(cts-err),max(cts+err)]
;  xrange=[min(time-timerr),max(time+timerr)]
  
  plot,xrange,yrange,/nodata,xtitle='seconds since BAT trigger',ytitle='cts/s',/xlog,/ylog,yrange=yrange,_extra=_extra
  
  wt=where(type eq 0,nwt)
  if nwt gt 0 then $
     oploterror,time[wt],cts[wt],timerr[wt],err[wt],psym=3,/nohat;,color=!blue,errcolor=!blue
  
  pc=where(type eq 1,npc)
  if npc gt 0 then $
     oploterror,time[pc],cts[pc],timerr[pc],err[pc],psym=8,/nohat;,color=!red,errcolor=!red
  
  if n_elements(w) gt 0 then begin
     pc=where(type[w] eq 1,npc)
     wt=where(type[w] eq 0,nwt)
     color=!red
     if nwt gt 0 then $
        oploterror,time[w[wt]],cts[w[wt]],timerr[w[wt]],err[w[wt]],psym=3,/nohat,color=color,errcolor=color
     if npc gt 0 then $
        oploterror,time[w[pc]],cts[w[pc]],timerr[w[pc]],err[w[pc]],psym=8,/nohat,color=color,errcolor=color
  endif 
  
  return 
end 

pro fit_noflares2,file,outfile,small=small,battime=battime,siglim=siglim,lc=lc,phil=phil,qdp=qdp,_extra=_extra,gtifile=gtifile,flux=flux
  
  if keyword_set(help) then begin 
     print,'syntax - fit_flares,file,outfile'
     return
  endif 

;  if not keyword_set(phil) then begin 
;     if n_elements(file) eq 0 then file='lc_newout.txt'
;     if n_elements(outfile) eq 0 then outfile='lc_newout_noflares.txt'
;  endif
  phil=1
  if keyword_set(phil) then begin
     if exist('WTCURVE.qdp') then spawn,'cat WTCURVE.qdp PCCURVE.qdp > CURVE.qdp' else $
        spawn,'cp PCCURVE.qdp CURVE.qdp'
     if n_elements(file) eq 0 then file='CURVE.qdp'
     if n_elements(outfile) eq 0 then outfile='CURVE_noflares.qdp'
  endif 
  if keyword_set(qdp) then begin
     outfile=file+'_noflares.qdp'
  endif 
     
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  simpctable
  defsymbols
  
;  if keyword_set(small) then window,0,xsize=900,ysize=600 else window,0,xsize=1300,ysize=800
  
  k='n'
;  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,junk1,type
  if n_elements(lc) eq 0 then lc=lcout2fits(file,phil=phil,qdp=qdp)
  time=lc.time
  tstarted=lc.tstart
  tstoped=lc.tstop
  cts=lc.src_rate
  err=lc.src_rate_err
  type=lc.type
  bg=lc.tot_back_cts
  src=lc.src_counts
  sig=lc.det_sig
  expt=lc.exptime
  sigma=src/sqrt(src+bg*2)
  wdet=where(err ne 0)
  
  timerr=((time-tstarted)+(tstoped-time))/2.
  type=fix(type)
  w0=where(cts gt 0 and finite(err) and err ne 0)
  time=time[w0] & timerr=timerr[w0] & cts=cts[w0] & err=err[w0] & type=type[w0]
  back=bg                       ;*expt
 
  timerr=((time-tstarted)+(tstoped-time))/2.
  r=0
  lc=lc[w0]
  print,'Select GTIs of Flares to remove'

  user_gtis,w,tstarts,tstops,_extra=_extra,part='Flare '+ntostr(r+1),noplot=noplot,zoom=zoom,lc=lc,flux=flux

  i=intarr(n_elements(time)+2)
  i[w+2]=1  ;;;;;;; 3 OR 2?????????????
  w2=where(i eq 0)

  readcol,file,lines,format='(a)',delim='&'

  wl=where(lines ne '')
;  lines=lines[wl]
;  lines=lines[w0]

  writecol,outfile,lines[w2],header=['time    t_start    t_stop   src_rate   src_rate_err   tot_hard   tot_hard_err   exptime    src_counts  back_ctrate   det_sig   psf_corr   junk   junk   rate1   rate2   rate3  rate1_err   rate2_err    rate3_err   hard1   hard2   hard1_err   hard2_err src_rate   back_rate   back_area_corr   pu_corr    psf_corr  somejunk  e1_cts  e2_cts  e3_cts  somejunk somejunk somejunk tot_back_cts  hard_ratio_1  hard_ratio_2  hard_ratio_err1  hard_ratio_err2 mean_time sigma_time','junk2']
  
  print,'Writing out '+outfile
  ;;select flare
  ;;write out lcout

;  if n_elements(battime) ne 0 then $
;    writecol,'flares_gtis.dat',tstarts+battime,tstops+battime
  
  if n_elements(gtifile) eq 0 then gtifile='flares_gtis.dat'
  writecol,gtifile,tstarts,tstops
;  lc=lc[w2[2:*]]
;  lc=lc[w]

  lc=lc[w2[2:*]-2]

  return
end 
