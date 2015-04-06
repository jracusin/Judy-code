@fit_functions
@fit_functions_flares
pro check_cand,grb

  cd,'~/GRBs/'

  cd,grb
  
  lcfile='lc_fit_out_idl_int9.dat'
;  if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
  if exist(lcfile) then begin 
     read_lcfit,lcfile,pnames,p,lc=lc
     dela=[lc.pow1-lc.pow2,lc.pow2-lc.pow3,lc.pow3-lc.pow4,lc.pow4-lc.pow5]
     pow=[lc.pow1,lc.pow2,lc.pow3,lc.pow4,lc.pow5]
     powerr=[lc.pow1err[0],lc.pow2err[0],lc.pow3err[0],lc.pow4err[0],lc.pow5err[0]]/1.4
     delaerr=abs(sqrt((powerr[0:3])^2+(powerr[1:*])^2))
     tb=[lc.break1,lc.break2,lc.break3,lc.break4]
     w=where(dela le -1.5 and pow[0:3] ne 0 and pow[1:*] ne 0 and abs(dela)-delaerr*2.56 gt 1.5,nw)

     q=where(pow ne 0,nq)
     print,grb
     print,'alpha = ',pow[q]
     print,'alphaerr = ',powerr[q]
     print,'tbreak = ',tb[q[0:nq-2]]
     print,'delta alpha = ',dela[q[0:nq-2]]
     print,'delta alpha err= ',delaerr[q[0:nq-2]]
  endif 
  stop

return
end 

pro steep_candidates

  cd,'~/GRBs/'
  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  wgrbs=''
  t3=5.*86400.
  t6=10.*86400.
  f3=fltarr(n)
  f6=fltarr(n)
  for i=0,n_elements(grbs)-1 do begin

     cd,grbs[i]
;     print,grbs[i]

     lcfile='lc_fit_out_idl_int9.dat'
;     if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
     if exist(lcfile) then begin 
        read_lcfit,lcfile,pnames,p,lc=lc
        dela=[lc.pow1-lc.pow2,lc.pow2-lc.pow3,lc.pow3-lc.pow4,lc.pow4-lc.pow5]
        pow=[lc.pow1,lc.pow2,lc.pow3,lc.pow4,lc.pow5]
        powerr=[lc.pow1err[0],lc.pow2err[0],lc.pow3err[0],lc.pow4err[0],lc.pow5err[0]]/1.4
        delaerr=abs(sqrt((powerr[0:3])^2+(powerr[1:*])^2))
        tb=[lc.break1,lc.break2,lc.break3,lc.break4]
;        w=where(dela le -1.5 and pow[0:3] ne 0 and pow[1:*] ne 0 and abs(dela)-delaerr*2.56 gt 1.5,nw)
        w=where(dela le -1.5 and pow[0:3] ne 0 and pow[1:*] ne 0 and abs(dela)-delaerr*1.5 gt 1.5,nw)
        ;;; 1.5 is arbitrary
        if strtrim(pnames[0],2) ne 'nofit' then begin
           mo=fit_models(pnames,p)
           if exist('UL_specfits.fits') and mo ne '' then begin 
              spec=mrdfits('UL_specfits.fits',1,/silent)
              corr=spec[n_elements(spec)-1].unabs_cfratio
              tmp=execute('f3[i]='+mo+'(t3,p)*corr')
              tmp=execute('f6[i]='+mo+'(t6,p)*corr')
           endif 
           if nw gt 0 then begin 
              if abs(pow[w[0]]) lt 0.9 and tb[w[0]] gt 1500 then begin 
                 q=where(pow ne 0,nq)
                 print,grbs[i]
                 print,'alpha = ',pow[q]
                 print,'alphaerr = ',powerr[q]
                 print,'tbreak = ',tb[q[0:nq-2]]
                 print,'delta alpha = ',dela[q[0:nq-2]]
                 print,'delta alpha err= ',delaerr[q[0:nq-2]]
                 wgrbs=[wgrbs,grbs[i]]
              endif 
           endif 
        endif 
;if grbs[i] eq 'GRB131128A' then stop
     endif 
     cd,'~/GRBs/'

  endfor 
  wgrbs=wgrbs[1:*]
  help,wgrbs

  w3=where(f3 gt 0 and f3 lt 1)
  w6=where(f6 gt 0 and f6 lt 1)

  cd,'~/proposals/Chandra_cycle17/'
  begplot,name='flux_dist_5_10_days.ps',/land,font='helvetica'
  !p.multi=[0,2,1]
  plothist,alog10(f3[w3]),bin=0.1,xrange=[-16,-10],xtitle='log flux @ 5 days',yrange=[0,60]
  med=median(alog10(f3[w3]))
  std=stddev(alog10(f3[w3]))
  oplot,[med,med],[0,100],line=2
  oplot,med+[std,std],[0,100],line=1
  oplot,med-[std,std],[0,100],line=1
  legend,['median='+ntostr(10^med)],box=0,/top,/right

  plothist,alog10(f6[w6]),bin=0.1,xrange=[-16,-10],xtitle='log flux @ 10 days',yrange=[0,60]
  med=median(alog10(f6[w6]))
  std=stddev(alog10(f6[w6]))
  oplot,[med,med],[0,100],line=2
  oplot,med+[std,std],[0,100],line=1
  oplot,med-[std,std],[0,100],line=1
  legend,['median='+ntostr(10^med)],box=0,/top,/right

  !p.multi=0
  endplot
  spawn,'ps2pdf flux_dist_5_10_days.ps'
  stop

  return
end 

pro max_chandra

  ;;; make plots for Max's Chandra cycle 17 proposal

  cd,'~/GRBs/'
;  maxgrbs=['GRB050730','GRB051117A','GRB060218','GRB060413','GRB060607A','GRB070110','GRB070311','GRB081029','GRB100901A','GRB111209A','GRB120326A','GRB130831A']
  ;;; disregarding 060218 because it's weird, and 100901A
  ;;; because it looks like it has a later JB
  type1=['GRB051117A','GRB070110','GRB130831A','GRB141121A'] ;;; has flattening
  type2=['GRB050730','GRB060413','GRB060607A','GRB061202','GRB070311','GRB081029','GRB100219A','GRB100508A','GRB111229A','GRB120326A','GRB131128A'] ;;; just steep

;  nm=n_elements(maxgrbs)
  n1=n_elements(type1)
  n2=n_elements(type2)
  grbs=file_search('GRB*/')
  n=n_elements(grbs)

  com=['plotsym,0,1,/fill',$       
;       'oplot,lc[n_elements(lc)-2:*].time*z1,lc[n_elements(lc)-2:*].src_rate*f,color=!orange,psym=8',$
       'oplot,[9.*86400.,9.*86400],[1e-15,1e-5],line=2',$
       'oplot,[16.*86400.,16.*86400],[1e-15,1e-5],line=2',$
       "xyouts,1.8e5,1e-10,'9 days',/data",$
       "xyouts,1.5e6,1e-10,'16 days',/data"]

  com2=['addmo=fit_models(pnames,p,np2,/addbreak,basemo=basemo2)',$
        'xw=[2,3,3,3,3,2,3,3,3,3,3]',$
        'yfit2=call_function(basemo2,lc[n_elements(lc)-3:*].time*z1,[p[0:np2-1],lc[n_elements(lc)-3].time*z1,1.2])',$
        'oplot,lc[n_elements(lc)-3:*].time*z1,yfit2*f,color=color[i],line=2']


  plot_all_xrt_lcs,overgrb=type1,/flux,add='max_t1_swift_grb_',overcom=com[0:4],dir='~/proposals/Chandra_cycle17/Max/',/overdata,/lastthick;,/nounder
  lc=mrdfits('~/GRBs/GRB130831A/UL_lc_chandra.fits',1)
  spec=mrdfits('~/GRBs/GRB130831A/UL_specfits.fits',1)
  plots,lc.time,lc.src_rate*spec[n_elements(spec)-1].unabs_cfratio,color=!green,psym=8
  oplot,[lc.tstart,lc.tstop],[lc.src_rate,lc.src_rate]*spec[n_elements(spec)-1].unabs_cfratio,color=!green

  plot_all_xrt_lcs,overgrb=type2,/flux,add='max_t2_swift_grb_',overcom=com[1:*],dir='~/proposals/Chandra_cycle17/Max/',/overdata;,/nounder;,overgcom=com2


  stop
  maxgrbs=[type1,type2]
  maxgrbs=maxgrbs[sort(maxgrbs)]
  nm=n_elements(maxgrbs)

  day=86400d
  for i=0,nm-1 do begin
     cd,maxgrbs[i]
     
     lc=lcout2fits(/silent)
     spec=mrdfits('UL_specfits.fits',1)
     lcfile='lc_fit_out_idl_int9.dat'
;     if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
     read_lcfit,lcfile,pnames,p,lc=lc
     mo=fit_models(pnames,p,basemo=basemo,np)
     wb=where(p[0:np-1] gt 1.5 and p[0:np-1] lt 10,nwb)
     tb=p[wb[nwb-1]-1]
     tmp=execute('yfit='+basemo+'(tb+5*day,p)')

     print,maxgrbs[i],tb,p[wb[nwb-1]],tb+5*day,yfit*spec[n_elements(spec)-1].cfratio

     cd,'~/GRBs/'
  endfor 





stop
  begplot,name='~/proposals/Chandra_cycle17/Max/max_swift_grb_lc_compare_flux.ps',/color,/land,font='helvetica',/encap
  colors=[!blue,!cyan,!green,!red,!orange,!sienna,!salmon,!magenta,!forestgreen,!purple,!dodgerblue]

  plot,[10,1e8],[1e-15,1e6],/nodata,/xlog,/ylog,xtitle='Time Since Trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-1!N s!U-1!N)',charsize=2,yrange=[1e-15,1e-6],/ysty,yminor=9,yticks=9,xrange=[10,1e8],/xsty,xminor=9

  match,latgrbs,grbs,m1,m2
  dont_match,latgrbs,grbs,dm1,dm2

  lat=grbs[m2]
  nlat=n_elements(lat)
  ogrbs=grbs[dm2]
  for i=0,n_elements(ogrbs)-1 do begin

     cd,ogrbs[i]
     print,ogrbs[i]
     
     color=!grey50
     lc=lcout2fits(/silent)
     if exist('UL_specfits.fits') then begin 
        spec=mrdfits('UL_specfits.fits',1)
        corr=spec[n_elements(spec)-1].unabs_cfratio
;corr=1.
        if n_elements(lc) gt 1 then begin 
           det=where(lc.src_rate_err ne 0,ndet)

           if ndet gt 2 then oplot,lc[det].time,lc[det].src_rate*corr,color=color
;        oploterror,lc[det].time,lc[det].src_rate,lc[det].src_rate_err,/nohat,errcolor=color,psym=3
;           for j=0,ndet-1 do begin 
;              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*corr,color=color
;              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*corr,color=color
;           endfor 

        endif 
     endif 
     cd,'..'
  endfor 
  for i=0,nlat-1 do begin

     cd,lat[i]
     print,lat[i]

     wlat=where(lat[i] eq latgrbs,nwlat)
     color=colors[wlat[0]]
     lc=lcout2fits(/silent)
     if exist('UL_specfits.fits') then begin 
        spec=mrdfits('UL_specfits.fits',1)
        corr=spec[n_elements(spec)-1].unabs_cfratio
;corr=1.
        if n_elements(lc) gt 1 then begin 
           det=where(lc.src_rate_err ne 0,ndet)
           oplot,lc[det].time,lc[det].src_rate*corr,color=color
           
;        oploterror,lc[det].time,lc[det].src_rate,lc[det].src_rate_err,/nohat,errcolor=color,psym=3

           for j=0,ndet-1 do begin 
              oplot,[lc[det[j]].time,lc[det[j]].time],[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*corr,color=color
              oplot,[lc[det[j]].tstart,lc[det[j]].tstop],[lc[det[j]].src_rate,lc[det[j]].src_rate]*corr,color=color
           endfor
        endif 
     endif 

     cd,'..'
  endfor 

  oplot,[5.*86400.,5.*86400],[1e-15,1e-5],line=2
  oplot,[10.*86400.,10.*86400],[1e-15,1e-5],line=2
  xyouts,7e4,2e-15,'8 days',/data
  xyouts,1e6,2e-15,'15 days',/data


  legend,[lat],textcolor=[colors[0:nlat-1]],/top,/right,box=0,charsize=1.5

  endplot

  cd,'~/proposals/Chandra_cycle17/Max/'
  spawn,'ps2pdf max_swift_grb_lc_compare_flux.ps'


return
end 
