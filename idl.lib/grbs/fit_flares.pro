pro write_outfile,tstart,tstop,exp_fact,pufact,ultstarts,ultstops,norm2,pow2,sigma2,chisq2,dof2,tpeak,peak,tpeak1,peak1,increase,increase1,signif,flag,file,ulratio,medtime,latespec=latespec
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  
  file=''
  read,prompt='Output file name (default flare.out)? ',file
  if file eq '' then file='flare.out'
  
  openw,lun,file,/get_lun
  s=' '
  printf,lun,'TSTART '+ntostr(tstart)
  printf,lun,'TSTOP '+ntostr(tstop)
  printf,lun,'EXP_FACT '+ntostr(exp_fact)
  printf,lun,'PUFACT '+ntostr(pufact)
  ults1='' & ults2=''
  for i=0,n_elements(ultstarts)-1 do begin
     ults1=ults1+s+ntostr(ultstarts[i])
     ults2=ults2+s+ntostr(ultstops[i])
  endfor 
  printf,lun,'UL_TSTARTS'+ults1
  printf,lun,'UL_TSTOPS'+ults2
  printf,lun,'UL_NORM '+ntostr(ulnorm)+s+ntostr(ulsigma[0])
  printf,lun,'UL_POW '+ntostr(ulpow)+s+ntostr(ulsigma[1])
  printf,lun,'UL_CHISQ/DOF '+ntostr(ulchisq)
  printf,lun,'UL_DOF '+ntostr(uldof)
  printf,lun,'RISE_NORM '+ntostr(norm2[0])+s+ntostr(sigma2[0,0])
  printf,lun,'RISE_POW '+ntostr(pow2[0])+s+ntostr(sigma2[1,0]);[0,1])
  printf,lun,'RISE_CHISQ/DOF '+ntostr(chisq2[0])
  printf,lun,'RISE_DOF '+ntostr(dof2[0])
  printf,lun,'DECAY_NORM '+ntostr(norm2[1])+s+ntostr(sigma2[0,1]);[1,0])
  printf,lun,'DECAY_POW '+ntostr(pow2[1])+s+ntostr(sigma2[1,1])
  printf,lun,'DECAY_CHISQ/DOF '+ntostr(chisq2[1])
  printf,lun,'DECAY_DOF '+ntostr(dof2[1])
  printf,lun,'RESET_FLAG '+ntostr(flag[0])+' '+ntostr(flag[1])
  printf,lun,'INTERCEPT_TPEAK '+ntostr(tpeak)
  printf,lun,'INTERCEPT_PEAK '+ntostr(peak)
  printf,lun,'INTERCEPT_INCREASE '+ntostr(increase)
  printf,lun,'MAX_TPEAK '+ntostr(tpeak1)
  printf,lun,'MAX_PEAK '+ntostr(peak1)
  printf,lun,'MAX_INCREASE '+ntostr(increase1)
  printf,lun,'SIGNIF '+ntostr(signif)
  printf,lun,'ULRATIO '+ntostr(ulratio)  
  if latespec then begin
     printf,lun,'UL_MEAN_TIME '+ntostr(medtime)
     printf,lun,'UL_NORM2 '+ntostr(ulnorm2)+s+ntostr(ulsigma2[0])
     printf,lun,'UL_POW2 '+ntostr(ulpow2)+s+ntostr(ulsigma2[1])
     printf,lun,'UL_CHISQ2/DOF2 '+ntostr(ulchisq2)
     printf,lun,'UL_DOF2 '+ntostr(uldof2)
  endif 

;  printf,lun,' '+ntostr()

  close,lun,/file
  
  return
end 

pro write_parfile,tstart,tstop,exp_fact,pufact,i,ultstarts,ultstops,ulfiles,ffiles,ra,dec,backra,backdec,bat_trig,brad,srad,ultstarts2,ultstops2,outfile,ulratio,medtime
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
;  file='flarespec_flare'+ntostr(i+1)+'.par'
  file=outfile+'.par'
  print,'Writing out spectral script input file: '+file
  printf,llun,'Writing out spectral script input file: '+file
  
  openw,lun,file,/get_lun
  
  if n_elements(ulfiles) ne 0 then printf,lun,'UPL_FNAMES '+ntostrarr(ulfiles) else printf,lun,'UPL_FNAMES '
  if n_elements(ffiles) ne 0 then printf,lun,'FLARE_FNAMES '+ntostrarr(ffiles) else printf,lun,'FLARE_FNAMES '
  if n_elements(ra) ne 0 then printf,lun,'RA '+ntostr(ra) else printf,lun,'RA '
  if n_elements(dec) ne 0 then printf,lun,'DEC '+ntostr(dec) else printf,lun,'DEC '
  if n_elements(bat_trig) ne 0 then printf,lun,'BAT_TRIGGER '+ntostr(bat_trig,format='(f12.2)') else printf,lun,'BAT_TRIGGER '
  if n_elements(backra) ne 0 then printf,lun,'RA_UPL_BACK '+ntostr(backra) else printf,lun,'RA_UPL_BACK none'
  if n_elements(backdec) ne 0 then printf,lun,'DEC_UPL_BACK '+ntostr(backdec) else printf,lun,'DEC_UPL_BACK none'
  if n_elements(brad) ne 0 then printf,lun,'UPL_BACK_RAD '+ntostr(brad) else printf,lun,'UPL_BACK_RAD none'
  if n_elements(backra) ne 0 then printf,lun,'RA_FL_BACK '+ntostr(backra) else printf,lun,'RA_FL_BACK none'
  if n_elements(backdec) ne 0 then printf,lun,'DEC_FL_BACK '+ntostr(backdec) else printf,lun,'DEC_FL_BACK none'
  if n_elements(brad) ne 0 then printf,lun,'FL_BACK_RAD '+ntostr(brad) else printf,lun,'FL_BACK_RAD none'
  if n_elements(ultstarts2) ne 0 then printf,lun,'UPL_GTI_START '+ntostrarr(ultstarts2) else printf,lun,'UPL_GTI_START '+ntostrarr(ultstarts)
  if n_elements(ultstops2) ne 0 then printf,lun,'UPL_GTI_STOP '+ntostrarr(ultstops2) else printf,lun,'UPL_GTI_STOP '+ntostrarr(ultstops)
  printf,lun,'UPL_CANON no'
  printf,lun,'FLARE_GTI_START '+ntostr(tstart)
  printf,lun,'FLARE_GTI_STOP '+ntostr(tstop)
  printf,lun,'UPL_SLOPE '+ntostr(ulpow)
  printf,lun,'UPL_N '+ntostr(ulnorm)
  if n_elements(srad) ne 0 then printf,lun,'SRC_RAD '+ntostr(srad) else printf,lun,'SRC_RAD 30'
  printf,lun,'SRC_IN_RAD none'
  if n_elements(srad) ne 0 then printf,lun,'UPL_RAD '+ntostr(srad) else printf,lun,'UPL_RAD 30'
  printf,lun,'UPL_IN_RAD none'
  printf,lun,'BINNING_UPL 20'
  printf,lun,'BINNING_FLARE 20'
  printf,lun,'CORR_FACT '+ntostr(exp_fact)
  printf,lun,'PU_FACT '+ntostr(pufact)
  printf,lun,'NH_UPL none'
  printf,lun,'NH_FLARE none'
  printf,lun,'ULRATIO '+ntostr(ulratio)

  
  close,lun,/file
  
  
  return
end 

pro make_outplot,i,w,wb,tstarts,tstops,norm2,pow2,sigma2,tstart,tstop,chisq,dof2,tpeak,peak,tpeak1,peak1,increase,increase1,signif,exp_fact,pufact,ulratio,medtime,outfile,_extra=_extra,latespec=latespec,title=title
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  name=outfile+'_lc_fit.ps'
;  begplot,name='flare'+ntostr(i+1)+'_lc_fit.ps',/land,/color
  begplot,name=name,/land,/color
  
;  plot_base_lc,wul,_extra=_extra,/ps
  plot_base_lc,wul,/ps,title=title
  t=[1,time,1e8]
  
  if n_elements(w) ne 0 then oploterror,time[w],cts[w],timerr[w],err[w],/nohat,psym=3,errcolor=!red,color=!red
  color=[!blue,!green]
  plotsym,0,0.7,/fill
;  oplot,time[w],cts[w],psym=8,color=!red
  for j=0,1 do begin 
     q=where(time gt tstarts[j] and time lt tstops[j])
     oploterror,time[q],cts[q],timerr[q],err[q],color=color[j],psym=3,errcolor=color[j],/nohat
     oplot,t,10d^(norm2[j])*t^pow2[j],color=color[j],thick=1.
     wpc=where(curr_ftype[q] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[q[wpc]],cts[q[wpc]],psym=8,color=color[j]
  endfor
  wpc=where(curr_ftype[w] eq 1,nwpc)
  if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
    
  oplot,t,10d^(ulnorm)*t^ulpow,color=!red,thick=1.
  oplot,[1,1e8],[peak,peak],thick=1.
  oplot,[tpeak,tpeak],[1e-6,1e6],thick=1.
  oplot,[1,1e8],[peak1,peak1],line=1,thick=1.
  oplot,[tpeak1,tpeak1],[1e-6,1e6],line=1,thick=1.
  
  if n_elements(wb) gt 1 then begin
     oploterror,time[wb],cts[wb],timerr[wb],err[wb],color=!cyan,psym=3,errcolor=!cyan,/nohat
     oplot,t,10d^(ulnorm2)*t^ulpow2,color=!cyan,thick=1.
     wpc=where(curr_ftype[wb] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[wb[wpc]],cts[wb[wpc]],psym=8,color=!cyan
  endif 
     
  clr=!p.color
;  colors=[replicate(!red,3),replicate(!blue,4),replicate(!green,4),replicate(!black,8)]
  legend,['log ULNorm = '+sigfig(ulnorm,3)+!tsym.plusminus+sigfig(ulsigma[0],2),$
          'ULPow = '+sigfig(ulpow,3)+!tsym.plusminus+sigfig(ulsigma[1],2),$
          !tsym.chi+'!U2!N/dof = '+sigfig(ulchisq,3),$
          'ULdof = '+ntostr(fix(uldof)),$
          'log RiseNorm = '+sigfig(norm2[0],3)+!tsym.plusminus+sigfig(sigma2[0,0],2),$
          'RisePow = '+sigfig(pow2[0],3)+!tsym.plusminus+sigfig(sigma2[1,0],2),$
          'tstart = '+ntostr(round(tstart))+' s',$
          !tsym.chi+'!U2!N/dof = '+sigfig(chisq[0],3),$
          'Risedof = '+ntostr(fix(dof2[0])),$
          'log DecayNorm = '+sigfig(norm2[1],3)+!tsym.plusminus+sigfig(sigma2[0,1],2),$
          'DecayPow = '+sigfig(pow2[1],3)+!tsym.plusminus+sigfig(sigma2[1,1],2),$
          'tstop = '+ntostr(round(tstop))+' s',$
          !tsym.chi+'!U2!N/dof = '+sigfig(chisq[1],3),$
          'Decaydof = '+ntostr(fix(dof2[1])),$
          'Intercept Tpeak = '+ntostr(round(tpeak))+' s',$
          'Intercept Peak = '+sigfig(peak,3)+'  cts/s',$
          'Intercept Increase = '+sigfig(increase,4),$
          'Max point Tpeak = '+ntostr(round(tpeak1))+' s',$
          'Max point Peak = '+sigfig(peak1,3)+'  cts/s',$
          'Max point Increase = '+sigfig(increase1,4),$
          'Significance = '+sigfig(signif,4),$
          'exp factor = '+sigfig(exp_fact,3),$
          'pu factor = '+sigfig(pufact,3),$
          'ulratio = '+sigfig(ulratio,5)],$
     /top,/right,textcolor=[replicate(!red,4),replicate(!blue,5),$
                            replicate(!green,5),$
                            replicate(!black,10)],box=0,charsize=1 ;,$
  
;     line=[replicate(-1,12),0,0,0,1,1,1,-1]
  if latespec then begin
     legend,[strarr(24),$
             'UL MeanTime = '+ntostr(medtime),$
             'log ULNorm2 = '+sigfig(ulnorm2,3)+!tsym.plusminus+sigfig(ulsigma2[0],2),$
             'ULPow2 = '+sigfig(ulpow2,3)+!tsym.plusminus+sigfig(ulsigma2[1],2),$
             !tsym.chi+'!U2!N/dof = '+sigfig(ulchisq2,3),$
             'ULdof2 = '+ntostr(fix(uldof2))],$
        /top,/right,box=0,textcolor=[replicate(!black,24),replicate(!cyan,5)],charsize=1
  endif 
  
  
  endplot
  
  return
end 

pro plot_base_lc,w,wb,_extra=_extra,ps=ps
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  ww=where(sigma ge 3,nw)
  wulim=where(sigma lt 3,nwulim)
  if n_elements(yrange) eq 0 then yrange=[min(cts-err),max(cts+err)]
  plot,time,cts,psym=3,xtitle='seconds since BAT trigger',ytitle='cts/s',/xlog,/ylog,yrange=yrange,_extra=_extra
  for i=0,n_elements(time)-1 do oplot,[time[i]-timerr[i],time[i]+timerr[i]],[cts[i],cts[i]]
;  for i=0,n_elements(time)-1 do oplot,[tstart[i],tstop[i]],[cts[i],cts[i]]
  oploterror,time[ww],cts[ww],timerr[ww],err[ww],psym=3,/nohat
  plotsym,1,5,thick=4
  if nwulim gt 0 then oplot,time[wulim],cts[wulim],psym=8
  
  wpc=where(curr_ftype eq 1,nwpc)
  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill
  if nwpc gt 0 then oplot,time[wpc],cts[wpc],psym=8 
  
  if n_elements(w) ne 0 then begin 
     oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!red,errcolor=!red,/nohat
     wpc=where(curr_ftype[w] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
  endif 
  
  if n_elements(wb) gt 1 then begin
     oploterror,time[wb],cts[wb],timerr[wb],err[wb],color=!cyan,psym=3,errcolor=!cyan,/nohat
     oplot,t,10d^(ulnorm2)*t^ulpow2,color=!cyan,thick=1.
     
  endif 
  
  return
end 
  
pro calc_signif,src,bg,exp,tstart,tstop,signif,exp_fact,pufact,ulratio,medtime,latespec=latespec
  
   common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
   
   q=where(time gt tstart and time lt tstop)
;   ulcts=10d^ulnorm/(ulpow+1.)*(max(time[q])^(ulpow+1.)-min(time[q])^(ulpow+1.))
   ulcts=10d^ulnorm/(ulpow+1.)*(tstop^(ulpow+1.)-tstart^(ulpow+1.))*(total(expt[q])/(tstop-tstart))
;   tcts=total(src[q]*exp[q])
   tcts=total(src[q])
   
;   signif=(tcts-ulcts)/sqrt(tcts+ulcts)
   exp_fact=total(exp[q]*(src[q]-bg[q]))/total(src[q]-bg[q])
   
   ;;;change to be like exp_fact!!!!!!!!!!!!!!!!!!!
   pufacts=1./(((src[q]-bg[q])/expt[q])/cts[q]/exp[q])
   pufacts=cts[q]/((src[q]-bg[q])/expt[q])/exp[q]
   pufact=total(pufacts*(src[q]-bg[q]))/total(src[q]-bg[q])
   
   signif=(tcts-ulcts/exp_fact/pufact)/sqrt(tcts+ulcts/exp_fact/pufact)
   
   print,'    Flare events = '+ntostr(tcts)
   printf,llun,'    Flare events = '+ntostr(tcts)
   
   print,'    extrap under aglow events = '+ntostr(ulcts)
   printf,llun,'    extrap under aglow events = '+ntostr(ulcts)
   
;   print,'    Excess events = '+ntostr(tcts-ulcts)
;   printf,llun,'    Excess events = '+ntostr(tcts-ulcts)
   
   print,'    significance = '+ntostr(signif)
   printf,llun,'    significance = '+ntostr(signif)
;   print,'   alternate significance = '+ntostr(signif2)
;   printf,llun,'   alternate significance = '+ntostr(signif2)
   print,'    exp factor = '+ntostr(exp_fact)
   printf,llun,'    exp factor = '+ntostr(exp_fact)
   
   print,'    pu factor = '+ntostr(pufact)
   printf,llun,'    pu factor = '+ntostr(pufact)
   
   print,'    ulratio = '+ntostr(ulratio)
   printf,llun,'    ulratio = '+ntostr(ulratio)
   
   legend,[strarr(20),'Significance = '+sigfig(signif,4),$
           'exp factor = '+sigfig(exp_fact,3),$
           'pu factor = '+sigfig(pufact,3),$
           'ulratio = '+sigfig(ulratio,5)],$
      /top,/right,box=0
   

   return
end

  
pro find_peaks,norm1,pow1,sigma1,norm2,pow2,sigma2,tstart,tstop,tpeak,$
               peak,tpeak1,peak1,increase,increase1,_extra=_extra
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  clr=!p.color
  icpt=10d^(norm1-norm2)^(1d/(pow2-pow1))
;  icpterr=sqrt((sigma2[0]/norm2)^2+(sigma2[1]/pow2)^2+(sigma1[0]/norm1)^2+(sigma1[1]/pow1)^2)*icpt
;  n1=10d^norm1
;  n2=10d^norm2
;  sn1=10d^sigma1[0];10d^(sigma1[0]+norm1)-10d^(norm1-sigma1[0])
;  sn2=10d^sigma2[0];10d^(sigma2[0]+norm2)-10d^(norm2-sigma2[0])
;  b=(1./(pow2-pow1))
;  dxdn1=1./n2*b*n1^(b-1.)
;  dxdn2=n1*n2^(-b-1.)*(-b)
;  dxdp1=alog(n1/n2)*(n1/n2)^b*b^2.
;  dxdp2=alog(n1/n2)*(n1/n2)^b*(-1d)*b^2.
;  icpterr=sqrt(sn1^2*dxdn1^2+sn2^2*dxdn2^2+sigma1[1]^2*dxdp1^2+sigma2[1]^2*dxdp2^2)
  
  tpeak=icpt
;  tpeakerr=icpterr
  peak=10d^norm1*tpeak^pow1
;  peakerr=sqrt((sigma1[0]/norm1)^2+(sigma1[1]/pow1)^2+(tpeakerr/tpeak)^2)*peak
;  peakerr=sqrt(sn1^2*(tpeak^pow1)^2+sigma1[1]^2*(n1*tpeak^pow1*alog(tpeak))^2)
  
  increase=peak/(10d^ulnorm*tpeak^ulpow)
;  calc_signif,ulnorm,ulpow,tpeak,peak,tstart,tstop,cts,time,timerr,signif
;  print,'   Intercept peak (time,rate,increase): '+ntostr(tpeak)+' +/- '+ntostr(tpeakerr)+' , '+ntostr(peak)+' +/- '+ntostr(peakerr)+' , '+ntostr(increase);+' , '+ntostr(signif)
;  printf,llun,'   Intercept peak (time,rate,increase): '+ntostr(tpeak)+' +/- '+ntostr(tpeakerr)+' , '+ntostr(peak)+' +/- '+ntostr(peakerr)+' , '+ntostr(increase)
  print,'    Intercept peak (time,rate,increase): '+ntostr(tpeak)+' , '+ntostr(peak)+' , '+ntostr(increase) ;+' , '+ntostr(signif)
  printf,llun,'    Intercept peak (time,rate,increase): '+ntostr(tpeak)+' , '+ntostr(peak)+' , '+ntostr(increase)
 
  
  wp=where(time gt tstart and time lt tstop,nw)
  if nw gt 0 then begin 
     peak1=max(cts[wp],t)
     tpeak1=time[wp[t]]
     tpeak1err=timerr[wp[t]]
     peak1err=err[wp[t]]
     increase1=peak1/(10d^ulnorm*tpeak1^ulpow)
     
;     print,'   Max point peak (time,rate,increase): ',ntostr(tpeak1)+' +/- '+ntostr(tpeak1err)+' , '+ntostr(peak1)+' +/- '+ntostr(peak1err)+' , '+ntostr(increase);ntostr(signif1)
;     printf,llun,'   Max point peak (time,rate,increase): ',ntostr(tpeak1)+' +/- '+ntostr(tpeak1err)+' , '+ntostr(peak1)+' +/- '+ntostr(peak1err)+' , '+ntostr(increase)
     print,'    Max point peak (time,rate,increase): ',ntostr(tpeak1)+' , '+ntostr(peak1)+' , '+ntostr(increase1)
     printf,llun,'    Max point peak (time,rate,increase): ',ntostr(tpeak1)+' , '+ntostr(peak1)+' , '+ntostr(increase1)
     oplot,[tpeak,tpeak],[1e-8,1e6]
     oplot,[1,1e7],[peak,peak]
     oplot,[tpeak1,tpeak1],[1e-8,1e6],line=1
     oplot,[1,1e7],[peak1,peak1],line=1
     estat='good'
     
     legend,[strarr(14),$
             'Intercept Tpeak = '+ntostr(round(tpeak))+' s',$
             'Intercept Peak = '+sigfig(peak,3)+'  cts/s',$
             'Intercept Increase = '+sigfig(increase,4),$
             'Max point Tpeak = '+ntostr(round(tpeak1))+' s',$
             'Max point Peak = '+sigfig(peak1,3)+'  cts/s',$
             'Max point Increase = '+sigfig(increase1,4)],$
        /top,/right,box=0
     
  endif else begin
     print,'Power-laws do not intersect'
     estat='bad'
     tpeak1=0
     peak1=0
     increase1=0
     legend,[strarr(14),$
             'Max point Tpeak = '+ntostr(round(tpeak1))+' s',$
             'Max point Peak = '+sigfig(peak1,3)+'  cts/s',$
             'Max point Increase = '+sigfig(increase1,4)],$
        /top,/right,box=0
 
  endelse 
  
  
  return
end

pro fit_under_aglow,w,ultstarts,ultstops,noplot=noplot,latespec=latespec,_extra=_extra,zoom=zoom
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  if n_elements(w) eq 0 then w=indgen(n_elements(time))
  xerr=(alog10(time[w]+timerr[w])-alog10(time[w]-timerr[w]))/2.
  yerr=(alog10(cts[w]+err[w])-alog10(cts[w]-err[w]))/2.
  
  if not keyword_set(latespec) then $
     fitexy,alog10(time[w]),alog10(cts[w]),ulnorm,ulpow,x_sig=xerr,y_sig=yerr,ulsigma,chisq else $
     fitexy,alog10(time[w]),alog10(cts[w]),ulnorm2,ulpow2,x_sig=xerr,y_sig=yerr,ulsigma2,chisq2
;  print,norm1,sigma1[0]
;  print,pow1,sigma1[1]
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'

;  ploterror,time,cts,timerr,err,psym=3,/nohat,/xlog,/ylog,_extra=_extra,xtitle=xtitle,ytitle=ytitle,xrange=xrange,yrange=yrange
;  oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!red,errcolor=!red,/nohat
  
  if not keyword_set(noplot) then begin 
     if not keyword_set(zoom) then plot_base_lc,wul,_extra=_extra else $
        plot_base_lc,wul
     
     color=!red
  endif else color=!cyan
  
  t=[1,time,1e8]
  if not keyword_set(latespec) then begin 
      oplot,t,10^(ulnorm)*t^ulpow,color=color
      uldof=n_elements(w)-2
      ulchisq=chisq/uldof
      legend,['log ULNorm = '+sigfig(ulnorm,3)+!vsym.plusminus+sigfig(ulsigma[0],2),$
              'ULPow = '+sigfig(ulpow,3)+!vsym.plusminus+sigfig(ulsigma[1],2),$
              !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq,3),$
              'ULdof = '+ntostr(fix(uldof))],$
        /top,/right,box=0,textcolor=[replicate(!red,4)]
   endif else begin
      oplot,t,10^(ulnorm)*t^ulpow,color=!red
;      uldof=n_elements(w)-2
;      ulchisq=chisq/uldof
      oplot,t,10^(ulnorm2)*t^ulpow2,color=color
      uldof2=n_elements(w)-2
      ulchisq2=chisq2/uldof2
      legend,['log ULNorm = '+sigfig(ulnorm,3)+!vsym.plusminus+sigfig(ulsigma[0],2),$
              'ULPow = '+sigfig(ulpow,3)+!vsym.plusminus+sigfig(ulsigma[1],2),$
              !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq,3),$
              'ULdof = '+ntostr(fix(uldof)),$
              'log ULNorm2 = '+sigfig(ulnorm2,3)+!vsym.plusminus+sigfig(ulsigma2[0],2),$
              'ULPow2 = '+sigfig(ulpow2,3)+!vsym.plusminus+sigfig(ulsigma2[1],2),$
              !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq2,3),$
              'ULdof2 = '+ntostr(fix(uldof2))],$
         /top,/right,box=0,textcolor=[replicate(!red,4),replicate(!cyan,4)]
  endelse 

  return
end 
  
pro fit_flare_leg,tstart,tstop,norm2,pow2,sigma2,icpt,w,chisq,fdof,_extra=_extra,status=status,zoom=zoom
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  q=where(time gt tstart and time lt tstop,nq)
  xerr=(alog10(time+timerr)-alog10(time-timerr))/2.
  yerr=(alog10(cts+err)-alog10(cts-err))/2.
  fitexy,alog10(time[q]),alog10(cts[q]),norm2,pow2,x_sig=xerr[q],y_sig=yerr[q],sigma2,chi2
  
  icpt=10d^(ulnorm-norm2)^(1d/(pow2-ulpow))
  icpterr=sqrt((sigma2[0]/norm2)^2+(sigma2[1]/pow2)^2+(ulsigma[0]/ulnorm)^2+(ulsigma[1]/ulpow)^2)*icpt
  
  fdof=nq-2
  chisq=chi2/fdof
  
  if fall then begin 
     desc=' decay ' 
     tm='Tstop '
  endif else begin 
     desc=' rise '
     tm='Tstart '
  endelse 
  
  if rise then print,'    Underlying afterglow (log n,pow,chisq/dof): '+ntostr(ulnorm)+' +/- '+ntostr(ulsigma[0])+' , '+ntostr(ulpow)+' +/- '+ntostr(ulsigma[1])+', '+ntostr(ulchisq)
  print,'    Flare '+desc+'(log n,pow,chisq/dof):                '+ntostr(norm2)+' +/- '+ntostr(sigma2[0])+' , '+ntostr(pow2)+' +/- '+ntostr(sigma2[1])+', '+ntostr(chisq)
  print,'    '+tm+'(s):        '+ntostr(icpt);+' +/- '+ntostr(icpterr)

  if rise then printf,llun,'    Underlying afterglow (log n,pow,chisq/dof): '+ntostr(ulnorm)+' +/- '+ntostr(ulsigma[0])+' , '+ntostr(ulpow)+' +/- '+ntostr(ulsigma[1])+', '+ntostr(ulchisq)
  printf,llun,'    Flare '+desc+'(log n,pow,chisq/dof):                '+ntostr(norm2)+' +/- '+ntostr(sigma2[0])+' , '+ntostr(pow2)+' +/- '+ntostr(sigma2[1])+', '+ntostr(chisq)
  printf,llun,'    '+tm+'(s):        '+ntostr(icpt);+' +/- '+ntostr(icpterr)
  
  
  if status then begin 
;     ploterror,time,cts,timerr,err,psym=3,/nohat,/xlog,/ylog,_extra=_extra,xtitle=xtitle,ytitle=ytitle,xrange=xrange,yrange=yrange
     if not keyword_set(zoom) then plot_base_lc,_extra=_extra else $
        plot_base_lc
     color=!blue
  endif else color=!green
  plotsym,0,1.0,/fill
  if n_elements(w) ne 0 then begin
     oploterror,time[w],cts[w],timerr[w],err[w],/nohat,psym=3,errcolor=!red,color=!red
     wpc=where(curr_ftype[w] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
  endif 
  
  oploterror,time[q],cts[q],timerr[q],err[q],color=color,psym=3,errcolor=color,/nohat
  wpc=where(curr_ftype[q] eq 1,nwpc)
  if nwpc gt 0 then oplot,time[q[wpc]],cts[q[wpc]],psym=8,color=color
  
  t=[1,time,1e8]
  oplot,t,10d^(ulnorm)*t^ulpow,color=!red
  oplot,t,10d^(norm2)*t^pow2,color=color
  
  if status then begin
     legend,['log Norm1 = '+sigfig(ulnorm,3)+!vsym.plusminus+sigfig(ulsigma[0],2),$
             'Pow1 = '+sigfig(ulpow,3)+!vsym.plusminus+sigfig(ulsigma[1],2),$
             !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq,3),$
             'dof = '+ntostr(fix(uldof)),$
             'log RiseNorm = '+sigfig(norm2,3)+!vsym.plusminus+sigfig(sigma2[0],2),$
             'RisePow = '+sigfig(pow2,3)+!vsym.plusminus+sigfig(sigma2[1],2),$
             'tstart = '+ntostr(round(icpt))+' s',$
             !vsym.chi+'!U2!N/dof = '+sigfig(chisq,3),$
             'dof = '+ntostr(fix(fdof))],$
        /top,/right,textcolor=[replicate(!red,4),replicate(color,5)],box=0
  endif else begin
     legend,[strarr(9),$
             'log DecayNorm = '+sigfig(norm2,3)+!vsym.plusminus+sigfig(sigma2[0],2),$
             'DecayPow = '+sigfig(pow2,3)+!vsym.plusminus+sigfig(sigma2[1],2),$
             'tstop = '+ntostr(round(icpt))+' s',$
             !vsym.chi+'!U2!N/dof = '+sigfig(chisq,3),$
             'dof = '+ntostr(fix(fdof))],$
        /top,/right,box=0,$
        textcolor=[replicate(clr,9),replicate(color,5)]
     
  endelse   
  return
end 

pro user_gtis,w,tstarts,tstops,_extra=_extra,part=part,flares=flares,noplot=noplot,latespec=latespec,zoom=zoom
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
;  ploterror,time,cts,timerr,err,psym=3,/nohat,/xlog,/ylog,_extra=_extra,xtitle=xtitle,ytitle=ytitle
  if not keyword_set(noplot) then begin
     if not keyword_set(zoom) then plot_base_lc,_extra=_extra else $
        plot_base_lc
  endif 
  
  plotsym,0,1.0,/fill
  x=0. & y=0. & i=1 & tstarts=0. & tstops=0.
  !mouse.button=0
;  WHILE  answer eq 'y' do begin 
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
        
        oplot,[xx,xx],[1e-8,1e4],color=color
        print,'***click*** ',xx,yy
        printf,llun,'***click*** ',xx,yy
        x=[x,xx]
        y=[y,yy]

        if i mod 2 eq 0 or (keyword_set(flares) and i mod 4 eq 0) then begin 
           if i eq 2 then wsmt='time gt '+ntostr(x[i-1])+' and time lt '+ntostr(x[i]) else begin
              wsmt=wsmt+'or time gt '+ntostr(x[i-1])+' and time lt '+ntostr(x[i])
           endelse 
           
           com='w=where('+wsmt+')'
           tmp=execute(com)
           
           if not keyword_set(flares) then begin
              if keyword_set(latespec) then clr=!cyan else clr=!red
              oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=clr,errcolor=clr,/nohat 
              wpc=where(curr_ftype[w] eq 1,nwpc)
              if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=clr
           endif else begin 
                            
              if rise then begin 
                 oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!blue,errcolor=!blue,/nohat
                 nw=n_elements(w)
                 wpc=where(curr_ftype[w] eq 1,nwpc)
                 if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!blue
              endif 
              
              if fall then begin 
                 wq=w[nw:*]
                 oploterror,time[wq],cts[wq],timerr[wq],err[wq],psym=3,color=!green,errcolor=!green,/nohat
                 wpc=where(curr_ftype[wq] eq 1,nwpc)
                 if nwpc gt 0 then oplot,time[wq[wpc]],cts[wq[wpc]],psym=8,color=!green
              endif 
           endelse 
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

pro fit_flares,file=file,_extra=_extra,claudio=claudio,help=help,ulfiles=ulfiles,ffiles=ffiles,ra=ra,dec=dec,backra=backra,backdec=backdec,bat_trig=bat_trig,brad=brad,srad=srad,ultstarts2=ultstarts2,ultstops2=ultstops2,zoom=zoom,title=title;,latespec=latespec
  
  if keyword_set(help) then begin 
     print,'syntax - fit_flares,file=file,/claudio,/help,_extra=_extra'
     print,'                 where file is something other version than lc_out.txt if desired'
     print,'                 /claudio makes your plot smaller if you are like Claudio'
     print,'                 _extra can give any extra plotting keywords like xrange=[0,10]'
     print,'                 /zoom will zoom into the xrange during flare fitting'
;     print,'                 /latespec '
     print,'                 optional keywords to fill in par file: '
     print,'                 ulfiles=ulfiles,ffiles=ffiles,ra=ra,dec=dec,'
     print,'                 backra=backra,backdec=backdec,bat_trig=bat_trig,brad=brad,srad=srad'
     return
  endif 
  
  common fit_flares_common,llun,fglun,time,timerr,cts,err,ulpow,ulnorm,ulsigma,ulchisq,uldof,estat,sigma,fall,rise,wul,expt,curr_ftype,ulnorm2,ulpow2,ulsigma2,ulchisq2,uldof2
  
  
  simpctable
  defsymbols
  if n_elements(file) eq 0 then file='lc_out.txt'
  
  openw,llun,'fit_flares.log',/get_lun
  openw,fglun,'flares_gti.dat',/get_lun
  
  ;read in data from David's LC script
  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  timerr=((time-tstarted)+(tstoped-time))/2.
  
  ;;bg is bg rate if lc_newout.txt
  ;; how can we check?
  
  ;get gti's from user
  answer=''
  ufile='under_aglow_gti0.dat'

  if keyword_set(claudio) then window,0,xsize=900,ysize=600 else window,0,xsize=1300,ysize=800
  read,prompt='Fit Underlying Afterglow (or use existing GTI file) (y/n)? ',answer
  uglowagain:
  
  k='n'
  
  while k ne 'y' do begin 
     if answer ne 'n' then begin 
        p=1
        while exist(ufile) do begin
           ufile='under_aglow_gti'+ntostr(p)+'.dat'
           p=p+1
        endwhile
        
        user_gtis,w,ultstarts,ultstops,_extra=_extra,part='Underlying afterglow estimation',zoom=zoom
        wul=w
        openw,lun,ufile,/get_lun
        for i=0,n_elements(ultstarts)-1 do printf,lun,ultstarts[i],ultstops[i]
        close,lun
        free_lun,lun
     endif else begin
        p=1
        while exist(ufile) do begin
           ufile='under_aglow_gti'+ntostr(p)+'.dat'
           p=p+1
        endwhile 
        ufile='under_aglow_gti'+ntostr(p-2)+'.dat'
        print,'Retrieving underlying afterglow from: '+ufile
        readcol,ufile,ultstarts,ultstops,/silent
        for i=0,n_elements(ultstarts)-1 do begin 
           if i eq 0 then wsmt='time gt '+ntostr(ultstarts[i])+' and time lt '+ntostr(ultstops[i]) else $
              wsmt=wsmt+'or time gt '+ntostr(ultstarts[i])+' and time lt '+ntostr(ultstops[i])
           com='w=where('+wsmt+')'
           tmp=execute(com)
        endfor 
     endelse 
     
     fit_under_aglow,w,ultstarts,ultstops,yrange=yrange,_extra=_extra,zoom=zoom
     print,'Writing out GTI file for Underlying Afterglow'
     k='y'
     read,k,prompt='Is this an acceptable fit? (y to use, n to redo) '
     if k eq '' then k='y'
     if k eq 'n' then answer='y'
 endwhile

 ulratio=1.0
 medtime=0.
 wb=0
 dolate=''
 print
 read,dolate,prompt='Will you use this same underlying GTI for the spectral analysis? (y/n) '
 if dolate eq 'n' then latespec=1 else latespec=0
 if keyword_set(latespec) then begin
;     print,'Fitting late time temporal parameters for spectral fits'
    print,'Select GTI for spectral analysis'
     k='n'
     while k ne 'y' do begin
         user_gtis,wb,ultstarts,ultstops,_extra=_extra,noplot=noplot,part='Late time spectral Underlying afterglow estimation',latespec=latespec,zoom=zoom
;         wul=wb
         fit_under_aglow,wb,ultstarts,ultstops,yrange=yrange,/noplot,/late,_extra=_extra,zoom=zoom
         oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!red,/nohat,errcolor=!red
         wpc=where(curr_ftype[w] eq 1,nwpc)
         if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
         print,'Writing out GTI file for late spectral Underlying Afterglow'
         read,k,prompt='Is this an acceptable fit? (y to use, n to redo) '
         if k eq '' then k='y'
     endwhile
     ;; calculating normalization
;     medtime=median(time[wb])
     medtime=total(time[wb]*(src[wb]-bg[wb]))/total((src[wb]-bg[wb]))
     ctsmed=10d^(ulnorm)*medtime^ulpow
     ctsmedlate=10d^(ulnorm2)*medtime^ulpow2
     ulratio=ctsmedlate/ctsmed

     print,'Weighted mean time in late spec region: '+ntostr(medtime)
     print,'Ratio of power laws at mean time: '+ntostr(ulratio)
 endif
  
  if n_elements(same) eq 0 then begin 
     r=0
     norm2=0d & pow2=0d & sigma2=dblarr(2,1) & chisq2=0d & dof2=0
  endif 
  answer=''
  breakup=0
  while answer ne 'n' do begin
     print
     print,'Fitting Flare '+ntostr(r+1)
     printf,llun
     printf,llun,'Fitting Flare '+ntostr(r+1)
     if not breakup then user_gtis,f,tstarts,tstops,_extra=_extra,/flares,part='Flare '+ntostr(r+1)
;     if i mod 4 eq 0 or i mod 4 eq 3 then status=0
;     if i mod 4 eq 1 or i mod 4 eq 2 then status=1
     
     for j=0,1 do begin
        rise=abs(j-1)
        fall=j
        
        fit_flare_leg,tstarts[j],tstops[j],n2,p2,s2,icpt,w,chisq,dof,_extra=_extra,status=abs(j-1),zoom=zoom
        norm2=[norm2,n2]
        pow2=[pow2,p2]
        sigma2=[[sigma2],[s2]]
        if j eq 0 then tstart=icpt
;        if j eq 1 then begin 
;           tstop=icpt
;           printf,fglun,tstart,tstop
;        endif 
;        chisq2=[chisq2,chisq]
        acc='' & line='' & bup=''
        newstart=0 & newstop=0
        flag=[0,0]
        if j eq 1 then begin
           tstop=icpt
           if breakup eq 0 then read,acc,prompt='Acceptable Tstart & Tstop? (y/n) ' $
           else begin
              print
              print,'Select next region of broken-up flare'
              acc='n'
           endelse 
           if acc eq 'n' then begin
              if breakup eq 0 then begin 
                 read,bup,prompt='Breaking up single flare into components? (y/n) '
                 if bup eq 'y' then begin
                    breakup=1 
                    zoom=1
                 endif else breakup=0
              endif 
              read,line,prompt='Pick new Tstart? (y/n) '
              if line eq 'y' then begin 
                 print,'Click on new Tstart'
                 cursor,xxx,yyy,/wait,/change
                 oplot,[xxx,xxx],[1e-6,1e4],color=!yellow
                 tstart=xxx
                 print,'    New Tstart (s): '+ntostr(tstart)
                 printf,llun,'    New Tstart (s): '+ntostr(tstart)
                 legend,[replicate('',28),'New Tstart = '+sigfig(tstart,5)+' s'],/top,/right,box=0,textcolor=[replicate(!white,28),!blue]
                 flag[0]=1
              endif 
              
              read,line,prompt='Pick new Tstop? (y/n) '
              if line eq 'y' then begin 
                 print,'Click on new Tstop'
                 cursor,xxx,yyy,/wait,/change
                 oplot,[xxx,xxx],[1e-6,1e4],color=!yellow
                 tstop=xxx
                 print,'    New Tstop (s): '+ntostr(tstop)
                 printf,llun,'    New Tstop (s): '+ntostr(tstop)
                 legend,[replicate('',29),'New Tstop = '+sigfig(tstop,5)+' s'],/top,/right,box=0,textcolor=[replicate(!white,29),!green]
                 flag[1]=1
              endif 

;              chisq=0.
           endif 
           
           printf,fglun,tstart,tstop
        endif 
        chisq2=[chisq2,chisq]
        dof2=[dof2,dof]
     endfor 
     
;stop     
;     if i mod 2 eq 1 then begin 
     find_peaks,norm2[r*2+1],pow2[r*2+1],sigma2[*,r*2+1],norm2[r*2+2],pow2[r*2+2],sigma2[*,r*2+2],tstart,tstop,tpeak,peak,tpeak1,peak1,increase,increase1
     if estat eq 'good' then calc_signif,src,bg,exp,tstart,tstop,signif,exp_fact,pufact,ulratio,medtime,latespec=latespec
     
     if latespec then begin
        t=[1,time,1e8]
        oploterror,time[wb],cts[wb],timerr[wb],err[wb],color=!cyan,psym=3,errcolor=!cyan,/nohat
        wpc=where(curr_ftype[wb] eq 1,nwpc)
        if nwpc gt 0 then oplot,time[wb[wpc]],cts[wb[wpc]],psym=8,color=!cyan
        oplot,t,10d^(ulnorm2)*t^ulpow2,color=!cyan,thick=1.
        t=[1,time,1e8]
        oplot,t,10d^(ulnorm2)*t^ulpow2,color=!cyan
        legend,[strarr(24),$
                'UL MeanTime = '+ntostr(medtime),$
                'log ULNorm2 = '+sigfig(ulnorm2,3)+!vsym.plusminus+sigfig(ulsigma2[0],2),$
                'ULPow2 = '+sigfig(ulpow2,3)+!vsym.plusminus+sigfig(ulsigma2[1],2),$
                !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq2,3),$
                'ULdof2 = '+ntostr(fix(uldof2))],$
           /top,/right,box=0,textcolor=[replicate(!black,24),replicate(!cyan,5)]
        
     endif 
   
     
     write_outfile,tstart,tstop,exp_fact,pufact,ultstarts,ultstops,norm2[r*2+1:r*2+2],pow2[r*2+1:r*2+2],sigma2[*,r*2+1:r*2+2],chisq2[r*2+1:r*2+2],dof2[r*2+1:r*2+2],tpeak,peak,tpeak1,peak1,increase,increase1,signif,flag,outfile,ulratio,medtime,latespec=latespec
     
     make_outplot,r,w,wb,tstarts,tstops,norm2[r*2+1:r*2+2],pow2[r*2+1:r*2+2],sigma2[*,r*2+1:r*2+2],tstart,tstop,chisq2[r*2+1:r*2+2],dof2[r*2+1:r*2+2],tpeak,peak,tpeak1,peak1,increase,increase1,signif,exp_fact,pufact,ulratio,medtime,outfile,_extra=_extra,latespec=latespec,title=title
     
     write_parfile,tstart,tstop,exp_fact,pufact,r,ultstarts,ultstops,ulfiles,ffiles,ra,dec,backra,backdec,bat_trig,brad,srad,ultstarts2,ultstops2,outfile,ulratio,medtime
     
     r=r+1
     read,answer,prompt='Fit another flare? (y/n) '
     if answer ne 'n' then begin 
        same='y'
        read,same,prompt='Same Underlying afterglow? (y/n) '
        if same eq 'n' then begin 
;           ufile='under_aglow_gti2.dat'
           goto,uglowagain
        endif 
     endif 

  endwhile  
  
  norm2=norm2[1:*]
  pow2=pow2[1:*]
  sigma2=sigma2[*,1:*]
  chisq2=chisq2[1:*]
  dof2=dof2[1:*]
  
  close,/all,/file
  
  readcol,'flares_gti.dat',tstarts,tstops,/silent
  
  print,'See output files (*gti*dat) for GTIs'
  print,'See output files ('+outfile+') for outputs'
  print,'See output files ('+outfile+'.par) for inputs into flarespec'
  
  
  return
end 
  
  
