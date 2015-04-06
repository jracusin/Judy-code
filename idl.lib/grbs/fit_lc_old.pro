pro write_outfile,tstart,tstop,ultstarts,ultstops,norm2,pow2,sigma2,chisq2,dof2,tpeak,peak,tpeak1,peak1,increase,increase1,signif,flag,file,leg=leg
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
    
  file=''
;  read,prompt='Output file name (default flare.out)? ',file
  input,'Output file name? ',file,'flare.out'
;  if file eq '' then file='flare.out'
  
  openw,lun,file,/get_lun
  s=' '
  printf,lun,'TSTART '+ntostr(tstart)
  printf,lun,'TSTOP '+ntostr(tstop)

  ults1='' & ults2=''
  for i=0,n_elements(ultstarts)-1 do begin
     ults1=ults1+s+ntostr(ultstarts[i])
     ults2=ults2+s+ntostr(ultstops[i])
  endfor 

  printf,lun,'UL_TSTARTS'+ults1
  printf,lun,'UL_TSTOPS'+ults2
  
  for leg=0,n_elements(ulnorm)-1 do begin 
     printf,lun,'UL_NORM'+ntostr(leg+1)+' '+ntostr(ulnorm[leg])+s+ntostr(ulsigma[0,leg])
     printf,lun,'UL_POW'+ntostr(leg+1)+' '+ntostr(ulpow[leg])+s+ntostr(ulsigma[1,leg])
     printf,lun,'UL_CHISQ/DOF'+ntostr(leg+1)+' '+ntostr(ulchisq[leg])
     printf,lun,'UL_DOF'+ntostr(leg+1)+' '+ntostr(uldof[leg])
     if leg gt 0 then begin
        icpt=10d^(ulnorm[leg]-ulnorm[leg-1])^(1d/(ulpow[leg-1]-ulpow[leg]))
        icpterr=sqrt((ulsigma[0,leg-1]/ulnorm[leg-1])^2+(ulsigma[1,leg-1]/ulpow[leg-1])^2+(ulsigma[0,leg]/ulnorm[leg])^2+(ulsigma[1,leg]/ulpow[leg])^2)*icpt
        printf,lun,'BREAK_TIME'+ntostr(leg)+ntostr(leg+1)+' = '+sigfig(icpt,3) ;+!vsym.plusminus+sigfig(icpterr,3)]
     endif 
  endfor
  
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
;  printf,lun,'SIGNIF '+ntostr(signif)


  close,lun,/file
  
  return
end 

pro make_outplot,i,w,tstarts,tstops,norm2,pow2,sigma2,tstart,tstop,chisq,dof2,tpeak,peak,tpeak1,peak1,increase,increase1,signif,exp_fact,pufact,ulratio,medtime,outfile,_extra=_extra,title=title
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  name=outfile+'_lc_fit.ps'
;  begplot,name='flare'+ntostr(i+1)+'_lc_fit.ps',/land,/color
  begplot,name=name,/land,/color
  
  plot_base_lc,w,/ps,title=title
  t=[1,time,1e8]
  
  if n_elements(w) ne 0 then oploterror,time[w],cts[w],timerr[w],err[w],/nohat,psym=3,errcolor=!red,color=!red
  color=[!blue,!green]
  plotsym,0,0.7,/fill
;  oplot,time[w],cts[w],psym=8,color=!red
  for j=0,1 do begin 
     q=where(time gt tstarts[j] and time lt tstops[j])
     oploterror,time[q],cts[q],timerr[q],err[q],color=color[j],psym=3,errcolor=color[j],/nohat
     oplot,t,10d^(norm2[j])*t^pow2[j],color=color[j],thick=1.
     wpc=where(type[q] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[q[wpc]],cts[q[wpc]],psym=8,color=color[j]
  endfor
  wpc=where(type[w] eq 1,nwpc)
  if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
  
  ulleg=''
  for leg=0,n_elements(ulnorm)-1 do begin 
     oplot,t,10d^(ulnorm[leg])*t^ulpow[leg],color=!red,thick=1.
     oplot,[1,1e8],[peak,peak],thick=1.
     oplot,[tpeak,tpeak],[1e-6,1e6],thick=1.
     oplot,[1,1e8],[peak1,peak1],line=1,thick=1.
     oplot,[tpeak1,tpeak1],[1e-6,1e6],line=1,thick=1.   
     
     ulleg=[ulleg,$
            'log ULNorm!L'+ntostr(leg+1)+'!N = '+sigfig(ulnorm[leg],3)+!tsym.plusminus+sigfig(ulsigma[0,leg],2),$
            'ULPow!L'+ntostr(leg+1)+'!N = '+sigfig(ulpow[leg],3)+!tsym.plusminus+sigfig(ulsigma[1,leg],2),$
            !tsym.chi+'!U2!N/dof!L'+ntostr(leg+1)+'!N = '+sigfig(ulchisq[leg],3),$
            'ULdof!L'+ntostr(leg+1)+'!N = '+ntostr(fix(uldof[leg]))]
     if leg gt 0 then begin
        icpt=10d^(ulnorm[leg]-ulnorm[leg-1])^(1d/(ulpow[leg-1]-ulpow[leg]))
        icpterr=sqrt((ulsigma[0,leg-1]/ulnorm[leg-1])^2+(ulsigma[1,leg-1]/ulpow[leg-1])^2+(ulsigma[0,leg]/ulnorm[leg])^2+(ulsigma[1,leg]/ulpow[leg])^2)*icpt
        ulleg=[ulleg,'break time'+ntostr(leg)+ntostr(leg+1)+' = '+sigfig(icpt,3)] ;+!vsym.plusminus+sigfig(icpterr,3)]
     endif 
  endfor 
  
  
  clr=!p.color
;  colors=[replicate(!red,3),replicate(!blue,4),replicate(!green,4),replicate(!black,8)]
  ulleg=ulleg[1:*]  
  textcolor=[replicate(!red,n_elements(ulleg)),replicate(!blue,5),$
             replicate(!green,5),replicate(!black,6)]
  
  legend,[ulleg,$
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
          'Max point Increase = '+sigfig(increase1,4)],$
      /top,/right,textcolor=textcolor,box=0,charsize=1 ;,$
  
  
  endplot
  
  return
end 

pro plot_base_lc,w,_extra=_extra,ps=ps
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill
  
  if n_elements(yrange) eq 0 then yrange=[min(cts-err),max(cts+err)]
  xrange=[min(time-timerr),max(time+timerr)]
  
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
  
pro calc_signif,src,bg,exp,tstart,tstop,signif,exp_fact,pufact,ulratio,medtime
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
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
               peak,tpeak1,peak1,increase,increase1,leg=leg,_extra=_extra
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  clr=!p.color
  icpt=10d^(norm1-norm2)^(1d/(pow2-pow1))
  tpeak=icpt
  peak=10d^norm1*tpeak^pow1
  
  increase=peak/(10d^ulnorm[leg]*tpeak^ulpow[leg])

  print,'    Intercept peak (time,rate,increase): '+ntostr(tpeak)+' , '+ntostr(peak)+' , '+ntostr(increase) ;+' , '+ntostr(signif)
  printf,llun,'    Intercept peak (time,rate,increase): '+ntostr(tpeak)+' , '+ntostr(peak)+' , '+ntostr(increase)
 
  
  wp=where(time gt tstart and time lt tstop,nw)
  if nw gt 0 then begin 
     peak1=max(cts[wp],t)
     tpeak1=time[wp[t]]
     tpeak1err=timerr[wp[t]]
     peak1err=err[wp[t]]
     increase1=peak1/(10d^ulnorm[leg]*tpeak1^ulpow[leg])
     
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

pro fit_under_aglow,w,ultstarts,ultstops,noplot=noplot,_extra=_extra,zoom=zoom,w2=w2,leg=leg
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  if n_elements(w) eq 0 then w=indgen(n_elements(time))
  xerr=(alog10(time[w]+timerr[w])-alog10(time[w]-timerr[w]))/2.
  yerr=(alog10(cts[w]+err[w])-alog10(cts[w]-err[w]))/2.
  
  fitexy,alog10(time[w]),alog10(cts[w]),uln,ulp,x_sig=xerr,y_sig=yerr,ulsig,chisq 
  
  ulnorm[leg]=uln
  ulpow[leg]=ulp
  ulsigma[*,leg]=ulsig
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'

  
  if not keyword_set(noplot) then begin 
     if not keyword_set(zoom) then plot_base_lc,w,_extra=_extra else $
        plot_base_lc,w
  endif 

  color=!red
;  endif else color=!cyan
  
  if n_elements(w2) gt 0 then begin
     plotsym,0,1.0,/fill
     oploterror,time[w2],cts[w2],timerr[w2],err[w2],/nohat,color=color,errcolor=color,psym=3
     pc=where(type[w2] eq 1,npc)
     if npc gt 0 then plots,time[w2[pc]],cts[w2[pc]],psym=8,color=color
     t=time[w]
  endif else t=[1,time,1e8]
  if leg eq 0 then t=[1,t]
  
  oplot,t,10^(uln)*t^ulp,color=color
  uld=n_elements(w)-2
  ulchi=chisq/uld
  uldof[leg]=uld
  ulchisq[leg]=ulchi
  
  if not finite(ulchi) then ulc=ntostr(ulchi) else ulc=sigfig(ulchi,3)
  leg2=['log ULNorm = '+sigfig(uln,3)+!vsym.plusminus+sigfig(ulsig[0],2),$
        'ULPow = '+sigfig(ulp,3)+!vsym.plusminus+sigfig(ulsig[1],2),$
        !vsym.chi+'!U2!N/dof = '+ulc,$
        'ULdof = '+ntostr(fix(uld))]
  
  if leg gt 0 then begin
     icpt=10d^(ulnorm[leg]-ulnorm[leg-1])^(1d/(ulpow[leg-1]-ulpow[leg]))
     icpterr=sqrt((ulsigma[0,leg-1]/ulnorm[leg-1])^2+(ulsigma[1,leg-1]/ulpow[leg-1])^2+(ulsigma[0,leg]/ulnorm[leg])^2+(ulsigma[1,leg]/ulpow[leg])^2)*icpt
     leg2=[leg2,'Break time = '+sigfig(icpt,3)];+!vsym.plusminus+sigfig(icpterr,3)]
     leg1=replicate('',leg*6)
     legs=[leg1,leg2]
  endif else begin 
     legs=leg2
     leg=0
  endelse 
  
  legend,legs,$
     /top,/right,box=0,textcolor=[replicate(!red,5+leg*6)]

  return
end 
  
pro fit_flare_leg,tstart,tstop,norm2,pow2,sigma2,icpt,w,chisq,fdof,_extra=_extra,status=status,zoom=zoom,leg=leg,w3=w3
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  q=where(time gt tstart and time lt tstop,nq)
  xerr=(alog10(time+timerr)-alog10(time-timerr))/2.
  yerr=(alog10(cts+err)-alog10(cts-err))/2.
  fitexy,alog10(time[q]),alog10(cts[q]),norm2,pow2,x_sig=xerr[q],y_sig=yerr[q],sigma2,chi2
  
  icpt=10d^(ulnorm[leg]-norm2)^(1d/(pow2-ulpow[leg]))
  icpterr=sqrt((sigma2[0]/norm2)^2+(sigma2[1]/pow2)^2+(ulsigma[0,leg]/ulnorm[leg])^2+(ulsigma[1,leg]/ulpow[leg])^2)*icpt
  
  fdof=nq-2
  chisq=chi2/fdof
  
  if fall then begin 
     desc=' decay ' 
     tm='Tstop '
  endif else begin 
     desc=' rise '
     tm='Tstart '
  endelse 
  
  if rise then print,'    Underlying afterglow (log n,pow,chisq/dof): '+ntostr(ulnorm[leg])+' +/- '+ntostr(ulsigma[0,leg])+' , '+ntostr(ulpow[leg])+' +/- '+ntostr(ulsigma[1,leg])+', '+ntostr(ulchisq[leg])
  print,'    Flare '+desc+'(log n,pow,chisq/dof):                '+ntostr(norm2)+' +/- '+ntostr(sigma2[0])+' , '+ntostr(pow2)+' +/- '+ntostr(sigma2[1])+', '+ntostr(chisq)
  print,'    '+tm+'(s):        '+ntostr(icpt);+' +/- '+ntostr(icpterr)

  if rise then printf,llun,'    Underlying afterglow (log n,pow,chisq/dof): '+ntostr(ulnorm[leg])+' +/- '+ntostr(ulsigma[0,leg])+' , '+ntostr(ulpow[leg])+' +/- '+ntostr(ulsigma[1,leg])+', '+ntostr(ulchisq[leg])
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
     wpc=where(type[w] eq 1,nwpc)
     if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!red
  endif 
  
  oploterror,time[q],cts[q],timerr[q],err[q],color=color,psym=3,errcolor=color,/nohat
  wpc=where(type[q] eq 1,nwpc)
  if nwpc gt 0 then oplot,time[q[wpc]],cts[q[wpc]],psym=8,color=color
  
  if n_elements(w3) gt 0 then begin
     plotsym,0,1.0,/fill
     oploterror,time[w3],cts[w3],timerr[w3],err[w3],/nohat,color=!red,errcolor=!red,psym=3
     pc=where(type[w3] eq 1,npc)
     if npc gt 0 then plots,time[w3[pc]],cts[w3[pc]],psym=8,color=!red
  endif 
;     t=time[w]
;  endif else t=[1,time,1e8]
;  if leg eq 0 then t=[1,t]
  
  t=[1,time,1e8]
  for l=0,n_elements(ulnorm)-1 do $     
     oplot,t,10d^(ulnorm[l])*t^ulpow[l],color=!red
     
  oplot,t,10d^(norm2)*t^pow2,color=color
  
  if status then begin
     legend,['log Norm1 = '+sigfig(ulnorm[leg],3)+!vsym.plusminus+sigfig(ulsigma[0,leg],2),$
             'Pow1 = '+sigfig(ulpow[leg],3)+!vsym.plusminus+sigfig(ulsigma[1,leg],2),$
             !vsym.chi+'!U2!N/dof = '+sigfig(ulchisq[leg],3),$
             'dof = '+ntostr(fix(uldof[leg])),$
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

pro user_gtis,w,tstarts,tstops,_extra=_extra,part=part,flares=flares,noplot=noplot,zoom=zoom
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  clr=!red;!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  if not keyword_set(noplot) then begin
     if not keyword_set(zoom) then plot_base_lc,_extra=_extra else $
        plot_base_lc
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
              oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=clr,errcolor=clr,/nohat 
              wpc=where(type[w] eq 1,nwpc)
              if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=clr
           endif else begin 
                            
              if rise then begin 
                 oploterror,time[w],cts[w],timerr[w],err[w],psym=3,color=!blue,errcolor=!blue,/nohat
                 nw=n_elements(w)
                 wpc=where(type[w] eq 1,nwpc)
                 if nwpc gt 0 then oplot,time[w[wpc]],cts[w[wpc]],psym=8,color=!blue
              endif 
              
              if fall then begin 
                 wq=w[nw:*]
                 oploterror,time[wq],cts[wq],timerr[wq],err[wq],psym=3,color=!green,errcolor=!green,/nohat
                 wpc=where(type[wq] eq 1,nwpc)
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

pro fit_lc,file,small=small
  
  if keyword_set(help) then begin 
     print,'syntax - fit_flares,file'
     return
  endif 
  
  if n_elements(file) eq 0 then file='lc_simp.txt'
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  simpctable
  defsymbols
  
  openw,llun,'fit_flares.log',/get_lun
  openw,fglun,'flares_gti.dat',/get_lun
  
  readcol,file,time,timerr,cts,err,type,format='(d,d,d,d,i)'
  
                                ;get gti's from user
  answer=''
  ufile='under_aglow_v2_gti0.dat'

  if keyword_set(small) then window,0,xsize=900,ysize=600 else window,0,xsize=1300,ysize=800
  read,prompt='Fit Underlying Afterglow (or use existing GTI file) (y/n)? ',answer
  uglowagain:
  
  k='n'
  
  bans=''
  b=0
  noplot=0
;  while bans ne 'n' do begin
     while k ne 'y' or bans ne 'n' do begin 
        if answer ne 'n' then begin 
           if bans ne 'y' then begin 
              p=1
              while exist(ufile) do begin
                 ufile='under_aglow_v2_gti'+ntostr(p)+'.dat'
                 p=p+1
              endwhile
           endif 
           
           user_gtis,w,ultstarts,ultstops,_extra=_extra,part='Underlying afterglow estimation',zoom=zoom
           if b gt 0 then append=1 else append=0
           openw,lun,ufile,/get_lun,append=append
           for i=0,n_elements(ultstarts)-1 do printf,lun,ultstarts[i],ultstops[i],b
           close,lun
           free_lun,lun
           waitplot=0
        endif else begin
           p=1
           while exist(ufile) do begin
              ufile='under_aglow_v2_gti'+ntostr(p)+'.dat'
              p=p+1
           endwhile 
           ufile='under_aglow_v2_gti'+ntostr(p-2)+'.dat'
           print,'Retrieving underlying afterglow from: '+ufile
           waitplot=1
        endelse 
        
        readcol,ufile,ultstarts,ultstops,bs,/silent
        bb=bs[uniq(bs)]
        nb=n_elements(bb)
        ulnorm=dblarr(nb) & ulpow=ulnorm & ulsigma=dblarr(2,nb) & ulchisq=ulnorm & uldof=intarr(nb)
        
        for j=0,nb-1 do begin 
           wb=where(bs eq bb[j])
           for i=0,n_elements(ultstarts[wb])-1 do begin 
              if i eq 0 then wsmt='time gt '+ntostr(ultstarts[wb[i]])+' and time lt '+ntostr(ultstops[wb[i]]) else $
                 wsmt=wsmt+'or time gt '+ntostr(ultstarts[wb[i]])+' and time lt '+ntostr(ultstops[wb[i]])
              com='w=where('+wsmt+')'
              tmp=execute(com)
              if n_elements(w2) gt 0 then pw2=w2
              if i eq 0 then w2=w else w2=[w2,w]
           endfor 
           if waitplot then begin 
              noplot=1
              if j eq 0 then plot_base_lc
           endif 
           if j eq 0 then w3=w2 else w3=[w3,w2]
           
           fit_under_aglow,w,ultstarts,ultstops,yrange=yrange,_extra=_extra,zoom=zoom,noplot=noplot,w2=w2,leg=j
        endfor 
         

        k='y'
        read,k,prompt='Is this an acceptable fit? (y to use, n to redo) '
        if k eq '' then k='y'
        if k eq 'n' then begin 
           answer='y'
           w2=pw2
        endif 
;     endwhile
     input,'Add another broken PL component? ',bans,'n'   
     if bans eq 'y' then begin
        b=b+1
        noplot=1
     endif 
  endwhile
  
  print,'Writing out GTI file for Underlying Afterglow'
  
  noplot=0
  print
  answer=''
  input,'Fit flares? ',answer,'y'
  ff=0
  r=0
  norm2=0d & pow2=0d & sigma2=dblarr(2,1) & chisq2=0d & dof2=0
  while answer ne 'n' do begin
     print
     print,'Fitting Flare '+ntostr(r+1)
     printf,llun
     printf,llun,'Fitting Flare '+ntostr(r+1)
     user_gtis,f,tstarts,tstops,_extra=_extra,/flares,part='Flare '+ntostr(r+1)
     
     tleg=where(ultstarts lt tstarts[0] and ultstops gt tstops[1],nleg)
     if nleg eq 0 then tleg=0
     leg=bs[tleg]
     
     for j=0,1 do begin
        rise=abs(j-1)
        fall=j

        fit_flare_leg,tstarts[j],tstops[j],n2,p2,s2,icpt,w,chisq,dof,_extra=_extra,status=abs(j-1),zoom=zoom,leg=leg,w3=w3
        norm2=[norm2,n2]
        pow2=[pow2,p2]
        sigma2=[[sigma2],[s2]]
        if j eq 0 then tstart=icpt
        
        acc='' & line='' & bup=''
        newstart=0 & newstop=0
        flag=[0,0]
        if j eq 1 then begin
           tstop=icpt
           
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
           
           printf,fglun,tstart,tstop
        endif 
        chisq2=[chisq2,chisq]
        dof2=[dof2,dof]
     endfor 
     
     find_peaks,norm2[r*2+1],pow2[r*2+1],sigma2[*,r*2+1],norm2[r*2+2],pow2[r*2+2],sigma2[*,r*2+2],tstart,tstop,tpeak,peak,tpeak1,peak1,increase,increase1,leg=leg
;    if estat eq 'good' then calc_signif,src,bg,exp,tstart,tstop,signif,exp_fact,pufact,ulratio,medtime
     
     write_outfile,tstart,tstop,ultstarts,ultstops,norm2[r*2+1:r*2+2],pow2[r*2+1:r*2+2],sigma2[*,r*2+1:r*2+2],chisq2[r*2+1:r*2+2],dof2[r*2+1:r*2+2],tpeak,peak,tpeak1,peak1,increase,increase1,signif,flag,outfile
     
     make_outplot,r,w3,tstarts,tstops,norm2[r*2+1:r*2+2],pow2[r*2+1:r*2+2],sigma2[*,r*2+1:r*2+2],tstart,tstop,chisq2[r*2+1:r*2+2],dof2[r*2+1:r*2+2],tpeak,peak,tpeak1,peak1,increase,increase1,signif,exp_fact,pufact,ulratio,medtime,outfile,_extra=_extra,title=title
     
     r=r+1
     read,answer,prompt='Fit another flare? (y/n) '
     if answer ne 'n' then begin 
        same='y'
        read,same,prompt='Same Underlying afterglow? (y/n) '
        if same eq 'n' then begin 
;           ufile='under_aglow_v2_gti2.dat'
           goto,uglowagain
        endif 
     endif 
     ff=1
  endwhile  
  
  if ff eq 1 then begin 
     norm2=norm2[1:*]
     pow2=pow2[1:*]
     sigma2=sigma2[*,1:*]
     chisq2=chisq2[1:*]
     dof2=dof2[1:*]
     
  endif 
  
  close,/all,/file
  
;  readcol,'flares_gti.dat',tstarts,tstops,/silent
  
;  print,'See output files (*gti*dat) for GTIs'
;  print,'See output files ('+outfile+') for outputs'
;  print,'See output files ('+outfile+'.par) for inputs into flarespec'
  
  
  return
end 
  
  
