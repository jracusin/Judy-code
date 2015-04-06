pro fit_flares,time,timerr,cts,err,tstart,tstop,norm1,pow1,norm2,pow2,sigma1,sigma2,icpt,fitunder=fitunder,_extra=_extra,w=w
  
  if n_params() eq 0 then begin
     print,'syntax - fit_flares,time,timerr,cts,err,tstart,tstop,norm1,pow1,norm2,pow2,icpt'
     return
  endif 
  
  clr=!p.color
  xtitle='Seconds since BAT trigger'
  ytitle='Counts s!U-1!L'
  if keyword_set(fitunder) then begin 
     if n_elements(w) eq 0 then w=indgen(n_elements(time))
     xerr=(alog10(time[w]+timerr[w])-alog10(time[w]-timerr[w]))/2.
     yerr=(alog10(cts[w]+err[w])-alog10(cts[w]-err[w]))/2.
     
     fitexy,alog10(time[w]),alog10(cts[w]),norm1,pow1,x_sig=xerr,y_sig=yerr,sigma1,chisq
     print,norm1,sigma1[0]
     print,pow1,sigma1[1]
     ploterror,time,cts,timerr,err,psym=3,/nohat,/xlog,/ylog,_extra=_extra,xtitle=xtitle,ytitle=ytitle
     oplot,time,10^(norm1)*time^pow1
     dof=n_elements(w)+2
     legend,['Norm1 = '+ntostr(norm1,4)+!tsym.plusminus+ntostr(sigma1[0],5),$
             'Pow1 = '+ntostr(pow1,4)+!tsym.plusminus+ntostr(sigma1[1],5),$
            !tsym.chi+'!U2!N = '+ntostr(chisq/dof)],$
        /top,/right,box=0;,charcolor=!red
     
  endif else begin 

     q=where(time gt tstart and time lt tstop,nq)
     xerr=(alog10(time+timerr)-alog10(time-timerr))/2.
     yerr=(alog10(cts+err)-alog10(cts-err))/2.
     fitexy,alog10(time[q]),alog10(cts[q]),norm2,pow2,x_sig=xerr[q],y_sig=yerr[q],sigma2,chisq
;     fit=linfit(alog10(time[q]),alog10(cts[q]),measure_errors=yerr[q],chisq=chisq,sigma=sigma2)
;     norm2=fit[0]
;     pow2=fit[1]
     
     icpt=10d^(norm1-norm2)^(1d/(pow2-pow1))
     icpterr=sqrt((sigma2[0]/norm2)^2+(sigma2[1]/pow2)^2+(sigma1[0]/norm1)^2+(sigma1[1]/pow1)^2)*icpt
 ;    peak=max(cts[q],p)
 ;    tpeak=time[q[p]]
 ;    pfit=((10d^norm1)*tpeak^pow1)
 ;    print,tpeak
 ;    print,'peak cts,peak fit:        ' ,peak,pfit
 ;    pfact=peak/pfit
 ;    psig=(peak-pfit)/sqrt(peak+pfit)
     
     print,'    Underlying afterglow: ',norm1,pow1
     print,'    Flare:                ',norm2,pow2
     print,'    Intercept (s):        ',ntostr(icpt)+' +/- '+ntostr(icpterr)
 ;    print,'    Tpeak:                ',ntostr(tpeak)
 ;    print,'    Peak factor:          ',ntostr(pfact)
 ;    print,'    Peak sig:             ',ntostr(psig)
     
     ploterror,time,cts,timerr,err,psym=3,/nohat,/xlog,/ylog,_extra=_extra,xtitle=xtitle,ytitle=ytitle
     if n_elements(w) ne 0 then oploterror,time[w],cts[w],timerr[w],err[w],/nohat,psym=3,errcolor=!red,color=!red
     oploterror,time[q],cts[q],timerr[q],err[q],color=!blue,psym=3,errcolor=!blue,/nohat
     oplot,time,10d^(norm1)*time^pow1,color=!red
     oplot,time,10d^(norm2)*time^pow2,color=!blue
;     plots,time[q[p]],cts[q[p]],psym=1,color=!green
     
     dof=nq+2
     legend,['Norm1 = '+ntostr(norm1,4)+!tsym.plusminus+ntostr(sigma1[0],5),$
             'Pow1 = '+ntostr(pow1,5)+!tsym.plusminus+ntostr(sigma1[1],5),$
             'Norm2 = '+ntostr(norm2,4)+!tsym.plusminus+ntostr(sigma2[0],5),$
             'Pow2 = '+ntostr(pow2,5)+!tsym.plusminus+ntostr(sigma2[1],5),$
             'Intercept = '+ntostr(icpt,6)+!tsym.plusminus+ntostr(icpterr,6), $
;             'Tpeak = '+ntostr(tpeak),$
     !tsym.chi+'!U2!N = '+ntostr(chisq/dof)],$
        /top,/right,textcolor=[!red,!red,!blue,!blue,clr,clr],box=0
                        
  endelse 
  return
end 
