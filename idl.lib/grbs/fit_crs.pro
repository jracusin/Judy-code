@closure_relations
@cr_paper_plots
pro fit_crs,alpha,alphaerr0,alphaerr1,gamma,gammaerr0,gammaerr1,plotevery=plotevery,ps=ps,name=name,plotlc=plotlc,nocolor=nocolor,skipfirst=skipfirst,noconsis=noconsis,twocomp=twocomp,latebr=latebr,title=title,lc=lc,crstr=crstr,plotcompat=plotcompat,plotcomm=plotcomm

  if n_params() eq 0 then begin 
     print,'syntax - fit_crs,alpha,alphaerr0,alphaerr1,gamma,gammaerr0,gammaerr1'
     return
  endif 

  nsig=2.
  n=n_elements(alpha)
  if keyword_set(plotcompat) and n gt 1 then begin 
;     if n gt 1 then begin
     compat=1 
     np=n+1
  endif else begin
     compat=0
     np=n
  endelse 

  if keyword_set(plotlc) then np=np+1
  
  alphaserr=fltarr(2,n)
  alphaserr[0,*]=alphaerr0
  alphaserr[1,*]=alphaerr1
  make_crstruct,n,crstr
  crstr.alpha=alpha
  crstr.alphaerr=alphaserr
  crstr.beta=gamma-1
  betaserr=fltarr(2,n)
  betaserr[0,*]=gammaerr0
  betaserr[1,*]=gammaerr1
  crstr.betaerr=betaserr
  
  which_alpha,alpha,alphaserr,j,posas,delalp,delalperr,twocomp=twocomp,latebr=latebr
  delalp=[0,delalp]
  delalperr=[[0,0],[delalperr]]

  for j=0,n-1 do begin 
     for pa=1,4 do begin 
        w=where(posas eq pa,nw)
        if n_elements(posas) ne 4 then begin 
           if nw eq 1 then com=execute('crstr[j].seg'+ntostr(pa)+'=1') 
        endif else crstr[j].seg0=1
     endfor 
  endfor 
  erase
  multiplot2,[1,n],/init
  for i=0,n-1 do begin 
     multiplot2
     if n_elements(title) gt 0 then begin 
        if i eq 0 then ttitle=title else ttitle=''
     endif 
     which_closure_relation,alpha[i],alphaerr0[i],alphaerr1[i],gamma[i],gammaerr0[i],gammaerr1[i],cr,posas[*,i],delalp[i],delalperr[*,i],gg,qstr=qstr,pstr=pstr,chisq=chisq,/noplot,nsig=nsig,title=ttitle,twocomp=twocomp
     
     for c=0,n_elements(gg)-1 do com=execute('crstr[i].(gg[c]+6)=sqrt(chisq[gg[c]])')
     if i gt 0 then begin 
        concat_structs,qstr0,qstr,qstrs
        qstr0=qstrs
        concat_structs,pstr0,pstr,pstrs
        pstr0=pstrs
     endif else begin
        qstr0=qstr
        pstr0=pstr
     endelse 
     
;     k=get_kbrd(10)
;     if k eq 's' then stop
     
  endfor 
  multiplot2,/reset,/default
  elim=1
  if not keyword_set(noconsis) then begin 
     if not keyword_set(twocomp) then begin 
        while elim gt 0 do cr_consistency_check,crstr,elim,qstrs,pstrs,nsig=nsig
     endif else begin 
        case n_elements(crstr) of
           2: begin 
              elim=1
              while elim gt 0 do cr_consistency_check,crstr,elim,qstrs,pstrs,nsig=nsig,/onlycsm
           end 
           3: begin
;              if keyword_set(latebr) then begin 
                 crstr1=crstr[0]
                 crstr2=crstr[1:2]
                 while elim gt 0 do cr_consistency_check,crstr2,elim,qstrs,pstrs,nsig=nsig 
                 print,'______________________________________'
                 concat_structs,crstr1,crstr2,crstr
                 elim=1
                 while elim gt 0 do cr_consistency_check,crstr,elim,qstrs,pstrs,nsig=nsig,/onlycsm,/nonu
;              endif else begin 
;                 crstr1=crstr[0:1]
;                 crstr2=crstr[2]
;                 while elim gt 0 do cr_consistency_check,crstr1,elim,qstrs,pstrs,nsig=nsig 
;                 print,'______________________________________'
;                 concat_structs,crstr1,crstr2,crstr
;                 elim=1
;                 while elim gt 0 do cr_consistency_check,crstr,elim,qstrs,pstrs,nsig=nsig,/onlycsm
;              endelse 
           end
           4: begin 
              crstr1=crstr[0:1]
              crstr2=crstr[2:3]
              while elim gt 0 do cr_consistency_check,crstr1,elim,qstrs,pstrs,nsig=nsig 
              elim=1
              print,'______________________________________'
              while elim gt 0 do cr_consistency_check,crstr2,elim,qstrs,pstrs,nsig=nsig 
              concat_structs,crstr1,crstr2,crstr
              elim=1
              print,'______________________________________'
              while elim gt 0 do cr_consistency_check,crstr,elim,qstrs,pstrs,nsig=nsig,/onlycsm,/nonu
           end 
        endcase 
     endelse  
  endif  
  
;  if n_elements(segment) eq 0 then begin 
;     seg=['I','II','III','IV']
;     if cr[0].seg1 eq 0 then seg=seg[1:*]
;  endif else seg=segment
  
  if keyword_set(ps) then begin
     if n_elements(name) eq 0 then name='cr_fits.ps'
     begplot,name=name,/color
  endif 
     
  nsig0=1.
  erase
;  if keyword_set(skipfirst) then np=np+1
  multiplot2,[1,np],/init
  
  if keyword_set(plotlc) then begin 
     multiplot2,/doxaxis
     if n_elements(lc) eq 0 then $
     fit_lc,/just,/phil,nocolor=nocolor,charsize=1.,/noleg else begin 
        wd=where(lc.src_rate_err gt 0,nwd)
        ploterror,lc[wd].time,lc[wd].src_rate,lc[wd].src_rate_err,psym=3,/xlog,/ylog,/nohat,ytickf='loglabels',charsize=1,title=title
        for d=0,nwd-1 do begin 
           oplot,[lc[wd[d]].tstart,lc[wd[d]].tstop],[lc[wd[d]].src_rate,lc[wd[d]].src_rate]
        endfor 
        if n_elements(plotcomm) gt 0 then begin 
           for u=0,n_elements(plotcomm)-1 do tmp=execute(plotcomm[u])
        endif 
     endelse 
        
  endif 
  
;  ncrs=n_elements(cr)
  ncrs=46
  for j=0,n-1 do begin 
     bin=fltarr(ncrs)
     if j eq 0 then ydowngap=0.05 else ydowngap=0
     for i=0,ncrs-1 do tmp=execute('bin[i]=crstr[j].('+ntostr(i+6)+')')
     wsig=where(bin le nsig0,nwsig)
     answer=intarr(ncrs)
     if nwsig gt 0 then answer[wsig]=1
     doxaxis=0
     if j eq n-1 then doxaxis=1 
;     if keyword_set(skipfirst) then multiplot2
     if n_elements(title) gt 0 then begin 
        if j eq 0 then ttitle=title else ttitle=''
     endif 

     multiplot2,ydowngap=ydowngap,doxaxis=doxaxis,yupgap=yupgap
     if j eq n-1 then xtitle=!tsym.psi_cap+' = '+!tsym.alpha+' - f('+!tsym.beta+')' else xtitle=''
     which_closure_relation,alpha[j],alphaerr0[j],alphaerr1[j],gamma[j],gammaerr0[j],gammaerr1[j],crsp,posas[*,j],delalp[j],delalperr[*,j],gg,psc=psc,answer=answer,qstr=qstr,pstr=pstr,nsig=nsig,xtitle=xtitle,charsize=1.,nocolor=nocolor,plotevery=plotevery,incaption=incaption,twocomp=twocomp,title=ttitle
     
     if alphaerr0[j]-alphaerr1[j] eq 0 then alphaerr=!tsym.plusminus+numdec(alphaerr0[j],2) else $
        alphaerr='!S!L-'+numdec(alphaerr0[j],2)+'!R!U+'+numdec(alphaerr1[j],2)
     alphaleg=!tsym.alpha+'='+numdec(alpha[j],2)+alphaerr
     if gammaerr0[j]-gammaerr1[j] eq 0 then betaerr=!tsym.plusminus+numdec(gammaerr0[j],2) else $
        betaerr='!S!L-'+numdec(gammaerr0[j],2)+'!R!U+'+numdec(gammaerr1[j],2)
     betaleg=!tsym.beta+'='+numdec(gamma[j]-1,2)+betaerr
     legend,[alphaleg,' ',betaleg],/bottom,/right,box=0,charsize=1
  endfor
  
  if keyword_set(compat) then begin 
     multiplot2,ydowngap=0.05
     plot_compatible_crs,crstr,0,charsize=0.8,nocolor=nocolor
  endif 
  
  k=get_kbrd(10)
  if k eq 's' then stop
  multiplot2,/reset,/default
  if keyword_set(ps) then endplot
  
  return
end 
