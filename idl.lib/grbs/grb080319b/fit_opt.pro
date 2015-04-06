@fit_functions
pro fit_opt,expo=expo,ps=ps,newt0=newt0,bkn=bkn,old=old,plotchi=plotchi,noleg=noleg,pow=pow,yoni=yoni
  
  if not keyword_set(pow) and not keyword_set(expo) and not keyword_set(bkn) and not keyword_set(yoni) then begin
     print,'need to specify model: exp, pow, bkn, yoni'
     return
  endif 
  
  dir='~/Desktop/GRB080319B/'
  cd,dir
  
  if keyword_set(old) then olc=mrdfits('composite_lc/Normalized_ground_UVOT.fits',1) else olc=mrdfits('composite_lc/Normalized_ground_UVOT_FINAL2.fits',1)
  multiplot2,/reset,/default
  
  s=sort(olc.time)
  olc=olc[s]
  
  q=where(olc.time ge 43. and olc.fluxerr ne 0. and olc.flux gt 0 and (strtrim(olc.filter,2) ne 'H' or olc.time gt 1e3) and olc.time lt 4e6,nq); and strtrim(olc.filter,2) ne 'J',nq) ; and olc.time le 1e5,nq)
;  q=where(olc.time ge 45. and olc.fluxerr ne 0. and strtrim(olc.filter) ne 'H' and olc.time le 1e5,nq)

  olc=olc[q]
  time=olc.time
;  wf=where(olc.fluxerr/olc.flux lt 0.1)
  fluxerr=olc.fluxerr
  flux=olc.flux
;  if keyword_set(old) then sys=9.5 else 
  sys=0.07
  fluxerr=sqrt(fluxerr^2+(sys*flux)^2)
  if keyword_set(old) then wd='Old' else wd='New'

;  ufilter=olc[rem_dup(olc.filter)].filter
;  ufilter=ufilter[[1]];,5,13]]
;  ufilter=[ufilter,'All        ']
  ufilter='All        '
  nu=n_elements(ufilter)
  fstr=create_struct('filter','','tau',0.,'tau_err',fltarr(2),$
                     'pow1',0.,'pow1_err',fltarr(2),'pow2',0.,'pow2_err',fltarr(2),$
                     'num',0,'chisq',0.,'dof',0,'tmin',0.,'tmax',0.)
  fstr=replicate(fstr,nu)
  
;  !p.multi=[0,4,4]
;  !p.multi=[0,2,2]
  !p.multi=0
  for i=0,nu-1 do begin 
;     if i lt nu-1 then w=where(olc.filter eq ufilter[i],nw) else begin
        w=indgen(nq) & nw=nq
;        w=where(strtrim(olc.filter,2) ne 'H',nw)
 ;    endelse 
     
 ;    print,nw
     tt=dblarr(2,nw)
     tt[0,*]=olc[w].tstart
     tt[1,*]=olc[w].tstop
     
     tmin=min(tt)
     tmax=max(tt)     
     
     fadd=''
     if keyword_set(plotchi) then fadd='_chisq'

;     if tmin lt 200 and tmax gt 3000 then begin 
        if keyword_set(expo) then begin
           model='exp_pow_pow'
           intmodel='int_exp_pow_pow'
           p=[1e6,9.,1.e6,2.5,1.e3,1.2]*1d
;           p=[1e6,9.,1.e6,2.7,1e4,2.0,1.e2,1.2]*1d
           pname=['Norm1','tau','Norm2','alpha1','Norm3','alpha2'];,'Norm4','alpha3']
           np=n_elements(p)
           parinfo = parinfo_struct(np)
           parinfo.value=p
           parinfo.parname=pname
           parinfo[0].limits=[0,0] ;;norm1
           parinfo[0].limited=[1,0]
           parinfo[1].limits=[0,0] ;;tau
           parinfo[1].limited=[1,0]
           parinfo[2].limits=[0,0] ;;norm2
           parinfo[2].limited=[1,0]
           parinfo[3].limits=[1.5,0] ;;alpha1
           parinfo[3].limited=[1,0]
           parinfo[4].limits=[0,0] ;;norm3
           parinfo[4].limited=[1,0]
           parinfo[5].limits=[1,0] ;;alpha2
           parinfo[5].limited=[1,0]
;           parinfo[6].limits=[0,0] ;;norm4
;           parinfo[6].limited=[1,0]
;           parinfo[7].limits=[0,0] ;;alpha3
;           parinfo[7].limited=[1,0]
;           title='Exp+Pow+Pow ('+wd+' LC, no H, +syserr)'
           title=''
           lname=[!tsym.tau+'=',!tsym.alpha+'1=',!tsym.alpha+'2=']
           ma=1
           add=',fe,fp1,fp2';,fp3'
        endif
        if keyword_set(bkn) then begin
           model='bkn2pow'
           intmodel='intbkn2pow'
           p=[1.e11,4.,100.,2.1,1000.,1.1]*1d
           title='Double Broken Pow (Old LC, no H)'
           lname=['alpha1=','alpha2=','alpha3=']
           add=''
           ma=6
        endif 
        if keyword_set(pow) then begin
           model='pow_pow_pow'
           intmodel='int_pow_pow_pow'
;           p=[1e20,4,1e4,1.3]
           p=[1.e15,9.,1.e6,2.5,1.e3,1.3]*1d
;           title='Pow+Pow+Pow ';('+wd+' LC, no H, +syserr)'
           
           if keyword_set(newt0) then begin
;              st0='39'
;              st0='30'
              title=title+' (T0='+st0+'s)'
              fadd='_t0'+st0+fadd
           endif else begin
;              title=title+' (T0=0s)'
              fadd='_t00'+fadd
           endelse 
;           lname=['alpha1=','alpha2=','alpha3=']
           lname=[!tsym.alpha+'1=',!tsym.alpha+'2=',!tsym.alpha+'3=']
           ma=1
           add=',fe,fp1,fp2'
;           add=',fp1,fp2'
        endif
        if keyword_set(yoni) then begin
           model='yoni_function'
           beta=0.54;0.7
           pp=2.*beta+1.
           alpha4=3.*(pp-1)/4.
           s2=1.84-0.4*pp
;           p=[1e3,6.,1.,60.,-1.,2.,5.,1.,1000.,-0.5,alpha4,s2]*1d
           p=[1e9,4.,1.,60.,-1.,2.,5.,1.,1000.,-0.5,alpha4,s2]*1d
           title=''
           lname=[!tsym.alpha+'1=',!tsym.alpha+'2=',!tsym.alpha+'3=',!tsym.alpha+'4=']
           pname=['Norm1','tau','Norm2','t0','alpha1','alpha2','s1','Norm3','t1','alpha3','alpha4','s2']
           np=n_elements(p)
           parinfo = parinfo_struct(np)
           parinfo.value=p
           parinfo.parname=pname
           parinfo[0].limits=[0,0] ;;norm1
           parinfo[0].limited=[1,0]
           parinfo[1].limits=[0,0] ;;tau
           parinfo[1].limited=[1,0]
           parinfo[2].limits=[0,0] ;;norm2
           parinfo[2].limited=[1,0]
           parinfo[3].limits=[40,100] ;;t0
           parinfo[3].limited=[1,1]
;           parinfo[4].limits=[-2,-0.5] ;;alpha1
;           parinfo[4].limited=[1,1]
           parinfo[4].fixed=1
           parinfo[5].fixed=1 ;;alpha2
;           parinfo[5].limits=[1.8,2.2] ;;alpha2
;           parinfo[5].limited=[1,1]
           parinfo[6].limits=[1,10] ;;s1
           parinfo[6].limited=[1,1]
           parinfo[7].limits=[0,0] ;;norm3
           parinfo[7].limited=[1,0]           
           parinfo[8].limits=[100,0] ;;t1
           parinfo[8].limited=[1,0]
           parinfo[9].fixed=1 ;;alpha3
;           parinfo[9].limits=[-1,0] ;;alpha3
;           parinfo[9].limited=[1,1]
           parinfo[10].fixed=1 ;;alpha4
;           parinfo[10].limits=[0.5,0] ;;alpha3
;           parinfo[10].limited=[1,0]
           parinfo[11].fixed=1
           add=',fe,fp1,fp2'
           ma=7
           fixed=intarr(12)
           wfix=where(parinfo.fixed eq 1)
           fixed[wfix]=1
           par=[1,3,6,8]
        endif 
        
 ;    endif else begin
 ;       if tmin lt 200 and tmax lt 3000 then begin
;           model='exp_pow'
;           intmodel='int_exp_pow'
;           p=[1.e6,9.,1.e6,2.2]*1d
;           ma=2
;           add=',fe,fp1'
;        endif 
;        if tmin gt 500 and tmax lt 3000 then begin 
;           model='pow'
;           intmodel='intpow'
;           p=[1.e6,2.2]*1d
;           ma=3
;           add=''
;        endif 
;        if tmin gt 1000 then begin 
;           model='pow_pow'
;           intmodel='int_pow_pow'
;           p=[1.e6,2.2,1.e3,1.1]*1d
;           add=',fp1,fp2'
;           ma=4
;        endif 
;        if tmin gt 2000 then begin 
;           model='pow'
;           intmodel='intpow'
;           p=[1.e3,1.1]*1d
;           ma=5
;           add=''
;        endif 
;     endelse 

     tmp=execute('mo='+model+'(time[w],p'+add+')')
     if model eq 'pow' then fp1=mo
     
     if keyword_set(ps) then begin
        begplot,name=model+'_all_opt'+fadd+'.eps',/color,/land,font='helvetica',/encaps,/cmyk
        thick=2
     endif 
     colors=[!hotpink,!cyan,!purple,!violet,!salmon,!magenta,!navyblue,!darkred,!orange,!firebrick,!orangered,!green,!forestgreen,!royalblue,!turquoise,!seagreen]

     erase     
     multiplot2,[1,2],/init
     multiplot2,ydowngap=0.,xgap=0.05
;     ufilter=olc[w[rem_dup(olc[w].filter)]].filter
     ufilter=['UVOT white','UVOT v','UVOT b','UVOT u','UVOT uvw1','UVOT uvm2','UVOT uvw2','B','V','R','SDSS-r','I','SDSS-i','J','H','K']
;     wfil=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,15]
;     ufilter=ufilter[wfil]
;     colors=colors[wfil]
     
;     ploterror,olc[w].time,olc[w].flux,olc[w].tbin/2.,fluxerr[w],psym=3,/nohat,/xlog,/ylog,yrange=[1e-4,1e5],xrange=[10,1e7],/xsty,/ysty,ytitle='Flux (mJy)',title=title,yminor=9 ;,title=ufilter[i]
     plot,[10,1e7],[1e-4,1e5],/nodata,/xlog,/ylog,yrange=[1e-5,1e5],xrange=[10,1e7],/xsty,/ysty,ytitle='Flux Density (mJy)',title=title,yminor=9,yticks=9,ytickf='loglabels' ;,title=ufilter[i]
     for j=0,n_elements(ufilter)-1 do begin 
        f=where(strtrim(olc[w].filter,2) eq ufilter[j],nf)
        if nf gt 0 then oploterror,time[w[f]],olc[w[f]].flux,olc[w[f]].tbin/2.,fluxerr[w[f]],psym=3,/nohat,errcolor=colors[j]
     endfor 

;  oplot,time,mo,color=!red

;  model='exp_pow_pow'
;  tt=time
;     model='int_exp_pow_pow'

     if not keyword_set(yoni) then newp=mpfitfun(intmodel,tt,olc[w].flux,fluxerr[w],p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo)
     if keyword_set(yoni) then newp=mpfitfun(model,time[w],olc[w].flux,fluxerr[w],p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo)

     chisq=total(((olc[w].flux-yfit)/fluxerr[w])^2)
     dof=nw-6
     oplot,time[w],yfit,thick=thick         ;,color=!green
;     mo=exp_pow_pow(time[w],newp,fe,fp1,fp2)
     tmp=execute('mo='+model+'(time[w],newp'+add+')')

     if ma ne 6 then begin 
        if ma eq 1 or ma eq 2 or ma eq 7 then oplot,time[w],fe,line=1,thick=thick ;,color=!red
        oplot,time[w],fp1,line=2,thick=thick ;,color=!red
        if ma eq 1 or ma eq 7 then oplot,time[w],fp2,line=3,thick=thick ;,color=!red
;        oplot,time[w],fp3,line=3,thick=thick ;,color=!red
     endif 
     stop     
;     if tmin lt 100 and tmax gt 1e4 then $
;     if nw gt 50 then $
     delchi0=chisqr_cvf(0.1,n_elements(newp))
     print,delchi0

     conf_error,tt,olc[w].flux,fluxerr[w],newp,perror,intmodel,perror2,pvarunit,bestchisq,yfits,log=log,pmin0=pmin0,delchi0=delchi0
;     stop
;     conf_error,time,olc[w].flux,fluxerr[w],newp,perror,model,perror2,pvarunit,bestchisq,yfits,log=log,pmin0=pmin0,fixed=fixed,par=par
;           perror2=fltarr(2,6)
     
     if not keyword_set(noleg) then legend,[lname+sigfig(newp[[1,3,5]],3)+'!S!E+'+sigfig(perror2[1,[1,3,5]],3)+'!R!I-'+sigfig(perror2[0,[1,3,5]],3),!tsym.chi+'!U2!N/dof='+sigfig(chisq,5)+'/'+ntostr(dof)+'='+sigfig(chisq/dof,3)],box=0,/top,/right
     
     
     upos=strpos(ufilter,'UVOT')
     ww=where(upos ne -1,nww)
     ufilt=ufilter
     for up=0,nww-1 do ufilt[up]=strmid(ufilter[up],upos[up]+5,10)
     
;     colors=[!hotpink,!cyan,!purple,!violet,!salmon,!magenta,!navyblue,!darkred,!orange,!firebrick,!orangered,!green,!forestgreen,!royalblue,!turquoise,!seagreen]
     legend,[ufilt[0:6]]+'                ',textcolor=colors[0:6],box=0,/top,/right
     legend,[ufilt[7:*]],textcolor=colors[7:*],box=0,/top,/right
     
;     seds=[150,350,600,5856,1.17e4,7.89e4,2.2e5]
;     for ss=0,6 do oplot,[seds[ss],seds[ss]],[1e-4,1e5]
     
     multiplot2,ydowngap=0.2
     
     chi=((olc[w].flux-yfit)/fluxerr[w])
     wneg=where(chi lt 0)
     chi=chi^2                  ;*chisq                   ;^2;-chisq
     chi[wneg]=chi[wneg]*(-1.)
     chierr=replicate(1.,nw);fluxerr[w];/abs(chi)
;     chierr=max(abs(chi))*fluxerr[w]/olc[w].flux
     
     if keyword_set(plotchi) then begin 
        plot,[10,1e7],[-10,10],/nodata,xtitle='Time since BAT trigger (s)',ytitle=!tsym.chi+'!U2!N contrib.',/xlog
        for j=0,n_elements(ufilter)-1 do begin 
           f=where(strtrim(olc[w].filter,2) eq ufilter[j],nf)
           oploterror,time[w[f]],chi[f],chierr[f],errcolor=colors[j],psym=3,/nohat
           for ff=0,nf-1 do oplot,[olc[w[f[ff]]].tstart,olc[w[f[ff]]].tstop],[chi[f[ff]],chi[f[ff]]],color=colors[j]
        endfor 
        oplot,[10,1e8],[0,0]
     endif else begin 
        ploterror,[10,1e7],[0,3],/nodata,/nohat,xrange=[10,1e7],yrange=[0,3],/xlog,/xsty,xtitle='time since BAT trigger (s)',ytitle='data/model'
        for j=0,n_elements(ufilter)-1 do begin 
           f=where(strtrim(olc[w].filter,2) eq ufilter[j],nf)
           oploterror,time[w[f]],olc[w[f]].flux/yfit[f],olc[w[f]].tbin/2.,fluxerr[w[f]]/yfit[f],psym=3,/nohat,errcolor=colors[j]
        endfor 
        oplot,[10,1e8],[1,1]
     endelse 
     
     print,chisq,dof,chisq/dof
     fstr[i].filter=ufilter[i]
     if ma eq 1 or ma eq 2 or ma eq 6 then begin 
        fstr[i].tau=newp[1]
        fstr[i].tau_err=perror2[*,1]
        fstr[i].pow1=newp[3]
        fstr[i].pow1_err=perror2[*,3]
        if ma eq 1 then begin 
           fstr[i].pow2=newp[5]
           fstr[i].pow2_err=perror2[*,5]
        endif 
     endif 
     if ma eq 4 then begin 
        fstr[i].pow1=newp[1]
        fstr[i].pow1_err=perror2[*,1]
        fstr[i].pow2=newp[3]
        fstr[i].pow2_err=perror2[*,3]
     endif
     if ma eq 3 then begin
        fstr[i].pow1=newp[1]
        fstr[i].pow1_err=perror2[*,1]
     endif 
     if ma eq 5 then begin 
        fstr[i].pow2=newp[1]
        fstr[i].pow2_err=perror2[*,1]
     endif 
     fstr[i].num=nw
     fstr[i].chisq=chisq
     fstr[i].dof=dof
     fstr[i].tmin=tmin
     fstr[i].tmax=tmax
     
                                ;    print,perror2
  endfor 
  multiplot2,/reset,/default
  
  if keyword_set(ps) then endplot
;  !p.multi=0
  
  w0=where(fstr.pow2_err[0] ne 0.)
;  colprint,fstr[w].filter,fstr[w].tau,fstr[w].tau_err,fstr[w].pow1,fstr[w].pow1_err,fstr[w].pow2,fstr[w].pow2_err,fstr[w].num,fstr[w].chisq,fstr[w].dof,fstr[w].chisq/fstr[w].dof,fstr[w].tmin,fstr[w].tmax
  colprint,strmid(fstr[w0].filter,0,8),fstr[w0].tau,fstr[w0].tau_err[0],fstr[w0].tau_err[1],fstr[w0].pow1,fstr[w0].pow1_err[0],fstr[w0].pow1_err[1],fstr[w0].pow2,fstr[w0].pow2_err[0],fstr[w0].pow2_err[1],fstr[w0].chisq/fstr[w0].dof
  
  
  
  stop
  return
end 
