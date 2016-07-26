;@fit_lc
pro lc_monte_wrap,dir,nsim=nsim,int=int,nsig=nsig
  
  cd,!mdata
  if n_elements(dir) eq 0 then dir=file_search('GRB*')
  ndir=n_elements(dir)
  g=0
  nw=ndir
  stop
  for i=g,nw-1 do begin
     cd,dir[i]
     print,dir[i],i
     lfile='lc_fit_out_idl_int9.dat'
     int='9'
;     if not exist(lfile) then begin
;        lfile='lc_fit_out_idl_int7.dat'
;        int='7'
;     endif 
     if exist(lfile) then begin 
        if numlines(lfile) gt 2 then begin 
;           if exist('flares_gtis.dat') or not
;           exist('lc_fit_out_idl_int7.dat') then $
           lc=lcout2fits(/phil)
           read_lcfit,lfile,pnames,p,perror,chisq,dof,breaks
           w0=where(perror eq 0,nw0)
           if nw0 gt 0 then $
              lc_monte_pow,lc,p,pnames,chisq,dof,outperr,breaks=breaks,/noplot,nsim=nsim,int=int,nsig=nsig
        endif 
     endif 
     cd,'..'
  endfor 
  
  return
end 

pro lc_monte_pow,lc0,p0,pnames,chisq,dof,outperr,noplot=noplot,ps=ps,nsim=nsim,nowrite=nowrite,int=int,nsig=nsig,file=file,uvot=uvot,fflux=fflux,flux=flux,breaks=breaks,mcfit=mcfit,mo=mo,add=add,outdir=outdir
  
  pt=systime(1)
  if n_elements(int) eq 0 then begin 
     int='9'
;     if n_elements(nsim) gt 0 then begin 
;        if nsim eq 1000 then int='7' else int='8'
;     endif else nsim=10000
  endif 
  if n_elements(nsim) eq 0 then begin
     print,'Need to define nsim'
     stop
  endif 
  print,'n = ',nsim
  print,'Using int',int
  if n_elements(add) eq 0 then add=''
  if n_elements(outdir) eq 0 then outdir=''

  ;;; 2 sigma confidence level
  if n_elements(nsig) ne 0 then begin 
     if nsig eq 1 then conf=0.68
     if nsig eq 2 then conf=0.9544997 
     if nsig eq 3 then conf=0.997
  endif else conf=0.9
  
  conf1=(1.-conf)/2.
  conf2=(1.-conf)/2.+conf
  print,'Using '+ntostr(conf)+' confidence interval'
  ;;; 90% confidence level
;  conf1=0.05
;  conf2=0.95
  
  if n_elements(lc0) eq 0 then begin 
     lc=lcout2fits(file,/phil)
  endif else lc=lc0
  if exist('flares_gtis.dat') then begin
     readcol,'flares_gtis.dat',f1,f2,format='(f,f)'
     for i=0,n_elements(f1)-1 do begin
        w=where(lc.time lt f1[i] or lc.time gt f2[i])
        lc=lc[w]
     endfor 
  endif 
  w=where(lc.src_rate_err gt 0,nlc)
;  nlc=n_elements(lc)
  exptime=lc[w].exptime
  corr_fact=lc[w].pu_corr
;  ncounts=exptime*lc.src_rate
;  flux=1.
  back=0
  if n_elements(fflux) eq 0 and n_elements(flux) eq 0 then begin 
     flux=1. 
     fflux=1.
  endif else begin 
     if fflux ne 1 then begin 
        flux=fflux
;        if min(lc.src_rate) lt 1e-7 then flux=1./fflux
        back=1
     endif 
  endelse 
  print,flux,fflux

  ncounts=lc[w].src_counts+lc[w].tot_back_cts
  backcounts=lc[w].tot_back_cts
  wb0=where(backcounts eq 0,nwb0)
  if nwb0 gt 0 then backcounts[wb0]=1e-4
  wdet=where(lc[w].src_rate_err gt 0)
  time=lc[w].time
  timerr=fltarr(2,nlc)
  timerr[0,*]=lc[w].time-lc[w].tstart
  timerr[1,*]=lc[w].tstop-lc[w].time
  print,'p0 = ',p0
  if n_elements(p0) eq 0 then begin 
     lcfile='lc_fit_out_idl_int8.dat'
     if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
     if not exist(lcfile) then lcfile='lc_fit_out_idl_int6.dat'
     read_lcfit,lcfile,pnames,p,perror,chisq,dof,breaks,np=np,nf=nf
  endif else begin
     p=p0
     perror=p*0.1
  endelse 
;  np=n_elements(p)
  p[0]=p[0]/flux
;  if n_elements(breaks) ne 0 then np=breaks*2+2
;  nflare=fix((np-(breaks*2+1))/3.)
;  fa=nflare*3
;  if fa gt 0 then begin 
;     moadd='gauss'+ntostr(fix(fa/3.))+'_' 
;  endif else moadd=''
  if n_elements(mo) eq 0 then mo=fit_models(pnames,p,np,nf) else begin
     nf=1
     np=n_elements(p)-nf*3
     ;; totally a special case
  endelse 
  np=np+nf*3
  fa=np-nf*3
;  if fa eq np then fa=0
fa=0
  intmo='int'+mo
  t=0
  if keyword_set(uvot) then begin 
     cnp=np+1
;     np=np-1
  endif 
  case breaks of
     0: begin
;        mo='pow'
;        intmo='intpow'
        t=time
     end 
     1: begin 
;        mo='bknpow'
;        intmo='intbknpow'
        w1=where(time gt 0 and time le p[fa+2],nw1)
        w2=where(time gt p[fa+2],nw2)
        if nw1 gt 0 then t=time[w1]
        t=[t,p[fa+2]]
        if nw2 gt 0 then t=[t,time[w2]]
;        t=[time[w1],p[2],time[w2]]
     end 
     2: begin 
;        mo='bkn2pow'
;        intmo='intbkn2pow'
        w1=where(time gt 0 and time le p[fa+2],nw1)
        w2=where(time gt p[fa+2] and time lt p[fa+4],nw2)
        w3=where(time gt p[fa+4],nw3)
        if nw1 gt 0 then t=time[w1]
        t=[t,p[fa+2]]
        if nw2 gt 0 then t=[t,time[w2]]
        t=[t,p[fa+4]]
        if nw3 gt 0 then t=[t,time[w3]]
;        t=[time[w1],p[2],time[w2],p[4],time[w3]]
     end 
     3: begin 
;        mo='bkn3pow'
;        intmo='intbkn3pow'
        w1=where(time gt 0 and time le p[fa+2],nw1)
        w2=where(time gt p[fa+2] and time lt p[fa+4],nw2)
        w3=where(time gt p[fa+4] and time lt p[fa+6],nw3)
        w4=where(time gt p[fa+6],nw4)
        if nw1 gt 0 then t=time[w1]
        t=[t,p[fa+2]]
        if nw2 gt 0 then t=[t,time[w2]]
        t=[t,p[fa+4]]
        if nw3 gt 0 then t=[t,time[w3]]
        t=[t,p[fa+6]]
        if nw4 gt 0 then t=[t,time[w4]]
;        t=[time[w1],p[2],time[w2],p[4],time[w3],p[6],time[w4]]
     end 
     4: begin 
;        mo='bkn4pow'
;        intmo='intbkn4pow'
        w1=where(time gt 0 and time le p[fa+2],nw1)
        w2=where(time gt p[fa+2] and time lt p[fa+4],nw2)
        w3=where(time gt p[fa+4] and time lt p[fa+6],nw3)
        w4=where(time gt p[fa+6] and time lt p[fa+8],nw4)
        w5=where(time gt p[fa+8],nw5)
        if nw1 gt 0 then t=time[w1]
        t=[t,p[fa+2]]
        if nw2 gt 0 then t=[t,time[w2]]
        t=[t,p[fa+4]]
        if nw3 gt 0 then t=[t,time[w3]]
        t=[t,p[fa+6]]
        if nw4 gt 0 then t=[t,time[w4]]
        t=[t,p[fa+8]]
        if nw5 gt 0 then t=[t,time[w5]]
;        t=[time[w1],p[2],time[w2],p[4],time[w3],p[6],time[w4],p[8],time[w5]]
     end 
  endcase
  if t[0] eq 0 then t=t[1:*]
  if keyword_set(uvot) then begin
     intmo='c'+intmo
     np=cnp
  endif 
;  if moadd eq '' then begin 
  timeerr=timerr[*,wdet]
;  if nf gt 0 then begin 
;     intmo=mo
;     noint=1
;     timeerr=timerr[0,wdet]
;  endif ;else noint=0
;  mo=moadd+mo

  tmp=execute('fp='+mo+'(t,p)')
  if keyword_set(uvot) then fp=fp+p[np-1]
  if n_elements(nsim) eq 0 then n=10000 else n=nsim
  pp=fltarr(np,n)
  seed=53464813d
  for i=0,n-1 do begin ;; loop over each simulated fit
     if i mod 100 eq 0 then print,'.',format='(a,$)'
     fcounts=fltarr(nlc)
     bcounts=fcounts
     for j=0,nlc-1 do begin ;;loop over each data point
        fcounts[j]=randomu(seed,1.,poi=ncounts[j])
        bcounts[j]=randomu(seed,1.,poi=backcounts[j])
     endfor 
;     frate=fcounts/exptime
;     fraterr=sqrt(fcounts)/exptime
     frate=(fcounts-bcounts)*corr_fact/exptime
     fraterr=sqrt(fcounts+bcounts)*corr_fact/exptime

     wdet=where(frate gt 0)
     ;;do fit
     fit_pow_model,time[wdet],frate[wdet],timeerr,fraterr[wdet],p,intmo,pnames,yfit,newp,perror,chisq2,dof2,weights,status=status,/silent,breaks=breaks,noint=noint,uvot=uvot

     if not finite(newp[0]) then newp=p
     if not keyword_set(noplot) then begin 
;        !p.multi=[0,1,2]
;        !x.omargin=[4,2]
;        !x.margin=[8,2]
;        !y.margin=[4,4]
;        ploterror,time[wdet],lc[wdet].src_rate,timerr[0,wdet],lc[wdet].src_rate_err,/xlog,/ylog,psym=3,/nohat,xtitle='time',ytitle='counts/s',yrange=yrange
;        oplot,t,fp,color=!green

     endif 

     t2=0
     if keyword_set(uvot) then np=np-1
     case breaks of
        0: begin
           t2=time
        end 
        1: begin 
           w1=where(time gt 0 and time le newp[fa+2],nw1)
           w2=where(time gt newp[fa+2],nw2)
           if nw1 gt 0 then t2=time[w1]
           t2=[t2,newp[fa+2]]
           if nw2 gt 0 then t2=[t2,time[w2]]
;           t2=[time[w1],newp[fa+2],time[w2]]
        end 
        2: begin 
           w1=where(time gt 0 and time le newp[fa+2],nw1)
           w2=where(time gt newp[fa+2] and time lt newp[fa+4],nw2)
           w3=where(time gt newp[fa+4],nw3)
           if nw1 gt 0 then t2=time[w1]
           t2=[t2,newp[fa+2]]
           if nw2 gt 0 then t2=[t2,time[w2]]
           t2=[t2,newp[fa+4]]
           if nw3 gt 0 then t2=[t2,time[w3]]
;           t2=[time[w1],newp[fa+2],time[w2],newp[fa+4],time[w3]]
        end 
        3: begin 
           w1=where(time gt 0 and time le newp[fa+2],nw1)
           w2=where(time gt newp[fa+2] and time lt newp[fa+4],nw2)
           w3=where(time gt newp[fa+4] and time lt newp[fa+6],nw3)
           w4=where(time gt newp[fa+6],nw4)
           if nw1 gt 0 then t2=time[w1]
           t2=[t2,newp[fa+2]]
           if nw2 gt 0 then t2=[t2,time[w2]]
           t2=[t2,newp[fa+4]]
           if nw3 gt 0 then t2=[t2,time[w3]]
           t2=[t2,newp[fa+6]]
           if nw4 gt 0 then t2=[t2,time[w4]]
;          t2=[time[w1],newp[fa+2],time[w2],newp[fa+4],time[w3],newp[fa+6],time[w4]]
        end 
        4: begin 
           w1=where(time gt 0 and time le newp[fa+2],nw1)
           w2=where(time gt newp[fa+2] and time lt newp[fa+4],nw2)
           w3=where(time gt newp[fa+4] and time lt newp[fa+6],nw3)
           w4=where(time gt newp[fa+6] and time lt newp[fa+8],nw4)
           w5=where(time gt newp[fa+8],nw5)
           if nw1 gt 0 then t2=time[w1]
           t2=[t2,newp[fa+2]]
           if nw2 gt 0 then t2=[t2,time[w2]]
           t2=[t2,newp[fa+4]]
           if nw3 gt 0 then t2=[t2,time[w3]]
           t2=[t2,newp[fa+6]]
           if nw4 gt 0 then t2=[t2,time[w4]]
           t2=[t2,newp[fa+8]]
           if nw5 gt 0 then t2=[t2,time[w5]]
;           t2=[time[w1],newp[fa+2],time[w2],newp[fa+4],time[w3],newp[fa+6],time[w4],newp[8],time[w5]]
        end 
     endcase
     if t2[0] eq 0 then t2=t2[1:*]

     tmp=execute('fnewp='+mo+'(t2,newp)')
     if keyword_set(uvot) then begin 
        np=cnp
        fnewp=fnewp+newp[np-1]
     endif 
     if not keyword_set(noplot) then begin 
;        !x.omargin=[10,2]
;        !x.margin=[12,2]
;        !y.margin=[6,6]
        if i mod 10 eq 0 then begin 
           ploterror,time[wdet],frate[wdet],timerr[0,wdet],fraterr[wdet],/xlog,/ylog,psym=3,/nohat,xtitle='time',ytitle='counts/s',yrange=[1e-4,1e4] ;yrange
           oplot,t2,fnewp,color=!green
           !p.multi=0
        endif 
;k=get_kbrd(10)
     endif 
     pp[*,i]=newp
;     pp[*,i]=newp[fa:*]
;     print,newp,chisq2
;     k=get_kbrd(10)
;     if k eq 's' then stop
  endfor 

  if keyword_set(ps) then begplot,name='lc_monte_err_plots_int'+int+'.eps',/encap;,/land
;  !x.omargin=[12,2]
;  !y.margin=[2,3]
;  !x.margin=[4,4]
;  nnp=fix(np/2)+1
  nnp=sqrt(np)+1
  !p.multi=[0,nnp-1,nnp]
 ; !x.omargin=[10,0]
 ; !x.margin=[8,1]
  outp=fltarr(np)
  outperr=fltarr(2,np)
  alpha=!tsym.alpha
;  pnames2=['Norm',alpha+'!L1!N','t!Lb,1!N',alpha+'!L2!N','t!Lb,2!N',alpha+'!L3!N','t!Lb,3!N',alpha+'!L4!N','t!Lb,4,!N',alpha+'!L5!N','c']
  pnames2=pnames
  cstruct='0.'
  for i=1,np-1 do cstruct=cstruct+',0.'
  tmp=execute('mcfit=create_struct(pnames,'+cstruct+')')
  mcfit=replicate(mcfit,nsim)

  if keyword_set(uvot) then np=np-1
  for i=0,np-1 do begin
     n5=round(n*conf1)-1
     n95=round(n*conf2)-1
     islog=0
     if max(pp[i,*])-min(pp[i,*]) gt 1e3 then begin ;and i ne 0 then begin 
        ppp=alog10(pp[i,*]) 
        bin=0.01
        outp[i]=10^median(ppp)
        sp=ppp[sort(ppp)]
        outperr[0,i]=outp[i]-10^sp[n5]
        outperr[1,i]=10^sp[n95]-outp[i]
        islog=1
     endif else begin 
        ppp=pp[i,*]
        bin=0.01
        outp[i]=median(ppp)
        sp=ppp[sort(ppp)]
        outperr[0,i]=outp[i]-sp[n5]
        outperr[1,i]=sp[n95]-outp[i]
     endelse 
;     if i gt 0 then begin 
     w0=where((ppp gt 0 or ppp le 0) and finite(ppp),nw0)
     if max(ppp)-min(ppp) gt 0 then begin 
        plothist,ppp[w0],bin=bin,xtitle=pnames2[i],charsize=2,ytitle='N',noplot=noplot
        if not keyword_set(noplot) then begin 
           oplot,[sp[n5],sp[n5]],[0,n],line=1
           oplot,[sp[n95],sp[n95]],[0,n],line=1
        endif 
;           if i eq 2 then stop
     endif
     for j=0,nsim-1 do begin
        if islog then mcfit[j].(i)=10.^ppp[0,j] else mcfit[j].(i)=ppp[0,j]
     endfor 
;     endif 
  endfor 
  !p.multi=0
  if keyword_set(ps) then begin
     endplot
     ps2pdf,'lc_monte_err_plots_int'+int+'.eps'
  endif 
  print
  print,'p = ',p
  print,transpose(perror[0,*])
  print,transpose(perror[1,*])
  print,'outp = ',outp
  print,transpose(outperr[0,*])
  print,transpose(outperr[1,*])

  outperr[*,0]=outperr[*,0]*flux
  p[0]=p[0]*flux
  if back then flux=1.

  if not keyword_set(nowrite) then begin 
     print,'writing out lc_fit_out_idl_int'+ntostr(int)+'.dat'
     openw,lun,'lc_fit_out_idl_int'+ntostr(int)+'.dat',/get_lun
;     norm=p[fa+0]
;     normerr=outperr[*,fa+0]
;     pnames=pnames[0:fa-1,fa+1:*]
;     if fa gt 0 then begin
;        p=[newp[0:fa-1],p[1:*]]
;        out=dblarr(2,n_elements(newp))
;        out[*,fa:*]=outperr
;        outperr=out
;     endif 
;;; JUST WRITE OUT PNAMES IN ORDER 
     for i=0,np-1 do begin
        j=i
        printf,lun,pnames[i]+' '+ntostr(p[j])+' '+ntostr(outperr[0,j])+' '+ntostr(outperr[1,j])
     endfor
;     printf,lun,'Norm '+ntostr(norm)+' '+ntostr(normerr[0])+' '+ntostr(normerr[1])
     printf,lun,'Chisq '+ntostr(chisq2)
     printf,lun,'dof '+ntostr(fix(dof))
     close,lun
     free_lun,lun

  endif
     ;;; write out monte carlo fit parameters
  fname=outdir+'lc_fit_out_idl_int'+ntostr(int)+'_mc'+add+'.fits'
  print,'writing out '+fname
  mwrfits,mcfit,fname,/create

  ptime,systime(1)-pt
  bleep,n=3
  return
end 
