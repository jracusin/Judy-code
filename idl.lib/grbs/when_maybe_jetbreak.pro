@fit_functions
@fit_functions_flares
pro plot_fit,time,p,breaks,model,yfit,t,color=color,line=line
  
;  np=n_elements(p)
;  breaks=round((np-1)/2.-0.5)
  ;; t=0.
  ;; if breaks eq 0 then t=time else begin 
  ;;    if breaks ge 1 then begin 
  ;;       b=p[2]
  ;;       w1=where(time lt b,nw1)
  ;;       if nw1 gt 0 then t=[t,time[w1],b]
  ;;    endif
  ;;    if breaks ge 2 then begin    
  ;;       b=p[4]
  ;;       w2=where(time ge p[2] and time lt b,nw2)
  ;;       if nw2 gt 0 then t=[t,time[w2],b]
  ;;    endif 
  ;;    if breaks ge 3 then begin
  ;;       b=p[6]
  ;;       w3=where(time ge p[4] and time lt b,nw3)
  ;;       if nw3 gt 0 then t=[t,time[w3],b]
  ;;    endif 
  ;;    if breaks ge 4 then begin
  ;;       b=p[8]
  ;;       w4=where(time ge p[6] and time lt b,nw4)
  ;;       if nw4 gt 0 then t=[t,time[w4],b]
  ;;    endif 
  

  ;;    w5=where(time ge b,nw5)
  ;;    if nw5 gt 0 then t=[t,time[w5]]
  ;;    t=t[1:*]
  ;; endelse 
  if breaks[0] eq 0 then t=time else t=[time,p[breaks]]
  s=sort(t)
  t=t[s]

  tmp=execute('yfit='+model+'(t,p)')
  oplot,t,yfit,color=color,line=line

  return
end 
  
pro when_maybe_jetbreak,dir,altbreak,altbreak_rate,go=go,nsig=nsig,cr=cr,phil=phil,masterdir=masterdir,lcout=lcout,lcfile=lcfile,yrange=yrange,flux=flux

;  if n_elements(masterdir) eq 0 then mdir=!mdata else mdir=masterdir
;  cd,mdir
  cd,'~/GRBs'

  if n_elements(dir) ne 0 then select=1 else select=0
  if n_elements(flux) eq 0 then flux=1.

  if not exist('maybe_jetbreak.fits') or select then begin 
     if not select then dir=file_search('GRB*')
     ndir=n_elements(dir)
     
     alpha_jb=1.
     n=1000.
     delchi=2.706  ;;90% 1 parameter of interest
     altbreak=fltarr(ndir) & altbreak_rate=fltarr(ndir)
     ftests=dblarr(ndir)
     g=0
     colprint,dir,indgen(ndir)
     if not keyword_set(go) then stop

     for q=g,ndir-1 do begin 
        cd,dir[q]
        print,q,'  ',dir[q]
        chisq=fltarr(n)
;        if n_elements(lcout) eq 0 then begin 
;           lcout='lc_newout_noflares.txt'
;           lcout='lc_newout_phil2.txt'
;           if not exist(lcout) then lcout='lc_newout_chandra.txt'
;           if not exist(lcout) and keyword_set(phil) then lcout='lc_newout_phil.txt'
;           if not exist(lcout) then lcout='lc_newout.txt'
;        endif 
;        print,lcout
;        if n_elements(lcfile) eq 0 then begin
           lcfile='lc_fit_out_idl_int9.dat'
;           if not exist(lcfile) then $
;              lcfile='lc_fit_out_idl_int7.dat'
;        endif 
        if exist(lcfile) then begin 
           read_lcfit,lcfile,pname,p,perror,chisqs,doff,breaks,lc=lc
           if n_elements(pname) gt 1 then begin 
;        print,'old reduced chi2',chisqs/doff
;              rlc=lcout2fits(lcout,/silent)
              rlc=lcout2fits(/silent)
              if flux ne 1. then begin 
                 p[0]=p[0]/flux
                 rlc.src_rate=rlc.src_rate/flux
                 rlc.src_rate_err=rlc.src_rate_err/flux
              endif 
              wdet=where(rlc.src_rate_err gt 0.,nwdet)
              time=rlc[wdet].time
              terr=fltarr(2,nwdet)
              terr[0,*]=rlc[wdet].time-rlc[wdet].tstart
              terr[1,*]=rlc[wdet].tstop-rlc[wdet].time
              
;              np=n_elements(p)
              mo=fit_models(pname,p,np,nf)
              alpha=[p[np-1]]
              alphaerr=[perror[*,np-1]]
              print,numdec(alpha[0],2)+' {-'+numdec(alphaerr[0],2)+'+'+numdec(alphaerr[1],2)+'}'

;        dof=nwdet-n_elements(p)-1.     
              dof=doff-1. ;;is this right?
 
              model=fit_models(pname,p,/addbreak)
              intmodel='int'+model
              omo=fit_models(pname,p,breaks=breaks)
              intomo='int'+omo
              nbreaks=n_elements(breaks)
              if breaks[0] eq 0 then nbreaks=0
              if nbreaks gt 0 then tbreak=p[breaks[nbreaks-1]]

;              if breaks gt 0 then tbreak=[p(np-2)] else tbreak=min(rlc.time)
              tmax=max(rlc[wdet].time)
              tmin=min(rlc[wdet].time)
              tdiff=tmax-tmin
;           t=(findgen(n)+1)/(n+2)*tdiff[0]+tbreak[0]
              t=reverse(alog10(n/(findgen(n)+1)+1e-3))/alog10(n+1)*tdiff[0]+tmin
              timebin=fltarr(2,nwdet)
              timebin[0,*]=rlc[wdet].tstart
              timebin[1,*]=rlc[wdet].tstop
              y=rlc[wdet].src_rate
              yerr=rlc[wdet].src_rate_err
              
;           ploterror,time,y,yerr,psym=3,/xlog,/ylog,yrange=[1e-5,max(y)],/nohat,title=dir[q]
              plot_like_qdp,title=dir[q],file=lcout,yrange=yrange,lc=rlc
;              stop
               if dof gt 0 then begin
;;                  case breaks of
;;                     0: begin
;;                        intmodel='intbknpow'
;;                        model='bknpow'
;;                        omo='pow'
;;                        intomo='intpow'
;;                        pnames=['norm','pow1','break1','pow2']
;;                     end 
;;                     1: begin
;;                        intmodel='intbkn2pow'
;;                        model='bkn2pow'
;;                        omo='bknpow'
;;                        intomo='intbknpow'
;;                        pnames=['norm','pow1','break1','pow2','break2','pow3']
;;                     end 
;;                     2: begin
;;                        intmodel='intbkn3pow'
;;                        model='bkn3pow'
;;                        omo='bkn2pow'
;;                        intomo='intbkn2pow'
;;                        pnames=['norm','pow1','break1','pow2','break2','pow3','break3','pow4']
;;                     end 
;;                     3: begin
;; ;                       print,'Already has jet break'
;;                        intmodel='intbkn4pow'
;;                        model='bkn4pow'
;;                        omo='bkn3pow'
;;                        intomo='intbkn3pow'
;;                        pnames=['norm','pow1','break1','pow2','break2','pow3','break3','pow4','break4','pow5']
;; ;                       plot_fit,time,p,omo,yfit,timeb,color=!red
;; ;                       goto,jump
;;                     end 
;;                     4: begin
;;                        print,'Already has jet break'
;;                        omo='bkn4pow'
;;                        plot_fit,time,p,omo,yfit,timeb,color=!red
;;                        goto,jump
;;                     end 

;;                  endcase
                 
                 plot_fit,time,p,breaks,omo,yfit,timeb,color=!red

;           if alpha lt alpha_jb then begin 
;              if alpha_jb gt alpha+alphaerr[1]  then begin 
;              for j=0,nwdet-1 do oplot,[timebin[0,j],timebin[1,j]],[y[j],y[j]]
                 w=where(t gt tbreak[0] and t lt 1e9,nw)
                 for i=0,nw-1 do begin 
                    newp=[p[0:np-1],t[w[i]],alpha+alpha_jb,p[np:*]]
                    tmp=execute('yfit='+intmodel+'(timebin,newp)')
;                 tmp=execute('yfit='+model+'(time,newp)')
                    chisq[w[i]]=total((y-yfit)^2/yerr^2)
                    if chisq[w[i]] eq 0. then stop
                    wb=where(timeb lt t[w[i]])
                    wbu=where(timeb gt t[w[i]],nwbu)
                    if nwbu gt 0 then ntimeb=[timeb[wb],t[w[i]],timeb[wbu]]
                    tmp=execute('yfit='+model+'(ntimeb,newp)')
                    if i mod round(nw/10) eq 0 then oplot,ntimeb,yfit,line=2
;              print,chisq[i]/dof
                 endfor
                 
;              dchisq=chisq/dof-chisqs/doff
                 dchisq=chisq-chisqs
                 minchi=min(dchisq[w],m)
                 mint=findval(dchisq[w],t[w],minchi)
                 mint=mint[0]
                 wl=where(t[w] le t[w[m]],nwl)
                 low=findval(dchisq[w[wl]],t[w[wl]],delchi)
                 low=max(low)
                 if low eq -1 then  low=interpol(t[w[wl]],dchisq[w[wl]],delchi)
                 if low gt tbreak then begin 
                    if low lt tmax then begin 
                       newp=[p[0:np-1],low,alpha+alpha_jb,p[np:*]]
                       breaks2=[breaks,low]
;                    wlow=where(t lt low[0],nwlow)
;                    wup=where(t gt low[0],nwup)
;                    if nwup gt 0 then tt=[t[wlow],low,t[wup],max(t)*2.] else tt=[t[wlow],low]
;           tt=[timeb[wlow],low,timeb[nwlow:*]]
;           tt=[time[wlow],low,time[nwlow-1:*]]
;                    tmp=execute('yfinal='+model+'(tt,newp)')
;                    oplot,tt,yfinal,color=!green
                       plot_fit,t,newp,breaks2,model,yfinal,color=!green
                       oplot,[low,low],[1e-6,1e3],line=2
                       altbreak[q]=low
                       print,'altbreak = ',low
                       print,'new break slope = ',alpha+alpha_jb
                       tmp=execute('altbreak_rate[q]='+model+'(low,newp)')
                    endif else print,'break must be before tmax'
                 endif else print,'no additional break possible'
                 
;              fit_pow_model,time,y,terr,yerr,newp,intmodel,pnames,fyfit,newp2,fperror,fchisq,fdof,status=status,name=name
                 wtimey=where(time lt mint)
                 wtimey2=where(time gt mint)
                 tmp=execute('oldy='+intomo+'(timebin,p)')
                 oldy=rotate(oldy,4)
                 oldchisq=total((y-oldy)^2/yerr^2)
                 minp=[p[0:np-1],mint,alpha+alpha_jb,p[np:*]]              
;              timey=[time[wtimey],mint,time[wtimey2]]
;              tmp=execute('fyfit2='+model+'(timey,minp)')
;              oplot,timey,fyfit2,color=!blue
;              plot_fit,time,minp,model,fyfit2,color=!blue
;              oplot,[mint,mint],[1e-6,1e4],color=!blue,line=2
                 tmp=execute('yfinal='+intmodel+'(timebin,minp)')
                 fchisq=total((y-yfinal)^2/yerr^2)
                 dof1=nwdet-np
                 if abs(mint-t[n-1]) gt 100 then dof2=nwdet-np-1 else begin
                    dof2=dof1
                    fchisq=chisqs
                 endelse 
                 print,'old reduced chi2',oldchisq,dof1,oldchisq/dof1
                 print,'new reduced chi2',fchisq,dof2,fchisq/dof2
                 ft=ftest(chisqs,fchisq,np,nwdet,1)
                 ftests[q]=ft
                 print,'ftest = ',ft,1.-f_pdf(ft,dof1,dof2) ;;compares old chisq to new min chisq assuming break of delalp=alpha3+1
                 
                 pos=[0.6, 0.6, 0.9, 0.9]
                 if min(y) gt 1e-2 then pos=[0.15,0.15,0.5,0.5]
;              pos=[0.32,0.27,0.57,0.52]
                 plot,t[w],dchisq[w],/xlog,POS=pos,/noerase,yrange=[min(dchisq[w]),min(dchisq[w])+5],charsize=1.,xtitle='Time since BAT trigger (s)',ytitle=!tsym.delta_cap+!tsym.chi+'!U2!N'
                 oplot,[low,low],[-100,1000],line=1
;              oplot,[mint,mint],[-100,1000],line=1,color=!blue

;              endif else print,ntostr(alpha[0])+'+/-'+ntostr(alphaerr[1])+' slope too close to 2'
;           endif else print,'alpha steeper than '+ntostr(alpha_jb)
                 
              endif else print,'Not enough data points'
              jump:
           endif else print,'Not enough data points for fit'
        endif else print,'No LC fit' 
        if not keyword_set(go) then begin 
           k=get_kbrd(10)
           if k eq 's' then stop
        endif 
        cd,'..'
     endfor 
     
     maybe=create_struct('GRB','','tlastpos',0.,'ctr_tlastpos',0.)
     maybe=replicate(maybe,n_elements(dir))
     maybe.grb=dir
     maybe.tlastpos=altbreak
     maybe.ctr_tlastpos=altbreak_rate

;     if not select then 
     mwrfits,maybe,'maybe_jetbreak.fits',/create
  endif else maybe=mrdfits('maybe_jetbreak.fits',1)
  
  if n_elements(cr) gt 0 then begin 
     for i=0,n_elements(maybe)-1 do begin
        w=where(strtrim(cr.grb,2) eq strtrim(maybe[i].grb,2),nw)
        if nw gt 0 then begin
           cr[w].tlastpos=maybe[i].tlastpos
           cr[w].ctr_tlastpos=maybe[i].ctr_tlastpos
        endif 
     endfor 
     if nsig eq 2. then signame='_2sig' else signame='_3sig'
     if not select then mwrfits,cr,'closure_relations_total'+signame+'.fits',/create
  endif 
  
  if not keyword_set(go) then stop
  return
end 
