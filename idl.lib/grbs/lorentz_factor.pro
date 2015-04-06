@calc_eiso
pro calc_uvot_gam,grbstr
  
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  tags=tag_names(grbstr)
  wt=where(strtrim(tags,2) eq 'TPEAK',nwt)
  if nwt eq 0 then begin 
     tags=['tpeak','gam_tpeak','gam_tpeak_lim','gam_pp','gam_pp_lim',$
           'gam_zp','gam_zp_lim','gam_zp2','gam_zp2_lim']
     values=['0d','0.','0','0.','0','0.','0','0.','0']
     add_tags,grbstr,tags,values,grbstr2
     grbstr=grbstr2
;;; 1: < lim, 2: approx, 3: > lim
  endif 

  for k=0,2 do begin 
     case k of
        0: begin
           samp=where(grbstr.who eq 'BAT')
           kdir='BAT_UVOT_LC/'
           color=!grey50
;                 color=!grey70
        end 
        1: begin
           samp=where(grbstr.who eq 'GBM')
           kdir='GBM_UVOT_LC/'
;                 color=!grey40
           color=!grey20
        end 
        2: begin
           samp=where(grbstr.who eq 'LAT')
           kdir='LAT_UVOT_LC/'
        end 
        
     endcase
     wke=where(grbstr[samp].eke gt 0)
     samp=samp[wke]
     ufiles=kdir+'norm/'+strtrim(grbstr[samp].grb,2)+'_norm_u.txt'
     ex=0
     for j=0,n_elements(ufiles)-1 do ex=[ex,exist(ufiles[j])]
     ex=ex[1:*]
     wex=where(ex eq 1,nk)
     ex=wex

     for i=0,nk-1 do begin
        x=samp[ex[i]]
        gdir=kdir+strtrim(grbstr[x].grb,2)

        fitfile=gdir+'/lc_fit_out_idl_int7.dat'
        if exist(fitfile) then begin 
           read_lcfit,fitfile,pname,p,perr
           np=n_elements(p)-1

           if np le 2 then begin ;; single PL
              lc=lcout2fits(gdir+'/lc_newout.txt')
              if lc[0].tstart lt 500 then begin 
                 grbstr[x].tpeak=lc[0].tstart ;;; > lim
                 grbstr[x].gam_tpeak_lim=3
              endif 
           endif 
           if np gt 2 then begin 
              if p[1] lt 0 then begin
                 grbstr[x].tpeak=p[2]
                 grbstr[x].gam_tpeak_lim=2
              endif 
              if p[1] gt 0 then begin 
                 lc=lcout2fits(gdir+'/lc_newout.txt')
                 if lc[0].tstart lt 500 then begin 
                    grbstr[x].tpeak=lc[0].tstart ;;; > lim
                    grbstr[x].gam_tpeak_lim=3
                 endif 
              endif                  
           endif 
           if grbstr[x].tpeak gt 0 then begin 
              lorentz_factor,grbstr[x].z,ek=grbstr[x].eke,tp=grbstr[x].tpeak,n0=1.,gam2a=gam2a
              grbstr[x].gam_tpeak=gam2a

           endif 
        endif 
     endfor
  endfor 

  readcol,'lf.csv',grb,lf1,lf1_source,lf2,lf3,lf4,lf4_source,format='(a,a,a,a,a,a,a)',delim=',',skip=2
  match,strtrim(grbstr.grb,2),'GRB'+grb,m1,m2  

;;; 1: < lim, 2: approx, 3: > lim
  v=strmid(lf1,1,4)*1.
  l=strmid(lf1,0,1)
  w3=where(l[m2] eq '>')
  grbstr[m1[w3]].gam_pp=v[m2[w3]]
  grbstr[m1[w3]].gam_pp_lim=3

  w3=where(l[m2] eq '<')
  grbstr[m1[w3]].gam_pp=v[m2[w3]]
  grbstr[m1[w3]].gam_pp_lim=1

  v=strmid(lf2,1,4)*1.
  l=strmid(lf2,0,1)
  w1=where(l[m2] eq '<')
  grbstr[m1[w1]].gam_zp=v[m2[w1]]
  grbstr[m1[w1]].gam_zp_lim=1

  v=strmid(lf3,1,4)*1.
  l=strmid(lf3,0,1)
  w3=where(l[m2] eq '>')
  grbstr[m1[w3]].gam_zp2=v[m2[w3]]
  grbstr[m1[w3]].gam_zp2_lim=3

  w=where(grbstr.gam_tpeak gt 0 or grbstr.gam_pp gt 0 or grbstr.gam_zp gt 0 or grbstr.gam_zp2 gt 0,nw)
  
  ;; plot all lower lims and range, color coded by sample, named by GRB, symbol
  ;; by method
;  wll=where(grbstr.gam_tpeak_lim eq 1 or grbstr.gam_zp_lim eq 1)
;  wul=where(grbstr.gam_pp_lim eq 3 or grbstr.gam_zp2_lim eq 3)
;  wap=where(grbstr.gam_tpeak_lim eq 2)

  ;; tpeak = psym 2, pp = psym=4, zp = psym=8, zp2 = psym=5

  begplot,name='~/Fermi/Swift_pop_study/lf.eps',/encap,/color,font='helvetica'
  !x.margin=[10,8]
  plotsym,0,1,/fill
  plot,[0,1500],[0,nw+2],/nodata,xtitle=!tsym.gamma_cap,yticks=1,ytickname=replicate(' ',6),yrange=[0,nw+2],/ysty
  gg=!tsym.gamma+!tsym.gamma
  legend,[gg,'','t!Lpeak!N','','ZP10','','2-Zone '+gg],box=0,charsize=1,pos=[1500,nw+2],textcolor=[!magenta,!white,!green,!white,!orange,!white,!purple,!white];[!p.color,!white,!p.color,!white,!p.color,!white,!p.color,!white],psym=[4,3,2,3,8,3,5]
  legend,['BAT','GBM','LAT'],box=0,color=[!grey50,!blue,!red],textcolor=[!grey50,!blue,!red],charsize=1,pos=[1500,nw-6]
  for i=0,nw-1 do begin

     y=i+1
     grb=grbstr[w[i]].grb
     print,grb
     
     case grbstr[w[i]].who of
        'BAT': color=!grey50
        'GBM': color=!blue
        'LAT': color=!red
     endcase 
        
     gams=[grbstr[w[i]].gam_tpeak,grbstr[w[i]].gam_pp,grbstr[w[i]].gam_zp,grbstr[w[i]].gam_zp2]
     gamlims=[grbstr[w[i]].gam_tpeak_lim,grbstr[w[i]].gam_pp_lim,grbstr[w[i]].gam_zp_lim,grbstr[w[i]].gam_zp2_lim]
     whogam=[1,2,3,4]
     wno0=where(gams ne 0,n0)
     gams=gams[wno0]
     gamlims=gamlims[wno0]
     whogam=whogam[wno0]


     wul=where(gamlims eq 1,nul)
     wll=where(gamlims eq 3,nll)
     wap=where(gamlims eq 2,nap)
     oplot,[0,1500],[y,y],line=1

;     thick=5
;     if nll gt 0 and nul gt 0 then begin
;        if gams[wll] lt gams[wul] then plots,[gams[wll],gams[wul]],[y,y],thick=thick,color=!cyan
;        if gams[wll] gt gams[wul] then begin
;           for k=0,nll-1 do plots,[gams[wll[k]],1500],[y,y],thick=thick,color=!orange
;           arrow,1480,y,1500,y,/data,color=!orange,/solid ;,hsize=10

;           for k=0,nul-1 do plots,[0,gams[wul[k]]],[y,y],thick=thick,color=!orange
;           arrow,20,y,0,y,/data,color=!orange,/solid ;,hsize=10

;           print,gams[wll],gams[wul]
;        endif 
;     endif 
;     if nll gt 0 and nul eq 0 then begin 
;        for k=0,nll-1 do begin 
;           plots,[gams[wll[k]],1500],[y,y],thick=thick,color=!magenta
;        endfor
;        arrow,1480,y,1500,y,/data,color=!magenta,/solid;,hsize=10
;     endif 
;     if nll eq 0 and nul gt 0 then begin
;        plots,[0,gams[wul]],[y,y],thick=thick,color=!green
;        arrow,20,y,0,y,/data,color=!green,/solid
;     endif 

     xyouts,-350,y,grb,color=color,size=1
     for j=0,n0-1 do begin 
        case whogam[j] of
           1: ccolor=!green
           2: ccolor=!magenta
           3: ccolor=!orange
           4: ccolor=!purple
;           1: plots,gams[j],y,psym=2,color=color
;           2: plots,gams[j],y,psym=4,color=color
;           3: plots,gams[j],y,psym=8,color=color
;           4: plots,gams[j],y,psym=5,color=color
        endcase
        if gamlims[j] eq 1 then arrow,gams[j]+20,y,gams[j],y,/data,color=ccolor,/solid,hsize=!d.x_size/40.
        if gamlims[j] eq 2 then plots,gams[j],y,psym=2,color=ccolor
        if gamlims[j] eq 3 then arrow,gams[j],y,gams[j]+20,y,/data,color=ccolor,/solid,hsize=!d.x_size/40.
     endfor 

  endfor 

  endplot
stop
  bat=where(grbstr.who eq 'BAT' and grbstr.gam_tpeak ne 0)
  gbm=where(grbstr.who eq 'GBM' and grbstr.gam_tpeak ne 0)
  lat=where(grbstr.who eq 'LAT' and grbstr.gam_tpeak ne 0)

  bin=50
  plothist,[-100,grbstr[bat].gam_tpeak],bin=bin,xrange=[0,1000],yrange=[0,10],xtitle=!tsym.gamma_cap,/fill,fcolor=!grey50,color=!grey50
  plothist,[-100,grbstr[gbm].gam_tpeak],bin=bin,xrange=[0,1000],yrange=[0,10],/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
  plothist,[-100,grbstr[lat].gam_tpeak],bin=bin,xrange=[0,1000],yrange=[0,10],/over,color=!red,line=2
  oplot,[0,1000],[0,0]
  legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]


stop
  return
end 

pro lorentz_factor,z,beta=beta,epse=epse,epsb=epsb,fnu=fnu,eng=eng,tm=tm,n0=n0,astar=astar,emax=emax,delt=delt,fnorm=fnorm,ek=ek,tp=tp,alpha=alpha,f0norm=f0norm,epeak=epeak,enorm=enorm,time=time,plnorm=plnorm,pl2=pl2,plenorm=plenorm,gam2a=gam2a,gam2b=gam2b

  if n_params() eq 0 then begin
     print,'syntax:  lorentz_factor,z,beta=beta,epse=epse,epsb=epsb,fnu=fnu,eng=eng,tm=tm,n0=n0,astar=astar,emax=emax,delt=delt,fnorm=fnorm,ek=ek,tp=tp'
     print,'Method 1 - highest E photon pair production blah'
     print,'           needs beta,delt,fnorm[,emax]'
     print,'           or needs beta, delt, alpha, emax, enorm, f0norm, epeak'
     print,'           and maybe plnorm,pl2,plenorm'
     print,'Method 2 - LC peak forward shock deceration time stuff'
     print,'           needs Ek,tp,(n0 or astar)'
     print,'Method 3 - forward shock during prompt emission end of first peak'
     print,'           needs beta,fnu,epse,epsb,eng,tm'
     return
  end 
  
  if n_elements(beta) eq 0 then beta=1.25
  if n_elements(epse) eq 0 then epse=0.3
  if n_elements(epsb) eq 0 then epsb=0.1
  if n_elements(fnu) eq 0 then fnu=4d-28 ;;; BAT lim
  if n_elements(n0) eq 0 then n0=1.      ;;cm-3
  if n_elements(astar) eq 0 then astar=0.1 

  ;;;; 3 methods
  ;; highest energy photon (Lithwich & Sari 2001)
  ;; deceleration time (Molinari 2007)
  ;; forward shock during prompt emission (Zou & Piran 2010)

  h=4.135e-15 ;; ev*s
  mpc2cm=3.08568025d24 
  ev2erg=1.602d-12 ;; ev->erg
  dist=lumdist(z,h0=72,lambda=0.73)*mpc2cm
  c=3d10                ;; cm/s
;  me=9.10938215d-31*1E3 ;;; g
  mec2=0.510998910d*1d3 ;; keV
  astar1=astar/10^(-1.)

  ;;; method 1 > lim

  sigt=6.6524586d-25 ;; cm^2
                                ; need beta, emax, delt (var timescale), f (norm at 1 MeV)
  mec2_mev=mec2*1d-3
  if n_elements(emax) ne 0 and n_elements(delt) ne 0 and n_elements(fnorm) ne 0 then begin 
     emax_mev=emax*1d-3
     tau=(11./180.)*sigt*dist^2*(mec2_mev)^(beta+1.)*fnorm/(c^2*delt*(-beta-1.))
     gam0=tau^(1./(-2.*beta+2.))*(emax_mev/(mec2_mev))^((-beta-1.)/(-2.*beta+2.))*(1.+z)^((-beta-1.)/(-beta+1.))
     print,'Method 1a - Gamma > ',gam0,tau
  endif 
  if n_elements(delt) ne 0 and n_elements(fnorm) ne 0 then begin 
     tau=(11./180.)*sigt*dist^2*(mec2_mev)^(beta+1.)*fnorm/(c^2*delt*(-beta-1.))
     gam0=tau^(1./(-beta+3))*(1.+z)^((-beta-1.)/(-beta+3.))
     print,'Method 1b - Gamma > ',gam0,tau
  endif 
  if n_elements(delt) ne 0 and n_elements(alpha) ne 0 and n_elements(emax) ne 0 and $
     n_elements(enorm) ne 0 and n_elements(f0norm) ne 0 and n_elements(epeak) ne 0 then begin
     ;; Science paper suppliment method
     fb=0.1
     ec=(alpha-beta)*epeak/(2.+alpha)
     e=10^(dindgen(1000)*2./333.+1.)
     e=[e,ec]
     s=sort(e)
     e=e[s]
     w=where(e ge 8 and e le 30e6)
     b=band(e,f0norm,alpha,epeak/(2.+alpha),beta,enorm)
     fec0=int_tabulated(e[w],b[w],/double)
     print,'flux 8 keV-30 GeV',fec0
     w=where(e ge 100e3 and e le 10e6)
     b=band(e,f0norm,alpha,epeak/(2.+alpha),beta,enorm)
     fec0=int_tabulated(e[w],b[w],/double)
     print,'flux 100 MeV-10 GeV',fec0

;;     w=where(e ge ec and e le 1d7)
     if n_elements(pl2) eq 0 and n_elements(plnorm) eq 0 and n_elements(plenorm) eq 0 then $
        b=band(ec,f0norm,alpha,epeak/(2.+alpha),beta,enorm) else begin
           b=band_pl(ec,f0norm,alpha,epeak/(2.+alpha),beta,enorm,plnorm,pl2,plenorm)
           plot,e,e*e*band_pl(e,f0norm,alpha,epeak/(2.+alpha),beta,enorm,plnorm,pl2,plenorm),/xlog,/ylog
           oplot,e,e*e*band(e,f0norm,alpha,epeak/(2.+alpha),beta,enorm),line=1
           oplot,e,e*e*simple_pl(e,plnorm,pl2,plenorm),line=2
        endelse 
;           b=simple_pl(ec,plnorm,pl,enorm)
     fec0=b;int_tabulated(e[w],b[w],/double)
;     print,fec0
     fec=fec0*time;*2d-4
;     fec=f0norm
;     f=(ec/(e/enorm))^(beta)*f0norm*time
;     fec1=int_tabulated(e[w],f[w],/double)
;     print,fec1
;     fec=f0norm
     x=sigt*(dist/(c*delt))^2.*ec*fec*fb
     gam0=x^(1./(2.*(1.-beta)))*(1.+z)^((beta+1.)/(beta-1.))*(emax*ec/mec2^2.)^((beta+1.)/(2.*(beta-1.)))
     tau=sigt*(dist/(c*delt))^2.*ec*fec*(1.+z)^(-2.*(beta+1.))*gam^(2.*(beta-1.))*(emax*ec/mec2^2)^(-beta-1.)*fb
     print,'Method 1c - Gamma > ',gam0,tau
  endif 

  ;;; method 2 ~ no lim unless time is the lim
  
                                ; need Ek, tp (afterglow), n0 or Astar
  ;; ISM
  if n_elements(ek) ne 0 and n_elements(tp) ne 0 then begin 
     ek52=ek/1d52
     tp2=tp/1d2
     gam0=190.*ek52^(1./8)*n0^(-1./8)*tp2^(-3./8)*(1.+z)^(3./8)
     print,'Method 2 - ISM Gamma ~ ',gam0
     gam2a=gam0
     ;; WIND
     gam0=82.5*ek52^(1./4)*astar1^(-1./4)*tp2^(-1./4)*(1.+z)^(1./4.)
     print,'Method 2 - Wind Gamma ~ ',gam0
     gam2b=gam0
  endif 

  ;;; method 3
  ;; ISM < lim

  p=-2.*beta ;;; assuming this is all nu > nu_c which should be true in gamma-ray during prompt?
  y=0.      ;;;???? Compton Y par - ignore????
;  n0=1. ;;; cm-3  ;; let's assume
; needs beta,fnu,epse,epsb,eng,tm'
  if n_elements(eng) ne 0 and n_elements(tm) gt 0 then begin 

     fnu28=fnu/1d-28
     nu=eng/h*1d3
     nu20=nu/1d20
     d28=dist/1d28
;  d28=dist/1d28
     epse12=epse/10^(-0.5)
     epsb1=epsb/10^(-1.)
;  gam0=340.*(1+z)^(1/4.)*fnu28^(1./9)*d28^(2./9)*n0^(-1./8)*epse12^(-1./6)*epsb1^(-1./72)*nu20^(5./36)*tm^(-2./9)*(1+y)^(1./9)
;  print,'Method 3 - ISM Gamma < ',gam0  
     gam0=340.*2.4^(-(p-2.5)/(9.*(p+2)))*(fnu28*(1.+z)^((p+2.)/2.)*d28^2*n0^(-(p+2.)/4.)* $
                                          epse12^(-(p-1.))*epsb1^(-(p-2.)/4.)*nu20^(p/2.)*tm^(-2.)* $
                                          (3.*(p-2.)/(p-1))^(-(p-1.))*(1.+y))^(1./(2.*p+4.))

     print,'z = '+ntostr(z)
     print,'dist = '+ntostr(d28*1d28)
     print,'Eps_e = '+ntostr(epse)
     print,'Eps_b = '+ntostr(epsb)
     print,'p = '+ntostr(p)
     print,'nu = '+ntostr(nu)
     print,'Method 3 - ISM Gamma < ',gam0

     ;; WIND < lim

  ;;; assumes p=2.5 hmm...
     gam0=50.*fnu28^(2./9.)*d28^(4./9.)*astar1^(-1/4.)*epse12^(-1/3.)*epsb1^(-1./36.)* $
          nu20^(5./18.)*tm^(1./18)*(1+y)^(2./9)
     print,'Method 3 - WIND Gamma < ',gam0
  endif 

  return
end 
