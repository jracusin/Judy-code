@fit_functions
@fit_functions_flares
pro loc_acc

  ;;; 0.4-2 keV
  counts=[4.4577084,9.031399,18.65031,37.071636,58.60647,81.065445,123.357124,245.1997,526.04913,1025.8748,2039.1542,4210.9595]
  r90=[123.53336,93.35332,49.845238,35.615437,26.614456,21.510979,18.387964,11.615028,9.708707,6.1326475,4.14316,3.4631622]

  plot,counts,r90,/xlog,/ylog,psym=2,/iso
  l=linfit(alog10(counts),alog10(r90))
  oplot,counts,10^l[0]*counts^l[1],color=!green

  lx=mrdfits('~/iLobster/simulations/lobs_list_0.4_2.fits',1)
  grb=mrdfits('~/Swift/swift_grb_properties.fits',1)
  w=where(grb.t90 le 2. and grb.t90 gt 0.,nw)
  grb=grb[w]
  lx=lx[w]
  z=grb.z
  w=where(z le 0)
  z[w]=0.7
  mpc2cm=3.08568025d24
  dist200=440*mpc2cm
  
  sim=create_struct('grb','','count3_2000',0.,'r3_2000',0.,'count45_2000',0.,'r45_2000',0.,$
                    'count3_500',fltarr(4),'r3_500',fltarr(4),'count45_500',fltarr(4),'r45_500',fltarr(4))
  sim=replicate(sim,nw)
  sim.grb=grb.grb
  fact=fltarr(nw)

  tstart=[0.,0.,500,1000,1500]
  tstop=[2000.,500,1000,1500,2000]
  t1=[3.,45.]*60.
  for i=0,nw-1 do begin ;; each burst
     dist=lumdist(z[i],h0=71,lambda=0.73,omega_m=0.27)*mpc2cm
     fact[i]=lx[i].lx_ratio*dist^2/dist200^2/kcorr(z[i],[1.,grb[i].phind],/pow)

     ;;; 1 tile, tdel=2000, t1=3 min
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[0]+t1[0],tstop[0]+t1[0]],grb[i].p)
     sim[i].count3_2000=f*(tstop[0]-tstart[0])*fact[i]
     sim[i].r3_2000=252.8*sim[i].count3_2000^(-0.534)
     ;;; 1 tile, tdel=2000, t1=45 min
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[0]+t1[0],tstop[0]+t1[1]],grb[i].p)
     sim[i].count45_2000=f*(tstop[0]-tstart[0])*fact[i]
     sim[i].r45_2000=252.8*sim[i].count45_2000^(-0.534)
     ;;; 4 tile, tdel=500, t1=3 min
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[1]+t1[0],tstop[1]+t1[0]],grb[i].p)
     sim[i].count3_500[0]=f*(tstop[1]-tstart[1])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[2]+t1[0],tstop[2]+t1[0]],grb[i].p)
     sim[i].count3_500[1]=f*(tstop[2]-tstart[2])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[3]+t1[0],tstop[3]+t1[0]],grb[i].p)
     sim[i].count3_500[2]=f*(tstop[3]-tstart[3])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[4]+t1[0],tstop[4]+t1[0]],grb[i].p)
     sim[i].count3_500[3]=f*(tstop[4]-tstart[4])*fact[i]
     ;;; 4 tile, tdel=500, t1=45 min
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[1]+t1[1],tstop[1]+t1[1]],grb[i].p)
     sim[i].count3_500[0]=f*(tstop[1]-tstart[1])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[2]+t1[1],tstop[2]+t1[1]],grb[i].p)
     sim[i].count3_500[1]=f*(tstop[2]-tstart[2])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[3]+t1[1],tstop[3]+t1[1]],grb[i].p)
     sim[i].count3_500[2]=f*(tstop[3]-tstart[3])*fact[i]
     f=call_function('int'+strtrim(grb[i].model,2),[tstart[4]+t1[1],tstop[4]+t1[1]],grb[i].p)
     sim[i].count3_500[3]=f*(tstop[4]-tstart[4])*fact[i]

     sim[i].r3_500=252.8*sim[i].count3_500^(-0.534)
     sim[i].r45_500=252.8*sim[i].count45_500^(-0.534)


  endfor 

  plothist,alog10(sim.r3_2000),bin=0.2,xtitle='Log R90 (arcsec)'


stop
  return
end 

pro plot_rand_pow
  
  cd,'~/iLobster/simulations'
  files=['rand_period_rn0.10_n2592.txt','rand_period_rn0.20_n2592.txt','rand_period_rn0.30_n2592.txt']
  readcol,files[0],period1,totpow1
  readcol,files[1],period2,totpow2
  readcol,files[2],period3,totpow3

  n=2592.
  plot,period1,totpow1/n;,/xlog,/ylog
  oplot,period2,totpow2/n,color=!blue
  oplot,period3,totpow3/n,color=!green
  oplot,period1,((period1*0.13+0.25)),color=!red
  
  stop
  return
end 

pro plot_results

  simfiles=file_search('~/iLobster/simulations/smbhb_sim*fits')
  n=n_elements(simfiles)

  nsig=3.
  ntrials=1.
  sigthresh=mpchilim(-((1-gauss_cdf(nsig))/2./ntrials)+1d,1)
  frac=fltarr(n) & per=fltarr(n) & perror=fltarr(n) & flux=fltarr(n)
  for i=0,n-1 do begin
     sim=mrdfits(simfiles[i],1)
     w=where(sim.lomb_pow ge sigthresh,nw)
     if nw gt 0 then begin 
        frac[i]=nw*1./n_elements(sim)
        if nw gt 1 then perror[i]=median(abs(sim[w].fit_period_err[0])) else perror[i]=abs(sim[w].fit_period_err[0])
     endif 
     per[i]=sim[0].period
     flux[i]=sim[0].flux
  endfor 

  pers=[30,60,90,182,365]
  symbol=['*','o','tu','s','S']
  p=plot([0,1],[0,0.1],/nodata,xtitle='Fraction Above Threshold',ytitle='Mean Error on Period',xrange=[1e-4,0.6],yrange=[0,0.03],/xlog)
  for i=0,4 do begin 
     w=where(per eq pers[i])
     color=['green','red','blue']
     for j=0,2 do begin 
        p2=plot([-1,frac[w[j]]],[-1,perror[w[j]]/per[w[j]]],symbol=symbol[i],/overplot,color=color[j],linestyle='none',/sym_filled,sym_fill_color=color[j],xrange=[0,0.6],yrange=[0,0.03])
     endfor
  endfor 

  t1=text(0.75,0.8,'flux=$10^{-11}$',color='red')
  t1=text(0.75,0.77,'flux=$10^{-10}$',color='green')
  t1=text(0.75,0.74,'flux=$10^{-9}$',color='blue')
;  t2=text(0.65,0.82,symbol[0]+' per='+pers[0]+' days')

  colprint,per,flux,frac,perror/per
  p.save,'~/iLobster/simulations/frac_frac.png'
  p.refresh
  p.close

stop
  return
end 

pro get_amp

  fvar=0.1
  flux=1e-10
  ctsflux=1.75542/3.6775e-09
  ctr=flux*ctsflux
  dt=10.5*60.
  ndays=1826
  freq=[1./(ndays*86400.),1./(86400.)]
  pow=freq^(-1.) ;; red noise
  x1 = ts_gen(ndays, dt=86400., freq=freq, pow=pow,time=time,/log)
  var=sqrt(fvar^2*ctr^2+(sqrt(ctr*dt)/dt)^2)
  x5=x1*sqrt(var/mean(x1^2))

  stop
  return
end 

pro run_sims,g

;  m=[0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2]
;  per=[365,365,365,182,182,182,90,90,90,60,60,60,30,30,30]
;  f=[1e-11,1e-10,1e-9,1e-11,1e-10,1e-9,1e-11,1e-10,1e-9,1e-11,1e-10,1e-9,1e-11,1e-10,1e-9]
  per=[300,300,300,300,300];,182,90,60,30]
  f=[1e-11,1e-11,1e-11,1e-11,1e-11]
  rn=[0.05,0.1,0.2,0.3,0.4]
  outdir='~/iLobster/simulations/rednoise_knob/'

  if n_elements(g) eq 0 then g=0

  n=n_elements(m)
  n=1
stop
  for i=g,n-1 do $
  smbhb,per=per[i],f=f[i],nyears=5,rednoise=rn[i],outdir=outdir;,/skip


  return
end 

pro list_sims

  cd,'~/iLobster/simulations/rednoise_knob/'
  files=file_search('smbhb_sim_*.fits')
  colprint,files
  return
end 

pro pink_noise

  restore,'~/iLobster/simulations/pink_noise_idlsave.dat'
  sim=mrdfits('smbhb_sim_0.2m_365p_1.0e-10f_1n.fits',1)
  i=round(randomu(seed,1000)*2592)  
  !p.multi=[0,1,2]
  plothist,sim[i].lomb_pow,x1,y1,bin=10,xtitle='Max Lomb Pow',xrange=[0,600]
  plothist,maxpow,x2,y2,/over,color=!red,bin=10,xrange=[0,600]

  plothist,sim[i].fit_period,x3,y3,bin=10,yrange=[0,150],xtitle='Period (days)',xrange=[0,700]
  plothist,maxper,x4,y4,bin=10,/over,color=!red,yrange=[0,150],xrange=[0,700]
  oplot,[365,365],!y.crange,line=2
  !p.multi=0
stop
return
end 

pro rand_period,rednoise_amp

  pt=systime(1)
  ndays=1826.
  pmin=10.
  pmax=5*365.
  tfact=1.
  freq=[1./(ndays*86400.*tfact),1./(86400.*tfact)]
  pow=freq^(-1.) ;; red noise
  
  n=2592.;1e3
  maxpow=fltarr(n)
  maxper=maxpow
;  totpow=fltarr(ndays*50.)
  for i=0L,n-1 do begin
     if i mod 100 eq 0 then print,i
     x1 = ts_gen(ndays, dt=86400.*tfact, freq=freq, pow=pow,time=time,/log)

     sx=stddev(x1)
     nx=sx/(rednoise_amp/2.)
     x2=x1/nx

     t=time/86400.

;     !p.multi=[0,1,2]
;     plot,t,x1

     lomb,t,x2,lpow,period=period,pmin=pmin,pmax=pmax,oversamp=50.

;     plot,period,lpow

     maxpow[i]=max(lpow,m)
     maxper[i]=period[m]
     if i eq 0 then totpow=lpow else $
        totpow=totpow+lpow
  endfor 
;  !p.multi=0

  plot,period,totpow/n,xtitle='Period',ytitle='Avg pow',title='rm='+ntostr(rednoise_amp,3)+' , n='+ntostr(n)
  f=linfit(period,totpow/n)
  print,f
  oplot,period,period*f[1]+f[0],color=!green

  print,'red noise',rednoise_amp
  save,filename='rand_period_rn'+numdec(rednoise_amp,2)+'_n'+ntostr(fix(n))+'.dat'
  ptime,systime(1)-pt
  bleep
  stop
;;; want to stack power as function of period to get average
  ;;; rn=0.35, 0.33505, 0.13079
  ;;; rn=0.1, 0.291125, 0.13166
  ;; ntrials matters more than amplitude

  return
end 

pro sampling,visible,doplot=doplot,ra=ra,dec=dec,nyears=nyears

  ;;; 5-deg grid ;; or do 1 deg grid?
  ra=fltarr(72,36)
  dec=fltarr(72,36)
  for i=0,71 do begin
     for j=0,35 do begin
        ra[i,j]=i*5.
        dec[i,j]=j*5-90.
     endfor
  endfor 

;  year=['2019','2020','2021','2022','2023']
  if n_elements(nyears) eq 0 then nyears=5
  year=ntostr(indgen(nyears)+2019)
  ndayyear=[365,366,365,365,365]
  date=[year[0]+'-'+ntostr(indgen(ndayyear[0])+1)+'-00:00:00']
  for i=1,n_elements(year)-1 do date=[date,year[i]+'-'+ntostr(indgen(ndayyear[i])+1)+'-00:00:00']
  n=n_elements(date)

  visible=intarr(n,72*36)
  fracvistime=fltarr(n)
  startvis=fltarr(72*36)
  fracvispoint=fltarr(72*36)

  suncon=45.
  mooncon=15.

;  n=365.*3  ;;; for each day of the year, is each point visible
  plotsym,0,1,/fill
  for i=0,n-1 do begin
     d=date[i]
     sunpos_utc,d,sunra,sundec,jd=jd
     moonpos,jd,moonra,moondec
     sundist=separation(ra,dec,sunra,sundec)/3600.
     moondist=separation(ra,dec,moonra,moondec)/3600.
     w=where(sundist gt suncon and moondist gt mooncon,nw)
     visible[i,w]=1
     fracvistime[i]=nw*1./2592.
     if keyword_set(doplot) then begin 
        map_set,/aitoff
        oplot,ra,dec,psym=8
        oplot,ra[w],dec[w],psym=8,color=!green
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 

  for i=0,2592-1 do begin
     w=where(visible[*,i] eq 1,nw)
     b=where(visible[*,i] eq 0,nb)
     fracvispoint[i]=nw/365.
     if nb gt 0 then startvis[i]=b[0]
  endfor 

  !p.multi=[0,1,2]
  plothist,fracvistime,bin=0.001,xtitle='Fraction of sky points visible on each day'
  plothist,fracvispoint,bin=0.01,xtitle='Fraction of year each point is visible'
  !p.multi=0
  

  ;;; still need to account for earth constraint and other ISS
  ;;; obstructions
  ;;; ISS has orbit altitude of 400 km and inclination of 51.6 deg

  return
end 

pro stuff_for_amy
  ;; Amy needs list of z & nH (galactic & intrinsic)

  cd,'~/GRBs'

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  l=create_struct('grb','','z',0.,'nhgal',0.,'nh',0.,'nherr',fltarr(2),'z2',0.,'phind',0.,'phinderr',fltarr(2),'lx_ratio',0.)

  l=replicate(l,n_elements(g))

  for i=0,n_elements(g)-1 do begin
     l[i].grb=g[i].grb
     l[i].z=g[i].z
     specfile=strtrim(g[i].grb,2)+'/UL_specfits.fits'
     if exist(specfile) then begin 
        spec=mrdfits(specfile,1)
        nspec=n_elements(spec)
        l[i].nhgal=spec[nspec-1].nhgal*1d22
        l[i].nh=spec[nspec-1].nh
        l[i].nherr=spec[nspec-1].nherr
        l[i].z2=spec[nspec-1].z
        l[i].phind=spec[nspec-1].phind
        l[i].phinderr=spec[nspec-1].phinderr
     endif 
  endfor 
  
  mwrfits,l,'~/iLobster/simulations/z_nh_list_for_Amy.fits',/create

  return
end 

pro ilobs_xrt_conv
  
  ;;; need to create xspec sims to calculate count ratio for lobster &
  ;;; XRT for range of powerlaw index

  ;;; setup base model, fakeit, ignore
  ;;; look through each burst & model parameters, output burst name
  ;;; and model counts

  l=mrdfits('~/iLobster/simulations/z_nh_list_for_Amy.fits',1)

  cd,'~/iLobster/simulations/'

;  if not exist('sim_lob.dat') or not exist('sim_xrt.dat') then begin 
     spawn,'rm sim_lob.fak'
     spawn,'rm sim_xrt.fak'
     for j=0,1 do begin
        ;; 0=iLobster
        ;; 1=XRT
        case j of 
           0: begin
              xbfile='~/iLobster/simulations/sim_lob.xcm'
              xofile='~/iLobster/simulations/sim_lob.out'
              rmf='~/iLobster/responses/lobster_fl30cm_-50C_spot_01oct14.rsp'
              arf=''
              pref='sim_lob'
;              ig=['**-0.4','2.0-**']
;              ig=['**-0.3','5.0-**']
              ig=['**-0.3','10-**']
              datfile='~/iLobster/simulations/sim_lob.dat'
           end
           1: begin
              xbfile='~/iLobster/simulations/sim_xrt.xcm'
              xofile='~/iLobster/simulations/sim_xrt.out'
              rmf='/Users/jracusin/CALDB/data/swift/xrt/cpf/rmf/swxpc0to12s6_20010101v014.rmf'
              arf='/Users/jracusin/CALDB/data/swift/xrt/cpf/arf/swxpc0to12s6_20010101v013.arf'
              pref='sim_xrt'
              ig=['**-0.3','10.0-**']
              datfile='~/iLobster/simulations/sim_xrt.dat'
           end 
        endcase 
        openw,lun,xbfile,/get_lun
        
        printf,lun,'mo pow'
        printf,lun,'2'
        printf,lun,'1'
        printf,lun,'fakeit none'
        printf,lun,rmf
        printf,lun,arf
        printf,lun   ;; counting stats
        printf,lun,pref   ;; fake prefix
        printf,lun,pref+'.fak'
        printf,lun,'1,1,1'  ;;; exposure time, etc
        printf,lun 
        if j eq 0 then printf,lun,'dummyrsp 0.1 12 1200'

;        printf,lun,'ignore **-0.3'
        for i=0,n_elements(ig)-1 do printf,lun,'ignore '+ig[i]
        printf,lun,'set fileid [open '+datfile+' w]'
        
        for i=0,n_elements(l)-1 do begin
           print,l[i].grb
           printf,lun,'mo wabs*zwabs*pow'
           printf,lun,ntostr(l[i].nhgal/1d22)
           printf,lun,ntostr(l[i].nh/1d22)
           if l[i].z2 gt 0 then printf,lun,ntostr(l[i].z2) else printf,lun,'0'
           printf,lun,ntostr(l[i].phind)
           printf,lun,'1'

           printf,lun,'show rate'
           printf,lun,'tclout rate 1'
           printf,lun,'set rte [string trim $xspec_tclout]'
           printf,lun,'regsub -all { +} $rte { } crte'
           printf,lun,'set lrte [split $crte]'
;     printf,lun,'puts $fileid "rate [lindex $lrte 0]"' 
           printf,lun,'puts $fileid "'+l[i].grb+' model_rate [lindex $crte 2]"'
        endfor 
        printf,lun,'close $fileid'
        printf,lun,'exit'
        printf,lun
        
        close,lun
        free_lun,lun
        print,'XSPEC'
        spawn,'xspec - '+xbfile+' > '+xofile

     endfor 
;  endif 

  readcol,'sim_lob.dat',lgrb,mr,lrate,format='(a,a,f)'
  readcol,'sim_xrt.dat',xgrb,mr,xrate,format='(a,a,f)'

  l.lx_ratio=lrate/xrate
  mwrfits,l,'~/iLobster/simulations/lobs_list.fits',/create


  return
end 

pro smbhb,per=per,m=m,flux=flux,nyears=nyears,rednoise_amp=rednoise_amp,skip=skip,doplot=doplot,outdir=outdir

  pt=systime(1)
  if n_elements(per) eq 0 then per=182. ;/7. ;;; period in days
  if n_elements(m) eq 0 then m=0.20
  if n_elements(flux) eq 0 then flux=1e-11
  if n_elements(nyears) eq 0 then nyears=5
  if n_elements(rednoise_amp) eq 0 then rednoise_amp=0.35
  if n_elements(outdir) eq 0 then outdir='~/iLobster/simulations/latest/'
  nsim=2592

;  sim0=mrdfits('~/iLobster/simulations/smbhb_sim_0.0m_0p_1.0e-11f_5yr_0.0rn_0point.fits',1)
;  avgpow=fltarr(3641)
;  for i=0,3641-1 do avgpow[i]=total(sim0.lpow[i])
;  avgpow=avgpow/3641.

  ;; rn=[0.1,0.2,0.3]
  ;; files=['rand_period_rn0.10_n2592.txt','rand_period_rn0.20_n2592.txt','rand_period_rn0.30_n2592.txt']
  ;; wrn=where(rn eq rednoise_amp,nwrn)
  ;; readcol,files[wrn],period1,totpow1
  ;; if nwrn eq 0 and rednoise_amp gt 0 then begin
  ;;    print,'no red noise template'
  ;;    stop
  ;; endif 

;  add='';'_subavg'

  simname=outdir+'smbhb_sim_'+ntostr(m,3)+'m_'+ntostr(fix(per))+'p_'+numdec(flux,1,/sci)+'f_'+ntostr(fix(nyears))+'yr_'+numdec(rednoise_amp,2)+'rn'
  print,simname
  if keyword_set(skip) then begin
     sim=mrdfits(simname+'.fits',1)
     goto,skip
  endif 
  tfact=1.  
  sampling,visible,ra=ra,dec=dec,nyears=nyears ;;; assumes daily
  ndays=n_elements(visible[*,0])
;  ndays=round(1096/tfact);+1
  sim=create_struct('ra',0.,'dec',0.,'flux',0.,'modulation',0.,'period',0.,$
                    'fit_period',0.,'fit_period_err',fltarr(2),$
                    'lomb_pow',0.,'p',0.,'ntrials',0.,'significance',0.,$
                    'accuracy',0.,'visible',intarr(ndays),'visfrac',0.,$
                    'fit_period2',0.,'fit_period_err2',fltarr(2),'lomb_pow2',0.,$
                    'lperiod',fltarr(3641),'lpow',fltarr(3641))
  sim=replicate(sim,nsim)                                 ;; 5 deg grid on sky
  sim.visible=visible[indgen(ndays)*tfact,*]
  for i=0,nsim-1 do begin
     sim[i].ra=ra[i]
     sim[i].dec=dec[i]
  endfor 

;  sim.gapstart=startvis
;  sim.gapwidth=(1.-frac)*365.
  for i=1000,nsim-1 do begin 

     print,i
     t=findgen(ndays)*tfact           ;; 3 years
     nt=n_elements(t)
;     dt=10.5*60.*tfact                 ;;; exposure of each pointing
;     in seconds
     dt=1700. ;; assumes 16.3% SAA, 9.4% slewing+settling, +17% constraints
;  dtd=10.5/60./24. ;; exposure of each pointing in days
     
     ang=t/per*360.
     f=sin(ang*!dtor)*m/2.+1.
;     ctsflux=2.51497/4.7566e-09 ;; for gamma=1.8 spectrum
     ctsflux=1.75542/3.6775e-09 ;; for gamma=1.8 and abs=1e21 spectrum
     
     back=0.3e-4 ;; cts/s/sq arcmin
     area=8.3^2.
     backrate=back*area

     sim[i].flux=flux
     sim[i].modulation=m
     sim[i].period=per

     vf=where(sim[i].visible eq 1,nvf)
     sim[i].visfrac=nvf*1./ndays
     ;;; sample t,f - remove 3 months every year for sun constraint
;     s=round(randomu(seed0,1.)*365.) 
;     s=startvis[i]
;     s=sim[i].gapstart
     ;; don't do 3 months draw from distribution
     ;; but still random start

;     len=round(sim[i].gapwidth)
     t0=t
     f0=f
     
     w=where(sim[i].visible eq 1,nw)
;     w0=where(sim[i].visible eq 0,nw0)
     t=t[w]
     f=f[w]

     ;; print,sim[i].gapwidth,sim[i].gapstart
     ;; if frac[i] lt 1. then begin 
     ;;    rem=intarr(nt)
     ;;    rem[s:s+len]=1
     ;;    rem[s+365:s+len+365]=1
     ;;    if 365*2+len+s lt nt then rem[s+365*2:s+len+365*2]=1 else begin 
     ;;       if s+365*2 lt nt then rem[s+365*2:*]=1
     ;;       rem[0:len-(nt-365*2-s)]=1
     ;;    endelse
     ;;    w=where(rem eq 0)
     ;;    t=t[w]
     ;;    f=f[w]
     ;; endif 
     flux1=sim[i].flux*f
     cts1=flux1*ctsflux
     ;;; Gaussian
;     srccounts=randomn(seed1,nt)*sqrt(sim[0].flux*ctsflux*dt)+cts1*dt ;;; random cts in normdist about median
;     backcounts=randomn(seed2,nt)*sqrt(backrate*dt)/2.+backrate*dt
     ;; Poissian
;     srccounts=randomu(seed1,nt,poisson=sim[i].flux*ctsflux*dt*f);+cts1*dt ;;; random cts in poisson
;     randomp,x,-2.,ndays,range=[1.,1.35] ;;; red noise for AGN variability
     
     if rednoise_amp gt 0 then begin 
        freq=[1./(ndays*86400.*tfact),1./(86400.*tfact)]
        pow=freq^(-1.) ;; red noise
        x1 = ts_gen(ndays, dt=86400.*tfact, freq=freq, pow=pow,time=time,/log)
;     sx=stddev(x)
;     nx=sx/0.35
;     x2=x/nx
;     wx=where(f+x2 lt 0)
;     x2[wx]=-f[wx]+0.01

;     x3 = ts_gen(ndays, dt=86400.*tfact, freq=freq, pow=pow,time=time,/log)
;     sx=stddev(x3)
;     nx=sx/0.35
;     x4=x3/nx
        
        x5=x1                   ;+x3
        sx=stddev(x5)
        nx=sx/(rednoise_amp/2.)
        x6=x5/nx
        wx=where(f+x6 lt 0)
        x6[wx]=-f[wx]+0.01
     endif else x6=fltarr(ndays)

     srccounts=fltarr(ndays)
     ctr=sim[i].flux*ctsflux-backrate
;     for j=0,nw-1 do
;     srccounts[j]=randomu(seed1,1,poisson=sim[i].flux*ctsflux*dt*(f[j]+x6[j]))
     for j=0,nw-1 do srccounts[j]=randomu(seed1,1,poisson=ctr*dt*(f[j]+x6[j]))
     backcounts=randomu(seed2,nt,poisson=backrate*dt) ;+backrate*dt
     counts=srccounts+backcounts
;  for i=0,nt-1 do counts[i]=randomu(seed,1.,poisson=cts1[i]*dt) ;flux*dt*1e-11)
     cts2=counts/dt
     flux2=cts2/ctsflux
     rate2=(flux2-mean(flux2))*ctsflux
;     if nw0 gt 0 then rate2[w0]=0.
     pmin=10.
     pmax=3*365.
     
     lomb,t,rate2*hanning(n_elements(t)*1d),lpow,period=period,pmin=pmin,pmax=pmax,oversamp=20.
     np=n_elements(lpow)

;     if rednoise_amp gt 0 then begin 
;     lpow2=lpow/((period*0.13+0.25))*2./avgpow
;        lpow2=lpow/(totpow1/nsim*1.)*2.
;     endif else 
     lpow2=lpow;/avgpow
     lpow2=lpow2*2./stddev(lpow2)
     lpow2=lpow2+(2.-mean(lpow2))

;;;                  from fit to pink noise only periodigram x 1000
     maxpow2=max(lpow2,wm2)
     lpow=lpow2
     wm=wm2

     w=where(lpow[2:*]-lpow[0:np-2] gt 0 and lpow[1:*]-lpow[3:*] gt 0,nw)  ;;; make sure only peaks
     w=w+1 ;;; to get correct peak, zero indexing issue

;     w=where(lpow[0:np-1]-lpow[1:*] gt 0 and lpow[0:*]-lpow[-1:*] gt 0,nw)  ;;; make sure only peaks
     maxpow=max(lpow[w],wm) 
     wm=w[wm]
     print,period[wm],lpow[wm]
     sim[i].fit_period=period[wm]
     sim[i].lomb_pow=lpow[wm]
     sim[i].lperiod=period
     sim[i].lpow=lpow

     ;;; second highest peak
     sw=sort(lpow[w])
     maxpow2=lpow[w[sw[nw-2]]]
     wm2=where(lpow eq maxpow2)
     sim[i].fit_period2=period[wm2]
     sim[i].lomb_pow2=lpow[wm2]

     p=1.-chisqr_pdf(lpow[wm],2.)
     ntrials=((1./pmin)-(1./pmax))/(1./(max(t)-min(t)))
     sig=p*ntrials
     sim[i].p=p
     sim[i].ntrials=ntrials
;     lomb_pow must be > val below to be nsig significant
;     print,mpchilim(-((1-gauss_cdf(nsig))/2./ntrials)+1d,1)

;     sig=3d
;     p=(1-gauss_cdf(nsig))/2.
;     ntrials=1000.
;     pt=p/ntrials
;     print,sigprob(-pt+1d)
;     print,mpchilim(-pt+1d,1)

     print,p,ntrials,sig
     if sig gt 1 then sig=1.
     sim[i].significance=sigprob(1.-sig)

     n=10
     tint=findgen(100)/100.*n*10.+(period[wm[0]]-n*5.)
     if wm-n gt 0 then wmn=wm-n else wmn=0
     if wm+n lt np-1 then wmnp=wm+n else wmnp=np-1
     fint=interpol(lpow[wmn:wmnp],period[wmn:wmnp],tint)

     sigma=1.                   ;2.706 ;;; 90%, or do multiple confidence levels
     del1=interpol(tint[0:50],fint[0:50],lpow[wm]-sigma)
     del2=interpol(tint[49:*],fint[49:*],lpow[wm]-sigma)

     sim[i].fit_period_err=[period[wm]-del1,del2-period[wm]]

     sim[i].accuracy=sim[i].fit_period-sim[i].period
     if period[wm] lt 0 then stop

     tint2=findgen(100)/100.*n*10.+(period[wm2[0]]-n*5.)
     if wm2-n gt 0 then wmn2=wm2-n else wmn2=0
     if wm2+n lt np-1 then wmnp2=wm2+n else wmnp2=np-1
     fint2=interpol(lpow[wmn2:wmnp2],period[wmn2:wmnp2],tint2)

     sigma=1.                   ;2.706 ;;; 90%, or do multiple confidence levels
     del12=interpol(tint2[0:50],fint2[0:50],lpow[wm2]-sigma)
     del22=interpol(tint2[49:*],fint2[49:*],lpow[wm2]-sigma)

     sim[i].fit_period_err2=[period[wm2]-del22,del12-period[wm2]]



     if i eq 0 then begin
;        begplot,name='~/iLobster/simulations/smbhb_example.ps',/land,/color,font='helvetica'

        ;; p=plot(t,flux2,xtitle='Days',ytitle='Flux (0.3-5 keV) (erg cm-2 s-1)',symbol='dot',linestyle='none',layout=[1,2,1],yrange=[0.02,3]*sim[i].flux,margin=[0.2,0.1,0.1,0.1],thick=2)
        ;; p2=plot(t0,f0*sim[0].flux,color='green',/overplot,layout=[1,2,1],thick=3)

        ;; p4=plot(period,pow,xtitle='Period',ytitle='Lomb pow',/xlog,layout=[1,2,2],/current,thick=2,yrange=[0,max(pow)])
        ;; p5=plot([period[wm],period[wm]],!y.crange,color='orange',/overplot,layout=[1,2,2])
        ;; p6=plot([52.,52.]*7.,!y.crange,/overplot,layout=[1,2,2])
        ;; p7=plot(tint,fint,color='red',/overplot,layout=[1,2,2])
        ;; p8=plot([del1,del1],!y.crange,color='orange',linestyle=2,/overplot,layout=[1,2,2])
        ;; p9=plot([del2,del2],!y.crange,color='orange',linestyle=2,/overplot,layout=[1,2,2])
        ;; p.save,'~/iLobster/simulations/smbhb_example.png'
        ;; p.refresh
        ;; p.close
     endif 

     if keyword_set(doplot) or i eq 0 then begin ;and sim[i].visfrac lt 1. then begin 
        if i eq 0 then begplot,name=simname+'_example.ps',/color,font='helvetica'
        !p.multi=[0,1,4]
        plot,t0,sim[i].visible,xrange=[0,ndays],/xsty,charsize=1.5
        legend,'RA/Dec='+numdec(sim[i].ra,0)+','+numdec(sim[i].dec,0),/top,/right,box=0
        plot,t,x6,xtitle='Days',ytitle='Pink Noise',xrange=[0,ndays],/xsty,charsize=1.5
;        plot,t,flux2,xtitle='Days',ytitle='Flux (0.3-5 keV) (erg cm-2 s-1)',psym=3,yrange=[0.02,3]*sim[i].flux,xrange=[0,ndays],/xsty,charsize=1.5
        plot,t,rate2*hanning(n_elements(t)),psym=3
;        oplot,t,f*((ctr+backrate)/ctsflux),color=!green,psym=3
        oplot,t,(f-1.)*ctr,color=!green,psym=3

        plot,period,lpow,xtitle='Period',ytitle='Lomb pow',/xlog,charsize=1.5,yrange=[0,lpow[wm]]
        oplot,[period[wm],period[wm]],!y.crange,color=!orange
        oplot,[period[wm2],period[wm2]],!y.crange,color=!yellow
        oplot,[per,per],!y.crange
        oplot,tint,fint,color=!red
        oplot,tint2,fint2,color=!red
        oplot,[del1,del1],!y.crange,color=!orange,line=2
        oplot,[del2,del2],!y.crange,color=!orange,line=2
        !p.multi=0
        if i eq 0 then endplot else begin 
           k=get_kbrd(10)
           if k eq 's' then stop
        endelse 
     endif 
;k=get_kbrd(10)
  endfor 
  mwrfits,sim,simname+'.fits',/create
;  mwrfits,sim,'~/iLobster/simulations/smbhb.fits',/create
  spawn,'convert '+simname+'_example.ps '+simname+'_example.pdf'

  skip:
  ;; need to remove some sampling
  ;; need plot of accuracy of period vs precision of measuring period
  ;; need to vary flux, period, modulation to get limits

;  plothist,sim.accuracy
;  begplot,name='~/iLobster/simulations/smbhb.ps',/land,font='helvetica',/color
  p=plot(sim.accuracy,-sim.fit_period_err[0],symbol='dot',linestyle='none',xtitle='Accuracy (days)',ytitle='Precision (days, 68% confidence)',yrange=[-50,100])
  p1=plot(sim.accuracy,sim.fit_period_err[1],symbol='dot',color='green',/overplot,linestyle='none')

  t1=text(0.2,0.8,'m='+ntostr(m*100,2)+'%',font_size=10)
  t2=text(0.2,0.75,'p='+ntostr(per,3)+' days',font_size=10)
  t3=text(0.2,0.7,'flux='+numdec(flux,1,/sci)+'$ erg cm^{-2} s^{-1}$',font_size=10)


  p.save,simname+'.png'
  p.refresh
  p.close

  wgood=where(sim.fit_period-sim.fit_period_err[0] gt 0)
;  sim=sim[wgood]
  p=errorplot(sim.fit_period,sim.lomb_pow,sim.fit_period_err,fltarr(n_elements(sim)),symbol='dot',linestyle='none',xtitle='Fit Period (days)',ytitle='Periodigram Power',xrange=[10,2000],/xlog,errorbar_capsize=0.,/xstyle)
  p2=plot([per,per],[0,round(max(sim.lomb_pow)/10.+1.)*10.],/overplot,linestyle='--',color='red')
  p3=errorplot(sim.fit_period2,sim.lomb_pow2,sim.fit_period_err2,fltarr(n_elements(sim)),symbol='dot',linestyle='none',errorbar_color='green',/overplot,errorbar_capsize=0.,color='green')

  for i=0,1 do begin
     if i eq 0 then begin
        fp=sim.fit_period
        fp_err=sim.fit_period_err
        lp=sim.lomb_pow
        off=0.
        color='black'
     endif else begin
        fp=sim.fit_period2
        fp_err=sim.fit_period_err2
        lp=sim.lomb_pow2
        off=0.3
        color='green'
     endelse 

     if i eq 0 then begin 
        t1=text(0.2,0.8-off,'m='+ntostr(m*100,2)+'%',font_size=8,color=color)
        t2=text(0.2,0.77-off,'p='+ntostr(per,3)+' days',font_size=8,color=color)
        t3=text(0.2,0.74-off,'flux='+numdec(flux,1,/sci)+'$ erg cm^{-2} s^{-1}$',font_size=8,color=color)
        t3=text(0.2,0.71-off,'red noise amplitude = '+numdec(rednoise_amp,2),font_size=8,color=color)
     endif 
     nsig=3. & ntrials=2592.

     con1=consistent(sim.period,fp,fp_err,wcon=w1,nw=nw1)
     con2=consistent(sim.period,fp,fp_err*2.,wcon=w2,nw=nw2)
     con3=consistent(sim.period,fp,fp_err*3.,wcon=w3,nw=nw3)
     con4=consistent(sim.period,fp,0.05*sim.period,wcon=w4,nw=nw4)

     t6=text(0.5,0.8-off,numdec(nw4*100./ntrials,1)+' right within 5%',color=color,font_size=8)
     t5=text(0.5,0.77-off,'('+numdec(nw1*100./ntrials,1)+','+numdec(nw2*100./ntrials,1)+','+numdec(nw3*100./ntrials,1)+')% right within $(1,2,3)\sigma$',font_size=8,color=color)

     sigthresh=mpchilim(-((1-gauss_cdf(nsig))/2./1.)+1d,1)
     w0=where(lp gt sigthresh,nw0)  
     con1=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*1.,wcon=w1,nw=nw1)
     con2=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*2.,wcon=w2,nw=nw2)
     con3=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*3.,wcon=w3,nw=nw3)
     t4=text(0.5,0.73-off,numdec(nw0*100./ntrials,1)+'% Pow > '+ntostr(fix(sigthresh))+' (3$\sigma, 1 trial$)',font_size=8,color=color)
     t4=text(0.5,0.70-off,'  ('+numdec(nw1*100./nw0,1)+','+numdec(nw2*100./nw0,1)+','+numdec(nw3*100./nw0,1)+')% right within $(1,2,3)\sigma$',font_size=8,color=color)

     sigthresh=mpchilim(-((1-gauss_cdf(nsig))/2./ntrials)+1d,1)
     w0=where(lp gt sigthresh,nw0)  
     con1=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*1.,wcon=w1,nw=nw1)
     con2=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*2.,wcon=w2,nw=nw2)
     con3=consistent(sim[w0].period,sim[w0].fit_period,sim[w0].fit_period_err*3.,wcon=w3,nw=nw3)
;  w1=where(abs(sim[w0].fit_period-sim[w0].period)-abs(sim[w0].fit_period_err[0])*1. lt 0,nw1)
;  w2=where(abs(sim[w0].fit_period-sim[w0].period)-abs(sim[w0].fit_period_err[0])*2. lt 0,nw2)
;  w3=where(abs(sim[w0].fit_period-sim[w0].period)-abs(sim[w0].fit_period_err[0])*3. lt 0,nw3)
     t4=text(0.5,0.67-off,numdec(nw0*100./ntrials,1)+'% Pow > '+ntostr(fix(sigthresh))+' (3$\sigma, '+ntostr(nsim,4)+' trials$)',font_size=8,color=color)
     if nw0*1./ntrials gt 1e-3 then begin 
;        t4=text(0.5,0.63-off,'Of Those: ',font_size=8,color=color)
        t4=text(0.5,0.64-off,'  ('+numdec(nw1*100./nw0,1)+','+numdec(nw2*100./nw0,1)+','+numdec(nw3*100./nw0,1)+')% right within $(1,2,3)\sigma$',font_size=8,color=color)
;     t4=text(0.6,0.67,'  '+numdec(nw2*100./nw0,1)+'% right within $2\sigma$',font_size=8)
;     t4=text(0.6,0.64,'  '+numdec(nw3*100./nw0,1)+'% right within $3\sigma$',font_size=8)
     endif 
  endfor 
  p.save,simname+'_pow_p.png'
  p.refresh
  p.close
  
;stop

  w=where(abs(sim.accuracy) lt per*0.1,n10) ; and fp_err[0] lt per*0.1,n10)
  print,'within 10%: ',n10*1./nsim
  w=where(abs(sim.accuracy) lt per*0.2,n20) ; and fp_err[0] lt per*0.2,n20)
  print,'within 20%: ',n20*1./nsim
  w=where(abs(sim.accuracy) lt per*0.3,n30) ; and fp_err[0] lt per*0.3,n30)
  print,'within 30%: ',n30*1./nsim

  w=where(fp+fp_err[1] gt sim.period and fp-fp_err[0] lt sim.period,nw) ;;; what fraction of time are we right within errors
  print,'right within 1-sigma errors: ',nw*1./nsim
  nsig=2.
  w=where(fp+fp_err[1]*nsig gt sim.period and fp-fp_err[0]*nsig lt sim.period,nw) ;;; what fraction of time are we right within errors
  print,'right within 2-sigma errors: ',nw*1./nsim
  nsig=3.
  w=where(fp+fp_err[1]*nsig gt sim.period and fp-fp_err[0]*nsig lt sim.period,nw) ;;; what fraction of time are we right within errors
  print,'right within 3-sigma errors: ',nw*1./nsim

  ptime,systime(1)-pt
  bleep
;  legend,['Flux = '+ntostr(sim[0].flux
;  endplot
;  spawn,'ps2pdf ~/iLobster/simulations/smbhb.ps ~/iLobster/simulations/smbhb.pdf'
  
;  stop
  return
end 
