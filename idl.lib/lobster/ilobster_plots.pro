@fit_functions
@fit_functions_flares
@calc_eiso2
pro synergy

  xrange=[2017,2025]
  yrange=[0,8]
  p=plot(xrange,yrange,xrange=xrange,yrange=yrange,/nodata,xtitle='Year',margin=[0.13,0.1,0.13,0.1],xminor=1,yminor=0,ymajor=0)
  
  width=0.8
  b=0.2
  
  obs=create_struct('name','','year',fltarr(2),'color','','comp_name',strarr(5),'comp_year',fltarr(5))
  obs=replicate(obs,7)
  
  ;;; iLobster
  i=3
  obs[i].name='ISS-Lobster'
  obs[i].year=[2020,2025]
  obs[i].color='red'
  obs[i].comp_name=['Prime','Extended']
  obs[i].comp_year=[2022]
  
  ;;; LIGO
  i=6
  obs[i].name='Advanced LIGO/Virgo'
  obs[i].year=[2015,2025]
  obs[i].color='cornflower'
  obs[i].comp_name=['Commissioning','Design Sensitivity','+LIGO India']
  obs[i].comp_year=[2019,2022]
  
  ;;; JWST
  i=1
  obs[i].name='JWST'
  obs[i].year=[2018.75,2025]
  obs[i].color='spring green'

  ;;; LSST
  i=2
  obs[i].name='LSST'
  obs[i].year=[2023,2025]
  obs[i].color='dark orange'

  ;;; NICER
  i=4
  obs[i].name='NICER'
  obs[i].year=[2016.9,2025]
  obs[i].color='yellow'
  obs[i].comp_name=['Prime','Extended']
  obs[i].comp_year=[2018.5]

  ;;; Fermi
  i=5
  obs[i].name='Fermi'
  obs[i].year=[2008,2025]
  obs[i].color='magenta'
  obs[i].comp_name='Extended'

  ;;; LOFAR/MWA
  i=0
  obs[i].name='LOFAR/MWA'
  obs[i].year=[2012,2025]
  obs[i].color='grey'

  obs0=obs
  w=where(obs.year[0] lt xrange[0])
  obs[w].year[0]=xrange[0]


  for i=0,n_elements(obs)-1 do begin 
     x=[obs[i].year[0],obs[i].year[0],obs[i].year[1],obs[i].year[1],obs[i].year[0]]
     y=[i+width,i+b,i+b,i+width,i+width]
     p1=polygon(x,y,/data,fill_color=obs[i].color,transparency=20,thick=2,color='grey')
     t1=text(obs[i].year+0.2,i+0.8,obs[i].name,/data,font_style=1)
     if strtrim(obs[i].comp_name[0],2) ne '' then begin 
        wcomp=where(strtrim(obs[i].comp_name,2) ne '',ncomp)
        year=[obs[i].year[0],obs[i].comp_year[0:ncomp-1],obs[i].year[1]]
        for j=0,ncomp-1 do begin 
           t2=text(year[j]+0.1,i+width/2.,obs[i].comp_name[j],/data,font_size=10)
           p2=plot([year[j+1],year[j+1]],[i+b,i+width],linestyle='--',/data,/overplot)
        endfor 
     endif 
     print,obs[i].name
  endfor 

  p.save,'~/iLobster/2014_proposal/synergy_plot.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close  

return
end 

function bb,e,kt
  f=8.05*e^2/((kt^4.)*(exp(e/kt)-1))
return,f
end 

pro grbspec,sylvain=sylvain

  f=dindgen(90)/10.+1.
  e=[f/10.,f,f*10.,f*1e2,f*1e3,f*1e4]
  kev2erg=1.602e-12*1d3

;  n=kev2erg^2*1e-2;9e-20;
  n=1e-17
  ;;; band only
  if keyword_set(sylvain) then begin 
     ep=44.                     ;47.
     alpha=0.75                 ;0.29
     beta=-2.29                 ;-2.12
     nb=1.3                     ;157.2
;     nb=157.2
  endif else begin 
     ep=47.
     alpha=0.29
     beta=-2.12
     nb=3.
;     ep=98.
;     alpha=-0.29
;     beta=-2.28
;     nb=6.
  endelse 

  e0=ep/(2.+alpha)
  fb=band2(e,alpha,e0,beta)*n*nb

  ;; band+bb
  if keyword_set(sylvain) then begin 
     ;;; +0.044 - +0.054
;     ep=111.
;     alpha=-0.24
;     beta=-2.40
     ep=231.
     alpha=-1.12
     kt=10.99
     nbb=50.
     nb2=40
;    nbb=100.3e-3
;     nb2=95.83
  endif else begin 
     ;;; +0.054 - +0.080
;     ep=433.
;     alpha=-1.36
;     beta=-6.
;     kt=11.81
;     nbb=18.
;     nb2=32.
     ;;; +0.014 - +0.022
;     ep=501.
;     alpha=-1.32
;     beta=-3
;     kt=20.24
;     nbb=60.
;     nb2=60.
  endelse 
  e0=ep/(2.+alpha)
  fbb=bb(e,kt)*n*nbb
  fb2=cutoff_pl2(e,alpha,ep)*n*nb2
;  fb2=band2(e,alpha,e0,beta)*n*nb2

  begplot,name='~/iLobster/grbspec.eps',/color,font='helvetica',/land
  plot,[1,1],[1,1],/nodata,/xlog,/ylog,xrange=[1e-1,4e4],/xsty,yrange=[1e-17,1e-13],charsize=2,/ysty,xtitle='Energy (keV)',ytitle=!tsym.nu+'F!L'+!tsym.nu+'!N (erg!U2!N cm!U-2!N s!U-1!N kev!U-1!N)',xtickformat='loglabels'
  polyfill,[0.3,6,6,0.3,0.3],[1e-17,1e-17,1e-13,1e-13,1e-17],color=!grey90
  polyfill,[10,1000,1000,10,10],[1e-17,1e-17,1e-13,1e-13,1e-17],color=!grey90
;  polyfill,[8,4e4,4e4,8,8],[1e-17,1e-17,3e-17,3e-17,1e-17],orient=45,color=!grey50
  xyouts,2,5e-14,'ISS-Lobster',charsize=2
  xyouts,0.8,3e-14,'WFI',charsize=2
  arrow,0.75,3.5e-14,0.3,3.5e-14,/data,/solid,thick=5,hthick=3
  arrow,2.5,3.5e-14,6,3.5e-14,/data,/solid,thick=5,hthick=3
  xyouts,50,3.2e-14,'GTM',charsize=2
  arrow,43,3.5e-14,10,3.5e-14,/data,/solid,thick=5,hthick=3
  arrow,220,3.5e-14,1000,3.5e-14,/data,/solid,thick=5,hthick=3
  xyouts,95,1.3e-17,'Fermi-GBM',charsize=2
  arrow,80,1.5e-17,8,1.5e-17,/data,/solid,thick=5,hthick=3
  arrow,3e3,1.5e-17,4e4,1.5e-17,/data,/solid,thick=5,hthick=3
  arrow,1.,2.5e-17,1.,2.8e-16,/data,/solid;,thick=5,hthick=3
  arrow,1.,2.8e-16,1.,2.5e-17,/data,/solid

  axis,xaxis=0,xtickname=replicate(' ',6),xrange=[1e-1,4e4],/xsty
  axis,xaxis=1,xtickname=replicate(' ',6),xrange=[1e-1,4e4],/xsty
  axis,yaxis=0,ytickname=replicate(' ',5),yrange=[1e-17,1e-13],/ysty
  oplot,e,e^2*fb,color=!blue,thick=20
  w=where(e^2*(fb2+fbb) gt n*1e-3)
  oplot,e[w],e[w]^2*(fbb[w]+fb2[w]),color=!red,thick=20
  oplot,e[w],e[w]^2*fbb[w],color=!red,line=2,thick=10
  oplot,e[w],e[w]^2*fb2[w],color=!red,line=2,thick=10

  xyouts,2e3,1.2e-14,'Band',color=!blue,charsize=2
  xyouts,2.5e3,5e-16,'Band + ',color=!red,charsize=2
  xyouts,2.5e3,2e-16,'Blackbody',color=!red,charsize=2

;  legend,['Band','Band + Blackbody'],/top,/left,textcolor=[!blue,!red],box=0,charsize=2

  endplot

  spawn,'convert ~/iLobster/grbspec.eps ~/iLobster/grbspec.pdf'
  stop

  return
end 
pro shb_plot

  grb=mrdfits('~/Swift/swiftgrb.fits',1)
  dir='~/stuff_for_people/Nora/'
  readcol,dir+'comblist.csv',stuff,format='(a)',delim='#'
  nstuff=n_elements(stuff)
  grbs=strarr(nstuff) & mission=grbs & ee=grbs
  for i=0,nstuff-1 do begin
     chunks=str_sep(stuff[i],',')
     grbs[i]=chunks[0]
     if strmid(grbs[i],0,1)*1. gt 1 then grbs[i]='0'+grbs[i]
     mission[i]=chunks[1]
     ee[i]=chunks[2]
  endfor 
;  w=where(strtrim(ee,2) ne 'EE' or )
;  grbs=grbs[w]
;  ee=ee[w]

  match,'GRB'+strtrim(grbs,2),strtrim(grb.name,2),m1,m2
  dont_match,'GRB'+strtrim(grbs,2),strtrim(grb.name,2),dm1,dm2

  grb=grb[m2]
;  w=where(grb.bat_t90 lt 20.)
;  grb=grb[w]

  begplot,name='~/iLobster/sgrb_plot.eps',/land,/color,font='helvetica'
  !x.margin=[5,1]
  !y.margin=[2,0]
  xrange=[10,1e6]
  yrange=[1e-13,1e-5]
;  yrange=[1e-15,1e-7]
  plot,[10,1e6],[1e-13,1e-5],/xlog,/ylog,/nodata,xtitle='Time (s)',ytitle='0.3-5.0 keV Flux (erg cm!U-2!N s!U-1!N)',xrange=xrange,yrange=yrange,/xsty,/ysty,charsize=2,yticks=8,yminor=9,title='X-ray afterglows of sGRBs scaled to 400 Mpc' ;,title='X-ray afterglows real flux'
  t=15.*60.
  t2=t+40.*60.
  thresh=1e-11
  mthresh=25./8*3.5e-10
;  ltime=[1,2,5,10,20,50,100,200,400]
;  lflux=[7.3e-9,3.5e-9,1.5e-9,1.1e-9,6.2e-10,2.4e-10,1.5e-10,7.0e-11,4.9e-11]
;  lobtime=[ltime,1e3,1e4,1e5,1e6-600,1e6,2e6,1e7]+100.
;  lobtime=[lobtime,10]
;  lobtime=lobtime[sort(lobtime)]
;  lobflux=10^interpol(alog10(lflux),alog10(ltime),alog10(lobtime))

  lobtime=[10,20,50,100,200,400,1000,2000,1e4,2e4,4e4,8e4]
;  lobflux=[9.29e-10,4.64e-10,1.86e-10,9.29e-11,4.64e-11,2.61e-11,1.33e-11,7.71e-12,2.7e-12,1.88e-12,1.27e-12,8.47e-13] ;; 4 sigma
   lobflux=[9.29e-10,4.64e-10,1.86e-10,1.28e-10,7.22e-11,4.01e-11,2.07e-11,1.24e-11,4.38e-12,2.90e-12,1.82e-12,1.01e-12] ;; 6 sigma
  nl=n_elements(lobtime)
  lobtime=[lobtime,1e5,1e6,2e6,1e7]
  lobflux=[lobflux,replicate(1e-12,4)];,lobflux[nl-1]/sqrt(lobtime[nl:*]/lobtime[nl-1])]

  w=where(lobtime ge 10 and lobtime le 1e6)
  lobtime=lobtime[w]
  lobflux=lobflux[w]


  nl=n_elements(lobtime)


;  polyfill,[xrange[0],xrange[1],xrange[1],t2,t2,t,t,xrange[0],xrange[0]],[yrange[0],yrange[0],thresh,thresh,thresh,thresh,yrange[1],yrange[1],yrange[0]],color=!grey70
;  polyfill,[xrange[0],lobtime,xrange[1],xrange[1],xrange[0]]+600.,[yrange[0],lobflux,yrange[0],yrange[0],yrange[0]],color=!grey70
;  polyfill,[xrange[0],xrange[0],t,t,xrange[0]],[yrange[0],yrange[1],yrange[1],yrange[0],yrange[0]],color=!grey70
;  polyfill,[1.,1.,11.,11.,1.]*60.,[yrange[0],yrange[1],yrange[1],yrange[0],yrange[0]],color=!lightsteelblue
;  polyfill,[xrange[0],lobtime,xrange[1],xrange[0]],[yrange[0],lobflux,yrange[0],yrange[0]],color=!grey70
  axis,xrange=xrange,/xsty,xtickname=replicate(' ',7)
  axis,xaxis=1,xrange=xrange,/xsty,xtickname=replicate(' ',7)
  axis,yaxis=0,yrange=yrange,/ysty,ytickname=replicate(' ',9),yticks=8,yminor=9
  mpc2cm=3.08568025d24
  dist200=400*mpc2cm
  eng=dindgen(98)/10.+0.3
  de=0.1
  plotsym,1,3
  detect=intarr(n_elements(grb))
  mdetect=intarr(n_elements(grb))
  f100=fltarr(n_elements(grb))
  f1000=fltarr(n_elements(grb))
  mos=strarr(n_elements(grb))
  for i=0,n_elements(grb)-1 do begin
     dir='~/GRBs/'+strtrim(grb[i].name,2)+'/'
     if exist(dir+'UL_specfits.fits') and exist(dir+'PCCURVE.qdp') then begin 
        lc=lcout2fits(dir+'lc_newout_phil.txt')
        spec=mrdfits(dir+'UL_specfits.fits',1)

        z=grb[i].redshift
;        color2=!red
;        color=!firebrick
        color=!p.color
        if z eq 0. then begin 
           z=0.7
           color=!grey70
;           color2=!blue
;           color=!royalblue
        endif 
        dist=lumdist(z,h0=71,lambda=0.73,omega_m=0.27)
        dist=dist*mpc2cm
        r=spec[0].cfratio*4.*!pi*dist^2
        z1=1.+z

        ;;; need to convert from 0.3-10 to 0.3-6 keV
        conv=total(pow(eng[0:57],[1.,spec[0].phind])*de)/total(pow(eng,[1.,spec[0].phind])*de)
        
        r=spec[0].unabs_cfratio*conv*dist^2/dist200^2

        w=where(lc.src_rate_err ne 0,nw)
        if nw gt 1 then begin
           oplot,lc[w].time/z1,lc[w].src_rate*r,color=color

           lcfile=dir+'lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile=dir+'lc_fit_out_idl_int7.dat'
           read_lcfit,lcfile,pnames,p
           mo=fit_models(pnames,p)
           mos[i]=mo
           if mo ne '' then begin 
              tmp=execute('yfit='+mo+'(lc[w].time,p)')
              tmp=execute('f100[i]='+mo+'(100.,p)*r')
              tmp=execute('f1000[i]='+mo+'(1000.,p)*r')
           endif 
;           oplot,lc[w].time/z1,yfit*r,color=color
        endif 

        w=where(lc.src_rate_err ne 0 and lc.time gt t and lc.src_rate*r gt thresh,nw)
        if nw gt 0 then detect[i]=1
        w=where(lc.src_rate_err ne 0 and lc.time gt 45.*60. and lc.src_rate*r gt mthresh,nw)
        if nw gt 0 then mdetect[i]=1

     endif else detect[i]=3  ;;; no actual light curve
  endfor 

;  oplot,lobtime,lobflux,line=2
  !p.charthick=1
  !p.charsize=2
  c=!p.color
  c=!orange
  
  oplot,[11,11],[yrange[0],6e-6],thick=20,color=c
  xyouts,12.,3e-6,'Tiling of GTM Sky Region',color=c
  arrow,11,2e-6,25.*60.,2e-6,/data,thick=20,hsize=!D.X_SIZE /40.,/solid,color=c

;  c=!orange
;  oplot,[4.*60,4.*60.],[yrange[0],1.5e-6],thick=20,color=c
;  xyouts,4.*60+30.,7e-7,'Follow-up Fermi-GBM Position',color=c
;  arrow,4.*60,4.5e-7,600.,4.5e-7,/data,thick=20,hsize=!D.X_SIZE / 40.,/solid,color=c

  c=!blue
  oplot,[10.*60,10.*60],[yrange[0],2e-7],thick=20,color=c
  xyouts,10.*60+100.,1e-7,'Follow-up GW Trigger Position',color=c
;  xyouts,8.5e3,1e-7,'hemisphere',color=c
  arrow,10.*60,6e-8,2000.,6e-8,/data,thick=20,hsize=!D.X_SIZE / 40.,/solid,color=c

;  c=!magenta
;  oplot,[55*60.,55*60.],[9e-12,4e-8],thick=20,color=c
;  xyouts,60.*60,2e-8,'Follow-up GW opposite',color=c
;  xyouts,4e4,8.5e-9,'hemisphere',color=c
;  arrow,55.*60,1e-8,7e3,1e-8,/data,thick=20,hsize=!D.X_SIZE / 40.,/solid,color=c

;  legend,['Known Redshift','Unknown Redshift','(Assume z=0.7)'],/right,/center,textcolor=[!red,!blue,!blue],box=0,charsize=2
;        xyouts,15,3e-12,'WFI Threshold',charsize=2
;  xyouts,15,1.5e-11,'WFI Threshold',charsize=2
;  xyouts,15,1e-11,'WFI 6'+!tsym.sigma,color=!black
;  xyouts,15,4e-12,'Sensitivity',color=!black
;  arrow,30,3d-11,30,3e-10,/data,thick=5,hthick=3,/solid

;        xyouts,30,4e-6,'Average time to ',charsize=2
;        xyouts,30,1e-6,'begin follow-up ',charsize=2
;        arrow,1e2,5e-7,1e3,5e-7,/data,/solid,thick=5,hthick=3

  !p.charsize=1.8
  c=!red
  oplot,[50,1e6],[lobflux[2],lobflux[2]],thick=20,color=c,line=5
;  polyfill,[10.1,575,575,10.1,10.1],[lobflux[2],lobflux[2],7e-11,7e-11,lobflux[2]],color=!c
  polyfill,[4e4,7e5,7e5,4e4,4e4],[6e-10,6e-10,2.5e-10,2.5e-10,6e-10],color=!white
  xyouts,4.3e4,3e-10,'50 s Sensitivity',color=c

  oplot,[2000,1e6],[lobflux[7],lobflux[7]],thick=20,color=c,line=5
;  polyfill,[550,1e6,1e6,550,550],[lobflux[7],lobflux[7],5e-12,5e-12,lobflux[7]],color=!c
  polyfill,[3e4,8e5,8e5,3e4,3e4],[4e-11,4e-11,1.7e-11,1.7e-11,4e-11],color=!white
  xyouts,3.1e4,2e-11,'2000 s Sensitivity',color=c

  legend,['Known Redshift','Assumed Median z=0.7'],/top,/right,box=0,textcolor=[!p.color,color]
  
  endplot
  spawn,'convert ~/iLobster/sgrb_plot.eps ~/iLobster/sgrb_plot.pdf'
  help,detect
  nd=n_elements(detect)
  w0=where(detect eq 0,nw0)
  w1=where(detect eq 1,nw1)
  w2=where(detect eq 3,nw2)
  print,nd,nw0,nw1,nw2
  n=nd-nw2
  print,n,nw1*1./n
  
  stop

  return
end

pro ilobster_plots

  ;;; simplified transient_plot
  ;;; redshift distributions plot
  
  begplot,name='~/iLobster/z_mission_lifetime.eps',/encap,font='helvetica',/color
  !x.margin=[10,2]
  z=[0,5,6,10,12,14]
  det=[290,28,14,2.1,1.1,0.5]/6. ;;; Lobster-sat rate/year divided by 2 for eff area, and 3 for fraction of redshifts
;  det=[det-det[1:*],det[n_elements(det)-1]]
;  bl=[313,65,42,6.7,2.7,1.1]
  sz=[0,1,2,3,4,5,6,8]
;  sdet=[146,101,72,36,13,5,2,1]
  sgrb=mrdfits('~/Swift/swiftgrb.fits',1)
  w=where(sgrb.redshift ne 0 and strtrim(sgrb.redshift_type,2) eq 'S' or strtrim(sgrb.redshift_type,2) eq 'P')
  sgrb=sgrb[w]
  nsz=n_elements(sz)
  sdet=intarr(nsz)
  for i=0,nsz-1 do begin
     w=where(sgrb.redshift ge sz[i],nw); and sgrb.redshift lt sz[i+1],nw)
     sdet[i]=nw
  endfor 
;  sdet=[sdet,0.0001]
;  sz=sz[0:nsz-1]
;  sdet=[sdet-sdet[1:*],sdet[n_elements(sdet)-1]]
;  colprint,sz,sdet
;  sdet[nsz-1]=2

  aplot,1,[0,10],[1,1000],/nodata,/ylog,xtitle='Redshift z',ytitle='Number of GRBs with Redshift > z',yrange=[1,1000],/ysty,/xsty,ytickformat='loglabels'
  zz=indgen(141)/10.
  d=10^interpol(alog10(det),z,zz,/spline)
;  b=10^interpol(alog10(bl),z,zz,/spline)
  s=10^interpol(alog10(sdet),sz,zz,/spline)
;  oplot,z,det*3,psym=1,color=!red,symsize=5
;  oplot,z,bl*3,psym=1,color=!red,symsize=5
;  oplot,sz,sdet
  oplot,zz,d*2,thick=10,color=!blue
  oplot,zz,d*4,thick=10,color=!red
;  oplot,zz,b*3,line=1,thick=5,color=!blue
;  oplot,zz,b*6,line=1,thick=5,color=!red
  oplot,zz,s,line=2,thick=10,color=!green

  legend,['ISS-Lobster in 2 years','ISS-Lobster in 4 years','Swift Observed (8 years)'],/top,/right,line=[0,0,2],thick=[10,10,10],box=0,color=[!blue,!red,!green],charsize=1.5
;  xyouts,1,100,'1 yr'
;  xyouts,2.5,100,'Butler et al. 2010';'3 yrs'
;  xyouts,8.5,100,'Bromm & Loeb 2002';color=!red,'6 yrs'
;  legend,['3 yrs','6 yrs'],textcolor=[!blue,!red],/bottom,/left,box=0
;  xyouts,1,20,'Butler et al. 2010';'3 yrs'
;  xyouts,7,200,'Bromm & Loeb 2002';color=!red,'6 yrs'

  endplot
  spawn,'convert ~/iLobster/z_mission_lifetime.eps ~/iLobster/z_mission_lifetime.pdf'

  begplot,name='~/iLobster/compare_ilobster.eps',/encap,font='helvetica',/color,/land

  !x.margin=[8,1]
  !y.margin=[14,0]
  plot,[1995,2025],[8e-12,1e-9],/nodata,xtitle='Year',/ylog,/xsty,/ysty,charsize=2,yrange=[8e-12,1e-9],title='Full Sky Sensitivity in 1 day'

  xyouts,1990.8,8e-12,'0.3-5.0 keV Sensitivity',charsize=2,orient=90
  xyouts,1992,2e-11,'(erg cm!U-2!Ns!U-1!N)',charsize=2,orient=90

  ;;;lob,bat,maxi,asm
  ltime=[2017.75,2020,2024]
  lflux=1.24e-11

  mtime=[2009.75,2014]
  mflux=3.2e-10

  btime=[2004.9,2014]
  bflux=5e-10

  atime=[1996,2011]
  aflux=4e-10

;  oplot,ltime,[lflux,lflux]
;  oplot,mtime,[mflux,mflux]
  oplot,atime,[aflux,aflux],thick=10
  xyouts,atime[0],aflux*1.2,'RXTE/ASM',charsize=2

  arrow,ltime[0],lflux,ltime[1],lflux,/data,thick=10,hsize=!D.X_SIZE / 64.
  oplot,[ltime[1],ltime[2]],[lflux,lflux],line=2,thick=10
  xyouts,ltime[0]-8,lflux*1.2,'ISS-Lobster/WFI',charsize=3.

  arrow,btime[0],bflux,btime[1],bflux,/data,thick=10,hsize=!D.X_SIZE / 64.
  xyouts,btime[0],bflux*1.2,'Swift/BAT',charsize=2

  arrow,mtime[0],mflux,mtime[1],mflux,/data,thick=11,hsize=!D.X_SIZE / 64.
  xyouts,mtime[0],mflux*0.6,'MAXI',charsize=2

  endplot
  spawn,'convert ~/iLobster/compare_ilobster.eps ~/iLobster/compare_ilobster.pdf'
 !y.margin=[4,0]
 
  begplot,name='~/iLobster/cumulative_grbs.eps',/encap,font='helvetica',/color,/land
  
  plot,[5,14],[0,25],/nodata,xtitle='Redshift',ytitle='Cumulative Number of High-z GRBs',/xsty,/ysty,charsize=2
  ;;; 2-year mission
  d=d*2.
  cdet=d
  w=where(zz ge 5 and zz le 9)
  for i=0,n_elements(d)-2 do cdet[i]=d[i]-d[i+1:*] ;det[0]-(total(det)-total(det[0:i]))
  oplot,zz[w],[0,cumulative(cdet[w])],color=!blue,thick=10
  xyouts,7,10,'ISS-Lobster GRBs (2 year mission)',color=!blue,charsize=2
  w2=where(zz ge 9)
  oplot,zz[w2],[cumulative(cdet[w2])+max(cumulative(cdet[w]))],color=!blue,thick=10,line=2

  ;;; 4-year extended mission
  d=d*2.
  cdet=d
  w=where(zz ge 5 and zz le 11)
  for i=0,n_elements(d)-2 do cdet[i]=d[i]-d[i+1:*] ;det[0]-(total(det)-total(det[0:i]))
  oplot,zz[w],[0,cumulative(cdet[w])],color=!red,thick=10
  xyouts,7,19,'ISS-Lobster GRBs (4 year mission)',color=!red,charsize=2
  w2=where(zz ge 11)
  oplot,zz[w2],[cumulative(cdet[w2])+max(cumulative(cdet[w]))],color=!red,thick=10,line=2

  ws=where(zz ge 5 and zz le max(sgrb.redshift))
  csdet=cumulative(s)
  for i=0,n_elements(s)-2 do csdet[i]=s[i]-s[i+1:*]
  oplot,zz[ws],[0,cumulative(csdet[ws])],color=!green,thick=10
  xyouts,8.4,6,'Swift GRBs (8 years)',color=!green,charsize=2

  xyouts,7,23,'Cumulative Distributions',charsize=2
  endplot
  spawn,'convert ~/iLobster/cumulative_grbs.eps ~/iLobster/cumulative_grbs.pdf'



;;; rate comparison

  z=findgen(10)
  l=[290,250,170,105,54,28,14,8,5,3.5];/2.
  s=[103,85,53,29,14,5.7,2.5,1.2,0.7,0.4]
  zz=indgen(101)/10.
  lz=10^interpol(alog10(l),z,zz,/spline)
  sz=10^interpol(alog10(s),z,zz,/spline)

  plothist,sgrb.redshift,xhist,yhist,bin=0.2,/noplot

  begplot,name='~/iLobster/compare_rate_grbs.eps',/encap,font='helvetica',/color,/land
    
  plot,[0,10],[1e-1,400],/ylog,/nodata,xtitle='Redshift z!L0!N',ytitle='z > z!L0!N GRB Rate (yr!U-1!N)',/xsty,/ysty,charsize=2,ytickformat='loglabels'
  xx=[zz,reverse(zz)]
  s=1.64
;  lzu=lz+sqrt(lz)*s
;  sigl=sqrt(lz)*s/(lz*alog(10))*3.
  sigl=sqrt(lz*s)
  lzu=lz+sigl
  lzd=reverse(lz-sigl)
  yy=[lzu,lzd]
  polyfill,xx,yy,color=!lightblue
  oplot,zz,lzu,color=!blue,line=2,thick=10
  oplot,zz,reverse(lzd),color=!blue,line=2,thick=10


;  oplot,z,l,color=!blue,thick=10;,line=2
  oplot,zz,lz,color=!blue,thick=10
  nz=n_elements(zz)

;  oplot,z,s,color=!red,thick=10,line=2
  sigs=sqrt(sz)/2.
  szu=sz+sigs
  szd=reverse(sz-sigs)
;  w=where(szu le 0.1)
;  szu[w]=0.101
  w=where(szd le 0.1)
;  wd=where(szd ge 0.1)
  szd[w]=0.101
  yy=[szu,szd]
;  wu=where(szu ne 0.101)
  polyfill,xx,yy,color=!salmon
  oplot,zz,szu,color=!red,line=2,thick=10
  oplot,zz,reverse(szd),color=!red,line=2,thick=10
  oplot,zz,sz,color=!red,thick=10

  oplot,zz,lz/3.,color=!blue,line=1,thick=10

  n=n_elements(yhist)
  y=fltarr(n)
  for i=0,n-1 do y[i]=total(yhist[i:*])
  oplot,xhist,y/8.,line=1,psym=10
  axis,yaxis=1,yticks=3,yrange=[1e-1,400],/ysty,ytickv=[0.1,1,10,1e2],ytickname=replicate(' ',4),yminor=9
  axis,yaxis=0,yticks=3,yrange=[1e-1,400],/ysty,ytickv=[0.1,1,10,1e2],ytickname=replicate(' ',4),yminor=9
  axis,xaxis=0,xtickname=replicate(' ',6)

  xyouts,5,70,'All ISS-Lobster GRBs',color=!blue,charsize=2
;  oplot,zz,s/7.5,color=!red,thick=10;,line=2
  xyouts,1,50,'All Swift GRBs',color=!red,charsize=2,orient=330
  xyouts,0.5,3,'Swift Measured',charsize=2
  xyouts,0.5,130,'Predicted ISS-',color=!blue,charsize=2,orient=340
  xyouts,3.1,40,'Lobster measured z',color=!blue,charsize=2,orient=328

;  xyouts,2,43,'Distribution of Measured GRB Redshifts',charsize=2
  endplot
  spawn,'convert ~/iLobster/compare_rate_grbs.eps ~/iLobster/compare_rate_grbs.pdf'



  stop
  return
end 
