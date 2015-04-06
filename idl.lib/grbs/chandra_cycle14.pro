@fit_functions
@fit_functions_flares
pro chandra_cycle14

  ;;; eiso
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  begplot,name='~/proposals/Chandra_cycle14/eiso_plots.eps',/encap,/color,font='helvetica',/land
  sgrbs=where(grbstr.who eq 'BAT' and grbstr.eiso ne 0)
  lgrbs=where(grbstr.who eq 'LAT' and grbstr.eiso ne 0)
  ggrbs=where(grbstr.who eq 'GBM' and grbstr.eiso ne 0)

  plot,[48,56],[0,25],/nodata,/xsty,ytitle='N',xtitle='log E!L'+!tsym.gamma+',iso, 10 keV-10 MeV!N (erg)',charsize=2
  plothist,alog10(grbstr[sgrbs].eiso),bin=0.3,/over,/fill,fcolor=!grey50,color=!grey50
;  plothist,alog10(grbstr[ggrbs].eiso),bin=0.3,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
  plothist,alog10(grbstr[lgrbs].eiso),bin=0.3,/over,color=!red,line=0
  oplot,[0,100],[0,0]
;  legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red] ,charsize=2
  legend,['BAT','LAT'],/top,/right,box=0,textcolor=[!grey50,!red] ,charsize=2
  endplot

  ;;; should plot GBM fluence vs burst date - highlighting co-swift &
  ;;;                                         swift follow-up
  ;;; shit - don't have these numbers past GBM catalog
  ;;; need also only those in LAT FoV

  stop
  ;;; table data

  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB100728A','GRB110625A','GRB110731A']  
  nlat=n_elements(latgrbs)

  cd,'~/GRBs/'

  t30=86400.*30.
  t60=86400.*60.
;  f30=dblarr(nlat) & f60=f30 & r30=f30 & r60=f30 & x30=f30 & x60=f30
;  & xr30=f30 & xr60=f30
  if not exist('~/proposals/Chandra_cycle14/lat_bursts.fits') then begin 
     lat=create_struct('grb','','pnames',strarr(25),'p',fltarr(25),$
                       'phind',0.,'unabs_cfratio',0d,'z',0d,'model','',$
                       'xrt_rate30',0d,'xrt_rate60',0d,$
                       'cxo_rate30',0d,'cxo_rate60',0d,$
                       'flux30',0d,'flux60',0d,$
                       'exp30',0d,'exp60',0d)
     lat=replicate(lat,nlat)

     for i=0,nlat-1 do begin 
        lat[i].grb=latgrbs[i]
        lcfitfile=latgrbs[i]+'/lc_fit_out_idl_int8.dat'
        if not exist(lcfitfile) then lcfitfile=latgrbs[i]+'/lc_fit_out_idl_int7.dat'
        read_lcfit,lcfitfile,pnames,p,perr,np=np
        spec=mrdfits(latgrbs[i]+'/UL_specfits.fits',1)
        ns=n_elements(spec)
        mo=fit_models(pnames,p,np,nf)
        lat[i].model=mo
        lat[i].p[0:n_elements(p)-1]=p
        lat[i].pnames[0:n_elements(pnames)-1]=pnames

        tmp1=execute('lat[i].xrt_rate30='+mo+'(t30,p)')
        tmp2=execute('lat[i].xrt_rate60='+mo+'(t60,p)')
        if tmp1 eq 0 or tmp2 eq 0 then stop
        lat[i].phind=spec[ns-1].phind
        lat[i].z=spec[ns-1].z
        lat[i].unabs_cfratio=spec[ns-1].unabs_cfratio
        lat[i].flux30=lat[i].xrt_rate30*lat[i].unabs_cfratio
        lat[i].flux60=lat[i].xrt_rate60*lat[i].unabs_cfratio

        pimmsfile='pimms.xco'
        com1='model pl '+ntostr(spec[ns-1].phind)+' '+ntostr(spec[ns-1].nh)+' z '+ntostr(spec[ns-1].z)+' '+ntostr(spec[ns-1].nhgal*1d22)
        com2='from flux photons 0.3-10.0'
        com3='instrument chandra acis-s 0.2-10.0'
        com4='go '+ntostr(lat[i].flux30)+' flux ergs 0.3-10.0'
        com5='output pimms.dat 0.2 10.0 0.005'
        com6='quit'
        writecol,pimmsfile,[com1,com2,com3,com4,com5,com6]
        spawn,'pimms @'+pimmsfile+'> pimms.log'
        readcol,'pimms.log',lines,delim='$',format='(a)'
        for l=0,n_elements(lines)-1 do begin
           spos=strpos(lines[l],'predicts')
           if spos ne -1 then begin
              str=str_sep(lines[l],' ')
              ctr30=str[3]
           endif
        endfor 
        lat[i].cxo_rate30=ctr30
        lat[i].cxo_rate60=lat[i].flux60/lat[i].flux30*ctr30
        
        ncounts=10.
        lat[i].exp30=ncounts/lat[i].cxo_rate30
        lat[i].exp60=ncounts/lat[i].cxo_rate60

     endfor 

     mwrfits,lat,'~/proposals/Chandra_cycle14/lat_bursts.fits',/create
  endif else lat=mrdfits('~/proposals/Chandra_cycle14/lat_bursts.fits',1)
stop
  a=' & '
  f=1e-14
  for i=0,nlat-1 do begin
     
     w=where(lat[i].p ne 0)
     mo=fit_models(lat[i].pnames[w],lat[i].p[w],np,nf)
     
     print,lat[i].grb,a,lat[i].p[np-1],a,lat[i].flux30,a,lat[i].flux60,a,lat[i].cxo_rate30,a,lat[i].cxo_rate60,a,lat[i].exp30,a,lat[i].exp60
  endfor 
;  colprint,latgrbs,xr30,xr60,f30,f60,r30,r60,x30,x60

     stop
     return
  end 
