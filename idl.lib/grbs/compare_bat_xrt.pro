@calc_eiso2
@fit_functions
@fit_functions_flares
pro compare_bat_xrt,l

  ;;; plot bat fluence vs XRT flux at 240 ks.
  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog_parse.fits',1)
  bat=mrdfits('~/jetbreaks/batcat.fits',1)

  convert_gbm_bat_fluence,1.916e-4,0.002e-4,10,1000.,566.,-0.85,-2.36,sb
  x=0.002*3.1e-11
  cd,'~/GRBs'

  if n_elements(l) gt 10 then goto,skipcrap

  grbs=file_search('GRB*/')
  n=n_elements(grbs)
  t=240e3 ;;; 240 ks
  l=create_struct('grb','','bat_fluence',0.,'bat_fluence_err',0.,'xrt_ctr',0d,'xrt_ctr_err',dblarr(2),'xfact',0d,'xrt_flux',0d,'xrt_flux_err',dblarr(2))
  l=replicate(l,n)

  s1=round(1000.*(1-0.67)/2.)
  s2=round(1000.*(1.-(1-0.67)/2.))

  for i=0,n-1 do begin
     cd,grbs[i]
     print,grbs[i]
     l[i].grb=grbs[i]
     lcfile='lc_fit_out_idl_int8.dat'
     if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'
     if exist(lcfile) and exist('UL_specfits.fits') then begin 
        read_lcfit,lcfile,pnames,p,perr
        if pnames[0] ne 'nofit' then begin 
           lc=lcout2fits(/phil)
           det=where(lc.src_rate_err gt 0)
           if max(lc[det].tstop) gt t then begin 
              spec=mrdfits('UL_specfits.fits',1)
              if n_elements(spec) eq 1 then xs=n_elements(spec)-1
              corr=spec[xs].unabs_cfratio

              l[i].xfact=corr
              mo=fit_models(pnames,p,np,nf,basemo=basemo)
              tmp=execute('f='+mo+'(t,p)')
              l[i].xrt_ctr=f
              mcfile='lc_fit_out_idl_int8_mc.fits'
              if not exist(mcfile) then mcfile='lc_fit_out_idl_int7_mc.fits'
              if exist(mcfile) then begin 
                 mcfit=mrdfits(mcfile,1)
                 c=dblarr(1000)

                 for j=0,999 do begin 
                    mcp=0.
                    for k=0,n_elements(p)-1 do mcp=[mcp,mcfit[j].(k)]
                    mcp=mcp[1:*]
                    tmp=execute('c[j]='+mo+'(t,mcp)')
                 endfor 
                 s=sort(c)
                 l[i].xrt_ctr_err[0]=c[s[500]]-c[s[s1]]
                 l[i].xrt_ctr_err[1]=c[s[s2]]-c[s[500]]
              endif 
              w=where(strtrim(bat.grb,2) eq strtrim(l[i].grb,2),nw)
              if nw gt 0 then begin 
                 l[i].bat_fluence=bat[w[0]].sbat_15_150
                 l[i].bat_fluence_err=bat[w[0]].sbat_15_150_err
              endif 
           endif 
        endif 
     endif 
     cd,'..'
  endfor 

  w=where(l.xrt_ctr_err[0] lt 1e-6)
  l[w].xrt_ctr_err[0]=0.1*l[w].xrt_ctr
  l[w].xrt_ctr_err[1]=0.1*l[w].xrt_ctr

  l.xrt_flux=l.xrt_ctr*l.xfact
  l.xrt_flux_err[0]=l.xrt_ctr_err[0]*l.xfact
  l.xrt_flux_err[1]=l.xrt_ctr_err[1]*l.xfact
  skipcrap:

  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090902B','GRB090926A','GRB091003'];,'GRB100414A']  

  g=[53,165,168,292,310,316]
  bat_fluence=dblarr(6)
  for i=0,n_elements(g)-1 do begin 
     convert_gbm_bat_fluence,gbm[g[i]].fluence_band,gbm[g[i]].fluence_band_err,10,1000.,gbm[g[i]].epeak_band,gbm[g[i]].alpha_band,gbm[g[i]].beta_band,b
     bat_fluence[i]=b
  endfor 

  b=[419,485,486,544,550,555]  ;;;lat bursts followed-up by swift
  
  l[b].bat_fluence=bat_fluence

  j=[504,633,714,722]  ;;; joint bat/lat triggers

  begplot,name='~/Fermi/Swift_Fermi/BAT_fluence_XRT_flux_LAT_bursts.ps',font='helvetica',/color
;simpctable
  !p.multi=[0,1,2]
  w=where(l.bat_fluence ne 0 and l.xrt_flux ge 5e-15)
  plotsym,0,0.8,/fill
;  loadct,4
  blue=!blue
  green=!green
  red=!red
  plot,l[w].bat_fluence,l[w].xrt_flux,psym=8,/xlog,/ylog,xtitle='BAT Fluence (15-150 keV)',ytitle='XRT Flux @240 ks (0.3-10 keV)',xrange=[1e-8,1e-4],yrange=[1e-15,1e-11]
;  ploterror,l[w].bat_fluence,l[w].xrt_flux,l[w].bat_fluence_err,l[w].xrt_flux_err[0],psym=8,/nohat,/xlog,/ylog,xtitle='BAT Fluence (15-150 keV)',ytitle='XRT Flux @240 ks (0.3-10 keV)',xrange=[1e-8,1e-4],yrange=[1e-15,1e-11]

  oplot,l[b].bat_fluence,l[b].xrt_flux,psym=2,color=blue

  oplot,l[j].bat_fluence,l[j].xrt_flux,psym=2,color=green

;  oploterror,l[b].bat_fluence,l[b].xrt_flux,l[b].bat_fluence_err,l[b].xrt_flux_err[0],psym=2,/nohat,color=blue,errcolor=blue

;  oploterror,l[j].bat_fluence,l[j].xrt_flux,l[j].bat_fluence_err,l[j].xrt_flux_err[0],psym=2,/nohat,color=green,errcolor=green

  plotsym,1,2,thick=4
  plots,sb,x,psym=8,color=red
  plots,sb,x,psym=2,color=red

  legend,['BAT GRBs','GBM/LAT GRBs w/ Swift Follow-up','BAT/GBM/LAT joint triggers','GRB 120624B'],textcolor=[!p.color,blue,green,red],box=0,charsize=1.

  bb=where(l[b].bat_fluence ne 0 and l[b].xrt_flux ne 0)
  bb=b[bb]

  jj=where(l[j].bat_fluence ne 0 and l[j].xrt_flux ne 0)
  j=j[jj]

  xx=x
  plot,[5,10],[0,35],/nodata,xtitle='log BAT Fluence/XRT Flux'
  plothist,[4.,alog10(l[w].bat_fluence/l[w].xrt_flux),12.],bin=0.1,x,y,/noplot
  oplot,x,y,psym=10
  plothist,[4.,alog10(l[b].bat_fluence/l[b].xrt_flux),12.],x,y,bin=0.1,/over,color=!blue,/noplot
  oplot,x,y,color=!blue,psym=10
  plothist,[5.,alog10(sb/xx),12.],x,y,bin=0.1,/over,color=!red,/noplot
  oplot,x,y,color=!red,psym=10
  plothist,[4.,alog10(l[j].bat_fluence/l[j].xrt_flux),12.],bin=0.1,/over,color=!green,x,y,/noplot
  oplot,x,y,color=!green,psym=10

  oplot,[5,10],[0,0]
  !p.multi=0
  endplot
stop
  return
end 
  
  
