pro weird_burst

  goto,skip
  gdir='~/GRBs/'
  cd,gdir
  grbs=strtrim(file_search('GRB*'),2)

  ng=n_elements(grbs)
  
;  begplot,name='~/Desktop/GRB101225A/composite_xrt_lcs_cts.ps',/color,/land
  begplot,name='~/Desktop/GRB101225A/composite_xrt_lcs_flux.ps',/color,/land
;  plot,[10,1e7],[1d-5,1d4],/xlog,/ylog,/nodata,xtitle='Time since trigger (s)',ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)'
  plot,[10,1e7],[1d-15,1d-6],/xlog,/ylog,/nodata,xtitle='Time since trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)'

  for i=0,ng-1 do begin
     cd,grbs[i]
     print,grbs[i]
     file='lc_newout_phil.txt'
     if exist(file) then begin 
        if numlines(file) gt 5 then begin 
           lc=lcout2fits(file)
           if exist('UL_specfits.fits') then begin 
              spec=mrdfits('UL_specfits.fits',1,/silent)
              ns=n_elements(spec)       
              f=spec[ns-1].unabs_cfratio
;           f=1.
              oploterror2,lc.time,lc.src_rate*f,[[lc.time-lc.tstart],[lc.tstop-lc.time]],[[lc.src_rate_err],[lc.src_rate_err]]*f,psym=3,/nohat
           endif 
        endif 
     endif 
     cd,'..'
  endfor 

  cd,'~/Desktop/GRB101225A/'
  lc2=lcout2fits(/phil)
  spec=mrdfits('UL_specfits.fits',1,/silent)
  ns=n_elements(spec)       
  f=spec[ns-1].unabs_cfratio
;           f=1.

  oploterror2,lc2.time,lc2.src_rate*f,[[lc2.time-lc2.tstart],[lc2.tstop-lc2.time]],[[lc2.src_rate_err],[lc2.src_rate_err]]*f,psym=3,/nohat,color=!red,errcolor=!red

  endplot

  skip:
  begplot,name='~/Desktop/GRB101225A/GRB060218_050904_101225A_compare.ps',/color,/land
  cd,'~/Desktop/GRB101225A/'
  lc2=lcout2fits(/phil)

  lc=lcout2fits('~/GRBs/GRB060218/lc_newout_phil.txt')
  lc3=lcout2fits('~/GRBs/GRB050904/lc_newout_phil.txt')

  ploterror,lc.time,lc.src_rate,lc.src_rate_err,/nohat,psym=3,/xlog,/ylog,xrange=[10,1e7],yrange=[1d-4,1d3],/ysty,xtitle='Time since trigger (s)',ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)'
  for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]
  oploterror,lc2.time,lc2.src_rate,lc2.src_rate_err,psym=3,errcolor=!red,color=!red,/nohat
  for i=0,n_elements(lc2)-1 do oplot,[lc2[i].tstart,lc2[i].tstop],[lc2[i].src_rate,lc2[i].src_rate],color=!red
;  oploterror,lc3.time,lc3.src_rate,lc3.src_rate_err,psym=3,errcolor=!blue,color=!blue,/nohat
;  for i=0,n_elements(lc3)-1 do oplot,[lc3[i].tstart,lc3[i].tstop],[lc3[i].src_rate,lc3[i].src_rate],color=!blue


  legend,['GRB 060218','GRB 101225A','GRB 050904'],box=0,textcolor=[!p.color,!red,!blue],/top,/right
  endplot
  stop

  return
end
     
