pro GRB090902B_comb_lc

  cd,'~/Desktop/GRB090902B/'
  readcol,'lat3.dat',tstart,tstop,stat,flux,fluxerr1,fluxerr2

  time=(tstop-tstart)/2.+tstart

  latf2c=1.6e-6 ;; erg/MeV
  plot,time,flux*latf2c,psym=3,/xlog,/ylog,xrange=[1,1e6],yrange=[1e-14,1e-4],$
       xtitle='Time since trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-1!N s!U-1!N)'

  tstart[0]=1
  f1=flux-fluxerr1
  f2=flux+fluxerr2
  w=where(f1 lt 0)
  f1[w]=1e-14

  for i=0,n_elements(tstart)-1 do begin
     oplot,[tstart[i],tstop[i]],[flux[i],flux[i]]*latf2c
     oplot,[time[i],time[i]],[f1[i],f2[i]]*latf2c
  endfor 

  lc=lcout2fits(/phil)
  xrtf2c=7.8e-11
  oploterror,lc.time,lc.src_rate*xrtf2c,lc.src_rate_err*xrtf2c,/nohat,psym=3,color=!red,errcolor=!red
  for i=0,n_elements(lc)-1 do begin
     oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]*xrtf2c,color=!red
  endfor
  legend,['LAT','XRT'],box=0,textcolor=[!p.color,!red],/top,/right


stop
return
end
