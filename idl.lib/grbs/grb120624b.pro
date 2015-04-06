pro grb120624b

  ccts=25.
  cexp=10e3
  cf=1.616e-14
  cferr=sqrt(ccts)/ccts*cf
  trigtime=362269182.016d
  ct=date2met('2012-06-30-8:56:22')+5000.-trigtime

  readcol,'~/Desktop/GRB120624B/flux.qdp',time,errpos,errneg,flux,fluxerr,format='(d,d,d,d,d)'

  ploterror,time,flux,fluxerr,/nohat,psym=3,/xlog,/ylog,xtitle='Time (s)',ytitle='Flux (erg cm!U-2!N s!U-1!N)'
  for i=0,1 do oplot,[time[i]+errneg[i],time[i]+errpos[i]],[flux[i],flux[i]],color=!black

  plots,ct,cf,psym=2,color=!red
  oplot,[ct,ct],[cf-cferr,cf+cferr],color=!red
  oplot,[ct-5e3,ct+5e3],[cf,cf],color=!red

return
end 
