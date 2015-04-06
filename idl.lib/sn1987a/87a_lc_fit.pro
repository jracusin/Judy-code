pro 87a_lc_fit

  readcol,'~/SN87A/fluxes.dat',day,f1,f1err,f2,f2err

  ploterror,day,f1,f1err,psym=3,/nohat

  stop
return
end 
