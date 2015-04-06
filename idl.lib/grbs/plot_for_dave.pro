@fit_functions
pro plot_for_dave

  cd,'~/GRBs/GRB061121'

  begplot,name='~/Desktop/grb061121_latetime_w_chandra.ps',/land,font='helvetica'
  plot_like_qdp,/phil,xrange=[1e4,1e7],yrange=[1e-5,10],ytickformat='loglabels',lc=lc,/nocolor

  flux=2.1315212d-15         
  spec=mrdfits('UL_specfits.fits',1)

  ch=flux/spec[1].unabs_cfratio

  plotsym,1,3,thick=5
  chtime=5370903.9                          
  tstart=5353207.4                          
  tstop=5388600.4                          
  plots,chtime,ch,psym=8;,color=!green
  oplot,[tstart,tstop],[ch,ch];,color=!green

  read_lcfit,'lc_fit_out_idl_int7.dat',pnames,p           
  oplot,lc.time,bkn3pow(lc.time,p)             
  time=[lc.time,p[2],p[4],p[6],1d7]   
  time=time(sort(time))
  oplot,time,bkn3pow(time,p)    
  
  tlast=1.13949e+06
  
  newp=[p,tlast,p[7]+1]
  w=where(time ge tlast)

  oplot,time[w],bkn4pow(time[w],newp),line=2

  endplot
end 
