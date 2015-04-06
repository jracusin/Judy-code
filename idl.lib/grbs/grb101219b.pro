@fit_functions
@fit_functions_flares
pro grb101219b

  ;;; plot for paper by Josefin Larsson

  cd,'~/GRBs/GRB101219B'

  begplot,name='GRB101219B_XRT_LC.eps',/encap,/color,/land,font='helvetica'
  spec=mrdfits('UL_specfits.fits',1)
  plot_like_qdp,flux=spec[1].unabs_cfratio,lc=lc
  read_lcfit,'lc_fit_out_idl_int9.dat',pnames,p

  time=[lc.time,p[2],p[4]]
  s=sort(time)
  time=time[s]
  f=gauss1_bkn2pow(time,p)*spec[1].unabs_cfratio
  f2=bkn2pow(time,p[0:5])*spec[1].unabs_cfratio
  f1=gauss(time,p[6:8])*spec[1].unabs_cfratio
  oplot,time,f
  oplot,time,f1,line=1
  oplot,time,f2,line=1
  oplot,[p[2],p[2]],[1e-14,1e-6],color=!green,line=2
  oplot,[p[4],p[4]],[1e-14,1e-6],color=!green,line=2
  endplot

  return
end 
  
