pro grb070125_plots,ps=ps
  
  if keyword_set(ps) then begplot,name='grb070125_lc_flux_cxo.ps',/landscape,font='helvetica',/color
  
  cs=1.5
  ratio=5.40699e-13/0.00801581
  plot_like_qdp,charsize=cs,flux=ratio,xrange=[4e4,5e6],/xsty,xtitle='Time since trigger (s)',title=''
  
  plotsym,1,4,thick=6
  plots,3.435264e6,2e-15,psym=8  ;;chandra
  oplot,[3.420264e6,3.450264e6],[2e-15,2e-15]
  
  plotsym,0,1,/fill
  legend,['XRT',''],textcolor=[!red,!p.color],box=0,charsize=cs,psym=[8,3],color=[!red,!p.color],/top,/right
  plotsym,1,1.5,thick=3
  legend,['','Chandra'],textcolor=[!red,!p.color],box=0,charsize=cs,/top,/right,psym=[3,8],color=[!red,!p.color]
  
  if keyword_set(ps) then endplot
  
  return
end 
