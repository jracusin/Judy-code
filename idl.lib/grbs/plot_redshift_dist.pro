pro plot_redshift_dist

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  l=where(g.t90 ge 2. and g.z ne 0)
  s=where(g.t90 le 2. and g.t90 ne 0 and g.z ne 0)

  bin=0.2

  calc_cdf,g[s].z,xs,ps
  calc_cdf,g[l].z,xl,pl

  begplot,name='~/Swift/redshift_dist.eps',/encap,/land,/color,font='helvetica'
  xrange=[0,10]
  plot,xrange,[0,1],/nodata,xrange=xrange,/xsty,xtitle='z',ytitle='P',charsize=2.,title='Cumulative Redshift Distribution',thick=10
  plot_cdf,xl,pl,/over,color=!grey50,thick=10
  plot_cdf,xs,ps,/over,color=!red,line=1,thick=10

  legend,['Long','Short'],/bottom,/right,box=0,textcolor=[!grey50,!red],line=[0,1],color=[!grey50,!red],thick=10,charsize=2
  endplot
  spawn,'convert ~/Swift/redshift_dist.eps ~/Swift/redshift_dist.pdf'

return
end 
