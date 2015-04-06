pro jsi_talk

  g=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  w=where(g.eiso ne 0 and g.eiso ge 1d49)
  s=where(strtrim(g[w].dur,2) eq 'short')
  l=where(strtrim(g[w].dur,2) eq 'long')

  s=w[s]
  l=w[l]

  begplot,name='~/Swift/energetics_long_short.eps',/land,/color,/encap,font='helvetica'
  bin=0.2
  plot,[50,55],[0,20],xrange=[50,55],yrange=[0,20],/xsty,/ysty,xtitle='log E!Liso!N (erg)',ytitle='N',/nodata
  plothist,alog10(g[l].eiso),/fill,bin=bin,xrange=[50,55],fcolor=!grey,/over
  plothist,alog10(g[s].eiso),/fill,forient=45,bin=bin,color=!red,fline=1,/over,fcolor=!red,xrange=[50,55]
  legend,['Long','Short'],textcolor=[!grey70,!red],box=0,/top,/right,charsize=2
  endplot
  spawn,'convert ~/Swift/energetics_long_short.eps ~/Swift/energetics_long_short.pdf'

  stop
return
end
