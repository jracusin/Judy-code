pro make_het_plot,ps=ps
  
  cd,'~/Desktop/GRB080319B/nino/tmp/'
  
  if keyword_set(ps) then begplot,name='het_spec.eps',/land,/color,/encaps,/cmyk
  readcol,'flusdiv.txt',wave,flux
  
  plot,wave,flux/1e-16,xrange=minmax(wave),/xsty,yrange=[-0.5,7],/ysty,xtitle='Wavelength ('+!tsym.angstrom+')',ytitle='F!L'+!tsym.lambda+'!N(10!U-16!N erg cm!U-2!N s!U-1!N)',xmargin=[15,0]
  
  colors=[!p.color,!blue,!red,!green]
;  legend,['z = 0.937','z = 0.760','z = 0.715','z = 0.530'],/top,/right,textcolor=colors,box=0,charsize=1.5

  legend,['z = 0.937','z = 0.715'],/top,/right,textcolor=colors[[0,2]],box=0,charsize=1.5
  
  lines=[5406,5426,5524,5751,5855,4532.791,4564.49,6548.31,7445,4781,4800,4881.5,8414,6022,6077]
  linenames=['MgII','','MgI','FeI','','MnII','','FeI','CaI','MgII','','MgI','NaI','CaII H & K','']
  z=[0,0,0,0,0,1,1,1,1,2,2,2,3,3,3]
  w=where(z eq 0 or z eq 2)
  z=z[w]
  lines=lines[w]
  linenames=linenames[w]
  move=[120,0,100,30,0,120,0,100,100,120,0,100,100,200,0]
  move=move[w]
  
  for i=0,n_elements(lines)-1 do begin
     pl=[2.5,2.8]
     if lines[i] gt 4700 then pl=[2.3,2.6]
     if lines[i] gt 4800 then pl=[1.6,1.9]
     if lines[i] gt 5400 then pl=[2.3,2.6]
     if lines[i] gt 5500 then pl=[1.6,1.9]
     if lines[i] gt 5700 then pl=[2.1,2.4]
     if lines[i] gt 6000 then pl=[1.9,2.2]
     if lines[i] gt 6500 then pl=[1.7,2.]
     if lines[i] gt 7000 then pl=[1.3,1.6]
     if lines[i] gt 8000 then pl=[0.5,0.8]
     oplot,[lines[i],lines[i]],pl,color=colors[z[i]]
     xyouts,lines[i]-move[i],pl[0]-0.3,linenames[i],color=colors[z[i]],charsize=1.
  endfor 
  
  atmos=[6560,6800,5570,7600,9350]
  y=[3.2,2.9,5.0,2.4,1.9]
  xyouts,atmos-20,y,!tsym.earth,charsize=1.0
  
  if keyword_set(ps) then endplot
  
  stop
  return
end 
