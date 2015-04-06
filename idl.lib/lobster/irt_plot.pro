pro jeff_data

  dir='~/Lobster/Proposal/'

  file='lobster_sn_ft1.0_t300.txt'

  

return
end

pro irt_plot

  readcol,'~/Lobster/Proposal/highz-ir.csv',grb,z,eiso,time,mag,band,flux,format='(a,f,d,f,f,a,f)',skip=1

  eiso=eiso*1d52
  begplot,name='~/Lobster/Proposal/irt_plot.eps',/land,/encap,/color,font='helvetica'
  !x.margin=[12,1]
  !p.charsize=2.
;  !p.multi=[0,2,1]
  plotsym,0,2,/fill
;  colors=[!grey80,!grey60,!grey40,!grey20]
  colors=[!blue,!orange,!purple,!forestgreen]
  plot,[-1,20],[25,10],/nodata,psym=8,xtitle='Observed Time (hours)',ytitle='J Magnitude',/yno,xrange=[-1,20],/xsty,yrange=[25,10],/ysty,yminor=1,yticks=1
  cc=lonarr(n_elements(eiso))
  for i=0,3 do begin
     w=where(alog10(eiso) ge 51.+i and alog10(eiso) lt 51.+i+1.)
     oplot,time[w],mag[w],color=colors[i],psym=8
     cc[w]=colors[i]
  endfor 
  axis,yaxis=0,yminor=5,yticks=3
  axis,yaxis=1,ytickname=ntostr([-3,-2,-1,0,1,2]),ytickv=[23.,20.5,18.,15.5,13.,10.5],yticks=6,yminor=5,ytitle='log flux (mJy)'
  oplot,[-10,20],[23,23],line=2,color=!red
  xyouts,0,24,'IRT Imaging Sensitivity (300 s)',color=!red
  erg=['51','52','53','54','55']
  leg=['10!U'+erg[0:3]+'!N<E!L'+!tsym.gamma+',iso!N<'+'10!U'+erg[1:4]+'!N erg']
  leg=[leg[0],'',leg[1],'',leg[2],'',leg[3]]
  colors=[colors[0],0,colors[1],0,colors[2],0,colors[3]]
  legend,leg,/top,/right,charsize=1.5,color=colors,textcolor=colors

;  plot,time,alog10(flux),psym=8,/yno,xrange=[-1,15],/xsty,yrange=[-3.4,2.2],/ysty


;  w=where(mag lt 15,nw)
;  w=[w,indgen(5)*2+1]
;  w=w[rem_dup(w)]
;  w=w[sort(w)]
;  nw=n_elements(w)
  
  w=indgen(n_elements(grb))
  nw=n_elements(w)
;  yoff=[0,0,0,0.5,0,0.5,0,0,-0.5,0.5,0,0,-0.5,0.2,-0.5,0.2]
;  xoff=fltarr(nw)
;  xoff[[0,3,5,8,11]]=-5-time[w[[0,3,5,8,11]]]
;  xoff[[12,14]]=-2
  yoff=[0,0,0,0.6,0,0.8,0,0.5,0.5,0,-0.7,0,0.2,0,0.2]
  xoff=fltarr(nw)
  xoff[[5,10]]=-1-time[w[[5,10]]]
  xoff[3]=-1.3-time[w[3]]

  for i=0,nw-1 do xyouts,time[w[i]]+0.4+xoff[w[i]],mag[w[i]]+0.2+yoff[w[i]],charsize=1.5,numdec(z[w[i]],1),color=cc[w[i]];,grb[w[i]]+'(z='+numdec(z[w[i]],1)+')';+ntostr(i),color=cc[w[i]]
colprint,indgen(nw),time,mag,z

;  !p.multi=0
  endplot

  stop
  return
end 
