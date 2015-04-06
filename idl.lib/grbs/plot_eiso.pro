pro plot_eiso,cr,last,w4,w3g,w4g,gold,silver,pewter,iron,nsig=nsig,chandra=chandra

  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  if n_elements(cr) eq 0 then cr=mrdfits(!adata+'closure_relations_total'+signame+'.fits',1)
  
  ;;;PRE-SWIFT EISO & THETA & EGAMMA FROM BLOOM ET AL. (2003)
  preiso=[141.6,54.57,2198.2,2105.36,53.55,601.2,14378.92,8614.96,1763.49,2559.52,49.7,1121.55,5353.69,11618.99,1693.26,437.49,751.37,3954.79,2797.36,8578.41,136.11,455.59,672.34,720.08,7749.76,556.01,378.24]
   
  pretheta=[21.63,7.13,11.21,4.93,3.36,5.33,4.57,13.14,22.3,6.16,3.2,31.19,6.38,7.81,3.13,12.67]
  prez=[0.8349,0.9578,0.9662,1.6004,1.6187,0.8424,1.02,2.0335,1.181,2.0369,1.4768,0.4509,2.14,0.6899,1.254,2.332]
  preiso2=[54.57,2198.2,601.2,14378.92,1763.49,2559.52,5353.69,437.49,751.37,2797.36,8578.41,136.11,672.34,720.08,7749.76,556.01]
  pregam=[3.84,17.00,11.47,53.14,3.04,11.05,17.03,11.46,56.20,16.17,13.33,19.67,4.17,6.68,11.58,13.53]
  pretbreak=[25,2.2,3.4,2.04,1.2,1.0,1.2,7.3,25.,1.8,0.93,33.,1.77,1.67,0.43,7.6]
  pren=[1.0,10.,28.0,10.,0.29,10.,4.7,27.0,27.0,27.0,1.7,10.,10.,10.,10.,30.0]
  
  if keyword_set(chandra) then begin 
     readcol,'~/Chandra/energetics.txt',cgrb,cjbt,ceiso,ctheta,cegam,class,delim=',',format='(a,d,d,d,d,i)'
  ;;; 1 = no jb
  ;;; 2 = jb
  ;;; 3 = maybe jb (upper lims)
  endif 

  !x.margin=[12,0]
  outdir='~/papers/jetbreaks1/'
  yrange=[0,10]
  xrange=[48,55]
;  last4=w4[w4g]
  jbs=gold;w4[w4g]
  w=where(cr[jbs].z gt 0 and cr[jbs].eiso ne 3.7 and cr[jbs].eiso gt 0)
  jbs=jbs[w]
  mjbs=[silver,pewter,iron]
  w=where(cr[mjbs].z gt 0 and cr[mjbs].eiso ne 3.7 and cr[mjbs].eiso gt 0)
  mjbs=mjbs[w]

;  w=where(cr[jbs].eiso gt 0 and cr[jbs].eiso ne 3.7); and cr[jbs].who_eiso ne 'shb')
;  ws=where(cr[jbs].eiso gt 0 and cr[jbs].eiso ne 3.7); and cr[jbs].who_eiso eq 'shb')
  wm=where(cr[mjbs].eiso gt 0 and cr[mjbs].eiso ne 3.7)
  begplot,name=outdir+'eiso_dist.eps',/land,/color,/encap
  
  plot,xrange,yrange,/nodata,xtitle='log E!L'+!tsym.gamma+',iso!N (erg)',ytitle='N',xrange=xrange,/xstyle,yrange=yrange,/ysty
  plothist,alog10(cr[jbs].eiso*1d52),bin=0.4,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=!grey70,color=!grey70,fline=0
  plothist,alog10(cr[mjbs].eiso*1d52),bin=0.4,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,line=2;min=xrange[0],max=xrange[1]
  plothist,alog10(preiso*1d50),/over,bin=0.4,/fill,/fline,forient=45,xrange=xrange,/xstyle,yrange=yrange,/ysty;,min=xrange[0],max=xrange[1]

  if keyword_set(chandra) then begin 
     cno=where(class eq 1)
     cyes=where(class eq 2)
     cmb=where(class eq 3)
     ccolor=[!red,!green,!blue]
     
     plothist,alog10(ceiso[cno]),bin=0.4,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[0],color=ccolor[0],/fline
     plothist,alog10(ceiso[cyes]),bin=0.4,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[1],color=ccolor[1],/fline
     plothist,alog10(ceiso[cmb]),bin=0.4,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[2],color=ccolor[2],/fline
  endif 
     
;  plothist,alog10(cr[jbs[ws]].eiso*1d52),/over,bin=0.4,xrange=xrange,/xstyle,color=!red,yrange=yrange,/ystty,min=xrange[0],max=xrange[1]
  oplot,xrange,[0,0]
  oplot,[0,0],yrange
  endplot

;;;E-gamma  
  theta=cr.theta
;  w=where(cr[jbs].tbreak gt 0)
;  jbs=jbs[w]
;  type_jb,cr,w4[w3g],wonlyei=wonlyei,/silent,nsig=nsig
;  theta[w4[w4g[wonlyei]]]=theta[w4[w3g[wonlyei]]]
;  wz=where(cr[jbs].z gt 0)
;  theta=theta[jbs]
;  mwz=where(cr[mjbs].z gt 0)
;  mtheta=theta[mjbs]
  egam=alog10(cr[jbs].eiso*1d52*(1.-cos(theta[jbs]*!dtor)))
  megam=alog10(cr[mjbs].eiso*1d52*(1.-cos(theta[mjbs]*!dtor)))
  w=where(egam gt 0)
  egam=egam[w]
  w=where(megam gt 0)
  megam=megam[w]
  help,pretbreak,prez,preiso2,pregam
  pretheta2=dblarr(n_elements(prez))
  for i=0,n_elements(prez)-1 do pretheta2[i]=jet_angle(pretbreak[i],z=prez[i],eiso=preiso2[i]*1d50);,n=pren[i])
  pregam2=alog10(preiso2*1d50*(1-cos(pretheta2*!dtor)))
  
  begplot,name=outdir+'egamma.eps',/land,/encap,/color
  xrange=[48,52]
  yrange=[0,10]
  
  plot,xrange,yrange,/nodata,xtitle='log E!L'+!tsym.gamma+'!N (erg)',ytitle='N',xrange=xrange,/xsty,yrange=yrange,/ysty
  plothist,egam,bin=0.2,xrange=xrange,/xsty,yrange=yrange,/ysyt,/over,fcolor=!grey70,color=!grey70,/fill,fline=0
  plothist,megam,bin=0.2,xrange=xrange,/xsty,yrange=yrange,/ysyt,/over,line=2,fline=0
  plothist,pregam2,bin=0.2,/over,xrange=[48,52],/xsty,/fill,/fline,forient=45,yrange=yrange,/ysty,min=xrange[0],max=xrange[1]

  if keyword_set(chandra) then begin
     ccolor=[!red,!green,!blue]
     plothist,alog10(cegam[cno]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[0],color=ccolor[0],/fline
     plothist,alog10(cegam[cyes]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[1],color=ccolor[1],/fline
     plothist,alog10(cegam[cmb]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[2],color=ccolor[2],/fline
  endif 


  oplot,xrange,[0,0]
  oplot,[0,0],yrange
  endplot

;;;THETA
  yrange=[0,10]
  xrange=[0,25]
  theta=cr[jbs].theta
  mtheta=cr[mjbs].theta
  begplot,name=outdir+'theta.eps',/land,/encap,/color
  
  plot,xrange,yrange,/nodata,xtitle=!tsym.theta+'!Lj!N (deg)',xrange=xrange,/xsty,ytitle='N',yrange=yrange,/ysty
  plothist,theta,x1,y1,bin=1,/over,xrange=xrange,yrange=yrange,/ysty,/fill,fcolor=!grey70,/xsty,fline=0,color=!grey70
  plothist,mtheta,bin=1,xrange=xrange,yrange=yrange,/xsty,/ysty,/over,line=2,fline=0
  plothist,pretheta2,x1,y1,bin=1,/over,xrange=xrange,yrange=yrange,/ysty,/fill,/fline,forient=45,/xsty,min=xrange[0],max=xrange[1]
  if keyword_set(chandra) then begin
     ccolor=[!red,!green,!blue]
     plothist,ctheta[cno],bin=1,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[0],color=ccolor[0],/fline
     plothist,ctheta[cyes],bin=1,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[1],color=ccolor[1],/fline
     plothist,ctheta[cmb],bin=1,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[2],color=ccolor[2],/fline
  endif 

  oplot,xrange,[0,0]
  oplot,[0,0],yrange
  endplot  

;;;TBREAK
  day=86400.
  yrange=[0,12]
  xrange=[3,7]
  tbreak=cr[jbs].tbreak         ;/86400.
  mtbreak=cr[mjbs].tbreak
  w=where(mtbreak gt 0)
  mtbreak=mtbreak[w]
  begplot,name=outdir+'tbreak.eps',/land,/encap,/color
  
  plot,xrange,yrange,/nodata,xtitle='log t!Lb!N (s)',xrange=xrange,/xsty,ytitle='N',yrange=yrange,/ysty
  plothist,alog10(tbreak),bin=0.2,xrange=xrange,/xsty,yrange=yrange,/ysty,/over,/fill,fcolor=!grey70,color=!grey70,fline=0
  plothist,alog10(mtbreak),x,y,bin=0.2,xrange=xrange,/xsty,yrange=yrange,/ysty,/over,line=2,fline=0
  plothist,alog10(pretbreak*day),bin=0.2,/over,xrange=xrange,yrange=yrange,/ysty,/fill,/fline,forient=45,line=0
  if keyword_set(chandra) then begin
     ccolor=[!red,!green,!blue]
     plothist,alog10(cjbt[cno]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[0],color=ccolor[0],/fline
     plothist,alog10(cjbt[cyes]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[1],color=ccolor[1],/fline
     plothist,alog10(cjbt[cmb]),bin=0.2,xrange=xrange,/xstyle,yrange=yrange,/ysty,/over,/fill,fcolor=ccolor[2],color=ccolor[2],/fline
  endif 

  oplot,alog10([day,day]),[0,100],line=1
  oplot,xrange,[0,0]
  oplot,[0,0],yrange
  endplot

  !x.margin=[8,3]
  begplot,name=outdir+'ks_gold_preswift.eps',/encap
  !p.multi=[0,1,2]
  kstwop,tbreak,pretbreak*day,d,prob,/plot,xtitle='t!Lb!N'
  kstwop,theta,pretheta*day,d,prob,/plot,xtitle=!tsym.theta+'!Lj!N'
  !p.multi=0
  endplot
stop


  return
end 
