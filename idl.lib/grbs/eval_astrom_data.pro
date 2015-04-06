pro eval_astrom_data,grb
  
  w=where(grb.nsrc gt 0,nw)
  
  grbs=grb[w]
  
  toterr=grbs.new_xerr+grbs.oerr
  rmserr=sqrt(grbs.new_xerr^2+grbs.oerr^2)
  
  offset=grbs.xo_off-toterr ;;real offset outside error circles
  
  window,0
  !p.multi=[0,2,2]
  plothist,offset,bin=0.5,xtitle='X/O offset - (New Xerr + Oerr)',xrange=[-6,4]
  
  q=where(offset gt 0,nq)
  print,'Number of GRBs outside error circles: '+ntostr(nq)+' of '+ntostr(nw)
;  print,'Mean offset: '+ntostr(mean(offset))
  print,'Mean/Median X/O offset: '+ntostr(mean(grbs.xo_off))+' '+ntostr(median(grbs.xo_off))
  print,'Mean/Median New Xerr: '+ntostr(mean(grbs.new_xerr))+' '+ntostr(median(grbs.new_xerr))
  print,'Mean/Median of offset outside in all: '+ntostr(mean(offset))+' '+ntostr(median(offset))
  print,'Mean/Median of offset outside in outliers: '+ntostr(mean(offset[q]))+' '+ntostr(median(offset[q]))
  print,'Mean/Median of Expotime: '+ntostr(mean(grbs.expotime))+' '+ntostr(median(grbs.expotime))
;  t=where(grbs.nsrc gt 3,nt)
;  t2=where(offset[t] le 0.,nt2)
  nt=nw
  t2=where(offset le 0.,nt2)
  print,'Accuracy: '+ntostr(nt2*1./nt)
  
  p=where(grbs.nsrc le 3 and offset gt 0.)
  
  dra=(grbs.new_xra-grbs.ora)*cos(grbs.new_xdec*!dtor)*3600.
  ddec=(grbs.new_xdec-grbs.odec)*3600.
  plot,dra,ddec,psym=2,/iso,xtitle='dRA (New - Lorella Opt)',ytitle='dDec (New - Lorella Opt)',xrange=[-4,8]
  tvcircle,mean(grbs.xo_off),0,0,/data
  

  
  readcol,'~/astrometry/butler/ot.txt',tid,grbname,rah,ram,ras,decd,decm,decs,boerr,format='(l,a,a,a,a,a,a,a,a)'
  hms2radec,rah,ram,ras,decd,decm,decs,bora,bodec
  match,grbs.tid,tid,m1,m2
  
  s=sort(grbs[m1].new_xerr)
  bsep1=separation(grbs[m1[s]].new_xra,grbs[m1[s]].new_xdec,grbs[m1[s]].ora,grbs[m1[s]].odec)
  bsep2=separation(grbs[m1[s]].new_xra,grbs[m1[s]].new_xdec,bora[m2[s]],bodec[m2[s]])

  toterr2=grbs[m1].new_xerr+boerr[m2]
  rmserr2=sqrt(grbs[m1].new_xerr^2+boerr[m2]^2)
  offset2=grbs[m1].xo_off-toterr2 ;;real offset outside error circles
  plothist,offset2,bin=0.5,xtitle='X/O offset - '+!tsym.sqrt+'(New Xerr^2 + Oerr^2)',xrange=[-6,4]
  print,'Mean/Median of offset outside in all: '+ntostr(mean(offset2))+' '+ntostr(median(offset2))
  q=where(offset2 gt 0,nq)
  print,'Number of GRBs outside error circles: '+ntostr(nq)+' of '+ntostr(nw)
  nt=nw
  t2=where(offset2 le 0.,nt2)
  print,'Accuracy: '+ntostr(nt2*1./nt)
  
  dra=(grbs[m1].new_xra-bora[m2])*cos(grbs[m1].new_xdec*!dtor)*3600.
  ddec=(grbs[m1].new_xdec-bodec[m2])*3600.
  plot,dra,ddec,psym=2,/iso,xtitle='dRA (New - Butler Opt)',ytitle='dDec (New - Butler Opt)',xrange=[-4,8]
  tvcircle,mean(bsep2),0,0,/data
  
  
  !p.multi=0
  window,1
  if not keyword_set(ps) then window,1 else begplot,name='Judy_fig3'+msuf+'.ps',/land
  n=n_elements(m1)
;  s=indgen(n)
  plot,indgen(n),bsep1,psym=5,/ylog,yrange=[0.04,15],/ystyle,xrange=[0,n+2],/xstyle,$
     ytitle='Position offset from OT (arcsec)',title=title
  oplot,indgen(n),bsep2,psym=1,color=!red
  oplot,grbs[m1[s]].new_xerr
  oplot,grbs[m1[s]].new_xerr+grbs[m1[s]].oerr,line=1
  oplot,grbs[m1[s]].new_xerr+boerr,line=1,color=!red
  oplot,sqrt(grbs[m1[s]].new_xerr^2+grbs[m1[s]].oerr^2),line=2
  oplot,sqrt(grbs[m1[s]].new_xerr^2+boerr[m2[s]]^2),line=2,color=!red
  oplot,grbs[m2[s]].xerr,psym=10,color=!green
  legend,['XRT team err','Judy err','Judy+Lorella optical','Judy+Butler optical',!tsym.sqrt+'(Butler!U2!N+Optical!U2!N)'],line=[0,0,1,1,2],/top,/left,box=0,color=[!green,!p.color,!p.color,!red,!p.color],textcolor=[!green,!p.color,!p.color,!red,!p.color]
  
  w1=where(bsep2 lt grbs[m1].new_xerr,nw1)
  w2=where(bsep2 lt grbs[m1].new_xerr+boerr[m2],nw2)
  w3=where(bsep2 lt sqrt(grbs[m1].new_xerr^2+boerr[m2]^2),nw3)
  print,nw1*1./nw,nw2*1./nw,nw3*1./nw
  


  stop
  return
end 
