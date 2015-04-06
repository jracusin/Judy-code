pro xray_jb_compare,ps=ps

  cr=mrdfits(!adata+'closure_relations_total_2sig.fits',1)

  cd,!adata+'chandra'
  grb=file_search('GRB*/')
  n=n_elements(grb)
  ;;; jb 0=no, 1=yes, 2=maybe
  jb=[0,1,2,2,0,0,0,1,1]
  jbtime=dblarr(n) & jblim=jbtime & tstop=jbtime & mjblim=jbtime & jbt=jbtime
  mjb=mrdfits('maybe_jetbreak.fits',1)

;;;  maybe_jet_break,cr,w,last,nsig=2
  w=where((cr.class eq 1234 or cr.class eq 234) and cr.z gt 0 and cr.eiso gt 0)
  u=uniq(cr[w].grb)
  last=0
  for i=0,n_elements(u)-1 do begin
     q=where(cr.grb eq cr[w[u[i]]].grb,nq)
     last=[last,q[nq-1]]
  endfor 
  last=last[1:*]

  for i=0,n-1 do begin
     
     cd,grb[i]
     lcfile='lc_fit_comb.dat'
     if not exist(lcfile) then lcfile='lc_fit_xrt.dat'
     read_lcfit,lcfile,pname,p,perror
     lc=lcout2fits('lc_newout_chandra.txt')

     np=n_elements(p)
     print,np,lcfile,grb[i]

     if jb[i] eq 1 then jbtime[i]=p[np-2]
     if jb[i] eq 2 then mjblim[i]=mjb[i].tlastpos
     if jb[i] eq 0 then jblim[i]=mjb[i].tlastpos
     tstop[i]=max(lc.tstop)
     if mjb[i].tlastpos eq 0 then jblim[i]=tstop[i]
;     jbt[i]=max([jbtime[i],mjblim[i],jblim[i]])

     cd,'..'
  endfor 
  w0=where(jb eq 0)
  w1=where(jb eq 1)
  w2=where(jb eq 2)
  jbt[w0]=jblim[w0]
  jbt[w1]=jbtime[w1]
  jbt[w2]=mjblim[w2]

  xrange=[1d4,1d8]
  if keyword_set(ps) then begplot,name='~/Chandra/xray_energ_compare.ps',/color
  multiplot2,[1,2],/init
  multiplot2
  plotsym,0,1,/fill
  egam=(1.-cos(cr[last].theta*!dtor))*cr[last].eiso
  plot,cr[last].tbreak,cr[last].theta,/xlog,psym=4,ytitle=!tsym.theta+'!Lj!N (deg)',xrange=xrange,yrange=[0,50]
  ;;;need chandra numbers (z,theta,eiso,etc)
  readcol,'~/Chandra/chandra_energetics.csv',grb,jb,z,eiso,format='(a,i,f,d)'

  ctheta=jbtime
  for i=0,n_elements(z)-1 do ctheta[i]=jet_angle(jbt[i]/86400.,z=z[i],eiso=eiso[i]/1d52)
  cegam=(1.-cos(ctheta*!dtor))*eiso;/1d52

  oplot,jbt[w1],ctheta[w1],psym=8,color=!green
  oplot,mjblim[w2],ctheta[w2],psym=8,color=!blue
  oplot,jblim[w0],ctheta[w0],psym=8,color=!red
  plotsym,7,3
  oplot,mjblim[w2],ctheta[w2],psym=8,color=!blue
  oplot,jblim[w0],ctheta[w0],psym=8,color=!red
  plotsym,2,3
  oplot,mjblim[w2],ctheta[w2],psym=8,color=!blue
  oplot,jblim[w0],ctheta[w0],psym=8,color=!red

  plotsym,0,1,/fill
  legend,['Prominent Jet Breaks','Chandra Jet Breaks','Chandra Maybe Jet Break','Chandra No Jet Break'],psym=[4,8,8,8],color=[!p.color,!green,!blue,!red],/top,/left,box=0  

;;;;
  multiplot2
  plotsym,0,1,/fill
  plot,cr[last].tbreak,egam*1d52,yrange=[1d48,1d52],/xlog,psym=4,/ylog,xtitle='t!Lbreak!N (s)',ytitle='E!L'+!tsym.gamma+'!N (erg)',/ysty,xrange=xrange
 
  oplot,jbt[w1],cegam[w1],psym=8,color=!green
  oplot,mjblim[w2],cegam[w2],psym=8,color=!blue
  oplot,jblim[w0],cegam[w0],psym=8,color=!red  
  plotsym,7,3
  oplot,mjblim[w2],cegam[w2],psym=8,color=!blue
  oplot,jblim[w0],cegam[w0],psym=8,color=!red  
  plotsym,2,3
  oplot,mjblim[w2],cegam[w2],psym=8,color=!blue
  oplot,jblim[w0],cegam[w0],psym=8,color=!red  
  xyouts,jbt*0.8,cegam*0.6,'GRB '+grb,charsize=1.

  plotsym,0,1,/fill

  multiplot2,/reset,/default

  if keyword_set(ps) then endplot

  class=intarr(n_elements(cegam))
  class[w0]=1
  class[w1]=2
  class[w2]=3

  writecol,'~/Chandra/energetics.txt',grb,jbt,eiso,ctheta,cegam,class,delim=','

stop
  return
end 
