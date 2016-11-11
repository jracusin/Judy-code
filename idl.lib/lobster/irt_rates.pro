pro kilonovae

  ;;; using Jeff's sims for 0.5m diameter, 1" pix

  d=[10.,50,100,200]

  t=[0.15,0.2,0.25,0.3,0.35,0.4,0.5,0.75,1,2,3,4,5] ;; days
  ri=[[0.6,0,0,0],[2.6,0.1,0,0],[0.2,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
  iz=[[37.4,1.3,0.3,0],[40.7,1.5,0.3,0.1],[9.0,0.3,0.1,0],[2.3,0.1,0,0],[0.5,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0.1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
  jh=[[651.9,43.8,11.3,2.8],[657.9,44.1,11.3,2.8],[291.4,14.8,3.7,0.9],[89.4,3.8,1.0,0.2],[30.5,1.2,0.3,0.1],[1.8,0.1,0,0],[5.8,0.2,0.1,0],[18.9,0.8,0.2,0.0],[83.4,3.6,0.9,0.2],[302.1,15.1,3.7,0.9],[403.4,21.7,5.3,1.2],[376.1,19.7,4.8,1.1],[339.5,17.3,4.2,1.0]]

  begplot,name='kilonovae_sn.ps',/color
  multiplot,[1,4],/init
  multiplot
  plot,[0,6],[0,1],/nodata,ytitle='S/N',yrange=[0,700],/ysty,charsize=2
  plotsym,0,1,/fill
  oplot,t,ri[0,*],psym=8,color=!green
  oplot,t,iz[0,*],psym=8,color=!red
  oplot,t,jh[0,*],psym=8,color=!blue
  oplot,[0,6],[5,5],line=2
  legend,['D='+ntostr(fix(d[0]))+' Mpc','RI','IZ','JH'],textcolor=[!p.color,!green,!red,!blue],box=0,/top,/right,charsize=2

  multiplot
  plot,[0,6],[0,700],/nodata,ytitle='S/N',yrange=[0,60],/ysty,charsize=2
  oplot,t,ri[1,*],psym=8,color=!green
  oplot,t,iz[1,*],psym=8,color=!red
  oplot,t,jh[1,*],psym=8,color=!blue
  oplot,[0,6],[5,5],line=2
  legend,['D='+ntostr(fix(d[1]))+' Mpc'],box=0,/top,/right,charsize=2

  multiplot
  plot,[0,6],[0,700],/nodata,ytitle='S/N',yrange=[0,20],/ysty,charsize=2
  oplot,t,ri[2,*],psym=8,color=!green
  oplot,t,iz[2,*],psym=8,color=!red
  oplot,t,jh[2,*],psym=8,color=!blue
  oplot,[0,6],[5,5],line=2
  legend,['D='+ntostr(fix(d[2]))+' Mpc'],box=0,/top,/right,charsize=2

  multiplot
  plot,[0,6],[0,700],/nodata,xtitle='Time Since Merger (days)',ytitle='S/N',yrange=[0,10],/ysty,charsize=2
  oplot,t,ri[3,*],psym=8,color=!green
  oplot,t,iz[3,*],psym=8,color=!red
  oplot,t,jh[3,*],psym=8,color=!blue
  oplot,[0,6],[5,5],line=2
  legend,['D='+ntostr(fix(d[3]))+' Mpc'],box=0,/top,/right,charsize=2

  multiplot,/reset,/default
  endplot
  ps2pdf,'kilonovae_sn.ps'

  volrate=10/1e3^3 ;; Mpc-3 yr-1
  gwhorizon=d
  gwallskyrate=volrate*gwhorizon^3*4./3.*!pi
  beamangle=16.
  allsky=4.*!pi*(180d/!pi)^2.
  beamfrac=2.*!pi*beamangle^2./allsky
  rate=gwallskyrate/beamfrac
  print,rate
  
stop
  return
end 

pro grb_spectra
  
  readcol,'jeff_grb_simple.txt',z,f1,f2,f3,f4,f5,f6
  t=[810,1450,3000,6000]
  lam0=[0.6,0.9,1.2,1.5,1.8,2.1]
  lam1=[0.9,1.2,1.5,1.8,2.1,2.4]
  lam=(lam0+lam1)/2.
  lamerr=lam1-lam

  begplot,name='irt_grb_zspec.ps',/color
  
  yr=[300,220,150,100]
  multiplot,[1,4],/init
  for j=0,3 do begin 
     color=[!darkred,!red,!salmon,!orange,!yellow,!lightgreen,!green,!forestgreen,!blue,!dodgerblue,!cyan,!purple,!magenta,!violet]
     multiplot
     if j eq 3 then xtitle='Wavelength ('+!tsym.mu+'m)' else xtitle=''
     plot,[0.5,3],[0,300],/nodata,xtitle=xtitle,ytitle='S/N',charsize=2,xrange=[0,3],/xsty,yrange=[0,yr[j]],/ysty

     for ii=0,13 do begin
        i=ii+j*14.
        f=[f1[i],f2[i],f3[i],f4[i],f5[i],f6[i]]
        oplot,lam,f,color=color[ii]
     endfor 
     
     oplot,[0,3.0],[3,3],line=2
     if j eq 0 then legend,'z = '+ntostr(round(z[0:13])),textcolor=color,box=0,/top,/right,charsize=2
     legend,['T0+'+ntostr(t[j])+' s'],box=0,/top,/left
  endfor 
  multiplot,/reset,/default
  endplot
  ps2pdf,'irt_grb_zspec.ps'

stop
  return
end 

pro irt_rates

return
end 
