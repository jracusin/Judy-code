pro wfi_sens_compare

  xrange=[2010,2022]
  p=plot(xrange,[8e-12,1e-9],/nodata,xtitle='Year',/ylog,/xsty,/ysty,font_size=14,yrange=[4e-11,1e-9],title='Full Sky Sensitivity in 1 day',position=[0.15,0.4,0.95,0.9],xrange=xrange,ytitle='0.3-5.0 keV Sensitivity (erg cm!U-2!Ns!U-1!N)')

  readcol,'~/iLobster/2014_proposal/lobster_sensitivity.dat',time, bcount, mcount, crabflux, grbflux, hgrbflux
  lobtime=time
  lobflux=grbflux
  f300=interpol(lobflux,lobtime,300.)

;  t=text(0.05,0.5,'0.3-5.0 keV Sensitivity',font_size=14,orient=90)
;  t=text(0.09,0.6,'(erg cm!U-2!Ns!U-1!N)',font_size=14,orient=90)

  ;;;lob,bat,maxi,asm
  ltime=[2019.4,2021.4,2022]
;  lflux=1.24e-11
  lflux=f300

  mtime=[2009.75,2016]
  mflux=3.2e-10

  btime=[2004.9,2014]
  bflux=5e-10

  atime=[1996,2012]
  aflux=4e-10

;  oplot,ltime,[lflux,lflux]
;  oplot,mtime,[mflux,mflux]
  p2=plot([xrange[0],atime[1]],[aflux,aflux],thick=3,/overplot)
  t2=text(atime[1]+0.2,aflux*0.9,'RXTE/ASM',font_size=12,/data)

  a3=arrow([ltime[0],ltime[1]],[lflux,lflux],/data,thick=3)
;  p3=plot([ltime[1],ltime[2]],[lflux,lflux],line=1,thick=2,/overplot)
  t3=text(ltime[0]-2,lflux*1.2,'ISS-Lobster Lite WFI',font_size=14,/data,font_style='bold')

  a4=arrow([xrange[0],btime[1]],[bflux,bflux],/data,thick=2)
  t4=text(btime[0]+6.0,bflux*1.1,'Swift/BAT',font_size=12,/data)

  a5=arrow([xrange[0],mtime[1]],[mflux,mflux],/data,thick=2)
  t5=text(mtime[0]+1.,mflux*0.7,'MAXI',font_size=12,/data)


  p.save,'~/iLobster/LobLite/compare_lob.png'
  p.close

stop
  return 
end
