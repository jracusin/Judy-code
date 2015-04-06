pro irt_simspec

  begplot,name='~/Lobster/Proposal/irt_simspec_090423.eps',/land,/color,font='helvetica'
  add=['z8','z12','z15']

  xrange=[0.5,2.2]
  yrange=[-100,300]
;  yrange=[1d-4,1d2]
;  plot,xrange,yrange,/xsty,/ysty,xrange=xrange,yrange=yrange,/xlog,/ylog,/nodata,xtitle='Wavelength ('+!tsym.mu+'m)',ytitle='F!L'+!tsym.nu+'!U ('+!tsym.mu+'Jy)'
  plot,xrange,yrange,/xsty,/ysty,xrange=xrange,yrange=yrange,/nodata,xtitle='Wavelength ('+!tsym.mu+'m)',ytitle='Net Counts',charsize=2,/xlog,xticks=2,xtickv=[0.5,1.0,2.0],xminor=5;,title='IRT Spectrum of GRB 090423 (z=8.2)'
  
  lya=1215d-10*1d6*(1.+14.)
  color=[!green,!red,!blue]
  for i=1,1 do begin 
     readcol,'lobster_sn_ft1.0_t3000_090423_z14_F2.14.txt',lam,fnu,f,snr,noise,net,/silent

     w=where(lam eq 0.6)
     nw=n_elements(f)

     iback=indgen(w[1])
     i1=indgen(w[2]-w[1])+w[1]
     i2=indgen(w[3]-w[2])+w[2]
     i3=indgen(nw-w[3])+w[3]
     x=where(fnu eq 0)
     fnu[x]=1d-2
     oplot,[lya,lya],[-200,500],line=2
     xyouts,1.5,200,'Ly '+!tsym.alpha,charsize=2
     oplot,lam[i1],net[i1],color=color[i]
  endfor
 
;  legend,['z=8','z=12','z=15'],textcolor=color,box=0,/top,/left,charsize=2
;  oplot,lam[i2],net[i2],color=!blue
;  oplot,lam[i3],net[i3],color=!green

;  legend,[
  endplot

  stop
return
end
