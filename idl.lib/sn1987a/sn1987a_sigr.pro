pro sn1987a_sigr
  
;  cd,'/Volumes/Firewire1/racusin/sn1987a/image_analysis/'
  cd,'/Volumes/Firewire1/racusin/sn1987a/'
  
;  imfiles='image_analysis/sn1987a'+ntostr(indgen(19)+1)+'_300-8000_cr.fits'
;  for i=0,18 do begin
;     im=mrdfits(imfiles[i])
;     p=[0.001,70.,10.,0.001,120.,10.] 
;     x=im[*,100]
  
  
;  stop
  dir=['image_analysis/','simulations/0deg_torus/','simulations/45deg_torus/','simulations/45deg_torus_4spot/']
  add=['_free','_free','_free','_free']  
  title=['real','0deg_torus','45deg_torus','45deg_torus_4spot']
  odir=[dir[0],'simulations/','simulations/','simulations/']
  outname=odir+'sn1987a_sigr_'+title+'.ps'
  for i=0,3 do begin
     print,outname[i]
     begplot,name=outname[i],/land
     cd,dir[i]
     ring=mrdfits('ring'+add[i]+'/new_sn1987a_fpars.fits',1)  
     ptsrc=mrdfits('ptsrc'+add[i]+'/new_sn1987a_fpars.fits',1)
     bilat=mrdfits('bilat'+add[i]+'/new_sn1987a_fpars.fits',1)  
     lobes=mrdfits('lobes'+add[i]+'/new_sn1987a_fpars.fits',1)
     
     xrange=[8,18]
     yrange=[0,5]
     bin=0.4
;     erase
     multiplot2,[1,4],/init
     multiplot2 & plothist,ring.sigr,xrange=xrange,/xsty,bin=bin,yrange=yrange,/ysty,title=title[i]
     legend,'ring',/top,/left,box=0
     oplot,[mean(ring.sigr),mean(ring.sigr)],[0,5],line=2
     multiplot2 & plothist,ptsrc.sigr,xrange=xrange,/xsty,bin=bin,yrange=yrange,/ysty
     legend,'ptsrc',/top,/left,box=0
     oplot,[mean(ptsrc.sigr),mean(ptsrc.sigr)],[0,5],line=2
     multiplot2 & plothist,bilat.sigr,xrange=xrange,/xsty,bin=bin,ytitle='N',yrange=yrange,/ysty
     legend,'bilat',/top,/left,box=0
     oplot,[mean(bilat.sigr),mean(bilat.sigr)],[0,5],line=2
     multiplot2 & plothist,lobes.sigr,xrange=xrange,/xsty,bin=bin,xtitle='Sig r',yrange=yrange,/ysty
     legend,'lobes',/top,/left,box=0
     oplot,[mean(lobes.sigr),mean(lobes.sigr)],[0,5],line=2
     multiplot2,/reset,/default
     
     colprint,ring.sigr,ptsrc.sigr,bilat.sigr,lobes.sigr
     print
     print,median(ring.sigr),median(ptsrc.sigr),median(bilat.sigr),median(lobes.sigr)
     cd,'/Volumes/Firewire1/racusin/sn1987a/'
;     k=get_kbrd(10)
     endplot
  endfor
  
  return
end 
