@ipncircle
pro loc_plot

  im=image('~/BurstCube/ligo_map_2019.png',margin=0.0,image_dimensions=[1000,520])
  m=map('Mollweide',label_show=0,/overplot,/current,position=[0.112,0.19,0.955,0.78],grid_longitude=360,grid_latitude=180)
  bigcirc,330,-30,13.,x,y
  p=plot(x,y,thick=5,position=[0.112,0.19,0.955,0.78],/overplot,/current,color='magenta')
  t=text(0.4,0.27,'BurstCube',font_style='bold italic',color='magenta',font_size=15)

  im.save,'~/BurstCube/LIGO_bc_loc_plot.png'
  im.close

  return
end 

pro gw_vol_plot

  yr=[2015,2016,2017,2019];,2022.]
  yrerr=[0.25,0.5,0.75,1.]
  ligo=[60,100,145,200];,200.]
  ligoerr=[20,20,25,0];,0.]
  virgo=[0,40,72.5,95.5,130.]
  virgoerr=[0,20,12.5,32.5,0]

  ligo=ligo+ligoerr
  virgo=virgo+virgoerr

;  lvol=!pi*(ligo)^3.
;  vvol=!pi*(virgo)^3.

  p=plot([2015,2020],[0,1],xtitle='Year',ytitle='Detection Range (Mpc)',yrange=[0,350],xrange=[2015,2020],/nodata,margin=[0.12,0.1,0.13,0.1],xminor=11,font_size=14.,position=[0.13,0.1,0.92,0.5],ytickvalue=[0,100,200,300]);,axis_style=1
  for i=0,3 do begin
;     x=yr[i]+[0,1,1,0,0]
;     y=ligo[i[0]]+[-ligoerr[i],-ligoerr[i],ligoerr[i],ligoerr[i],-ligoerr[i]]
;     y=!pi*y^3.
     ;poly=polygon(x,y,/data,fill_color='dark blue')
     p2=plot([yr[i],yr[i]+yrerr[i]],[ligo[i],ligo[i]],color='dark blue',/overplot,/current,thick=5)
     if i eq 3 then t=text(2019.3,ligo[i]+10.,'LIGO',/overplot,/current,color='dark blue',/data,font_size=14.)
;     y=virgo[i[0]]+[-virgoerr[i],-virgoerr[i],virgoerr[i],virgoerr[i],-virgoerr[i]]
;     y=!pi*y^3.
     ;poly=polygon(x,y,/data,fill_color='green')
     p3=plot([yr[i],yr[i]+yrerr[i]],[virgo[i],virgo[i]],color='green',/overplot,/current,thick=5,linestyle='--')
     if i eq 3 then t=text(2019.28,virgo[i]+10.,'Virgo',/overplot,/current,color='green',/data,font_size=14.)
     p4=plot([yr[i],yr[i]+yrerr[i]],[ligo[i],ligo[i]]*1.5,color='red',/overplot,/current,thick=5)

  endfor 

  x=yr[3]+[0,1,1,0,0]
;  y=em+[-emerr,-emerr,emerr,emerr,-emerr]
;  y=!pi*y^3
;  poly=polygon(x,y,/data,fill_color='red')
  t=text(2018.4,ligo[3]*1.5+10.,'LIGO+All Sky EM',/overplot,/current,color='red',/data,font_size=14.)

;  p4=plot([yr[3],yr[3]+1.],[virgo[3],virgo[3]]*1.5,color='red',/overplot,/current,thick=5,linestyle='--')
;  t=text(2018.4,virgo[3]*1.5-20.,'Virgo+All Sky EM',/overplot,/current,color='red',/data,font_size=14.)

;  n=[0.01,0.1,0.2,0.4,0.6,1.,1.5,2.]
;;   n=[0.01,0.1,1,10.]
;;   nn=n_elements(n)
;;   for i=0,nn-1 do begin
;;      for j=1,9 do begin 
;;         v=(n[i]*j/10.*3./(!pi*4.))^(1./3.)*1e3/1.5
;; ;        print,n[i]*j,v
;;         p5=plot([2019.9,2020],[v,v],/current,/overplot)
;;      endfor 
;;   endfor 
;;   nd=[2,replicate(1,nn-1)]
;;   ns=strarr(nn)
;;   for i=0,nn-1 do ns[i]=numdec(n[i],nd[i])
;;   v=round((n/10.*3./(!pi*4.))^(1./3.)*1e3)/1.5
;  a=axis(1,location='right',tickvalues=v,title='GW/sGRB Rate (yr!U-1!N)',textpos=1,tickdir=1,clip=0,/data,tickname=ns,minor=0)
;  a=axis(0,location='top',minor=11)
  
;  pp=plot([0,1],[0,1],/current,position=[0.22,0.52,0.62,0.85],/nodata,xrange=[2019,2020],yrange=[0.01,3],xtickvalues=[2019,2020],xminor=11,ytickvalues=[1,2,3],ytitle='GW/sGRB Rate (yr!U-1!N)',xtitle='Year',font_size=14.)

  p=plot([2015,2020],[0,1],ytitle='GW/sGRB Rate (yr!U-1!N)',yrange=[0.0,5],xrange=[2015,2020],/nodata,margin=[0.12,0.1,0.13,0.1],xminor=11,font_size=14.,position=[0.13,0.5,0.92,0.92],/current,xtickname=strarr(6));,ytickname=['0.01','0.1','1','10']);,axis_style=1
  
  nligo=4./3.*!pi*(ligo*1e-3)^3*10.*1.5^6
  nvirgo=4./3.*!pi*(virgo*1e-3)^3*10.*1.5^6

  gbm=0.59;^(1./3.)
  bc=0.57;^(1./3.)
  bat=0.26;^(1./3.)
  comb=0.9;(1.3*0.5);^(1./3.)
  off=[0,0];[0.05,-0.05]

  for i=0,3 do begin 
;     p2=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]],color='dark blue',/overplot,/current,thick=5)
;     p3=plot([yr[i],yr[i]+yrerr[i]]+off,[nvirgo[i],nvirgo[i]],color='green',/overplot,/current,thick=5,linestyle='--')
     p4=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]]*gbm,color='purple',/overplot,/current,thick=5)
     p5=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]]*bc,color='royal blue',/overplot,/current,thick=5)
     p6=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]]*bat,color='violet',/overplot,/current,thick=5)
     p7=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]]*comb,color='dark orange',/overplot,/current,thick=5)
     p8=plot([yr[i],yr[i]+yrerr[i]]+off,[nligo[i],nligo[i]],color='red',/overplot,/current,thick=5)

     if i eq 3 then begin
        t=text(0.74,0.70,'LIGO+GBM',/overplot,/current,color='purple',font_size=14.)
        t=text(0.67,0.64,'LIGO+',/overplot,/current,color='royal blue',font_size=14.)
        t=text(0.76,0.64,'BurstCube',/overplot,/current,font_style='bold italic',font_size=14.,color='royal blue')
        t=text(0.76,0.54,'LIGO+BAT',/overplot,/current,color='violet',font_size=14.)
        t=text(0.5,0.75,'LIGO+BAT+GBM+',/overplot,/current,color='dark orange',font_size=14.)
        t=text(0.76,0.75,'BurstCube',/overplot,/current,font_style='bold italic',font_size=14.,color='dark orange')
        t=text(0.67,0.83,'LIGO+All Sky EM',/overplot,/current,color='red',font_size=14.)

     endif 

  endfor 


  p.save,'~/BurstCube/gw_horizon_plot.png'
  p.close
;  oplot,yr,lvol,psym=2 ;;; LIGO
;  oplot,yr,vvol,psym=4,color=!red ;;; Virgo

  stop

  return
end 
