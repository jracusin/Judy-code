pro ilobster_paper_plots

  ;;; SHORT BURST LC PLOT
  
  dir='~/iLobster/Paper/Taka/'
  begplot,name=dir+'opt_sgrb_plot.ps',/land,/color,font='helvetica'
  !x.margin=[5,1]
  !y.margin=[2,0]

  yrange=[25,12]
  plot,[1e2,1e6],yrange,/xlog,xrange=[10,1e6],yrange=yrange,/ysty,/xsty,xtitle='Time (s)',ytitle='R [AB] (mag)',/nodata,title='Optical afterglows of sGRBs scaled to 400 Mpc',charsize=2

;  readcol,dir+'swift_shortgrb_optlc_400mpc.qdp',time,flux,fluxerr,format='(a,f,f)'
  readcol,dir+'swift_shortgrb_optlc_400mpc_with_unknownz.qdp',time,flux,fluxerr,format='(a,f,f)'
  w=where(strtrim(time,2) eq 'NO',nw)
  time=double(time)

  j=0
  dist7=lumdist(0.7,h0=71,lambda=0.73,omega_m=0.27)
  dist5=lumdist(0.5,h0=71,lambda=0.73,omega_m=0.27)
  f=fltarr(n_elements(time))
  f[0:w[9]]=1.
  f[w[10]:*]=dist7^2/dist5^2
  tf=fltarr(n_elements(time))
  tf[0:w[9]]=1.
  tf[w[10]:*]=(1.+0.7)/(1.+0.5)
  zpt=-21.61
  flux2=10.^((flux-zpt)/(-2.5))*f
  mag2=alog10(flux2)*(-2.5)+zpt
  color=!p.color
  for i=0,nw-1 do begin
     if i ge 10 then color=!grey70 else color=!p.color
     plotsym,0,1,/fill,color=color
     oplot,time[j:w[i]-1]*tf,mag2[j:w[i]-1],color=color;,psym=1
     oplot,time[j:w[i]-1]*tf,mag2[j:w[i]-1],color=color,psym=8
     oploterror,time[j:w[i]-1]*tf,mag2[j:w[i]-1],fluxerr[j:w[i]-1],psym=3,/nohat,errcolor=color,color=color
;     print,time[j:w[i]-1]
;     print,'----'
     j=w[i]+1
  endfor   

  c=!blue
 
  oplot,[10.*60,10.*60],[25,12],thick=20,color=c
  xyouts,10.*60+100.,12.9,'Follow-up GW Trigger Position',color=c,charsize=1.8
  arrow,10.*60,13.2,2000.,13.2,/data,thick=20,hsize=!D.X_SIZE / 40.,/solid,color=c

  !p.charsize=1.8
  c=!red
  oplot,[1200,1e6],[22,22],thick=20,color=c,line=5
;  polyfill,[4e4,7e5,7e5,4e4,4e4],[6e-10,6e-10,2.5e-10,2.5e-10,6e-10],color=!white
  xyouts,1.3e3,21.7,'1200 s Sensitivity',color=c

;  legend,['Known Redshift','Assumed Median
;  z=0.7'],/top,/right,box=0,textcolor=[!p.color,!grey70]
  xyouts,3.2e4,14,'Known Redshift'
  xyouts,8e3,14.8,'Assumed Median z=0.7',color=!grey70

  endplot
  spawn,'convert '+dir+'opt_sgrb_plot.ps '+dir+'opt_sgrb_plot.pdf'

stop
return
end 
