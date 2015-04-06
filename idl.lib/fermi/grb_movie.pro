pro grb_movie

  cd,'~/Fermi/Movies/grbmovie/'
  file='080916C_lc.dat'
  readcol,file,row,time,energy,class,delim='*',format='(l,d,d,i)',skip=3
  time=time-243216761
  w=where(class eq 3)
  class[w]=2

  !x.margin=[4,0]
  !y.margin=[0,0]
  plothist,time,x,y,xtitle='Time (seconds)',ytitle='Number of Photons',bin=0.2,/noplot
  n=n_elements(x)
  for i=0,n-1 do begin
     ii=ntostr(i)
     if i lt 10 then ii='0'+ii
     if i lt 100 then ii='0'+ii
     fname='lc'+ntostr(ii)
     begplot,name=fname+'.ps',/color,font='helvetica'
     polyfill,[-100,500,500,-100],[-100,-100,500,500,-100],color=!black
;     set_plot, 'ps'
;     !p.font=0
;     device, filename=fname+'.ps', /helvetica
;     simpctable
;     !p.thick=5
;     !p.background=!black
     !p.color=!white
     multiplot,[1,2],/init
     plotsym,0,0.7,/fill
     color=[!red,!blue,!green,!orange]
     multiplot

     plot,[0,50],[1,1e4],/nodata,title='GRB 080916C',/ylog,yrange=[5,1e4],/ysty,ytitle='Energy (MeV)';,background=!black,color=!white
     for j=0,2 do begin
        w=where(class eq j)
        oplot,time[w],energy[w],psym=8,color=color[j]
     endfor 
     ;; make that second symbols light up
     plotsym,0,1.,/fill
     for j=0,2 do begin
        w=where(class eq j)
        q=where(abs(time[w]-x[i]) le 0.2,nq)
        if nq gt 0 then oplot,time[w[q]],energy[w[q]],psym=8,color=color[j]
     endfor
     oplot,[x[i],x[i]],[5,1e4],line=1

     multiplot
     plot,x,y,xtitle='Time (seconds)',ytitle='Number of Photons',xrange=[0,50],/xsty;,bin=0.2
     oplot,[x[i],x[i]],[0,150],line=1
;     xyouts,40,-7,'Credit: NASA/Fermi Team',color=!white,/data,charsize=0.7
     multiplot,/reset,/default
;     device,/close
;     set_plot,'x'
     endplot
     spawn,'convert '+fname+'.ps '+fname+'.pdf'

  endfor 

;  spawn,'convert -rotate 270 -delay 20> lc*.ps grbmovie.gif'
  spawn,'convert -delay 102.2 -loop 1 -dispose previous lc*.pdf grbmovie.gif'

  return
end 
