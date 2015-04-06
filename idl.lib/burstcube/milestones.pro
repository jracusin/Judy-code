pro milestones

  p=plot([2016,2020],[0,1],/nodata,ytickname=' ',margin=[0.15,0.1,0.1,0.1],xtitle='FY',xminor=11,xrange=[2016,2020],yrange=[0,1])
  xaxis=axis(0,location='top',minor=11)
  
  t=text(2016.3,0.9,'Design',/overplot,/data,font_style='it',font_size=10)
  p=plot([2017,2017],[0,1],line='--',/overplot)
  t=text(2017.2,0.9,'Construction',/overplot,/data,font_style='it',font_size=10)
  p=plot([2018,2018],[0,1],line='--',/overplot)
  t=text(2018.0,0.9,'Integration',/overplot,/data,font_style='it',font_size=10)
  p=plot([2018.5,2018.5],[0,1],line='--',/overplot)
  t=text(2018.6,0.9,'Testing',/overplot,/data,font_style='it',font_size=10)
  p=plot([2019,2019],[0,1],line='--',/overplot)
  t=text(2019.2,0.9,'Operations',/overplot,/data,font_style='it',font_size=10)

  t=text(0.02,0.7,'Instrument',/overplot,color='blue')
  t=text(0.05,0.66,'Reviews',/overplot,color='blue',font_size=10)
  
  t=text(0.02,0.38,'Spacecraft',/overplot,color='red')
  t=text(0.05,0.34,'Reviews',/overplot,color='red',font_size=10)

  t=text(2016.4,0.7,'PDR',/overplot,/data,color='blue',font_size=8)
  t=text(2016.4,0.3,'PDR',/overplot,/data,color='red',font_size=8) ;; preliminary design review

  t=text(2016.9,0.7,'CDR',/overplot,/data,color='blue',font_size=8)
  t=text(2016.9,0.3,'CDR',/overplot,/data,color='red',font_size=8) ;; critical design review

  t=text(2017.9,0.7,'ISR',/overplot,/data,color='blue',font_size=8) ;; instrument ship review
  t=text(2017.9,0.3,'SIR',/overplot,/data,color='red',font_size=8) ;; spacecraft integration review

  t=text(2018.4,0.5,'I&T',/overplot,/data,color='purple',font_size=8) ;; integration and testing

  t=text(2018.8,0.5,'FRR',/overplot,/data,color='purple',font_size=8) ;; flight readiness review

  t=text(2019,0.5,'Launch',/overplot,/data,color='purple',font_size=8) ;; launch

  t=text(2019.5,0.5,'Operations',/overplot,/data,color='purple',font_size=8) ;; operations
;; post launch assessment review
;; Interface Control Documents

  p.save,'~/Burstcube/milestones.png'
  p.close

  return
end 
  
