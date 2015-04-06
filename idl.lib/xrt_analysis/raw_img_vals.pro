pro raw_img_vals,im
  
  rdis,im
  x=[90,120]
  y=[20,50]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[1]+10,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2
  
  x=[190,220]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[1]+10,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2

  x=[410,440];[285,315]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[1]+10,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2

  
  x=[510,540]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[1]+10,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2
  
  
  x=[90,120]
  y=[550,580]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[0]-30,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2

  
  x=[170,200]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[0]-30,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2

  x=[410,440]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[0]-30,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2
  
  x=[510,540]
  oplot,x,y
  oplot,x,[y[1],y[0]]
  xyouts,x[0],y[0]-30,ntostr(round(mean(im[x[0]:x[1],y[0]:y[1]])),3),charsize=2

  
  
end 
