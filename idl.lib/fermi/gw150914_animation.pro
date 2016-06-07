@lat_gtis
pro gw150914_animation

  trigtime=date2met('2015-09-14-09:50:45.4',/fermi)
  get_ft2,trigtime,ft2

  w=where(ft2.start gt trigtime-100 and ft2.stop lt trigtime+1e4,nw)
  ft2=ft2[w]

  era=ft2.ra_zenith-180
  edec=-ft2.dec_zenith

  m=map('hammer',label_show=0,/current,linestyle=1,$
        thick=2,center_longitude=0,$
        color='grey',position=[0.05,0.05,0.95,0.95]);,/hide)

  for i=0,nw-1 do begin
     skycircle,era[i],edec[i],67.,x,y,n=100.
     p=plot(x,y,symbol='.',/overplot)
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor 



  return
end 
