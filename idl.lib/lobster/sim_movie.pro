pro sim_movie

  evt=mrdfits('/Users/jracusin/iLobster/simulations/movie/xsim/ilobster/grb-1000s.evt',1)

  for i=140,180,2 do begin
     w=where(evt.time eq i)
     plot,evt[w].x,evt[w].y,psym=3,/iso,xrange=[0,2000],yrange=[0,2000]
     legend,ntostr(evt[w[0]].time),/top,/right
     k=get_kbrd(10)
;     wait,0.5
  endfor 

return
end 
