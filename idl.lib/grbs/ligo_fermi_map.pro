@lat_gtis
pro ligo_fermi_map,ligofile,trigtime,ft2=ft2

  simpctable
;  ligo2radec,ligofile,ra,dec
  if n_elements(ft2) eq 0 then get_ft2,trigtime,ft2
  
  m=min(abs(ft2.stop-trigtime),wm)

  print,m

;  map_set,0,0,/mollweide,grid=30
  plotsym,0,0.5,/fill
;  plots,ra,dec,color=!red,psym=8
  mollview,ligofile,colt=-3,window=-1,png='ligo_map.png',title=' ',/nobar
  im=image('ligo_map.png',margin=0,image_dimensions=[800,510])
  m=map('Mollweide',label_show=0,grid_longitude=30,grid_latitude=30,linestyle=1,margin=[0.008,0,0.008,0],/current,center_longitude=0)
  ;; LAT FoV
  skycircle,ft2[wm].ra_scz,ft2[wm].dec_scz,65.,x,y
 ; oplot,x,y,color=!magenta,thick=3
  p=polygon(x,y,color='magenta',thick=3,/data,target=m)
;  plots,ft2[wm].ra_scz,ft2[wm].dec_scz,color=!magenta,psym=1,symsize=3

;  ;;GBM FoV
;  skycircle,ft2[wm].ra_scz,ft2[wm].dec_scz,97.,x,y
;  oplot,x,y,color=!green,thick=3
;  plots,ft2[wm].ra_scz,ft2[wm].dec_scz,color=!green,psym=1,symsize=3

  ;; Earth
  era=ft2[wm].ra_zenith-180.
  if era lt 0. then era=era+360.
  edec=-ft2[wm].dec_zenith
;  if edec lt -90 then edec=edec+180.
  skycircle,era,edec,66,x,y
  p=polygon(x,y,color='blue',thick=3,/data,target=m,/fill_background,fill_color='blue',/current,fill_transparency=50)
;  oplot,x,y,color=!blue,thick=3
 ; plots,era,edec,color=!blue,psym=1,symsize=3

  print,era,edec
  print,ft2[wm].ra_scz,ft2[wm].dec_scz

  stop
end
