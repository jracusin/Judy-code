pro sample_for_kim

  g=mrdfits('swift_grb_properties.fits',1)
  w=where(strtrim(g.gbmname,2) ne '' and g.z gt 0,nw)
  g=g[w]

  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  match,strtrim(bat.grb,2),strtrim(g.grb,2),m1,m2
  bat=strarr(nw)
  bat[*]='no'
  bat[m2]='yes'
  
  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090531B','GRB090902B','GRB090926A','GRB091003','GRB091208B','GRB100414A','GRB100728A','GRB110625A','GRB110709A','GRB110731A','GRB120624B','GRB120711A','GRB121011A','GRB130305A','GRB130427A','GRB130502B','GRB130504C','GRB130518A','GRB130606B','GRB130702A','GRB131014A','GRB131108A','GRB131231A','GRB140102A','GRB140323A']

  match,strtrim(latgrbs,2),strtrim(g.grb,2),m1,m2
  lat=strarr(nw)
  lat[*]='no'
  lat[m2]='yes'

  list=indgen(nw)
;  list=m2

  readcol,'~/Swift/xrt_grb_positions.txt',grb,ra,dec,err,format='(a,f,f,f)',delim='|
  grb=strtrim(strcompress(grb,/rem),2)
  match,grb,strtrim(g.grb,2),m1,m2
  latang=fltarr(n_elements(g),3)
  for i=2,n_elements(m1)-1 do begin
     print,grb[m1[i]]
     get_lat_angle,ra[m1[i]],dec[m1[i]],g[m2[i]].trigtime,dist
     latang[m2[i],*]=dist
  endfor 

  writecol,'~/stuff_for_people/Kim/sample.dat',g[list].grb,g[list].gbmname,g[list].z,bat[list],lat[list],latang[list,0],latang[list,1],latang[list,2],header='GRB        GBMNAME      z   BATdet LATdet angT0 angT30 angT60'

stop
return
end 
