@lat_gtis
function vector,ra,dec
  ra1=ra*!dtor
  dec1=dec*!dtor
  v=[cos(ra1)*cos(dec1),$
     sin(ra1)*cos(dec1),$
     sin(dec1)]
return,v
end 

function getrotationmatrix,self,axis,theta

  daxis=dot(axis,axis)
  axis = axis/daxis[0]
  a = cos(theta/2)
  bcd = -axis*sin(theta/2)
  b=bcd[0]
  c=bcd[1]
  d=bcd[2]
  arr=([[a*a+b*b-c*c-d*d, 2.*(b*c+a*d),2.*(b*d-a*c)],$
        [2.*(b*c-a*d), a*a+c*c-b*b-d*d, 2.*(c*d+a*b)],$
        [2.*(b*d+a*c), 2.*(c*d-a*b), a*a+d*d-b*b-c*c]])
  return,arr
end 

function rotates,self,angle,axis
  ang=angle*!dtor
  matrix=getrotationmatrix(self,axis,ang)
  arr=dot(matrix,self)

  return,arr
end 

function getradec,ra_scx,dec_scx,ra_scz,dec_scz,theta,phi

  vx=vector(ra_scx,dec_scx)
  vz=vector(ra_scz,dec_scz)

  vxx=rotates(vx,phi,vz)
  vy=crossp(vz,vxx)
  
  vzz=rotates(vz,theta,vy)

  ra=atan(vzz[1],vzz[0])*!radeg
  dec=asin(vzz[2])*!radeg

  if ra lt 0 then ra=ra+360.

  radec=[ra,dec]

  return,radec
end 


function getdetectorangles,ra_scx,dec_scx,ra_scz,dec_scz,sourceRa,sourceDec,detector,det,dist=dist

  w=where(detector eq strtrim(det.det,2))
  t=det[w].zenith
  p=det[w].azimuth

  radec=getradec(ra_scx,dec_scx,ra_scz,dec_scz,t,p)

  dist=separation(sourceRa,sourceDec,radec[0],radec[1])/3600.

return,radec
end 

pro gbm_angles,sourcera,sourcedec,trigtime,det

  det=create_struct('det','','zenith',0.,'azimuth',0.,'angles',0.)
  det=replicate(det,14)

  det.det=['n'+ntostr(indgen(10)),'na','nb','b0','b1']
  det.zenith=[20.58,45.31,90.21,45.24,90.27,89.79,20.43,46.18,89.97,45.55,90.42,90.32,90.0,90.0]
  det.azimuth=[45.89,45.11,58.44,314.87,303.15,3.35,224.93,224.62,236.61,135.19,123.73,183.74,0,180]

  get_ft2,trigtime,ft2
  if n_elements(ft2) gt 1 then begin 
     w=where(ft2.start ge trigtime) ; and ft2.time lt trigtime+1)
     w=w[0]

  ;;; need to get ra_scx,dec_scx,ra_scz,dec_scz,sourceRa,sourceDec
  ;;; from somewhere
  ;;; input source ra,dec, time and grab from FT2?
     dang=fltarr(14)
     for i=0,13 do begin
        dang=getdetectorangles(ft2[w].ra_scx,ft2[w].dec_scx,ft2[w].ra_scz,ft2[w].dec_scz,sourceRa,sourceDec,det[i].det,det,dist=dist)
        det[i].angles=dist
;     print,dist
     endfor 
  endif
;  print,dang

  return
end 
