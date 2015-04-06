pro coord_offset,ra,dec,raoff,decoff,newra,newdec
  
  if n_params() lt 4 then begin
     print,'Syntax - coord_offset,ra,dec,raoff,decoff,newra,newdec'
     print,'              (all in descimal degrees)'
     return
  end
  
;  if dec lt 0 then b=-90.-dec else b=90.-dec
;  rar=ra*!dtor
;  decr=dec*!dtor
;  raoffr=raoff*!dtor
;  decoffr=decoff*!dtor
  
;   A=acos((sin(decr)*(1.-cos(raoffr)))/(cos(decr)*sin(raoffr)))
;   arclenr=acos(cos(decoffr)*cos(raoffr)+sin(decoffr)*sin(raoffr)*cos(A))
;   arclen=arclenr*180./!pi
;   azir=asin(sin(A)*sin(decoffr)/sin(arclenr))
;   azi=azir*180./!pi
;   print,A,arclen,azi
  
  vect,dec,ra,v
  vn1=rotvec(3,ra,v)
  vn2=rotvec(2,-1.*dec,vn1)
  vn3=rotvec(2,decoff,vn2)
  vn4=rotvec(3,-1.*raoff,vn3)
  vn5=rotvec(2,dec,vn4)
  vn6=rotvec(3,-1.*ra,vn5)
  vnew=vn6
;  vn2=x[j]*VOLD(1)+Y(J)*VOLD(2)+Z(J)*VOLD(3)
  elaz2,vnew,newdec,newra
  
  
  return
end 
