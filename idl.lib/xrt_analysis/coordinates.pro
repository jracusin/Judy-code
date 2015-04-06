pro coordinates,detx,dety,sra,sdec,roll,x1,y1,x2,y2,raw=raw,rahms,decdms,ra,dec,notam=notam,boresight=boresight
  
  if n_params() eq 0 then begin 
     print,'syntax - coordinates,detx,dety,sra,sdec,roll,x1,y1,x2,y2,'
     print,'             rahms,decdms,ra,dec,raw=raw,/notam'
     print,'             if raw then raw=1 for amp1, raw=2 for amp2, '
     print,'                                 and detx=rawx, dety=rawy'
     return
  endif 
  
  if n_elements(raw) ne 0 then begin
     amp=raw
     if amp eq 1 then detx=detx+1 else detx=600-detx
     dety=dety+1
  endif 
  
  if keyword_set(notam) then begin 
     x1=327.64
     y1=237.2
     x2=261.324
     y2=242
  endif 
  
  
  x0=327.64D
  y0=237.2D
  
  if n_elements(boresight) eq 0 then begin 
     B6=298.2d;300D                  ;xrt boresight C
     C6=299.3d;300D                  ;nxrt boresight R
  endif else begin
     B6=boresight[0]
     C6=boresight[1]
  endelse 
  D6=0D ;xrt boresight theta_x(d)
  E6=D6*!dtor ;xrt boresight theta_x(rad)
  B18=9.496 ;TAM Plate Scale
  B8=atan(4D-5/3.5)*!radeg*3600.
  
  B17=X1*1D
  C17=Y1*1D
  D17=X0*1D
  E17=Y0*1D
  K17=X2*1D
  L17=Y2*1D
  B5=detx*1D
  C5=dety*1D
  
  F17=B17-D17 ;dX1
  G17=C17-E17 ;dY1
  
  H17=sqrt(F17^2+G17^2)
  
  if F17 gt 0 and G17 gt 0 then I17=atan(G17/F17) ;theta1
  if F17 lt 0 and G17 gt 0 then I17=!pi-atan(abs(G17/F17))
  if F17 lt 0 and G17 lt 0 then I17=!pi+atan(G17/F17)
  if F17 gt 0 and G17 lt 0 then I17=2*!pi-atan(abs(G17/F17))
  if F17 eq 0 and G17 ge 0 then I17=!pi/2.
  if F17 eq 0 and G17 lt 0 then I17=3*!pi/2.
  
  I18=I17*!radeg
  B19=17D ;Tam ang offset
  J18=I18+B19
  J17=J18*!dtor
  
  F19=H17*(B18/B8)*cos(J17+E6) ;dCt
  G19=-H17*(B18/B8)*sin(J17+D6) ;dRt
  
  B7=B5-B6-F19 ;delta C
  C7=C5-C6-G19 ;delta R
  D7=sqrt(C7^2+B7^2) ;delta
  B15=B17-D17 ;tam deltas
  C15=C17-E17 ;tam deltas
  
  
  P12=sra*1D
  Q12=sdec*1D
  R12=roll*1D
  P13=P12*!dtor ;sra(rad)
  Q13=Q12*!dtor ;sdec(rad)
  R13=R12*!dtor ;roll(rad)
  
  H23=cos(P13)*cos(Q13)
  I23=sin(P13)*cos(Q13)
  J23=sin(Q13)
  
  if B7 gt 0 and C7 ge 0 then F6=atan(C7/B7)
  if B7 lt 0 and C7 ge 0 then F6=!pi-atan(abs(C7/B7))
  if B7 lt 0 and C7 lt 0 then F6=!pi+atan(C7/B7)
  if B7 gt 0 and C7 lt 0 then F6=2*!pi-atan(abs(C7/B7))
  if B7 eq 0 and C7 ge 0 then F6=!pi/2.
  if B7 eq 0 and C7 lt 0 then F6=3*!pi/2.
  
  H5=D7*sin(E6+F6)
  
  I5=-D7*cos(E6+F6)
  J5=H5*B8
  J10=0D ;S/C x-axis boresight correction
  K5=I5*B8
  K10=0D ;S/C x-axis boresight correction
  J11=J5-J10
  K11=K5-K10
  L10=0
  
  L11=sqrt(J11^2+K11^2)
  M10=L10*!dtor
  if J11 gt 0 and K11 ge 0 then N10=atan(K11/J11)
  if J11 lt 0 and K11 ge 0 then N10=!pi-atan(abs(K11/J11))
  if J11 lt 0 and K11 lt 0 then N10=!pi+atan(K11/J11)
  if J11 gt 0 and K11 lt 0 then N10=2*!pi-atan(abs(K11/J11))
  if J11 eq 0 and K11 ge 0 then N10=!pi/2.
  if J11 eq 0 and K11 lt 0 then N10=3*!pi/2.
  
  K18=5.66D
  M17=261.324D
  N17=242D
  O17=K17-M17-(1.01*F17)
  P17=L17-N17-(1.01*G17)
  Q17=sqrt(O17^2+P17^2)
  
  if O17 gt 0 and P17 ge 0 then R17=atan(P17/O17)
  if O17 lt 0 and P17 ge 0 then R17=!pi-atan(abs(P17/O17))
  if O17 lt 0 and P17 lt 0 then R17=!pi+atan(P17/O17)
  if O17 gt 0 and P17 lt 0 then R17=2*!pi-atan(abs(P17/O17))
  if O17 eq 0 and P17 ge 0 then R17=!pi/2.
  if O17 eq 0 and P17 lt 0 then R17=3*!pi/2.

  R18=R17*!radeg
  S18=R18+B19+R12
  S17=S18*!dtor
  O19=-K18*Q17*sin(S17)
  P19=-K18*Q17*cos(S17)
  
  P7=(-L11*cos(M10-R13+N10)+O19)/3600D
  Q7=(-L11*sin(M10-R13+N10)+P19)/3600D
  
  B26=P7*!dtor
  C26=Q7*!dtor
  
  D26=cos(B26)*cos(C26)
  E26=sin(B26)*cos(C26)
  F26=sin(C26)
  
  H26=D26*cos(P13)*cos(Q13)-E26*sin(P13)-F26*cos(P13)*sin(Q13)
  I26=D26*sin(P13)*cos(Q13)+E26*cos(P13)-F26*sin(P13)*sin(Q13)
  J26=D26*sin(Q13)+F26*cos(Q13)
  
  
  if H26^2+I26^2 eq 0 then P23=0 else $
     if I26 gt 0 then P23=acos(H26/sqrt(H26^2+I26^2)) else $
     P23=-acos(H26/sqrt(H26^2+I26^2))
  Q23=asin(J26/sqrt(H26^2+I26^2+J26^2))
  
  ra=(P23*!radeg+360) mod 360
  dec=Q23*!radeg
  
  rah=ra/15D
  irah=fix(rah)
  iram=fix((rah-irah)*60.)
  iras=((rah-irah)*60.-fix((rah-irah)*60.))*60.
  rahms=[irah,iram,iras]
  idec=fix(dec)
  idecm=fix(abs(dec-idec)*60.)
  idecs=(abs(dec-idec)*60.-fix(abs(dec-idec)*60.))*60.
  decdms=[idec,idecm,idecs]
  
  return
end 
