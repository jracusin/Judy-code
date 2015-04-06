pro predict_slew,ra1,dec1,ra2,dec2,era,edec,sra,sdec,mra,mdec,pra,pdec,tslew,myplot=myplot,usephi=usephi,noplot=noplot,dostop=dostop
  
  econ=67.+28.
  scon=45.
  mcon=21.
  
  vect,dec1,ra1,p0
  vect,dec2,ra2,p1
  vect,edec,era,vearth         
  vect,sdec,sra,vsun
  vect,mdec,mra,vmoon

  b=(p1+p0)/sqrt(total((p1+p0)^2))
  m=(p1-p0)/sqrt(total((p1-p0)^2))
  x=crossp(p1,p0)/sqrt(total((crossp(p1,p0))^2))
  
  alpha=sqrt((1.+dot(p0,p1))/2.)
  phis=dblarr(3,2)
  phi_l=dblarr(3) & phi_u=dblarr(3)
  
  for k=0,2 do begin            ;loop through constraints
     case k of
        0: begin
           ve=vearth
           theta_e=econ*!dtor
        end
        1: begin
           ve=vmoon
           theta_e=mcon*!dtor
        end 
        2: begin
           ve=vsun
           theta_e=scon*!dtor
        end 
     endcase 
     
     u=0.5*(dot(ve,x)^2-(alpha^2-2*alpha*dot(ve,b)*cos(theta_e)+dot(ve,b)^2))
     v=dot(ve,x)*(dot(ve,b)-alpha*cos(theta_e))
     w=sin(theta_e)^2-0.5*(dot(ve,x)^2+(alpha^2-2*alpha*dot(ve,b)*cos(theta_e)+dot(ve,b)^2))
     
     tcs=w/sqrt(u^2+v^2)
;     if tcs gt 1 then tcs=1-tcs
;     print,tcs
;     stop
;     while tcs gt 1 do tcs=1./tcs
;stop
     
     phi1=0.5*(atan(v,u)+acos(tcs))
     phi2=0.5*(atan(v,u)-acos(tcs))
;print,phi1,phi2 
     phis[k,0]=phi1
     phis[k,1]=phi2

;  phi=min(abs([phi1,phi2] mod (2*!pi)))
  
     for j=0,1 do begin ;loop through 2 solutions
        phi=phis[k,j]
     
        jump:
        
        if phi lt 0 then begin
           while phi lt 0 do phi=phi+2*!pi
           goto,jump
        endif else begin
           a=x*cos(phi)+b*sin(phi)           
           
           theta_c=atan(dot(crossp(ve,p0),a),(dot(crossp(p0,a),crossp(ve,a))))
           if theta_c lt 0 then while theta_c lt 0 do theta_c=theta_c+2*!pi 
        
           theta_f=atan(dot(crossp(p1,p0),a),(dot(crossp(p0,a),crossp(p1,a))))
           if theta_f lt 0 then while theta_f lt 0 do theta_f=theta_f+2*!pi 
           if theta_f lt theta_c then begin
              phi=phi-!pi
              goto,jump
           endif else begin
              dadphi=-x*sin(phi)+b*cos(phi)
              c0=(1-cos(theta_c))
              c1=dot(p0,dadphi)
              c2=dot(p0,a)
              c3=sin(theta_c)
              c4=crossp(dadphi,p0)
              dpdphi=dblarr(3)
              dpdphi=c0[0]*(c1[0]*a+c2[0]*dadphi)-c3[0]*c4
;              dpdphi=c0[0]*(c1[0]*a+c2[0]*dadphi)-c3[0]*c4
              res=dot(dpdphi,ve)
              if res lt 0 then phi_l[k]=phi else phi_u[k]=phi
           endelse 
        endelse
;        print,k,res,phi*!radeg
     endfor 
  endfor 

  ;which phi?
  if n_elements(usephi) eq 0 then begin   
     phi_ccw=0d
     phi_cw=!pi*2d
     ;sift phi from specastro xls program
     for i=0,2 do begin 
        ;ccw
        if phi_l[i] lt phi_u[i] then begin 
           if not ((phi_l[i] lt phi_ccw) and (phi_ccw lt phi_u[i])) then $
              phi_ccw=phi_l[i]
        endif else begin
           if not ((phi_l[i] lt phi_ccw) or (phi_ccw lt phi_u[i])) then $
              phi_ccw=phi_l[i]
        endelse 
        ;cw
        if phi_l[i] lt phi_u[i] then begin
           if not ((phi_l[i] lt phi_cw) and (phi_cw lt phi_u[i])) then $
              phi_cw=phi_u[i] 
        endif else begin
           if not ((phi_l[i] lt phi_cw) or (phi_cw lt phi_u[i])) then $
              phi_cw=phi_u[i]
        endelse 
     endfor 
     while phi_ccw gt (2*!pi) do phi_ccw=phi_ccw-2*!pi
     d1=phi_ccw
;     if phi_ccw gt !pi then d1=-(phi_ccw - (2*!pi))
     while phi_cw gt (2*!pi) do phi_cw=phi_cw-(2*!pi)
     d2=phi_cw
;     if phi_cw gt !pi then d2=-(phi_cw - (2*!pi))
     if d1 lt d2 then phi=phi_ccw else phi=phi_cw
     if d1 eq d2 then phi=0d
     
  endif else phi=usephi*!dtor
  
  ; Great Circle Distance:
  p1r = [ra1,dec1] * !dtor	;To radians
  p2r = [ra2,dec2] * !dtor
  dlon = 2*!pi+ p2r(0) - p1r(0) ;delta longitude
  while dlon gt !pi do dlon = dlon - 2*!pi ;to -pi to +pi
  cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
  if cosd gt 1.0d then cosd=1.
  dst = acos(cosd)
  
;  p=dblarr(3,100)
  ndeg=fix(theta_f*!radeg)+1
  pra=dblarr(ndeg)
  pdec=dblarr(ndeg)

  ;pick phi then recalc a & theta's
;  a=x*cos(phi)+b*sin(phi)
  a=dblarr(3)
  for r=0,2 do a[r]=x[r]*cos(phi)+b[r]*sin(phi)
  theta_c=atan(dot(crossp(ve,p0),a),(dot(crossp(p0,a),crossp(ve,a))))
  if theta_c lt 0 then while theta_c lt 0 do theta_c=theta_c+2*!pi 
  theta_f=atan(dot(crossp(p1,p0),a),(dot(crossp(p0,a),crossp(p1,a))))
  if theta_f lt 0 then while theta_f lt 0 do theta_f=theta_f+2*!pi 
  
  dt=theta_f;abs(theta_f-theta_c)
  theta=indgen(ndeg)/(ndeg-1.)*dt;+theta_c
  q=[a[0]*sin(theta_f/2.),a[1]*sin(theta_f/2.),a[2]*sin(theta_f/2.),cos(theta_f/2.)]
  for i=0,ndeg-1 do begin
     p=cos(theta[i])##p0+(1-cos(theta[i]))*dot(p0,a)##a-sin(theta[i])##crossp(a,p0)
     
;     q[*,i]=[a[0]*sin(theta[i]/2.),a[1]*sin(theta[i]/2.),a[2]*sin(theta[i]/2.),cos(theta[i]/2.)]
     elaz2,p,el,az
     pdec[i]=el
     pra[i]=az
     
  endfor 
; elaz,p,lat,lon 
  
  if not keyword_set(noplot) then begin 
     if not keyword_set(myplot) then begin 
        plotsym,0,/fill,1
        plots,-1.*pra,pdec, color = 75, thick=5,symsize=ssize
     endif else begin
        plot,[ra1,ra2],[dec1,dec2],psym=1,xrange=[0,360],yrange=[-90,90],xst=1,yst=1
  
        tvcircle,econ,era,edec,/data
        tvcircle,scon,sra,sdec,/data
        tvcircle,mcon,mra,mdec,/data
        drawarc,[ra1,dec1],[ra2,dec2],100,dst*!radeg,/nowset
        plots,pra,pdec,color=200,psym=1  
     endelse 
  endif 
;  drawcirc,phi*!radeg,lat0,theta_s,200,/nowset
  
  
  
  ;;GET SLEW DURATION
  theta_slew=theta_f;*2.
  theta_roll=a[0]*theta_slew+atan(q[0,*]*q[3,*],q[3,*]^2)       
  while theta_roll lt 0 do theta_roll=theta_roll+2*!pi
  albar=0.001d             ;rad/sec^2
  ombar=0.028d               ;rad/sec
  ombarx=1.58d*!dtor           ;deg/sec
  g=theta_roll/theta_slew
  tfil=2d
  
  ombarrs=min([ombar/sqrt(1.+2*a[0]*g+g^2),ombarx/sqrt(1.-a[0]^2)])
  albarrs=albar/sqrt(1.+2.*a[0]*g+g^2+(1-a[0]^2)*theta_roll^2)
  albarrl=sqrt((albar^2-(1.-a[0]^2)*g^2*ombarrs^4)/(1.+2*a[0]*g+g^2))
  dt_test=sqrt((theta_slew/albarrs)+(tfil/2.)^2)-(tfil/2.)
  om_test=theta_slew/(tfil+dt_test)
  if om_test le ombarrs then Tslew=theta_slew/om_test+om_test/albarrs else $
     Tslew=theta_slew/ombarrs+ombarrs/albarrl
        
  Tslew=Tslew+10.
  tslew1=slewtime(theta_slew,theta_roll,phi)
;  tslew2=slewtime(theta_slew,phi,theta_roll)
  print,[theta_slew,theta_roll,phi]*!radeg
  print,tslew[0],tslew1
  print,dst*!radeg,theta_slew*!radeg
  tslew=max([tslew[0],tslew1])
  
  if keyword_set(dostop) then stop
  return
end 
