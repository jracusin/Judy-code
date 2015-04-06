pro predict_slewtime,theta_roll,theta_f,a,tslew
  albar=0.001d                  ;rad/sec^2
  ombar=0.028d               ;rad/sec
  ombarx=1.58d*!dtor           ;deg/sec
  g=theta_roll/theta_f
  tfil=2d
  
  ombarrs=min([ombar/sqrt(1.+2*a[0]*g+g^2),ombarx/sqrt(1.-a[0]^2)])
  albarrs=albar/sqrt(1.+2.*a[0]*g+g^2+(1-a[0]^2)*theta_roll^2)
  albarrl=sqrt((albar^2-(1.-a[0]^2)*g^2*ombarrs^4)/(1.+2*a[0]*g+g^2))
  dt_test=sqrt((theta_f/albarrs)+(tfil/2.)^2)-(tfil/2.)
  om_test=theta_f/(tfil+dt_test)
  if om_test le ombarrs then Tslew=theta_f/om_test+om_test/albarrs else $
     Tslew=theta_f/ombarrs+ombarrs/albarrl
  Tslew=Tslew[0]+10.
  
  
;  sundist=calc_coord_offset(sra,sdec,ra1,dec1)/3600.*!dtor
;  tslew2=slewtime(theta_f,theta_roll,phi)
;  tslew3=slewtime(theta_f,phi,theta_roll)
;  tslew4=slewtime(theta_f,sundist,theta_roll)
  
;  tslew=max([tslew0,tslew1,tslew2,tslew3,tslew4])
  
  return
end 
pro predict_slew,ra1,dec1,ra2,dec2,era,edec,sra,sdec,mra,mdec,pra,pdec,tslew,myplot=myplot,usephi=usephi,noplot=noplot,dostop=dostop,theta_roll=theta_roll,color=color,dist=dist,noslew=noslew,phi=phi,tslew0=tslew0,tslew1=tslew1
  
  noslew=0
  theta_roll2=theta_roll
  econ=66.+28.
  scon=45.
  mcon=21.
 
;  p0=cel2xyz(ra1*!dtor,dec1*!dtor)
;  p1=cel2xyz(ra2*!dtor,dec2*!dtor)
;  vearth=cel2xyz(era*!dtor,edec*!dtor)
;  vsun=cel2xyz(sra*!dtor,sdec*!dtor)
;  vmoon=cel2xyz(mra*!dtor,mdec*!dtor)
  vect2,dec1,ra1,p0
  vect2,dec2,ra2,p1
  vect2,edec,era,vearth         
  vect2,sdec,sra,vsun
  vect2,mdec,mra,vmoon

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
           ve=vsun
           theta_e=scon*!dtor
        end 
        2: begin
           ve=vmoon
           theta_e=mcon*!dtor
        end 
     endcase 
     
     u=0.5*(dot(ve,x)^2-(alpha^2-2*alpha*dot(ve,b)*cos(theta_e)+dot(ve,b)^2))
     v=dot(ve,x)*(dot(ve,b)-alpha*cos(theta_e))
     w=sin(theta_e)^2-0.5*(dot(ve,x)^2+(alpha^2-2*alpha*dot(ve,b)*cos(theta_e)+dot(ve,b)^2))
     
;     print,u,v,w
     tcs=w/sqrt(u^2+v^2)
;     print,tcs
;     if tcs gt 1 then stop ;print,tcs
;     if tcs gt 1 then tcs=1-tcs
;     print,tcs
;     stop
;     while tcs gt 1 do tcs=1./tcs
;stop
;     print,tcs
     
     wt=where(tcs gt 1,nwt)
     if nwt gt 0 then tcs[wt]=1.
     
     phi1=0.5*(atan(v,u)+acos(tcs))
     phi2=0.5*(atan(v,u)-acos(tcs))
     
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
              dadphi=dadphi[*]
              c0=(1-cos(theta_c))
              c1=dot(p0,dadphi)
              c2=dot(p0,a)
              c3=sin(theta_c)
              c4=crossp(dadphi,p0)
              dpdphi=c0[0]*(c1[0]*a+c2[0]*dadphi)-c3[0]*c4
              
              res=dot(dpdphi,ve)
              if res lt 0 then phi_l[k]=phi else phi_u[k]=phi
           endelse 
        endelse
;        print,k,res,phi*!radeg
     endfor 
  endfor 
  
;  print,phi_l;*!radeg
;  print,phi_u;*!radeg
  
  ;which phi?
  if n_elements(usephi) eq 0 then begin   
     phiccw=0.
     phicw=2*!pi     
     w=where(phi_u-phi_l lt 0,nw)
     if nw gt 0 then phi_u[w]=phi_u[w]+2*!pi
;     print,phi_u*!radeg
;     phiccw=phi_l[0]
;     phicw=phi_u[0]
     gphi=fltarr(360)
     iphi=findgen(360)*!dtor
     for i=0,2 do begin
        w=where(iphi gt phi_l[i] and iphi lt phi_u[i],nw)
        if nw gt 0 then begin 
           if phi_u[i] gt 2*!pi then begin
              w1=where(iphi lt (phi_u[i] mod (2*!pi)))
              w=[w,w1]
              w=w[rem_dup(w)]
           endif 
           gphi[w]=gphi[w]+1
        endif 

;        if phi_l[i] lt phiccw and phiccw lt phi_u[i] then $
;           phiccw=phiccw else begin
;           if phi_l[i] lt phiccw+2*!pi and phiccw+2*!pi lt phi_u[i] then phiccw=phiccw else phiccw=phi_l[i]
;        endelse 
;        if phi_l[i] lt phicw and phicw lt phi_u[i] then $
;           phicw=phicw else begin 
;           if phi_l[i] lt phicw+2*!pi and phicw+2*!pi lt phi_u[i] then phicw=phiccw else phicw=phi_u[i]
;        endelse 
     endfor 
     wphi=where(gphi gt 2,nwphi)
     if nwphi eq 0 then begin 
        print,'No single-axis slew possible'
        noslew=1
        wphi=where(gphi gt 1,nwphi)
     endif 
     mphi=min([iphi[wphi],2*!pi-iphi[wphi]],wm)
     if wm ge nwphi then wm=wm-nwphi
     
     phi=iphi[wphi[wm[0]]]
     
     mphi=[phi_u,phi_l,0d] mod (2*!pi)
     wphi2=where(abs(phi-mphi) lt 2.0*!dtor or abs(phi-mphi) gt 358*!dtor,nwphi)
     if nwphi gt 0 then phi=mphi[wphi2[0]]

;     if keyword_set(dostop) then stop

  endif else phi=usephi*!dtor
  
  print,phi
  ; Great Circle Distance:
  p1r = [ra1,dec1] * !dtor	;To radians
  p2r = [ra2,dec2] * !dtor
  dlon = 2*!pi+ p2r(0) - p1r(0) ;delta longitude
  while dlon gt !pi do dlon = dlon - 2*!pi ;to -pi to +pi
  cosd = sin(p1r(1))*sin(p2r(1)) + cos(p1r(1))*cos(p2r(1))*cos(dlon)
  if cosd gt 1.0d then cosd=1.
  dst = acos(cosd)
  
;  p=dblarr(3,100)

  ;pick phi then recalc a & theta's
  a=x*cos(phi)+b*sin(phi)
  theta_c=atan(dot(crossp(ve,p0),a),(dot(crossp(p0,a),crossp(ve,a))))
  if theta_c lt 0 then while theta_c lt 0 do theta_c=theta_c+2*!pi 
  theta_f=atan(dot(crossp(p1,p0),a),(dot(crossp(p0,a),crossp(p1,a))))
  if theta_f lt 0 then while theta_f lt 0 do theta_f=theta_f+2*!pi 
  
  ndeg=fix(theta_f*!radeg)+1
  pra=dblarr(ndeg)
  pdec=dblarr(ndeg)

  dt=theta_f;abs(theta_f-theta_c)
  theta=indgen(ndeg)/(ndeg-1.)*dt;+theta_c
  q=[a[0]*sin(theta_f/2.),a[1]*sin(theta_f/2.),a[2]*sin(theta_f/2.),cos(theta_f/2.)]
  dist=0.
  for i=0,ndeg-1 do begin
     p=cos(theta[i])##p0+(1-cos(theta[i]))*dot(p0,a)##a-sin(theta[i])##crossp(a,p0)
     
;     q[*,i]=[a[0]*sin(theta[i]/2.),a[1]*sin(theta[i]/2.),a[2]*sin(theta[i]/2.),cos(theta[i]/2.)]
     elaz2,p,el,az
     pdec[i]=el
     pra[i]=az
     if i gt 0 then $
        dist=dist+calc_coord_offset(pra[i-1],pdec[i-1],pra[i],pdec[i])/3600d
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
        if n_elements(color) eq 0 then color=200
        plots,pra,pdec,color=color,psym=1  
     endelse 
  endif 
;  drawcirc,phi*!radeg,lat0,theta_s,200,/nowset
  
  
;  print,theta_f*!radeg,dist
  ;;GET SLEW DURATION
  theta_roll=a[0]*theta_f+atan(q[0,*]*q[3,*],q[3,*]^2)       
;  print,theta_roll*!radeg
  
  predict_slewtime,theta_roll,theta_f,a,tslew0
  predict_slewtime,theta_roll2,theta_f,a,tslew1
;  stop
;  print,tslew0,tslew1
;  tslew=max([tslew0,tslew1])
  tslew=tslew0
;  tslew1=0
  
  if keyword_set(dostop) then stop
  return
end 
