function slewtime,theta,phi,roll
  
  ;from matlab program
  ;phi=phi
  ;alfa=roll
  ;theta=roll
  
  VSACACCLIM     = 0.001d
  VSMAXSLEWRATE  = 1.58d*!dtor
  VSACWLIM       = 0.028d
  VSACSLEWMARGIN = 0.9d
  VSACSETTLETIME = 10.0d 
  
  acc_min    = VSACACCLIM
  wsafehold  = VSMAXSLEWRATE    
  w_min      = VSACWLIM
  slewmargin = VSACSLEWMARGIN   
  settletime = VSACSETTLETIME   
;  rho=0
;  time = rho                    
;  dff1= rho                     
;  dist = fltarr(1,2001)         
  stheta=sin(theta)                 
  ctheta=cos(theta)                 
  
  phi1 = phi                  
  roll1 = roll                

  sa = sin(phi1)               
  ca = cos(phi1)               
  sth = sin(roll1)             
  cth= cos(roll1)              
  
  term = 1-ctheta^2*ca^2+2*ctheta*ca*sth*stheta*sa-sth^2*stheta^2*sa^2 
  rh = acos((sth*stheta*ca+sa*cth^2-sa*cth^2*ctheta+sa*ctheta)/sqrt(term)) 
  
  ratio1 = theta/sqrt(theta^2+rh^2)
  ratio2 = max([.001,rh/sqrt(theta^2+rh^2)])
  
  amin = acc_min*ratio1*slewmargin
  wmin = w_min*ratio1*slewmargin
  
  if wmin gt wsafehold then wmin=wsafehold

  amax = acc_min*ratio2*slewmargin
  wmax = w_min*ratio2*slewmargin
            
  if wmin gt sqrt(theta*amin) then ta = 2*sqrt(theta/amin) else $
     ta = wmin/amin+theta/wmin
  if wmax gt sqrt(rh*amax) then tr = 2*sqrt(rh/amax) else $
     tr = wmax/amax+rh/wmax
  
  time = max([ta,tr])+settletime

  return,time
end 
