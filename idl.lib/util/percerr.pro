function percerr,sigx0,x0,sigx1,x1,sigx2,x2,sigx3,x3,sigx4,x4

  if n_elements(x1) eq 0 then begin 
     x1=1
     sigx1=0.
  endif 

  if n_elements(x2) eq 0 then begin 
     x2=1
     sigx2=0.
  endif 

  if n_elements(x3) eq 0 then begin 
     x3=1
     sigx3=0.
  endif 

  if n_elements(x4) eq 0 then begin 
     x4=1
     sigx4=0.
  endif 

  sigx0=sigx0*1d
  x0=x0*1d
  sigx1=sigx1*1d
  x1=x1*1d
  sigx2=sigx2*1d
  x2=x2*1d
  sigx3=sigx3*1d
  x3=x3*1d
  sigx4=sigx4*1d
  x4=x4*1d

  psig=sqrt((sigx0/x0)^2+(sigx1/x1)^2+(sigx2/x2)^2+(sigx3/x3)^2+(sigx4/x4)^2)

  return,psig
end
