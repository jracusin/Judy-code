pro read_liang_eiso,grb,eiso,z,limit
  
  file='~/jetbreaks/liang_eisok.tex'
  readcol,file,grb,z,reg,p,eps,y,theta,logekiso,logek,lognum,lognuc,ref,delim=' & ',format='(a,a,a,a,a,a,a,a,a,a,a,a)'
  n=n_elements(grb)

  eiso=dblarr(n) & thetaj=eiso
  limit=intarr(n)
  for i=0,n-1 do begin
     g=strpos(theta[i],'>')
     if g ne -1 then begin 
        limit[i]=1
        thetaj[i]=strmid(theta[i],1,5)
     endif else thetaj[i]=theta[i]
     eiso[i]=(10d^logekiso[i])/1d52
     
  endfor 
  
  return
end 

