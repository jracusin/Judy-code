pro image_positions,ldps,xguess,yguess
  
  files=''
  for i=0,n_elements(ldps)-1 do begin
     file=findfile('*LDP'+ntostr(ldps[i])+'*im*image*')
     if file[0] ne '' then files=[files,file]
  endfor 
  lista=files[1:*]
  
  
  
;  readcol,'HD131156off3.cat',lista,format='(a)'
  nl=n_elements(lista)
  xout=dblarr(nl)
  yout=dblarr(nl)
  scra=dblarr(nl)
  scdec=dblarr(nl)
  for j=0,n_elements(lista)-1 do begin
     tab=mrdfits(lista[j],0,hdr,/silent)
     cntrd,tab,241.42,524.32,xo,yo,5
     print,lista[j],xo,yo
     xout[j]=xo
     yout[j]=yo
     scra[j]=sxpar(hdr,'RA')
     scdec[j]=sxpar(hdr,'DEC')
  endfor
  print,'Mean X-centr: ',mean(xout)
  print,'St dev X-centr: ',stdev(xout)
  print,'Mean Y-centr: ',mean(yout)
  print,'St dev Y-centr: ',stdev(yout)
  print,'Mean SC RA: ',mean(scra)
  print,'Mean SC DEC: ',mean(scdec)s
  
  
  return
end 
