pro write_astrom_par,mra,mdec,x,y
  
  n=n_elements(mra)
  
  radec,mra,mdec,ihr, imin, xsec, ideg, imn, xsc
  
  delim=':';' '
;  print,'J2000'
;  print,'14 12 06.298 +16 58 18.60 J2000.0'
  for i=0,n-1 do begin
     
     print,ntostr(i+1)+','+ntostr(ihr[i])+':'+ntostr(imin[i])+':'+ntostr(xsec[i])+',+'+ntostr(ideg[i])+':'+ntostr(imn[i])+':'+ntostr(xsc[i]);+' J2000.0'
;     print,ntostr(x[i])+':'+ntostr(y[i])
     
  endfor 
  
  
  
  return
end 
