pro match_pix,x1,y1,x2,y2,m1,m2
  
  if n_params() eq 0 then begin
     print,'syntax - match_pix,x1,y1,x2,y2,m1,m2'
     return
  endif 
  
  p1=ntostr(x1)+'_'+ntostr(y1)
  p2=ntostr(x2)+'_'+ntostr(y2)
  
  match,p1,p2,m1,m2
  
  return
end 
