function linfunc,t,p
  
  norm=p[0]
  m1=p[1]
  
  yfit=norm+t*m1
  
  return,yfit
end 
