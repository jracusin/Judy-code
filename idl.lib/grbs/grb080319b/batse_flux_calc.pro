pro batse_fluence_calc
  
  file='~/Desktop/GRB080319B/batse_fluence_table.txt'
  
  readcol,file,trig,f1,f1err,f2,f2err,format='(i,d,d,d,d)'
  readcol,file,f3,f3err,f4,f4err,format='(d,d,d,d)'
  
  f=f1+f2+f3+f4
  
  plothist,f*1d8
  
  
  stop
  return
end 
