pro calc_suncons,ra,dec
  
  if n_params() eq 0 then begin
     print,'syntax - calc_sunpos,ra,dec,today'
     print,'              date format: [2007,01,01]'
     return
  endif 
  
;  jd=2400000.5d
  now=systime(/julian);-jd
  
  con1=1
  con2=1
  n=60
;  ras=dblarr(n) & decs=dblarr(n)  
  sras=dblarr(n) & sdecs=dblarr(n)
  for i=0d,n-1 do begin
     date=now+i
     sunpos,date,sra,sdec
     sep=separation(sra,sdec,ra,dec)/3600.
;     ras[i]=ra
;     decs[i]=dec
     sras[i]=sra
     sdecs[i]=sdec
     
     daycnv,date,yr,mn,day,hr
     if sep gt 45. and con1 then begin
        print,'Reached S/C sun constraint: ',sep,yr,mn,day
;        con1=0
;        stop
     endif 
     if sep gt 75. and con2 then begin
        print,"Reached Neil's sun constraint: ",sep,yr,mn,day
        con2=0
     endif 
     
     
  endfor 
  
  plot,sras,sdecs,psym=1,xrange=[0,360],yrange=[-90,90],/xsty,/ysty
  plots,ra,d,psym=2,color=!red
  
  stop
  return
end 
     
     
  
