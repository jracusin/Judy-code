pro ps2pdf,file

;  base=strsplit(file,'.ps',/ex)
  s=strpos(file,'.ps')
  base=strmid(file,0,s)
  pdffile=base+'.pdf'
  spawn,'ps2pdf '+file+' '+pdffile
  
  return
end 
