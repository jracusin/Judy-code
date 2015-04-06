function strsame,str,len
  
  if type(str) ne 7 then strr=ntostr(str) else strr=str
  islen=strlen(strr)
  sp=' '
  s=''
  n=n_elements(str)
  outstr=strarr(n)
  for i=0,n-1 do begin 
     s=''
     nlen=len-islen[i]
     if nlen gt 0 then for j=0,nlen-1 do s=s+sp
     
     outstr[i]=strr[i]+s
  endfor 

  if n_elements(outstr) eq 1 then outstr=outstr[0]
  return,outstr
end 
