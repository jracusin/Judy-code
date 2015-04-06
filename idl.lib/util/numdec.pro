function numdec,num0,ndec,sci=sci,tex=tex,idlplot=idlplot
  
  if n_params() eq 0 then begin
     print,'syntax -  numstring=numdec(num,ndec,sci=sci,tex=tex)   i.e. numstr=(0.123,2)=0.12'
     return,-1
  endif 
  
  num=num0
  if keyword_set(sci) then begin
     lg=round(alog10(num)-0.5d)
     num=num/10d^lg
  endif 

  n=n_elements(num)
  wneg=where(num lt 0,nneg)
  nnum=abs(num)

  nummult=(10d)^ndec
  numbig=nnum*nummult
  
  numround=round(numbig)
  nlog=round(alog10(nnum)-0.5)
  w=where(nlog lt 0,nw)
  if nw gt 0 then nlog[w]=0
  nfig=ndec+2+nlog
  
  numstr=strarr(n)
  for i=0,n-1 do numstr[i]=ntostr(numround[i]/nummult,nfig[i])
  if n eq 1 then numstr=numstr[0]
  if nneg gt 0 then numstr[wneg]='-'+numstr[wneg]
  numstr_out=numstr
  if keyword_set(sci) then begin
     numstr_out=numstr+'e'+ntostr(lg)
     if keyword_set(tex) then numstr_out=numstr+'\times 10^{'+ntostr(lg)+'}'
     if keyword_set(idlplot) then numstr_out=numstr+'x10!U'+ntostr(lg)+'!N'
  endif 
  
  w=where(num0 eq 0,nw)
  if nw gt 0 then numstr_out[w]='0.0'
  if keyword_set(tex) then numstr_out='$'+numstr+'$'

  return,numstr_out
end  
