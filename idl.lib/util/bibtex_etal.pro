pro bibtex_etal,bibfile

  readcol,bibfile,lines,format='(a)',delim='$'

  a=where(strpos(lines,'author') ne -1,n)
  t=where(strpos(lines,'title') ne -1)
  as=t-1

  num=intarr(n)
  for i=0,n-1 do begin
     wna=0
     while wna ne -1 do begin
        alines=''
        for j=0,as[i]-a[i] do alines=alines+lines[a[i]+j]
        wna=strpos(alines,'{',wna+1)
        if wna ne -1 then num[i]=num[i]+1
     endwhile 
     num[i]=num[i]-1
     if num[i] gt 5 then begin 
        print,num[i]
        print,lines[a[i]:as[i]]
     endif 
stop
  endfor 



stop
  return
end 

