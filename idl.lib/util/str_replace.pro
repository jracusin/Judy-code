function str_replace,str,search,replace

  if n_params() eq 0 then begin
     print,'syntax - new_string=str_replace(original string, search for string , replace with this string)'
     return,''
  end 

  nstr=n_elements(str)
  newstr=strarr(nstr)

  for i=0,nstr-1 do begin 
;     without=strsplit(str[i],search,/ex)
;     newstr[i]=strjoin(without,replace)
     spos=strpos(str[i],search)
     if spos[0] ne -1 then begin 
        slen=strlen(search)
        tslen=strlen(str[i])
        without=[strmid(str[i],0,spos),strmid(str[i],spos+slen,tslen-spos-slen)]
        newstr[i]=strjoin(without,replace)
     endif else newstr[i]=str[i]
  endfor 

  return,newstr
end 
