pro read_butler_eiso,grb,eiso,eisoerr,z
  
  file='~/jetbreaks/butler_eiso.tex'
  readcol,file,grb,a,b,epk,s,chisq,epkobs,niso,eisos,zs,delim=' & ',format='(a,a,a,a,a,a,a,a,a,a)'
  n=n_elements(grb)
  
  eiso=dblarr(n)
  eisoerr=fltarr(2,n)
  z=eiso
  for i=0,n-2 do begin
     sep=str_sep(eisos[i],'$')
     emid=sep[1]
     pmpos=strpos(emid,'pm')
     if pmpos eq -1 then begin 
        sep=str_sep(emid,'^')
        eiso[i]=sep[0]
        emid=sep[1]
        sep=str_sep(emid,'{')
        eplus=strtrim(sep[1],2)
        eisoerr[1,i]=strmid(eplus,1,strlen(eplus)-3)
        eminus=strtrim(sep[2],2)
        eisoerr[0,i]=strmid(eminus,1,strlen(eminus)-2)
     endif else begin
        sep=str_sep(emid,'\pm')
        eiso[i]=sep[0]
        eisoerr[*,i]=sep[1]
     endelse 
     pos=strpos(zs[i],'$')
     if pos ne -1 then begin
        zz=strtrim(zs[i],2)
        zz=strmid(zz,1,strlen(zz)-2)
        sep=str_sep(zz,'^')
        z[i]=sep[0]
     endif 

  endfor 
  
  return
end 
