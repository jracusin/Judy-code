pro read_pars1,file,params,values
  
  ngrb=0
  npar=n_elements(params)
  values=dblarr(1,npar)
  tmpval=dblarr(npar)
  gotit=intarr(npar)
  openr,lun,file,/get_lun
  line=''
  readf,lun,line
  while (not EOF(lun)) do begin 
     i=0
     for i=0,npar-1 do begin 
        if strpos(line,params[i]) ne -1 then begin 
           if i eq 0 then ngrb=ngrb+1
           chunks=str_sep(line,' ')
           w=where(chunks ne '')
           chunks=chunks[w]
           tmpval[i]=chunks[6] 
           gotit[i]=1
        endif 
     endfor 
        
     wg=where(gotit eq 0,nwg)
     if nwg eq 0 then begin 
        if ngrb gt 1 then begin
           tmpvalues=values
           values=dblarr(ngrb,npar)
           values[0:ngrb-2,*]=tmpvalues[*,*]
        endif 
        if ngrb gt 0 then values[ngrb-1,*]=tmpval
        gotit[*]=0
        tmpval[*]=0
     endif 
     
     readf,lun,line
     
  endwhile
     
  close,lun
  free_lun,lun
  
  return
end 
