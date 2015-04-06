pro read_specfit,specstr,dir=dir,append=append
  
  if n_params() eq 0 then begin
     print,'syntax - read_specfit,specstr,dir=dir'
     return
  endif 
  
  d2=dblarr(2)
  specstr=create_struct('seg',0,$
                        'mode','',$
                        'nev',0L,$
                        'nev2',0L,$
                        'z',0.,$
                        'nhgal',0d,$
                        'nh',0d,$
                        'nh_err',d2,$
                        'pow',0d,$
                        'pow_err',d2,$
                        'norm',0d,$
                        'norm_err',d2,$
                        'norm2',0d,$
                        'norm2_err',d2,$
                        'chisq',0d,$
                        'dof',0L,$
                        'rate',0d,$
                        'rate2',0d,$
                        'model_rate',0d,$
                        'model_rate2',0d,$
                        'flux',0d,$
                        'flux_err',d2,$
                        'unabs_flux',0d,$
                        'flux2',0d,$
                        'flux2_err',d2,$
                        'unabs_flux2',0d,$
                        'pu_corr',0d,$
                        'exptime',0d,$
                        'exptime2',0d)
  
  if n_elements(append) eq 0 then append=''
  if n_elements(dir) eq 0 then dirs='spec/' else dirs=dir+'/'
  datfiles=file_search(dirs+'seg*'+append+'.dat')
  if append eq '' then begin 
     s=strpos(datfiles,'s.d')
     w=where(s eq -1)
     datfiles=datfiles[w]
  endif 

  n=n_elements(datfiles)
  specstr=replicate(specstr,n)
  
  for i=0,n-1 do begin
     file=datfiles[i]
     segpos=strpos(file,'seg')
     seg=strmid(file,segpos+3,1)
     
     openr,lun,file,/get_lun
     line=readline(lun)
     
     if line[0] ne 'N_ev' then begin 
        specstr[i].nh_err=[line[1],line[2]]
        
        line=readline(lun)
        specstr[i].pow_err=[line[1],line[2]]
        
        line=readline(lun)
        specstr[i].norm_err=[line[1],line[2]]
        line=readline(lun)
        if line[0] eq 'norm_err' then begin
           specstr[i].norm2_err=[line[1],line[2]]
           line=readline(lun)
        endif 
;        if n_elements(line) eq 5 then $
;           specstr[i].norm2_err=[line[3],line[4]]
        
        specstr[i].nhgal=line[1]
        line=readline(lun)
        specstr[i].nh=line[1]
        line=readline(lun)
        if line[0] eq 'z' then begin
           specstr[i].z=line[1]
           line=readline(lun)
        endif 
;           specstr[i].nh=line[1]
;           readcol,dirs+'nhgal.out',blah1,format='(a)',delim='&'
;           blah1=blah1[n_elements(blah1)-1]
;           blah1=str_sep(blah1,' ')
;           specstr[i].nhgal=blah1[n_elements(blah1)-1]*1e-22
;        endelse 
        
;        line=readline(lun)
        specstr[i].pow=line[1]
        
        line=readline(lun)
        specstr[i].norm=line[1]
        line=readline(lun)
        if line[0] eq 'norm' then begin
           specstr[i].norm2=line[1]
           line=readline(lun)
        endif 
;        if n_elements(line) eq 3 then $
;           specstr[i].norm2=line[2]
        
;        line=readline(lun)
        specstr[i].chisq=line[1]
        
        line=readline(lun)
        specstr[i].dof=line[1]
        
        line=readline(lun)
        specstr[i].rate=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].rate2=line[2]        
        
        line=readline(lun)
        specstr[i].model_rate=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].model_rate2=line[2]        
        
        line=readline(lun)
        specstr[i].flux=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].flux2=line[2]

        line=readline(lun)
        specstr[i].flux_err=[line[1],line[2]]
        if n_elements(line) eq 5 then $
           specstr[i].flux2_err=[line[3],line[4]]

        line=readline(lun)
        specstr[i].unabs_flux=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].unabs_flux2=line[2]

        line=readline(lun)
        specstr[i].nev=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].nev2=line[2]
     endif else begin 
        specstr[i].nev=line[1]
        if n_elements(line) eq 3 then $
           specstr[i].nev2=line[2]
     endelse 
     
     if not eof(lun) then begin
        line=readline(lun)
        if strtrim(line[0],2) eq 'pu_corr' then specstr[i].pu_corr=line[1]
        if strtrim(line[0],2) eq 'exptime' then begin
           specstr[i].exptime=line[1]
           if n_elements(line) eq 3 then specstr[i].exptime2=line[2]
        endif 
     endif 
     
     if not eof(lun) then begin
        line=readline(lun)
        if strtrim(line[0],2) eq 'pu_corr' then specstr[i].pu_corr=line[1]
        if strtrim(line[0],2) eq 'exptime' then begin
           specstr[i].exptime=line[1]
           if n_elements(line) eq 3 then specstr[i].exptime2=line[2]
        endif 
     endif 
        
     specstr[i].seg=seg
     
     close,lun
     free_lun,lun
  
     specstr[i].nh_err=abs(specstr[i].nh-specstr[i].nh_err)
     specstr[i].norm_err=abs(specstr[i].norm-specstr[i].norm_err)
     specstr[i].norm2_err=abs(specstr[i].norm2-specstr[i].norm2_err)
     specstr[i].pow_err=abs(specstr[i].pow-specstr[i].pow_err)
     specstr[i].flux_err=abs(specstr[i].flux-specstr[i].flux_err)
     specstr[i].flux2_err=abs(specstr[i].flux2-specstr[i].flux2_err)
     
  endfor 
  
  return
end 
