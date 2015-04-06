pro run_fluecorr,onelog=onelog
  
  gdir='/bulk/axiom/morris/GRB_flares_output2/'
  openw,llun,gdir+'run_fluecorr.log2',/get_lun
;  openw,lun,gdir+'run_fluecorr.table',/get_lun
  
  dir=file_search(gdir+'GRB*',/mark_directory)
  errcodes=0
  flare=''
  
  for i=0,n_elements(dir)-1 do begin
     print,dir[i]
     printf,llun,dir[i]
     odir=strmid(dir[i],37,25)
     cd,dir[i]
     mdirs=file_search('fltemp*',/mark_dir)
     if mdirs[0] ne '' then begin 
        fnum=strmid(mdirs[0],6,1)
        if not exist('flare'+fnum) then spawn,'mkdir flare'+fnum
     endif 
        
     fdirs=file_search('flare*',/mark_dir)
     
     if fdirs[0] ne '' then begin 
        for j=0,n_elements(fdirs)-1 do begin
           print,'..............................................................'
           print,fdirs[j]
           printf,llun,'..............................................................'
           printf,llun,fdirs[j]
;        fnum=strmid(fdirs[j],5,1)
           tmp=str_sep(fdirs[j],'flare')
           tmp2=str_sep(tmp[1],'/')
           fnum=tmp2[0]
        
           if not keyword_set(onelog) then fluecorr,fnum,errcode=err $
           else fluecorr,fnum,errcode=err,/onelog,llun=llun
           
;        err=0
;        printf,lun,odir+fdirs[j]+' '+ntostr(err)
           flare=[flare,odir+fdirs[j]]
           errcodes=[errcodes,err]
        endfor 
     endif 
  endfor 
 
  flare=flare[1:*]
  errcodes=errcodes[1:*]
  
  close,llun;,/file
  free_lun,llun
  
  writecol,gdir+'run_fluecorr.table',flare,errcodes
  cd,'../'
  
  ;readcol,'run_fluecorr.table',flare,errcodes,format='(a,i)'
  s=sort(errcodes)
  
  writecol,gdir+'run_fluecorr.table.sort',flare[s],errcodes[s]
  
  return
end 
