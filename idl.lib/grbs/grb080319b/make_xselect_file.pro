pro make_xselect_file,j,evfiles,w,tmin,tmax,mode,regfile,xselfile,evname,phaname,src=src,bg=bg,pu=pu,noregion=noregion,add=add,timefilter=timefilter
  
  if n_elements(add) eq 0 then add=''
  nw=n_elements(w)
  if keyword_set(pu) then pus='pu' else pus=''
  if n_elements(timefilter) eq 0 then timefilter='time_filter.flt'
  if keyword_set(src) then begin 
     phaname=pus+'seg'+ntostr(j+1)+add+mode+'.pha'
     evname=pus+'seg'+ntostr(j+1)+add+mode+'.evt'
     xselfile='xsel'+ntostr(j+1)+add+mode+'.xco'
     timefilter='time_filter'+add+mode+'.flt'
  endif 
  if keyword_set(bg) then begin 
     phaname=pus+'seg'+ntostr(j+1)+add+mode+'bg.pha'
     evname=pus+'seg'+ntostr(j+1)+add+mode+'bg.evt'
     xselfile='xsel'+ntostr(j+1)+add+mode+'bg.xco'
     timefilter='time_filter'+add+mode+'bg.flt'
  endif 
  ;;xselect script

  openw,lun,xselfile,/get_lun
  printf,lun
  printf,lun
  printf,lun,'clear all'
  printf,lun,'yes'
  printf,lun
  printf,lun,'query yes'
  printf,lun
  printf,lun,'read ev '+evfiles[w[0]]
  printf,lun,'./'
  printf,lun
  if nw gt 1 then for i=1,nw-1 do printf,lun,'read ev '+evfiles[w[i]]
  printf,lun
  printf,lun
  printf,lun,'ext cu'
  
  if not keyword_set(noregion) then begin 
     printf,lun,'filter region '+regfile   
     printf,lun,'ext cu'
  endif 

  printf,lun,'filter time file '+timefilter
  openw,flun,timefilter,/get_lun
  printf,flun,sigfig(tmin,12)+'  '+sigfig(tmax,12)
  close,flun
  free_lun,flun

  printf,lun,'ext cu'
  printf,lun,'ext spec'
  printf,lun,'save spec'
  printf,lun,phaname
  printf,lun
  printf,lun
  printf,lun,'ext ev'
  printf,lun,'save ev'
  printf,lun,evname
  printf,lun,'yes'
  printf,lun
  printf,lun
  
  printf,lun,'exit'
  printf,lun
  close,lun
  free_lun,lun                 
  return
end
