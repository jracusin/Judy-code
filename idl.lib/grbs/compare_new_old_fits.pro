pro compare_new_old_fits
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  
  oldfile='lc_fit_out_idl.dat'
  newfile='lc_fit_out_idl_int.dat'
  diff=fltarr(ndir)
  for i=0,ndir-1 do begin
     print,dir[i]
     cd,dir[i]
     
     if exist(oldfile) and exist(newfile) then begin 
        read_lcfit,oldfile,pname,oldp,oldperror,oldchisq,olddof,oldbreaks
        read_lcfit,newfile,pname,p,perror,chisq,dof,breaks
        j=[i*2+1]
        
        if n_elements(p) eq n_elements(oldp) then begin 
           diff[i]=oldp[j]-p[j]
        endif 
     endif else begin
        if exist(oldfile) and not exist(newfile) then begin
           if numlines(oldfile) gt 2 then begin
              print,'Need to redo fit!'
              stop
           endif else print,'skip fit'
        endif else print,'Needs to be fit'
        
 ;;       print,'oldfile ',exist(oldfile)
 ;;       print,'newfile ',exist(newfile)
     endelse 
     
     cd,'..'
  endfor 
     
  plothist,diff
  
  stop
  
  
  return
end
