pro data_for_derek
  
  dir='~/stuff_for_people/derek'
  cd,dir
  dir2='/bulk/wolverines/racusin/grbs/'
  
  readcol,'burst.dat',bursts,format='(a)'
  nb=n_elements(bursts)
  
  openw,lun,'grb_lightcurve_fits.csv',/get_lun
  printf,lun,'GRB,Norm,pow1,tbreak1,pow2,tbreak2,pow3,tbreak3,pow4'
  
  for i=0,nb-1 do begin
     bdir=dir2+'GRB'+strtrim(bursts[i],2)
     if exist(bdir) then begin 
        read_lcfit,bdir+'/lc_fit_out_idl_int2.dat',pname,p,perror
        printf,lun,'GRB'+bursts[i]+','+ntostrarr(p,',')
     endif 

  endfor 
  
  close,lun
  
  return
  
end 
  
