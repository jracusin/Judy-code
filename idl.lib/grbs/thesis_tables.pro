pro thesis_tables
  
  cd,!mdata
  cr=mrdfits('closure_relations_total_2sig.fits',1)
  
  grbs=cr[rem_dup(cr.grb)].grb
  n=n_elements(grbs)
  openw,lun,'temporal_table.txt',/get_lun
  openw,slun,'spectral_table.txt',/get_lun
  openw,tlun,'times_table.txt',/get_lun
  aa=' & '
  ks=1e-3
  for i=0,n-1 do begin
     read_specfit,spec,dir=strtrim(grbs[i],2)+'/spec/',append='_2s'
     w=where(strtrim(cr.grb,2) eq strtrim(grbs[i],2),nw)
     
     alphas='$'+numdec(cr[w].alpha,2)+'^{+'+numdec(cr[w].alphaerr[1],2)+'}_{-'+numdec(cr[w].alphaerr[0],2)+'}$'
     tbreak='$'+numdec(cr[w].tbreak*ks,1)+'^{+'+numdec(cr[w].tbreakerr[1]*ks,1)+'}_{-'+numdec(cr[w].tbreakerr[0]*ks,1)+'}$'
     tbreaks='$'+numdec(cr[w].tbreak,1)+'^{+'+numdec(cr[w].tbreakerr[1],1)+'}_{-'+numdec(cr[w].tbreakerr[0],1)+'}$'
     betas='$'+numdec(cr[w].beta,2)+'^{+'+numdec(cr[w].betaerr[1],2)+'}_{-'+numdec(cr[w].betaerr[0],2)+'}$'
     nhgal=numdec(spec[0].nhgal,2)
     
     l=where(spec.nh lt 0.01,nl)
     nh=spec.nh
     if nl gt 0 then nh[l]=0.
     nh='$'+numdec(nh,2)+'^{+'+numdec(spec.nh_err[1],2)+'}_{-'+numdec(spec.nh_err[0],2)+'}$'
     l=where(spec.nh gt 10,nl)
     if nl gt 0 then nh[l]=''
     l=where(spec.nh lt 0.01 and spec.nh_err[0] lt 0.01 and spec.nh_err[1] lt 0.01,nl)
     if nl gt 0 then nh[l]='0.00'
     
     if nw eq 1 then begin
        thing=alphas[w]+aa+aa+aa+aa+aa+aa
        sthing=nh[w]+aa+betas[w]+aa+aa+aa+aa+aa+aa
     endif 
     if nw eq 2 then begin 
        if cr[w[0]].seg1 eq 1 then begin
           thing=alphas[0]+aa+tbreaks[1]+aa+alphas[1]+aa+aa+aa+aa 
           sthing=nh[0]+aa+betas[0]+aa+nh[1]+aa+betas[1]+aa+aa+aa+aa
        endif else begin
           thing=aa+aa+alphas[0]+aa+tbreak[1]+aa+alphas[1]+aa+aa
           sthing=aa+aa+nh[0]+aa+betas[0]+aa+nh[1]+aa+betas[1]+aa+aa
        endelse 
     endif 
     if nw eq 3 then begin 
        if cr[w[0]].seg1 eq 1 then begin
           thing=alphas[0]+aa+tbreaks[1]+aa+alphas[1]+aa+tbreak[2]+aa+alphas[2]+aa+aa 
           sthing=nh[0]+aa+betas[0]+aa+nh[1]+aa+betas[1]+aa+nh[2]+aa+betas[2]+aa+aa
        endif else begin 
           thing=aa+aa+alphas[0]+aa+tbreak[1]+aa+alphas[1]+aa+tbreak[2]+aa+alphas[2]
           sthing=aa+aa+nh[0]+aa+betas[0]+aa+nh[1]+aa+betas[1]+aa+nh[2]+aa+betas[2]
        endelse 
     endif 
     if nw eq 4 then begin
        thing=alphas[0]+aa+tbreaks[1]+aa+alphas[1]+aa+tbreak[2]+aa+alphas[2]+aa+tbreak[3]+aa+alphas[3]
        sthing=nh[0]+aa+betas[0]+aa+nh[1]+aa+betas[1]+aa+nh[2]+aa+betas[2]+aa+nh[3]+aa+betas[3]
     endif 
     
     printf,lun,strmid(cr[w[0]].grb,3,7)+aa+thing+' \\'
     printf,slun,strmid(cr[w[0]].grb,3,7)+aa+nhgal+aa+sthing+' \\'
     
     tstart='$'+numdec(cr[w[0]].tstart,1)+'$'+aa+'$'+numdec(cr[w[0]].theta_tstart,1)+'$'
     tstop='$'+numdec(cr[w[0]].tstop*ks,1)+'$'+aa+'$'+numdec(cr[w[0]].theta_stop,1)+'$'
     tlastdet='$'+numdec(cr[w[0]].tlastdet*ks,1)+'$'+aa+'$'+numdec(cr[w[0]].theta_lastdet,1)+'$'
     if abs(cr[w[0]].tlastdet-cr[w[0]].tstop) lt 10 then tlastdet=aa
     tlastpos='$'+numdec(cr[w[0]].tlastpos*ks,1)+'$'+aa+'$'+numdec(cr[w[0]].theta_lastpos,1)+'$'
     if abs(cr[w[0]].tlastdet-cr[w[0]].tlastpos) lt 10 or cr[w[0]].tlastpos eq 0 then tlastpos=aa

     printf,tlun,strmid(cr[w[0]].grb,3,7)+aa+tstart+aa+tstop+aa+tlastdet+aa+tlastpos+' \\'
  endfor 
  
  close,lun
  free_lun,lun
  
  close,slun
  free_lun,slun
  
  close,tlun
  free_lun,tlun
  return
end 
     
