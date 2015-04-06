pro combine_wtpc_fsout,fswtstr,fspcstr,fsstr,wtstr,pcstr,str,fspcstr2,pcstr2
  
  make_flarespec_outs_struct,fsstr
  
  struct_assign,fswtstr,fsstr
  
  model=['pow','cutpow','grbm','bb']
  comp=['','_fl','_upl']
  abs=['','_unabs']
  ff=['flux','rate','fluence']
  wtdel=ntostr(wtstr.tstop-wtstr.tstart)
  pcdel=ntostr(pcstr.tstop-pcstr.tstart)
  if n_elements(fspcstr2) ne 0 then pcdel2=ntostr(pcstr2.tstop-pcstr2.tstart)
  del=ntostr(str.tstop-str.tstart)
  for m=0,3 do begin ;model 
     for i=0,2 do begin ;component 
        if i eq 0 then nf=1 else nf=2
        for j=0,1 do begin ;abs
           for f=0,nf do begin ;flux/rate/fluence
              if f ne 1 then begin
                 if n_elements(fspcstr2) eq 0 then begin 
                    com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+wtdel+'+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel+')/'+del
                    com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2)'
                 endif else begin
                    com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+wtdel+'+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel+'+fspcstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel2+')/'+del
                    com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fspcstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2)'
                 endelse 
                 print,com,execute(com)
                 print,com2,execute(com2)
                 tmp=execute(com) & tmp2=execute(com2)
;              print,tmp,tmp2
              endif else begin
                 if n_elements(fspcstr2) eq 0 then $
                    com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+'_rate'+comp[i]+'*'+wtdel+'+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel+')/'+del else $
                    com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+'_rate'+comp[i]+'*'+wtdel+'+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel+'+fspcstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+pcdel2+')/'+del
                 tmp=execute(com)
                 print,com,tmp
              endelse               
              if tmp eq 0 or tmp2 eq 0 then stop
           endfor 
        endfor 
     endfor 
  endfor 

  
  return
end 

function calc_par_err,f,r,c,sigf,n,sign,p,sigp,uln,siguln,ulp,sigulp,t0,t1,upl,ff
  
  dxdf=c/r
;  dxdr=-f*c/r^2
  dxdc=f/r
  if ff eq 0 then delt=t1-t0 else delt=1
  dcdn=1./(p+1.)*(1./delt)*(t1^(p+1.)-t0^(p+1.))*10^n*alog(10.)
  dcdp=(10^n*(p+1.)*(((t1^(p+1.)*alog(t1))-t1^(p+1.))-((t0^(p+1.)*alog(t0))-t0^(p+1.))))/(p+1.)^2/delt
  dcdnul=1./(ulp+1.)*(1./delt)*(t1^(ulp+1.)-t0^(ulp+1.))*10^uln*alog(10.)
  dcdpul=(10^uln*(ulp+1.)*(((t1^(ulp+1.)*alog(t1))-t1^(ulp+1.))-((t0^(ulp+1.)*alog(t0))-t0^(ulp+1.))))/(ulp+1.)^2/delt
;  sigc2=dcdn^2*((10^(n+sign)-10^(n-sign))/2)^2+dcdp^2*sigp^2
;  sigc2=dcdn^2*((10^n-10^(n-sign)))^2+dcdp^2*sigp^2
  switch upl of 
     1: sigc2=dcdn^2*sign^2+dcdp^2*sigp^2 ;tot
     2: sigc2=dcdn^2*sign^2+dcdp^2*sigp^2+dcdnul^2*siguln^2+dcdpul^2*sigulp^2 ;fl
     3: sigc2=dcdnul^2*siguln^2+dcdpul^2*sigulp^2 ;upl
  endswitch
  
  sigx=sqrt(dxdf^2*sigf^2+dxdc^2*sigc2)
  
;  stop
  return,sigx
end 
  
pro estimate_fspc_dat,fswtstr,wtstr,pcstr,str,fspcstr
  
  make_flarespec_outs_struct,fspcstr
  
  ;;calculating total counts under power-law pc decay
  if wtstr.tstart lt pcstr.tstart then begin
     norm=pcstr.decay_norm
     pow=pcstr.decay_pow
  endif else begin
     norm=pcstr.rise_norm
     pow=pcstr.rise_pow
  endelse 
  pcctstot=(10^norm[0])/(pow[0]+1.)*(pcstr.tstop^(pow[0]+1.)-pcstr.tstart^(pow[0]+1.))
  ulcts=(10^pcstr.ul_norm[0])/(pcstr.ul_pow[0]+1.)*(pcstr.tstop^(pcstr.ul_pow[0]+1.)-pcstr.tstart^(pcstr.ul_pow[0]+1.))
  pccts=pcctstot-ulcts ;counts in upl subtracted flare
  delt=(pcstr.tstop-pcstr.tstart)
  pcctr=pccts/delt
  totpcctr=pcctstot/delt
  ulctr=ulcts/delt
  
  ; err=calc_par_err(f,r,c,sigf,sigr,n,sign,p,sigp,uln,siguln,ulp,sigulp,t1,t0)
  
  model=['pow','cutpow','grbm','bb']
  comp=['','_fl','_upl']
  comprs=[['totpcctr','pcctr','ulctr'],$
          ['','',''],$
          ['pcctstot','pccts','ulcts']]
  abs=['','_unabs']
  ff=['flux','rate','fluence']
  for m=0,3 do begin ;model
     for i=0,2 do begin ;component
        if i eq 0 then nf=1 else nf=2
        for j=0,1 do begin ;abs
           for f=0,nf do begin ;flux/rate/fluence
              if f ne 1 then begin 
                 com='fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=fswtstr.'+model[m]+'_flux'+comp[i]+'/fswtstr.'+model[m]+'_rate'+comp[i]+'*'+comprs[i,f]
                 tmp=execute(com)
                 print,com,tmp
                 com2='fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=calc_par_err(fswtstr.'+model[m]+abs[j]+'_flux'+comp[i]+',fswtstr.'+model[m]+'_rate'+comp[i]+','+comprs[i,f]+',fswtstr.'+model[m]+abs[j]+'_flux'+comp[i]+'_err,norm[0],norm[1],pow[0],pow[1],pcstr.ul_norm[0],pcstr.ul_norm[1],pcstr.ul_pow[0],pcstr.ul_pow[1],pcstr.tstart,pcstr.tstop,'+ntostr(i+1)+','+ntostr(f)+')'
                 tmp2=execute(com2)
                 print,com2,tmp2
              endif else begin
;                 com='fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=fswtstr.'+model[m]+'_rate'+comp[i]+'+fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]
                 com='fspcstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'='+comprs[0,0];[i,0]
                 tmp=execute(com)
                 print,com,tmp
;                 stop
              endelse 
                 
              if tmp eq 0 or tmp2 eq 0 then stop
           endfor 
        endfor 
     endfor 
     
  endfor 
  
  return 
end 

pro write_flarespec_out,str,fnum=fnum
  
  file=''
  read,prompt='Flarespec Output file name (Hit return for default)? ',file
  if file eq '' then file='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt'
  print,'Writing out '+file
  
  openw,lun,file,/get_lun
  
  printf,lun,'nH_upl '+ntostr(str.pow_nH_upl)
  printf,lun,'PhInd_upl '+ntostr(str.pow_PhInd_upl)
  printf,lun,'norm_upl '+ntostr(str.pow_norm_upl)
  printf,lun,'nH_fl '+ntostr(str.pow_nH_fl)
  printf,lun,'PhInd_fl '+ntostr(str.pow_PhInd_fl)
  printf,lun,'norm_fl '+ntostr(str.pow_norm_fl)
  printf,lun,'chisq/dof '+ntostr(str.pow_chisqdof)
  printf,lun,'dof '+ntostr(str.pow_dof)
  printf,lun,'flux '+ntostr(str.pow_flux)
  printf,lun,'flux_err '+ntostrarr(str.pow_flux_err)
  printf,lun,'rate '+ntostr(str.pow_rate)
  printf,lun,'flux_fl '+ntostr(str.pow_flux_fl)
  printf,lun,'flux_fl_err '+ntostrarr(str.pow_flux_fl_err)
  printf,lun,'rate_fl '+ntostr(str.pow_rate_fl)
  printf,lun,'unabsflux '+ntostr(str.pow_unabs_flux)
  printf,lun,'unabsflux_err '+ntostrarr(str.pow_unabs_flux_err)
  printf,lun,'unabsrate '+ntostr(str.pow_unabs_rate)
  printf,lun,'unabsflux_fl '+ntostr(str.pow_unabs_flux_fl)
  printf,lun,'unabsflux_fl_err '+ntostrarr(str.pow_unabs_flux_fl_err)
  printf,lun,'unabsrate_fl '+ntostr(str.pow_unabs_rate_fl)
  printf,lun,'flux_upl '+ntostr(str.pow_flux_upl)
  printf,lun,'flux_upl_err '+ntostrarr(str.pow_flux_upl_err)
  printf,lun,'rate_upl '+ntostr(str.pow_rate_upl)
  printf,lun,'unabsflux_upl '+ntostr(str.pow_unabs_flux_upl)
  printf,lun,'unabsflux_upl_err '+ntostrarr(str.pow_unabs_flux_upl_err)
  printf,lun,'unabsrate_upl '+ntostr(str.pow_unabs_rate_upl)
  printf,lun,'nH_fl_err '+ntostrarr(str.pow_nH_fl_err)
  printf,lun,'PhInd_fl_err '+ntostrarr(str.pow_PhInd_fl_err)
  printf,lun,'norm_fl_err '+ntostrarr(str.pow_norm_fl_err)
  printf,lun,'fluence_fl '+ntostr(str.pow_fluence_fl)
  printf,lun,'fluence_fl_err '+ntostrarr(str.pow_fluence_fl_err)
  printf,lun,'fluence_upl '+ntostr(str.pow_fluence_upl)
  printf,lun,'fluence_upl_err '+ntostrarr(str.pow_fluence_upl_err)
  printf,lun,'unabs_fluence_fl '+ntostr(str.pow_unabs_fluence_fl)
  printf,lun,'unabs_fluence_fl_err '+ntostrarr(str.pow_unabs_fluence_fl_err)
  printf,lun,'unabs_fluence_upl '+ntostr(str.pow_unabs_fluence_upl)
  printf,lun,'unabs_fluence_upl_err '+ntostrarr(str.pow_unabs_fluence_upl_err)
  printf,lun
  printf,lun,'nH_upl '+ntostr(str.cutpow_nH_upl)
  printf,lun,'PhInd_upl '+ntostr(str.cutpow_PhInd_upl)
  printf,lun,'norm_upl '+ntostr(str.cutpow_norm_upl)
  printf,lun,'nH_fl '+ntostr(str.cutpow_nH_fl)
  printf,lun,'PhInd_fl '+ntostr(str.cutpow_PhInd_fl)
  printf,lun,'HighE_fl '+ntostr(str.cutpow_HighE_fl)
  printf,lun,'norm_fl '+ntostr(str.cutpow_norm_fl)
  printf,lun,'chisq/dof '+ntostr(str.cutpow_chisqdof)
  printf,lun,'dof '+ntostr(str.cutpow_dof)
  printf,lun,'flux '+ntostr(str.cutpow_flux)
  printf,lun,'flux_err '+ntostrarr(str.cutpow_flux_err)
  printf,lun,'rate '+ntostr(str.cutpow_rate)
  printf,lun,'flux_fl '+ntostr(str.cutpow_flux_fl)
  printf,lun,'flux_fl_err '+ntostrarr(str.cutpow_flux_fl_err)
  printf,lun,'rate_fl '+ntostr(str.cutpow_rate_fl)
  printf,lun,'unabsflux '+ntostr(str.cutpow_unabs_flux)
  printf,lun,'unabsflux_err '+ntostrarr(str.cutpow_unabs_flux_err)
  printf,lun,'unabsrate '+ntostr(str.cutpow_unabs_rate)
  printf,lun,'unabsflux_fl '+ntostr(str.cutpow_unabs_flux_fl)
  printf,lun,'unabsflux_fl_err '+ntostrarr(str.cutpow_unabs_flux_fl_err)
  printf,lun,'unabsrate_fl '+ntostr(str.cutpow_unabs_rate_fl)
  printf,lun,'flux_upl '+ntostr(str.cutpow_flux_upl)
  printf,lun,'flux_upl_err '+ntostrarr(str.cutpow_flux_upl_err)
  printf,lun,'rate_upl '+ntostr(str.cutpow_rate_upl)
  printf,lun,'unabsflux_upl '+ntostr(str.cutpow_unabs_flux_upl)
  printf,lun,'unabsflux_upl_err '+ntostrarr(str.cutpow_unabs_flux_upl_err)
  printf,lun,'unabsrate_upl '+ntostr(str.cutpow_unabs_rate_upl)
  printf,lun,'nH_fl_err '+ntostrarr(str.cutpow_nH_fl_err)
  printf,lun,'PhInd_fl_err '+ntostrarr(str.cutpow_PhInd_fl_err)
  printf,lun,'HighE_fl_err '+ntostrarr(str.cutpow_HighE_fl_err)
  printf,lun,'norm_fl_err '+ntostrarr(str.cutpow_norm_fl_err)
  printf,lun,'fluence_fl '+ntostr(str.cutpow_fluence_fl)
  printf,lun,'fluence_fl_err '+ntostrarr(str.cutpow_fluence_fl_err)
  printf,lun,'fluence_upl '+ntostr(str.cutpow_fluence_upl)
  printf,lun,'fluence_upl_err '+ntostrarr(str.cutpow_fluence_upl_err)
  printf,lun,'unabs_fluence_fl '+ntostr(str.cutpow_unabs_fluence_fl)
  printf,lun,'unabs_fluence_fl_err '+ntostrarr(str.cutpow_unabs_fluence_fl_err)
  printf,lun,'unabs_fluence_upl '+ntostr(str.cutpow_unabs_fluence_upl)
  printf,lun,'unabs_fluence_upl_err '+ntostrarr(str.cutpow_unabs_fluence_upl_err)
  printf,lun  
  printf,lun,'nH_upl '+ntostr(str.grbm_nH_upl)
  printf,lun,'PhInd_upl '+ntostr(str.grbm_PhInd_upl)
  printf,lun,'norm_upl '+ntostr(str.grbm_norm_upl)
  printf,lun,'nH_fl '+ntostr(str.grbm_nH_fl)
  printf,lun,'alpha_fl '+ntostr(str.grbm_alpha_fl)
  printf,lun,'beta_fl '+ntostr(str.grbm_beta_fl)
  printf,lun,'T_fl '+ntostr(str.grbm_T_fl)
  printf,lun,'norm_fl '+ntostr(str.grbm_norm_fl)
  printf,lun,'chisq/dof '+ntostr(str.grbm_chisqdof)
  printf,lun,'dof '+ntostr(str.grbm_dof)
  printf,lun,'flux '+ntostr(str.grbm_flux)
  printf,lun,'flux_err '+ntostrarr(str.grbm_flux_err)
  printf,lun,'rate '+ntostr(str.grbm_rate)
  printf,lun,'flux_fl '+ntostr(str.grbm_flux_fl)
  printf,lun,'flux_fl_err '+ntostrarr(str.grbm_flux_fl_err)
  printf,lun,'rate_fl '+ntostr(str.grbm_rate_fl)
  printf,lun,'unabsflux '+ntostr(str.grbm_unabs_flux)
  printf,lun,'unabsflux_err '+ntostrarr(str.grbm_unabs_flux_err)
  printf,lun,'unabsrate '+ntostr(str.grbm_unabs_rate)
  printf,lun,'unabsflux_fl '+ntostr(str.grbm_unabs_flux_fl)
  printf,lun,'unabsflux_fl_err '+ntostrarr(str.grbm_unabs_flux_fl_err)
  printf,lun,'unabsrate_fl '+ntostr(str.grbm_unabs_rate_fl)
  printf,lun,'flux_upl '+ntostr(str.grbm_flux_upl)
  printf,lun,'flux_upl_err '+ntostrarr(str.grbm_flux_upl_err)
  printf,lun,'rate_upl '+ntostr(str.grbm_rate_upl)
  printf,lun,'unabsflux_upl '+ntostr(str.grbm_unabs_flux_upl)
  printf,lun,'unabsflux_upl_err '+ntostrarr(str.grbm_unabs_flux_upl_err)
  printf,lun,'unabsrate_upl '+ntostr(str.grbm_unabs_rate_upl)
  printf,lun,'nH_fl_err '+ntostrarr(str.grbm_nH_fl_err)
  printf,lun,'alpha_fl_err '+ntostrarr(str.grbm_alpha_fl_err)
  printf,lun,'beta_fl_err '+ntostrarr(str.grbm_beta_fl_err)
  printf,lun,'temp_fl_err '+ntostrarr(str.grbm_temp_fl_err)
  printf,lun,'norm_fl_err '+ntostrarr(str.grbm_norm_fl_err)
  printf,lun,'fluence_fl '+ntostr(str.grbm_fluence_fl)
  printf,lun,'fluence_fl_err '+ntostrarr(str.grbm_fluence_fl_err)
  printf,lun,'fluence_upl '+ntostr(str.grbm_fluence_upl)
  printf,lun,'fluence_upl_err '+ntostrarr(str.grbm_fluence_upl_err)
  printf,lun,'unabs_fluence_fl '+ntostr(str.grbm_unabs_fluence_fl)
  printf,lun,'unabs_fluence_fl_err '+ntostrarr(str.grbm_unabs_fluence_fl_err)
  printf,lun,'unabs_fluence_upl '+ntostr(str.grbm_unabs_fluence_upl)
  printf,lun,'unabs_fluence_upl_err '+ntostrarr(str.grbm_unabs_fluence_upl_err)
;  printf,lun,'beta_fl_err '+ntostr(str.grbm_beta_fl_err)
;  printf,lun,'temp_fl_err '+ntostr(str.grbm_temp_fl_err)
;  printf,lun,'fluence_fl '+ntostr(str.grbm_fluence_fl)
;  printf,lun,'fluence_fl_err '+ntostr(str.grbm_fluence_fl_err)
;  printf,lun,'fluence_upl '+ntostr(str.grbm_fluence_upl)
;  printf,lun,'fluence_upl_err '+ntostr(str.grbm_fluence_upl_err)
;  printf,lun,'unabs_fluence_fl '+ntostr(str.grbm_unabs_fluence_fl)
;  printf,lun,'unabs_fluence_fl_err '+ntostr(str.grbm_unabs_fluence_fl_err)
;  printf,lun,'unabs_fluence_upl '+ntostr(str.grbm_unabs_fluence_upl)
;  printf,lun,'unabs_fluence_upl_err '+ntostr(str.grbm_unabs_fluence_upl_err)
  printf,lun  
  printf,lun,'nH_upl '+ntostr(str.bb_nH_upl)
  printf,lun,'PhInd_upl '+ntostr(str.bb_PhInd_upl)
  printf,lun,'norm_upl '+ntostr(str.bb_norm_upl)
  printf,lun,'nH_fl '+ntostr(str.bb_nH_fl)
  printf,lun,'PhInd_fl '+ntostr(str.bb_PhInd_fl)
  printf,lun,'normp_fl '+ntostr(str.bb_normp_fl)
  printf,lun,'kT_fl '+ntostr(str.bb_kT_fl)
  printf,lun,'normbb_fl '+ntostr(str.bb_normbb_fl)
  printf,lun,'chisq/dof '+ntostr(str.bb_chisqdof)
  printf,lun,'dof '+ntostr(str.bb_dof)
  printf,lun,'flux '+ntostr(str.bb_flux)
  printf,lun,'flux_err '+ntostrarr(str.bb_flux_err)
  printf,lun,'rate '+ntostr(str.bb_rate)
  printf,lun,'flux_fl '+ntostr(str.bb_flux_fl)
  printf,lun,'flux_fl_err '+ntostrarr(str.bb_flux_fl_err)
  printf,lun,'rate_fl '+ntostr(str.bb_rate_fl)
  printf,lun,'unabsflux '+ntostr(str.bb_unabs_flux)
  printf,lun,'unabsflux_err '+ntostrarr(str.bb_unabs_flux_err)
  printf,lun,'unabsrate '+ntostr(str.bb_unabs_rate)
  printf,lun,'unabsflux_fl '+ntostr(str.bb_unabs_flux_fl)
  printf,lun,'unabsflux_fl_err '+ntostrarr(str.bb_unabs_flux_fl_err)
  printf,lun,'unabsrate_fl '+ntostr(str.bb_unabs_rate_fl)
  printf,lun,'flux_upl '+ntostr(str.bb_flux_upl)
  printf,lun,'flux_upl_err '+ntostrarr(str.bb_flux_upl_err)
  printf,lun,'rate_upl '+ntostr(str.bb_rate_upl)
  printf,lun,'unabsflux_upl '+ntostr(str.bb_unabs_flux_upl)
  printf,lun,'unabsflux_upl_err '+ntostrarr(str.bb_unabs_flux_upl_err)
  printf,lun,'unabsrate_upl '+ntostr(str.bb_unabs_rate_upl)
  printf,lun,'nH_fl_err '+ntostrarr(str.bb_nH_fl_err)
  printf,lun,'PhInd_fl_err '+ntostrarr(str.bb_PhInd_fl_err)
  printf,lun,'normp_fl_err '+ntostrarr(str.bb_normp_fl_err)
  printf,lun,'normbb_fl_err '+ntostrarr(str.bb_normbb_fl_err)
  printf,lun,'kT_fl_err '+ntostrarr(str.bb_kT_fl_err)
  printf,lun,'fluence_fl '+ntostr(str.bb_fluence_fl)
  printf,lun,'fluence_fl_err '+ntostrarr(str.bb_fluence_fl_err)
  printf,lun,'fluence_upl '+ntostr(str.bb_fluence_upl)
  printf,lun,'fluence_upl_err '+ntostrarr(str.bb_fluence_upl_err)
  printf,lun,'unabs_fluence_fl '+ntostr(str.bb_unabs_fluence_fl)
  printf,lun,'unabs_fluence_fl_err '+ntostrarr(str.bb_unabs_fluence_fl_err)
  printf,lun,'unabs_fluence_upl '+ntostr(str.bb_unabs_fluence_upl)
  printf,lun,'unabs_fluence_upl_err '+ntostrarr(str.bb_unabs_fluence_upl_err)
  
  close,lun,/file  
  
  return
end 

pro read_flarespec_outs,file,str
  
  make_flarespec_outs_struct,str
  
  openr,lun,file,/get_lun
  i=0
  while not eof(lun) do begin
     
     line=readline(lun)
     nline=n_elements(line)
     if nline eq 2 then str.(i)=line[1]
     if nline eq 3 then str.(i)=line[1:2]
     if nline gt 1 then i=i+1
     
  endwhile
  
  
  close,lun,/file
  return
end 

pro make_flarespec_outs_struct,struct
  
  d2=dblarr(2)
  struct=create_struct('pow_nH_upl',0d,$
                       'pow_PhInd_upl',0d,$
                       'pow_norm_upl',0d,$
                       'pow_nH_fl',0d,$
                       'pow_PhInd_fl',0d,$
                       'pow_norm_fl',0d,$
                       'pow_chisqdof',0d,$
                       'pow_dof',0L,$
                       'pow_flux',0d,$
                       'pow_flux_err',d2,$
                       'pow_rate',0d,$
                       'pow_flux_fl',0d,$
                       'pow_flux_fl_err',d2,$
                       'pow_rate_fl',0d,$
                       'pow_unabs_flux',0d,$
                       'pow_unabs_flux_err',d2,$
                       'pow_unabs_rate',0d,$
                       'pow_unabs_flux_fl',0d,$
                       'pow_unabs_flux_fl_err',d2,$
                       'pow_unabs_rate_fl',0d,$
                       'pow_flux_upl',0d,$
                       'pow_flux_upl_err',d2,$
                       'pow_rate_upl',0d,$
                       'pow_unabs_flux_upl',0d,$
                       'pow_unabs_flux_upl_err',d2,$
                       'pow_unabs_rate_upl',0d,$
                       'pow_nH_fl_err',d2,$
                       'pow_PhInd_fl_err',d2,$
                       'pow_norm_fl_err',d2,$
                       'pow_fluence_fl',0d,$
                       'pow_fluence_fl_err',d2,$
                       'pow_fluence_upl',0d,$
                       'pow_fluence_upl_err',d2,$
                       'pow_unabs_fluence_fl',0d,$
                       'pow_unabs_fluence_fl_err',d2,$
                       'pow_unabs_fluence_upl',0d,$
                       'pow_unabs_fluence_upl_err',d2,$
                       $
                       'cutpow_nH_upl',0d,$
                       'cutpow_PhInd_upl',0d,$
                       'cutpow_norm_upl',0d,$
                       'cutpow_nH_fl',0d,$
                       'cutpow_PhInd_fl',0d,$
                       'cutpow_HighE_fl',0d,$
                       'cutpow_norm_fl',0d,$
                       'cutpow_chisqdof',0d,$
                       'cutpow_dof',0L,$
                       'cutpow_flux',0d,$
                       'cutpow_flux_err',d2,$
                       'cutpow_rate',0d,$
                       'cutpow_flux_fl',0d,$
                       'cutpow_flux_fl_err',d2,$
                       'cutpow_rate_fl',0d,$
                       'cutpow_unabs_flux',0d,$
                       'cutpow_unabs_flux_err',d2,$
                       'cutpow_unabs_rate',0d,$
                       'cutpow_unabs_flux_fl',0d,$
                       'cutpow_unabs_flux_fl_err',d2,$
                       'cutpow_unabs_rate_fl',0d,$
                       'cutpow_flux_upl',0d,$
                       'cutpow_flux_upl_err',d2,$
                       'cutpow_rate_upl',0d,$
                       'cutpow_unabs_flux_upl',0d,$
                       'cutpow_unabs_flux_upl_err',d2,$
                       'cutpow_unabs_rate_upl',0d,$
                       'cutpow_nH_fl_err',d2,$
                       'cutpow_PhInd_fl_err',d2,$
                       'cutpow_HighE_fl_err',d2,$
                       'cutpow_norm_fl_err',d2,$
                       'cutpow_fluence_fl',0d,$
                       'cutpow_fluence_fl_err',d2,$
                       'cutpow_fluence_upl',0d,$
                       'cutpow_fluence_upl_err',d2,$
                       'cutpow_unabs_fluence_fl',0d,$
                       'cutpow_unabs_fluence_fl_err',d2,$
                       'cutpow_unabs_fluence_upl',0d,$
                       'cutpow_unabs_fluence_upl_err',d2,$
                       $                     
                       'grbm_nH_upl',0d,$
                       'grbm_PhInd_upl',0d,$
                       'grbm_norm_upl',0d,$
                       'grbm_nH_fl',0d,$
                       'grbm_alpha_fl',0d,$
                       'grbm_beta_fl',0d,$
                       'grbm_T_fl',0d,$
                       'grbm_norm_fl',0d,$
                       'grbm_chisqdof',0d,$
                       'grbm_dof',0L,$
                       'grbm_flux',0d,$
                       'grbm_flux_err',d2,$
                       'grbm_rate',0d,$
                       'grbm_flux_fl',0d,$
                       'grbm_flux_fl_err',d2,$
                       'grbm_rate_fl',0d,$
                       'grbm_unabs_flux',0d,$
                       'grbm_unabs_flux_err',d2,$
                       'grbm_unabs_rate',0d,$
                       'grbm_unabs_flux_fl',0d,$
                       'grbm_unabs_flux_fl_err',d2,$
                       'grbm_unabs_rate_fl',0d,$
                       'grbm_flux_upl',0d,$
                       'grbm_flux_upl_err',d2,$
                       'grbm_rate_upl',0d,$
                       'grbm_unabs_flux_upl',0d,$
                       'grbm_unabs_flux_upl_err',d2,$
                       'grbm_unabs_rate_upl',0d,$
                       'grbm_nH_fl_err',d2,$
                       'grbm_alpha_fl_err',d2,$
                       'grbm_beta_fl_err',d2,$;
                       'grbm_temp_fl_err',d2,$;                     
                       'grbm_norm_fl_err',d2,$
                       'grbm_fluence_fl',0d,$
                       'grbm_fluence_fl_err',d2,$
                       'grbm_fluence_upl',0d,$
                       'grbm_fluence_upl_err',d2,$ 
                       'grbm_unabs_fluence_fl',0d,$
                       'grbm_unabs_fluence_fl_err',d2,$
                       'grbm_unabs_fluence_upl',0d,$
                       'grbm_unabs_fluence_upl_err',d2,$
;                       'grbm_fluence_fl',0d,$
;                       'grbm_fluence_fl_err',d2,$
;                       'grbm_fluence_upl',0d,$
;                       'grbm_fluence_upl_err',d2,$
;                       'grbm_unabs_fluence_fl',0d,$
;                       'grbm_unabs_fluence_fl_err',d2,$
;                       'grbm_unabs_fluence_upl',0d,$
;                       'grbm_unabs_fluence_upl_err',d2,$
                       $                     
                       'bb_nH_upl',0d,$
                       'bb_PhInd_upl',0d,$
                       'bb_norm_upl',0d,$
                       'bb_nH_fl',0d,$
                       'bb_PhInd_fl',0d,$
                       'bb_normp_fl',0d,$
                       'bb_kT_fl',0d,$
                       'bb_normbb_fl',0d,$
                       'bb_chisqdof',0d,$
                       'bb_dof',0L,$
                       'bb_flux',0d,$
                       'bb_flux_err',d2,$
                       'bb_rate',0d,$
                       'bb_flux_fl',0d,$
                       'bb_flux_fl_err',d2,$
                       'bb_rate_fl',0d,$
                       'bb_unabs_flux',0d,$
                       'bb_unabs_flux_err',d2,$
                       'bb_unabs_rate',0d,$
                       'bb_unabs_flux_fl',0d,$
                       'bb_unabs_flux_fl_err',d2,$
                       'bb_unabs_rate_fl',0d,$
                       'bb_flux_upl',0d,$
                       'bb_flux_upl_err',d2,$
                       'bb_rate_upl',0d,$
                       'bb_unabs_flux_upl',0d,$
                       'bb_unabs_flux_upl_err',d2,$
                       'bb_unabs_rate_upl',0d,$
                       'bb_nH_fl_err',d2,$
                       'bb_PhInd_fl_err',d2,$
                       'bb_normp_fl_err',d2,$
                       'bb_normbb_fl_err',d2,$
                       'bb_kT_fl_err',d2,$
                       'bb_fluence_fl',0d,$
                       'bb_fluence_fl_err',d2,$
                       'bb_fluence_upl',0d,$
                       'bb_fluence_upl_err',d2,$
                       'bb_unabs_fluence_fl',0d,$
                       'bb_unabs_fluence_fl_err',d2,$
                       'bb_unabs_fluence_upl',0d,$
                       'bb_unabs_fluence_upl_err',d2)
  return
end 

pro calc_signif,str,src,bg,exp,expt,time,cts,wtstr,pcstr,pcstr2
  
   q=where(time gt str.tstart and time lt str.tstop)
   ulcts=10d^str.ul_norm[0]/(str.ul_pow[0]+1.)*(str.tstop^(str.ul_pow[0]+1.)-str.tstart^(str.ul_pow[0]+1.))*(total(expt[q])/(str.tstop-str.tstart))
   tcts=total(src[q])
   
   str.exp_fact=total(exp[q]*(src[q]-bg[q]))/total(src[q]-bg[q])
   
   pufacts=1./(((src[q]-bg[q])/expt[q])/cts[q]/exp[q])
   pufacts=cts[q]/((src[q]-bg[q])/expt[q])/exp[q]
   str.pufact=total(pufacts*(src[q]-bg[q]))/total(src[q]-bg[q])
   
   str.signif=(tcts-ulcts/str.exp_fact/str.pufact)/sqrt(tcts+ulcts/str.exp_fact/str.pufact)
   
   wt=where(time gt wtstr.tstart and time lt wtstr.tstop)
   pc=where(time gt pcstr.tstart and time lt pcstr.tstop)
   wtulcts=10d^wtstr.ul_norm[0]/(wtstr.ul_pow[0]+1.)*(wtstr.tstop^(wtstr.ul_pow[0]+1.)-wtstr.tstart^(wtstr.ul_pow[0]+1.))*(total(expt[wt])/(wtstr.tstop-wtstr.tstart))
   pculcts=10d^pcstr.ul_norm[0]/(pcstr.ul_pow[0]+1.)*(pcstr.tstop^(pcstr.ul_pow[0]+1.)-pcstr.tstart^(pcstr.ul_pow[0]+1.))*(total(expt[pc])/(pcstr.tstop-pcstr.tstart))
   if n_elements(pcstr2) gt 0 then begin
      pc2=where(time gt pcstr2.tstart and time lt pcstr2.tstop)
      pculcts2=10d^pcstr2.ul_norm[0]/(pcstr2.ul_pow[0]+1.)*(pcstr2.tstop^(pcstr2.ul_pow[0]+1.)-pcstr2.tstart^(pcstr2.ul_pow[0]+1.))*(total(expt[pc2])/(pcstr2.tstop-pcstr2.tstart))
;      pculcts=pculcts+pculcts2
   endif 

   print,'                         wt        pc        comb'
;   print,'    Flare events =      '+ntostrarr(fix([wtcts,pccts,tcts]),'       ')
   print,'    extrap upl events = '+sigfig(wtulcts,4)+'      '+sigfig(pculcts,4)+'    '+sigfig(ulcts,4)
   print,'    significance =      '+sigfig(wtstr.signif,4)+'      '+sigfig(pcstr.signif,4)+'    '+sigfig(str.signif,4)
   
   
   return
end
pro write_flareout_file,str,latespec=latespec,fnum=fnum
  
  file=''
  read,prompt='Flare Out output file name (hit return for default)? ',file
  if file eq '' then file='flare'+ntostr(fnum)+'/flare.out'
  print,'Writing out '+file
  
  openw,lun,file,/get_lun
  s=' '
  printf,lun,'TSTART '+ntostr(str.tstart)
  printf,lun,'TSTOP '+ntostr(str.tstop)
  printf,lun,'EXP_FACT '+ntostr(str.exp_fact)
  printf,lun,'PUFACT '+ntostr(str.pufact)
  printf,lun,'UL_TSTARTS '+ntostrarr(str.ul_tstarts)
  printf,lun,'UL_TSTOPS '+ntostrarr(str.ul_tstops)
  printf,lun,'UL_NORM '+ntostrarr(str.ul_norm)
  printf,lun,'UL_POW '+ntostrarr(str.ul_pow)
  printf,lun,'UL_CHISQ/DOF '+ntostr(str.ul_chisqdof)
  printf,lun,'UL_DOF '+ntostr(str.ul_dof)
  printf,lun,'RISE_NORM '+ntostrarr(str.rise_norm)
  printf,lun,'RISE_POW '+ntostrarr(str.rise_pow)
  printf,lun,'RISE_CHISQ/DOF '+ntostr(str.rise_chisqdof)
  printf,lun,'RISE_DOF '+ntostr(str.rise_dof)
  printf,lun,'DECAY_NORM '+ntostrarr(str.decay_norm)
  printf,lun,'DECAY_POW '+ntostrarr(str.decay_pow)
  printf,lun,'DECAY_CHISQ/DOF '+ntostr(str.decay_chisqdof)
  printf,lun,'DECAY_DOF '+ntostr(str.decay_dof)
  printf,lun,'RESET_FLAG '+ntostrarr(str.reset_flag)
  printf,lun,'INTERCEPT_TPEAK '+ntostr(str.intercept_tpeak)
  printf,lun,'INTERCEPT_PEAK '+ntostr(str.intercept_peak)
  printf,lun,'INTERCEPT_INCREASE '+ntostr(str.intercept_increase)
  printf,lun,'MAX_TPEAK '+ntostr(str.max_tpeak)
  printf,lun,'MAX_PEAK '+ntostr(str.max_peak)
  printf,lun,'MAX_INCREASE '+ntostr(str.max_increase)
  printf,lun,'SIGNIF '+ntostr(str.signif)
  printf,lun,'ULRATIO '+ntostr(str.ulratio)  
  if keyword_set(latespec) then begin
     printf,lun,'UL_MEAN_TIME '+ntostr(str.ul_mean_time)
     printf,lun,'UL_NORM2 '+ntostrarr(str.ul_norm2)
     printf,lun,'UL_POW2 '+ntostrarr(str.ul_pow2)
     printf,lun,'UL_CHISQ2/DOF2 '+ntostr(str.ul_chisqdof2)
     printf,lun,'UL_DOF2 '+ntostr(str.ul_dof2)
  endif 
  close,lun,/file
  
  return
end 

pro read_flareout_file,file,flstr,latespec=latespec


  openr,lun,file,/get_lun
  par=''
  value=0d
  err=''
  tstarts='' & tstops=''
  while not eof(lun) do begin
     line=readline(lun,delim=' ')
     w=where(line eq '',nw)
     if nw gt 0 then begin
        nnw=where(line ne '')
        line=line[nnw]
     endif 
     par=[par,line[0]]
     value=[value,line[1]]
     if n_elements(line) eq 3 and line[0] ne 'UL_TSTARTS' and line[0] ne 'UL_TSTOPS' then err=[err,line[2]] else err=[err,'']
     if line[0] eq 'UL_TSTARTS' then tstarts=line[1:*]
     if line[0] eq 'UL_TSTOPS' then tstops=line[1:*]
     
  endwhile
  
  close,lun,/file
  n=n_elements(tstarts)
  make_flareout_struct,flstr,n
  
  par=par[1:*]
  value=value[1:*]
  err=err[1:*]
  
  flstr.tstart=value[0]
  flstr.tstop=value[1]
  flstr.exp_fact=value[2]
  flstr.pufact=value[3]
  flstr.ul_tstarts=tstarts
  flstr.ul_tstops=tstops
  flstr.ul_norm=[value[6],err[6]]
  flstr.ul_pow=[value[7],err[7]]
  flstr.ul_chisqdof=value[8]
  flstr.ul_dof=value[9]
  flstr.rise_norm=[value[10],err[10]]
  flstr.rise_pow=[value[11],err[11]]
  flstr.rise_chisqdof=value[12]
  flstr.rise_dof=value[13]
  flstr.decay_norm=[value[14],err[14]]
  flstr.decay_pow=[value[15],err[15]]
  flstr.decay_chisqdof=value[16]
  flstr.decay_dof=value[17]
  flstr.reset_flag=[value[18],err[18]]
  flstr.intercept_tpeak=value[19]
  flstr.intercept_peak=value[20]
  flstr.intercept_increase=value[21]
  flstr.max_tpeak=value[22]
  flstr.max_peak=value[23]
  flstr.max_increase=value[24]
  flstr.signif=value[25]
  flstr.ulratio=value[26]
  if n_elements(value) gt 27 then begin 
     flstr.ul_mean_time=value[27]
     flstr.ul_norm2=[value[28],err[28]]
     flstr.ul_pow2=[value[29],err[29]]
     flstr.ul_chisqdof2=value[30]
     flstr.ul_dof2=value[31]
     latespec=1
  endif
  
  
  return
end 
pro make_flareout_struct,struct,n
  
  ;deal with errors & many gtis
  derr=dblarr(2)
  if n eq 1 then dn=0d else dn=dblarr(n)
  struct=create_struct('TSTART',0d,$
                       'TSTOP',0d,$
                       'EXP_FACT',0d,$
                       'PUFACT',0d,$
                       'UL_TSTARTS',dn,$
                       'UL_TSTOPS',dn,$
                       'UL_NORM',derr,$
                       'UL_POW',derr,$
                       'UL_CHISQDOF',0d,$
                       'UL_DOF',0L,$
                       'RISE_NORM',derr,$
                       'RISE_POW',derr,$
                       'RISE_CHISQDOF',0d,$
                       'RISE_DOF',0L,$
                       'DECAY_NORM',derr,$
                       'DECAY_POW',derr,$
                       'DECAY_CHISQDOF',0d,$
                       'DECAY_DOF',0L,$
                       'RESET_FLAG',intarr(2),$
                       'INTERCEPT_TPEAK',0d,$
                       'INTERCEPT_PEAK',0d,$
                       'INTERCEPT_INCREASE',0d,$
                       'MAX_TPEAK',0d,$
                       'MAX_PEAK',0d,$
                       'MAX_INCREASE',0d,$
                       'SIGNIF',0d,$
                       'ULRATIO',0d,$
                       'UL_MEAN_TIME',0d,$
                       'UL_NORM2',derr,$
                       'UL_POW2',derr,$
                       'UL_CHISQDOF2',0d,$
                       'UL_DOF2',0L)
  return
end 

pro combine_wtpc_flareout,wtfout,pcfout,fswtfile,fspcfile,fnum=fnum,twopc=twopc
  
  if n_elements(fnum) eq 0 then fnum=1
  
  if keyword_set(twopc) then twopc=1 else twopc=0
  
  if n_elements(wtfout) eq 0 then begin
     wtfout='flare'+ntostr(fnum)+'_wt/flare'+ntostr(fnum)+'_wt.out'
     pcfout='flare'+ntostr(fnum)+'_pc/flare'+ntostr(fnum)+'_pc.out'
     fswtfile='flare'+ntostr(fnum)+'_wt/fit_result_flare.pha.dat.rfmt'
     fspcfile='flare'+ntostr(fnum)+'_pc/fit_result_flare.pha.dat.rfmt'
     fsuplfile='flare'+ntostr(fnum)+'_wt/fit_result_upl.pha.dat.rfmt'
     if twopc then begin
        pcfout='flare'+ntostr(fnum)+'_pc_1/flare'+ntostr(fnum)+'_pc_1.out'
        fspcfile='flare'+ntostr(fnum)+'_pc_1/fit_result_flare.pha.dat.rfmt'
        pcfout2='flare'+ntostr(fnum)+'_pc_2/flare'+ntostr(fnum)+'_pc_2.out'
        fspcfile2='flare'+ntostr(fnum)+'_pc_2/fit_result_flare.pha.dat.rfmt'
     endif 
     if not exist('flare'+ntostr(fnum)) then spawn,'mkdir flare'+ntostr(fnum)
  endif 
  
  ;;extract tstart & tstop
  ;;copy all wt info into new flare.out
  ;;recalc exp_fact?, pufact?, signif
  ;;adjust fluence & mean flux by ratio of counts in wt/pc
  
  file='lc_out.txt'
  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  
  print,'Reading fit_flares output files'
  read_flareout_file,wtfout,wtstr,latespec=latespec
  read_flareout_file,pcfout,pcstr,latespec=latespec
  if keyword_set(twopc) then read_flareout_file,pcfout2,pcstr2,latespec=latespec
  
  n=n_elements(wtstr.ul_tstarts)
  make_flareout_struct,str,n
  ;;combine tstart & tstop
  if not twopc then begin 
     str.tstart=min([wtstr.tstart,pcstr.tstart])
     str.tstop=max([wtstr.tstop,pcstr.tstop])
  endif else begin 
     str.tstart=min([wtstr.tstart,pcstr.tstart,pcstr2.tstart])
     str.tstop=max([wtstr.tstop,pcstr.tstop,pcstr2.tstop])
  endelse 
  ntags=n_tags(str)
  wtags=indgen(ntags)
  wtags=[wtags[4:24],wtags[26:*]]
;  for i=2,ntags-5 do str.(i)=wtstr.(i)
  for i=2,n_tags(wtstr)-1 do str.(i)=wtstr.(i)
  
  print,'Combing WT & PC fit_flares output files'
  calc_signif,str,src,bg,exp,expt,time,cts,wtstr,pcstr,pcstr2
  
  print,'Reading flarespec output files'
  read_flarespec_outs,fswtfile,fswtstr
  print,'Combining WT & PC flarespec output file'
  if not twopc then begin 
     if exist(fspcfile) then read_flarespec_outs,fspcfile,fspcstr $
     else estimate_fspc_dat,fswtstr,wtstr,pcstr,str,fspcstr
     combine_wtpc_fsout,fswtstr,fspcstr,fsstr,wtstr,pcstr,str
  endif else begin 
     if exist(fspcfile) then read_flarespec_outs,fspcfile,fspcstr $
     else estimate_fspc_dat,fswtstr,wtstr,pcstr,str,fspcstr
     if exist(fspcfile2) then read_flarespec_outs,fspcfile2,fspcstr2 $
     else estimate_fspc_dat,fswtstr,wtstr,pcstr2,str,fspcstr2
     combine_wtpc_fsout,fswtstr,fspcstr,fsstr,wtstr,pcstr,str,fspcstr2,pcstr2
  endelse 
  
  ;;get flux & fluence of PC data then add that to flux & fluence of WT
     
     
     
  write_flareout_file,str,latespec=latespec,fnum=fnum
  write_flarespec_out,fsstr,fnum=fnum
  
  outdir=' flare'+ntostr(fnum)+'/'
  spawn,'cp '+fsuplfile+outdir
  spawn,'cp flare'+ntostr(fnum)+'_wt/*ps'+outdir
  if exist('flare'+ntostr(fnum)+'_wt/flare_flag.txt') then $
     spawn,'cp flare'+ntostr(fnum)+'_wt/flare_flag.txt'+outdir
  
;  stop
  
  
  return
end 
