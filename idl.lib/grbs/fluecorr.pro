pro output_fluecorr,wtfout,wtfsout,str,fsstr,llun,dir,latespec=latespec
  
  spawn,'cp '+wtfout+' '+wtfout+'.orig'
  spawn,'cp '+wtfsout+' '+wtfsout+'.orig'
  
  write_flareout_file,str,wtfout+'.new',llun
  write_flarespec_out,fsstr,wtfsout+'.new',llun
  
;  write_flareout_file,str,wtfout,llun,latespec=latespec
;  write_flarespec_out,fsstr,wtfsout,llun

;  spawn,'chmod 664 '+dir+'flare*.out' ;'*new'
;  spawn,'chmod 664 '+dir+'fit_result_flare.pha.dat.rfmt'
  spawn,'chmod 664 '+dir+'*new'
  spawn,'chmod 664 '+dir+'*orig'
  spawn,'chmod 664 '+dir+'*estmd'
  
  return
end 

pro combine_fsout,fsstr,str,wtstr,fswtstr,outstr,fsoutstr,outstr2,fsoutstr2,llun=llun,errcode=errcode,flueperc1=flueperc1,flueperc2=flueperc2
  
  print,'Combining output files with estimated files'
  printf,llun,'Combining output files with estimated files'
  
  make_flarespec_outs_struct,fsstr
  
  struct_assign,fswtstr,fsstr
  
  model=['pow','cutpow','grbm','bb']
  comp=['','_fl','_upl']
  abs=['','_unabs']
  ff=['flux','rate','fluence']
  wtdel=ntostr(wtstr.tstop-wtstr.tstart)
  outdel=ntostr(outstr.tstop-outstr.tstart)
  outdel2=''
  if n_elements(fsoutstr2) ne 0 then begin
     outdel2=ntostr(outstr2.tstop-outstr2.tstart)
     dodouble=1
  endif else dodouble=0
  if double(outdel2) lt 0 then begin
     print,'Cannot do double sided correction, delta is negative'
     printf,llun,'Cannot do double sided correction, delta is negative'
     dodouble=0
  endif 
  
  del=ntostr(str.tstop-str.tstart)
  print,outdel+'  '+wtdel+'  '+outdel2+'  '+del
  printf,llun,outdel+'  '+wtdel+'  '+outdel2+'  '+del
  for m=0,3 do begin            ;model 
     bcom='parval=fswtstr.'+model[m]+'_nh_fl'
     btmp=execute(bcom)
;     bcom2='parval2=fsoutstr2.'+model[m]+'_nh_fl'
;     btmp2=execute(bcom2)
     
     if parval ne -999 then begin ;check for empty .rfmt for this model
;        if parval2 eq -999 and dodouble then dodouble=0
        for i=0,2 do begin      ;component 
           if i eq 0 then nf=1 else nf=2
           for j=0,1 do begin   ;abs
              for f=0,nf do begin ;flux/rate/fluence
                 case f of 
                    0: begin    ;flux
                       if dodouble eq 0 then begin
                          weights=ntostr([wtdel,outdel]*1./del)
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[0]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[1]+')'
                          com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2*'+weights[0]+'^2+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2*'+weights[1]+'^2)'
                       endif else begin
                          weights=ntostr([wtdel,outdel,outdel2]*1./del)
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[0]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[1]+'+fsoutstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[2]+')'
                          com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2*'+weights[0]+'^2+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2*'+weights[1]+'^2+fsoutstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2*'+weights[2]+'^2)'
                       endelse 
;                    print,com,execute(com)
;                    print,com2,execute(com2)
                       tmp=execute(com) & tmp2=execute(com2)
                       if tmp eq 0 then print,com
                       if tmp2 eq 0 then print,com2

                    end
                    1: begin    ;rate
                       if dodouble eq 0 then begin
                          weights=ntostr([wtdel,outdel]*1./del)
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+'_rate'+comp[i]+'*'+weights[0]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[1]+')'
                       endif else begin
                          weights=ntostr([wtdel,outdel,outdel2]*1./del)
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+'_rate'+comp[i]+'*'+weights[0]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[1]+'+fsoutstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'*'+weights[2]+')'
                       endelse 
                       
                       tmp=execute(com)
                       
                       if tmp eq 0 then print,com
                    end 
                    2: begin    ;fluence
                       if dodouble eq 0 then begin 
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+')'
                          com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2)'
                       endif else begin
                          com='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'+fsoutstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]
                          com2='fsstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=sqrt(fswtstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2+fsoutstr2.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err^2)'
                       endelse 
;                    print,com,execute(com)
;                    print,com2,execute(com2)
                       tmp=execute(com) & tmp2=execute(com2)
                       if tmp eq 0 then print,com
                       if tmp2 eq 0 then print,com2
                       if m eq 0 and i eq 1 and j eq 0 then begin
                          com='flueperc1=fsoutstr.'+model[m]+'_fluence_fl/fsstr.'+model[m]+'_fluence_fl*100.'
                          tmp=execute(com)
                          print,'Fluence correction 1 is '+sigfig(flueperc1,4)+'%'
                          printf,llun,'Fluence correction 1 is '+sigfig(flueperc1,4)+'%'
                          flueperc2=100.
                          if dodouble ne 0 then begin 
                             com2='flueperc2=fsoutstr2.'+model[m]+'_fluence_fl/fsstr.'+model[m]+'_fluence_fl*100.'
                             tmp2=execute(com2)
                             print,'Fluence correction 2 is '+sigfig(flueperc2,4)+'%'
                             printf,llun,'Fluence correction 2 is '+sigfig(flueperc2,4)+'%'
                          endif 
                       endif 
                       
                    end
                 endcase 
                 if tmp eq 0 or tmp2 eq 0 then begin ;stop
                    errcode=5
                    if not keyword_set(onelog) then begin 
                       free_lun,llun
                       close,llun ;,/file
                       spawn,'chmod 664 '+odir+'fluecorr.log'
                    endif 
                    return
                 endif 
              endfor
           endfor 
        endfor
     endif 
  endfor  
  
  return
end 

pro fill_outstr,outstr,wtstr,region 
  
  ;;need to fillin relevant values of outstr (tstart,tstop)
  
  if region eq 2 then begin 
     norm=wtstr.decay_norm[0]
     pow=wtstr.decay_pow[0]
  endif else begin
     norm=wtstr.rise_norm[0]
     pow=wtstr.rise_pow[0]
  endelse 
  ulnorm=wtstr.ul_norm[0]
  ulpow=wtstr.ul_pow[0]
  
  icpt=10d^(norm-ulnorm)^(1d/(ulpow-pow))
  
  if region eq 1 then begin
     outstr.tstart=icpt
     outstr.tstop=wtstr.tstart
  endif 
  if region eq 2 then begin
     outstr.tstop=icpt
     outstr.tstart=wtstr.tstop
  endif 
  
  for i=4,n_tags(outstr)-1 do outstr.(i)=wtstr.(i)
  
  return
end 

pro estimate_fs_dat,region,fswtstr,wtstr,outstr,fsoutstr,sigr,llun,pfs=pfs
  
;  npfs=0
;  if n_elements(pfs) gt 0 then 
  tpfs=where(pfs eq 1,npfs)
  if npfs eq 0 then make_flarespec_outs_struct,fsoutstr
  
  ;;calculating total counts under power-law out decay
;  if wtstr.tstart lt outstr.tstart then begin
  if region eq 2 then begin 
     norm=wtstr.decay_norm
     pow=wtstr.decay_pow
  endif else begin
     norm=wtstr.rise_norm
     pow=wtstr.rise_pow
  endelse 
  outctstot=(10^norm[0])/(pow[0]+1.)*(outstr.tstop^(pow[0]+1.)-outstr.tstart^(pow[0]+1.))
  ulcts=(10^outstr.ul_norm[0])/(outstr.ul_pow[0]+1.)*(outstr.tstop^(outstr.ul_pow[0]+1.)-outstr.tstart^(outstr.ul_pow[0]+1.))
  outcts=outctstot-ulcts ;counts in upl subtracted flare
  delt=(outstr.tstop-outstr.tstart)
  outctr=outcts/delt
  outtotctr=outctstot/delt
  ulctr=ulcts/delt
;  sigr=sqrt(totwtcts)/(wtstr.tstop-wtstr.tstart) ;wrong, now calculated outside subpro
  print,'estimated integrate counts in missing region '+ntostr(outctstot)
  printf,llun,'estimated integrate counts in missing region '+ntostr(outctstot)
  ; err=calc_par_err(f,r,c,sigf,sigr,n,sign,p,sigp,uln,siguln,ulp,sigulp,t1,t0)
  
  model=['pow','cutpow','grbm','bb']
  comp=['','_fl','_upl']
  comprs=[['outtotctr','outctr','ulctr'],$
          ['','',''],$
          ['outctstot','outcts','ulcts']]
  abs=['','_unabs']
  ff=['flux','rate','fluence']
  for m=0,3 do begin            ;model
     if pfs[m] eq 1 then begin 
     for i=0,2 do begin ;component
        if i eq 0 then nf=1 else nf=2
        for j=0,1 do begin ;abs
           for f=0,nf do begin ;flux/rate/fluence
              if f ne 1 then begin 
                 com='fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=fswtstr.'+model[m]+'_flux'+comp[i]+'/fswtstr.'+model[m]+'_rate'+comp[i]+'*'+comprs[i,f]
                 tmp=execute(com)
                 if tmp eq 0 then print,com,tmp
                 ;;;what if NaN?
                 
                 com2='fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'_err=calc_par_err(fswtstr.'+model[m]+abs[j]+'_flux'+comp[i]+',fswtstr.'+model[m]+'_rate'+comp[i]+','+comprs[i,f]+','+'fswtstr.'+model[m]+abs[j]+'_flux'+comp[i]+'_err,'+ntostr(sigr)+',norm[0],norm[1],pow[0],pow[1],outstr.ul_norm[0],outstr.ul_norm[1],outstr.ul_pow[0],outstr.ul_pow[1],outstr.tstart,outstr.tstop,'+ntostr(i+1)+','+ntostr(f)+')'

                 tmp2=execute(com2)
                 if tmp2 eq 0 then print,com2,tmp2
                 
;                 stop                 
              endif else begin
;                 com='fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'=fswtstr.'+model[m]+'_rate'+comp[i]+'+fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]
                 com='fsoutstr.'+model[m]+abs[j]+'_'+ff[f]+comp[i]+'='+comprs[0,0];[i,0]
                 tmp=execute(com)
                 if tmp eq 0 then print,com,tmp

              endelse 
;                 print,model[m],comp[i],ff[f],comprs[i,f]                 
              if tmp eq 0 or tmp2 eq 0 then stop
           endfor 
        endfor 
     endfor 
  endif      
  endfor 
  
  return 
end

function calc_par_err,f,r,c,sigf,sigr,n,sign,p,sigp,uln,siguln,ulp,sigulp,t0,t1,upl,ff
  
;  sigr=sqrt(c0)/(t1-t0) ;c0=total WT counts
  dxdf=c/r
  dxdr=-f*c/r^2
  dxdc=f/r
  if ff eq 0 then delt=t1-t0 else delt=1.
  
  dcdn=(1./(p+1.)*(t1^(p+1.)-t0^(p+1.))*10^n*alog(10.))/delt
  dcdp=(10^n*(t1^(p+1)*alog(t1)-t0^(p+1)*alog(t0))/(1+p)-10^n*(t1^(p+1)-t0^(p+1))/(1+p)^2)/delt
;  dcdn=1./(p+1.)*(1./delt)*(t1^(p+1.)-t0^(p+1.))*10^n*alog(10.)
;  dcdp=(10^n*(p+1.)*(((t1^(p+1.)*alog(t1))-t1^(p+1.))-((t0^(p+1.)*alog(t0))-t0^(p+1.))))/(p+1.)^2/delt
  dcdnul=(1./(ulp+1.)*(t1^(ulp+1.)-t0^(ulp+1.))*10^uln*alog(10.))/delt
  dcdpul=(-1.*10^uln*(t1^(ulp+1)-t0^(ulp+1))/(1+ulp)^2+10^uln*(-t0^(ulp+1)*alog(t0)+t1^(ulp+1)*alog(t1))/(1+ulp))/delt
;  dcdnul=1./(ulp+1.)*(1./delt)*(t1^(ulp+1.)-t0^(ulp+1.))*10^uln*alog(10.)
;  dcdpul=(10^uln*(ulp+1.)*(((t1^(ulp+1.)*alog(t1))-t1^(ulp+1.))-((t0^(ulp+1.)*alog(t0))-t0^(ulp+1.))))/(ulp+1.)^2/delt
  
;;  sigc2=dcdn^2*((10^(n+sign)-10^(n-sign))/2)^2+dcdp^2*sigp^2
;;  sigc2=dcdn^2*((10^n-10^(n-sign)))^2+dcdp^2*sigp^2
  case upl of 
     1: sigc2=dcdn^2*sign^2+dcdp^2*sigp^2 ;tot
     2: sigc2=dcdn^2*sign^2+dcdp^2*sigp^2+dcdnul^2*siguln^2+dcdpul^2*sigulp^2 ;fl
     3: sigc2=dcdnul^2*siguln^2+dcdpul^2*sigulp^2 ;upl
  endcase
  
  sigx=sqrt(dxdf^2*sigf^2+dxdr^2*sigr^2+dxdc^2*sigc2)
;  if ff eq 2 then stop
  return,sigx
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
  
  
  close,lun;,/file
  free_lun,lun
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

pro write_flareout_file,str,file,latespec=latespec,llun;,fnum=fnum
  
;  file=''
;  read,prompt='Flare Out output file name (hit return for default)? ',file
;  if file eq '' then file='flare'+ntostr(fnum)+'/flare.out'
  print,'Writing out '+file
  printf,llun,'Writing out '+file
  
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
  close,lun;,/file
  free_lun,lun
  
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
  
  close,lun;,/file
  free_lun,lun
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
  endif else latespec=0
  
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

pro write_flarespec_out,str,file,llun;,fnum=fnum
  
;  file=''
;  read,prompt='Flarespec Output file name (Hit return for default)? ',file
;  if file eq '' then file='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt'
  print,'Writing out '+file
  printf,llun,'Writing out '+file
  
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
  
  close,lun;,/file  
  free_lun,lun
  
  return
end 



pro calc_fluecorr,region,wtstr,fswtstr,outstr,fsoutstr,outfile,fsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum,onlyfs=onlyfs,pfs=pfs
  
  ;;reg=0=none reg=1=rise reg=2=decay reg=3=both
  n=n_elements(wtstr.ul_tstarts)
  if not keyword_set(onlyfs) then make_flareout_struct,outstr,n
  npfs=0
  if n_elements(pfs) gt 0 then begin
     if n_elements(pfs) eq 8 then begin
        pfs2=pfs[4:*]
        pfs1=pfs[0:3]
;        stop
     endif else pfs1=pfs 
     tpfs=where(pfs1 eq 1,npfs) 
  endif else begin
     pfs1=[1,1,1,1]
     pfs2=pfs1
  endelse 
  if npfs eq 0 then make_flarespec_outs_struct,fsoutstr
  
  print,pfs1
  if region eq 1 or region eq 2 then begin ;;correct just rise or decay
     if not keyword_set(onlyfs) then fill_outstr,outstr,wtstr,region
     estimate_fs_dat,region,fswtstr,wtstr,outstr,fsoutstr,sigr,llun,pfs=pfs1
     print
     outfile='flare'+ntostr(fnum)+'/flare.out.'+ntostr(region)+'.estmd'     
     write_flareout_file,outstr,outfile,llun,latespec=latespec
     fsoutfile='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt.'+ntostr(region)+'.estmd'
     write_flarespec_out,fsoutstr,fsoutfile,llun
  endif 
  if region eq 3 then begin ;;correct both rise & decay
     if not keyword_set(onlyfs)  then fill_outstr,outstr,wtstr,1
     estimate_fs_dat,1,fswtstr,wtstr,outstr,fsoutstr,sigr,llun,pfs=pfs1
     if not keyword_set(onlyfs) then begin 
        make_flareout_struct,outstr2,n
        fill_outstr,outstr2,wtstr,2
     endif 
     make_flarespec_outs_struct,fsoutstr2
     
     estimate_fs_dat,2,fswtstr,wtstr,outstr2,fsoutstr2,sigr,llun,pfs=pfs2
     
     print
     outfile='flare'+ntostr(fnum)+'/flare.out.1.estmd'
     write_flareout_file,outstr,outfile,llun,latespec=latespec
     fsoutfile='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt.1.estmd'
     write_flarespec_out,fsoutstr,fsoutfile,llun
     
     outfile2='flare'+ntostr(fnum)+'/flare.out.2.estmd'
     write_flareout_file,outstr2,outfile2,llun,latespec=latespec
     fsoutfile2='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt.2.estmd'
     write_flarespec_out,fsoutstr,fsoutfile2,llun
     
     outfile=[outfile,outfile2]
     fsoutfile=[fsoutfile,fsoutfile2]
     
  endif 
  
  return
end 

pro fluecorr_mode,reg,wtstr,fswtstr,outstr,fsoutstr,modeoutfile,modefsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum,latespec=latespec,errcode=errcode
  
  onlyfs=0
  if reg eq 1 or reg eq 2 then begin 
;     outfile='flare'+ntostr(fnum)+'_pc/flare_pc.out'
;     fsoutfile='flare'+ntostr(fnum)+'_pc/fit_result_flare.pha.dat.rfmt'
     outfile='fltemp'+ntostr(fnum)+'_pc/flare_pc.out'
     fsoutfile='fltemp'+ntostr(fnum)+'_pc/fit_result_flare.pha.dat.rfmt'     
     read_flareout_file,outfile,outstr,latespec=latespec
     if exist(fsoutfile) then read_flarespec_outs,fsoutfile,fsoutstr
     if exist(outfile) and not exist(fsoutfile) then onlyfs=1 else onlyfs=0
  endif 
  if reg eq 3 then begin 
;     outfile='flare'+ntostr(fnum)+'_pc_1/flare_pc_1.out'  ;or flare.out???
;     fsoutfile='flare'+ntostr(fnum)+'_pc_1/fit_result_flare.pha.dat.rfmt'
     outfile='fltemp'+ntostr(fnum)+'_pc_1/flare_pc_1.out'  ;or flare.out???
     fsoutfile='fltemp'+ntostr(fnum)+'_pc_1/fit_result_flare.pha.dat.rfmt'
     read_flareout_file,outfile,outstr,latespec=latespec
     if exist(fsoutfile) then read_flarespec_outs,fsoutfile,fsoutstr
     if exist(outfile) and not exist(fsoutfile) then onlyfs=1 else onlyfs=0
     
;     outfile2='flare'+ntostr(fnum)+'_pc_2/flare_pc_2.out'
;     fsoutfile2='flare'+ntostr(fnum)+'_pc_2/fit_result_flare.pha.dat.rfmt'
     outfile2='fltemp'+ntostr(fnum)+'_pc_2/flare_pc_2.out'
     fsoutfile2='fltemp'+ntostr(fnum)+'_pc_2/fit_result_flare.pha.dat.rfmt'
     read_flareout_file,outfile2,outstr2,latespec=latespec
     if exist(fsoutfile2) then read_flarespec_outs,fsoutfile2,fsoutstr2
     
     fsoutfile=[fsoutfile,fsoutfile2]
     outfile=[outfile,outfile2]
  endif 
  
  if exist(fsoutfile[0]) then begin
     ;;check to see if pc .rfmt is filled with -999
     nhind=[3,40,79,120]
     pfs=intarr(4)
     run_cf=0
     for i=0,3 do begin 
        if fsoutstr.(nhind[i]) eq -999 then begin 
           pfs[i]=1
           run_cf=1
        endif 
     endfor
     if n_elements(fsoutfile) eq 2 then begin 
        nhind=[3,40,79,120]
        pfs2=intarr(4)
        run_cf=0
        for i=0,3 do begin 
           if fsoutstr.(nhind[i]) eq -999 then begin 
              pfs2[i]=1
              run_cf=1
           endif 
        endfor
        pfs=[pfs,pfs2]
     endif 
     
     if run_cf then calc_fluecorr,reg,wtstr,fswtstr,outstr,fsoutstr,modeoutfile,modefsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum,onlyfs=onlyfs,pfs=pfs
     
     modefsoutfile=fsoutfile 
     modeoutfile=outfile
  endif else begin
     errcode=1
     print,'No .rfmt file!!!'
     stop
;     return
  endelse 
  ;calc_fluecorr,reg,wtstr,fswtstr,outstr,fsoutstr,modeoutfile,modefsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum,onlyfs=onlyfs
  
 ; if exist(fsoutfile) then read_flarespec_outs,fsoutfile,fsoutstr $
 ; else estimate_fsout_dat,fswtstr,wtstr,outstr,str,fsoutstr
  
  return
end 

pro fluecorr,fnum,noplot=noplot,errcode=errcode,onelog=onelog,llun=llun
  
  if n_params() eq 0 then begin
     print,'syntax fluecorr,fnum(i.e. 1,2,3)'
     return
  endif 
  
;  if n_elements(fnum) eq 0 then fnum=1
  
  dir='flare'+ntostr(fnum)+'/'
  fout=dir+'flare.out'
  fsfile=dir+'fit_result_flare.pha.dat.rfmt'
  flagfile=dir+'flare_flags.txt'
  spawn,'rm '+dir+'*new'
  odir=dir
  
  errcode=2 
  if not keyword_set(onelog) then openw,llun,dir+'fluecorr.log',/get_lun
  
  if not exist(fout) or not exist(fsfile) or not exist(flagfile) then begin
     dir='fltemp'+ntostr(fnum)+'_wt/'
     fout=dir+'flare_wt.out'
     fsfile=dir+'fit_result_flare.pha.dat.rfmt'
     flagfile=dir+'flare_flags.txt'
     if not exist(fout) or not exist(fsfile) or not exist(flagfile) then begin
        print,'Necessary input files do not exist'
        printf,llun,'Necessary input files do not exist'
        errcode=1
        if not keyword_set(onelog) then begin 
           free_lun,llun
           close,llun           ;,/file
           spawn,'chmod 664 '+odir+'fluecorr.log'
        endif 
        return
     endif 
  endif 
  
  read_flagfile,flags,flagfile
  
  if flags.fluecorr_miss eq 0 and flags.fluecorr_over eq 0 and flags.fluecorr_mode eq 0 then begin
     print,'No need to do any fluence corrections'
     print,'This flare is neither missing data, overlapping with another, nor changing mode'
     printf,llun,'No need to do any fluence corrections'
     printf,llun,'This flare is neither missing data, overlapping with another, nor changing mode'
     errcode=0
     if not keyword_set(onelog) then begin 
        free_lun,llun
        close,llun              ;,/file
        spawn,'chmod 664 '+odir+'fluecorr.log'
     endif 
     return
  endif 
  
  if (flags.fluecorr_miss eq 1 and flags.miss_reg eq 0) or (flags.fluecorr_over eq 1 and flags.over_reg eq 0) or (flags.fluecorr_mode eq 1 and flags.mode_reg eq 0) then begin
     print,'flare_flags.txt is not filled in properly!!!'
     print,'If you specify a need for a correction, the region must be specified too'
     printf,llun,'flare_flags.txt is not filled in properly!!!'
     printf,llun,'If you specify a need for a correction, the region must be specified too'
     errcode=3
     if not keyword_set(onelog) then begin 
        free_lun,llun
        close,llun              ;,/file
        spawn,'chmod 664 '+odir+'fluecorr.log'
     endif 
     return
  endif 
    
  
  ;;need this?
  file='lc_out.txt'
  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  
  print,'Reading fit_flares & flarespec output files'
  print
  printf,llun,'Reading fit_flares & flarespec output files'
  printf,llun
  wtfout=fout
  wtfsout=fsfile
;  wtfout='flare'+ntostr(fnum)+'/flare.out'
;  wtfsout='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt'
  read_flareout_file,wtfout,wtstr,latespec=latespec
  
  
  if wtstr.ul_dof eq 0 or wtstr.rise_dof eq 0 or wtstr.decay_dof eq 0 then begin
     print,'The temporal fits are terrible, we cannot do fluence corrections!'
     printf,llun,'The temporal fits are terrible, we cannot do fluence corrections!'
     errcode=4
     if not keyword_set(onelog) then begin 
        free_lun,llun
        close,llun              ;,/file
        spawn,'chmod 664 '+odir+'fluecorr.log'
     endif 
     return
  endif 
  
  read_flarespec_outs,wtfsout,fswtstr
  
  wt=where(time gt wtstr.tstart and time lt wtstr.tstop)
  wttime=total(expt[wt])
;  wtulcts=(10^wtstr.ul_norm[0])/(wtstr.ul_pow[0]+1.)*(wtstr.tstop^(wtstr.ul_pow[0]+1.)-wtstr.tstart^(wtstr.ul_pow[0]+1.))
  sigr=sqrt(total(src[wt]+bg[wt]))/wttime
  print,'total counts in existing data of flare '+ntostr(total(src[wt]-bg[wt]));-wtulcts
  printf,llun,'total counts in existing data of flare '+ntostr(total(src[wt]-bg[wt]));-wtulcts
  
  missoutfile='' & missfsoutfile='' & overoutfile='' & overfsoutfile=''
  modeoutfile='' & modefsoutfile=''
  regions=['','rise','decay','both']
  
  if flags.fluecorr_miss then begin
     print,'---------------------------------------------------------------------------'
     print,'Correcting missing data on flare '+regions[flags.miss_reg]
     printf,llun,'---------------------------------------------------------------------------'
     printf,llun,'Correcting missing data on flare '+regions[flags.miss_reg]
     calc_fluecorr,flags.miss_reg,wtstr,fswtstr,outstr,fsoutstr,missoutfile,missfsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum
  endif 
  
  if flags.fluecorr_over then begin
     print,'---------------------------------------------------------------------------'
     print,'Correcting overlapping data on flare '+regions[flags.over_reg]
     printf,llun,'---------------------------------------------------------------------------'
     printf,llun,'Correcting overlapping data on flare '+regions[flags.over_reg]
     if n_elements(outstr) eq 0 then $
        calc_fluecorr,flags.over_reg,wtstr,fswtstr,outstr,fsoutstr,overoutfile,overfsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum else $
        calc_fluecorr,flags.over_reg,wtstr,fswtstr,outstr2,fsoutstr2,overoutfile,overfsoutfile,outstr3,fsoutstr3,sigr,llun,fnum=fnum
  endif 
  
  if flags.fluecorr_mode then begin
     print,'---------------------------------------------------------------------------'
     print,'Correcting mode changing data on flare '+regions[flags.mode_reg]
     printf,llun,'---------------------------------------------------------------------------'
     printf,llun,'Correcting mode changing data on flare '+regions[flags.mode_reg]
     if n_elements(outstr2) eq 1 then begin
        print,'Cannot correct 3 times'
        printf,llun,'Cannot correct 3 times'
        if not keyword_set(onelog) then begin 
           free_lun,llun
           close,llun           ;,/file
           spawn,'chmod 664 '+odir+'fluecorr.log'
        endif 
        return
     endif 
     spawn,'cp fltemp'+ntostr(fnum)+'_wt/fit_result_flare.pha.dat.rfmt flare'+ntostr(fnum)+'/'
     spawn,'cp fltemp'+ntostr(fnum)+'_wt/flare_wt.out flare'+ntostr(fnum)+'/flare.out'
     if n_elements(outstr) eq 0 then $
        fluecorr_mode,flags.mode_reg,wtstr,fswtstr,outstr,fsoutstr,modeoutfile,modefsoutfile,outstr2,fsoutstr2,sigr,llun,fnum=fnum,latespec=latespec,errcode=errcode else begin
        fluecorr_mode,flags.mode_reg,wtstr,fswtstr,outstr2,fsoutstr2,modeoutfile,modefsoutfile,outstr3,fsoutstr3,sigr,llun,fnum=fnum,latespec=latespec,errcode=errcode
     endelse 
     wtfout='flare'+ntostr(fnum)+'/flare.out'
     wtfsout='flare'+ntostr(fnum)+'/fit_result_flare.pha.dat.rfmt'
  endif 
     ;fluecorr_mode,reg,wtstr,wtfsstr,outstr,fsoutstr,modefiles,outstr2,fsoutstr2,sigr,fnum=fnum
  outfiles=[missoutfile,overoutfile,modeoutfile]
  fsoutfiles=[missfsoutfile,overfsoutfile,modefsoutfile]
  w=where(outfiles ne '')
  outfiles=outfiles[w]
  fsoutfiles=fsoutfiles[w]
  
  n=n_elements(wtstr.ul_tstarts)
  make_flareout_struct,str,n
  
  if n_elements(outfiles) eq 2 then begin 
     str.tstart=min([wtstr.tstart,outstr.tstart,outstr2.tstart])
     str.tstop=max([wtstr.tstop,outstr.tstop,outstr2.tstop])
     for i=2,n_tags(wtstr)-1 do str.(i)=wtstr.(i)
     print,'---------------------------------------------------------------------------'
     printf,llun,'---------------------------------------------------------------------------'
     combine_fsout,fsstr,str,wtstr,fswtstr,outstr,fsoutstr,outstr2,fsoutstr2,llun=llun,errcode=errcode,flueperc1=flueperc1,flueperc2=flueperc2

     if flueperc1 gt 5. and flueperc2 lt 5. then begin
        str.tstart=min([wtstr.tstart,outstr.tstart])
        str.tstop=max([wtstr.tstop,outstr.tstop])
        for i=2,n_tags(wtstr)-1 do str.(i)=wtstr.(i)
        print,'---------------------------------------------------------------------------'
        printf,llun,'---------------------------------------------------------------------------'
        print,'Fluence correction 2 < 5%, NOT INCLUDING IT'
        printf,llun,'Fluence correction 2 < 5%, NOT INCLUDING IT'
        print,'Recombining data not including segment 2'
        printf,llun,'Recombining data not including segment 2'
        errcode=6
        combine_fsout,fsstr,str,wtstr,fswtstr,outstr,fsoutstr,llun=llun,errcode=errcode,flueperc1=flueperc1,flueperc2=flueperc2

     endif 
     if flueperc1 lt 5. and flueperc2 gt 5. then begin 
        str.tstart=min([wtstr.tstart,outstr2.tstart])
        str.tstop=max([wtstr.tstop,outstr2.tstop])
        for i=2,n_tags(wtstr)-1 do str.(i)=wtstr.(i)
        print,'---------------------------------------------------------------------------'
        printf,llun,'---------------------------------------------------------------------------'
        print,'Fluence correction 1 < 5%, NOT INCLUDING IT'
        printf,llun,'Fluence correction 1 < 5%, NOT INCLUDING IT'
        print,'Recombining data not including segment 1'
        printf,llun,'Recombining data not including segment 1'
        errcode=6
        combine_fsout,fsstr,str,wtstr,fswtstr,outstr2,fsoutstr2,llun=llun,errcode=errcode,flueperc1=flueperc1,flueperc2=flueperc2
     endif 
     
  endif else begin
     str.tstart=min([wtstr.tstart,outstr.tstart])
     str.tstop=max([wtstr.tstop,outstr.tstop])
     for i=2,n_tags(wtstr)-1 do str.(i)=wtstr.(i)
     combine_fsout,fsstr,str,wtstr,fswtstr,outstr,fsoutstr,llun=llun,errcode=errcode,flueperc1=flueperc1,flueperc2=flueperc2
  endelse
  
  if not keyword_set(noplot) then begin
     replot_xrt_lc,background=!white,color=!black
     t=[1,time,1e7]
     fill_box,(str.tstart+str.tstop)/2.,1e3,str.tstop-str.tstart,1,color=!green,/border,ymin=1e-4
     fill_box,(wtstr.tstart+wtstr.tstop)/2.,1e3,wtstr.tstop-wtstr.tstart,1,color=!grey50,ymin=1e-4,/border
     oplot,t,10^wtstr.rise_norm[0]*t^wtstr.rise_pow[0],color=!black,line=1
     oplot,t,10^wtstr.decay_norm[0]*t^wtstr.decay_pow[0],color=!black,line=1
     oplot,t,10^wtstr.ul_norm[0]*t^wtstr.ul_pow[0],color=!black
  endif 

  print
  print,'---------------------------------------------------------------------------'
  printf,llun
  printf,llun,'---------------------------------------------------------------------------'
  
  if flueperc1 gt 5. and flueperc2 gt 5. then $
     output_fluecorr,wtfout,wtfsout,str,fsstr,llun,odir,latespec=latespec $
  else begin
     printf,llun,'fluence correction < 5%, not writing output file'
     print,'fluence correction < 5%, not writing output file'
     errcode=6
  endelse 
  
;  crash:
;  errcode=5
  
  if not keyword_set(onelog) then begin 
     free_lun,llun
     close,llun                 ;,/file

     spawn,'chmod 664 '+odir+'fluecorr.log'
  endif 
     
     
  return
end 
