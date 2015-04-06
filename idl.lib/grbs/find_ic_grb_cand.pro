pro find_ic_grb_cand
  
  gold=['GRB050128 ', 'GRB050315 ', 'GRB050319 ', 'GRB050505 ', 'GRB050713B', 'GRB050814 ', 'GRB050820A', 'GRB051016B', 'GRB051109A','GRB051221A', 'GRB060124 ', 'GRB060206 ', 'GRB060428A', 'GRB060510A', 'GRB060605 ', 'GRB060614 ', 'GRB060729 ', 'GRB060813 ','GRB060814 ', 'GRB061021 ', 'GRB061121 ', 'GRB061222A', 'GRB070306 ', 'GRB070328 ', 'GRB070419B', 'GRB070420 ', 'GRB070508 ']
  w=where(gold ne 'GRB050319 ')
  gold=gold[w]
  n=n_elements(gold)
  
  cd,!mdata
  pre_cts=lonarr(n) & post_cts=lonarr(n)
  pre_chisq=fltarr(n) & post_chisq=fltarr(n)
  pre_dof=lonarr(n) & post_dof=lonarr(n)
  pre_pow=fltarr(n) & post_pow=fltarr(n)
  pre_powerr=fltarr(2,n) & post_powerr=fltarr(2,n)
  pre_lc=fltarr(n) & post_lc=fltarr(n)
  pre_lcerr=fltarr(2,n) & post_lcerr=fltarr(2,n)
  pre_nh=fltarr(n) & post_nh=fltarr(n)
  pre_nherr=fltarr(2,n) & post_nherr=fltarr(2,n)
  z=fltarr(n)
  sdiff=fltarr(n) & tdiff=fltarr(n) & ndiff=fltarr(n)
  
  sig=2.
  for i=0,n-1 do begin
     
     read_specfit,spec,dir=strtrim(gold[i],2)+'/spec/'
     read_lcfit,strtrim(gold[i],2)+'/lc_fit_out_idl_int2.dat',pname,p,perror,chisq,dof,breaks
     ns=n_elements(spec)
     np=n_elements(p)
     pre_cts[i]=spec[ns-2].nev
     pre_chisq[i]=spec[ns-2].chisq
     pre_dof[i]=spec[ns-2].dof
     pre_pow[i]=spec[ns-2].pow-1
     pre_powerr[*,i]=spec[ns-2].pow_err/1.645
     pre_nh[i]=spec[ns-2].nh
     pre_nherr[*,i]=spec[ns-2].nh_err/1.645
     pre_lc[i]=p[np-3]
     pre_lcerr[*,i]=perror[*,np-3]/1.645
     z[i]=spec[0].z
     
     post_cts[i]=spec[ns-1].nev
     post_chisq[i]=spec[ns-1].chisq
     post_dof[i]=spec[ns-1].dof
     post_pow[i]=spec[ns-1].pow-1
     post_nh[i]=spec[ns-1].nh
     post_nherr[*,i]=spec[ns-1].nh_err/1.645
     post_powerr[*,i]=spec[ns-1].pow_err/1.645
     post_lc[i]=p[np-1]
     post_lcerr[*,i]=perror[*,np-1]/1.645
     
     if pre_pow[i] gt post_pow[i] then sdiff[i]=(pre_pow[i]-post_pow[i])/sqrt(pre_powerr[0,i]^2+post_powerr[1,i]^2) else sdiff[i]=(pre_pow[i]-post_pow[i])/sqrt(pre_powerr[1,i]^2+post_powerr[0,i]^2)
     
     tdiff[i]=(post_lc[i]-pre_lc[i])/sqrt(pre_lcerr[1,i]^2+post_lcerr[0,i]^2)
     
     ndiff[i]=(pre_nh[i]-post_nh[i])/sqrt(post_nherr[1,i]^2+pre_nherr[0,i]^2)
;sdiff[i]=pre_pow[i]-post_pow[i]-pre_powerr[1,i]*sig-post_powerr[0,i]*sig else $
;        sdiff[i]=post_pow[i]-pre_pow[i]-pre_powerr[0,i]*sig-post_powerr[1,i]*sig
  endfor 
  
  w=where(sdiff gt sig and tdiff gt sig)
  s='    '
  print,'                   Pre-Jet Break                                      Post-Jet Break'
  print,'GRB        cts  alpha  alphaerr   beta     betaerr  chisq     cts  alpha   alphaerr  beta   betaerr    chisq   alp-sig  beta-sig'
  pc=ntostr(post_cts)
  wsp=where(post_cts lt 1000)
  pc[wsp]=pc[wsp]+' '
  colprint,gold[w],ntostr(pre_cts[w],4),numdec(pre_lc[w],3),numdec(pre_lcerr[0,w],3),numdec(pre_lcerr[1,w],3),numdec(pre_pow[w],3),numdec(pre_powerr[0,w],3),numdec(pre_powerr[1,w],3),numdec(pre_chisq[w]/pre_dof[w],3)+s,pc[w],numdec(post_lc[w],3),numdec(post_lcerr[0,w],3),numdec(post_lcerr[1,w],3),numdec(post_pow[w],3),numdec(post_powerr[0,w],3),numdec(post_powerr[1,w],3),numdec(post_chisq[w]/post_dof[w],3),'   '+numdec(tdiff[w],2)+'  ',numdec(sdiff[w],2)
  
  
  stop
  return
end 
