@fit_functions
pro predict_suzaku_091020

  cd,'~/Desktop/GRB091020/'
  read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perror
  t=indgen(62)
  ydn2md,2009,293+t,m,d
  day=86400.

  firatio=1.580E-04/1e-4
  biratio=2.298E-04/1e-4
  expo=20e3
  ctr=bkn2pow(t*day,p)
  flux=8.2e-11
  cxo=0.00314
;  colprint,'Month','Day','XRT CTR','XIS FI CTR','XIS BI CTR','CTR in
;  20ks, flux'
  print,'Month    Day     XRT CTR          XIS FI CTR           XIS  BI CTR     XIS FI 20 ks   XIS BI 20 ks       flux           CXO CTR            CXO CTS'
  colprint,ntostr(m),d,ctr,ctr*firatio,ctr*biratio,ctr*firatio*expo,ctr*biratio*expo,ctr*flux,ctr*cxo/1e-3,ctr*cxo/1e-3*30e3


  return
end 
