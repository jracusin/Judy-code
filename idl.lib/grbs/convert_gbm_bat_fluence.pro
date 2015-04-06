pro convert_gbm_bat_fluence,sgbm,sgbmerr,e1,e2,epeak,alpha,beta,sb,hr,hrerr,sberr

  be1=15. ;;keV
  be2=150. ;;keV
  be=dindgen((be2-be1))+be1
  ;; Zhang Hardness ratio
  be1=50. 
  be2=100.
  be_hr1=dindgen((be2-be1))+be1
  be1=25. 
  be2=50.
  be_hr2=dindgen((be2-be1))+be1
  
  e=dindgen((e2-e1))+e1
  e0=epeak/(2.+alpha)

  if epeak eq 0 and beta eq 0 then begin
     bf=simple_pl2(be,alpha)
     bf1=simple_pl2(be_hr1,alpha)
     bf2=simple_pl2(be_hr2,alpha)
     f=simple_pl2(e,alpha)
  endif 

  if epeak ne 0 and beta eq 0 then begin
     bf=cutoff_pl2(be,alpha,epeak)
     bf1=cutoff_pl2(be_hr1,alpha,epeak)
     bf2=cutoff_pl2(be_hr2,alpha,epeak)
     f=cutoff_pl2(e,alpha,epeak)
  endif 

  if epeak ne 0 and beta ne 0 then begin
     bf=band2(be,alpha,e0,beta)
     bf1=band2(be_hr1,alpha,e0,beta)
     bf2=band2(be_hr2,alpha,e0,beta)
     f=band2(e,alpha,e0,beta)
  endif 

  ev2erg=1.60217646d-12
  kev2erg=ev2erg*1d3

  bk=int_tabulated(be,bf*be,/double)*kev2erg
  bk1=int_tabulated(be_hr1,bf1*be_hr1,/double)*kev2erg
  bk2=int_tabulated(be_hr2,bf2*be_hr2,/double)*kev2erg

  k=int_tabulated(e,f*e,/double)*kev2erg

  sb=sgbm*bk/k
  sberr=sgbmerr/sgbm*sb

  sb1=sgbm*bk1/k
  sb2=sgbm*bk2/k
  hr=sb1/sb2
  hrerr=sgbmerr/sgbm*hr
;print,sb,sb1,sb2,hr
  return
end 
