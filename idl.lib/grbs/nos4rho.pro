pro nos4rho

  ;;; collect numbers for Rodrigo's paper

  grbstr=mrdfits('grb_struct_pop_study.fits',1)
  readcol,'~/Fermi/Swift_pop_study/grb_t90.txt',grb,t90s,format='(a,a)'

  grb=strtrim('GRB'+grb,2)
  n=n_elements(grb)
  w=where(t90s ne 'n/a')
  t90=fltarr(n)
  t90[w]=t90s[w]*1.
  grb=grb[w]
;  t90=t90[w]*1.
  
  g=sort(grbstr.grb)
  grbstr=grbstr[g]
  match,strtrim(grbstr.grb,2),grb,m1,m2
  help,grbstr,grb,m1,m2

  w=where(grbstr.eiso gt 0 and grbstr.eke gt 0 and grbstr.z gt 0 and grbstr.thetaj gt 0 and grbstr.xjb eq 1)
  w2=where(grbstr.eiso gt 0 and grbstr.eke gt 0 and grbstr.z gt 0 and grbstr.xjb eq 0)
;  help,w2

  colprint,grbstr[w].grb,grbstr[w].eiso,grbstr[w].eke,grbstr[w].z,grbstr[w].thetaj,t90[w],grbstr[w].who,grbstr[w].xjb
  colprint,grbstr[w2].grb,grbstr[w2].eiso,grbstr[w2].eke,grbstr[w2].z,grbstr[w2].thetaj,t90[w2],grbstr[w2].who,grbstr[w2].xjb

  help,w,w2
  stop

  return
end 
