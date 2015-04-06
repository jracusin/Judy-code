pro gbm_grb_z

  readcol,'Xray_sample.dat',grb,z,format='(a,f)' ;;; Nino's list
  w=where(z ne 999)
  z=z[w]
  grb=grb[w]

  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog_parse.fits',1)
  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  w=where(strtrim(gbm.grb,2) ne '')
  match,strtrim(grb,2),strtrim(gbm[w].grb,2),m1,m2

  colprint,grb[m1]+'   ',gbm[w[m2]].grb_name,z[m1]
  help,m1

  q=where(bat.z ne 0)
  match,strtrim(bat[q].grb,2),strtrim(gbm[w].grb,2),m1,m2
  colprint,bat[q[m1]].grb+'   ',gbm[w[m2]].grb_name,bat[q[m1]].z
  help,m1

stop
return
end 
