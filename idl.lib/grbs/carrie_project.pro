pro carrie_project

  readcol,'LIKE_all.txt',GRBNAME, GRBMET, TSTART, TSTOP, LIKE_MY_TS_GRB, LIKE_MY_FLUX, LIKE_MY_FLUX_ERR, LIKE_MY_FLUX_ENE, LIKE_MY_FLUX_ENE_ERR,format='(a,l,f,f,l,l,f,l,f)'
  readcol,'LAT_gti_XRT_ctrate.txt',name,MET, GTI_START, GTI_STOP, GTI_MEAN, CTRATE, CTRATE_negERR, CTRATE_posERR, RA, DEC, ERR,format='(a,d,d,d,d,d,d,d,f,f,f)'

  for i=0,n_elements(grbname)-1 do begin
     w=where(grbmet
