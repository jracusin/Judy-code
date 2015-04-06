pro make_compare_lc_page
  
  cd,!mdata
;  lcstat=mrdfits('lc_stats.fits',1)
  mylcs=file_search('GRB*/GRB*lc.gif')
  mygrb=strarr(n_elements(mylcs))
  for i=0,n_elements(mylcs)-1 do begin
     tmp=str_sep(mylcs[i],'/')
     tmp2=strmid(tmp[0],3,8)
     mygrb[i]=tmp2
  endfor 

  lelcs=file_search('leicester/*/curve.gif')
  
  nlelcs=n_elements(lelcs)
  letid=strarr(nlelcs)
  for i=0,nlelcs-1 do begin
     tmp=str_sep(lelcs[i],'/')
     letid[i]=tmp[1]
  endfor 
  
  letid=letid*1L
  
  readcol,'grb_table.csv',grb,time,tid,z,delim=',',format='(a,a,l,f)'
  match,mygrb,grb,m1,m2
  
  match,tid[m2],letid,m1b,m2b
  
  s=sort(grb[m2[m1b]])
  
;  colprint,mylcs[m1[m2b]],lelcs[m1b]
  
  openw,lun,'compare_lcs.html',/get_lun
  printf,lun,'<html>'
  printf,lun,'<body>'
  printf,lun,'<p><img src="'+mylcs[m1[m1b[s]]]+'" width=400><img src="'+lelcs[m2b[s]]+'" width=400></p>'
  printf,lun,'</body>'
  printf,lun,'</html>'
  close,lun
  free_lun,lun
stop  
  return
end 
  
