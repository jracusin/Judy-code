pro choose_gbm_det,dir,ps=ps

  if n_elements(dir) ne 0 then cd,dir
  if keyword_set(ps) then begplot,name='GBM_raw_spec.ps'
  pha=file_search('*cspec*pha')
  npha=n_elements(pha)
  if npha ne 14 then stop
  maxcts=dblarr(npha)
  dets=strarr(npha)
  !p.multi=[0,3,5]
  for i=0,npha-1 do begin
     det=strmid(pha[i],10,2)
     dets[i]=det
     spec1=mrdfits(pha[i],1,/unsign)
     spec2=mrdfits(pha[i],2,/unsign)
     
     counts=dblarr(128)
     eng=counts
     for j=0,128-1 do begin
        counts[j]=total(spec2.counts[j])
        eng[j]=(spec1[j].e_max-spec1[j].e_min)/2.+spec1[j].e_min
;        for k=0,n_elements(spec2)-1 do counts[j]=counts[j]+total(spec2[k].counts[j])
     endfor 
     if i le 1 then w=where(eng lt 1e4)
     if i gt 1 then w=where(eng lt 900)
     plot,eng[w],counts[w],title=det,xtitle='Energy (keV)',ytitle='Counts',/xlog,/ylog,yrange=[1e3,1e6],xrange=[1,2e4]
     maxcts[i]=max(counts[w])
     legend,[ntostr(long(maxcts[i]))],/top,/right,box=0,charsize=1
  endfor 
  s=reverse(sort(maxcts))
  colprint,dets[s],maxcts[s]
  tmp=max(maxcts[0:1],b)
  print,'Brightest BGO: '+dets[b]
  nai=indgen(12)+2
  s2=reverse(sort(maxcts[nai]))
  print,'Brighest NaIs: '+ntostrarr(dets[nai[s2[0:3]]],',')

  !p.multi=0
  if keyword_set(ps) then endplot
 
  return
end 
