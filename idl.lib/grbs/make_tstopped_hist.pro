pro make_total_time_hist
  cd,!mdata
  dir=file_search('GRB*')
  n=n_elements(dir)
  
  file='lc_newout.txt'
  tottime=dblarr(n)
  for i=0,n-1 do begin
     cd,dir[i]
     if exist(file) then begin 
        print,dir[i]
        lc=lcout2fits()
        tottime[i]=total(lc.exptime)
     endif 
     cd,'..'
  endfor 
  
  w=where(tottime ne 0)
  plothist,alog10(tottime[w]),bin=0.1,xtitle='log Total Exposure Time (s)',ytitle='N'
  
  stop
  return
end 


pro make_last_point_hist
  
  cd,!mdata
  dir=file_search('GRB*')
  n=n_elements(dir)
  
  file='lc_newout.txt'
  point=dblarr(n)
  for i=0,n-1 do begin
     cd,dir[i]
     if exist(file) then begin 
        print,dir[i]
        lc=lcout2fits()
        wnul=where(lc.src_rate_err gt 0,nwnul)
        if nwnul gt 0 then begin 
           mtime=max(lc[wnul].time,m)
           point[i]=lc[wnul[m]].src_rate
        endif 
     endif 
     cd,'..'
  endfor 
  
  w=where(point ne 0)
  plothist,alog10(point[w]),bin=0.1,xtitle='log Last Detection Count Rate (0.3-10.0 keV) (s!U-1!N)',ytitle='N'
  
  stop
  return
end 
pro make_tstopped_hist
  
  cd,!mdata
  dir=file_search('GRB*')
  n=n_elements(dir)
  
  file='lc_newout.txt'
  tstop=dblarr(n)
  for i=0,n-1 do begin
     cd,dir[i]
     if exist(file) then begin 
        print,dir[i]
        lc=lcout2fits()
        tstop[i]=max(lc.tstop)
     endif 
     cd,'..'
  endfor 
  
  w=where(tstop ne 0)
  plothist,alog10(tstop[w]),bin=0.1,xtitle='log GRB observation stop time (s)',ytitle='N'
  
  stop
  return
end 
     
