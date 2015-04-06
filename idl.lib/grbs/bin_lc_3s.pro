pro bin_lc_3s,src,bg,bsttime,gti
  
  srctime=src.time-bsttime
  bgtime=bg.time-bsttime
  gtime=gti.time-bsttime
  
  sevt=srctime[0]

  sn=0d
  i=1
  while sn lt 3. do begin
     sevt=[sevt,srctime[i]]
     wb=where(bgtime gt min(sevt) and bgtime lt max(sevt)
     
     i=i+1
  endwhile
