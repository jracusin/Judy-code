pro run_confidlev,src,bg,lc=lc
  
  if n_params() eq 0 then begin 
     if n_elements(lc) eq 0 then lc=lcout2fits()
     src=lc.src_counts
     bg=lc.tot_back_cts
  endif 
  n=n_elements(src)
  
  for i=0,n-1 do confidlev,bg[i],src[i]+bg[i],0.9973,smin,smax
  
  return
end 
