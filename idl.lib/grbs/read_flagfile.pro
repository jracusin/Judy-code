pro read_flagfile,str,flfile
  
  if n_params() eq 1 then flfile='flare_flags.txt'
  
  readcol,flfile,col1,col2,delim='|',format='(a,a)',/silent
  
  str=create_struct('fluecorr_miss',0,$
                    'miss_reg',0,$
                    'fluecorr_over',0,$
                    'over_reg',0,$
                    'fluecorr_mode',0,$
                    'mode_reg',0,$
                    'canon',0,$
                    'upl_q',0,$
                    'other',0,$
                    'lc_reg',0,$
                    'rate',0,$
                    'serendip',0)
  
  ans=intarr(12)
  col2=strtrim(col2,2)
  
  y=where(col2 eq 'yes',ny)
  if ny gt 0 then ans[y]=1
  r=where(col2 eq 'rise',nr)
  if nr gt 0 then ans[r]=1
  d=where(col2 eq 'decay',nd)
  if nd gt 0 then ans[d]=2
  b=where(col2 eq 'both',nb)
  if nb gt 0 then ans[b]=3
  ans[9]=col2[9]
  
  for i=0,11 do str.(i)=ans[i]
  

  return
end 
