pro predict_ctr
  
  read_lcfit,'lc_fit_out_idl_int2.dat',pname,p
  i=indgen(40)
  day=86400.
  colprint,i,bkn3pow(i*day,p)
  
  lc=lcout2fits('correct_xrt_lc_rate.txt')
  
  stop
  return
end 
