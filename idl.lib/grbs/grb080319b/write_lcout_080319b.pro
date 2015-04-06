pro write_lcout_080319b

  readcol,'xrt_lc_rate.dat',time,tbin,rate,raterr
  lc=lcout2fits(/empty)   
  lc=replicate(lc,n_elements(time))       
  lc.time=time     
  lc.tstart=time-tbin     
  lc.tstop=time+tbin        
  lc.src_rate=rate      
  lc.src_rate_err=raterr   

  ;;worst case scenario
  lc.exptime=tbin*2.
  lc.src_counts=lc.src_rate*lc.exptime
  lc.tot_back_cts=0.05*lc.src_counts
  lc.pu_corr=1.

  write_lc,lc,'lc_newout_comb.txt'

return
end 
