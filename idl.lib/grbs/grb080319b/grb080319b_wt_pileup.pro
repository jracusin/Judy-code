pro grb080319b_wt_pileup,results=results,dir=dir,ps=ps,bigbin=bigbin
  
  if n_elements(dir) eq 0 then dir=''
  cd,'~/Desktop/GRB080319B/'
  trigtime=227599971.904;227599969.0
  lcfile0='sw00306757000xwtw2posr.lc'
  lcfile='uncorr_wtlc.qdp'
  readcol,lcfile,time,halfbin,ctr,ctrerr,exp
  hdr=headfits(lcfile0)
  xrt_t0=sxpar(hdr,'TSTART')
  time=time+xrt_t0-trigtime
  
  ;;USES LC_WRAP UN-PU-CORR NO EXCLUSION
;  lc=lcout2fits('lc_newout_uncorr.txt')
;  w=where(lc.time lt 2000)
;  lc=lc[w]
;  time=lc.time
;  ctr=lc.src_rate
  
  xrtdatadir='00306757000/'
  xrtoutdir='00306757000-xrt/'
  
;  srcra=217.919584849d
;  srcdec=36.3025587729d
  srcra=217.91975d
  srcdec=36.30271d
  
  
  if not keyword_set(results) then $
     wt_pileup,xrtdatadir,xrtoutdir,time,ctr,srcra,srcdec,217.86597d,36.268376d,z=0.937,trigtime,bigbin=bigbin else begin
     plot_wtpu_outputs,t,ctr,trigtime,tstart,tstop,dir='wt_pileup40/',ps=ps,bigbin=bigbin
     colprint,indgen(10)+1,tstart,tstop
  endelse 
     
  
  stop
  return
end 
