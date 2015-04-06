pro grb051109a_lc
  
  ;.com ~dmorris/Idl/plot_lc_prelease11
  files=['seg0_v10/sw00163136000xwtw2po_cl_ff.evt','seg0_v10/sw00163136000xpcw2po_cl.evt','seg1_v7/sw00163136001xpcw2po_cl.evt','seg2_v5/sw00163136002xpcw2po_cl.evt']
  ;plot_lc,[0,1,1,1],files,330.31502,40.823473,20,330.21634,40.857516,40,[30,30,50,50],date2met('2005-313-01:12:20'),4,2.08,1
  
  readcol,'lc_out.txt',time,tstart,tstop,rate,err,hard,harderr,exptime,scts,bcts,skip=2,format='(d,d,d,d,d,d,d)',/silent
  
  fit_bpow3,time,rate,err,a,/doplot
  
  steppar,time,rate,1/err^2,a,par1ind=1,par1min=2.5,par1max=4.3,nstep1=30,function='bpow3',/noder,/plot,chi2red=89.44,chi2val=chi2val,dof=103,par1val=par1val
  
  return
end 
