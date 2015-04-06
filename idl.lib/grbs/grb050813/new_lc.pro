pro new_lc
  
  ;filters use temp <-52 and extraction radius of 5 pix
  btime=date2met('2005-225-06:45:09.44')
  files=['sw00150139000xpcw4po_ff.evt','sw00150139001xpcw4po_cl.evt','sw00150139003xpcw4po_cl.evt','sw00150139004xpcw4po_cl.evt']
  ;.com plot_lc_release14
  plot_lc,[1,1,1,1],files,241.98837,11.248753,10,242.00641,11.357903,40,5,btime,4,2.08,1,1
