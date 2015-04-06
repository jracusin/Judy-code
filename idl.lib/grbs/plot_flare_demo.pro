pro plot_flare_demo,ps=ps

  grbs=['GRB050502B','GRB051117A','GRB061121','GRB070110','GRB090715B']
  n=n_elements(grbs)

  if keyword_set(ps) then begplot,name='~/Swift/XRT_flare_demo.ps',/color,/lan
  colors=[!green,!blue,!red,!orange,!magenta]
  plot,[10,1e7],[1e-4,1e8],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Count Rate (arbitrarily scaled)'

  for i=0,n-1 do begin
     cd,grbs[i]
     lc=lcout2fits(/phil)
     wdet=where(lc.src_rate_err gt 0,ndet)
     x=10^i
     oploterror,lc[wdet].time,lc[wdet].src_rate*x,lc[wdet].src_rate_err*x,/nohat,psym=3,errcolor=colors[i]
     for j=0,ndet-1 do oplot,[lc[j].tstart,lc[j].tstop],[lc[j].src_rate,lc[j].src_rate]*x,color=colors[i]

     cd,'..'
  endfor 
  legend,grbs,box=0,/top,/right,textcolor=colors
  
  if keyword_set(ps) then endplot
  return
end 
