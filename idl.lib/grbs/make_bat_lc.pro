pro make_bat_lc,bat1,bat4,btime,bfluxcts
  
  flux=cts*b
  ferr=err*b
  w1=where(cts-err le 1e-6,nw1)
  w0=where(cts-err gt 1e-6,nw0)
  
  erase
  multiplot,[1,5],/init
  
  yrange=[-2e-7,6e-7]
  w=where(bat4.time-btime gt -1 and bat4.time-btime lt 2)
  energies=['15-25 keV','25-50 keV','50-100 keV','100-350 keV']
  for i=0,3 do begin 
;     if i eq 0 then title='Maskweighted 64 ms lightcurve' else title=''
     if i eq 2 then ytitle='Flux (10!U-7!N erg cm!U-2!N s!U-1!N)' else ytitle=''
     multiplot & ploterror,bat4[w].time-btime,bat4[w].rate[i]*b,bat4[w].error[i]*b,xticklen=.05,psym=10,yrange=yrange,title=title,ytitle=ytitle,/nohat,ystyle=1,yminor=4,ytickname=['-2','0','2','4',' '],yticks=4
     legend,energies[i],/top,/left,box=0
  endfor 
  
  multiplot & ploterror,bat1[w].time-btime,bat1[w].rate*b,bat1[w].error*b,xtitle='T-T0 (s)',xticklen=.05,psym=10,/nohat,yrange=yrange,ystyle=1,yminor=4,ytickname=['-2','0','2','4',' '],yticks=4
  legend,'15-350 keV',/top,/left,box=0
  
  return
end 
