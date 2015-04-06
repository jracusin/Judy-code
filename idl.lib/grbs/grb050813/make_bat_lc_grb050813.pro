pro make_bat_lc
  
  file='/bulk/shadow/racusin/grbs/grb050813/bat_lc.txt'
  readcol,file,time,cts,err,format='(d,d,d)'
  b= 2.0589d-7/1.166d-1         ; bat flux/bat counts
  btime=date2met('2005-225-06:45:09.44')
  flux=cts*b
  ferr=err*b
  w1=where(cts-err le 1e-6,nw1)
  w0=where(cts-err gt 1e-6,nw0)
;  ploterror,time[w0],flux[w0],ferr[w0],/xlog,/ylog,yrange=[1e-6,1e-2],xtitle='T-T0 (s)',ytitle='Flux!L(15-350 keV)!N [erg cm!U-2!N s!U-1!N',title='GRB050813 BAT lightcurve',psym=1,xrange=[0.05,1],/nohat,xstyle=1,charsize=2
 
;  plotsym,1,3,thick=4
;  if nw1 gt 0 then oplot,time[w1],flux[w1]+ferr[w1],psym=8
  
  bat4=mrdfits('/bulk/shadow/racusin/grbs/grb050813/050813_BATout/lc/sw00150139000b_4chan_64ms.lc',1)
  bat1=mrdfits('/bulk/shadow/racusin/grbs/grb050813/050813_BATout/lc/sw00150139000b_1chan_64ms.lc',1)
  
  erase
  batcts=2.0589e-7/1.166e-1
  multiplot,[1,5],/init
  
  yrange=[-2e-7,6e-7]
  w=where(bat4.time-btime gt -1 and bat4.time-btime lt 2)
  energies=['15-25 keV','25-50 keV','50-100 keV','100-350 keV']
  for i=0,3 do begin 
;     if i eq 0 then title='Maskweighted 64 ms lightcurve' else title=''
     if i eq 2 then ytitle='Flux (10!U-7!N erg cm!U-2!N s!U-1!N)' else ytitle=''
     multiplot & ploterror,bat4[w].time-btime,bat4[w].rate[i]*batcts,bat4[w].error[i]*batcts,xticklen=.05,psym=10,yrange=yrange,title=title,ytitle=ytitle,/nohat,ystyle=1,yminor=4,ytickname=['-2','0','2','4',' '],yticks=4
     legend,energies[i],/top,/left,box=0
  endfor 
  
  multiplot & ploterror,bat1[w].time-btime,bat1[w].rate*batcts,bat1[w].error*batcts,xtitle='T-T0 (s)',xticklen=.05,psym=10,/nohat,yrange=yrange,ystyle=1,yminor=4,ytickname=['-2','0','2','4',' '],yticks=4
  legend,'15-350 keV',/top,/left,box=0
  
  return
end 
