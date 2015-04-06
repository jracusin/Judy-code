pro position_tracking,frinfo,time,dist,snapshot,dir=dir,skip=skip,title=title
  
  print,'Reading in frames'
  if n_elements(frinfo) eq 0 then read_frames,frinfo,dir=dir
  
  ldps=frinfo[rem_dup(frinfo.ldp)].ldp
  nldp=n_elements(ldps)
  
  time=0D & dist=0D & snapshot=0L
  simpctable
  defsymbols
  color=[!red,!orange,!yellow,!green,!blue,!cyan]
  print,'Assembling pointing data for LDPs '+ntostr(min(ldps))+'-'+ntostr(max(ldps))
;  plot,[0,3000],[0,30],/nodata,xtitle='time(s) since settling',ytitle='position offset (arcsec)'
  for i=0L,nldp-1 do begin 
     
     ldp=ldps[i]
     wldp=where(frinfo.ldp eq ldp)
      
     wpt=where(frinfo[wldp].obs_mode eq 'POINTING',nwpt)
     if nwpt gt 1 then begin 
        wpt=wldp[wpt]
        
        t=frinfo[wpt].tstart
        s=sort(t)
        t=t[s]
        
        mintime=min(t,wmin)
        maxtime=max(t,wmax)
        
        t=t-mintime
        ra=frinfo[wpt[s]].ra
        dec=frinfo[wpt[s]].dec
        
        htime=(maxtime-mintime)/2.
        whalf=where(t gt htime,nw)
        if nw le 1 then w=indgen(nwpt)
        
        mra=median(ra[whalf])
        mdec=median(dec[whalf])
        
        ras=(ra-mra)*3600.*cos(mdec*!dtor) ;alter based on spreadsheet
        decs=(dec-mdec)*3600.
        
        time=[time,t]
        d=sqrt(ras^2+decs^2)
        md=mean(d[whalf])
        dist=[dist,d-md]
        snapshot=[snapshot,ldp[s]]
        
     if not keyword_set(skip) then begin 
        !p.multi=[0,1,2]
        plot,ras,decs,xtitle=!vsym.delta_cap+'ra (arcsec)',ytitle=!vsym.delta_cap+'dec (arsec)',title='LDP '+ntostr(ldp)
        ind=[wmin,reverse((n_elements(ras)-1)/(indgen(5)+1))]
        for j=0,5 do plots,ras[ind[j]],decs[ind[j]],color=color[j],psym=1
        plots,0.,0.,psym=2,color=!purple
        plot,t,d-md
        !p.multi=0
        
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endif 
  endfor 
  
  time=time[1:*]
  dist=dist[1:*]
  snapshot=snapshot[1:*]
;  w=where(abs(dist) lt 1000)
;  dist=dist[w]
;  time=time[w]
;  snapshot=snapshot[w]
  s=sort(time)
  time=time[s]
  dist=dist[s]
  snapshot=snapshot[s]
  
  logt=alog10(time*0.01)
  w=where(logt eq -!values.d_infinity)
  nw=where(logt ne -!values.d_infinity)
  logt[w]=min(logt[nw])
  bs=0.04
  h=histogram(logt,reverse_ind=ri,binsize=bs)
  
;  bs=1.
;  h=histogram(time,reverse_ind=ri,binsize=bs)
  nh=n_elements(h)
  t=dblarr(nh) & d=t & dsig=t
  for i=0L,nh-1 do begin 
     
;     t[i]=i*bs+bs/2.              
     IF ri[i] NE ri[i+1] THEN begin
        t[i]=median(time[ri[ri[i] : ri[i+1]-1]])
        d[i]=median(dist[ri[ri[i] : ri[i+1]-1]])
        if n_elements([ri[ri[i] : ri[i+1]-1]]) gt 1 then $
           dsig[i]=stddev(dist[ri[ri[i] : ri[i+1]-1]])
     endif 
  endfor 
  
  w=where(t gt 0 and dsig gt 0)
  t=t[w]
  d=d[w]
  dsig=dsig[w]
  
  plot,[min(t),t],[0,d],psym=10,/xlog,xtitle='time(s) since settling',ytitle='position offset (arcsec)',xrange=[1,1e4],title=title,yrange=[-5,15]
  oplot,[min(t),t],[0,d-dsig],psym=10,color=!red
  oplot,[min(t),t],[0,d+dsig],psym=10,color=!red
  oplot,[min(t),t],[0,d],psym=10
  
;  plot,time[s],dist[s],xtitle='time(s) since settling',ytitle='position offset (arcsec)',psym=3;,/xlog,xrange=[1D-1,1D3],psym=3
  
  return
end 
