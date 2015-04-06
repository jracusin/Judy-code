pro rebin_hardrat

  file='hardrat.qdp'
;  readcol,file,time,terr1,terr0,rate,err,format='(d,d,d,d,d)'
  readcol,file,lines,format='(a)',delim='$',comment='!'
  sm=strpos(lines,'NO')
  wl=where(sm ne -1)

  time=0d & terr1=time & terr0=time & rate=time & err=time
  for i=0,n_elements(lines)-1 do begin 
     chunks=str_sep(lines[i],' ')
     if chunks[0] ne 'READ' then begin 
        if chunks[0] eq 'NO' then chunks=dblarr(5)
        chunks=chunks*1d
        time=[time,chunks[0]]
        terr1=[terr1,chunks[1]]
        terr0=[terr0,chunks[2]]
        rate=[rate,chunks[3]]
        err=[err,chunks[4]]
     endif 
  endfor 
  time=time[1:*] & terr1=terr1[1:*] & terr0=terr0[1:*] & rate=rate[1:*] & err=err[1:*]

  h=create_struct('time',0d,'tstart',0d,'tstop',0d,$
                   'hardrate',0d,'hardrate_err',0d,$
                   'softrate',0d,'softrate_err',0d,$
                   'ratio',0d,'ratio_err',0d,'type',0)

  w=where(time eq 0)

;  w=where(time[1:*]-time[0:*] lt 0,nw)
  nt=n_elements(time)
  w=[0,w,nt]

  ww=w[1:*]-w
  h=replicate(h,ww[1]+ww[3])
;     x=xx[w[i]:w[i+1]]
;     n=w[i]+1-w[i]
;     h[xx[0:n]].time=time[x]
  h[w[0]:w[1]].time=time[0:w[1]]
  h[w[0]:w[1]].tstart=time[0:w[1]]+terr0[0:w[1]]
  h[w[0]:w[1]].tstop=time[0:w[1]]+terr1[0:w[1]]
  h[w[0]:w[1]].hardrate=rate[0:w[1]]
  h[w[0]:w[1]].hardrate_err=err[0:w[1]]
  h[w[0]:w[1]].softrate=rate[w[1]+1:w[2]]
  h[w[0]:w[1]].softrate_err=err[w[1]+1:w[2]]
  h[w[0]:w[1]].ratio=rate[w[2]+1:w[3]]
  h[w[0]:w[1]].ratio_err=err[w[2]+1:w[3]]
  h[w[0]:w[1]].type=0

  wn=w[1]+1
  wn1=w[3]+1
  wn2=w[4]+1
  wn3=w[5]+1
  h[wn:*].time=time[wn1:w[4]]
  h[wn:*].tstart=time[wn1:w[4]]+terr0[wn1:w[4]]
  h[wn:*].tstop=time[wn1:w[4]]+terr1[wn1:w[4]]
  h[wn:*].hardrate=rate[wn1:w[4]]
  h[wn:*].hardrate_err=err[wn1:w[4]]
  h[wn:*].softrate=rate[wn2:w[5]]
  h[wn:*].softrate_err=err[wn2:w[5]]
  h[wn:*].ratio=[rate[wn3:*],0]
  h[wn:*].ratio_err=[err[wn3:*],0]
  h[wn:*].type=1

  q=where(h.time ne 0,nh)
  h=h[q]

  erase
  multiplot2,[1,2],/init
  multiplot2
  ploterror,h.time,h.softrate,h.softrate_err,/nohat,psym=3,/xlog,/ylog,ytitle='Count rate',ytickformat='loglabels'
  for i=0,n_elements(h)-1 do oplot,[h[i].tstart,h[i].tstop],[h[i].softrate,h[i].softrate]

  oploterror,h.time,h.hardrate,h.hardrate_err,/nohat,psym=3,errcolor=!red
  for i=0,nh-1 do oplot,[h[i].tstart,h[i].tstop],[h[i].hardrate,h[i].hardrate],color=!red

;  oploterror,h.time,h.ratio,h.ratio_err,/nohat,psym=3,errcolor=!blue
  multiplot2
  ploterror,h.time,h.ratio,h.ratio_err,/nohat,psym=3,errcolor=!blue,/xlog,xtitle='Time (s)',ytitle='Hardness Ratio'
  for i=0,nh-1 do oplot,[h[i].tstart,h[i].tstop],[h[i].ratio,h[i].ratio],color=!blue

  nn=2
  terr=dblarr(nh/nn+1) & ratio=terr & time=terr & rerr=terr
  for i=0,nh-2,nn do begin
     nn1=nn-1
     terr[i/nn]=(h[i+nn1].tstop-h[i].tstart)/2.
     ratio[i/nn]=weighted_mean(h[i:i+nn1].ratio,h[i:i+nn1].ratio_err)
     rerr[i/nn]=sqrt(total(h[i:i+nn1].ratio_err^2))/nn*1.
     time[i/nn]=mean(h[i:i+nn1].time)
  endfor 

  oploterror,time,ratio,terr,rerr,psym=3,/nohat

  multiplot2,/reset,/default
stop
return
end 

  
