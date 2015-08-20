pro gbm_trig_search,t
  
  ev=mrdfits('~/Fermi/gbmtrig/CTTE/glg_tte_na_150819376_v00.fit',1)
  if n_elements(t) eq 0 then $
     t=mrdfits('~/Fermi/gbmtrig/CTTE/glg_tte_na_150819376_v00.fit',2)

  w=where(t[1:*].time-t.time gt 100)
  tstop=[t[w].time,max(t.time)]
  tstart=[t[0].time,t[w+1].time]

  bin=10.
  plothist,t.time
  for i=0,n_elements(tstart)-1 do begin
     oplot,[tstart[i],tstart[i]],[0,10000],color=!green
     oplot,[tstop[i],tstop[i]],[0,10000],color=!red
     w=where(t.time ge tstart[i] and t.time le tstop[i],nw)

     plothist,t[w].time,x,y,bin=1
     
     for j=0,n_elements(x)-1 do begin
        w1=where((x-x[j] ge -15 and x-x[j] le -5) or (x-x[j] ge 5 and x-x[j] le 15),nw1)
        back=total(y[w1])/nw1
        ;;;; totally broken ...

        t0=t[w].time-t[w[j]].time
        w1=where((t0 ge -15 and t0 le -5) or (t0 ge 5 and t0 le 15),nw1)
        back=nw1/20.
        src=



;     plothist,t[w].time,x10,y10,bin=10,/noplot
;     plothist,t[w].time,x1,y1,bin=1,/noplot
;     back=y10/10.
;     src=y1
     
;     for j=1,n_elements(back)-2 do begin
;        w1=where(x1 ge x10[j] and x1 le x10[j+1])
;        sigma=(src[w1]-back[j-1])/sqrt(src[w1])
;        w2=where(sigma gt 5.,nw2)
;        if nw2 gt 0 then begin 
;           oplot,[t[w[w2]].time,t[w[w2]].time],[0,1e4],color=!blue
;           colprint,t[w[w2]].time,sigma[w2]
;        endif 
;     endfor 
  endfor 

  stop

  return
end 
