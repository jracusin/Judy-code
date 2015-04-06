pro wt_psf,exl,r,expf,ef,xc,yc,expo,expfrac,expcorr,psffrac,psfcorr,evmap,cmap
  
  !p.multi=[0,3,2]
  
  nr=n_elements(r)
  exf=ef;expf;-[expf[0:nr-2]]
;  xc=300.
;  yc=300.
  rc=1000
  rc0=400
  
  xmap=intarr(rc,rc) & ymap=xmap & rmap=fltarr(rc,rc) & map=rmap
  
  for i=0,rc-1 do begin 
     xmap[*,i]=indgen(rc)
     ymap[i,*]=indgen(rc)
  endfor 
  
  for i=0,rc-1 do begin
     rmap[*,i]=sqrt((xmap[*,i]-xc)^2.+(ymap[*,i]-yc)^2)
  endfor 
  
  for i=0,rc-1 do begin
     for j=0,rc-1 do begin
        map[i,j]=interpol(exf,r,rmap[i,j])
     endfor
  endfor 
  
  cmap=map*expo
  
  wt=fltarr(rc) & rr=wt & wtc=wt
  for i=rc0,rc0+199 do begin
     wtc[i]=total(expo[i,rc0:rc0+199])
     wt[i]=total(map[i,rc0:rc0+199])
;     rr[i]=sqrt((i-xc)^2)
     rr[i]=i
  endfor 
;  wtc=(wtc-min(wtc))
;  wtc=wtc/max(wtc)
  wt=(wt-min(wt))
  wt=wt/max(wt)
  
;  plot,rr,wt,/xlog,/ylog,xrange=[0.1,300],/xstyle
  
  plot,rr,wt,/yno,xrange=[rc0+50,rc0+150]
  plot,rr,wtc,/yno,xrange=[rc0+50,rc0+150]
  
  wexcl=where(rr gt xc-exl/2. and rr lt xc+exl/2.,nwexcl)
  wtot=where(rr ge xc-20 and rr le xc+20)
  
  if nwexcl gt 0 then psffrac=total(wt[wexcl])/total(wt[wtot]) else psffrac=0.
  print,'missing psffrac=',psffrac
  psfcorr=1./(1.-psffrac)
  print,'PSF corr=',psfcorr
  
;  if nwexcl gt 0 then expfrac=total(wtc[wexcl])/total(wtc[wtot]) else expfrac=0.
  wuse=where((rr ge xc-20 and rr le xc-exl/2.) or (rr ge xc+exl/2. and rr le xc+20),nwuse)
  expfrac=total(wtc[wuse])/(max(wtc)*nwuse)
  print,'expfrac=',expfrac
  expcorr=1./(expfrac)
  print,'EXP corr=',expcorr
  
  minwt=min(wt)
  if n_elements(exl) gt 0 then begin
     oplot,[xc-exl/2.,xc-exl/2.],[0,1e8],color=!green,line=2
     oplot,[xc+exl/2.,xc+exl/2.],[0,1e8],color=!green,line=2
     oplot,[xc-20,xc-20],[0,1e8],color=!green
     oplot,[xc+20,xc+20],[0,1e8],color=!green
  endif 
  
  
  rdis,map,xmn=rc0,ymn=rc0,xmx=rc0+200,ymx=rc0+200
  rdis,expo,xmn=rc0,ymn=rc0,xmx=rc0+200,ymx=rc0+200
  rdis,cmap,xmn=rc0,ymn=rc0,xmx=rc0+200,ymx=rc0+200
;  rdis,cmap+evmap,xmn=200,ymn=200,xmx=400,ymx=400
  
;  rdis,evmap,xmn=200,ymn=200,xmx=400,ymx=400
  oplot,[xc-20,xc-20,xc-exl/2.,xc-exl/2.,xc-20],[yc-10,yc+10,yc+10,yc-10,yc-10],color=!green
  oplot,[xc+20,xc+20,xc+exl/2.,xc+exl/2.,xc+20],[yc-10,yc+10,yc+10,yc-10,yc-10],color=!green

  


  return
end 
