pro det_bad_test,sky=sky
  
  grbs=['GRB050820A','GRB051109A','GRB060428A','GRB060614','GRB061021','GRB061121','GRB070328']
  cd,!mdata
  ngrb=n_elements(grbs)
  
  !p.multi=[0,2,4]
  for i=0,ngrb-1 do begin 
     
     ev=mrdfits(grbs[i]+'/spec/seg4pc.evt',1)
  
     if keyword_set(sky) then begin
        x=ev.x & y=ev.y
     endif else begin
        x=ev.detx & y=ev.dety
     endelse 
  
     plot,x,y,psym=1,/yno,xtitle='Det x',ytitle='Det y',/iso,title=grbs[i],charsize=1.5,symsize=0.5
     w=where(ev.pi lt 50)
     oplot,x[w],y[w],psym=1,color=!red,symsize=0.5
     
  endfor 
  !p.multi=0
  
  stop
  return
end 
  

pro nh_evol_test
  
  cr=mrdfits(!mdata+'closure_relations_total_3sig.fits',1)
  ncr=n_elements(cr)
  cd,!mdata
  dirs=file_search('GRB*')
  n=n_elements(dirs)
  nh=dblarr(ncr)
  nherr=dblarr(2,ncr)
  
  for i=0,n-1 do begin
     dir=dirs[i]+'/spec'
     if exist(dir) then begin 
        read_specfit,spec,dir=dir
        wcr=where(strtrim(cr.grb,2) eq dirs[i],nwcr)
        if nwcr ne n_elements(spec) then begin 
           print,dir,'No matchy'
        endif else begin
           nh[wcr]=spec.nh
           nherr[*,wcr]=spec.nh_err;/1.645
        endelse 
     endif 
     
  endfor 
  
  
  u=uniq(cr.grb)
  nu=n_elements(u)
  dnh23=fltarr(nu)
  
  for i=0,nu-1 do begin 
     w=where(cr.grb eq cr[u[i]].grb,nw)
     if nw gt 1 then begin
        w2=where(cr[w].seg2 eq 1,nw2)
        w3=where(cr[w].seg3 eq 1,nw3)
        w4=where(cr[w].seg4 eq 1,nw4)
        if nw2 gt 0 and nw3 gt 0 then begin 
           if nh[w[w2]] gt nh[w[w3]] then dnh23[i]=(nh[w[w2]]-nh[w[w3]])/sqrt(nherr[0,w[w2]]^2+nherr[1,w[w3]]^2) else  dnh23[i]=(nh[w[w3]]-nh[w[w2]])/sqrt(nherr[1,w[w2]]^2+nherr[0,w[w3]]^2)
           
        endif 
     endif 
  endfor   
  
  ;;;NEED TO CHECK IF NH 2-3 ARE CONSISTENT, IF SO THEN CHOOSE BEST FIT (SMALLEST ERROR) NH TO FREEZE FOR OTHER FITS
  
  
  print,cr[u[where(dnh23 gt 3.)]].grb
  
  stop
  tb=cr.tbreak
  wt=where(tb eq 0)      
  tb[wt]=cr[wt].tstart  
  colprint,cr.grb,nh,nherr[0,*],nherr[1,*],cr.beta+1,cr.betaerr[0,*],cr.betaerr[1,*],tb,cr.tstop,cr.z
  
  
  stop
  return
end 
pro spec_evol_test,nsig=nsig,quad=quad
  
  nsig=1.
  if nsig eq 2. then signame='_2sig' else begin
     signame='_3sig'
  endelse 
  cr=mrdfits(!mdata+'closure_relations_total'+signame+'.fits',1)
  w4=where(cr.seg4 eq 1)
  
  u=uniq(cr[w4].grb)
  u=w4[u]
  nu=n_elements(u)
  dbeta23=fltarr(nu) & dbeta24=dbeta23 & dbeta34=dbeta23 & dbeta12=dbeta23
  dbeta23err=dbeta23 & dbeta24err=dbeta23err & dbeta34err=dbeta23err & dbeta12err=dbeta23err

;  plot,[0,4],[0,nu],/nodata,ytitle='i',xtitle=!tsym.beta
  
  colors=[!red,!green,!blue,!yellow,!orange,!purple,!magenta,!cyan]
  for i=0,nu-1 do begin
     w=where(cr.grb eq cr[u[i]].grb,nw)
     if nw gt 1 then begin
        w1=where(cr[w].seg1 eq 1,nw1)
        w2=where(cr[w].seg2 eq 1,nw2)
        w3=where(cr[w].seg3 eq 1,nw3)
        w4=where(cr[w].seg4 eq 1,nw4)
        if nw1 gt 0 and nw2 gt 0 then begin 
           dbeta12[i]=abs(cr[w[w2]].beta-cr[w[w1]].beta)
           if not keyword_set(quad) then begin 
              if cr[w[w2]].beta gt cr[w[w1]].beta then dbeta12err[i]=cr[w[w2]].betaerr[0]+cr[w[w1]].betaerr[1] else dbeta12err[i]=cr[w[w2]].betaerr[1]+cr[w[w1]].betaerr[0]
           endif else begin 
              if cr[w[w2]].beta gt cr[w[w1]].beta then dbeta12err[i]=sqrt(cr[w[w2]].betaerr[0]^2+cr[w[w1]].betaerr[1]^2) else dbeta12err[i]=sqrt(cr[w[w2]].betaerr[1]^2+cr[w[w1]].betaerr[0]^2)
           endelse 
        endif 
        if nw2 gt 0 and nw3 gt 0 then begin 
           dbeta23[i]=abs(cr[w[w3]].beta-cr[w[w2]].beta)
           if not keyword_set(quad) then begin 
              if cr[w[w3]].beta gt cr[w[w2]].beta then dbeta23err[i]=cr[w[w3]].betaerr[0]+cr[w[w2]].betaerr[1] else dbeta23err[i]=cr[w[w3]].betaerr[1]+cr[w[w2]].betaerr[0]
           endif else begin 
              if cr[w[w3]].beta gt cr[w[w2]].beta then dbeta23err[i]=sqrt(cr[w[w3]].betaerr[0]^2+cr[w[w2]].betaerr[1]^2) else dbeta23err[i]=sqrt(cr[w[w3]].betaerr[1]^2+cr[w[w2]].betaerr[0]^2)
           endelse 
        endif 
        if nw2 gt 0 and nw4 gt 0 then begin 
           dbeta24[i]=abs(cr[w[w4]].beta-cr[w[w2]].beta)
           if not keyword_set(quad) then begin 
              if cr[w[w4]].beta gt cr[w[w2]].beta then dbeta24err[i]=cr[w[w4]].betaerr[0]+cr[w[w2]].betaerr[1] else dbeta24err[i]=cr[w[w4]].betaerr[1]+cr[w[w2]].betaerr[0]
           endif else begin 
              if cr[w[w4]].beta gt cr[w[w2]].beta then dbeta24err[i]=sqrt(cr[w[w4]].betaerr[0]^2+cr[w[w2]].betaerr[1]^2) else dbeta24err[i]=sqrt(cr[w[w4]].betaerr[1]^2+cr[w[w2]].betaerr[0]^2)
           endelse 
        endif 
        if nw3 gt 0 and nw4 gt 0 then begin 
           dbeta34[i]=abs(cr[w[w4]].beta-cr[w[w3]].beta)
           if not keyword_set(quad) then begin 
              if cr[w[w4]].beta gt cr[w[w3]].beta then dbeta34err[i]=cr[w[w4]].betaerr[0]+cr[w[w3]].betaerr[1] else dbeta34err[i]=cr[w[w4]].betaerr[1]+cr[w[w3]].betaerr[0]
           endif else begin 
              if cr[w[w4]].beta gt cr[w[w3]].beta then dbeta34err[i]=sqrt(cr[w[w4]].betaerr[0]^2+cr[w[w3]].betaerr[1]^2) else dbeta34err[i]=sqrt(cr[w[w4]].betaerr[1]^2+cr[w[w3]].betaerr[0]^2)
           endelse 
        endif 
     endif 
  endfor 
  
  ;;;if dist < 0 then consistent
  
  v=1.;nsig;/1.645 ;; 3 sigma
  erase
  multiplot,/init,[1,4]
  
  w=where(dbeta12 ne 0)
  multiplot
  d12=dbeta12[w]-dbeta12err[w]*v
  w12=where(d12 gt 0,nw12)
  if nw12 gt 0 then print,cr[u[w[w12]]].grb
  plothist,d12,bin=0.1,xrange=[-2,1]
  legend,['12'],/top,/right,box=0
  q=where(d12 gt 0,nq)
  print,'12',nq,nq*1./nu
  
  w=where(dbeta23 ne 0)
  multiplot
  d23=dbeta23[w]-dbeta23err[w]*v
  w23=where(d23 gt 0,nw23)
  if nw23 gt 0 then print,cr[u[w[w23]]].grb
  plothist,d23,bin=0.1,xrange=[-2,1]
  legend,['23'],/top,/right,box=0
  q=where(d23 gt 0,nq)
  print,'23',nq,nq*1./nu
  
  w=where(dbeta24 ne 0)
  multiplot
  d24=dbeta24[w]-dbeta24err[w]*v
  w24=where(d24 gt 0,nw24)
  if nw24 gt 0 then print,cr[u[w[w24]]].grb
  plothist,d24,bin=0.1,xrange=[-2,1]
  legend,['24'],/top,/right,box=0
  q=where(d24 gt 0,nq)
  print,'24',nq,nq*1./nu
  
  w=where(dbeta34 ne 0)
  multiplot
  d34=dbeta34[w]-dbeta34err[w]*v
  w34=where(d34 gt 0,nw34)
  if nw34 gt 0 then print,cr[u[w[w34]]].grb
  plothist,d34,bin=0.1,xrange=[-2,1],xtitle=!vsym.delta_cap+!vsym.beta
  legend,['34'],/top,/right,box=0
  q=where(d34 gt 0,nq)
  print,'34',nq,nq*1./nu
  multiplot,/reset
  
  stop
  
  return
end 
