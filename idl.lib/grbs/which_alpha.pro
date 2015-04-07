function which_alpha,pnames,p,time,inds

  mo=fit_models(pnames,p,np)

;  np=n_elements(p)-nflares*3
  if np mod 2 eq 1 then np=np-1
  case np of
     2: n=1
     4: n=[1,3]
     6: n=[1,3,5]
     8: n=[1,3,5,7]
     10: n=[1,3,5,7,9]
  endcase
  alphas=p[n]
  if np gt 2 then btime=p[n[1:np/2-1]-1] else btime=0

  ntime=n_elements(time)
  aspecarr=fltarr(ntime)
  inds=fltarr(ntime)
  for i=0,ntime-1 do begin
     case np of
        2: aspec=alphas[0]
        4: begin
           if time[i] lt btime[0] then aspec=alphas[0]
           if time[i] gt btime[0] then aspec=alphas[1]
        end 
        6: begin
           if time[i] lt btime[0] then aspec=alphas[0]
           if time[i] gt btime[0] and time[i] lt btime[1] then aspec=alphas[1]
           if time[i] gt btime[1] then aspec=alphas[2]
        end 
        8: begin
           if time[i] lt btime[0] then aspec=alphas[0]
           if time[i] gt btime[0] and time[i] lt btime[1] then aspec=alphas[1]
           if time[i] gt btime[1] and time[i] lt btime[2] then aspec=alphas[2]
           if time[i] gt btime[2] then aspec=alphas[3]
        end 
        10: begin
           if time[i] lt btime[0] then aspec=alphas[0]
           if time[i] gt btime[0] and time[i] lt btime[1] then aspec=alphas[1]
           if time[i] gt btime[1] and time[i] lt btime[2] then aspec=alphas[2]
           if time[i] gt btime[2] and time[i] lt btime[3] then aspec=alphas[3]
           if time[i] gt btime[3] then aspec=alphas[4]
        end 
     endcase
     aspecarr[i]=aspec
     ind=where(p eq aspec)
     inds[i]=ind
  endfor 

  return,aspecarr
end 
