pro interp_eph,ephfile,tstart,tstop,ninterp,stk,eph=eph,ieph=ieph
  
  if n_params() eq 0 then begin 
     print,'syntax - interp_eph,ephfile,tstart,tstop,ninterp,stkfile,eph=eph,ieph=ieph'
     return
  endif
  
  if n_elements(eph) eq 0 then eph=readeph(ephfile)
  if n_elements(ninterp) eq 0 then n=2 else n=ninterp;30 sec resolution
  
  tstartmet=date2met(tstart)
  tstopmet=date2met(tstop)
  w=where(eph.met ge tstartmet-1 and eph.met le tstopmet+1,nw)
  ieph=create_struct('date','',$
                    'dy','',$
                    'time','',$
                    'met',0d,$
                    'x',0d,$
                    'y',0d,$
                    'z',0d,$
                    'lat',0d,$
                    'lon',0d,$
                    'ra',0d,$
                    'dec',0d,$
                    'era',0d,$
                    'edec',0d)
    
  ieph0=ieph
  ieph=replicate(ieph,nw*n)
  print,nw
  for i=0L,nw-1 do begin ;loop over each minute
     
     j=w[i]
     k=i*n+1
     k1=i*n+n-1
     
     eph0=eph[j]
     
     struct_assign,eph0,ieph0 ;put eph vals at each minute into new struct
     for z=0,n_tags(ieph0)-1 do ieph[k-1].(z)=ieph0.(z)
     
     intval=(dindgen(n-1)+1)/n

     p=[eph[j].met,eph[j+1].met]
     ieph[k:k1].met=interpolate(p,intval)
     ieph[k:k1].date=met2date(ieph[k:k1].met)
     
     p=[[eph[j].x,eph[j].y,eph[j].z],$
        [eph[j+1].x,eph[j+1].y,eph[j+1].z]] 
;     stop
     interpv=interpolate(p,intval,cubic=-0.5)
     
     p=[[eph[j].lat,eph[j].lon],$
        [eph[j+1].lat,eph[j+1].lon]]
     interpll=interpolate(p,intval,cubic=-0.5)
     
     p=[[eph[j].ra,eph[j].dec],$
        [eph[j+1].ra,eph[j+1].dec]]
     interprd=interpolate(p,intval,cubic=-0.5)

     
     y=0
     for x=k,k1 do begin 
        d=met2date_judy(ieph[x].met)
        ieph[x].dy=ntostr(fix(d[1]))+'/'+ntostr(fix(d[0]))
        t=str_sep(ieph[x].date,'-')
        ieph[x].time=t[2]
                
        ieph[x].x=interpv[0,y]
        ieph[x].y=interpv[1,y]
        ieph[x].z=interpv[2,y]
     
        ieph[x].lat=interpll[0,y]
        ieph[x].lon=interpll[1,y]
     
        ieph[x].ra=interprd[0,y]
        ieph[x].dec=interprd[1,y]
        
        elaz2,-1.*[ieph[x].x,ieph[x].y,ieph[x].z],edec,era
        ieph[x].era=era
        ieph[x].edec=edec
        
        y=y+1
     endfor 
  endfor 
  
  sd=met2date_judy(ieph[0].met)
  ed=met2date_judy(ieph[n_elements(ieph)-1].met)
  stk='STK_EPH_'+ntostr(fix(sd[0]))+ntostr(fix(sd[1]))+'_'+ntostr(fix(ed[0]))+ntostr(fix(ed[1]))+'_00.txt.interp'
  openw,lun,stk,/get_lun
  printf,lun,'"Time (UTCJ4)" "x (km)" "y (km)" "z (km)" "Lat (deg)" "Lon (deg)" "RightAscension (deg)" "Declination (deg)"'
  for i=0L,nw*n-1 do begin 
     printf,lun,ieph[i].dy+' '+ieph[i].time+' '+ntostr(ieph[i].x)+' '+ntostr(ieph[i].y)+' '+ntostr(ieph[i].z)+' '+ntostr(ieph[i].lat)+' '+ntostr(ieph[i].lon)+' '+ntostr(ieph[i].ra)+' '+ntostr(ieph[i].dec)
     
  endfor 
  close,lun
  free_lun,lun
  
  
  
  return
end 
        
       
     
     
     
     
     
     
