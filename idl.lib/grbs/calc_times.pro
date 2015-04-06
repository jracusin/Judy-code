pro calc_times,evtfile,battime,start,stop,exptime
  
  nfiles=n_elements(evtfile)
  start=dblarr(nfiles) & stop=start & exptime=start
  for i=0,nfiles-1 do begin 
     ev=mrdfits(evtfile[i],1,hdr)
     exptime[i]=sxpar(hdr,'EXPOSURE')
     
     start[i]=ev[0].time
     stop[i]=ev[n_elements(ev)-1].time
     
     print,'bin start: '+ntostr(start[i]-battime)
     print,'bin stop:  '+ntostr(stop[i]-battime)
     print,'median:    '+ntostr((stop[i]-start[i])/2.+(start[i]-battime))
  endfor 
     
  return
end 
