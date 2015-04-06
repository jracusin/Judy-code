pro add_exptime2specfit
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  g=0
  stop
  
  for i=g,ndir-1 do begin
     cd,!mdata+dir[i]
     print,ntostr(i)+'  '+dir[i]
     if exist('spec') then begin
        cd,'spec'
        read_specfit,spec
        if spec[0].exptime eq 0 then begin 
           segfiles=file_search('seg*dat')
           nseg=n_elements(segfiles)
           pcfiles=file_search('seg*pc.evt')
           wtfiles=file_search('seg*wt.evt')
           wpc=where(pcfiles ne '',nwpc)
           if nwpc gt 0 then evtfiles=pcfiles[wpc]
           wwt=where(wtfiles ne '',nwwt)
           if nwwt gt 0 then evtfiles=[evtfiles,wtfiles[wwt]]
           nevt=n_elements(evtfiles)
           exptime=dblarr(nevt)
           seg=intarr(nevt)
           nev=intarr(nevt)
           for j=0,nevt-1 do begin 
              hdr=headfits(evtfiles[j])
              exptime[j]=sxpar(hdr,'EXPOSURE')
              nev[j]=sxpar(hdr,'NEVENTS')
              seg[j]=strmid(evtfiles[j],3,1)
           endfor 
           useg=seg[rem_dup(seg)]
           for j=0,n_elements(useg)-1 do begin 
           openw,lun,segfiles[j],/get_lun,/append
              w=where(seg*1 eq j+1,nw)
              exp=''
              if nw eq 1 then exp=ntostr(exptime[w]) ;;1 ev file
              if nw eq 2 then begin 
                 if nev[w[1]] ge 60. and nev[w[0]] ge 60. then $ ;;2 ev files
                    exp=ntostr(exptime[w[1]])+' '+ntostr(exptime[w[0]])
                 if nev[w[0]] ge 60. and nev[w[1]] lt 60. then exp=ntostr(exptime[w[0]]) ;;PC only
                 if nev[w[0]] lt 60. and nev[w[1]] ge 60. then exp=ntostr(exptime[w[1]]) ;;WT only
              endif
              if exp eq '' then stop
              print,'exptime '+exp
           printf,lun,'exptime '+exp
           close,lun
           free_lun,lun
           endfor 
;           k=get_kbrd(10)
;           if k eq 's' then stop
        endif 
     endif 
  endfor 
  
  return
end 
