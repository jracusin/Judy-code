pro find_grbs

  ;;; plateau in WT mode - Chryssa asked at Capital Chats @GW 06/08/15

  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  wp=where(strpos(g.type,'II-III') ne -1 or strpos(g.type,'II-IV') ne -1,np)
  wnop=where(strpos(g.type,'II-III') eq -1 and strpos(g.type,'II-IV') eq -1)
  
  tplateau_start=dblarr(n_elements(g))
  answer=strarr(n_elements(g))
  
  for i=0,np-1 do begin
     cd,'~/GRBs/'+strtrim(g[wp[i]].grb,2)
     lc=lcout2fits()
     segs=str_sep(g[wp[i]].type,'-')
     wp0=where(segs eq '0',nw0)
     wp1=where(segs eq 'I',nw1)
     wp2=where(segs eq 'II',nw2)	
     wp3=where(segs eq 'III',nw3)	
     if nw1 gt 0 and (nw2 gt 0 or nw3 gt 0) then tplateau_start[wp[i]]=g[wp[i]].p[wp1[0]*2+2]
     wt=where(lc.type eq 0 and lc.time gt tplateau_start[wp[i]],nwt)
     if nwt gt 3 and total(lc[wt].src_counts) gt 1000 then answer[wp[i]]='Y' else answer[wp[i]]='N'
     cd,'~/GRBs/'
  endfor 

  y=where(answer eq 'Y')
  help,y

  stop
return
end 
