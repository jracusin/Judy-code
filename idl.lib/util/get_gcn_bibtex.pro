pro tablenote_grb,grb,tablenote
  
  ;;; make latex table

  read_grb_z,grbs,gcn
  match,strtrim(grbs,2),strtrim(grb,2),m1,m2
  gcn=gcn[m1]
  
  alphabet,az
  tablenote=''
  n=n_elements(gcn)

  for i=0,n-1 do begin 
;     tablenote=[tablenote+' \tablenotetext{'+az[i]+'}{\cite{GCN'+strtrim(gcn[i],2)+'}}']
;     gpos=strpos(gcn[i],'GCN')
     s=strlen(strtrim(gcn[i],2))
     if s eq 4 then $
;     if gpos ne -1 then $
        tablenote=[tablenote+' $^{'+az[i]+'}$\cite{GCN'+strtrim(gcn[i],2)+'}'] else $
           tablenote=[tablenote+' $^{'+az[i]+'}$\cite{'+strtrim(gcn[i],2)+'}']
           
     if i lt n-1 then tablenote=tablenote+','
  endfor
  tablenote='\tablerefs{'+tablenote+'}'
;  print,tablenote
  
  return
end 

pro get_ads

  cd,'~/Swift/redshifts/bib/'
  p=mrdfits('~/Swift/redshifts/grbs_perley_z.fits',1)
  grb='GRB'+p.grb
  w=where(strpos(p.ref[0],'ADS') ne -1 or strpos(p.ref[0],'ads') ne -1,nw)
  for i=0,nw-1 do begin 
     ads=strsplit(p[w[i]].ref[0],'/',/ex)
     url='"http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode='+strtrim(ads[1],2)+'&data_type=BIBTEX&db_key=AST&nocookieset=1"'
;     outfile=strtrim(grb[w[i]],2)+'_ADS.html'
     outfile=strtrim(ads[1],2)+'_ADS.html'
     com='wget '+url+' -O '+outfile
     print,com
     spawn,com
     if numlines(outfile) eq 0 then spawn,'rm '+outfile
;;; need to swap & for %26
  endfor 

  return
end 

pro combine_gcn_bib
  
  ;;; combine gcns into one file

;  cd,'~/papers/jetbreaks1/bib/'
;  read_grb_z,grb,gcn
;  w=where(gcn ne '')
;  grb=grb[w]
;  gcn=gcn[w]

  cd,'~/Swift/redshifts/bib/'
  p=mrdfits('~/Swift/redshifts/grbs_perley_z.fits',1)
  grb=p.grb
  gcn=p.ref[0]
  w=where(strpos(p.ref[0],'GCN') ne -1,nw)
  grb=grb[w]
  gcn=gcn[w]

  ;; files=''

  ;; for i=0,nw-1 do begin
  ;;    gg=strsplit(gcn[i],'GCN',/ex)
  ;;    gg=gg[0]*1
  ;;    if gg lt 1000 then gg='0'+gg
  ;;    if gg lt 100 then gg='0'+gg
  ;;    if gg lt 10 then gg='0'+gg
  ;;    ff='gcn_'+strtrim(gg,2)+'_*.html'
  ;;    print,ff
  ;;    f=file_search(ff)
  ;;    if n_elements(f) gt 1 then stop
  ;;    print,f
  ;;    files=[files,f]
  ;; endfor 
  gfiles=file_search('gcn*html')
  afiles=file_search('*ADS*html')
  files=[gfiles,afiles]
  
  
  com='cat '+ntostrarr(files)+' > grb_z.bib'
  print,com
  spawn,com
  
;  readcol,'grb_z.bib',lines,format='(a)',delim='#',/silent
  openr,lun,'grb_z.bib',/get_lun
  lines=''
  readf,lun,lines
  line=''
  while ~ eof(lun) do begin 
     readf,lun,line
     lines=[lines,line]
  endwhile
  close,lun
  free_lun,lun

  qpos=strpos(lines,'Query')
  w=where(qpos eq -1)
  lines=lines[w]
  rpos=strpos(lines,'Retrieved')
  w=where(rpos eq -1)
  lines=lines[w]
  apos=strpos(lines,'@ARTICLE')
  w=where(apos ne -1,nw)
  for i=0,nw-1 do begin 
     pgrb=''
     if strpos(lines[w[i]],'GCN') ne -1 then begin
        chunks=strsplit(lines[w[i]],'.',/ex)
        q=where(strpos(p.ref[0],'GCN') ne -1 and strtrim(p.ref[0],2) eq 'GCN'+strtrim(chunks[1],2) ,nw)
        pgrb='GRB'+p[q].grb
     endif else begin 
        chunks=strsplit(lines[w[i]],'{,',/ex)
        q=where(strpos(p.ref[0],chunks[1]) ne -1)
        pgrb='GRB'+p[q].grb
     endelse
     if pgrb[0] ne '' then lines[w[i]]='@ARTICLE{'+strtrim(pgrb[0],2)+'_zref,'
  endfor 
  w=where(strtrim(lines,2) ne '')
  lines=lines[w]
  openw,lun,'grb_z.bib',/get_lun
  for i=0,n_elements(lines)-1 do printf,lun,lines[i],format='(a)'
  close,lun
  free_lun,lun
;  writecol,'grb_z.bib',lines,format='(a)'
stop
  return
end 
  
pro read_grb_z,grb,gcn
  

  cd,'~/Swift/redshifts/bib/'
  p=mrdfits('~/Swift/redshifts/grbs_perley_z.fits',1)
  w=where(strpos(p.ref[0],'GCN') ne -1,nw)
  grb='GRB'+p[w].grb
  gcn=strmid(p[w].ref[0],3,5)

;;   read gcn info - obsolete

;; cd,'~/papers/jetbreaks1/bib/'
;;   file='~/jetbreaks/grb_z.csv'
;;   readcol,file,lines,format='(a)',delim='$'  
;;   gcn=strarr(n_elements(lines)-1) & grb=gcn
;;   for i=0,n_elements(lines)-2 do begin
;;      stuff=str_sep(lines[i+1],',')
;;      grb[i]=stuff[0]
;;      year='20'+strmid(grb[i],0,2)
;;      gpos=strpos(stuff[2],'GCN')
;;      if gpos[0] ne -1 then begin
;;         gcn[i]=strmid(stuff[2],4,4)*1
;;      endif else begin
;;         gcn[i]=strtrim(stuff[2],2)
;;      endelse 
;;   endfor 
  return
end 
  
pro get_gcns

  ;;; get bib files for grbs in csv file
  ;;; need to create this file or collect this input from Perley?

  cd,'~/Swift/redshifts/bib/'
  p=mrdfits('~/Swift/redshifts/grbs_perley_z.fits',1)
  w=where(strpos(p.ref[0],'GCN') ne -1,nw)
  for i=0,nw-1 do begin
     year=strmid(p[w[i]].grb,0,2)
     if year lt 30. and year gt 3 then begin 
        gcn=strmid(p[w[i]].ref[0],3,5)
        year='20'+year
        print,p[w[i]].grb,' ',gcn,' ',year
        file='gcn_'+strtrim(ntostr(gcn),2)+'_*.html'
        file=file_search(file)
        if strtrim(file,2) eq '' then begin 
           get_gcn_bibtex,gcn,year,p[w[i]].grb
           endif $
           else print,file,' exists'
     endif 
  endfor 

  ;; cd,'~/papers/jetbreaks1/bib/'
  ;; file='~/jetbreaks/grb_z.csv'
  ;; readcol,file,lines,format='(a)',delim='$'
  ;; for i=0,n_elements(lines)-1 do begin
  ;;    stuff=str_sep(lines[i],',')
  ;;    grb=stuff[0]
  ;;    year='20'+strmid(grb,0,2)
  ;;    gpos=strpos(stuff[2],'GCN')
  ;;    if gpos[0] ne -1 then begin
  ;;       gcn=strmid(stuff[2],4,4)*1
  ;;       get_gcn_bibtex,gcn,year
  ;;    endif 
  ;;    gpos=strpos(stuff[3],'GCN')
  ;;    if gpos[0] ne -1 then begin 
  ;;       gcn=strmid(stuff[3],4,4)*1
  ;;       get_gcn_bibtex,gcn,year
  ;;    endif 
  ;; endfor 
  
  return
end 

pro get_gcn_bibtex,gcn,year,grb
  
  ;;; download bib files
;  az=['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'] 
  az=['W','J','M','S','B','R','H','T','C','P','G','A','L','D','K','E','Y','F','N','I','O','Q','U','V','X','Z']
 
  i=0
  outfile='gcn_out.html'
  grbs=''
  gcns=''
  nwno=0
  if exist('nofiles.txt') then begin 
     readcol,'nofiles.txt',grbs,gcns,format='(a,a)'
     wno=where(gcns eq gcn,nwno)
  endif
  
  while i lt 26 and not exist(outfile) and nwno eq 0 do begin 
     if gcn lt 10000 then a='.' else a=''
     url='"http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode='+year+'GCN..'+ntostr(gcn)+a+'...1'+az[i]+'&data_type=BIBTEX&db_key=AST&nocookieset=1"'
     outfile='gcn_'+ntostr(gcn)+'_'+az[i]+'.html'
     com='wget '+url+' -O '+outfile
     print,com
     spawn,com
     if numlines(outfile) eq 0 then spawn,'rm '+outfile
     i=i+1
  endwhile
  if not exist(outfile) then begin
     if exist('nofiles.txt') then $ 
        readcol,'nofiles.txt',grbs,gcns,format='(a,a)'
     grbs=[grbs,grb]
     gcns=[gcns,gcn]
     
     writecol,'nofiles.txt',grbs,gcns,format='(a,a)'
  endif 
  

  return
end 
