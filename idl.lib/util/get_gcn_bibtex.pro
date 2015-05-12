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
pro combine_gcn_bib
  
  ;;; combine gcns into one file

  cd,'~/papers/jetbreaks1/bib/'
  read_grb_z,grb,gcn
  w=where(gcn ne '')
  grb=grb[w]
  gcn=gcn[w]
  files=''

  for i=0,n_elements(grb)-1 do begin
     gg=strtrim(gcn[i],2)
     if gg lt 1000 then gg='0'+gg
     if gg lt 100 then gg='0'+gg
     if gg lt 10 then gg='0'+gg
     ff='gcn_'+strtrim(gg,2)+'*.html'
     print,ff
     f=file_search(ff)
     if n_elements(f) gt 1 then stop
     print,f
     files=[files,f]
  endfor 
  
  com='cat '+ntostrarr(files)+' > grb_z.bib'
  print,com
  spawn,com
  
  readcol,'grb_z.bib',lines,format='(a)',delim='#'

  qpos=strpos(lines,'Query')
  w=where(qpos eq -1)
  lines=lines[w]
  rpos=strpos(lines,'Retrieved')
  w=where(rpos eq -1)
  lines=lines[w]
  apos=strpos(lines,'@ARTICLE')
  w=where(apos ne -1)
  lines[w]='@ARTICLE{GCN'+strtrim(gcn,2)+','
  w=where(lines ne '')
  lines=lines[w]
  writecol,'grb_z.bib',lines
  return
end 
  
pro read_grb_z,grb,gcn
  
  ;;; read gcn info

  cd,'~/papers/jetbreaks1/bib/'
  file='~/jetbreaks/grb_z.csv'
  readcol,file,lines,format='(a)',delim='$'  
  gcn=strarr(n_elements(lines)-1) & grb=gcn
  for i=0,n_elements(lines)-2 do begin
     stuff=str_sep(lines[i+1],',')
     grb[i]=stuff[0]
     year='20'+strmid(grb[i],0,2)
     gpos=strpos(stuff[2],'GCN')
     if gpos[0] ne -1 then begin
        gcn[i]=strmid(stuff[2],4,4)*1
     endif else begin
        gcn[i]=strtrim(stuff[2],2)
     endelse 
  endfor 
  return
end 
  
pro get_gcns

  ;;; get bib files for grbs in csv file
  cd,'~/papers/jetbreaks1/bib/'
  file='~/jetbreaks/grb_z.csv'
  readcol,file,lines,format='(a)',delim='$'
  for i=0,n_elements(lines)-1 do begin
     stuff=str_sep(lines[i],',')
     grb=stuff[0]
     year='20'+strmid(grb,0,2)
     gpos=strpos(stuff[2],'GCN')
     if gpos[0] ne -1 then begin
        gcn=strmid(stuff[2],4,4)*1
        get_gcn_bibtex,gcn,year
     endif 
     gpos=strpos(stuff[3],'GCN')
     if gpos[0] ne -1 then begin 
        gcn=strmid(stuff[3],4,4)*1
        get_gcn_bibtex,gcn,year
     endif 
  endfor 
  
  return
end 

pro get_gcn_bibtex,gcn,year
  
  ;;; download bib files
  az=['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'] 
 
  i=0
  outfile='gcn_out.html'
  while i lt 26 and not exist(outfile) do begin 
     url='"http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode='+year+'GCN..'+ntostr(gcn)+'....1'+az[i]+'&data_type=BIBTEX&db_key=AST&nocookieset=1"'
     outfile='gcn_'+ntostr(gcn)+'_'+az[i]+'.html'
     com='wget '+url+' -O '+outfile
     print,com
     spawn,com
     if numlines(outfile) eq 0 then spawn,'rm '+outfile
     i=i+1
  endwhile
  

  return
end 
