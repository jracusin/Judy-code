pro grab_eph

  l=mrdfits('~/GRBs/Integral_SPI/combined_triglist.fits',1)
  n=n_elements(l)

  for i=0,n-1 do begin 
     d=strsplit(l[i].trigdate,'- :',/ex)
     url='http://www.isdc.unige.ch/integral/ibas/results/triggers/spiacs/'+d[0]+'-'+d[1]+'/'+strtrim(l[i].trigid,2)+'.eph'
     outfile='~/GRBs/Integral_SPI/'+strtrim(l[i].trigid,2)+'.eph'
     if not exist(outfile) then spawn,'wget '+url+' -O '+outfile
  endfor 

  for i=0,n-1 do begin
     outfile='~/GRBs/Integral_SPI/'+strtrim(l[i].trigid,2)+'.eph'
     readcol,outfile,ra,dec,dist,format='(f,f,f)'
     l[i].integral_ra=ra[0]
     l[i].integral_ra_err=ra[1]
     l[i].integral_dec=dec[0]
     l[i].integral_dec_err=dec[1]
     l[i].integral_dist=dist[0]
     l[i].integral_dist_err=dist[1]
  endfor 

  mwrfits,l,'~/GRBs/Integral_SPI/combined_triglist.fits',/create
  return
end 

pro combine_triglists

  triglist=file_search('~/GRBs/Integral_SPI/trig*fits')
  n=n_elements(triglist)

  l0=mrdfits(triglist[0],1)
  for i=1,n-1 do begin
     l=mrdfits(triglist[i],1)
     concat_structs,l0,l,newl
     l0=newl

  endfor 
  q=where(strtrim(l.type,2) eq '&nbsp')
  l[q].type=''

  mwrfits,newl,'/Users/jracusin/GRBs/Integral_SPI/combined_triglist.fits',/create

  return
end 

pro html2fits

  triglist=file_search('~/GRBs/Integral_SPI/trig*html')

  n=n_elements(triglist)
  c0=create_struct('trigid','','trigdate','','trigtime',0d,'type','',$
                   'sigma',0.,'duration',0.,'max_count',0,'comment','',$
                  'integral_ra',0.,'integral_ra_err',0.,$
                   'integral_dec',0.,'integral_dec_err',0.,$
                   'integral_dist',0.,'integral_dist_err',0.)

  for i=0,n-1 do begin
     readcol,triglist[i],lines,format='(a)',delim='|'
     w=where(strpos(lines,'ibas_acs_web.cgi/?trigger=') ne -1,nw)
     c=replicate(c0,nw)
     for j=0,nw-1 do begin 
        s=strsplit(lines[w[j]],'="',/ex)
        c[j].trigid=s[4]
;        print,c[j].trigid
     endfor 
     w=where(strpos(lines,'tstart=') ne -1,nw)
     for j=0,nw-1 do begin 
        if j lt nw-1 then lines0=lines[w[j]:w[j+1]-1] else lines0=lines[w[j]:*]
        w1=where(strpos(lines0,'<td') ne -1)
        if strpos(lines[w[j]],'IGR') ne -1 then lines0[0]='< >'+lines[w[j]+1]
        lines0=lines0[w1]
        s=strsplit(lines0[0],'<>',/ex)
        c[j].trigdate=strtrim(s[2],2)
        print,c[j].trigdate
        c[j].trigtime=date2met(c[j].trigdate,/fermi)
        s=strsplit(lines0[2],'<>',/ex)
        c[j].type=s[1]
        s=strsplit(lines0[4],'<>',/ex)
        c[j].sigma=s[1]
        s=strsplit(lines0[5],'<>',/ex)
        if n_elements(s) gt 1 then if s[1] ne '&nbsp' then c[j].duration=s[1]
        s=strsplit(lines0[6],'<>',/ex)
        c[j].max_count=s[1]
        s=strsplit(lines0[7],'<>',/ex)
        if n_elements(s) gt 1 then c[j].comment=s[1]
     endfor 
     mwrfits,c,strmid(triglist[i],0,50)+'.fits',/create
  endfor 

  return
end 

pro download_trigs

  jdnow=systime(1,/julian,/utc)
  daycnv,jdnow,yr,mn,day,hr

  years=indgen(yr-2008+1)+2008
  years=ntostr(years)
  months=indgen(12)+1
  months=ntostr(months)
  months[0:8]='0'+months[0:8]

  print,years,months

  for i=0,n_elements(years)-1 do begin
     for j=0,11 do begin
        
        url='"http://www.isdc.unige.ch/integral/ibas/cgi-bin/ibas_acs_web.cgi?month='+years[i]+'-'+months[j]+'&showall=on"'
        print,url
        outfile='~/GRBs/Integral_SPI/triglist_'+years[i]+'_'+months[j]+'.html'
        if not exist(outfile) then $
           spawn,'wget '+url+' -O '+outfile
     endfor 
  endfor 

  return
end 

pro integral_triggers

  l=mrdfits('~/GRBs/Integral_SPI/combined_triglist.fits',1)
  print,l[rem_dup(l.type)].type
  w=where(strtrim(l.type,2) eq 'Confirmed GRB' or strtrim(l.type,2) eq 'Possible GRB' or strtrim(l.type,2) eq 'Short Spike')
  l=l[w]

stop
return
end 
