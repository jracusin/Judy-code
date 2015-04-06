pro xrt_daily,today,notrends=notrends,noday=noday,nostripchart=nostripchart,$
              notam=notam
  
  ;assume begin run in directory of first pass of new day
  
  webdir='~/webdir/'
  webdird=webdir+'daily/'
  hpdir='~/hotpix/'
  
  if n_elements(today) eq 0 then begin 
     jdnow=systime(/julian)
     daycnv,jdnow,yr,mn,day,hr
     dy = ymd2dn(yr,mn,day)
     if dy lt 100 then dy='0'+ntostr(dy)
     if dy lt 10 then dy='0'+ntostr(dy)
     today=ntostr(yr)+ntostr(dy)
  endif 
  yesterday=ntostr(today*1L-1L)
  
  ;;hotpix
  if not keyword_set(noday) then begin 

     plot_hotpix,yesterday,yesterday,npix=npix ;make daily plots
     if npix eq 0 then return
     hpbase='hotpix_'+yesterday+'_'+yesterday
     spawn,'rm '+webdird+hpbase+'.jpg*'
     spawn,'/usr/bin/convert -resize 400x400 -rotate 270 -page 600x750 '+hpdir+hpbase+'.ps '+webdird+hpbase+'.jpg'
     spawn,'chmod 655 '+webdird+hpbase+'.jpg*'
     hpdfiles=hpbase+'.jpg.*'
     hpdf=file_search(webdird+hpdfiles)
     nhpdf=n_elements(hpdf)
     ext=strarr(nhpdf)
     for i=0,nhpdf-1 do begin 
        tmp=str_sep(hpdf[i],'.')
        ext[i]=tmp[n_elements(tmp)-1]
     endfor 
     ext=ext*1
     
     s=sort(ext)
     hpdf=hpdf[s]             
     ext=ext[s]
     next=n_elements(ext)
     if next gt 2 then ext=[ext[next-2:next-1],ext[0:next-3]] else ext=ext
     
     hpdhtml=webdird+hpbase+'.html'
     openw,lun,hpdhtml,/get_lun
     
     for i=0,n_elements(hpdf)-1 do begin
        if i eq 1 then br='<br>' else br=''
        printf,lun,'<IMG SRC='+hpbase+'.jpg.'+ntostr(ext[i])+'>'+br
     endfor 
     close,lun
     free_lun,lun
     spawn,'chmod 655 '+hpdhtml
  
     date=filedate2readdate(yesterday)
     dayfile=webdir+date+'.html'
  
     readcol,dayfile,lines,format='(a)',delim='$'
     outline="<a href = 'daily/"+hpbase+".html'>Daily Hot Pixel Tracking <br>"
     w=where(lines eq outline,nw)
     if nw eq 0 then begin 
        openu,lun,dayfile,/get_lun,/append
        printf,lun,outline
        close,lun
        free_lun,lun
     endif 
     
     ;daily stripchart
     if not keyword_set(nostripchart) then begin 
         daily_stripchart,yesterday,outfile
         outline1="<a href = 'daily/"+outfile+"'>"+outfile+"<br>"
         spawn,'chmod 655 '+webdird+outfile
     endif else outline1=''
     ;daily tam
     if not keyword_set(notam) then begin 
         daily_tam,yesterday,outfile
         outline2="<a href = 'daily/"+outfile+"'>"+outfile+"<br>"
     endif else outline2=''
     w=where(lines eq outline1 or lines eq outline2,nw)
     if nw eq 0 then begin 
        openu,lun,dayfile,/get_lun,/append
        printf,lun,outline1
        printf,lun,outline2
        close,lun
        free_lun,lun
     endif 
;     spawn,'chmod 655 '+webdird+outfile
  endif 
  
  close,/all,/file
  
  ;make cumulative hotpix plots
  if not keyword_set(notrends) then begin 
     plot_hotpix_trends
     hpfile='~/hotpix/hotpix_trends.ps'
     hpbase='hotpix_trends'
     hpsbase=hpbase+'_summary'
     hpbcbase=hpbase+'_bc'
;;     hpdffile='~/webdir/hotpix_trends.pdf'
;;     spawn,'ps2pdf '+hpfile+' '+hpdffile
;;     spawn,'chmod 655 '+hpdffile
;;     spawn,'mv '+hpdffile+' '+webdir
;     spawn,'rm '+webdird+hpbase+'.jpg*'
;     spawn,'/usr/bin/convert -resize 400x400 -page 600x750 '+hpdir+hpbase+'.ps '+webdird+hpbase+'.jpg'
;     spawn,'chmod 655 '+webdird+hpbase+'.jpg*'
;     hpdfiles=hpbase+'.jpg.*'
;     hpdf=file_search(webdird+hpdfiles)
;     nhpdf=n_elements(hpdf)
     
;     readcol,hpdir+'hotpix_trends_dat.txt',nrep,pixx,pixy,ext,format='(L,A,A,L)',delim=','
;     wn=where(nrep gt 1,nwn)
;     wn0=where(nrep eq 1,nwn0)
     
     spawn,'/usr/bin/convert -resize 600x600 -page 600x750 '+hpdir+hpsbase+'.ps '+webdird+hpsbase+'.jpg'
     hpdsfiles=file_search(webdird+hpsbase+'.jpg.*')
     spawn,'chmod 655 '+webdird+hpsbase+'.jpg*'
     
     spawn,'/usr/bin/convert -resize 600x600 -page 600x750 '+hpdir+hpbcbase+'.ps '+webdird+hpbcbase+'.jpg'
     hpbcfiles=file_search(webdird+hpbcbase+'.jpg.*')
     spawn,'chmod 655 '+webdird+hpbcbase+'.jpg*'
     
     hpdhtml=webdir+hpbase+'.html'
     openw,lun,hpdhtml,/get_lun
     
     for i=0,n_elements(hpdsfiles)-1 do $
        printf,lun,'<IMG SRC=daily/'+hpsbase+'.jpg.'+ntostr(i)+'>'
;     printf,lun,'<IMG SRC=daily/'+hpdsfiles[1]+'><br>'
     printf,lun,'<br>'
     printf,lun,'<br><center><b>Hot Columns</b></center><br>'
     for i=0,n_elements(hpbcfiles)-1 do $
        printf,lun,'<IMG SRC=daily/'+hpbcbase+'.jpg.'+ntostr(i)+'>'
;     printf,lun,'<center><b>Recurrent Hot Pixels</b></center><br>'
;;     for i=0,n_elements(nwn)-1 do $
;     printf,lun,'<IMG SRC=daily/'+hpbase+'.jpg.'+ntostr(ext[wn]-1)+'>'
;     printf,lun,'<br><center><b>Non-Recurrent Hot Pixels</b></center><br>'
;;     for i=0,n_elements(nwn0)-1 do $
;     printf,lun,'<IMG SRC=daily/'+hpbase+'.jpg.'+ntostr(ext[wn0]-1)+'>'
     
     close,lun
     free_lun,lun
     spawn,'chmod 655 '+hpdhtml
     
  endif 
  
  ;add bias, startup, raw frames
  
  close,/all
  
  
  return
end 
