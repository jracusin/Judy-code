pro combine_images,imfiles,exfiles,region=region,outfile=outfile,addfilt=addfilt,noexit=noexit,useex=useex
  
  if n_elements(outfile) eq 0 then outfile='combined_image'
  if n_elements(imfiles) eq 0 then $
     imfiles=file_search('*-xrt/sw*pcw4po_cl.evt')

  if imfiles[0] eq '' then imfiles=file_search('*-xrt/sw*pcw2po_cl.evt')
  if keyword_set(useex) then exfiles=file_search('*-xrt/sw*pcw2po_ex.img')
  if n_elements(exfiles) gt 0 then ex=1 else ex=0
  
  print,imfiles
  if n_elements(addfilt) eq 0 then addfilt=''
  
  ;;check for pnt same then xselect else ximage sum
  pntra=0d
  for i=0,n_elements(imfiles)-1 do begin
     hdr=headfits(imfiles[i])
     pntra=[pntra,sxpar(hdr,'RA_PNT')]
  endfor 
  pntra=pntra[1:*]
  
;  if n_elements(imfiles) gt 1 then begin 
;      dpt=pntra[1:*]-pntra[0]
;      wdpt=where(dpt ne 0,nwdpt)
;      if nwdpt eq 0 then begin
;          openw,lun,'xselect_combim.batch',/get_lun
;          printf,lun
;          printf,lun,'read ev '+imfiles[0]
;          printf,lun,'./'
;          printf,lun
;          printf,lun,'read ev '+imfiles[1:*]
;          printf,lun,'extract ev'
;          printf,lun,'save ev combined_events.evt'
;          printf,lun
;          printf,lun,'exit'
;          printf,lun
;          close,lun
;          free_lun,lun
;          spawn,'xselect @xselect_combim.batch'
;          imfiles2='combined_events.evt'
;      endif else imfiles2=imfiles
;  endif else 
imfiles2=imfiles
       
  openw,lun,'xim.batch',/get_lun
  printf,lun,'read/fits/size=1000 '+addfilt+' '+imfiles2[0]
  if ex then printf,lun,'read/fits/size=1000/expo '+exfiles[0]
  printf,lun,'save'
  for i=1,n_elements(imfiles2)-1 do begin
     printf,lun,'read/fits/size=1000 '+imfiles2[i]
     if ex then printf,lun,'read/fits/size=1000/expo '+exfiles[i]
     printf,lun,'sum'
     printf,lun,'save'
  endfor 
  printf,lun,'cpd '+outfile+'.ps/cps'
  if not ex then printf,lun,'disp' else printf,lun,'disp/corr'
  printf,lun,'detect/snr=3'
  printf,lun,'grid/ticks/disp'
  if n_elements(region) gt 0 then printf,lun,'circle/regionfile='+region+'/disp'
  if not ex then printf,lun,'write/fits '+outfile+'.fits' else begin
     printf,lun,'write/fits/disp '+outfile+'_corr.fits'
     printf,lun,'write/fits/exp '+outfile+'_ex.fits'
     printf,lun,'write/fits '+outfile+'.fits'
  endelse 
  
  printf,lun,'cpd /xw'
  if not ex then printf,lun,'disp' else begin
     printf,lun,'disp'
     printf,lun,'disp/exp'
     printf,lun,'disp/corr'
  endelse 
  printf,lun,'detect/snr=3/filedet='+outfile+'.det'
  if n_elements(region) gt 0 then printf,lun,'circle/regionfile='+region+'/disp'
  if not keyword_set(noexit) then printf,lun,'exit'
  
  close,lun,/file
  
  spawn,'ximage @xim.batch'
  
  if ex then begin 
     hdr0=headfits(imfiles2[0])
     hdrl=headfits(imfiles2[n_elements(imfiles2)-1])
     openw,lun,'head.txt',/get_lun
     printf,lun,'MJDREFF   7.428703700000000E-04 / MJD reference (fraction of day)'
     printf,lun,'TSTART    '+ntostr(sxpar(hdr0,'TSTART'))+' / time start'
     printf,lun,'TSTOP     '+ntostr(sxpar(hdrl,'TSTOP'))+' / time stop'
     printf,lun,"DATAMODE  'PHOTON'"
     printf,lun,'DATE-OBS  '+sxpar(hdr0,'DATE-OBS')+' / Start date of observations'
     printf,lun,'DATE-END  '+sxpar(hdrl,'DATE-END')+' / End date of observations'
     
     close,lun
     free_lun,lun
     spawn,'fmodhead '+outfile+'.fits+0 head.txt'
     spawn,'fmodhead '+outfile+'_corr.fits+0 head.txt'
     spawn,'fmodhead '+outfile+'_ex.fits+0 head.txt'
  endif 
  
  return
end 
;MJDREFF   7.428703700000000E-04 / MJD reference (fraction of day)
;TSTART    1.761274472481233E+08 / time start
;TSTOP     1.761388769527517E+08 / time stop
;DATAMODE  'PHOTON'
;DATE-OBS  '2006-08-01T12:17:26' / Start date of observations
;DATE-END  '2006-08-01T15:27:58' / End date of observations
