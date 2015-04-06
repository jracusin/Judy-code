pro get_centroid_pos,imfile,detfile,src,ximage=ximage,wavdetect=wavdetect,suffix=suffix
  
  if n_elements(suffix) eq 0 then suffix=''
  
  go=0
  if keyword_set(ximage) then begin 
     openr,lun,detfile,/get_lun
     keepgoing=1
     while not eof(lun) and keepgoing do begin
        line=readline(lun,delim=' ')
        if line[0] eq '!' then keepgoing=1 else keepgoing=0
        if line[1] eq 'Exposure' then exptime=line[4]
     endwhile
     close,lun
     free_lun,lun
     
     readcol,detfile,n,cts,x,y,vig,rah,ram,ras,decd,decm,decs,errrad,hbox,prob,snr,format='(i,a,d,d,f,i,i,d,a,i,d,i,i,d,d)',skip=14
     
     hms2radec,rah,ram,ras,decd,decm,decs,ra,dec
     n=n_elements(cts)
     cnts=dblarr(n)
     for i=0,n-1 do begin 
        tmp=str_sep(cts[i],'+/-')
        cnts[i]=tmp[0]
     endfor 
     counts=cnts*exptime

     go=1
  endif 
  
  if keyword_set(wavdetect) then begin 
     src=mrdfits(detfile,1,/silent)
     ra=src.ra
     dec=src.dec
     snr=src.src_significance
     n=n_elements(src)
     counts=src.net_counts
     go=1
  endif 
  
  if not go then begin
     print,'Need to specify ximage or wavdetect'
     return
  endif 
  
  src=create_struct('ra',0d,'dec',0d,'ra_err',0d,'dec_err',0d,'src_significance',0d,$
                   'counts',0d)
  src=replicate(src,n)
  src.src_significance=snr
  src.counts=counts
  
  ;;grab pos err stuff from caldb
  poserr=mrdfits('/bulk/pkg/caldb/data/swift/xrt/bcf/instrument/swxposerr20010101v003.fits',3)
  hpd=poserr.par1
  p=-poserr.par2
  errsys=poserr.errsys
  
  cp=['yes','no']
  boxrad=0.2
  for i=0,n-1 do begin 
;     for j=0,1 do begin 
     j=0
     
     com='xrtcentroid clobber=yes infile='+imfile+' outfile=centroid.txt calcpos='+cp[j]+' totalint='+ntostr(counts[i])+" unit='COUNTS' outdir=./ boxra="+ntostr(ra[i])+' boxdec='+ntostr(dec[i])+' boxradius='+ntostr(boxrad)+' interactive=no'
     spawn,com

;        if j eq 0 then begin 
     readcol,'centroid.txt',text,equal,number,delim=' ',format='(a,a,a)'
     src[i].ra=number[0]
     src[i].dec=number[1]
;        endif else begin
;           readcol,'centroid.txt',text,number,delim='=',format='(a,a)'
     src[i].ra_err=hpd*(src[i].counts)^p/sqrt(2.)
     src[i].dec_err=src[i].ra_err
;           src[i].ra_err=number[0]/3600.
;           src[i].dec_err=number[0]/3600.
;        endelse 
;     endfor 
     print,'Source '+ntostr(i)+' of '+ntostr(n)
  endfor 
  
  if keyword_set(ximage) then mwrfits,src,'src_xrtcentroid'+suffix+'.fits',/create
  if keyword_set(wavdetect) then mwrfits,src,'src_wavdetect'+suffix+'.fits',/create
  
  return
end 
