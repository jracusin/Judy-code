PRO check_rowb

; The region where we calculate the bias statistics
  bstart = 50
  bend = 199
  
  spawn,'rm tempfile.txt'
  openw,lun,'tempfile.txt',/get_lun ;/append

;    spawn,'ls *bias_row1.fits',file ; > bbb'
;    spawn,'wc bbb > www.txt'
;    readcol,'www.txt',nn
;    spawn,'rm www.txt'
  file=findfile('*bias_row1.fits')
  nn = size(file)
  if file[0] ne '' then begin
;        readcol,'bbb',file,format='(a)'
;        spawn,'rm bbb'
     for k=0,n_elements(file)-1 do begin
        mmm=0 & sss=0
        ima=readfits(file[k],h,/silent)
; changed to [50:199] to remove bias turn down.
        mmm=mean(ima[bstart:bend])
        sss=stddev(ima[bstart:bend])
        ldp=strmid(file[k],strpos(file[k],'_LDP')+4,strpos(file[k],'bias')-strpos(file[k],'_LDP')-5)
        print,file[k],' ',ldp,' ',mmm,'+-',sss
        tstart = fxpar(h,'TSTART')
        tstop  = fxpar(h,'TSTOP')
        printf,lun,ldp,file[k],tstart,tstop,mmm,sss,bstart,bend,format='(i,1x,a,a,a,f,f,f,f)'
     endfor
  endif else  print,'NO RAW BIAS IN THIS DIRECTORY'
  
  close,lun
  free_lun,lun
  
  readcol,'tempfile.txt',ldp,filename,starttime,stoptime,media,sigma,biasstart,biasend,format='(f,a,f,f,f,f,i,i)'  
;spawn,'rm tempfile.txt'
  mtime=(starttime+stoptime)/2.
    
  iu=sort(ldp)
  openw,lun,'check_row_bias.xls'
  printf,lun,"LDP,Filename,Start Time, Stop Time, Mean, Sigma, Bias, Bias start, Bias Stop"
  for i=0,n_elements(ldp)-1 do  printf,lun,ldp[iu[i]],filename[iu[i]],starttime[iu[i]],stoptime[iu[i]],mtime[iu[i]],media[iu[i]],sigma[iu[i]],biasstart[iu[i]],biasend[iu[i]],format='(i10,", ",a50,", ",f14.2,", ",f14.2,", ",f14.2,", ",f12.5,", ",f12.5,", ",i5,", ",i5)'
  printf,lun,''
  printf,lun,"Avg Mean, Avg StdDev"
  printf,lun,mean(media),',',mean(sigma)
  close,lun
  free_lun,lun


end
