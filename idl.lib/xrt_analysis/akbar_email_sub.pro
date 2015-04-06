function check_aktime,time,form=form
  
  if time gt 94608000.0 then begin 
     ntime=time
     form='(a,F16.5)'
  endif else begin 
     ntime=''
     form='(a,a)'
  endelse 
  
  return,ntime
end 

pro akbar_email_sub,dir,submit=submit,outdir=outdir
  
  if n_params() eq 1 then cd,dir else begin
     print,'syntax - akbar_email_sub,dir,/submit,outdir=outdir'
  endelse 
  get_file_begin,filebegin
  
  if n_elements(outdir) eq 0 then outdir=''
  openw,aklun,outdir+'akbar_email.txt',/get_lun
  printf,aklun,'<BEGIN>'
  printf,aklun,'<PROJECT=SWIFTXRTDS2>'
  if not keyword_set(submit) then printf,aklun,'<OPTION=VERIFY>' else $
     printf,aklun,'<OPTION=SUBMIT>'
  printf,aklun,'<COVER>'
  
  ;;archive date
  year_start = strpos(filebegin,'_') + 1
  year=strmid(filebegin,year_start,4)
  day=strmid(filebegin,year_start+4,3)
  hour=strmid(filebegin,year_start+7,2)
  minute=strmid(filebegin,year_start+9,2)
  printf,aklun,'Day of Day-Month-Year Date [format=I2]: '
  printf,aklun,'Month of Day-Month-Year Date [format=A3]: '
  printf,aklun,'Year of Day-Month-Year Date [format=I4]: '
  printf,aklun,'Hour of Day-Month-Year Date [format=I2]: '
  printf,aklun,'Minute of Day-Month-Year Date [format=F5.2]: '
  printf,aklun,'Day of Year [format=I3]: '+day
  printf,aklun,'Year of Day-of-year Date [format=I4]: '+year
  printf,aklun,'Hour of Day-of-year Date [format=I2]: '+hour
  printf,aklun,'Minute of Day-of-year Date [format=F5.2]: '+minute
  
  ;;Archive Tag
  mode='Pipeline'               ;ask user?
  printf,aklun,'Mode That Data Is From [format=A10,unit=Pipeline/Playback]: '+mode
  rootname=filebegin
  printf,aklun,'Root Filename [format=A20,required]: '+rootname
  analyst='XRT'                 ;need specific?
  printf,aklun,'Analyst Initials [format=A3,required]: ',analyst
  printf,aklun,'Reviewed? [format=A1,unit=Y/N]: '+'N' ;XDS will change
;  printf,aklun,'Review Date [format=F16.5]: '
;  printf,aklun,'Modification Date [format=F16.5]: '
  printf,aklun,'Analysis Method [format=A4,unit=Auto/Man]: '+'Auto'
;  note=''
;  printf,aklun,'Archive Tag Note [format=A80]: '
  
  ;;stripchart
  
  files=filebegin+['_ahkVC0.0','_tdrsshk.0','_ahk.0','_ahk_tdrss_vc6.0']+'.stripchart_table.fits'
  apids=['0X484 RT','0X534 RT','0X484 SSR','0X534 SSR']
  data='N'
  cat='N'
  gaps='N'
  prt='N'
  for i=0,3 do begin 
     file=files[i]
     apid=apids[i]
     sampnostart=''
     sampnostop=''
     starttime=''
     stoptime=''
     note=''
     f165='(a,a)'
     
     if exist(file) then begin 
        if file_size(file) gt 0 then begin 
           data='Y'
           f165='(a,F16.5)'
           scvc0tab = mrdfits(file,1,h,/silent)
           s=size(scvc0tab)
           starttime=scvc0tab[0].sctime
           stoptime=scvc0tab[s[1]-1].sctime
           sampnostart='1'
           sampnostop=ntostr(s[1])
        endif else data='N'
     endif 
     starttime=check_aktime(starttime*1D,f=strf)
     stoptime=check_aktime(stoptime*1D,f=stpf)
     printf,aklun,'Stripchart APID '+apid+' Data? [format=A1,unit=Y/N]: '+data
     printf,aklun,'Stripchart APID '+apid+' Catalog? [format=A1,unit=Y/N]: '+cat
     printf,aklun,'Stripchart APID '+apid+' Time-gaps? [format=A1,unit=Y/N]: '+gaps
     printf,aklun,'Stripchart APID '+apid+' Print? [format=A1,unit=Y/N]: ',prt
     printf,aklun,'Stripchart APID '+apid+' Sample Number Start [format=I10]: '+sampnostart
     printf,aklun,'Stripchart APID '+apid+' Sample Number Stop [format=I10]: '+sampnostop
     printf,aklun,'Stripchart APID '+apid+' Start Time [format=F16.5]: ',starttime,format=strf
     printf,aklun,'Stripchart APID '+apid+' Stop Time [format=F16.5]: ',stoptime,format=stpf
;     printf,aklun,'Stripchart APID '+apid+' Note [format=A80]: '
     
     if i eq 2 then begin 
        ahkstart=starttime
        ahkstop=stoptime
     endif 
  endfor 
  
  ;;errors
  files=filebegin+['_errorsVC0','_errors_tdrss','_errors','_errors_tdrss_vc6']+'.txt'
  apids=['Malindi RT','TDRSS RT','Malindi SSR','TDRSS SSR']
  for i=0,3 do begin 
     whatline=0
     nerrors=0
     file=files[i]
     apid=apids[i]
     run='N'
     prt='N'
     starttime=''
     stoptime=''
     note=''
     f165='(a,a)'
     sf165=f165
     if exist(file) then begin 
        run='Y'
        f165='(a,F16.5)'
        sf165=f165
        openr,stlun,file,/get_lun
        line=''
        readf,stlun,line
        while (not EOF(stlun)) do begin 
           whatline=whatline+1
           if whatline eq 6 then begin
              chunks=str_sep(line,' ')
              date=strtrim(chunks[0],1)
              met=date2met(date)
              starttime=ntostr(long(met))+ntostr((met-long(met)))
           endif 
           readf,stlun,line
        endwhile
        
        chunks=str_sep(line,' ')
        date=strtrim(chunks[0],1)
        met=date2met(date)
        stoptime=ntostr(long(met))+ntostr((met-long(met)))
        close,stlun
        free_lun,stlun
     endif

     starttime=check_aktime(starttime*1D,f=strf)
     stoptime=check_aktime(stoptime*1D,f=stpf)
     
     printf,aklun,apid+' Scanerr Run? [format=A1,unit=Y/N]: '+run
     printf,aklun,apid+' Scanerr Print? [format=A1,unit=Y/N]: ',prt
     printf,aklun,apid+' Scanerr Start Time [format=F16.5]: ',starttime,format=strf
     printf,aklun,apid+' Scanerr Stop Time [format=F16.5]: ',stoptime,format=stpf
;     printf,aklun,apid+' Scanerr Note [format=A80]: '
     
  endfor 
  
  ;;modechange
  files=filebegin+['_modechangeVC0.0','_modechange_tdrss.0','_modechange.0','_modechange_tdrss_vc6.0']+'.modechange.log'
;  apids=['Malindi RT','TDRSS RT','Malindi SSR','TDRSS SSR']
  apids=['Malindi SSR','Malindi RT','TDRSS SSR','TDRSS RT']
  whatline=0
  for i=0,3 do begin 
     file=files[i]
     apid=apids[i]
     run='N'
     prt='N'
     starttime=''
     stoptime=''
     note=''
     f165='(a,a)'
     sf165=f165
     if exist(file) then begin 
        f165='(a,F16.5)'
        sf165=f165
        run='Y'
        openr,stlun,file,/get_lun
        line=''
        readf,stlun,line
        while (not EOF(stlun)) do begin 
           whatline=whatline+1
           if whatline eq 5 then begin
              chunks=str_sep(line,' ')
              date=strtrim(chunks[1],1)
              met=date2met(date)
              starttime=ntostr(long(met))+ntostr((met-long(met)))
           endif 
           readf,stlun,line
        endwhile
        chunks=str_sep(line,' ')
        date=strtrim(chunks[1],1)
        met=date2met(date)
        stoptime=ntostr(long(met))+ntostr((met-long(met)))
        close,stlun
        free_lun,stlun
     endif
     
     starttime=check_aktime(starttime*1D,f=strf)
     stoptime=check_aktime(stoptime*1D,f=stpf)
     
     printf,aklun,apid+' Modechange Run? [format=A1,unit=Y/N]: '+run
     printf,aklun,apid+' Modechange Print? [format=A1,unit=Y/N]: ',prt
     printf,aklun,apid+' Modechange Start Time [format=F16.5]: ',starttime,format=strf
     printf,aklun,apid+' Modechange Stop Time [format=F16.5]: ',stoptime,format=stpf
;     printf,aklun,apid+' Modechange Note [format=A80]: '
  endfor 
  
  ;;TDRSS analysis
  ;;position message  ;;centroiding error    
  
  filebase=['positionmessage','positionmessage_tdrss','centroidingerr','centroidingerr_tdrss','postagestamp','postagestamp_tdrss','lightcurve','lightcurve_tdrss','spectrum','spectrum_tdrss']
  files=filebegin+'_'+filebase+[replicate('.txt',4),'*.fits','*tdrss*.fits','*.fits','*tdrss*.fits','*.fits','*tdrss*.fits']
  notetype=['Position','Position','Centroid Error','Centroid Error','Postage Stamp','Postage Stamp','Lightcurve','Lightcurve','Spectrum','Spectrum']
  
  for i=0,n_elements(files)-1 do begin 
     file=files[i]
     file0=filebase[i]+'.0'
     run=['N','N']
     data=['N','N']
     isgrb=['N','N']
     prt='N'
     time=''
     targetid=''
     segno=''
     isgrb=strarr(2)
     vals=strarr(2,3)
     note=''
     f165='(a,a)'
     ngrb=0
     if exist(file) then begin
        if file_size(file) gt 0 then begin 
           run[0]='Y'
           data[0]='Y'
           isgrb[0]='Y'
           f165='(a,F16.5)'
           if i le 3 then begin 
              read_pars1,file,['Message secs','Target ID','Obs. Number'],vals
              time=ntostr(vals[*,0]*1D)
              targetid=ntostr(long(vals[*,1]))
              segno=ntostr(fix(vals[*,2]))
              s=size(vals)
              vals=strarr(s[1],s[2])
              vals[*,0]=time
              vals[*,1]=targetid
              vals[*,2]=segno
              ngrb=s[1]
              if n_elements(vals) gt 3 then begin
                 data[1]='Y'
                 run[1]='Y'
                 isgrb[1]='Y'
              endif 
           endif 
           if i ge 4 then begin
              pfiles=file_search(file)
              vals=strarr(n_elements(pfiles),3)
              for k=0,n_elements(pfiles)-1 do begin 
                 tmpfits=mrdfits(pfiles[k],0,hdr,/silent)
                 time=strtrim(sxpar(hdr,'TSTART'),1)
                 targetid=strtrim(sxpar(hdr,'TARGETID'),1)
                 segno=strtrim(sxpar(hdr,'OBS_SEG'),1)
                 vals[k,0]=time
                 vals[k,1]=targetid
                 vals[k,2]=segno
              endfor 
              data[1]='Y'
              run[1]='Y'
              isgrb[1]='Y'
           endif 
;           if i eq 1 then pvals=vals
;           if i eq 2 then ptvals=vals
;           if i eq 3 then cvals=vals
;           if i eq 4 then ctvals=vals
        endif
     endif 
     
     for j=0,1 do begin 
        jj='('+ntostr(j+1)+')'
        printf,aklun,'TDRSS Analysis '+file0+' Data?'+jj+' [format=A1,unit=Y/N]: '+data[j]
        printf,aklun,'TDRSS Analysis '+file0+' Run?'+jj+' [format=A1,unit=Y/N]: '+run[j]
        printf,aklun,'TDRSS Analysis '+file0+' Print?'+jj+' [format=A1,unit=Y/N]: ',prt,format='(a,a1)'
;        if ((i eq 4) or (i eq 5)) then printf,aklun,'TDRSS Analysis '+file0+' Verify FITS?'+jj+' [format=A1,unit=Y/N]: ','',format='(a,a1)'
;        if (i ge 6) then begin
;           printf,aklun,'TDRSS Analysis '+file0+' Verify State Time?'+jj+' [format=A1,unit=Y/N]: ',''
;           printf,aklun,'TDRSS Analysis '+file0+' Verify Stop Time?'+jj+' [format=A1,unit=Y/N]: ',''
;           printf,aklun,'TDRSS Analysis '+file0+' Verify Livetime?'+jj+' [format=A1,unit=Y/N]: ',''
;        endif 
        printf,aklun,'TDRSS Analysis '+file0+' Reports as a GRB?'+jj+' [format=A1,unit=Y/N]: '+isgrb[j]
        if ngrb gt 1 then begin 
           printf,aklun,'TDRSS Analysis '+file0+' Time'+jj+' [format=F16.5]: ',vals[j,0],format=f165
           printf,aklun,'TDRSS Analysis '+file0+' Target ID'+jj+' [format=I10]: '+vals[j,1]
           printf,aklun,'TDRSS Analysis '+file0+' Segment Number'+jj+' [format=I10]: '+vals[j,2]
        endif 
                                ;       if i mod 2 eq 1 then printf,aklun,'TDRSS Analysis '+notetype[i]+' Note'+jj+' [format=A80]: '
        
     endfor 
  endfor 
  
  ;;;science
  
  ;;PASS1
  file0=filebegin+'_science.0'
  fileall=filebegin+'_science.all'
  cat=''
  if exist(fileall) then begin
     file=fileall
     cat='Y'
  endif else begin
     file=file0
     cat='N'
  endelse 
  file=file+'.timeline'
  starttime=''
  verify=''
  report=''
  note=''
  report2=''
  starttime2=''
  targetid=''
  segno=''
  targetid2=''
  segno2=''
  f165='(a,a)'
  sf165=f165
  f165b=f165
  oneim=0
  scistop=''
  scistart=''
  if exist(file) then begin 
     openr,stlun,file,/get_lun
     line=''
     newldp=0
     while ((not eof(stlun)) and (newldp eq 0)) do begin 
;     for q=0,3 do readf,stlun,line
        readf,stlun,line
        if (strpos(line,'New LDP') ne -1) then begin 
           chunks=str_sep(line,' ')
           w=where(chunks ne '')
           chunks=chunks[w]
           scistart=chunks[4]
           newldp=1
        endif 
     endwhile 
;     endif else begin 
;        for q=0,3 do readf,stlun,line
;        chunks=str_sep(line,' ')
;        w=where(chunks ne '')
;        chunks=chunks[w]
;        scistart=chunks[4]
;     endelse 
     
     while (not EOF(stlun)) do begin 
        chunks=str_sep(line,' ')
        w=where(chunks ne '',nw)
        if nw gt 0 then chunks=chunks[w]
        if (strpos(line,'Target ID') ne -1 ) then begin
           if chunks[13] gt 100000 then begin 
              if oneim eq 0 then begin 
                 targetid=chunks[13]
                 segno=chunks[16]
              endif else begin
                 targetid2=chunks[13]
                 segno2=chunks[16]
              endelse 
           endif 
        endif 
        if strpos(line,' IM') ne -1 then begin
           f165='(a,F16.5)'
           sf165=f165
           if oneim eq 0 then starttime=chunks[4] else begin 
              starttime2=chunks[4]*1D
              report2='Y'
              f165b='(a,F16.5)'
           endelse 
           if strpos(starttime,'-') ne -1 then starttime=chunks[3]*1D
           report='Y'
           oneim=1
        endif 
        if oneim eq 1 then begin
           targetid2=''
           segno2=''
        endif 
        if n_elements(chunks) eq 14 then oldline=line
        readf,stlun,line
     endwhile
     
     chunks=str_sep(oldline,' ')
     w=where(chunks ne '')
     chunks=chunks[w]
     if n_elements(chunks) ge 5 then scistop=chunks[4]
     
     close,stlun
     free_lun,stlun
  endif
  
  starttime=check_aktime(starttime*1D,f=strf)
  starttime2=check_aktime(starttime2*1D,f=strf2)
  printf,aklun,'Science Data Start Time [format=F16.5]: '
  printf,aklun,'Science Data Stop Time [format=F16.5]: '
  printf,aklun,'Science Data pass1 science.0 Cat? [format=A1,unit=Y/N]: '+cat
  printf,aklun,'Science Data pass1 science.0 Print? [format=A1,unit=Y/N]: ',prt
  printf,aklun,'Science Data pass1 science.0 Verify? [format=A1,unit=Y/N]: '+verify
  printf,aklun,'Science Data pass1 science.0 Reports as a New GRB?(1) [format=A1,unit=Y/N]: '+report
  printf,aklun,'Science Data pass1 science.0 GRB Image Time(1) [format=F16.5]: ',starttime,format=strf
  printf,aklun,'Science Data pass1 science.0 Target ID(1) [format=I10]: '+targetid
  printf,aklun,'Science Data pass1 science.0 Segment Number(1) [format=I10]: '+segno
  printf,aklun,'Science Data pass1 science.0 Reports as a New GRB?(2) [format=A1,unit=Y/N]: '+report2
  printf,aklun,'Science Data pass1 science.0 GRB Image Time(2) [format=F16.5]: ',starttime2,format=strf2
  printf,aklun,'Science Data pass1 science.0 Target ID(2) [format=I10]: '+targetid2
  printf,aklun,'Science Data pass1 science.0 Segment Number(2) [format=I10]: '+segno2
;  printf,aklun,'Science Data pass1 science.0 Note [format=A80]: '
  
  
  ;;check programs 
  
  files=['mean_bias_summary.xls','check_row_bias.xls','raw_spec_result_summary.xls','check_posta.xls','pc_spec_result_summary.xls']
  mindex=[1,5,2,1,6]
  
  modes=['BIAS','BIAS','RAW','POSTA','PC']
  fnames=['bias_map.fits','bias_row.fits','raw_image.fits','postagestamp.0','pc_events.fits']
  
  for i=0,n_elements(files)-1 do begin 
     file=files[i]
     mode=modes[i]
     fname=fnames[i]
     tfile=filebegin+'_science.0.timeline'
     data='N'
     ammode=''
     line=''
     excel='N'
     mean=''
     sigma=''
     fwhm=''
     cal=''
     if exist(tfile) then begin
                                ;get modes (AUTO/MAN)
        openw,alun,outdir+'awktmp',/get_lun
        printf,alun,'/'+mode+'/{print}'
        close,alun
        free_lun,alun
        spawn,'awk -f '+outdir+'awktmp '+tfile+' > '+outdir+'awktmp.txt'
        spawn,'rm '+outdir+'awktmp'
        if (file_size(outdir+'awktmp.txt') gt 0) then begin 
           openr,alun,outdir+'awktmp.txt',/get_lun
           readf,alun,line ;assumes same mode thoughout data set, uses first mode
           chunk=str_sep(line,' ')
           w=where(chunk ne '')
           chunk=chunk[w]
           ammode=chunk[3]
;           print,ammode,fnames[i]
;           if i eq 2 then stop
           if ((ammode ne 'Auto') and (ammode ne 'Manual')) then begin 
              if (chunk[2] eq 'Auto') or (chunk[2] eq 'Manual') then $
                 ammode=chunk[2] else ammode='' 
           endif 
           if ammode eq 'Manual' then ammode='Man'
        endif 
        spawn,'rm '+outdir+'awktmp.txt'
        if exist(file) then begin 
           data='Y'
           excel='Y'
           line=''
           openr,flun,file,/get_lun
           readf,flun,line
           if not eof(flun) then begin 
              readf,flun,line
              chunk=str_sep(line,',')
              w=where(chunk ne '')
              chunk=chunk[w]
              mean=strtrim(chunk[mindex[i]],2)
              sigma=strtrim(chunk[mindex[i]+1],2)
              
              if (i eq 2) and (n_elements(chunk) ge 8 )then begin 
                 fwhm=strtrim(chunk[7],2)
                 cal=strtrim(chunk[6],2)
              endif
              if (i eq 4)  and (n_elements(chunk) ge 19 )then begin 
                 fwhm=strtrim(chunk[9],2)
                 cal=strtrim(chunk[8],2)
              endif 
           endif 
           while not eof(flun) do begin 
              chunk=readline(flun,delim=',')
              if ((i eq 0) and (n_elements(chunk) eq 4)) or ((i eq 1) and (n_elements(chunk) eq 2)) then begin 
                 
                 mean=strtrim(chunk[0],2)
                 sigma=strtrim(chunk[1],2)
              endif 
           endwhile
;else begin 
;              mean=''
;              sigma=''
;              data='N'
;              fwhm=''
;              cal=''
;           endelse 
           close,flun
           free_lun,flun
        endif 
     endif 
     
     printf,aklun,'Science Mode Analysis '+fname+' Data? [format=A1,unit=Y/N]: '+data
     printf,aklun,'Science Mode Analysis '+fname+' Mode? [format=A4,unit=Auto/Man]: '+ammode
     printf,aklun,'Science Mode Analysis '+fname+' Excel? [format=A1,unit=Y/N]: ',excel
     printf,aklun,'Science Mode Analysis '+fname+' Spreadsheet? [format=A1,unit=Y/N]: ',excel,format='(a,a1)'
     if ((i ne 2) and (i ne 4)) then begin 
        printf,aklun,'Science Mode Analysis '+fname+' Mean [format=F16.4]: '+mean
        printf,aklun,'Science Mode Analysis '+fname+' Sigma [format=F16.4]: '+sigma
     endif else begin 
        if i eq 4 then begin 
           printf,aklun,'Science Mode Analysis '+fname+' FWHM [format=F16.4]: '+fwhm
           printf,aklun,'Science Mode Analysis '+fname+' Cal [format=F16.4]: '+cal
        endif 
        
        printf,aklun,'Science Mode Analysis '+fname+' Noise Mean [format=F16.4]: '+mean
        printf,aklun,'Science Mode Analysis '+fname+' Noise Sigma [format=F16.4]: '+sigma
        printf,aklun,'Science Mode Analysis '+fname+' FWHM [format=F16.4]: '+fwhm
        printf,aklun,'Science Mode Analysis '+fname+' Cal [format=F16.4]: '+cal
     endelse 
  endfor 
  
  ;;RAW IMAGE
  ;;cannot do some parameters automatically!!
  
  if exist(fileall) then file=fileall else file=file0
  sfile='*raw*image*'
  ldps=strarr(3)
  frno=strarr(3)
  amps=strarr(3)
  if exist(sfile) then begin   
;     rfile=file_search(sfile+'2.fits')
;     w=where(rfile ne '',nrfile)
     rfile=findfile(sfile)
     nrfile=n_elements(rfile)
     if nrfile gt 0 then begin 
        for i=0,nrfile-1 do begin
           ldppos=strpos(rfile[i],'LDP')
           ldppos2=strpos(rfile[i],'.raw')
           ldp=strmid(rfile[i],ldppos+3,ldppos2-ldppos-3)
;           ldp=strsplit(ldp,'.',/ex)
           ldps[i]=ldp
           frpos=strpos(rfile[i],'image_')
           fr=strmid(rfile[i],frpos+6,2)
           fr=strsplit(fr,'.',/ex)
           frno[i]=fr
           rf=mrdfits(rfile[i],0,rhdr,/silent)
           amps[i]=strtrim(sxpar(rhdr,'AMP'),1)
        endfor 
     endif 
  endif 
  
  sfile='raw_spec_result_summary.xls'
  singpix=['','','']
  fwhm=singpix
  noise=singpix
  if exist(sfile) then begin
     openr,lun,sfile,/get_lun
     line=readline(lun)
     i=0
     while not eof(lun) do begin
        line=readline(lun,delim=',')
        singpix[i]=line[6]
        fwhm[i]=line[7]
        noise[i]=line[9]
        i=i+1
     endwhile
     close,lun
     free_lun,lun
  endif 
  
  for i=0,2 do begin
     if singpix[i] ne '' then begin 
        sp=singpix[i]*1.
        form='(a,F10.4)'
        fw=fwhm[i]*1.
        no=noise[i]*1.
     endif 

     printf,aklun,'Raw Data LDP('+ntostr(i+1)+') [format=I10]: '+ldps[i]
     printf,aklun,'Raw Data Frame Number('+ntostr(i+1)+') [format=I10]: '+frno[i]
     printf,aklun,'Raw Data Gain('+ntostr(i+1)+') [format=A2,unit=Hi/Lo]: '
     printf,aklun,'Raw Data Amp('+ntostr(i+1)+') [format=I2,unit=1/2]: '+amps[i]
     printf,aklun,'Raw Data Single Pixel('+ntostr(i+1)+') [format=F10.4]: ',sp,format=form
     printf,aklun,'Raw Data FWHM('+ntostr(i+1)+') [format=F10.4]: ',fw,format=form
     printf,aklun,'Raw Data Noise('+ntostr(i+1)+') [format=F10.4]: ',no,format=form
  endfor 
  
  ;;Filter Integrity
  
  ldps=strarr(4)
  backdn=strarr(4)
  amps=strarr(4)
  if exist(sfile) then begin   
     rfile=file+'_LDP*.raw_image_1.fits'
     rfile=file_search(rfile)
     w=where(rfile ne '',nrfile)
     for i=0,nrfile-1 do begin
        ldppos=strpos(rfile[i],'LDP')
        ldppos2=strpos(rfile[i],'.raw')
        ldp=strmid(rfile[i],ldppos+3,ldppos2-ldppos-3)
;        ldp=strsplit(ldp,'.',/ex)
        ldps[i]=ldp
        backdn[i]=strtrim(sxpar(rhdr,'MEDIAN'),2)
        amps[i]=strtrim(sxpar(rhdr,'AMP'),2)
     endfor 
  endif 
  
  for i=0,3 do begin 
     ldp=ldps[i]
     amp=amps[i]
     bd=backdn[i]
     
     printf,aklun,'Filter Integrity LDP('+ntostr(i+1)+') [format=I10]: '+ldp
     printf,aklun,'Filter Integrity LED('+ntostr(i+1)+') [format=A3,unit=ON/OFF]: '
     printf,aklun,'Filter Integrity Gain('+ntostr(i+1)+') [format=A2,unit=Hi/Lo]: '
     printf,aklun,'Filter Integrity Amp('+ntostr(i+1)+') [format=I2,unit=1/2]: '+amp
     printf,aklun,'Filter Integrity Back DN('+ntostr(i+1)+') [format=F10.4]: '+bd
     printf,aklun,'Filter Integrity Note('+ntostr(i+1)+') [format=A80]: '
  endfor 
  
  
  ;;;TAM
  ;;TAM-TWO-WIN
  
  file=filebegin+'_tam_two_win.0.log'
  
  tamdata='N'
  tamrun='N'
  tam2starttime=''
  tam2stoptime=''
  done=0
  x1=''
  x2=''
  y1=''
  y2=''
  sig1=''
  sig2=''
  f165='(a,a)'
  sf165=f165
  if exist(file) then begin 
     if (file_size(file) gt 0) then begin 
        f165='(a,F16.5)'
        sf165=f165
        tamdata='Y'
        tamrun=tamdata
        openr,tlun,file,/get_lun
        line=''
        readf,tlun,line
        while (not EOF(tlun)) do begin 
           if strpos(line,'First TAM time:') ne -1 then begin 
              chunks=str_sep(line,' ')
              w=where(chunks ne '')
              chunks=chunks[w]
              tam2starttime=chunks[3]*1D
           endif 
           if strpos(line,'Last TAM time:') ne -1 then begin 
              chunks=str_sep(line,' ')
              w=where(chunks ne '')
              chunks=chunks[w]
              tam2stoptime=chunks[3]*1D
           endif 
           if (strpos(line,'TAM Window 1 centroid, std dev:') ne -1) and (done eq 0) then begin 
              chunks=str_sep(line,' ')
              w=where(chunks ne '')
              chunks=chunks[w]
              x1=chunks[6]
              y1=chunks[7]
              sig1=chunks[8]
              readf,tlun,line
              chunks=str_sep(line,' ')
              w=where(chunks ne '')
              chunks=chunks[w]
              x2=chunks[6]
              y2=chunks[7]
              sig2=chunks[8]
              done=1
           endif 
           readf,tlun,line
        endwhile
        close,tlun
        free_lun,tlun
     endif 
  endif   

  filexls='tam2win_spec_result_summary.xls'
  led=''
  mean1=''
  mean2=''
  bsig1=''
  bsig2=''
  if exist(filexls) then begin
     if file_size(filexls) gt 0 then begin 
        openr,xlun,filexls,/get_lun
        line=''
        readf,xlun,line
        go=0
        done1=0
        while ((not eof(xlun)) and (go eq 0)) do begin 
           readf,xlun,line
           if (strpos(line,'win1') ne -1 and (done1 eq 0)) then begin 
              chunks=strtrim(str_sep(line,','),1)
              w=where(chunks ne '')
              chunks=chunks[w]
              led=chunks[5]
              thresh=chunks[1]
              mean1=chunks[2]
              bsig1=chunks[3]
              done1=1
           endif 
           if strpos(line,'win2') ne -1 then begin
              chunks=strtrim(str_sep(line,','),1)
              w=where(chunks ne '')
              chunks=chunks[w]
              mean2=chunks[2]
              bsig2=chunks[3]
              go=1
           endif 
        endwhile 
        close,xlun
        free_lun,xlun
     endif 
  endif 
  
  tam2starttime=check_aktime(tam2starttime*1D,f=strf)
  tam2stoptime=check_aktime(tam2stoptime*1D,f=stpf)
  
  printf,aklun,'TAM Two-Window Mode Data? [format=A1,unit=Y/N]: '+tamdata
  printf,aklun,'TAM Two-Window Mode Run? [format=A1,unit=Y/N]: '+tamrun
  printf,aklun,'TAM Two-Window Mode Print? [format=A1,unit=Y/N]: ',prt
  printf,aklun,'TAM Two-Window Mode Start Time [format=F16.5]: ',tam2starttime,format=strf
  printf,aklun,'TAM Two-Window Mode Stop Time [format=F16.5]: ',tam2stoptime,format=stpf
  printf,aklun,'TAM Two-Window Mode X1 [format=F10.4]: '+x1
  printf,aklun,'TAM Two-Window Mode Y1 [format=F10.4]: '+y1
  printf,aklun,'TAM Two-Window Mode Sig1 [format=F10.4]: '+sig1
  printf,aklun,'TAM Two-Window Mode X2 [format=F10.4]: '+x2
  printf,aklun,'TAM Two-Window Mode Y2 [format=F10.4]: '+y2
  printf,aklun,'TAM Two-Window Mode Sig2 [format=F10.4]: '+sig2
  printf,aklun,'TAM Two-Window Mode LED [format=I1,unit=0/1/2]: '+led
  printf,aklun,'TAM Two-Window Mode Back DN1 [format=F10.4]: '+mean1
  printf,aklun,'TAM Two-Window Mode Back Sig1 [format=F10.4]: '+bsig1
  printf,aklun,'TAM Two-Window Mode Back DN2 [format=F10.4]: '+mean2
  printf,aklun,'TAM Two-Window Mode Back Sig2 [format=F10.4]: '+bsig2
  
  
  ;;TAM-FULL-FRAME
  
  file=filebegin+'_tam_full_frame.0.log'
  
  tamdata=strarr(3)
  tamrun=strarr(3)
  tamdata[*]='N'
  tamrun[*]='N'
  tamstarttime=''
  tamstoptime=''
  done=0
  f165='(a,a)'
  sf165=f165
  if (exist(file)) then begin 
     f165='(a,F16.5)'
     sf165=f165
     openr,tlun,file,/get_lun
     line=''
     readf,tlun,line
     go=0
     while ((not EOF(tlun)) and (go eq 0)) do begin 
        if strpos(line,'First TAM time:') ne -1 then begin 
           chunks=str_sep(line,' ')
           w=where(chunks ne '')
           chunks=chunks[w]
           tamstarttime=chunks[3]*1D
        endif 
        if strpos(line,'Last TAM time:') ne -1 then begin 
           chunks=str_sep(line,' ')
           w=where(chunks ne '')
           chunks=chunks[w]
           tamstoptime=chunks[3]*1D
           go=1
        endif 
        readf,tlun,line
     endwhile
  endif 
  
  fileimg=filebegin+'_tam_full_frame.0.frame'+ntostr([1,2,3])+'.fits.gz'
  for i=0,2 do begin
     if exist(fileimg[i]) then begin 
        tamdata[i]='Y'
        tamrun[i]='Y'
     endif 
  endfor 
  
  led=''
  bmean=''
  bsigma=''
  thresh=''
  athresh=''
  x1=''
  y1=''
  dn1=''
  x2=''
  y2=''
  dn2=''
  filexls='tamff_spec_result_summary.xls'
  if exist(filexls) and file_size(filexls) gt 0 then begin
     openr,xlun,filexls,/get_lun
     line=''
     go=0
     while ((not eof(xlun)) and (go eq 0)) do begin 
        readf,xlun,line
        readf,xlun,line
        chunks=strtrim(str_sep(line,','),1)
        w=where(chunks ne '',nw)
        chunks=chunks[w]
        if nw ge 6 then begin 
           led=chunks[5]
           thresh=chunks[1]
           bmean=chunks[2]
           bsigma=chunks[3]
           athresh=chunks[4]
           x1=chunks[6]
           y1=chunks[7]
           dn1=chunks[8]
           x2=chunks[9]
           y2=chunks[10]
           dn2=chunks[11]
           go=1
        endif 
     endwhile 
     close,xlun
     free_lun,xlun
  endif 
  
  tamstarttime=check_aktime(tamstarttime,f=strf)
  tamstoptime=check_aktime(tamstoptime,f=stpf)
  
  printf,aklun,'TAM Full-Frame Mode Data? [format=A1,unit=Y/N]: '+tamdata[0]
  printf,aklun,'TAM Full-Frame Mode Run? [format=A1,unit=Y/N]: '+tamrun[0]
  printf,aklun,'TAM Full-Frame Mode Print? [format=A1,unit=Y/N]: ',prt
  printf,aklun,'TAM Full-Frame Mode Start Time [format=F16.5]: ',tamstarttime,format=strf
  printf,aklun,'TAM Full-Frame Mode Stop Time [format=F16.5]: ',tamstoptime,format=stpf
  printf,aklun,'TAM Full-Frame Mode X1 [format=F10.4]: ',x1
  printf,aklun,'TAM Full-Frame Mode Y1 [format=F10.4]: ',y1
  printf,aklun,'TAM Full-Frame Mode DN1 [format=F10.4]: ',dn1
  printf,aklun,'TAM Full-Frame Mode X2 [format=F10.4]: ',x2
  printf,aklun,'TAM Full-Frame Mode Y2 [format=F10.4]: ',y2
  printf,aklun,'TAM Full-Frame Mode DN2 [format=F10.4]: ',dn2
  printf,aklun,'TAM Full-Frame Mode LED [format=I1,unit=0/1/2]: '+led
  printf,aklun,'TAM Full-Frame Mode Background Mean [format=F10.4]: '+bmean
  printf,aklun,'TAM Full-Frame Mode Background Sigma [format=F10.4]: '+bsigma
  printf,aklun,'TAM Full-Frame Mode Threshold [format=F10.4]: '+thresh
  printf,aklun,'TAM Full-Frame Mode Number of Pixels Above Threshold [format=F10.4]: '+athresh
  
  
  ;;Data Statistics
  
  ahktime=double(ahkstop)-double(ahkstart) > 0D
  tam2time=double(tam2stoptime)-double(tam2starttime) > 0D
  tamfulltime=double(tamstoptime)-double(tamstarttime) > 0D
  scitime=double(scistop)-double(scistart) > 0D
  
  rate_calcs,scitime,tamfulltime,tam2time,ahktime,scisize,scirate,tamfullsize,tamfullrate,tam2size,tam2rate,ahksize,ahkrate,otherhksize,otherhkrate,/silent
  
  fahk=['(a,F16.5)','(a,F10.4)']
  ftam2=fahk 
  ftamff=fahk
  fsci=fahk
  
  if ahktime eq 0 then begin 
     fahk[*]='(a,a)'
     ahktime=''
  endif 
  if tam2time eq 0 then begin
     ftam2[*]='(a,a)'
     tam2time=''
  endif 
  if tamfulltime eq 0 then begin
     ftamff[*]='(a,a)'
     tamfulltime=''
  endif 
  if scitime eq 0 then begin
     fsci[*]='(a,a)'
     scitime=''
  endif 
  
  printf,aklun,'Data Statistics: AHK Data Volume [format=F10.4]: ',(ahksize),format='(a,f10.4)'
  printf,aklun,'Data Statistics: AHK Data Rate [format=F10.4]: ',(ahkrate),format='(a,f10.4)'
  printf,aklun,'Data Statistics: AHK Data Start Time [format=F16.5]: ',(ahkstart),format=fahk[0]
  printf,aklun,'Data Statistics: AHK Data Stop Time [format=F16.5]: ',(ahkstop),format=fahk[0]
  printf,aklun,'Data Statistics: AHK Data Total Time [format=F10.4]: ',(ahktime),format=fahk[1]
  printf,aklun,'Data Statistics: TAM Two-Window Data Volume [format=F10.4]: ',(tam2size),format='(a,f10.4)'
  printf,aklun,'Data Statistics: TAM Two-Window Data Rate [format=F10.4]: ',(tam2rate),format='(a,f10.4)'
  printf,aklun,'Data Statistics: TAM Two-Window Data Start Time [format=F16.5]: ',(tam2starttime),format=ftam2[0]
  printf,aklun,'Data Statistics: TAM Two-Window Data Stop Time [format=F16.5]: ',(tam2stoptime),format=ftam2[0]
  printf,aklun,'Data Statistics: TAM Two-Window Data Total Time [format=F10.4]: ',(tam2time),format=ftam2[1]
  printf,aklun,'Data Statistics: TAM Full-Frame Data Volume [format=F10.4]: ',(tamfullsize),format='(a,f10.4)'
  printf,aklun,'Data Statistics: TAM Full-Frame Data Rate [format=F10.4]: ',(tamfullrate),format='(a,f10.4)'
  printf,aklun,'Data Statistics: TAM Full-Frame Data Start Time [format=F16.5]: ',(tamstarttime),format=ftamff[0]
  printf,aklun,'Data Statistics: TAM Full-Frame Data Stop Time [format=F16.5]: ',(tamstoptime),format=ftamff[0]
  printf,aklun,'Data Statistics: TAM Full-Frame Data Total Time [format=F10.4]: ',(tamfulltime),format=ftamff[1]
  printf,aklun,'Data Statistics: Science Data Volume [format=F10.4]: ',(scisize),format='(a,f10.4)'
  printf,aklun,'Data Statistics: Science Data Rate [format=F10.4]: ',(scirate),format='(a,f10.4)'
  printf,aklun,'Data Statistics: Science Data Start Time [format=F16.5]: ',(scistart),format=fsci[0]
  printf,aklun,'Data Statistics: Science Data Stop Time [format=F16.5]: ',(scistop),format=fsci[0]
  printf,aklun,'Data Statistics: Science Data Total Time [format=F10.4]: ',(scitime),format=fsci[1]
  printf,aklun,'Data Statistics: Other HK Data Volume [format=F10.4]: ',(otherhksize),format='(a,f10.4)'
  printf,aklun,'Data Statistics: Other HK Data Rate [format=F10.4]: ',(otherhkrate),format='(a,f10.4)'
  printf,aklun,'Data Statistics: Other HK Data Start Time [format=F16.5]: ',(ahkstart),format=fahk[0]
  printf,aklun,'Data Statistics: Other HK Data Stop Time [format=F16.5]: ',(ahkstop),format=fahk[0]
  printf,aklun,'Data Statistics: Other HK Data Total Time [format=F10.4]: ',(ahktime),format=fahk[1]
  printf,aklun,'Summary [format=A800]: '
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  printf,aklun,'<END>'
  close,/all
;stop                                ;send email ;pgp
  
  if keyword_set(submit) then begin
     print,'GPG signing akbar_email.txt'
     spawn,'gpg --clearsign < akbar_email.txt > akbar_email_signed.txt '
     print,'Submitting akbar_email_signed.txt to AKBAR server'
     spawn,'fastmail -r swiftxrt@gmail.com -f "XRT XDS" -F swiftxrt@gmail.com akbar_email_signed.txt akbar@dbms1.gsfc.nasa.gov'
  endif
  
  
  return
end 
