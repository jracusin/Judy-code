#!/bin/sh

old_idl_path="$IDL_PATH"
IDL_PATH="<IDL_DEFAULT>":+/bulk/pkg/xray/idl_lib/xrt_pass1
export IDL_PATH

#STRIPCHART AHK
idl<<EOF
print,'***************STRIPCHART AHK*****************'
get_file_begin,filebegin
file=filebegin+'_ahk.0'
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'Analysis began: ',systime(),' EDT'
printf,unit,''
printf,unit,'  Program        ','      File             ','                 Run? ','Print?'
run='  N   '
close,unit
free_lun,unit
if (file_size(file) gt 0) then begin & $ 
    stripchart,file,/noprint,/quicklook & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'stripchart       ',file+'                  ',run,'  N'
close,unit
EOF

wait

#STRIPCHARK VC0AHK
idl<<EOF
print,'***************STRIPCHART AHKVC0.0************'
get_file_begin,filebegin
file=filebegin+'_ahkVC0.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    stripchart,file,/noprint,/quicklook & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'stripchart       ',file+'               ',run,'  N'
close,unit
EOF

wait

#STRIPCHART AHK_TDRSS_VC6
idl<<EOF
print,'***************STRIPCHART AHK_TDRSS_VC6*************'
get_file_begin,filebegin
file=filebegin+'_ahk_tdrss_vc6.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    stripchart,file,/noprint,/quicklook & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'stripchart       ',file+'        ',run,'  N'
close,unit

EOF

wait

#STRIPCHART TDRSSHK
idl<<EOF
print,'***************STRIPCHART AHK_TDRSSSHK*************'
get_file_begin,filebegin
file=filebegin+'_tdrsshk.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    stripchart,file,/noprint,/quicklook & $
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'stripchart       ',file+'              ',run,'  N'
close,unit

EOF

wait 

#ERRORS
idl<<EOF
print,'***************SCANERR & PARS1****************************'
get_file_begin,filebegin
file=filebegin+'_errors.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
   filetxt=filebegin+'_errors.txt' & $
   filepars=filebegin+'_errors.pars1.txt' & $
   print,filetxt & $
   print,filepars & $
   spawn,'scanerr -d '+file+' > '+filetxt & $
   spawn,'pars1 '+file+' > '+filepars & $
   run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'scanerr          ',file+'               ',run,'  N'
printf,unit,'pars1            ',file+'               ',run,'  N'
close,unit
EOF

#scanerr -d *errors.0 > *errors.txt
#pars1 *errors.0 > *errors.pars1.txt

#a2ps -1 -r --font-size=8 -Psneezy *errors.txt

wait 

#MODECHANGE
idl<<EOF
print,'***************MODECHANGE*************'
get_file_begin,filebegin
file=filebegin+'_modechange.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    modechange,file,/noprint,/quicklook & $
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'modechange       ',file+'           ',run,'  N'
close,unit
EOF

wait

#PASS1
idl<<EOF
print,'*********************PASS1************************'
get_file_begin,filebegin
file=filebegin+'_science.'
i=0
catstring='cat '
file0=file+'0'
file2=file0+' '
cat=0
run='  N   '
if (file_size(file0) gt 0) then begin & $
   while exist(file2) do begin & $
      file2=file+ntostr(i) & $
      catstring=catstring+file2+' ' & $
      i=i+1 & $
      file2=file+ntostr(i) & $
      if (i eq 2) then cat=1 & $
   endwhile & $
   if cat then begin & $
      catstring=catstring+'> '+file+'all' & $
      print,catstring & spawn,catstring & $
      pfile=file+'all' & $
   endif else pfile=file0 & $
   run='  Y   ' & $
   pass1,pfile,/quick & $

   tfile=pfile+'.timeline' & $
   if (file_size(tfile) gt 300) then begin & $
       openw,alun,'awk.txt',/get_lun & $
       printf,alun,'/New LDP/{print}' & $
       printf,alun,'/Product/{print}' & $
       printf,alun,'/IM/{print}' & $
       printf,alun,'/RAW/{print}' & $
       close,alun & $
       spawn,'awk -f awk.txt '+tfile+' > '+pfile+'.short.timeline' & $
       spawn,'rm awk.txt' & $
       tfile=pfile+'.short.timeline' & $ 
   endif & $
endif

openu,unit,'analysis.log',/get_lun,/append
printf,unit,'pass1            ',file+'all            ',run,'  N'
close,unit
EOF

wait 

#TDRSS ANALYSIS

idl<<EOF
print,'*********************TDRSS ANALYSIS************************'
get_file_begin,filebegin
file1=filebegin+'_positionmessage.0'
openu,unit,'analysis.log',/get_lun,/append
run='  N   '
if (file_size(file1) gt 0) then begin & $
   file1txt=filebegin+'_positionmessage.txt' & $
   print,file1,file1txt & $
   spawn,'pars1 '+file1+' > '+file1txt & $
   run='  Y   ' & $
endif 
printf,unit,'pars1            ',file1+'      ',run,'  N'

run='  N   '
file2=filebegin+'_positionmessage_tdrss.0'
if (file_size(file2) gt 0) then begin & $
   file2txt=filebegin+'_positionmessage_tdrss.txt' & $
   print,file2,file2txt & $
   spawn,'pars1 '+file2+' > '+file2txt & $
   run='  Y   ' & $
endif 
printf,unit,'pars1            ',file2,run,'  N'

run='  N   '
file3=filebegin+'_centroidingerror.0'
if (file_size(file3) gt 0) then begin & $ 
   file3txt=filebegin+'_centroidingerror.txt' & $
   print,file3,file3txt & $
   spawn,'pars1 '+file3+' > '+file3txt & $
   run='  Y   ' & $
endif
printf,unit,'pars1            ',file3+'     ',run,'  N'

run=' N   '
file4=filebegin+'_centroidingerror_tdrss.0'
if (file_size(file4) gt 0) then begin  & $
   file4txt=filebegin+'_centroidingerror_tdrss.txt' & $
   print,file4,file4txt & $
   spawn,'pars1 '+file4+' > '+file4txt & $
   run=' Y   ' & $
endif
printf,unit,'pars1            ',file4,run,' '+run
close,unit
EOF

wait

idl<<EOF
get_file_begin,filebegin
file=filebegin+'_postagestamp.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    postage_stamp,file,/quick & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'postage_stamp    ',file+'         ',run,'  N'
close,unit
free_lun,unit

file=filebegin+'_postagestamp_tdrss.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    postage_stamp,file,/quick & $
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'postage_stamp    ',file+'   ',run,'  N'
close,unit
free_lun,unit

file=filebegin+'_lightcurve.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    plc,file,/quick & $
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'plc              ',file+'           ',run,'  N'
close,unit
free_lun,unit

file=filebegin+'_lightcurve_tdrss.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    plc,file,/quick & $ 
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'plc              ',file+'     ',run,'  N'
close,unit
free_lun,unit

file=filebegin+'_spectrum.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    xrt_spectrum,file,/quick & $
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'xrt_spectrum     ',file+'             ',run,'  N'
close,unit
free_lun,unit

file=filebegin+'_spectrum_tdrss.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    xrt_spectrum,file,/quick & $ 
    run='  Y   ' & $
endif
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'xrt_spectrum     ',file+'       ',run,'  N'
close,unit
EOF

wait

#SCIENCE DATA ANALYSIS
idl<<EOF
print,'**********************BIAS MAP*****************************'
get_file_begin,filebegin
file=filebegin+'*bias*map*'
run='  N   '
if exist(file) then begin & $ 
    spawn,'gunzip *bias*map*' & $
    checkb & $ 
    spawn,'gzip *bias*map*' & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'checkb           ',file+'              ',run,' N/A'
close,unit
free_lun,unit

print,'**********************BIAS ROW*****************************'
get_file_begin,filebegin
file=filebegin+'*bias*row*'
run='  N   '
if exist(file) then begin & $ 
    check_rowb & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'check_rowb       ',file+'              ',run,' N/A'
close,unit
free_lun,unit

print,'**********************RAW IMAGE**************************'
get_file_begin,filebegin
file=filebegin+'*raw*image*'
run='  N   '
ri=''
if exist(file) then begin & $
    spawn,'gunzip *raw*image*' & $
    wait,1 & $
    raw_filter,/quick & $
    wait,1 & $
    spawn,'gzip *raw*image*' & $
    print,'Do secondary raw image analysis' & $
    run='  Y   ' & $
    ri='  **NEEDS RAW IMAGE ANALYSIS**' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'raw_filter       ',file+'             ',run,' N/A',ri
close,unit
free_lun,unit

print,'***********************IMAGING****************************'
get_file_begin,filebegin
file=filebegin+'*im*image*'
run='  N   '
if exist(file) then begin & $ 
    checksp_im,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'checksp_im       ',file+'              ',run,' N/A'
close,unit
free_lun,unit

wait,1
print,'*********************PILED UP PHOTODIODE*******************'
get_file_begin,filebegin
file=filebegin+'*pupd*'
run='  N   '
if exist(file) then begin & $ 
    pupd_filter,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'pupd_filter      ',file+'                  ',run,' N/A'
close,unit
free_lun,unit
EOF

wait

idl<<EOF
print,'*******************LOW RATE PHOTODIODE********************'
get_file_begin,filebegin
file=filebegin+'*lrpd*events*'
run='  N   '
if exist(file) then begin & $ 
    checksp_lrpd,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'checksp_lrpd     ',file+'           ',run,' N/A'
close,unit
free_lun,unit

wait,1 
print,'*********************WINDOW TIMING*************************'
get_file_begin,filebegin
file=filebegin+'*wt*events*'
run='  N   '
if exist(file) then begin & $ 
    checksp_wt,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'checksp_wt       ',file+'             ',run,' N/A'
close,unit
free_lun,unit

print,'*********************PHOTON COUNTING************************'
get_file_begin,filebegin
file=filebegin+'*pc*events*'
run='  N   '
if exist(file) then begin & $ 
    checksp_pc,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'checksp_pc       ',file+'             ',run,' N/A'
close,unit
free_lun,unit

wait,1
print,'**********************CHECK_POSTA***************************'
get_file_begin,filebegin
file=filebegin+'_postagestamp.0'
run='  N   '
if (file_size(file) gt 0) then begin & $
    check_posta,/quick & $
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'check_posta      ',file+'         ',run,' N/A'
close,unit
EOF

wait

#TAM
idl<<EOF
print,'*************************TAM*********************************'
get_file_begin,filebegin
file=filebegin+'_tam_two_win.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    tam,file,/noprint,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'tam              ',file+'          ',run,'  N'
close,unit

EOF

wait

#TAM FULL FRAME
idl<<EOF
get_file_begin,filebegin
file=filebegin+'_tam_full_frame.0'
run='  N   '
if (file_size(file) gt 0) then begin & $ 
    tam,file,/noprint,/quick & $ 
    run='  Y   ' & $
endif 
openu,unit,'analysis.log',/get_lun,/append
printf,unit,'tam              ',file+'       ',run,'  N'

close,unit

;send_xrt_outputs

print
print,'       *******      *******     **   **     ******'
print,'       **    **     **   **     ***  **     **    '
print,'       **     **    **   **     **** **     ******'
print,'       **    **     **   **     ** ****     **    '
print,'       *******      *******     **   **     ******'   
print
EOF

IDL_PATH=$old_idl_path
export IDL_PATH
