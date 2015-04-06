pro grb_lc_script,grbname,tid,battime,wt,dir=dir,mydir=mydir,skip=skip

  if n_params() eq 0 then begin 
     print,'syntax - grb_lc_script,grbname,targid,bat_time,WT code (0=use WT as is, 1=no WT,2=filter WT)'
     return
  end 

  ngrbs=n_elements(grbname)
  if not keyword_set(dir) then begin 
     if keyword_set(mydir) then begin
        cd,!grbs 
        dir='grb'+grbname   
     endif else begin 
        cd,!mdata
        dir='GRB'+grbname   
     endelse 
  endif 
  g=0

  print,dir

  if not exist(dir) then spawn,'mkdir '+dir
  cd,dir
  outdir=file_search('00*-xrt')

  year='20'+strmid(grbname,0,2)
  month=strmid(grbname,2,2)*1
  day=strmid(grbname,4,2)
  if outdir[0] eq '' then begin 
     get_sdc_data,year,month,tid
     
     if year ge 06*1 and month*1 ge 04 then teldef=1 else teldef=0
     
     run_xrtpipeline,/datamode
  endif 
;  if not keyword_set(skip) then begin 
  wtfile=findfile('00*-xrt/sw*wt*po*cl*.evt')

  if wt eq 2 then begin 
     wtpos=strpos(wtfile,'ff')
     wpos=where(wtpos ne -1)
     if wpos[0] eq -1 then begin
        print,'Need to filter WT file'
        openw,xlun,'xsel_filt_wt.batch',/get_lun
        printf,xlun
        printf,xlun,'cpd /xw'
        printf,xlun,'read e'
        printf,xlun,'./'
        printf,xlun,wtfile[0]
        printf,xlun
        printf,xlun,'ext cu'
        printf,xlun
        printf,xlun,'filter time cu'
        printf,xlun
        printf,xlun,'q'
        printf,xlun
        printf,xlun,'ext ev'
        tmp=str_sep(wtfile[0],'.evt')
        wtfile1=tmp[0]+'_ff.evt'
        printf,xlun,'save ev '+wtfile1
        printf,xlun
        printf,xlun
        printf,xlun,'exit'
        printf,xlun
        close,xlun
        free_lun,xlun
        spawn,'xselect @xsel_filt_wt.batch'
        spawn,'fappend '+wtfile[0]+'+3 '+wtfile1
        wtfile=wtfile1
        wpos=0
     endif else begin 
        wtfile1=wtfile[wpos]
        fits_info,wtfile1,/silent,n_ext=n_ext
        if n_ext lt 3 then spawn,'fappend '+wtfile[0]+'+3 '+wtfile1
     endelse 
     wtfile=wtfile[wpos]
;endif 

     wtfile=wtfile[0]
     hdr=headfits(wtfile)
     wttime=sxpar(hdr,'ONTIME')
  endif 

  wtfile=wtfile[0]
  if wt eq 1 then wtfile=''
  pcfiles=file_search('00*-xrt/sw*pc*po*cl.evt')
  
  w3pos=strpos(pcfiles,'w3')
  keepw3=where(w3pos eq -1)
  pcfiles=pcfiles[keepw3]
  
  infile='combined_image.fits'
  infile2='src.reg'
  
  if not exist(infile2) then begin 
     if not exist(infile) then $
        combine_images,pcfiles
;     infile=pcfiles[0]

;     cd,outdir[0]
;     xcenout=findfile('sw*pc*po*cl.txt')
     xcenout='combined_image.txt'
;     tmp=str_sep(infile,'.evt')
;     xcenout=tmp[0]+'.txt'
     if not exist(xcenout) then begin 
        xcen='xrtcentroid infile='+infile+' outfile='+xcenout+' outdir=./ calcpos=yes interactive=yes'
        print,xcen
        spawn,xcen
     endif 
     cen2reg,xcenout
  endif 
;     cd,'..'

  if not exist('bg.reg') then begin 
     openw,lun,'xim2.batch',/get_lun
     printf,lun,'read/fits/size=700 '+infile
     if wtfile ne '' then begin 
        printf,lun,'save'
        printf,lun,'read/fits/size=700 '+wtfile
        printf,lun,'sum'
     endif 
     printf,lun,'cpd /xw'
     printf,lun,'disp'
     printf,lun,'detect/snr=3'
     printf,lun,'grid/ticks'
     printf,lun,'circlereg/regionfile=src.reg/disp'
;    printf,lun,'circlereg/new/regionfile=bg.reg'
     printf,lun,'coord/cursor'
     printf,lun,'set fileid [open bg.reg a+]'
     printf,lun,'set par1 [string trim $coord(ra)]'
     printf,lun,'set par2 [string trim $coord(dec)]'
     printf,lun,'puts $fileid "# Region file format: DS9 version 3.0"'
     printf,lun,'puts $fileid "# Filename: bg.reg"'
     printf,lun,'puts $fileid "global color=green select=1 edit=1 move=1 delete=1 include=1 fixed=0 source"'
     printf,lun,'puts $fileid "fk5;circle( [lindex $par1] , [lindex $par2] ,60)"'
     printf,lun,'close $fileid'
     printf,lun,'circlereg/regionfile=bg.reg/disp'
     printf,lun
     printf,lun,'exit'
     printf,lun
     close,lun
     free_lun,lun
     spawn,'ximage @xim2.batch'
     
     
  endif 

  read_regfile,'src.reg',sra,sdec
  read_regfile,'bg.reg',bra,bdec
  if type(battime) eq 7 then begin 
     date=year+'-'+ntostr(month)+'-'+day+'-'+ntostr(battime)
     print,date
     trigtime=date2met(date)
  endif else trigtime=battime

  if not exist('lc_wrap3.par') then begin 
     strpc=''
     for i=0,n_elements(pcfiles)-1 do strpc=strpc+' '+pcfiles[i]
     openw,lun,'lc_wrap3.par',/get_lun
     printf,lun,'fnames '+wtfile+' '+strpc
     printf,lun,'ra_src '+sra
     printf,lun,'dec_src '+sdec
     printf,lun,'ra_back '+bra
     printf,lun,'dec_back '+bdec
     printf,lun,'bat_trig '+sigfig(trigtime,11)
     acsfiles=file_search('00*-xrt/*acs*')
     stracs=''
     for i=0,n_elements(acsfiles)-1 do stracs=stracs+' '+acsfiles[i]
     printf,lun,'acs_file '+stracs
     close,lun
     free_lun,lun
  endif 

  if not keyword_set(skip) then begin 
     if not exist('lc_newout.txt') then $
        lc_wrap,'lc_wrap3.par'
     
     replot_xrt_lc,file='lc_newout.txt',title=grbname
     k=get_kbrd(10)
     if k eq 's' then stop
  endif 

  close,/all,/file
;cd,'..'

  return
end 
