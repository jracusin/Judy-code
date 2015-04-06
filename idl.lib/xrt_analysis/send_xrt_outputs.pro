pro send_xrt_outputs
  
  @pass1_common
  @pass1_path_init
  
  
  webdir='~/webdir' ;need to get webdir and add to pass1_path_init & pass1_common
  ; Get email address
  GET_LUN, lumail
  openr,lumail,email_path, ERROR=ierr
  
  email_address = ''
  if (ierr eq 0) then readf,lumail,email_address
  FREE_LUN, lumail
  
  get_file_begin,filebegin,startdir
  outdir=filebegin+'_outputs'
  
  if not exist(outdir) then spawn,'mkdir '+outdir
  
  ;determines if there is a grb and filename of postagestamp and im_image
  psfile=filebegin+'_postagestamp.0.*.postage_stamp.fits'  
  grb=0
  psfiles=findfile(psfile)
  if (file_size(psfiles[0]) ne -1) then grb=1 ;grb in data set
  imfile=filebegin+'*.im_image*'
  if exist(imfile) then begin
     gimfile=filebegin+'*.im_image*.gz'
     gimfiles=findfile(gimfile)
     if n_elements(gimfiles) gt 0 then begin 
        imfiles=findfile(imfile)
        for i=0,n_elements(imfile)-1 do spawn,'gunzip '+imfiles[i]
     endif 
     imfile=filebegin+'*.im_image*'
     imfiles=findfile(imfile)
  endif 
  
  files=filebegin+['_ahk.0.stripchart_plots.ps', $
                   '_ahk.0.stripchart.log',$
                   '_ahkVC0.0.stripchart_plots.ps', $ 
                   '_ahkVC0.0.stripchart.log',$
                   '_ahk_tdrss_vc6.0.stripchart_plots.ps', $
                   '_ahk_tdrss_vc6.0.stripchart.log',$
                   '_tdrsshk.0.stripchart_plots.ps', $
                   '_tdrsshk.0.stripchart.log', $
                   '_errors.txt', $
                   '_errors.pars1.txt',$
                   '_science.all.timeline', $
                   '_science.0.timeline', $
                   '_science.all.short.timeline', $
                   '_science.0.short.timeline', $
                   '_science.all.log', $ 
                   '_science.all.err',$ 
                   '_science.0.log',$ 
                   '_science.0.err',$
                   '_positionmessage.txt',$
                   '_positionmessage_tdrss.txt',$
                   '_centroidingerror.txt',$
                   '_centroidingerror_tdrss.txt',$
                   '_postagestamp.0.postage_stamp.log',$
                   '_postagestamp_tdrss.0.postage_stamp.log']
  if exist(psfile) then files=[files,psfiles]
  if exist(imfile) then files=[files,imfiles]
  files=[files,filebegin+[ $ 
                            '_lightcurve.0.plc.log',$
                            '_lightcurve_tdrss.0.plc.log',$
                            '_lightcurve.0.plc.ps',$
                            '_lightcurve_tdrss.0.plc.ps',$
                            '_spectrum.0.spectrum.log',$
                            '_spectrum_tdrss.0.spectrum.log',$
                            '_spectrum.0.spectrum.ps',$
                            '_spectrum_tdrss.0.spectrum.ps',$
                            '_tam_two_win.0.trends.ps',$
                            '_tam_two_win.0.tam_hist.ps',$
                            '_tam2windiffs.ps',$
                            '_tam_two_win.0.tam_centroids',$
                            '_tam_two_win.0.log', $
                            '_tam_full_frame.0.frame1.fits.gz',$
                            '_tam_full_frame.0.frame2.fits.gz',$
                            '_tam_full_frame.0.frame3.fits.gz', $
                            '_tam_full_frame.0.log']]
  
  if find_ff_bias(bf) then files=[files,bf]
  
  files=[files,'mean_bias_summary.xls','check_row_bias.xls','lrpd_spec_result_summary.xls','im_spec_result_summary.xls','raw_spec_result_summary.xls','pupd_spec_result_summary.xls','pc_spec_result_summary.xls','wt_spec_result_summary.xls','analysis.log']
  
  nfiles=n_elements(files)
 
  use=-1
  for i=0,nfiles-1 do begin 
     if exist(files[i]) then if (file_size(files[i]) gt 0) then begin
         use=[use,i]
         spos=strpos(files[i],'ps')
         if spos[0] ne -1 then begin
             fname=str_sep(files[i],'.ps')
             pdfname=fname[0]+'.pdf'
             spawn,'ps2pdf '+files[i]+' '+pdfname
             files[i]=pdfname
         endif 
     endif 
  endfor 
  if n_elements(use) gt 1 then use=use[1:*]
 
  gfiles=''
  grbdetect=''
  for j=0,n_elements(use)-1 do gfiles=gfiles + ' ' +files[use[j]]
  
  if grb then grbdetect=' (includes new AT)'
  
  spawn,'cp '+gfiles+' '+outdir+'/'
  
; remove the previous tar file, we run this program multiple times now
  spawn,'rm '+outdir+'.tar.gz'

  spawn,'tar cvf '+outdir+'.tar '+outdir
  
  spawn,'gzip '+outdir+'.tar'
  
  website='http://www.swift.psu.edu/xrt/analysis/xrt_data_outputs.html'

  if exist(webdir) then begin
;     spawn,'chmod 655 '+outdir+'.tar.gz'
     spawn,'cp '+outdir+'.tar.gz '+webdir+'/'
     cd,webdir
     spawn,'gunzip < '+webdir+'/'+outdir+'.tar.gz | tar xvf -'
     spawn,'rm '+webdir+'/'+outdir+'.tar.gz'
     spawn,'rm '+webdir+'/'+outdir+'/*ps'
     datestr=str_sep(filebegin,'_')
     date=filedate2readdate(datestr[1])
     day=strmid(datestr[1],4,3)
     outline="<a href = '"+date+".html'>"+date+'</a> -- Day '+day+'<br>'
     dup=0
     if exist(webdir+'/xrtdatalist.html') then begin 
        openr,dlun,webdir+'/xrtdatalist.html',/get_lun ;checking for date existing
        line=''
        while (not eof(dlun) and (line ne outline)) do begin
           readf,dlun,line
           if line eq outline then dup=1
        endwhile
        close,dlun
        free_lun,dlun
     endif 
     if not dup then begin 
        openu,dlun,webdir+'/xrtdatalist.html',/get_lun,/append
        printf,dlun,outline
        close,dlun
        free_lun,dlun
     endif 
     
     sort_html_list,webdir+'/xrtdatalist.html',/date
     
     dup=0
     outline="<a href = '"+outdir+'/'+filebegin+".html'>"+filebegin+grbdetect+'</a> <br>'
     if exist(webdir+'/'+date+'.html') then begin
        openr,unit,webdir+'/'+date+'.html',/get_lun
        line=''
       
        while (not eof(unit) and (line ne outline)) do begin
           readf,unit,line
           if line eq outline then dup=1
        endwhile
        close,unit
        free_lun,unit
     endif 
     
     if not dup then begin 
        openu,unit,webdir+'/'+date+'.html',/get_lun,/append
        printf,unit,outline
        close,unit
        free_lun,unit
     endif 
     
     sort_html_list,webdir+'/'+date+'.html'
     
     cd,webdir+'/'+outdir
     openw,unit,filebegin+'.html',/get_lun ;also need to add webdir
     for i=0,n_elements(use)-1 do begin 
        printf,unit,"<a href = '"+files[use[i]]+"'>"+files[use[i]]+'</a> <br>'
        spawn,'chmod 655 '+files[use[i]]
     endfor
;     printf,unit,"<a href = '../"+outdir+'.tar.gz'+"'>"+outdir+'.tar.gz'+'</a> <br>'
     close,unit
     free_lun,unit
     
     spawn,'chmod 655 '+filebegin+'.html'
     spawn,'chmod 655 ../'+date+'.html'
     openw,unit,'../when_update.html',/get_lun
     printf,unit,'<body>'
     printf,unit,'Last Update: '+systime()+' EDT' ;EDT or UTC depends on what analysis computer clock is set to
     printf,unit,'</body>'
     close,unit
     free_lun,unit
  endif 
  
  cd,startdir
  
  return
end 
