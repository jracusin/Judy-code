pro tam, filename, showplot, debug_level, DEBUG5=debug5, PRINTPLOTS=printplots, $
         SHOWIMAGE=showimage, SHOWCCD=showccd, SHOWHK=showhk, NOINIT=noinit, NOPRINT=noprint,quicklook=quicklook
; 
; $Id: tam.pro 1.0 2001/11/05 13:23:35 burrows Exp $
;
;name	: tam
;author : Dave Burrows
;date	: 04/04/03
;lang	: IDL

;purpose: This program processes XRT TAM data files, and is based on 
;	the TAM program for XRT data processing.  The input
;	is a TAM data dump from the Swift XRT.  The input file must be
;	a data dump of a TAM Large Data Product file from the instrument.
;	It can be either a dump of Ap_ID 0x0541 (TAM IMAGE) or Ap_ID 0x0542
;	(TAM WINDOW).  
;	
;	Formats are defined in XRT-PSU-028.  
;;
;	Multiple LDPs can be included in the input file.  
;
;	Input parameters:
;		filename = name of input data file.  User will be prompted
;			for this if it is not given as a parameter.
;		show_plot: controls display of XY plots (currently baselines,
;			mean rows, and raw spectra):
;			0: don't plot them
;			1: plot with default scaling (default option)
;			2: open interactive plot viewer to generate plots
;		debug_level: sets amount of debugging output (0=min, 10=max)
;	   Keywords:
;		/debug: set debug_level to 5 (over-ridden if debug_level
;			is set explicitly)
;		/noinit: this keyword inhibits initialization of the
;			common block variables.  Used by XRT.PRO.
;		/printplots: this keyword controls which plots are printed.
;			= 0: don't print any plots.
;			= 1: print only summary plots (from Snapshot Trailer
;					and housekeeping).  Default value.
;			= 2: print all plots (including all CCD histograms). 
;			If printplots is set, it turns off interactive plots.
;		/noprint: same as printplots=0
;		/showimage: controls display of CCD and HK images:
;			= 0: no images displayed
;			= 1: displays CCD images in non-interactive mode
;			= 2: displays CCD and HK images using interactive widget.
;		    This keyword overrides the normal image display defaults
;			set by /noplots (showimage=1) and /quicklook (showimage=1).
;		/showccd: same as showimage, but only controls show_ccd_image.
;			Over-rides showimage for CCD images.
;		/showhk: same as showimage, but only controls show_hk_image.
;			Over-rides showimage for HK images.
;		/pixgen: sets plot boundaries of histograms to match the
;			pixel generator - only used for lab tests.
;
;	Error codes:
;	    0	no errors
;	    1	CHECK_POINTER_HEADER: found on-board pointer header ID
;			pattern instead of expected pattern
;	    2	CHECK_POINTER_HEADER: found TM bitstream ID instead of bit-swapped ID
;	    3	CHECK_POINTER_HEADER: found unknown bit pattern instead of pointer header ID
;	   11	START_NEW_DATA_BLOCK: at EOF - no new data to read
;	   12	START_NEW_DATA_BLOCK: found EOF - incomplete data block
;	   13	START_NEW_DATA_BLOCK: CCSDS Checksum error
;	   71	TAM: Found page number not 1 when looking for header (image)
;	   72	TAM: Found redundant LDP header when looking for header (image)
;	   73	TAM: found invalid header ID when looking for header (image)
;	   76	TAM: Found page number not 1 when looking for header (win)
;	   77	TAM: Found redundant LDP header when looking for header (win)
;	   78	TAM: found invalid header ID when looking for header (win)
;	   81	STRIPCHART_MODE: EOF encountered before end of data frame
;	   91	PROCESS_CCD_FRAME_HDR: EOF encountered before end of CCD header
;	  101	CHECK_SCIENCE_DATA_HDR: found on-board pointer header ID pattern instead of
;					expected pattern (bit-swapped)
;	  102	CHECK_SCIENCE_DATA_HDR: found TM bitstream ID instead of bit-swapped ID
;	  103	CHECK_SCIENCE_DATA_HDR: found unknown bit pattern instead of pointer header ID
;	  151	APPEND_DATA_BLOCK: EOF encountered
;	  153	APPEND_DATA_BLOCK: EOT found earlier by CHECK_ECC_BYTES
;
;
;  INSTALLATION NOTES: when installing TAM on a new system, the
;	common block variables *_path located in file pass1_path_init.pro
;	must be set to point to the location of the indicated files on
;	the new system.
;
; Rev:
;	08/12/04 by JLR: fixed problem with tam crashing on full frame data
;	05/31/04 by JLR: added if statement so tam histograms will not be 
;			 made if running on tam_full_frame, so TAM won't 
;			 crash there
;	05/31/04 by DNB: incorporated Alberto's changes to calculate
;		centroid in TAM and compare result with on-board calculation.
;	05/30/04 by DNB: added TAM_LLD and LED_SELECT to DTAS file: V2.7
;	05/27/04 by DNB: added output FITS binary table file: V2.6
;	05/26/04 by DNB: changed X-axis on plots to time instead of frame: V2.5
;	05/24/04 by DNB: added labels to plots, fixed bug with times: V2.4
;	05/21/04 by DNB: added output file for DTAS trending: V2.3
;	05/20/04 by JLR: added tam histograms of centroids
;	04/21/04 by DNB: printing out first and last times: V2.1
;	04/08/04 by DNB: V2.0 of TAM: added postscript output file for 
;		diagnostic plots.
;	04/08/03 by DNB: version of PASS1 for TAM data.
;	03/27/03 by DNB: fixed to handle EOT words properly.
;	09/10/02 by DNB: V2.1: bug fix in LRPD mode, modified some print statements, added
;		bias map.
;	09/08/02 by DNB: V2.0: released for Panter calibration
;	09/06/02 by DNB: V1.7: added image mode
;	09/05/02 by DNB: V1.6: added piled-up photodiode mode
;	09/04/02 by DNB: V1.5: added low-rate photodiode mode
;	09/02/02 by DNB: V1.4: added windowed timing mode
;	08/10/02 by DNB: added accumulated snapshot image; changed meanings
;		of baseline_correction options (removed original option 1)
;	08/09/02 by DNB: V1.3: incorporating photon-counting mode
;	05/02/02 by DNB: fixed bug in /printplots implementation
;	04/11/02 by DNB: deleted /noplots keyword, added /printplots keyword
;	03/12/02 by DNB: added pickfile for file error loop
;	02/23/02 by DNB: deleted path from input filename when selected via
;		pickfile.
;	02/09/02 by DNB: initialize split_type for each .sci file.  Changed 
;		definition of baseline_correction keyword.
;	02/08/02 by DNB: added check for presence of Snapshot Trailer
;	02/03/02 by DNB: moved HK cal file setup to after log file and error
;		file are opened.
;
; $Log: pass1.pro $
;

@pass1_common
;common ccd_hk_data	; TBD: stores CCD HK data for process_ccd_frame_hdr

@tam_common

  close,/all           ; close files in case any were left open by previous run

@pass1_path_init                ; initialize paths for input files

  version = 'V2.7'
  UTC_delta = 0.0

; "declare" variable types/initialize variables

  last_time = 0.0D

  if not (keyword_set(noinit)) then begin
@pass1_init                     ; initialize common block variables
@tam_init                       ; initialize common block variables.
  endif

  ID_type = 0
  files_in = strarr(1)


  GET_LUN, luin           ; Get Logical Unit Numbers for input and output files
  GET_LUN, luout
  GET_LUN, luerr
  GET_LUN, lulog

; Get email address
  GET_LUN, lumail
  openr,lumail,email_path, ERROR=ierr
  email_address = ''
  if (ierr eq 0) then readf,lumail,email_address
  FREE_LUN, lumail

  ccsds_hdr_id = 0

  if (n_params() eq 0) then begin
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select TAM data file: ', $
                       filter='*_tam_*',/must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
  endif else begin
     filein = filename
  endelse

  if (n_params() lt 2) then show_plot = 1 $
  else show_plot = showplot

  if (n_params() lt 3) then idebug = 0 $
  else idebug = debug_level

  if keyword_set(debug5) then idebug = 5

;idebug = 100

  print_plots=1
  if keyword_set(printplots) then begin
     print_plots = printplots
     show_plot = 1
     show_ccd_image = 1
     show_hk_image = 1
  endif

  if keyword_set(noprint) then begin
     print_plots = 0
     show_plot = 1
     show_ccd_image = 1
     show_hk_image = 1
  endif
  
  quick_look_mode = 0
  if keyword_set(quicklook) then begin
     quick_look_mode = 1
     print_plots=0
     verbosity = 0
     showimage=0
     show_plot = 0
     show_ccd_image = 0
     show_hk_image = 0
  endif

  if keyword_set(showimage) then begin
     show_ccd_image = showimage
     show_hk_image = showimage
  endif
  if keyword_set(showccd) then begin
     show_ccd_image = showccd
  endif
  if keyword_set(showhk) then begin
     show_hk_image = showhk
  endif
  pix_gen=0

; Check that file exists

  openr,luin,filein,error=file_error
  if (file_error ne 0) then repeat begin
     print,'Error opening input file.  Please try another file name.'
;	read,'Enter name of XRT input file: ', filein
     filein = pickfile(title='Select TAM data file: ', /must_exist)
     path_pos = strpos(filein, '/', /reverse_search)
     filein = strmid(filein,path_pos+1)
     openr,luin,filein,error=file_error
  endrep until (file_error eq 0)
  close,luin

; Find the file extension (the string following the last period
;		in the file name)
  start_pos = 0
  new_pos = strpos(filein,'.',start_pos)
  if (new_pos eq -1) then begin
     file_ext = ' '
  endif else begin
     while (new_pos ne -1) do begin
        new_pos = strpos(filein,'.',start_pos)
        if (new_pos ne -1) then str_pos = new_pos
        start_pos = new_pos + 1
     endwhile
     file_ext = strmid(filein,str_pos+1,3)
  endelse


; Now get the filename base
  filebase = filein

; Set up error and log files

  file_err = filebase + '.err'
  openw,luerr,file_err
  printf,luerr,'TAM ERROR FILE'
  printf,luerr,'TAM ' + version + ' executed on XRT data file ', filein
  printf,luerr,'     on ', runtime

  file_log = filebase + '.log'
  openw,lulog,file_log
  printf,lulog,'TAM LOG FILE'
  printf,lulog,'TAM ' + version + ' executed on XRT data file ', filein
  printf,lulog,'     on ', runtime

  GET_LUN, lutam
  file_tam = filebase + '.tam_centroids'
  openw,lutam,file_tam

  GET_LUN, ludtas
  file_dtas = 'pkt_' + filebase + '_dtas.trd'
  openw,ludtas,file_dtas
  printf,ludtas,format='(a)',$
     'TIME, X_TAM_POS_X1, X_TAM_POS_Y1, X_TAM_SIG_WIN_1, X_TAM_POS_X2, X_TAM_POS_Y2, X_TAM_SIG_WIN_2, X_TAM_LED_SELECT, X_TAM_LLD'

  print,'TAM ' + version + ' executed on XRT data file ', filein

; Set up housekeeping arrays
  load_hk_tables


; OPEN INPUT FILE(S), START PROCESSING DATA


  print,' '
  print,' '
  print,'************************************************************'
  print,'************************************************************'
  print,'TAM: STARTING MAIN PROCESSING LOOP' 
  printf,lulog,' '
  printf,lulog,' '
  printf,lulog,'************************************************************'
  printf,lulog,'************************************************************'
  printf,lulog,'TAM: STARTING MAIN PROCESSING LOOP' 

  print,'TAM: Opening input file ', filein
  printf,lulog,'TAM: Opening input file ', filein
  openr,luin,filein,/swap_if_little_endian

  print,'************************************************************'
  print,'************************************************************'
  print,' '
  printf,lulog,'************************************************************'
  printf,lulog,'************************************************************'
  printf,lulog,' '


                                ; Initialize variables that have to be reset every time loop starts

  buffer_pointer = 0  ; Initialize pointer to start at begininng of data buffer
  block_number = 0              ; Initialize block counter
  end_of_data = 0               ; Initialize flag


                                ; now process data frames

  while (not EOF(luin)) do begin

     bytes_read = -2
     byte1 = 0b
     byte2 = 0b
     repeat begin
        readu,luin,byte1,byte2
        ccsds_hdr_id = byte1*256U + byte2
        bytes_read = bytes_read+2
     endrep until ((ccsds_hdr_id eq '0d41'x) or (ccsds_hdr_id eq '0d42'x))
     if (bytes_read gt 0) then print_error,0,1,'FILE FORMAT ERROR: ' $
        + strtrim(string(bytes_read),2) $
        + ' bytes before valid CCSDS Header ID found'
     
     case ccsds_hdr_id of
        '0d41'x: tam_image, ccsds_hdr_id, buffer_pointer, error_code
        '0d42'x: tam_windows, ccsds_hdr_id, buffer_pointer, error_code
        else: begin
           print_error,0,1,'TAM: unrecognized Ap_ID = ' + string(ccsds_hdr)
           if (buffer_pointer ge 9) then $
              printf,lulog,format='(A,20Z4)','TAM: buffer values near EOT words: ',$
              data_buffer(buffer_pointer-9:buffer_pointer+10)
           
        endelse
     endcase

     if (error_code ne 0) then print_error,0,1, $
        '***** Returned to TAM with error code: ' $
        + strtrim(string(error_code)) + ' *****'

     if (idebug ge 10) then begin
        print,'TAM: Looping in while loop at pointer=', buffer_pointer
        printf,lulog,'TAM: Looping at pointer=',buffer_pointer
        printf,lulog,format='(a,z10)', $
           'TAM: find_id returned ID_type = ', ID_type
     endif

  endwhile

  print,format="('Exiting TAM at block ', i, ', buffer pointer @', i)", $
     block_number, buffer_pointer
  printf,lulog,format="('Exiting TAM at block ', i, ', buffer pointer @', i)", $
     block_number, buffer_pointer

  file_status = fstat(luin)
  print,'     File pointer = ', file_status.cur_ptr,' ;  File size = ', $
     file_status.size
  printf,lulog,'     File pointer = ', file_status.cur_ptr,' ;  File size = ', $
     file_status.size


                                ;*****************************************************************************
                                ; Start Post-Processing

  print,' '
  print,'****************************************************************'
  print,'****************************************************************'
  print,'   STARTING POST-PROCESSING for file ', filein
  print,'****************************************************************'
  print,'****************************************************************'

  close,luin


; Plot diagnostics

  if (iframe ge 1) then begin
     if (show_plot gt 0) then begin 
        old_window = !D.window
        if (n_elements(diagnostic_window) eq 0) then begin
           window,/free,title='TAM Processing Diagnostics'
           diagnostic_window = !D.window
        endif else wset, diagnostic_window
        !P.Multi = [0,1,6]
        plot, frame_count[0:iframe], tamx1[0:iframe], $
           yrange=[min(tamx1[0:iframe]),max(tamx1[0:iframe])], $
           title = 'TAM Window 1 Centroid X', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamy1[0:iframe], $
           yrange=[min(tamy1[0:iframe]),max(tamy1[0:iframe])], $
           title = 'TAM Window 1 Centroid Y', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamsig1[0:iframe], $
           yrange=[min(tamsig1[0:iframe]),max(tamsig1[0:iframe])], $
           title = 'TAM Window 1 Std Dev', xtitle='Frame #', ytitle = 'Pixels'
        plot, frame_count[0:iframe], tamx2[0:iframe], $
           yrange=[min(tamx2[0:iframe]),max(tamx2[0:iframe])], $
           title = 'TAM Window 2 Centroid X', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamy2[0:iframe], $
           yrange=[min(tamy2[0:iframe]),max(tamy2[0:iframe])], $
           title = 'TAM Window 2 Centroid Y', xtitle='Frame #', ytitle='Pixels'
        plot, frame_count[0:iframe], tamsig2[0:iframe], $
           yrange=[min(tamsig2[0:iframe]),max(tamsig2[0:iframe])], $
           title = 'TAM Window 2 Std Dev', xtitle='Frame #', ytitle = 'Pixels'

        XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
           'XRT TAM processing diagnostics from file ' + filein
        XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime
     endif 
     
     set_plot,'ps'
     device,filename=filebase + '.trends.ps', /landscape
     !P.Multi = [0,1,6]
     plot, tamtime[0:iframe], tamx1[0:iframe], $
        yrange=[min(tamx1[0:iframe]),max(tamx1[0:iframe])], $
        title = 'TAM Window 1 Centroid X', xtitle='S/C time (s)', ytitle='Pixels'
     plot, tamtime[0:iframe], tamy1[0:iframe], $
        yrange=[min(tamy1[0:iframe]),max(tamy1[0:iframe])], $
        title = 'TAM Window 1 Centroid Y', xtitle='S/C time (s)', ytitle='Pixels'
     plot, tamtime[0:iframe], tamsig1[0:iframe], $
        yrange=[min(tamsig1[0:iframe]),max(tamsig1[0:iframe])], $
        title = 'TAM Window 1 Std Dev', xtitle='S/C time (s)', ytitle = 'Pixels'
     plot, tamtime[0:iframe], tamx2[0:iframe], $
        yrange=[min(tamx2[0:iframe]),max(tamx2[0:iframe])], $
        title = 'TAM Window 2 Centroid X', xtitle='S/C time (s)', ytitle='Pixels'
     plot, tamtime[0:iframe], tamy2[0:iframe], $
        yrange=[min(tamy2[0:iframe]),max(tamy2[0:iframe])], $
        title = 'TAM Window 2 Centroid Y', xtitle='S/C time (s)', ytitle='Pixels'
     plot, tamtime[0:iframe], tamsig2[0:iframe], $
        yrange=[min(tamsig2[0:iframe]),max(tamsig2[0:iframe])], $
        title = 'TAM Window 2 Std Dev', xtitle='S/C time (s)', ytitle = 'Pixels'

     XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
        'XRT TAM processing diagnostics from file ' + filein
     XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime

     !P.Multi = 0               ; reset to one plot per page
     device,/close
     if (print_plots gt 0) then begin
        print,'Sending plots to printer'
        spawn,'lp ' + filebase + '.trends.ps'
     endif 

;;tam hist
     if filein ne strmid(filein,0,15)+'_tam_full_frame.0' then begin 
        set_plot,'ps'
        device,filename=filebase+'.tam_hist.ps',/landscape

        xtit=['frame1_xpos (pix)','frame1_ypos (pix)','frame2_xpos (pix)','frame2_ypos (pix)']
        title='TAM centroid postions'

        !p.multi=[0,2,2]

        plothist,tamx1[0:iframe],xhist,yhist,bin=.01,xtitle=xtit[0],title=title
        if n_elements(xhist) gt 5 then begin 
           a=[max(yhist),median(xhist),stddev(tamx1[0:iframe])]
           g=gaussfit(xhist,yhist,est=a,a1,nterms=3)
           oplot,xhist,g
           a1=strcompress(string(float(a1)),/remove_all)
           legend,['bin size = 0.01','mean = '+a1[1],'sigma = '+a1[2]],/top,/right,box=0
        endif 

        plothist,tamy1[0:iframe],xhist,yhist,bin=.01,xtitle=xtit[1],title=title
        if n_elements(xhist) gt 5 then begin 
           a=[max(yhist),median(xhist),stddev(tamy1[0:iframe])]
           g=gaussfit(xhist,yhist,est=a,a2,nterms=3)
           oplot,xhist,g
           a2=strcompress(string(float(a2)),/remove_all)
           legend,['bin size = 0.01','mean = '+a2[1],'sigma = '+a2[2]],/top,/right,box=0
        endif 

        plothist,tamx2[0:iframe],xhist,yhist,bin=.01,xtitle=xtit[2],title=title
        if n_elements(xhist) gt 5 then begin 
           a=[max(yhist),median(xhist),stddev(tamx2[0:iframe])]
           g=gaussfit(xhist,yhist,est=a,a3,nterms=3)
           oplot,xhist,g
           a3=strcompress(string(float(a3)),/remove_all)
           legend,['bin size = 0.01','mean = '+a3[1],'sigma = '+a3[2]],/top,/right,box=0
        endif

        plothist,tamy2[0:iframe],xhist,yhist,bin=.01,xtitle=xtit[3],title=title
        if n_elements(xhist) gt 5 then begin 
           a=[max(yhist),median(xhist),stddev(tamy2[0:iframe])]
           g=gaussfit(xhist,yhist,est=a,a4,nterms=3)
           oplot,xhist,g
           a4=strcompress(string(float(a4)),/remove_all)
           legend,['bin size = 0.01','mean = '+a4[1],'sigma = '+a4[2]],/top,/right,box=0
        endif 
        XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
           'XRT TAM histograms from file ' + filein
        XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime


        !p.multi=0
        device,/close
        set_plot,'X'

        if (print_plots gt 0) then spawn,'lp -d ' + filebase + '.tam_hist.ps'
        

; Now plot differences between on-board and calculated centroids

        diffx1 = tamx1 - calcx1
        diffy1 = tamy1 - calcy1
        diffx2 = tamx2 - calcx2
        diffy2 = tamy2 - calcy2
        
        if (show_plot gt 0) then begin 
           old_window = !D.window
           if (n_elements(plot_window) eq 0) and (show_plot gt 0) then begin
              window,/free,title='TAM On-Board vs Ground Differences'
              plot_window = !D.window
           endif else wset, plot_window
           !P.Multi = [0,2,2]
           plot,diffx1[where(calcx1 gt 0.0001)],diffy1[where(calcx1 gt 0.0001)],psym=1
           plot,diffx2[where(calcx2 gt 0.0001)],diffy2[where(calcx2 gt 0.0001)],psym=1
        endif
        
        set_plot,'ps'
        device,filename=filebase+'.tam_diff.ps',/landscape
        !P.Multi = [0,2,2]
        plot,diffx1[where(calcx1 gt 0.0001)],diffy1[where(calcx1 gt 0.0001)],psym=1, $
           title='TAM Window 1'
        plot,diffx2[where(calcx2 gt 0.0001)],diffy2[where(calcx2 gt 0.0001)],psym=1, $
           title='TAM Window 2'
        XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
           'XRT TAM centroid differences (flight-ground) from file ' + filein
        XYOUTS,alignment=0,/device,1000,8000,size=0.8,runtime

        !p.multi=0
        device,/close
        set_plot,'X'
        if (print_plots gt 0) then spawn,'lp ' + filebase + '.tam_diff.ps'
     endif 
; Now write these out as FITS tables

     print,' '
     print,'Writing out binary table file for TAM data'
     print,' '


; Write out the FITS file primary header
     TAM_fits_table_file = filebase + '.table.fits'
     mkhdr,header,tamx1         ; make dummy header
     file_date = bin_date(systime(0))
     date = string(file_date(0),format="(i4.4)") $
        + string(file_date(1),file_date(2),format="('-',i2.2,'-',i2.2)")
     sxaddpar,header,'DATE',date,'File creation date'
     sxaddpar,header,'ORIGIN','PSU X-ray Astronomy', $
        'Data from Penn State X-ray Astronomy'
     sxaddpar,header,'INSTRUME','XRT','Swift X-ray Telescope'
     sxaddpar,header,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
     sxaddhist,' ',header
     sxaddhist,'XRT TAM Data File created by TAM '+version, header
     sxaddhist,'Column 1 is the satellite time, columns 2-10 are ',header
     sxaddhist,'     the TAM data.',header	
     sxaddhist,'     Column 2 is the TAM Frame Number', header
     sxaddhist,'     Column 3 is the Target ID', header
     sxaddhist,'     Column 4 is the Observation Segment', header
     sxaddhist,'     Column 5: TAM Window 1 on-board centroid X position', header
     sxaddhist,'     Column 6: TAM Window 1 on-board centroid Y position', header
     sxaddhist,'     Column 7: TAM Window 1 on-board standard deviation', header
     sxaddhist,'     Column 8: TAM Window 2 on-board centroid X position', header
     sxaddhist,'     Column 9: TAM Window 2 on-board centroid Y position', header
     sxaddhist,'     Column 10: TAM Window 2 on-board standard deviation', header
     sxaddhist,'     Column 11: TAM Window 1 ground centroid X position', header
     sxaddhist,'     Column 12: TAM Window 1 ground centroid Y position', header
     sxaddhist,'     Column 13: TAM Window 2 ground centroid X position', header
     sxaddhist,'     Column 14: TAM Window 2 ground centroid Y position', header
     sxaddhist,' ', header

     mwrfits,undefined,TAM_fits_table_file,header[5:*],/create

; Make table extension header
     fxbhmake,table_hdr,iframe+1,'XRT_TAM','Swift XRT TAM data file',/date
     sxaddpar,table_hdr,'DATE',date,'File creation date'
     sxaddpar,table_hdr,'ORIGIN','PSU X-ray Astronomy', $
        'Data from Penn State X-ray Astronomy'
     sxaddpar,table_hdr,'INSTRUME','XRT','Swift X-ray Telescope'
     sxaddpar,table_hdr,'TELESCOP','Swift','Swift Gamma-Ray Burst Explorer'
     sxaddhist,' ',table_hdr
     sxaddhist,'XRT TAM Data File created by TAM '+version, header
     sxaddhist,'Column 1 is the satellite time, columns 2-10 are ',header
     sxaddhist,'     the TAM data.',header	
     sxaddhist,'     Column 2 is the TAM Frame Number', header
     sxaddhist,'     Column 3 is the Target ID', header
     sxaddhist,'     Column 4 is the Observation Segment', header
     sxaddhist,'     Column 5: TAM Window 1 on-board centroid X position', header
     sxaddhist,'     Column 6: TAM Window 1 on-board centroid Y position', header
     sxaddhist,'     Column 7: TAM Window 1 on-board standard deviation', header
     sxaddhist,'     Column 8: TAM Window 2 on-board centroid X position', header
     sxaddhist,'     Column 9: TAM Window 2 on-board centroid Y position', header
     sxaddhist,'     Column 10: TAM Window 2 on-board standard deviation', header
     sxaddhist,'     Column 11: TAM Window 1 ground centroid X position', header
     sxaddhist,'     Column 12: TAM Window 1 ground centroid Y position', header
     sxaddhist,'     Column 13: TAM Window 2 ground centroid X position', header
     sxaddhist,'     Column 14: TAM Window 2 ground centroid Y position', header
     sxaddhist,' ', table_hdr
     sxaddpar, table_hdr, 'TSORTKEY', 'TIME', 'Data sorted by Time column'


     data=dblarr(14,iframe+1)
     data[0,*] = tamtime[0:iframe]
     data[1,*] = frame_count[0:iframe]
     data[2,*] = targetid[0:iframe]
     data[3,*] = obsseg[0:iframe]
     data[4,*] = tamx1[0:iframe]
     data[5,*] = tamy1[0:iframe]
     data[6,*] = tamsig1[0:iframe]
     data[7,*] = tamx2[0:iframe]
     data[8,*] = tamy2[0:iframe]
     data[9,*] = tamsig2[0:iframe]
     data[10,*] = calcx1[0:iframe]
     data[11,*] = calcy1[0:iframe]
     data[12,*] = calcx2[0:iframe]
     data[13,*] = calcy2[0:iframe]

     fxbaddcol,1, table_hdr, data[0,0], 'S/C Time', TUNIT='seconds'
     fxbaddcol,2, table_hdr, data[1,0], 'TAM Frame', TUNIT='#'
     fxbaddcol,3, table_hdr, data[2,0], 'Target ID', TUNIT='#'
     fxbaddcol,4, table_hdr, data[3,0], 'Obs. Segment', TUNIT='#'
     fxbaddcol,5, table_hdr, data[4,0], 'TAM X1', TUNIT='pixel'
     fxbaddcol,6, table_hdr, data[5,0], 'TAM Y1', TUNIT='pixel'
     fxbaddcol,7, table_hdr, data[6,0], 'TAM Sig1', TUNIT='pixel' 
     fxbaddcol,8, table_hdr, data[7,0], 'TAM X2', TUNIT='pixel'
     fxbaddcol,9, table_hdr, data[8,0], 'TAM Y2', TUNIT='pixel'
     fxbaddcol,10, table_hdr, data[9,0], 'TAM Sig2', TUNIT='pixel'
     fxbaddcol,11, table_hdr, data[10,0], 'Calc X1', TUNIT='pixel'
     fxbaddcol,12, table_hdr, data[11,0], 'Calc Y1', TUNIT='pixel'
     fxbaddcol,13, table_hdr, data[12,0], 'Calc X2', TUNIT='pixel'
     fxbaddcol,14, table_hdr, data[13,0], 'Calc Y2', TUNIT='pixel'

     fxbcreate, luout, TAM_fits_table_file, table_hdr, extension

     for row=1,iframe+1 do begin
        for col=1,14 do begin
           fxbwrite,luout,data[col-1,row-1],col,row
        endfor
     endfor
     fxbfinish,luout



;    if (n_elements(old_window) ne 0) then $
;	if (old_window(0) ne -1) then wset, old_window
  endif



  if (error_count ne 0) then begin
     print,' '
     print,' '
     print,'*** ', error_count, ' ERRORS DETECTED ***'
     print,'    Examine file ', file_err, ' for list of errors.'
  endif

  printf,lulog,' '
  printf,lulog,'*******************************************************************'
  printf,lulog,'*******************************************************************'
  printf,lulog,'Done processing ', tam_frame,' TAM images'
  printf,lulog,format='(a,f20.6,a,a)','First TAM time: ', first_time, $
     ' = ', itos_time(first_time,first_utc)
  printf,lulog,format='(a,f20.6,a,a)',' Last TAM time: ', last_time, $
     ' = ', itos_time(last_time,last_utc)
  printf,lulog,format='(a,f20.6)',' Delta (seconds) = ', last_time-first_time




  close,/all
  spawn,'cp ' + file_dtas + ' ..'

  print,''
  print,'TAM terminating. '
  print,''

  print, 'TAM processing complete ...'

end
















