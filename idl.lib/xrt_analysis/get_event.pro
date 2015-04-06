; $Id: get_event.pro 4.6 1995/02/02 07:04:57 burrows Exp $
pro	get_event

;name   : GET_EVENT
;author : jeffrey a. mendenhall
;purpose: Get all X-ray events above specified event threshold, classify
;		split types according to specified split threshold, and
;		write out into photon file.  IDL version of Greg B.'s 
;		FORTRAN code.  Output file format is direct access,
;		42 byte records.  The first record is a header containing:
;			1) Number of events 			(INTEGER*4)
;			2) Event threshold (DN)			(INTEGER*4)
;			3) Split threshold (DN)			(INTEGER*4)
;			4) Event threshold (DN)			(REAL*4)
;			5) Split threshold (DN)			(REAL*4)
;			6) Event threshold (sigma)		(REAL*4)
;			7) Split threshold (sigma)		(REAL*4)
;			8) Read noise from mean frame		(REAL*4)
;			9) XMIN for processing			(INTEGER*2)
;		       10) XMAX for processing			(INTEGER*2)
;		       11) YMIN for processing			(INTEGER*2)
;		       12) YMAX for processing			(INTEGER*2) 
;		       13) CCD # (used by CUBIC)		(INTEGER*2)
;		Following records contain X-ray data, with one 42 byte
;		record per X-ray event.  These contain:
;			1) Classification: SG, SP, or MU
;				for single, split, or multiple	(CHAR*2)
;			2) X position (counting from 1)		(INTEGER*2)
;			3) Y position of event (from 1)		(INTEGER*2)
;			4) 3 x 3 array of event sizes for the
;				neighborhood of the event,
;				arranged as (-1,-1), (0,-1),
;				(+1,-1),(-1,0),(0,0),(+1,0),
;				(-1,+1),(0,+1),(+1,+1) relative
;				to the central event pixel, which
;				is in the (0,0) location).	(9 x REAL*4)
;date   : 1/29/94
;Modified: 2/25/94 by D.Janches to remove initial errors.
;	: 07/14/94 by DNB to use new file common blocks
;	: 07/25/94 by DNB to reduce processing time.  This program was
;		looping through all the events to count them, then
;		looping through them again to characterize them.
;		Instead, it now allocates structure arrays for the information,
;		where the array size is set by the number of pixels exceeding
;		the event threshold.  After processing the real events 
;		(and counting them), it then writes everything out in one
;		loop.
;	: 07/27/94 by DNB to include plots of spectral results.
;	: 04/04/96 by jam to ignore events occuring adjacent to bad columns.
;	08/26/96 by DNB: changed to mk_file_name to generate file names.
;   07/16/04 by JLR: fixed bug with reform so would work with IDL6.0
;  
;
@common
print,''
print,'			GET_EVENT.PRO	8/15/94'
print,''
print,'Get_event.pro is used to extract events from ccd images to '
print,'form a spectrum in subsequent processing.'
print,''

get_file_info

;; Assign a bunch of counters to zero.
NTOT=0L
NSG=0L
NSP=0L
NM=0L
NVERT=0L
NHORIZ=0L
NDIAG=0L
NLEFT=0L
NRIGHT=0L
NUP=0L
NDOWN=0L
event_size = lonarr(9)
singles = fltarr(3,3)
splits = fltarr(3,3)
mults = fltarr(3,3)
single_spec = intarr(4096)
split_spec = intarr(4096)
mult_spec = intarr(4096)

PREFIX = path_name + '/' + base_file_name
suffix = file_name_extension
NUMFRAME=num_files
num = numframe
NO = first_num
low = no
NBCOL=nbad_col
if (nbcol gt 0) then BCOLS=bad_col
MNFRAME=prefix+'.mean'
nrbot = process_ymin
nrtop = process_ymax
ncbot = process_xmin
nctop = process_xmax
if (idebug ge 2) then print,'GETEVENT_1: xmin,xmax,ymin,ymax = ',$
		ncbot,nctop,nrbot,nrtop
thresh=0
split=0
ans=''
count=0

print,''
print,'Extracting X-ray events from files: ', $
	mk_file_name(prefix, low, suffix)
print,'     to ', mk_file_name(prefix, (low+num-1),  suffix)
print,''
cont='Y'
if (batch_mode eq 0) then read,'Do you want to continue? ',cont
if (cont eq 'n') or (cont eq 'N') then goto, quit2

write_fits_flag = 1
if (batch_mode eq 0) then read,'Do you want FITS output? ',cont
if (cont eq 'n') or (cont eq 'N') then write_fits_flag = 0

time0 = systime(1)

get_lun,ludebug
if (idebug ge 8) then openw,ludebug,'get_event.debug'
        
; open meanframe file.
print,''
print,'Reading meanframe...'
print,''
mean=readfits(MNFRAME,hm)
current_filename = MNFRAME ; put into common block for SHOW_IMAGE
if show_img then show_image,mean,0.01,0.99

; set up thresholds
read_noise = sxpar(hm,'FIELD_RN')
xcal_rn_dn = sxpar(hm,'OVRCK_RN')
event_thresh = event_sig*read_noise
split_thresh = split_sig*read_noise

print,''
print,'Readnoise in field pixels of mean frame is:  ', read_noise, ' DN'
print,'Readnoise in overclocked pixels of mean frm: ', xcal_rn_dn, ' DN'
print,'   Event threshold set to ', event_thresh, ' DN'
print,'   Split threshold set to ', split_thresh, ' DN'

thresh = event_thresh
split = split_thresh

; set up header structure
	EVENT_HEADER = {HEADER, N_EVENTS:0L, IEVENT_THRESHOLD:0L, $
		ISPLIT_THRESHOLD:0L, EVENT_THRESHOLD:0.0, $
		SPLIT_THRESHOLD:0.0, EVENT_SIGMA:0.0, SPLIT_SIGMA:0.0, $
		READ_NOISE:0.0, XMIN:0, XMAX:0, YMIN:0, YMAX:0, CCD:0} 
	   ; Output file has 42 byte records for compatibility 
	   ;    w/ FORTRAN direct access file format with 
	   ;    fixed sized records (42 bytes per record needed for events)

; Open the catalog file that will be used by Event Browser.
openw, catalog_unit, prefix + '.cat', /GET_LUN


; loop over all the files and get data'
if (idebug ge 1) then begin
	print,''
	print,'GETEVENT_2: Begin looping over data.'
	print,''
endif

; Set up plotting arrays
pot_plot=fltarr(num_files)
tot_plot = pot_plot
sng_plot = tot_plot
spl_plot = tot_plot
mul_plot = tot_plot

get_lun,luphot
for n = 0,NUMFRAME-1 do begin
        ;ii=string(n+NO)
        ;ii=strcompress(ii,/remove_all)
	ii = n+NO
	name=mk_file_name(PREFIX, ii, suffix)
	names=findfile(name,count=COUNT)
	if count eq 0 then begin
		 print,'Sorry, could not locate files including the string "'$
			+name+'".'
		 goto,quit1
	end
	openw,luphot,mk_file_name(PREFIX, ii, 'phot')
	print,''
	print,'Working on frame ',name,' ...'
	data=readfits(name,h)
	current_filename = name ; put into common block for SHOW_IMAGE
	if show_img then show_image,data,0.01,0.99

; block bad columns
	if NBCOL gt 0 then data(BCOLS,*)=0
; subtract mean frame
	if (idebug ge 1) then begin
		print,'GETEVENT_3: data,mean,diff for pixels (50,100:105)'
		for j=100,105 do print,j,data(50,j),mean(50,j), $
			data(50,j)-mean(50,j)
	endif
	data = temporary( data ) - mean

; blank rows and columns out of interest
	s=size(data)
	xdim = s(1)  &  ydim=s(2)

	if (idebug ge 1) then print,'GETEVENT_4: Size of data array: ',s
	ncbot = ncbot > 1
	nrbot = nrbot > 1
	nctop = nctop < (xdim-2)
	nrtop = nrtop < (ydim-2)
	if (idebug ge 2) then print,'GETEVENT_5: xmin,xmax,ymin,ymax = ',$
		ncbot,nctop,nrbot,nrtop

; Set up a temporary array which is a copy of the data array, but with 
;    all the outer border pixels set to zero.  This will be used to find the 
;    locations of events.  The event data will be taken from the original data 
;    array so that the border of pixels around the processing area can be used 
;    to check for splits.
	data2 = data
	data2(0:(ncbot-1),*) = 0.0
	data2((nctop+1):(xdim-1),*) = 0.0
	data2(*,0:(nrbot-1)) = 0.0
	data2(*,(nrtop+1):(ydim-1)) = 0.0

	if (idebug ge 1) then print,'GETEVENT_6: Size of processed subarray: ', $
		size(where(data2 ne 0))

; find events above event threshold.

	potentials=where(data2 ge thresh, num_potentials)
	data2 = 0
	pot_plot(n) = num_potentials

	if (idebug ge 1) then print,'GETEVENT_7: Number of pixels above event threshold: ', num_potentials
	if (idebug ge 4) then print,'GETEVENT_8: Event locations: ',$
		potentials(0:((num_potentials-1) < 100))

        EVENTS = replicate({EVENT, QUALITY_FLAG:'  ', X_POS:0, Y_POS:0, $
		NEIGHBORHOOD:FLTARR(3,3)}, num_potentials)
 	if (idebug ge 1) then print,'Size of events array = ',size(EVENTS)

	NTEMP=0L
	num_skip = 0
	
	for j=0L,num_potentials-1 do begin
		loc=potentials(j)    
                if (idebug ge 6 and j le 100) then print,$
			'GETEVENT_9: Event #',j,' found at address ',loc

; Compute the event location in zero-based X/Y coordinates.
		temp=float(loc)/(float(xdim))
		ypos=fix(temp)
		xpos=fix(round((temp-float(ypos))*xdim))
		;if (xpos lt ncbot or xpos gt nctop) then goto,skip
		;if (ypos lt nrbot or ypos gt nrtop) then goto,skip

; if the x position is next to bad column then do not process - jam 4/3/96
		if (n_elements(bad_col) ne 0) then begin
                  locbad = where(bad_col eq xpos+1 or bad_col eq xpos-1, count)
                  if (count GT 0) then begin
                        num_skip=num_skip+1
                        goto,skip
                  endif
		endif

;Extract the 3x3 centered on the zero-based posn (xpos,ypos),
;but leave the central pixel set to zero.
		local = data(xpos-1:xpos+1, ypos-1:ypos+1)
		local(1,1) = 0

		xpos = xpos + 1		; Convert to human counting
		ypos = ypos + 1		; Convert to human counting

		if (idebug ge 4 and j le 100) then print,$
			'GETEVENT_10:    temp, xpos, ypos =',$
			temp,xpos,ypos
		
              	if (idebug ge 6 and j le 100) then begin
			print,'GETEVENT_11:    Event size: ',data(loc)
              		print,'GETEVENT_11:    Mean frame: ',mean(loc)
               		print,'GETEVENT_11:    Neighborhood: ',local
		endif
; check to see if they are local maxima
                if data(loc) gt max(local) then begin
; check for split events
		    lsplit=where(local ge split, split_count )
		    esplit = max(local)		; save split size 
						;     for singly split events
		    local(1,1) = data(loc)	; put event back into 
						;     neighborhood
		    if (lsplit(0) eq -1) then begin
; process single pixel events
			QUALMK='SG'
			NSG=NSG+1
			sng_plot(n) = sng_plot(n) + 1
			event_size(0) = event_size(0) + 1
			energy = round(local(1,1)) < 4095 > 0
			single_spec(energy) = single_spec(energy) + 1
		; accumulate average single pixel event
			singles = singles + local
	
                    endif else begin
			event_size(split_count) = event_size(split_count) + 1
			if  (split_count eq 1) then begin
; process singly split events
			    QUALMK='SP'
			    NSP=NSP+1
			    spl_plot(n) = spl_plot(n) + 1
			    energy = round(local(1,1)+esplit) < 4095 > 0
			    split_spec(energy) = split_spec(energy) + 1
			; accumulate average split pixel event
			    splits = splits + local
			    case lsplit(0) of		; classify split type
				0: ndiag = ndiag + 1
				1: begin
					nvert = nvert + 1
					ndown = ndown + 1
				   end
				2: ndiag = ndiag + 1
				3: begin
					nhoriz = nhoriz + 1
					nleft = nleft + 1
				   end
				5: begin
					nhoriz = nhoriz + 1
					nright = nright + 1
				   end
				6: ndiag = ndiag + 1
				7: begin
					nvert = nvert + 1
					nup = nup + 1
				   end
				8: ndiag = ndiag + 1
			    endcase
			endif else begin
; process multiply-split events
				QUALMK='MU'
				NM=NM+1
				mul_plot(n) = mul_plot(n) + 1
				energy = round(total(local)) < 4095 > 0
				mult_spec(energy) = mult_spec(energy) + 1
			; accumulate average multiply-split event
				mults = mults + local
			endelse
		    endelse

; write events to events structure
		    EVENTS(NTEMP).QUALITY_FLAG=QUALMK
		    EVENTS(NTEMP).X_POS=xpos
		    EVENTS(NTEMP).Y_POS=ypos
		    EVENTS(NTEMP).NEIGHBORHOOD=local
		    if (idebug ge 8 and ntemp le 100) then printf,ludebug,$
			EVENTS(NTEMP).QUALITY_FLAG,$
			EVENTS(NTEMP).X_POS, EVENTS(NTEMP).Y_POS, $
			EVENTS(NTEMP).NEIGHBORHOOD(1,1)
		    NTEMP=NTEMP+1
		    tot_plot(n) = tot_plot(n) + 1
		    NTOT=NTOT+1
		endif

	    skip:			; skip to here if event is outside allowed region
	endfor

	print,''
	print,'Number of events skipped due to bad cols= ',num_skip
	if (idebug ge 1) then print,'GETEVENT_12: Total events found in this file: ',NTEMP

	;; Write header to binary file and create FITS headers.
	EVENT_HEADER.N_EVENTS = NTEMP
	EVENT_HEADER.IEVENT_THRESHOLD = round(thresh)
	EVENT_HEADER.ISPLIT_THRESHOLD = round(split)
	EVENT_HEADER.EVENT_THRESHOLD  = thresh
	EVENT_HEADER.SPLIT_THRESHOLD  = split
	EVENT_HEADER.EVENT_SIGMA      = event_sig
	EVENT_HEADER.SPLIT_SIGMA      = split_sig
	EVENT_HEADER.READ_NOISE       = read_noise
	EVENT_HEADER.XMIN = ncbot
	EVENT_HEADER.XMAX = nctop
	EVENT_HEADER.YMIN = nrbot
	EVENT_HEADER.YMAX = nrtop
	if (idebug ge 2) then print,'GETEVENT_13: xmin,xmax,ymin,ymax = ',$
		ncbot,nctop,nrbot,nrtop
	writeu,luphot,event_header

	filename =  mk_file_name(PREFIX, ii, 'events.fits')
	fxhmake,  pheader, /INITIALIZE, /EXTEND 
	fxaddpar, pheader, 'INSTRUME', 'UNKNOWN'
	fxaddpar, pheader, 'TELESCOP', 'UNKNOWN'
	fxaddpar, pheader, 'OBS_MODE', 'UNKNOWN'
	fxaddpar, pheader, 'DATAMODE', 'UNKNOWN'

	fxaddpar, pheader, 'CREATOR',  'PRAXIS (get_event, version 1.22)'
	fxaddpar, pheader, 'ORIGIN',   'Penn State University'
	get_date, date_today
	fxaddpar, pheader, 'DATE',     date_today

	fxaddpar, pheader, 'EVTHRESH', thresh
	fxaddpar, pheader, 'SPTHRESH', split
	fxaddpar, pheader, 'EVSIGMA', event_sig
	fxaddpar, pheader, 'EVSIGMA', split_sig
	fxaddpar, pheader, 'RDNOISE', read_noise
	fxaddpar, pheader, 'XMIN', ncbot
	fxaddpar, pheader, 'XMAX', nctop
	fxaddpar, pheader, 'YMIN', nrbot
	fxaddpar, pheader, 'YMAX', nrtop


	;; Write the body of the binary table, and a FITS file.
	if (ntemp EQ 0) then begin
	  close,luphot
	  events = 0
	endif else begin
	  events = events(0:ntemp-1) 
	  writeu,luphot,events
	  close,luphot

	 if (write_fits_flag) then begin
	  ; Write an empty primary HDU.
	  writefits, filename, 0, pheader

	  ; Construct and write the binary table.
	  theader = 0  &  dum=temporary(theader)
	  fxaddpar, theader, 'EXTNAME',  'EVENTS'
	  bin_table = replicate( { TIME: 0D, $
				   CHIPX: 0, $
				   CHIPY: 0, $
				   PHAS:  fltarr(9) }, ntemp )

	  bin_table.TIME  = n+NO
	  bin_table.CHIPX = events.X_POS
	  bin_table.CHIPY = events.Y_POS

;;; The PHAS FITS column represents a 3x3 neighborhood around each event
;;; as a 9-element vector.  The pixels are stored in the order shown below
;;; to agree with the ASCA format, which was provided in a private
;;; communication with Ken Ebisawa.  I cannot find a document that says this.
;;;                  ^
;;;                | 6 7 8
;;;    RAWY=ccdRow | 4 0 5
;;;                | 1 2 3
;;;                |----------->
;;;                 RAWX=ccdColumn

	  neighborhood = events.NEIGHBORHOOD
      events = 0
      nneigh=n_elements(neighborhood(0,0,*))
	  bin_table[*].PHAS(0) = reform( /OVERWRITE, neighborhood(1,1,*) )
	  bin_table[*].PHAS(1) = reform( /OVERWRITE, neighborhood(0,0,*) )
	  bin_table[*].PHAS(2) = reform( /OVERWRITE, neighborhood(1,0,*) )
	  bin_table[*].PHAS(3) = reform( /OVERWRITE, neighborhood(2,0,*) )
	  bin_table[*].PHAS(4) = reform( /OVERWRITE, neighborhood(0,1,*) )
	  bin_table[*].PHAS(5) = reform( /OVERWRITE, neighborhood(2,1,*) )
	  bin_table[*].PHAS(6) = reform( /OVERWRITE, neighborhood(0,2,*) )
	  bin_table[*].PHAS(7) = reform( /OVERWRITE, neighborhood(1,2,*) )
	  bin_table[*].PHAS(8) = reform( /OVERWRITE, neighborhood(2,2,*) )
	  neighborhood = 0

	  fxaddpar, theader, 'TUNIT1', 'frame number'
	  fxaddpar, theader, 'TUNIT2', 'readout pixels'
	  fxaddpar, theader, 'TUNIT3', 'readout pixels'
	  fxaddpar, theader, 'TUNIT4', 'DN'

	  mwrfits, bin_table, filename, theader
	  bin_table = 0
	  printf, catalog_unit, filename
	 endif
	endelse


	; plot results
	old_window = !D.window
	if (summary_window lt 0) then begin
		window,-summary_window,title='Summary Window'
		summary_window = !D.window		; save window number
	endif else wset, summary_window

	!P.Multi=[0,1,5]
	plot,pot_plot(0:n),title='# pixels > event thresh', xtitle='File #', $
		ytitle='#'
	plot,tot_plot(0:n),title='Total # valid events', xtitle='File #', $
		ytitle='#'
	plot,sng_plot(0:n),title='# single pixel events', xtitle='File #', $
		ytitle='#'
	plot,spl_plot(0:n),title='# singly split events', xtitle='File #', $
		ytitle='#'
	plot,mul_plot(0:n),title='# multiply split events', xtitle='File #', $
		ytitle='#'
	!P.Multi=0	; reset to one plot per page
	wset, old_window
endfor			; end loop over input files
free_lun, luphot
printf,   catalog_unit, 'END'
free_lun, catalog_unit

if (iprint eq 1) then begin
    print,''
    print,'*********** Sending summary plot to printer **************'
    print,''
    set_plot,'ps'
    DEVICE, /LANDSCAPE
    !P.Multi=[0,1,5]
    plot,pot_plot,title='# pixels > event thresh', xtitle='File #', ytitle='#'
    plot,tot_plot,title='Total # valid events', xtitle='File #', ytitle='#'
    plot,sng_plot,title='# single pixel events', xtitle='File #', ytitle='#'
    plot,spl_plot,title='# singly split events', xtitle='File #', ytitle='#'
    plot,mul_plot,title='# multiply split events', xtitle='File #', ytitle='#'
    device,/close
    spawn,'lpr -r idl.ps'
    set_plot,'X'
    !P.Multi=0	; reset to one plot per page
endif

print,''
frmt = '(a, i, 5x, a, f5.1, a)
print,''
print,'*********************************************************************'
print,''
print,'Total number of events = ',NTOT
print,''
print,format=frmt,'Total number of single events =        ',NSG, ' = ', $
	100.0*nsg/NTOT, '% of total'
print,''
print,format=frmt,'Total number of singly split events =  ',NSP, ' = ', $
	100.0*nsp/NTOT, '% of total'
print,format=frmt,'    Horizontal splits:    ', nhoriz, '           = ', $
	100.0*nhoriz/nsp, '% of single splits'
print,format=frmt,'       Left splits:  ', nleft, '           = ', $
	100.0*nleft/nhoriz, '% of horizontal splits'
print,format=frmt,'       Right splits: ', nright, '           = ', $
	100.0*nright/nhoriz, '% of horizontal splits'
print,''
print,format=frmt,'    Vertical splits:      ', nvert, '           = ', $
	100.0*nvert/nsp, '% of single splits'
print,format=frmt,'       Up splits:    ', nup, '           = ', $
	100.0*nup/nvert, '% of vertical splits'
print,format=frmt,'       Down splits:  ', ndown, '           = ', $
	100.0*ndown/nvert, '% of vertical splits'
print,format=frmt,'    Diagonal splits:      ', ndiag, '           = ', $
	100.0*ndiag/nsp, '% of single splits'
print,''
print,format=frmt,'Total number of multiply split events = ',NM, ' = ', $
	100.0*nm/NTOT, '% of total'
print,''
print,'*********************************************************************'
print,''
print,'Histogram of event sizes: '
esize = indgen(9)+1
print,esize
print,event_size
event_frac = 100.0*event_size/NTOT
print,format='(9f8.1,a)',event_frac,' %'
print,''
print,''
print,'Average single pixel event:'
singles = singles/nsg
for j=2,0,-1 do print,singles(*,j)
print,''
print,''
print,'Average singly split event:'
splits = splits/nsp
for j=2,0,-1 do print,splits(*,j)
print,''
print,''
print,'Average multiply split event:'
mults = mults/nm
for j=2,0,-1 do print,mults(*,j)
print,''
print,'*********************************************************************'
print,''

print_file = prefix+'.getevent.out'
get_lun,luprt
openw,luprt,print_file
printf,luprt,'Processing results for files ', $
	mk_file_name(prefix, low,  suffix), ' to ', $
	mk_file_name(prefix, (low+num-1), suffix)
printf,luprt,'     using thresholds: ',thresh,split
printf,luprt,''
printf,luprt,'Summary of results:'
printf,luprt,''
printf,luprt,''
printf,luprt,'*********************************************************************'
printf,luprt,''
printf,luprt,'Total number of events = ',NTOT
printf,luprt,''
printf,luprt,'Total number of single events =         ',NSG
printf,luprt,''
printf,luprt,'Total number of singly split events =   ',NSP
printf,luprt,'    Horizontal splits:    ', nhoriz
printf,luprt,'       Left splits:  ', nleft
printf,luprt,'       Right splits: ', nright
printf,luprt,'    Vertical splits:      ', nvert
printf,luprt,'       Up splits:    ', nup
printf,luprt,'       Down splits:  ', ndown
printf,luprt,'    Diagonal splits:      ', ndiag
printf,luprt,''
printf,luprt,'Total number of multiply split events = ',NM
printf,luprt,''
printf,luprt,'*********************************************************************'
printf,luprt,''
printf,luprt,'Histogram of event sizes: '
printf,luprt,esize
printf,luprt,event_size
printf,luprt,format='(9f8.1,a)',event_frac,' %'
printf,luprt,''
printf,luprt,''
printf,luprt,'Average single pixel event:'
for j=2,0,-1 do printf,luprt,singles(*,j)
printf,luprt,''
printf,luprt,''
printf,luprt,'Average singly split event:'
for j=2,0,-1 do printf,luprt,splits(*,j)
printf,luprt,''
printf,luprt,''
printf,luprt,'Average multiply split event:'
for j=2,0,-1 do printf,luprt,mults(*,j)
printf,luprt,''
printf,luprt,'*********************************************************************'
printf,luprt,''
close,luprt
free_lun,luprt
print,''
print,'Elapsed time: ', systime(1)-time0, ' seconds'
print,''

if (show_plot eq 1) then !P.Multi=[0,1,4]	 ; use multiple plots
plotter,show_plot,esize,event_frac,xrange=[0,10], yrange=[0.0,100.0], $
	/HISTOGRAM, title='Histogram of event sizes', $
	xtitle='Event size (# pixels)', ytitle='Fraction of Events (%)'

print,''
print,'Plotting single event spectrum ...'
dn=indgen(4096)
ymx1 = max(single_spec(3*thresh:4095))
plotter,show_plot,dn,single_spec,yrange=[0,ymx1],title='Single event spectrum',$
	xtitle='DN', ytitle='# events/bin', /HISTOGRAM
print,''


print,'Plotting split event spectrum (central pixel plus split)...'
ymx2 = max(split_spec(3*thresh:4095))
plotter,show_plot,dn,split_spec,yrange=[0,ymx2],title='Split event spectrum',$
	xtitle='DN', ytitle='# events/bin',  /HISTOGRAM
print,''


print,'Plotting multiple event spectrum (sum of 3x3 neighborhood) ...'
ymx3 = max(mult_spec(3*thresh:4095))
plotter,show_plot,dn,mult_spec,yrange=[0,ymx3],$
	title='Multiply-split event spectrum',xtitle='DN', $
	ytitle='# events/bin',  /HISTOGRAM
if (show_plot eq 1) then !P.Multi=0	 ; revert to single plots


print,''
;dummy=''
;read,'Send output to printer? ',dummy
;dummy = strupcase(strmid(dummy,0,1))
;print,''
if (iprint gt 0) then begin
    ; Send print file to printer
	spawn,'lpr ' + print_file
    ; Send plots to printer
        set_plot,'ps'
	!P.Multi=[0,1,4]
	plot,esize,event_frac,xrange=[0,10],title='Histogram of event sizes', $
		xtitle='Event size (# pixels)', $
		ytitle='Fraction of Events (%)', psym=10, xticklen=-0.02

	plot,dn,single_spec,yrange=[0,ymx1],title='Single event spectrum',$
		xtitle='DN',psym=10, xticklen=-0.02

	plot,dn,split_spec,yrange=[0,ymx2],title='Split event spectrum',$
		xtitle='DN',psym=10

	plot,dn,mult_spec,yrange=[0,ymx3],$
		title='Multiply-split event spectrum',xtitle='DN',psym=10,$
		xticklen=-0.02
        
        device,/close
        spawn,'lpr -r idl.ps'
	set_plot,'X'
    	!P.Multi=0	; reset to one plot per page
endif

; plot single event spectrum on image window
plot,dn,single_spec,yrange=[0,ymx1],title='Single event spectrum',$
	xtitle='DN',psym=10, xticklen=-0.02

quit1: 
close,ludebug
free_lun,ludebug
quit2:

end


	


