pro write_snapshot_spec, snapshot_spec
;
; This procedure plots the snapshot_spec on the screen, prints the plot, and writes
;	the bias row to disk as a 1-D FITS file.
;

@pass1_common

; plot the snapshot spectrum
print_snapshot_trailer,8,0,'First 80 points of histogram:'	
for i=0,4 do printf,lushdr, $				; print data
	format="(16i6)",snapshot_spec((i*16):(i*16+15))
if (idebug ge 8) then begin
	for i=0,4 do print, $		; print data
		format="(16i5)",snapshot_spec((i*16):(i*16+15))
endif

case show_plot of
	1: print,'Plotting histogram...'
	2: print,'Calling interactive plotting widget to plot histogram ...'
	else: temp=0
endcase

if (max(snapshot_spec) gt 0) and (show_plot gt 0) then begin
	old_window = !D.window
	if (n_elements(spectrum_window) eq 0) then begin
		window,/free,title='Snapshot Accumulated CCD Spectrum'
		spectrum_window = !D.window
	endif else wset, spectrum_window

	p1plotter,show_plot,snapshot_spec, $
	    title='Accumulated Raw CCD Histogram (Snapshot Trailer) for file '+filebase,$
	    xtitle='DN',ytitle='Counts/bin',/histogram, min=0.5, binsize=1, $
		/use_active_window
	if (n_elements(old_window) ne 0) then $
		if (old_window(0) ne -1) then wset, old_window
endif

if ((show_plot eq 1) and (print_plots ge 1)) then begin	; send plot to printer
	set_plot,'ps'
	device,/landscape
	num_points = n_elements(snapshot_spec)
	x_data = lindgen(num_points)
	binsize = 1
	minbin = 0.5
	x_data = x_data * binsize + minbin + binsize/2.0
	snapshot_spec = [0, snapshot_spec, 0]
	x_data = [x_data(0)-binsize, x_data, x_data(num_points-1)+binsize]*4
	psym = 10
	ymin = min(snapshot_spec)
	ymax = max(snapshot_spec)
   	xrange = [min(x_data),max(x_data)]
	delta = ((ymax - ymin)*0.1/2.0) > 0.1
        yrange = [ymin-delta, ymax+delta]

      	plot, x_data, snapshot_spec, TITLE='Accumulated Raw CCD Histogram (Snapshot Trailer) for file '+filebase, PSYM=psym, $
	    XTITLE='DN', YTITLE='Counts/bin', XRANGE=xrange, YRANGE=yrange
	
	device,/close
	spawn,'lpr -r idl.ps'
	set_plot,'X'
endif


; set up the FITS header
file_info = fstat(luin)
file_date = bin_date(systime(0))
date = string(file_date(2),file_date(1),format="(i2.2,'/',i2.2,'/')") $
		+ strmid(string(file_date(0),format="(i4.4)"),2,2)

mkhdr,snapshot_hdr,snapshot_spec
sxaddhist, ' ', snapshot_hdr
sxaddhist,'*** SWIFT XRT *** Snapshot Spectrum processed by PASS1 '+version, $
			snapshot_hdr
sxaddpar, snapshot_hdr, 'ORIGIN','PSU X-ray Astronomy', $
			'Data from Penn State X-ray Astronomy'
sxaddpar, snapshot_hdr, 'TELESCOP', 'Swift', 'Swift Gamma-Ray Burst Explorer'
sxaddpar, snapshot_hdr, 'INSTRUME','XRT',$
			'XRT Instrument on Swift Gamma-ray Burst Explorer'
sxaddhist, ' ', snapshot_hdr
sxaddpar, snapshot_hdr, 'TARGETID',strtrim(string(target_id),2), 'Swift Target ID'
sxaddpar, snapshot_hdr, 'OBS_SEG', strtrim(string(obs_segment),2), 'Swift Observation Segment for this target'
sxaddpar, snapshot_hdr, 'SEQ_NUM',  strtrim(string(seq_num),2), 'Swift Sequence Number = Target_ID*1000+Obs_Seg'
sxaddhist, ' ', snapshot_hdr
sxaddpar, snapshot_hdr, 'CREATOR',  'PASS1 (write_snapshot_spec), '+version

sxaddhist, ' ', snapshot_hdr
sxaddhist, 'XRT Snapshot Spectrum (integrated raw spectrum for entire Snapshot)', snapshot_hdr
sxaddpar, snapshot_hdr, 'FILEIN',filebase,'Name of input data file'
sxaddpar, snapshot_hdr, 'DATE',date,'File creation date'
sxaddpar, snapshot_hdr, 'FRAME',snapshot_number,'Snapshot number of this snapshot spectrum'

sxaddhist, ' ', snapshot_hdr
sxaddhist, 'CCD Frame Time Stamps:', snapshot_hdr
sxaddpar, snapshot_hdr, 'TSTART', snapshot_begin, 'Satellite time of snapshot start'
sxaddpar, snapshot_hdr, 'TSTOP', snapshot_end, 'Satellite time of snapshot end'
sxaddpar, snapshot_hdr, 'TELAPSE', snapshot_end - snapshot_begin, 'Elapsed time'

; Create a FITS file
file_spec = filebase + '.snapshot_spec.fits'
sxaddpar, snapshot_hdr, 'FILE', file_spec, 'Name of this file'

writefits, file_spec, snapshot_spec, snapshot_hdr

end
