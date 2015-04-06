;; @(#)wt2evt.pro	1.3
;; Patrick Broos

;; Basic conversion from Windowed Timing data product to event list.
;; "infiles" should be a string array of WT data product filenames.
;; "outfile" is the output Event List you wish to create.
;;
;; Example:
;; infiles = findfile('pass_20043431312_science.0_LDP82.frame*.wt.events.fits')
;; forprint, infiles
;; wt2evt, infiles, 'mydata.evt'

PRO wt2evt, infiles, outfile

event_threshold = 60

; Make space for 64000 events per input frame.
tbl_length = 16000L * n_elements(infiles)
out_tbl = replicate( {TIME:0D, CCDFRAME:0L, CHIPX: 0, CHIPY:0, PHAS:[0,0,0]}, tbl_length )
num_events = 0L

for ii=0, n_elements(infiles)-1 do begin
  prev_num_events = num_events

  ; Put all the Windowed Timing pixels into a simulated CCD "frame".
  pheader = headfits(infiles[ii])
  in_tbl = mrdfits(infiles[ii], 1, in_tbl_header, /SILENT)
  sub_x = in_tbl.CHIPX
  sub_y = in_tbl.CHIPY
  
  sub_frame_cols = 2+max(sub_x)
  sub_frame_rows = 2+max(sub_y)
  
  sub_frame_dn = intarr(sub_frame_cols,sub_frame_rows)
  sz = size( sub_frame_dn )
  sub_frame_dn[sub_x,sub_y] = in_tbl.PHA
  
  
  ;; Test each pixel above the event threshold to determine if there's
  ;; an event at that location.
  crossings = where( sub_frame_dn GT event_threshold, cross_count )
      

    lr_neighbors = [0,2]
    for crossing_index = 0L, cross_count-1 do begin
      event_found = 0
      
      ;Find the X,Y position of the threshold crossing.
      index_to_point, crossings[crossing_index], sub_x, sub_y, sz
	  
      ; We will NOT test for a local maximum if the crossing is on the
      ; edge of sub_frame_dn (since we'd get an array bounds violation)
      if ((sub_x GT 0) AND (sub_x LT sub_frame_cols-1) AND $
	  (sub_y GT 0) AND (sub_y LT sub_frame_rows-1)) then begin
	  
        ;Check for a local maximum.
	island = sub_frame_dn[sub_x-1:sub_x+1, sub_y]
	event_found = (island[1] GE max(island[lr_neighbors]))
      endif
	  
      if (event_found) then begin
        ; print,island	    	    
	; Order the PHAS vector as Event Browser wants it.

	out_tbl[num_events].CHIPX   = sub_x
	out_tbl[num_events].CHIPY   = sub_y
	out_tbl[num_events].PHAS    = round( island[[1,0,2]] )
	out_tbl[num_events].TIME    = sxpar(pheader,'MET_STRT')
	out_tbl[num_events].CCDFRAME= sxpar(pheader,'CCD_FRAM')
	num_events = num_events + 1
      endif ; we found an event
    endfor ; crossing_index loop over the threshold crossings

  print, infiles[ii], num_events-prev_num_events, ' events'
endfor ; ii


print, num_events, ' total events found'
if (num_events GT 0) then begin
  ; Copy first infile's Primary Header to output file.
  pheader = headfits(infiles[0])
  writefits, outfile, 0, pheader
  
  ; Add EVENTS binary table.
  fxaddpar, theader, 'EXTNAME', 'EVENTS'
  mwrfits, out_tbl[0:num_events-1], outfile, theader
endif
return
end




