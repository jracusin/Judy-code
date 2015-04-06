; Bins input data array
; Keyword XRUN is to specify the xrange exactly with a 2-element array.
;  If not set, xrange is computed from the array INARR
;  If set, it may occur that some values of INARR are greater
;  than the maximum of XRUN.  In this case, the last bin is an
;  overflow bin, and the number of values of INARR placed in this bin
;  is returned by OVERFLOW, else OVERFLOW = 0
; Keyword LOG is set to bin the data in logarithmic bins, else data
;  is binned into linear bins.  
;
; R.S. Mallozzi, Aug, 1994

PRO BIN_DATA, inarr, num_bins, bin_cent, bin_edge, bin, overflow, $
              xrun=xrun, log=log

inarr_save = inarr

inarr = inarr(SORT(inarr))

if NOT KEYWORD_SET(xrun) then begin
 start_pt = FLOOR(MIN(inarr))
 end_pt   = CEIL(MAX(inarr))
endif else begin
 start_pt = xrun(0)
 end_pt   = xrun(1)
endelse

tarr = [start_pt, end_pt]
if KEYWORD_SET(log) then tarr = ALOG10(tarr)

bin_width = (tarr(1) - tarr(0)) / FLOAT(num_bins)
bin_edge  = FLTARR(num_bins+1)
 for i=0,num_bins do bin_edge(i) = tarr(0) + bin_width*i
bin_cent  = FLTARR(num_bins)
 for i=0,num_bins-1 do bin_cent(i) = bin_edge(i) + (bin_width/2.0)


if KEYWORD_SET(log) then inarr = ALOG10(inarr)

bin = INTARR(num_bins)
for i=0,num_bins-1 do begin
 index  = WHERE(inarr ge bin_edge(i) and inarr lt bin_edge(i+1))
 if i eq num_bins-1 then index = WHERE(inarr ge bin_edge(i))

 bin(i) = N_ELEMENTS(index)
 if TOTAL(index) eq -1 then bin(i) = 0
endfor
;print,'Number of events: ',FIX(TOTAL(bin))

; Last bin is an overflow bin
overf = WHERE(inarr_save gt end_pt, overflow)

;if KEYWORD_SET(log) then begin
; bin_cent = 10.0^bin_cent
; bin_edge = 10.0^bin_edge
;endif

inarr = inarr_save


 
END
