; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;	REBIN_DATA.PRO	
;
; PURPOSE:
;	Rebin data to desired temporal resolution.
;
; CALLING SEQUENCE:
;
;	REBIN_DATA, x_in, y_in, x_out, y_out [,keywords] 	
;
; INPUTS:
;
;	x_in		:  input x values -- 2 * n array (start and stop times)
;	y_in		:  input y values -- m * n array 
;						m = num of energy channels
;						n = num of data points
;	resolution	:  output data resolution 
;				NOTE :  Must be integer multiple of all data 
;					points within x_in. (except data gaps)
;
; KEYWORD PARAMETERS:
;
;       resolution      :  output data resolution
;	err_in		:  input errors   -- m * n array
;	tolerance	:  allowable difference in bin width (default value
;			   is 2.0 e-4)
;       
; OUTPUTS:
;       
;	x_out		:  output x values
;	y_out		:  output y values
;	err_out		:  output errors  (returned only if err_in is given)  
;
; PROCEDURE:
;        
;
; EXAMPLES:
;       
;
;
; MODIFICATION HISTORY:
;
;	Written, Peter.Woods@msfc.nasa.gov 
;		(256) 961-7637
;
;	05/21/98 - included check on format of y_in, must be m * n
;		   (column format)
;		 - made tolerance a keyword parameter
;       RSM, 1998 May
;            Changed error handling to use MESSAGE routine.
;            Updated to use [] index delimiters.
;            Added ERROR keyword
;       08/10/01 - allowed y_in and err_in to be a row vector when only 
;                  one energy band is being binned
;
;       08/24/02 - RDP: construct a lookup table corresponding to the binning.
;                  Passed through the LOOKUP keyword.
;-
;***************************************************************************

PRO REBIN_DATA, x_in, y_in, x_out, y_out, err_in = err_in, err_out = err_out, $
	resolution = resolution, tolerance = tolerance, error = error, $
	lookup = lookup


error = 0

have_err = N_ELEMENTS(err_in) NE 0
have_res = N_ELEMENTS(resolution) NE 0
have_tol = N_ELEMENTS(tolerance) NE 0
 
IF (have_err) THEN err = err_in

IF (have_res) THEN BEGIN
   res = resolution
ENDIF ELSE BEGIN
   MESSAGE, /CONTINUE, 'No output resolution given.'
   error = 1
   RETURN
ENDELSE

IF (have_tol) THEN BEGIN
   tolerance = tolerance
ENDIF ELSE BEGIN
   tolerance = 4.0e-4            ; tolerance for bin widths (seconds)
ENDELSE				 ; default is 2.0e-4


; Check to see if input data is in correct format

x_size = SIZE(x_in)
y_size = SIZE(y_in)

IF ((x_size[0] NE 2) OR (x_size[1] NE 2)) THEN BEGIN
   MESSAGE, /CONTINUE, 'x input (times) array must be (2, n).'
   error = 1
   RETURN
ENDIF

IF (y_size[0] GT 2) THEN BEGIN
   MESSAGE, /CONTINUE, 'y input (spectra) array must be (m, n) or (n).'
   error = 1
   RETURN
ENDIF

IF (y_size[0] EQ 1) THEN BEGIN
   yform = 'row'
   y_in = TRANSPOSE(y_in)
   y_size = SIZE(y_in)
ENDIF ELSE BEGIN
   yform = 'column'
ENDELSE


IF (have_err) THEN BEGIN
   e_size = SIZE(err_in)
   IF (e_size[0] GT 2) THEN BEGIN
	MESSAGE, /CONTINUE, 'y error array must be (m, n) or (n).'
        error = 1
	RETURN
   ENDIF
   IF ((yform EQ 'row') AND (e_size[0] EQ 1)) THEN BEGIN
   	err_in = TRANSPOSE(err_in)
	e_size = SIZE(err_in)
   ENDIF
   IF (e_size[1] NE y_size[1]) THEN BEGIN
	MESSAGE, /CONTINUE, 'y error array has different dimensions ' + $
            'than y array.'
        error = 1
	RETURN
   ENDIF
ENDIF
   

; Determine the number of data gaps.

x_start = x_in[0,1:*]
x_stop = x_in[1,0:(N_ELEMENTS(x_in[0,*]) - 2)]
x_diff = x_start - x_stop

gap_ind = WHERE( ABS(x_diff) GT tolerance ) + 1

num_gaps = N_ELEMENTS(gap_ind)


; Determine the number of data resolutions.

x_res = REFORM(x_in[1,*] - x_in[0,*])
x_res1 = x_res[1:*]
x_res2 = x_res[0:(N_ELEMENTS(x_res) - 2)]

x_res_diff = x_res1 - x_res2

change_data_ind = WHERE( ABS(x_res_diff) GT tolerance )
IF (change_data_ind[0] EQ -1) THEN BEGIN
   data_type_ind = [0]
ENDIF ELSE BEGIN
   data_type_ind = [0, (change_data_ind + 1)]
ENDELSE

data_res = FLTARR(N_ELEMENTS(data_type_ind))
FOR i = 0, (N_ELEMENTS(data_res) - 1) DO BEGIN
   IF (i LT (N_ELEMENTS(data_res) - 1)) THEN BEGIN
	res_sta = data_type_ind[i]
	res_end = data_type_ind[i+1] - 1
   ENDIF ELSE BEGIN
	res_sta = data_type_ind[i]
	res_end = N_ELEMENTS(x_res) - 1
   ENDELSE
   num_bins = FLOAT(res_end - res_sta + 1)
   data_res[i] = DOUBLE(TOTAL(x_res[res_sta:res_end])/num_bins)
ENDFOR

num_data_types = N_ELEMENTS(data_res)
 

; Make sure requested resolution is a valid one.

check_res = res/data_res
FOR i = 0, (N_ELEMENTS(check_res) - 1) DO BEGIN
   IF (check_res[i] LT 0.990) THEN BEGIN
	check_res[i] = 1.0
   ENDIF
   remainder = ABS(check_res[i] - ROUND(check_res[i]))
   IF (remainder GT 2.0e-2) THEN BEGIN

		MESSAGE, /CONTINUE, $
	   'Resolution at bin ' + STRTRIM(STRING(data_type_ind[i]), 2) + $
	   ' was not an integer multiple of each data point x; skipping.'
;        error = 1
;        RETURN
   ENDIF
ENDFOR

check_res = ROUND(check_res)


; Determine the total number of separate intervals to bin.

interval_ind = data_type_ind
FOR i = 0, (N_ELEMENTS(gap_ind) - 1) DO BEGIN
   duplicate = WHERE(data_type_ind EQ gap_ind[i])
   IF (duplicate[0] EQ -1) THEN interval_ind = [interval_ind, gap_ind[i]]
ENDFOR

interval_ind = interval_ind(SORT(interval_ind))
num_int = N_ELEMENTS(interval_ind)

old_int = FLTARR(3, num_int)
FOR i = 0, (num_int - 1) DO BEGIN
   IF (i NE (num_int - 1)) THEN BEGIN
   	first = FLOAT(interval_ind[i])
   	last = FLOAT(interval_ind[i+1] - 1)
   ENDIF ELSE BEGIN
   	first = FLOAT(interval_ind[i])
   	last = FLOAT(N_ELEMENTS(x_res) - 1)
   ENDELSE
   data_type_pos = WHERE(data_type_ind GT first)
   IF (data_type_pos[0] NE -1) THEN BEGIN
   	bins_per_int = FLOAT(check_res[data_type_pos[0] - 1])
   ENDIF ELSE BEGIN
	bins_per_int = FLOAT(check_res[N_ELEMENTS(check_res) - 1])
   ENDELSE
	
   old_int[0,i] = first
   old_int[1,i] = last
   old_int[2,i] = bins_per_int
ENDFOR


; Find the dimension of the new arrays.

new_int = FLTARR(2, num_int)
int_length = CEIL( (old_int[1,*] - old_int[0,*] + 1)/old_int[2,*] )

first = 0.
FOR i = 0, (num_int - 1) DO BEGIN
   last = first + FLOAT(int_length[i]) - 1.
   new_int[0,i] = first
   new_int[1,i] = last
   first = last + 1.
ENDFOR


num_chan = y_size[1]
size = TOTAL(int_length)

x_out = FLTARR(2, size)
y_out = FLTARR(num_chan, size)
IF (have_err) THEN err_out = FLTARR(num_chan, size)



; Assign values to these new arrays.

lookup = [0L]
FOR i = 0, (num_int - 1) DO BEGIN

   old_beg = old_int[0,i]                 
   old_end = old_int[1,i]
   new_beg = new_int[0,i]
   new_end = new_int[1,i]
   num_old_bins = old_end - old_beg + 1.
   num_new_bins = new_end - new_beg + 1.

   num_per_bin = old_int[2,i]
   IF (num_new_bins EQ 1.) THEN BEGIN
	remainder = num_old_bins
   ENDIF ELSE BEGIN
   	remainder = num_old_bins - ( (num_new_bins - 1) * num_per_bin )   
   ENDELSE
   have_remainder = remainder LT num_per_bin

   start_bin = old_beg

   FOR j = LONG(new_beg), LONG(new_end)  DO BEGIN
   	IF ( (have_remainder) AND (j EQ new_end) ) THEN BEGIN
	   stop_bin = start_bin + remainder - 1
   	ENDIF ELSE BEGIN
	   stop_bin = start_bin + num_per_bin - 1
   	ENDELSE

        IF (start_bin NE 0) THEN BEGIN  ;Must look for gaps!
	   IF (x_in[1, start_bin - 1] NE x_in[0, start_bin]) THEN BEGIN
	      lookup = [lookup, LONG(start_bin)]
	   ENDIF
	ENDIF
	
        lookup = [lookup, LONG(stop_bin)]
	x_out[0, j] = MIN( x_in[0, start_bin:stop_bin] )
	x_out[1, j] = MAX( x_in[1, start_bin:stop_bin] )

	start_bin = stop_bin + 1
   ENDFOR
	
   FOR j = 0, (num_chan - 1) DO BEGIN
   	IF (have_remainder) THEN BEGIN
	   IF (num_new_bins NE 1.) THEN BEGIN
   	   	y_out[j, new_beg:new_end-1] = REBIN(REFORM( $
		   y_in[j, old_beg:(old_end - remainder)]), num_new_bins - 1)
	   	y_out[j, new_end] = TOTAL( $
		   y_in[j, (old_end - remainder + 1):old_end] )/remainder
	   ENDIF ELSE BEGIN
	   	y_out[j, new_end] = TOTAL( $
		   y_in[j, (old_end - remainder + 1):old_end] )/remainder
	   ENDELSE
   	ENDIF ELSE BEGIN
   	   y_out[j, new_beg:new_end] = REBIN(REFORM( $
		y_in[j, old_beg:old_end]), num_new_bins )
   	ENDELSE
   ENDFOR


 IF (have_err) THEN BEGIN

   FOR j = 0, (num_chan - 1) DO BEGIN
   	IF (have_remainder) THEN BEGIN
	   IF (num_new_bins NE 1.) THEN BEGIN
   	   	err_out[j, new_beg:new_end-1] = SQRT(REBIN(REFORM( $
		   err_in[j, old_beg:(old_end - remainder)]^2.), $
		   num_new_bins - 1) * num_per_bin )/ num_per_bin
	   	err_out[j, new_end] = SQRT(TOTAL( $
		   err_in[j, (old_end - remainder + 1):old_end]^2.) )/remainder
	   ENDIF ELSE BEGIN
	   	err_out[j, new_end] = SQRT(TOTAL( $
		   err_in[j, (old_end - remainder + 1):old_end]^2.) )/remainder
	   ENDELSE
   	ENDIF ELSE BEGIN
   	   err_out[j, new_beg:new_end] = SQRT(REBIN(REFORM( $
		err_in[j, old_beg:old_end]^2.), num_new_bins )  * $
		num_per_bin )/ num_per_bin
   	ENDELSE
   ENDFOR

 ENDIF

ENDFOR

;Get rid of superfluous indices:
lastbin = lookup[N_ELEMENTS(lookup) - 1] + 1L
lookup = [lookup, lastbin]
lookup = lookup[UNIQ(lookup)] 

IF (yform EQ 'row') THEN BEGIN
   y_in = REFORM(y_in)
   y_out = REFORM(y_out)
   IF have_err THEN BEGIN
   	err_in = REFORM(err_in)
   	err_out = REFORM(err_out)
   ENDIF
ENDIF




END
