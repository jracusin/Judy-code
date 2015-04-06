PRO genrand2d, prob, xvals, yvals, nrand, xrand, yrand, $
               double=double, plt=plt, $
               nxbins=nxbins, nybins=nybins

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  
;    GENRAND2D
;       
; PURPOSE:  
;    Generate random numbers from a 2d probability function.
;	
;
; CALLING SEQUENCE:
;      genrand2d, prob, xvals, yvals, nrand, xrand, yrand, $
;                 double=double, nxbins=nxbins, nybins=nybins, /plt
;                 
;
; INPUTS: 
;    prob: input probablility function.  
;    xvals, yvals: x-y values corresponding to the surface prob z values
;    nrand: How many points to generate from the probability function.
;
; KEYWORD PARAMETERS:
;         /double: Return double precision?
;         /plt: make a plot of the generated data and input distribution
;         nxbins=nxbins, nybins=nybins: number of bins to use in plot
;       
; OUTPUTS: 
;    xrand, yrand:  arrays of random numbers generated from prob
; 
; PROCEDURE: 
;  Generates random numbers from the input distribution by generating x,y,z
;           values uniformly and keeping those that lie beneath the probability
;           surface.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofChicago 02-Mar-2003
;                                      
;-                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() LT 4 then begin
	print,'genrand2d, prob, xvals, yvals, nrand, xrand, yrand, double=double, plt=plt, nxbins=nxbins, nybins=nybins'
	return
  endif

  ;; This way only one seed is generated per IDL session.  
  COMMON genrand_seed_block, seed

  nx = n_elements(xvals)
  ny = n_elements(yvals)

  ss = size(prob)

  IF ss[0] NE 2 THEN message,'Probability surface must be a 2d array'

  if (nx ne ss[1]) OR (ny NE ss[2]) then begin
	print,'size(xvals) must equal x-size of prob, same for yvals'
	return
  endif

  IF NOT keyword_set(double) THEN double = 0

  ;; generate the random points

  xrand = dblarr(nrand)
  yrand = xrand

  xi = lindgen(nx)
  yi = lindgen(ny)

  minx = min(xvals, max=maxx)
  miny = min(yvals, max=maxy)

  maxprob = max(prob)
  minprob = 0d

  ngood = 0L
  nbad = nrand
  bad = lindgen(nbad)

  WHILE ngood LT nrand DO BEGIN 
      
;      IF NOT keyword_set(silent) THEN BEGIN 
;          IF ngood EQ 0 THEN print,'Generating ',ntostr(nbad) $
;          ELSE print,'Re-Generating ',ntostr(nbad)
;      ENDIF 
      
      randx = arrscl(randomu(seed, nbad, /double), $
                     minx, maxx, $
                     arrmin=0d, arrmax=1d )
      
      randy = arrscl(randomu(seed, nbad, /double), $
                     miny, maxy, $
                     arrmin=0d, arrmax=1d )
      
      randprob = arrscl(randomu(seed, nbad, /double), $
                        minprob, maxprob, $
                        arrmin=0d, arrmax=1d )

      rxi = interpol(xi, xvals, randx)
      ryi = interpol(yi, yvals, randy)

      interp_prob = interpolate(prob, rxi, ryi)
      
      tgood = where(randprob LE interp_prob, ntgood, $
                    comp=tbad, ncomp=ntbad)
      
      IF ntgood NE 0 THEN BEGIN  
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; we found some good ones
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          good_ids = bad[tgood]
          
          ;; copy in the good ones
          xrand[good_ids] = randx[tgood]
          yrand[good_ids] = randy[tgood]

          ngood = ngood + ntgood
          
          IF ntbad NE 0 THEN bad = bad[tbad]
          nbad = ntbad
          
      ENDIF 
      
      IF NOT double THEN BEGIN 
          xrand = float( temporary(xrand) )
          yrand = float( temporary(yrand) )
      ENDIF 

  ENDWHILE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; plot the results
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(plt) then begin
     ploth, xrand, yrand, /silent, xtitle='X rand', ytitle='Y rand', title='2-d Prob', nxbins=nxbins, nybins=nybins
     contour, prob, xvals, yvals, /overplot, nlevels=3

     legend,'Input distribution',line=0,/right,box=0

  endif

return
end









