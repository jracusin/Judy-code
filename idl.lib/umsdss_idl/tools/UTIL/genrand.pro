PRO genrand, prob, xvals , nrand, rand, $
             cumulative=cumulative, plt=plt, bin=bin, $
             double=double,old=old, quadratic=quadratic, method=method

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  
;    GENRAND
;       
; PURPOSE:  
;    Generate random numbers from an input probability function.
;	
;
; CALLING SEQUENCE:
;      genrand, prob, xvals , nrand, [, rand, /cumulative, /double, /quadratic,
;      plt=plt, bin=bin])
;                 
;
; INPUTS: 
;    prob: input probablility function.  
;    xvals: abscissae
;    nrand: How many points to generate from the probability function.
;
; KEYWORD PARAMETERS:
;         method=method: method=0 (default) use integral technique
;                        method=1 use 2-d cut technique 
;         cumulative: tells the program that prob is the cumulative 
;	         probablility.  Using this option saves processing time.
;                Note for method=1 this is not applicable.
;         /double: use double precision?
;         /quadratic: use quadratic interpolation?
;         /plt: plot histogram if requested
;         bin: how to bin the histogram (irrelevent if not /plt )
;       
; OUTPUTS: 
;    rand:  an array of random numbers generated from prob. size nrand
; 
; PROCEDURE: 
;  Method=0 (default)
;    Generates random numbers from the input distribution by inverting
;    the cumulative probability function.  Performs interpolation
;    using the interpol function.
;	
;  Method=1
;  Generates random numbers from the input distribution by generating x,y
;           values uniformly and keeping those that lie beneath the 
;           probability distribution
;  
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofM  1/??/99
;       March 2003: Use interpolation.  Introduced method=1 E.S.S.
;       
;                                      
;-                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  if N_params() LT 3 then begin
	print,'Syntax: genrand, prob, xvals , nrand [, rand, cumulative=cumulative, '+$
          'method=method, plt=plt,bin=bin,double=double,quadratic=quadratic] )'
        print,'Outputs rand, set of random numbers generated from prob.'
        print,'Use doc_library, "genrand" for more help'
	return
  endif

  ;; This way only one seed is generated per IDL session.  
  COMMON genrand_seed_block, seed

  nx = n_elements(xvals)
  if (nx ne n_elements(prob)) then begin
	print,'prob and xvals must be of same size'
	return
  endif
  IF NOT keyword_set(double) THEN double = 0

  IF n_elements(method) EQ 0 THEN method = 0

  IF method EQ 0 THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;
      ;; Integral method
      ;;;;;;;;;;;;;;;;;;;;;;;

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  If prob is not a cumulative probability function 
      ;;  we need to generate one from prob
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      if not keyword_set(cumulative)  then begin
          IF double THEN intfunc,double(prob), xvals, cumdist $
          ELSE intfunc,float(prob), xvals, cumdist
      endif else begin
          cumdist=prob
      endelse
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Normalize probability function 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
      norm = cumdist[nx-1]
      cumdist = cumdist/norm

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; generate a set of uniform random numbers from 0 to 1. Store in
      ;; the array testrand
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      testrand = randomu(seed,nrand)
 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; generate numbers from distribution prob 
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF float(!version.release) GE 5.4 THEN BEGIN 
          rand = interpol(xvals, cumdist, testrand,quadratic=quadratic)
      ENDIF ELSE BEGIN 
          IF keyword_set(quadratic) THEN message,'Cannot use /quadratic for IDL version '+!version.release+'.  Doing linear interpolation', /inf
          rand = interpol(xvals, cumdist, testrand)
      ENDELSE 
 
  ENDIF ELSE BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; monte-carlo 2d cut method
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      rand = dblarr(nrand)

      minx = min(xvals, max=maxx)
      maxprob = max(prob)
      minprob = 0d

      ngood = 0L
      nbad = nrand
      bad = lindgen(nbad)

      WHILE ngood LT nrand DO BEGIN 

;          IF NOT keyword_set(silent) THEN BEGIN 
;              IF ngood EQ 0 THEN print,'Generating ',ntostr(nbad) $
;              ELSE print,'Re-Generating ',ntostr(nbad)
;          ENDIF 

          randx = arrscl(randomu(seed, nbad, /double), $
                         minx, maxx, $
                         arrmin=0d, arrmax=1d )

          randprob = arrscl(randomu(seed, nbad, /double), $
                         minprob, maxprob, $
                         arrmin=0d, arrmax=1d )

          IF float(!version.release) GE 5.4 THEN BEGIN 
              interp_prob = interpol(prob, xvals, randx,quadratic=quadratic)
          ENDIF ELSE BEGIN 
              IF keyword_set(quadratic) THEN message,'Cannot use /quadratic for IDL version '+!version.release+'.  Doing linear interpolation', /inf
              interp_prob = interpol(prob, xvals, randx)
          ENDELSE 

          tgood = where(randprob LE interp_prob, ntgood, $
                        comp=tbad, ncomp=ntbad)

          IF ntgood NE 0 THEN BEGIN  

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; we found some good ones
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

              good_ids = bad[tgood]

              ;; copy in the good ones
              rand[good_ids] = randx[tgood]

              ngood = ngood + ntgood

              IF ntbad NE 0 THEN bad = bad[tbad]
              nbad = ntbad

          ENDIF 

      ENDWHILE 

      IF NOT double THEN rand = float(temporary(rand))

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; plot the results
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if keyword_set(plt) then begin
     if (not keyword_set(bin) ) then bin=2.*(xvals[1] - xvals[0])
     plothist,rand, bin=bin, /norm
     defsysv, '!red', exist=redexist
     IF redexist THEN clr = !red ELSE clr=!p.color
     if (not keyword_set(cumulative) ) then begin
	   oplot,xvals,float(prob),color=clr,thick=2
     endif
  endif

  IF n_elements(cumdist) NE 0 THEN cumulative = cumdist

return
end









