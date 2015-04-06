;+
; NAME:
;   LOMB
;   
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   Craig.Markwardt@nasa.gov
;
; PURPOSE:
;   Compute Lomb-Scargle periodogram of sampled data (possibly irregular)
;
; CALLING SEQUENCE:
;   LOMB, T, X, POW, [PMIN=, PMAX=, PERIOD=period, OVERSAMP=]
;
; DESCRIPTION: 
;
;  The procedure LOMB computes the Lomb-Scargle periodogram of a time
;  series, as described by Press et al in Numerical Recipes.  The time
;  series may be regularly or irregularly sampled.
;
;  The user supplies as input the times T and values X, and the result
;  is returned as the Lomb-Scargle normalized power POW.  The output
;  keyword PERIOD is computed by LOMB, and contains the period for
;  each power.  
;
;  By default the period spacing corresponds to Fourier sampling
;  between periods PMIN and PMAX.  If you wish to over-resolve the
;  periodogram, then set the OVERSAMP keyword to an integer greater
;  than 1.
;
;  Note that this function returns 2 x the standard Lomb-Scargle
;  periodogram, which makes it more comparable to the standard
;  Leahy-normalized power spectrum, and also makes noise powers
;  distributed as a chi^2 with 2 d.o.f.
;
; PARAMETERS:
;
;   T - input array, time values for each time series point, in
;       whatever desired units.
;
;   X - input array, sampled values for each time series point, in
;       whatever desired units.
;
;   POW - output array, normalized power for each output period.
;
; KEYWORD PARAMETERS:
;
;   PERIOD - upon return, PERIOD is set to an array containing the
;            periods used for computing POW.  There is a 1-to-1
;            correspondence between POW and PERIOD.  The units of
;            PERIOD are the same as T.
;
;   PMIN - input scalar, requested minimum period of periodogram, 
;          in the same units as T.
;          DEFAULT: smallest spacing between T values.
;
;   PMAX - input scalar, requested maximum period of periodogram,
;          in the same units as T.
;          DEFAULT: MAX(T) - MIN(T)
;
;   OVERSAMP - input scalar integer, requested oversampling of
;          the periodogram.
;          DEFAULT: 10 (i.e. 10 x Fourier sampling)
;
; SEE ALSO:
;
;   FFT
;
; MODIFICATION HISTORY:
;   Written, CM, ~1999
;   Documented, CM, Jul 2009
;   Removed private utility routine, CM, 25 Aug 2009
;
;  $Id: lomb.pro,v 1.6 2009/09/01 07:31:14 craigm Exp $
;
;-
; Copyright (C) 2009, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Note that this function returns 2 x the standard Lomb-Scargle
;; periodogram, which makes it more comparable to the standard
;; Leahy-normalized power spectrum, and also makes noise powers
;; distributed as a chi^2 with 2 d.o.f.
pro lomb, tj, xj, pow, oversamp=oversamp, pmin=pmin, pmax=pmax, $
          freqavg=ff, period=pp, epoch=epoch, nfbins=nfbins, $
          xerr=xerr, progress=progress

  if n_params() EQ 0 then begin
      message, 'USAGE: LOMB, T, X, POW, PERIOD=PP, PMIN=PMIN, PMAX=PMAX', $
        /info
      return
  endif

  tdiff = tj(1:*) - tj
  tbase = max(tj)-min(tj)
  if n_elements(pmin) EQ 0 then pmin = min(tdiff) > 1D-3
  if n_elements(pmax) EQ 0 then pmax = tbase
  if n_elements(oversamp) EQ 0 then oversamp = 10
  if n_elements(epoch) EQ 0 then epoch = min(tj)

  ;; Frequency ranges
  df = 1/tbase
  fmin = 1/pmax(0)
  fmax = 1/pmin(0)
  if fmax LE fmin then message, 'ERROR: PMIN must be less than PMAX'
  if n_elements(nfbins) EQ 0 then $
    nfbins = oversamp(0)*ceil(abs(fmax-fmin)/df)+1
  if nfbins(0) LT 10 then nfbins = 10
  nfbins = nfbins(0)

  ;; Compute frequencies and periods
  df1 = (fmax-fmin)/(nfbins-1)
  ff = (dindgen(nfbins)*df1) + fmin
  pp = 1/ff
  tpff = 2D*!dpi*ff*dcomplex(0,1)  ;; Pre-compute 2*pi*freq
  pow = double(ff)*0

  ;; Normalize data values by subtracting mean and dividing by variance
  n = n_elements(xj)
  if n_elements(xerr) LT n then begin
      ;; Unweighted
      weighted = 0
      xav = total(xj)/n
      xva = total((xj-xav)^2)/(n-1)
      xx  = (xj-xav)/sqrt(xva)
  endif else begin
      ;; Weighted
      weighted = 1
      wj = 1/xerr^2
      wtot = total(wj)
      xav = total(xj*wj)/wtot
      xx = (xj-xav)/xerr
  endelse
      
  ts  = tj - epoch   ;; Shifted times

  t0 = systime(1)
  for i = 0L, n_elements(ff)-1 do begin

      ;; Compute phase vectors
      zj = exp(tpff(i)*ts)

      ;; Shift phase angle according to Scargle et al
      th = sqrt(total(zj^2)) & rj = (conj(th)/abs(th))*zj
      cj = double(rj)        & sj = imaginary(rj)

      ;; Power value
      pow(i) = total(xx*cj)^2/total(cj^2) + total(xx*sj)^2/total(sj^2)
      if keyword_set(progress) then begin
          if (systime(1) - t0) GT 10d then begin
              print, 100.*double(i)/n_elements(ff), i, n_elements(ff), $
                format='("  ",F8.1," percent complete  (",I0," of ",I0,")")'
              t0 = systime(1)
          endif
      endif
  endfor

  return
end
