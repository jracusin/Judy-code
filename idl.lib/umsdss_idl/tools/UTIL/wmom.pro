PRO wmom, array, sigma, wmean, wsig, wmerr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    WMOM
;
; PURPOSE:
;    Find mean and sigma of an array, weighting by the standard
;         deviation of each point
;
; CALLING SEQUENCE: 
;    wmom, array, sigma, wmean, wsig, werr, wvar=wvar
;
; PROCEDURE: 
;    wi      = 1/sig[i]^2
;    wmean   = sum( xi*wi)/sum(wi)
;    wsig^2  = sum( wi*(xi-wmean)^2 )/sum(wi)
;            
;    wmerr^2 = sum( wi^2*(xi - wmean)^2 ) / ( sum(wi) )^2
;
; REVISION HISTORY:
;    Author: Erin Scott Sheldon UofMich  8/99
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: wmom, array, sigma, wmean, wsig, wmerr'
      return
  ENDIF

  g=where(sigma GT 0.0, ng)
  
  w = ( 1./sigma[g]^2 )
  wtot = total(w)
  
  wmean = total(w*array[g])/wtot
  
  ;; Uncertainty in the mean.
  ;;wmvar = total( w^2*(array[g] - wmean)^2 )/wtot^2
  ;;wmerr = sqrt(wmvar)

  wmerr = 1./sqrt(wtot)

  ;; Variance about the mean.
  wvar = total( w*(array[g] - wmean)^2)/wtot
  wsig=sqrt(wvar)

return
END
