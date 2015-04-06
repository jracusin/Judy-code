pro sigma_clip,array,amean,asigma,nsig=nsig,nIter=nIter,$
               plot=plot,_extra=ex,pause=pause,index=index,silent=silent

;+
; NAME:
;    SIGMA_CLIP
; PURPOSE:
;    a simple sigma clipping algoritm
;-

  if n_params() eq 0 or not keyword_set(nsig) or $
    not keyword_set(nIter) then begin
      print, '-syntax sigma_clip,arr,mean,sigma,nsig=nsig,'
      print,'nIter=nIter,print=print,pause=pause,_extra=ex,plot=plot'
      return
  endif

  IF NOT keyword_set(silent) THEN silent = 0 ELSE silent=1

  wif=n_elements(array)
  index=lindgen(wif)
  
  for i=0,nIter-1 do begin
      if keyword_set(plot) then BEGIN
          IF n_elements(parts) EQ 0 THEN parts=(max(array)-min(array))/50
          if i eq 0 then plothist,array[index],_extra=ex,bin=parts
          if i gt 0 then plothist2,array[index],/overplot,bin=parts,_extra=ex
      endif

      mom=moment(array[index], Maxmoment=2)
      m=mom(0)
      s=sqrt(mom(1))
      clip=nsig*s
      if NOT silent then print,' mean',m,'   sigma',s,'  number',wif 
      w=where(abs(array[index]-m) lt clip,wif)
      if wif eq 0 then begin
          IF NOT silent THEN print,'nsig is too small. Everything clipped on iteration',i
          ;mom=moment(array[index], Maxmoment=2)
          amean=m
          asigma=s
          return
      endif 
      if n_elements(pause) gt 0 then wait,pause
      index=index(w)
  endfor

  mom=moment(array[index], Maxmoment=2)
  amean=mom(0)
  asigma=sqrt(mom(1))
  if NOT silent then print,' final mean',amean,'   final sigma',asigma
  return
end


