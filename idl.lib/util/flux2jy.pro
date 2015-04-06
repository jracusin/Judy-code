function flux2jy,flux,gamma,low=low,high=high,eeff=eeff,fluxerr=fluxerr,gammaerr=gammaerr,silent=silent,ferr=ferr

  if n_params() eq 0 then begin
     print,'jy=flux2jy(flux,gamma,low=low,high=high,eeff=eeff)'
     print,'          defaults low=0.3 keV, high=10 keV, eeff=1 keV'
     return,0
  end 
  ;;;translated from Nora's python program
  
  ;; 1 erg = 1e-7 J
  ;; 1 Jy = 10-23 ergs cm-2 s-1 Hz-1 = 10^-30 W/cm^2/Hz = 10^-26 W/m^2/Hz
  ;; 1 eV = 1.60217653e-19 J
  ;; 1 keV = 1.60217653e-16/6.626e-34 = 2.42e+17 Hz
  erg2jy = 1d23/2.42d17

  if n_elements(low) eq 0 then low=0.3 ;keV
  if n_elements(high) eq 0 then high=10. ;keV
;  if n_elements(eeff) eq 0 then eeff=1. ;keV

  if n_elements(eeff) gt 0 then eav=eeff else $
     eav=1.
;     eav=10.^(alog10(high*low)/2.)
  eeff=eav

  ;; case sig of
  ;;    1: v=0.68
  ;;    2: v=0.954
  ;;    3: v=0.997
  ;;    else v=sig
  ;; endcase 
     
  ;; s1=round(1000.*(1-v)/2.)
  ;; s2=round(1000.*(1.-(1-v)/2.))

  if not keyword_set(silent) then begin 
     print,'low = ',low,' keV'
     print,'high = ',high,' keV'
     print,'Eeff = ',eeff,' keV'
  endif 
  
  if gamma eq 2 then begin 
     fav=flux/eav*alog10(high/low) 
     if n_elements(fluxerr) gt 0 then begin
        ferr=fluxerr/eav*alog10(high/low)
     endif 
  endif else begin
     fav=flux*(2.-gamma)/(eav*((high/eav)^(2.-gamma) - (low/eav)^(2.-gamma)))
     ;;; aahh, had this wrong for years, high/eav, not high/low

     ;;; error propagation is NOT CORRECT!!!
     if n_elements(fluxerr) gt 0 then begin
;        dfdensdg=flux*(-1./(eav*((high/eav)^(2.-gamma)-(low/eav)^(2.-gamma)))-(2.-gamma)*((low/eav)^(2.-gamma)*alog10(low/eav)-(high/eav)^(2.-gamma)*alog10(high/eav))/(eav*((high/eav)^(2.-gamma)-(low/eav)^(2.-gamma))^2.))
        dfdensdg=flux*eav^(1-gamma)*((-1./(high^(2-gamma)-low^(2-gamma)))-((2-gamma)*alog(eav)/(high^(2-gamma)-low^(2-gamma)))-((2-gamma)*(low^(2-gamma)*alog(low)-high^(2-gamma)*alog(high))/(high^(2-gamma)-low^(2-gamma))^2))
;        dfdensdg2=flux*eav^(1-gamma)*((gamma-2)*alog10(eav)+1.)/(gamma-2)^2

;        stop
;        dfdensdg=(-((high/low)^(2.-gamma)*(alog(low)-alog(high))-(low/eav)^(2.-gamma)*(alog(eav)-alog(low)))*flux*(2.-gamma)/(eav*((high/low)^(2.-gamma) - (low/eav)^(2.-gamma)))^2)-(flux/(eav*((high/low)^(2.-gamma) - (low/eav)^(2.-gamma))))
        dfdensdf=(2.-gamma)/(eav*((high/eav)^(2.-gamma) - (low/eav)^(2.-gamma)))       
;        dfdensdf=(2.-gamma)/(eav*((high/low)^(2.-gamma) - (low/eav)^(2.-gamma)))
;        ferr=sqrt(fluxerr^2*dfdensdf^2+ratioerr^2*dfdensdratio^2+gammaerr^2*dfdensdg^2)
        ferr=sqrt(fluxerr^2*dfdensdf^2+gammaerr^2*dfdensdg^2)
        ferr=erg2jy*ferr
     endif 

  endelse 

  fav=erg2jy*fav
  nu_eff=eav*1.60217653d-16/6.626d-34
  feff = fav
  eeff = eav

  return,feff
end 
