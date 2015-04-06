function uvot2jy,x,filt,mag=mag,flux=flux,extinction=extinction,freq=freq

  if n_params() eq 0 then begin
     print,'syntax - jy=uvot2jy(rate, filter [,/mag, /flux, freq=freq])'
     return,0
  endif 

  filter=['v','b','u','uvw1','uvm2','uvw2','white']
  lam_eff=[5402,4329,3501,2634,2231,2030,3471]*1d-8 ;; cm
  fluxfact=[2.614,1.472,1.63,4.00,8.50,6.2,0.37]*1d-16 ;; erg cm-2 s-1 ang-1
  zpt=[17.89,19.11,18.34,17.49,16.82,17.35,20.29]
  c=3d10                                               ;; cm/s
  freqs=c/lam_eff

  nf=n_elements(filt)
  if nf ne n_elements(x) then begin 
     w=where(strtrim(filt,2) eq filter or strtrim(filt,2) eq strupcase(filter),nwf)
     if nwf eq 0 then begin
        print,'No filter match for '+filt
        return,0
     end 
;     print,filter[w]
     fd=x*fluxfact[w[0]]*1d31*lam_eff[w[0]]^2/c
     if keyword_set(flux) then fd=x*1d31*lam_eff[w[0]]^2/c
     if keyword_set(mag) then fd=fluxfact[w[0]]*10^(-0.4*(x-zpt[w[0]]))*1d31*lam_eff[w[0]]^2/c
     freq=freqs[w[0]]
  endif else begin
     fd=dblarr(nf)
     freq=fd
     for i=0,nf-1 do begin
        w=where(strtrim(filt[i],2) eq filter or strtrim(filt[i],2) eq strupcase(filter),nwf)
        if nwf eq 0 then begin
           print,'No filter match for '+filt[i]
           return,0
        end 
;        print,filter[w]
        fd[i]=x[i]*fluxfact[w[0]]*1d31*lam_eff[w[0]]^2/c
        if keyword_set(flux) then fd[i]=x[i]*1d31*lam_eff[w[0]]^2/c
        if keyword_set(mag) then fd[i]=fluxfact[w[0]]*10^(-0.4*(x[i]-zpt[w[0]]))*1d31*lam_eff[w[0]]^2/c
        freq[i]=freqs[w[0]]
     endfor 
  endelse 

  return,fd
end 
