function opt2jy,x,filt,mag=mag,flux=flux,freq=freq

  if n_params() eq 0 then begin
     print,'syntax - jy=uvot2jy(mag, filter)'
     return,0
  endif 

  filter=['u','b','v','r','i','z','j','h','k','Rc','Ic']
  lam_eff=[0.36,0.44,0.545,0.64,0.79,0.91,1.26,1.6,2.22,0.64,0.79]*1d-4 ;; cm
  fzp=[1810,4260,3640,3080,2550,4810,1600,1080,670,2254,1196]*1d
  c=3d10                                               ;; cm/s
  freqs=c/lam_eff

  nf=n_elements(filt)
  if nf ne n_elements(x) then begin 
     w=where(filt[0] eq filter or filt[0] eq strupcase(filter),nwf)
     if nwf eq 0 then begin
        print,'No filter match for '+filt
        return,0
     end 
;     print,filter[w]
     fd=fzp[w[0]]*10^(-0.4*x);*1d31*lam_eff[w[0]]^2/c
     freq=freqs[w[0]]
  endif else begin
     fd=dblarr(nf)
     freq=fd
     for i=0,nf-1 do begin
        w=where(filt[i] eq filter or filt[i] eq strupcase(filter),nwf)
        if nwf eq 0 then begin
           print,'No filter match for '+filt[i]
           return,0
        end 
;        print,filter[w]
     fd[i]=fzp[w[0]]*10^(-0.4*x[i]);*1d31*lam_eff[w[0]]^2/c
     freq[i]=freqs[w[0]]
     endfor 
  endelse 

  return,fd
end 
