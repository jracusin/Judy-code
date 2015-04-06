@fit_functions
@fit_functions_flares
pro lc4amy

  l=mrdfits('~/Swift/swift_grb_properties.fits',1)

  cd,'~/iLobster/simulations/amy/'

  for i=0,n_elements(l)-1 do begin 
     t=findgen(1e4)+1.
     w=where(t gt l[i].tstart,nw)
     if nw gt 0 and strtrim(l[i].model,2) ne 'nofit' then begin 

        f=call_function(strtrim(l[i].model,2),t[w],l[i].p)*l[i].cfratio
        writecol,strtrim(l[i].grb,2)+'_lc_flux.dat',t[w],f

     endif 
  endfor 
return
end 
