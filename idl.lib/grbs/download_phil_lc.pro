pro download_phil_lc,grb,tids,flux=flux,bat=bat,dir=dir,onlycurve=onlycurve
  
  if n_params() eq 0 then begin
     print,'syntax - download_phil_lc,dir,tid'
     print,"         (e.g. dir='GRB080123')"
     return
  endif 
  
;  cd,!mdata
  if n_elements(dir) eq 0 then cd,'~/GRBs/' else print,dir
  for i=0,n_elements(grb)-1 do begin 
     if not exist(grb[i]) and n_elements(dir) eq 0 then begin
        print,'No directory exists for '+grb[i]+', making one'
        spawn,'mkdir '+grb[i]
     endif 
     if n_elements(dir) eq 0 then cd,grb[i]
     tid=tids[i]
     if n_elements(tids) eq 0 then begin 
        obs=file_search('00*')
        tid=strmid(obs[0],0,8)
        tid2=ntostr(tid)
     endif else begin
        if tid*1d lt 100000 then tid2='000'+tid else tid2='00'+tid
     endelse 
     print,grb[i],'  ',tid
     
     if not keyword_set(flux) and not keyword_set(bat) then begin 
        if not keyword_set(onlycurve) then begin 
           if exist('WTCURVE.qdp') then spawn,'rm WTCURVE.qdp'
           if exist('PCCURVE.qdp') then spawn,'rm PCCURVE.qdp'
           if exist('PCUL.qdp') then spawn,'rm PCUL.qdp'
           url='http://www.swift.ac.uk/xrt_curves/'+tid2+'/WTCURVE.qdp'
           com='wget '+url
           print,com
           spawn,com
           url='http://www.swift.ac.uk/xrt_curves/'+tid2+'/PCCURVE.qdp'
           com='wget '+url
           print,com
           spawn,com
           url='http://www.swift.ac.uk/xrt_curves/'+tid2+'/PCUL.qdp'
           com='wget '+url
           print,com
           spawn,com
        endif 
        if exist('curve.qdp') then spawn,'rm curve.qdp'
        url='http://www.swift.ac.uk/xrt_curves/'+tid2+'/curve.qdp'
        com='wget '+url
        print,com
        spawn,com

     endif else begin
        if keyword_set(flux) then $
           url='http://www.swift.ac.uk/xrt_curves/'+tid2+'/flux.qdp'
        if keyword_set(bat) then $
           url='http://www.swift.ac.uk/burst_analyser/'+tid2+'/bat/bat_flux_snr5_with_cf_XRTBAND.qdp.gz'
;http://www.swift.ac.uk/burst_analyser/00554620/bat/bat_flux_snr5_with_cf_DENSITY.qdp.gz
        com='wget '+url
        print,com
        spawn,com

     endelse 
     lc=lcout2fits(/phil)
     mwrfits,lc,'UL_lc.fits',/create
  
     if n_elements(dir) eq 0 then cd,'..'
  endfor
  
  return
end 
