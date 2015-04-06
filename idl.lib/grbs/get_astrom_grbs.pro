pro get_astrom_grbs
  
;  get_sdc_data,year,month,targetid
  
  grb=['grb050509','grb051227','grb060108','grb060204b','grb060206','grb060218','grb060219','grb060319','grb060323','grb060428','grb060510','grb060512','grb060602','grb060717','grb060801','grb060807','grb060814','grb060904a','grb060904b','grb060908']
  year='20'+strmid(grb,3,2)
  year=year*1
  month=strmid(grb,5,2)*1
  tid=[118749,174738,176453,180241,180455,191157,191512,202035,202505,207399,209351,209755,213190,219646,222154,223217,224552,227996,228006,228581]
  
  for i=0,n_elements(grb)-1 do begin
     spawn,'mkdir '+grb[i]
     cd,grb[i]
     
     get_sdc_data,year[i],month[i],tid[i]
     run_xrtpipeline
     
     cd,'../'
  endfor 
  
  return
end 
