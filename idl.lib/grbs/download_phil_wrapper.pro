pro download_phil_wrapper,whichgrb,onlycurve=onlycurve,onlyspec=onlyspec,bat=bat

;  readcol,'~/GRBs/grb_table_1328645746.txt',grbname,tid,format='(a,a)',delim='|',skip=1
  grbs=mrdfits('~/Swift/swiftgrb_list.fits',1) 
  grbname=grbs.grb
  tid=grbs.trignum

  grb=strcompress(grbname,/rem)
  n=n_elements(grb)
  tid=strtrim(tid,2)
  nostop=0
  if n_elements(whichgrb) gt 0 then begin
     w=where(grb eq whichgrb)
     grb=grb[w]
     tid=tid[w]
     n=1
     nostop=1
  endif 

  g=0
  colprint,indgen(n),grb
  print,g,n
  if not nostop then stop
  cd,'~/GRBs'
  for i=g,n-1 do begin 
     if not exist(grb[i]) then spawn,'mkdir '+grb[i]

        print,grb[i],tid[i]

        cd,grb[i]
        if not keyword_set(onlyspec) then download_phil_lc,grb[i],tid[i],dir='~/GRBs/',onlycurve=onlycurve,bat=bat
        if not keyword_set(onlycurve) then download_phil_spec,grb[i],tid[i],dir='~/GRBs/'
        cd,'..'
;     endif 
  endfor 
return
end 
  
