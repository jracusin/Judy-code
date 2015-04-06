pro make_jetbreak_tables,cr
  
  openw,lun,'everything_table.tex',/get_lun
  n=n_elements(cr)
  
  for i=0,n-1 do begin
     if cr[i].seg0 eq 1 then seg='?'
     if cr[i].seg1 eq 1 then seg='1'
     if cr[i].seg2 eq 1 then seg='2'
     if cr[i].seg3 eq 1 then seg='3'
     if cr[i].seg4 eq 1 then seg='4'
     grb=cr[i].grb
     if i gt 0 then lastgrb=cr[i-1].grb else lastgrb=''
     if lastgrb eq grb then grb=' '
     
     alphaerr=cr[i].alphaerr
     alpha=cr[i].alpha
     w=where(finite(alphaerr) eq 0,nw)
     if nw gt 0 then alerr='' else begin
        alerr0=sigfig(alphaerr[0],3)
        alerr1=sigfig(alphaerr[1],3)
     endelse 
     w=where(alpha lt 1e-5,nw)
     if nw gt 0 then al='0.00' else al=sigfig(alpha,3)
     
     printf,lun,grb+' & '+seg+' & $'+al+'^{+'+alerr1+'}_{-'+alerr0+'}$ & $'+sigfig(cr[i].beta,3)+'^{+'+sigfig(cr[i].betaerr[1],3)+'}_{-'+sigfig(cr[i].betaerr[0],3)+'}$ &'+' \\';;+crs
     
  endfor 
  
  close,lun
  free_lun,lun
  
  return
end 
