pro sunpos_utc,date,sunra,sundec,jd=jd
  
  if n_params() eq 0 then begin 
     print,'syntax -  sunpos_utc,date,sunra,sundec'
     print,"              e.g. date = '2012-10-01-12:00:00'"
     return
  end 
  chunks=str_sep(date,'-')
;  year=date[0]
  if n_elements(chunks) eq 4 then begin
     year=chunks[0]
     month=chunks[1]
     day=chunks[2]
     i=3
  endif else begin
     year=chunks[0]
     ydn2md,year,chunks[1],month,day
     i=2
  endelse 
  chunks=str_sep(chunks[i],':')
  partday=chunks[0]/24.+chunks[1]/60./24.+chunks[2]/60./60./24.
  jdcnv,year,month,day,partday*24.,jd

  sunpos,jd,sunra,sundec

  return
end 
