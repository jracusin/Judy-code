function today

  jdnow=systime(1,/julian,/utc)

  daycnv,jdnow,yr,mn,day,hr
;  dy = ymd2dn(yr,mn,day)

  mn=ntostr(mn)
  day=ntostr(day)
  if mn lt 10 then mn='0'+mn
  if day lt 10 then day='0'+day
;  print,ntostr(yr,4)+'-'+mn+'-'+day+'-00:00:00'
  t=date2met(ntostr(yr,4)+'-'+mn+'-'+day+'-00:00:00')

  return,t
end 
