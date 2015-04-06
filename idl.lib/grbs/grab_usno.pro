pro grab_usno,ra,dec
  
  com='wget "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&RA='+ntostr(ra)+'&DEC='+ntostr(dec)+'&SR=0.25&VERB=1&slf=ddd.ddd/dd.ddd" -O usno.txt'
  
  spawn,com
  
  return
end 
