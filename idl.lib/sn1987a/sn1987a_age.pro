PRO sn1987a_age,ages,obsnum,date=date


;  obs1967=5038
  IF n_elements(date) EQ 0 THEN BEGIN 
      date=[[1999,10,06],$
            [2000,1,17],$
            [2000,12,7],$
            [2001,4,25],$
            [2001,12,12],$
            [2002,5,15],$
            [2002,12,31],$
            [2003,7,8],$
            [2004,1,2],$
            [2004,7,22],$
            [2004,8,31],$
            [2005,1,11],$
            [2005,7,14],$
            [2006,1,28],$
            [2006,7,27],$
            [2007,1,19],$
            [2007,7,13],$
            [2008,1,10],$
            [2008,7,04],$
            [2009,1,05],$
            [2009,7,06],$
            [2009,9,8],$
            [2010,3,17],$
            [2010,3,17],$
            [2010,3,28],$
            [2010,9,29]]
      
      all=1
  ENDIF ELSE all=0

  s=n_elements(date)/3.
  ages=lonarr(s)
;  IF all THEN ages[0]=obs1967
;  juldate,[2000,12,7],jd1967
  juldate,[1987,2,23],jd0
  
  FOR i=0,s-1 DO BEGIN 
      juldate,date[*,i],jd
;      ages[i]=jd-jd1967+obs1967
      ages[i]=jd-jd0

  ENDFOR 

  obsnum=[indgen(17)+1,19,20,21]

  return
END 
