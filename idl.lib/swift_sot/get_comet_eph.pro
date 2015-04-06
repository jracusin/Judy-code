pro read_comet_email,ra,dec
  
;  print,'Waiting for response from horizons@ssd.jpl.nasa.gov'
;  wait,10
  i=0
  cont=1
;  while cont eq 1 and i le 10 do begin 
;     wait,3
;     com='fetchmail > fetchmail.txt'
;     print,com
;     spawn,com
     
;  print,'save email to this directory with filename comet73p.txt'
;  print,'press any key to continue'
;  k=get_kbrd(10)

     openr,lun,'fetchmail.txt',/get_lun
     stopthis=0
;  i=0
     line=readline(lun,delim=':')
;     if line[0] eq 'fetchmail' then begin
;        cont=1 
;        close,lun,/file
;     endif else cont=0
;     i=i+1
;  endwhile
  print,'reformating for csv file'
  while not eof(lun) and not stopthis do begin
     line=readline(lun,delim=',')
     if line[0] eq '$$SOE' then stopthis=1
;     print,line[0]
;     i=i+1
  endwhile
  
  line=readline(lun,delim=',')
  cra=line[3]
  cdec=line[4]
  
;  crah=strmid(cra,1,2)
;  cram=strmid(cra,4,2)
;  cras=strmid(cra,7,5)
  
;  cdecd=strmid(cdec,1,2)
;  cdecm=strmid(cdec,4,2)
;  cdecs=strmid(cdec,7,4)
  
;  hms2radec,crah,cram,cras,cdecd,cdecm,cdecs,ra,dec
  ra=cra
  dec=cdec
  
;  print,ra,dec
  
  return
end 
pro get_comet_roll,year,doy,cra,cdec,roll
  
  print,'calculating roll'
  print,'roll_range.py '+ntostr(year)+' '+ntostr(doy)+' '+ntostr(cra)+' '+ntostr(cdec)+' > roll.tmp'
  spawn,'/bulk/pkg/xray/python/roll_range.py '+ntostr(year)+' '+ntostr(doy)+' '+ntostr(cra)+' '+ntostr(cdec)+' > roll.tmp'
  openr,lun,'roll.tmp',/get_lun
  while not eof(lun) do line=readline(lun,delim='$')
  close,lun
  free_lun,lun
  tmp=str_sep(line,',')
  tmp2=str_sep(tmp[0],' ')
  roll=tmp2[n_elements(tmp2)-1]
  
  print,'roll = ',roll
  
  return
end 

pro write_comet_csv,ra,dec,roll,targid,targname,umode,xmode,duration,time,i,fname
  
  file=fname+'.'+ntostr(i)+'.csv'
  print,'Writing out csv file: '+file
  openw,lun,file,/get_lun

  printf,lun,targname+',Non-GRB,'+ntostr(targid)+',1,95,'+ntostr(ra)+','+ntostr(dec)+',0,'+ntostr(roll)+',0x0000,'+xmode+','+umode+','+ntostr(duration)+','+time+',90,300,1800,0'
   
  
;  Comet73P/3-C,Non-GRB,30665,1,95,337.45517, 14.36240, 50.000,0x0000,0x0000,0x20ed,1.0,137-00:05,90,60,1800
  close,lun,/file
  return
end 

pro write_horizon_email,lon,lat,jd
  
;  toadd='horizons@ssd.jpl.nasa.gov'
;  email='horizons_email.txt'
;  openw,lun,email,/get_lun  
;  printf,lun,"!$$SOF"

;  printf,lun,"COMMAND= 'DES=8P;NOFRAG;CAP'" ;;Tuttle
;  printf,lun,"COMMAND= 'DES=73P-C;NOFRAG;CAP'"
;   printf,lun,"COMMAND= 'DES=C/2006 M4;NOFRAG'"
;  printf,lun,"COMMAND= '136199;'"
;  printf,lun,"CENTER= 'coord@399'"
;  printf,lun,"COORD_TYPE= 'GEODETIC'"
;  printf,lun,"SITE_COORD= '"+ntostr(lon)+','+ntostr(lat)+",600'"
;  printf,lun,"MAKE_EPHEM= 'YES'"
;  printf,lun,"TABLE_TYPE= 'OBSERVER'"
;  printf,lun,"TLIST="+strtrim(string(jd,format='(f16.8)'),2)+"'"
;  printf,lun,"CAL_FORMAT= 'CAL'"
;  printf,lun,"TIME_DIGITS= 'MINUTES'"
;  printf,lun,"ANG_FORMAT= 'DEG'"
;  printf,lun,"OUT_UNITS= 'KM-S'"
;  printf,lun,"RANGE_UNITS= 'AU'"
;  printf,lun,"APPARENT= 'AIRLESS'"
;  printf,lun,"SOLAR_ELONG= '0,180'"
;  printf,lun,"SUPPRESS_RANGE_RATE= 'NO'"
;  printf,lun,"SKIP_DAYLT= 'NO'"
;  printf,lun,"EXTRA_PREC= 'NO'"
;  printf,lun,"R_T_S_ONLY= 'NO'"
;  printf,lun,"REF_SYSTEM= 'J2000'"
;  printf,lun,"CSV_FORMAT= 'YES'"
;  printf,lun,"OBJ_DATA= 'YES'"
;  printf,lun,"QUANTITIES= '1,9,13'"
  
;  close,lun,/file
  
;  print
;  print,'Emailing horizons@ssd.jpg.nasa.gov'
;  com='emailfile.py JOB swiftsot@gmail.com '+toadd+' '+email
;  print,com
;  spawn,com
  
  
  ;;CHANGE COMET DESIGNATION HERE
  ;;REPLACE ; WITH %3B
;  cometcom="COMMAND= 'DES=8P;NOFRAG;CAP'" ;;Tuttle
;  cometcom="COMMAND='DES=17P;NOFRAG;CAP'" ;;Holmes
;  cometcom="COMMAND= 'DES=73P-C;NOFRAG;CAP'"
;  cometcom="COMMAND= 'DES=C/2006 M4;NOFRAG'"
;  cometcom="COMMAND= '136199;'" ;;Eris  
;  cometcom="COMMAND='DES=8P%3BNOFRAG%3BCAP'" ;;Tuttle
;  cometcom="COMMAND='DES=C/2007 N3'" ;;Lulin
  cometcom="COMMAND='DES=22P%3BNOFRAG%3BCAP'" ;;22P/Kopff
  
  http='http://ssd.jpl.nasa.gov/horizons_batch.cgi?batch=1&'+cometcom+"&CENTER='coord@399'&COORD_TYPE='GEODETIC'&SITE_COORD='"+ntostr(lon)+','+ntostr(lat)+",600'&MAKE_EPHEM='YES'&TABLE_TYPE='OBSERVER'&TLIST="+strtrim(string(jd,format='(f16.8)'),2)+"'&CAL_FORMAT='CAL'&TIME_DIGITS='MINUTES'&ANG_FORMAT='DEG'&OUT_UNITS='KM-S'&RANGE_UNITS='AU'&APPARENT='AIRLESS'&SOLAR_ELONG='0,180'&SUPPRESS_RANGE_RATE='NO'&SKIP_DAYLT='NO'&EXTRA_PREC='NO'&R_T_S_ONLY='NO'&REF_SYSTEM='J2000'&CSV_FORMAT='YES'&OBJ_DATA='YES'&QUANTITIES='1,9,13'"
  
  com='wget -O fetchmail.txt "'+http+'"'
  print,com
  spawn,com
  
  return
end 

pro get_comet_eph,stkfile,time,targid,targname=targname,umode=umode,xmode=xmode,duration=duration,fname=fname,cometname=cometname
  
  if n_params() lt 3 then begin
     print,"syntax - get_comet_eph,stkfile,time,targid,umode=umode,xmode=xmode,duration=duration,fname=fname"
     print,'            [parameters can be arrays'
     print,"            time should be in format ('2006-100-00:00:00')]"
     return
  endif 

  ;;CHANGE COMET NAME HERE
;  cometname='Comet 8P/Tuttle'
;  cometname='Comet Lulin'
  cometname='Comet 22P/Kopff'
  
;  if n_elements(cometname) eq 0 then $
;  cometname='Comet 2006 M4'  
;  cometname='Asteroid 136199 Eris (2003 UB313)'
  
  n=n_elements(time)
  if n_elements(targname) eq 0 then targname=replicate(cometname,n)
  if n_elements(fname) eq 0 then fname='comet'
  if n_elements(umode) eq 0 then umode=replicate('0x30ed',n)
  if n_elements(xmode) eq 0 then xmode=replicate('0x0007',n)
  if n_elements(duration) eq 0 then duration=replicate('0.6',n)
  
  
  if n_elements(umode) eq 1 and n gt 1 then umode=replicate(umode,n)
  if n_elements(xmode) eq 1 and n gt 1 then xmode=replicate(xmode,n)
  if n_elements(duration) eq 1 and n gt 1 then duration=replicate(duration,n)
  
  readcol,stkfile,skip=1,date,tme,x,y,z,lat,lon,ra,dec,format='(a,a,f,f,f,f,f,f,f)'
  
  cfile=''
  for i=0,n_elements(time)-1 do begin 
     year=strmid(time[i],0,4)
     doy=strmid(time[i],5,3)
     ydn2md,year,doy,mo,d
     h=strmid(time[i],9,2)*1d
     m=strmid(time[i],12,2)*1d
     s=strmid(time[i],15,2)*1d
     juldate,[year,mo,d,h,m,s],jd
     jd=jd+2400000d
  
     eyear=strmid(date,4,4)
     edoy=strmid(date,0,3)
     eh=strmid(tme,0,2)
     em=strmid(tme,3,2)
     es=strmid(tme,6,2)
  
     if i eq 0 then ffile=fname+'_'+ntostr(doy)+'.csv'
     
     w=where(year eq eyear and doy eq edoy and h eq eh and m eq em and s eq es)
     
     print,'Getting comet position for '+time[i]
     
     write_horizon_email,lon[w],lat[w],jd

     read_comet_email,cra,cdec
  
     get_comet_roll,year,doy,cra,cdec,roll
     write_comet_csv,cra,cdec,roll,targid[i],targname[i],umode[i],xmode[i],duration[i],time[i],i,fname
     cfile=cfile+' '+fname+'.'+ntostr(i)+'.csv'
     print,'*******DONE******'
     close,/all,/file
  endfor 
  
  print,'Combining csv files'
  openw,lun,'header.csv',/get_lun
  printf,lun,'target_name ,  type , target_ID ,  segment ,  FoM ,  RA ,  dec , sun_hour, roll ,  BATmode ,  XRTmode ,  UVOTmode ,  duration ,  comment ,  priority ,  SSmin ,  SSmax,  co_point'
  close,lun,/file
  
  
  com='cat '+'header.csv '+cfile+'> '+ffile;fname+'_'+ntostr(doy)+'.csv'
  print,com
  spawn,com
  print,'Input '+ffile+' into TAKO'
  
  print
  print,'You still need to check for bright sources!'
  print,'Run burrito on the CSV file'
  
  return
end 
