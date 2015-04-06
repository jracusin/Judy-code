pro ppst_slewdist,file
  
  read_ppst,file,ppst
  find_eph,file,ephfile
  eph=readeph(ephfile)
  
  nslew=n_elements(ppst)-1
  
  slewdist=fltarr(nslew)
  slewtime=strarr(nslew)
  for i=0,nslew-1 do begin 
     ra1=ppst[i].ra
     dec1=ppst[i].dec
     ra2=ppst[i+1].ra
     dec2=ppst[i+1].dec
     if ra1 ne ra2 and dec1 ne dec2 then begin 
        slewbegin=ppst[i].endtime
        slewend=ppst[i+1].begtime
        slewtime[i]=ppst[i].enddate
        metime=min(abs(eph.met-slewbegin),m)
        era=eph[m].ra
        edec=eph[m].dec
        year=strmid(ppst[i].enddate,0,4)
        doy=strmid(ppst[i+1].begdate,5,3)
        ydn2md,year,doy,month,day
        partsec=slewbegin-date2met(year+'-'+doy+'-00:00:00')
        hour=partsec/3600.

        jdcnv,year,month,day,hour,jd
        
        ;;plotting sun/moon data for date
        sunpos,jd,sra,sdec
        moonpos,jd,mra,mdec

        predict_slew,ra1,dec1,ra2,dec2,era,edec,sra,sdec,mra,mdec,pra,pdec,tslew,dist=dist,theta_roll=1,/noplot
        slewdist[i]=dist
     endif 
  endfor 

  w=where(slewdist gt 90.,nw)
  if nw eq 0 then print,'NO SLEWS >90 DEGREE' else begin
     print,'SLEWS >90 DEGREES'
     print,'slew time               slew length'
     colprint,slewtime[w]+'       '+ntostr(slewdist[w])+' deg'
     
  endelse 
  
  return
end 
