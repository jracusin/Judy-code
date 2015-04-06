pro make_data_table
  
  dir='~/Desktop/GRB080319B/data_tables/'
  cd,dir
  
  openw,lun,'GRB080319B_lightcurves.dat',/get_lun
  printf,lun,'-----------------------------------------------------------------------------------------------------'
  printf,lun,''
  printf,lun,'OPTICAL/UV/NIR DATA'
  printf,lun,'      Time        Exposure(s)  Mag        Magerr        Filter   Telescope'
  
  ;;;;uvot
  readcol,'grb080319b_UVOTphot.txt',time,tbin,rate,raterr,mag,magerr1,magerr2,format='(f,f,f,f,f,f,f)',/silent
  filters=['white','v    ','b    ','u    ','uvw1 ','uvm2 ','uvw2 ']
  
  n=n_elements(time)
  w=where(time-time[1:*] gt 0)
  s='     '
  magerr=strarr(n)
  b=where((magerr2+magerr1) eq 0)
  magerr[b]=numdec(magerr1[b],2)+'       '
  b=where(magerr2+magerr1 ne 0)
  magerr[b]='+'+numdec(magerr1[b],2)+','+numdec(magerr2[b],2)
  for i=0,6 do begin 
     if i eq 0 then start=0 else start=w[i-1]+1
     if i le 5 then stop=w[i] else stop=n-1
     nn=stop-start+1
     ind=indgen(nn)+start
     for k=0,nn-1 do printf,lun,time[ind[k]],tbin[ind[k]]*2.,s+numdec(mag[ind[k]],2)+s+magerr[ind[k]]+'    '+filters[i]+s+'UVOT'
  endfor 
    
  
  ;;;ground based
  cd,'ground'
  
  filters=['B','V','sdssr','R','sdssi','I','J','H','Ks']
  files=filters+'phot.ascii'
  n=n_elements(files)
  for i=0,n-1 do begin
     readcol,files[i],time,exp,mag,err,ref,format='(f,f,f,f,a)',/silent
     case i of 
        0: refs=['Kait','Nickel']
        1: refs=['TORTORA','Pi-of-the-Sky','Kait','Nickel']
        2: refs=['Liverpool','Faulkes-N','Gemini-S','Gemini-N','HST']
        3: refs=['REM','Faulkes-North','Nickel']
        4: refs=['Gemini-S','Faulkes-N','Liverpool','Gemini-N','HST']
        5: refs=['REM','Kait','Nickel','Faulkes-N']
        6: refs=['REM','VLT']
        7: refs='REM'
        8: refs=['REM','VLT']
        
     endcase
     
     nn=n_elements(time)
     uref=rem_dup(ref)
     outrefs=strarr(nn)
     for j=0,n_elements(uref)-1 do begin
        w=where(ref eq ref[uref[j]])
        outrefs[w]=refs[j]
     endfor 
     w=where(mag gt 10.)
     sp=replicate(' ',nn)
     sp[w]=''
     
     our=where(outrefs ne 'Kait' and outrefs ne 'Nickel' and outrefs ne 'Gemini-S',nour)

     if nour gt 0 then $
        for k=0,nour-1 do printf,lun,time[our[k]],exp[our[k]],s+numdec(mag[our[k]],2)+sp[our[k]]+s+numdec(err[our[k]],2)+s+'      '+strsame(filters[i],5)+s+outrefs[our[k]]

  endfor 
  
  cd,'..'
  ;;radio
  readcol,'wsrt.dat',epoch,telescope,freq,deltat,deltatmid,integ,flux,gcn,format='(a,a,f,a,f,a,a,a)',skip=2,delim=','
  nr=n_elements(epoch)
  rlc=create_struct('time',0.,'tstart',0.,'tstop',0.,'telescope','','freq',0.,'flux',0.,'fluxerr',0.)
  rlc=replicate(rlc,nr)
  day=86400.
  rx=1e-3
  for i=0,nr-1 do begin
     sdel=float(str_sep(deltat[i],' - '))
     rlc[i].tstart=sdel[0]*day
     rlc[i].tstop=sdel[1]*day
     rlc[i].time=deltatmid[i]*day
     rlc[i].telescope=telescope[i]
     rlc[i].freq=freq[i]
     lpos=strpos(flux[i],'<')
     if lpos ne -1 then begin 
        rlc[i].flux=float(strmid(flux[i],lpos+1,6))*rx
        rlc[i].fluxerr=-1
     endif else begin 
        fsep=str_sep(flux[i],' +/- ')
        rlc[i].flux=fsep[0]*rx
        rlc[i].fluxerr=fsep[1]*rx
     endelse 
  endfor 
  w=where(rlc.fluxerr eq -1)
  fluxerr=numdec(rlc.fluxerr,3)
  fluxerr[w]='     '
  printf,lun,'-----------------------------------------------------------------------------------------------------'
  printf,lun,''
  printf,lun,'RADIO/mm DATA'
  printf,lun,'      Time        Exposure(s) FluxDens FluxDenserr Frequency  Telescope'  
  for k=0,n_elements(rlc)-1 do printf,lun,rlc[k].time,rlc[k].tstop-rlc[k].tstart,s+numdec(rlc[k].flux,2),s+fluxerr[k]+s,'  '+numdec(rlc[k].freq,2)+s,rlc[k].telescope
  
  ;;mm
  printf,lun,6.3e4,6e3,s+'4.1e-4','   1.2e-4','      97.98','     IRAM'
  printf,lun,'Flux density given in Jy'
  
  ;;;xrt
  lc=lcout2fits('correct_xrt_lc.txt')
  printf,lun,''
  printf,lun,'-----------------------------------------------------------------------------------------------------'
  printf,lun,''
  printf,lun,'X-RAY DATA'
  printf,lun,'        Time           Exposure(s)        Flux            Fluxerr      Telescope'  
  for k=0,n_elements(lc)-1 do printf,lun,lc[k].time,(lc[k].tstop-lc[k].tstart),s+ntostr(lc[k].src_rate)+s,ntostr(lc[k].src_rate_err)+s+'XRT'
  printf,lun,'0.3-10.0 keV fluxes in units of erg/cm2/s'
  
  ;;;KW
  printf,lun,'-----------------------------------------------------------------------------------------------------'
  printf,lun,''
  printf,lun,'GAMMA-RAY DATA'
;   readcol,'GRB080319B_KW_64ms_flux.dat',tkw,tbat,flux,err1,err2,err,format='(f,f,f,f,f,f)'
  readcol,'GRB080319B_KW_64ms.dat.txt',tkw,tbat,g1,g2,g3,sum,flux1,flux2,flux3,flux4,format='(f,f,f,f,f,f,f,f,f,f)'
   printf,lun,''
   printf,lun,'       Time         Rate1        Rate2        Rate3       Rate    Telescope'
;   for k=0,n_elements(tkw)-1 do printf,lun,tbat[k],flux[k]*1e-6,err[k]*1e-6,s+'KW'
   for k=0,n_elements(tkw)-1 do printf,lun,tbat[k],flux1[k],flux2[k],flux3[i],flux4[k],s+'KW'
   printf,lun,'KW 64ms count rate in bands 18-70 keV, 70-300 keV, 300-1160 keV, 18-1160 keV'
   
   ;;NEED BAT
   readcol,'306757_bat64ms_lc.txt',time,f1,f1err,f2,f2err,f3,f3err,f4,f4err,ftot,ftoterr
   printf,lun,''
   printf,lun,'       Time            Rate1         Rate1err        Rate2         Rate2err        Rate3         Rate3err        Rate4         Rate4err        Rate          Rateerr    Telescope'
   
   for k=0,n_elements(time)-1 do printf,lun,time[k],f1[k],f1err[k],f2[k],f2err[k],f3[k],f3err[k],f4[k],f4err[k],ftot[k],ftoterr[k],s+'BAT',format='(f,f,f,f,f,f,f,f,f,f,f,a)'
   printf,lun,'BAT 64ms count rate in bands 15-25 keV, 25-50 keV, 50-100 keV, 100-350 keV, 15-350 keV'
   
   printf,lun,''
   printf,lun,'All times in seconds since BAT trigger, all times are observation mid-times, and all errors are 1-sigma'
   
   close,lun
   free_lun,lun
  stop
  return
end 
