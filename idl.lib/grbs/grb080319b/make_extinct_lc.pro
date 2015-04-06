pro make_extinct_lc
  
  cd,'~/Desktop/GRB080319B/data_tables/'
  v0 = 0.185
  b0 = 0.243
  u0 = 0.306
  uvw1 = 0.416
  uvm2 = 0.530
  uvw2 = 0.560
  white = 0.309
  B = 0.238
  V = 0.180
  R = 0.146
  sdssr = 0.137
  sdssi = 0.106
  I = 0.106
  J = 0.048
  H = 0.031
  K = 0.017

  extinct=[white,v0,b0,u0,uvw1,uvm2,uvw2,b,v,sdssr,r,sdssi,i,j,h,k]
  filters=['white','v    ','b    ','u    ','uvw1 ','uvm2 ','uvw2 ','B','V','r','R','i','I','J','H','K']
  
  white=347.1
  uvw2=	203.0
  uvm2=223.1
  uvw1=	253.4
  u0=350.1
  b0=432.9
  v0=540.2
  B=440.0
  V=550.0
  R=640.0
  SDSSr=670.0
  I=790.0
  SDSSi=790.0
  J=1260.0
  H=1600.0
  K=2222.0

  effwav=[white,v0,b0,u0,uvw1,uvm2,uvw2,b,v,sdssr,r,sdssi,i,j,h,k]
  freq=3e17/effwav
;  freq=effwav
  
  v0=3648.8909
  b0=4059.0975
  u0=1446.4995
  uvw1=918.41254
  uvm2=755.37776
  uvw2=743.24203
  white=1944.7224
  sdssr=3635.
  sdssi=3626.
  u=1880
  b=4400
  v=3880
  r=3078
  i=2550
  j=1594
  h=1024
  k=667
  
  flux0=[white,v0,b0,u0,uvw1,uvm2,uvw2,b,v,sdssr,r,sdssi,i,j,h,k]
  
  openw,lun,'GRB080319B_extinction_corrected_lightcurves2.dat',/get_lun
  printf,lun,'Time           Exposure(s)  Mag      Magerr1,Magerr2         Flux(Jy)       Fluxerr1,Fluxerr2(Jy)        Frequency(Hz)    Filter   Telescope'
  
  ;;;;uvot
  readcol,'grb080319b_UVOTphot.txt',time,tbin,rate,raterr,mag,magerr1,magerr2,magul,fluxx,fluxxerr,format='(f,f,f,f,f,f,f,f,f)',/silent
  filters=['white','v    ','b    ','u    ','uvw1 ','uvm2 ','uvw2 ']
  
  n=n_elements(time)
  w=where(time-time[1:*] gt 0)
  s='     '
  magerr=strarr(n)
;  b=where((magerr2+magerr1) eq 0)
;  magerr[b]=numdec(magerr1[b],2)+'       '
;  b=where(magerr2+magerr1 ne 0)
  magerr='+'+numdec(magerr1,2)+','+numdec(magerr2,2)
  fluxes=0.
  for i=0,6 do begin 
     if i eq 0 then start=0 else start=w[i-1]+1
     if i le 5 then stop=w[i] else stop=n-1
     nn=stop-start+1
     ind=indgen(nn)+start
     mag[ind]=mag[ind]-extinct[i]     
     flux=flux0[i]*10^(-mag[ind]/2.5)
     fluxes=[fluxes,flux]
     fluxerr1=flux0[i]*(10^(-mag[ind]/2.5)-10^(-(mag[ind]+magerr1[ind])/2.5))
     fluxerr2=flux0[i]*(10^(-(mag[ind]+magerr2[ind])/2.5)-10^(-mag[ind]/2.5))
     wf0=where(fluxerr1-fluxerr2 ne 0,nwf0)
     fluxerr=strarr(nn)
     if nwf0 gt 0 then fluxerr[wf0]='+'+ntostr(fluxerr1[wf0])+',-'+ntostr(fluxerr2[wf0])
     wf0=where(fluxerr1-fluxerr2 eq 0,nwf0)
     if nwf0 gt 0 then fluxerr[wf0]=ntostr(fluxerr1[wf0])
     for k=0,nn-1 do printf,lun,strsame(time[ind[k]],11)+s+strsame(tbin[ind[k]]*2.,7)+s+strsame(numdec(mag[ind[k]],2),6)+s+strsame(magerr[ind[k]],15)+s+strsame(flux[k],12)+s+strsame(fluxerr[k],25)+s+strsame(freq[i],12)+s+filters[i]+s+'UVOT'
  endfor 
  fluxes=fluxes[1:*]

  ;;;ground based
  cd,'ground'
  
  filters=['B    ','V    ','SDSSr','R    ','SDSSi','I    ','J    ','H    ','K    ']
  files=strtrim(filters,2)+'phot.ascii'
  n=n_elements(files)
  for i=0,n-1 do begin
     readcol,files[i],time,exp,mag,err,ref,format='(f,f,f,f,a)',/silent
     case i of 
        0: refs=['Kait','Nickel']
        1: refs=['TORTORA','PiofSky','Kait','Nickel']
        2: refs=['Liverpool','Faulkes-N','Gemini-S','Gemini-N','HST']
        3: refs=['REM','Faulkes-N','Nickel']
        4: refs=['Gemini-S','Faulkes-N','Liverpool','Gemini-N','HST']
        5: refs=['REM','Nickel','Kait','Faulkes-N']
;        5: refs=['REM','Faulkes-N']
        6: refs=['REM','VLT','Pairitel']
        7: refs=['REM','Pairitel']
        8: refs=['REM','VLT']
     endcase
     
     nn=n_elements(time)
     uref=uniq(ref)
     outrefs=strarr(nn)
     for j=0,n_elements(uref)-1 do begin
        w=where(ref eq ref[uref[j]],nw)
        if nw gt 0 then outrefs[w]=refs[j]
     endfor 
     w=where(mag gt 10.)
     sp=replicate(' ',nn)
     sp[w]=''
     
;     our=where(outrefs ne 'Kait' and outrefs ne 'Nickel' and outrefs ne 'Gemini-S',nour)
     mag=mag-extinct[i+7]     
     flux=flux0[i+7]*10^(-mag/2.5)
     fluxes=[fluxes,flux]
     fluxerr=flux0[i+7]*(10^(-mag/2.5)-10^(-(mag+err)/2.5))
     fluxerr=ntostr(fluxerr)
     w=where(outrefs ne '')
     for k=0,nn-1 do printf,lun,strsame(time[w[k]],11)+s+strsame(exp[w[k]]*2.,7)+s+strsame(numdec(mag[w[k]],2),6)+s+strsame(err[w[k]],15)+s+strsame(flux[w[k]],12)+s+strsame(fluxerr[w[k]],25)+s+strsame(freq[i+7],12)+s+filters[i]+s+outrefs[w[k]]
     
;     for k=0,nn-1 do printf,lun,time[k],exp[k],s+numdec(mag[k],2)+sp[k]+s+numdec(err[k],2)+s+'       '+filters[i]+s+outrefs[k]

  endfor 
  
  close,lun
  
  readcol,'../GRB080319B_extinction_corrected_lightcurves2.dat',time,tbin,mag,magerr,flux,fluxerr,freqs,filter,tele,format='(f,f,f,a,f,a,f,a,a)',delim='  '
  
  cpos=strpos(fluxerr,',')
  wc=where(cpos ne -1,nwc)
  fluxerr1=fltarr(n_elements(time)) & fluxerr2=fluxerr1
  for i=0,nwc-1 do begin 
     sep=str_sep(fluxerr[wc[i]],',')
     fluxerr1[wc[i]]=sep[0]
     fluxerr2[wc[i]]=sep[1]
  endfor 
  wnoc=where(cpos eq -1)
  fluxerr1[wnoc]=fluxerr[wnoc]
  fluxerr2[wnoc]=fluxerr[wnoc]
  plot,time,flux,psym=3,/xlog,/ylog
  colors=[!red,!blue,!green,!orange,!yellow,!cyan,!magenta,!pink,!grey50,!salmon,!forestgreen,!royalblue,!p.color,!hotpink,!firebrick,!sienna]
  for i=0,n_elements(freq)-1 do begin
     w=where(abs(freqs-freq[i]) lt 1e10,nw)
     for j=0,nw-1 do begin 
        oplot,[time[w[j]]-tbin[w[j]]/2.,time[w[j]]+tbin[w[j]]/2.],[flux[w[j]],flux[w[j]]],color=colors[i]
        oplot,[time[w[j]],time[w[j]]],[flux[w[j]]-abs(fluxerr2[w[j]]),flux[w[j]]+fluxerr1[w[j]]],color=colors[i]
     endfor 
  endfor 
  
  stop
  return
end 
