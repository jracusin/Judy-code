pro correct_bloom

  v0=3648.8909
  b0=4059.0975
  u0=1446.4995
  uvw1=918.41254
  uvm2=755.37776
  uvw2=743.24203
  white=1944.7224
  sdssr=3635.
  sdssi=3626.
  u=1880.
  b=4400.
  v=3880.
  r=3078.
  i=2550.
  j=1594.
  h=1024.
  k=667.
  
  flux0=[white,v0,b0,u0,uvw1,uvm2,uvw2,b,v,sdssr,r,sdssi,i,j,h,k]*1d
  flux0=flux0[7:*]

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
  extinct=extinct[7:*]

  filters=['B    ','V    ','SDSSr','R    ','SDSSi','I    ','J    ','H    ','Ks    ']
  s='     '
  files=['pairitel2.txt','kait2.txt','nickel2.txt','gemini_s2.txt']
  telescopes=['pairitel','kait','nickel','gemini_s']
  num=[3,1,2,4]

  filters=['B    ','V    ','SDSSr','R    ','SDSSi','I    ','J    ','H    ','K    ']
  openw,lun,'new_bloom_values.dat',/get_lun
  for i=0,3 do begin 
     readcol,files[i],time,filt,exp,mag,magerr,bflux,bfluxerr,format='(f,a,f,f,f,f,f)',delim='&'

     nn=n_elements(time)
     corrmag=fltarr(nn) & flux=fltarr(nn) & fluxerr=fltarr(nn)
;;;need to read filters from files and apply appropriate extinction
;;;correction and flux zero point

     ufilt=filt[uniq(filt)]
     nfilt=n_elements(ufilt)
     
     for j=0,nfilt-1 do begin
        w=where(filt eq ufilt[j])
        whf=where(strtrim(filters,2) eq strtrim(ufilt[j],2),nwhf)
        whf=whf[0]

        if nwhf gt 0 then begin 
           corrmag[w]=mag[w];-extinct[whf]
           flux[w]=flux0[whf]*10^(-corrmag[w]/2.5)
           fluxerr[w]=flux0[whf]*(10^(-corrmag[w]/2.5)-10^(-(corrmag[w]+magerr[w])/2.5))
        endif 
;        print,median((bflux[w]/1d6)/10^(-mag[w]/2.5)),flux0[whf]

     endfor 
     good=where(flux ne 0,ng)
     printf,lun,telescopes[i]
     printf,lun,'       time        exp(s)  filter  fluxdens(Jy)  fluxdens_err(Jy)'
;     for g=0,ng-1 do printf,lun,time[good[g]],exp[good[g]],filt[good[g]],flux[good[g]],fluxerr[good[g]]
     w=good
;     for k=0,ng-1 do printf,lun,strsame(time[w[k]],11)+s+strsame(exp[w[k]]*2.,7)+s+strsame(numdec(mag[w[k]],2),6)+s+strsame(magerr[w[k]],15)+s+strsame(flux[w[k]],12)+s+strsame(fluxerr[w[k]],25)+s+strsame(freq[i+7],12)+s+filters[i]+s+' ^'+ntostr(num[k])

;     printf,lun,filters[good]
     for g=0,ng-1 do printf,lun,strsame(time[good[g]],7)+s+strsame(exp[good[g]],7)+s+strsame(mag[good[g]],7)+s+strsame(magerr[good[g]],7)+s+strsame(' ^'+ntostr(num[i]),7)+s+strsame(filt[good[g]],7);,flux[good[g]],fluxerr[good[g]]

  endfor 
  close,lun
  
;  mag=mag-extinct[i+7]     
;  flux=flux0[i+7]*10^(-mag/2.5)
;  fluxes=[fluxes,flux]
;  fluxerr=flux0[i]*(10^(-mag/2.5)-10^(-(mag+err)/2.5))
;  fluxerr=ntostr(fluxerr)
;  for k=0,nn-1 do printf,lun,strsame(time[k],11)+s+strsame(exp[k]*2.,7)+s+strsame(numdec(mag[k],2),6)+s+strsame(err[k],15)+s+strsame(flux[k],12)+s+strsame(fluxerr[k],25)+s+strsame(freq[i+7],12)+s+filters[i]+s+outrefs[k]

  return
end 
