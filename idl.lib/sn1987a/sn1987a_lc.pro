pro sn1987a_lc,ps=ps
  
  !x.margin=[15,0]
  if keyword_set(ps) then begplot,name='sn1987a_lc.eps',/land,/color,/encap,font='helvetica'
  
;  swlc=mrdfits('/bulk/shadow/racusin/sn1987a/combined_V2/lc_data.fits',1)
  rage=[1448,1645,1872,2258,2408,2715,3013]
  rflux=[0.07,0.15,0.19,0.27,0.32,0.33,0.41]*1d-13
  rfluxerr=[0.09,0.04,0.04,0.05,0.07,0.11,0.06]*1d-13
  nr=n_elements(rflux)
  
  cage=[4711,5038,5176,5407,5561,5791,5980,6157,6359,6533,6716,6914,7095,7271,7446,7626,7800,7993]
  cflux=[1.61,2.4,2.71,3.55,4.19,5.62,6.44,7.73,11.48,16.29,19.41,21.96,25.56,29.72,32.9,39.32,42.23,45.66]*1d-13
  corr=[1.05,1.04,1.04,1.06,1.1,1.08,1.12,1.18,1.1,1.02,1.04,1.04,1.05,1.06,1.07,1.04,1.03,1.03]
  cflux=cflux*corr
  cfluxerr=[0.45,0.08,0.52,0.17,0.17,0.23,0.27,0.08,0.12,0.25,0.20,0.12,0.08,0.14,0.07,0.11,0.15,0.10]
  cfluxerr=cfluxerr*cflux
  
;  cfluxerr=[0.66,0.22,0.54,0.43,0.46,0.45,0.52,0.62,0.698,0.65,0.97,1.165,1.38]
  
;  cflux2=[0.84,0.92,1.22,1.20,1.49,1.82,1.95,2.38,2.40,2.8,3.26]
;  cfluxerr2=[0.57,0.21,0.41,0.44,0.64,0.46,0.62,0.57,0.6,0.73,0.68]
  
;  cflux=[1.52,1.65,2.39,2.75,3.60,4.22,5.63,6.5,7.8,10.82,15.29,15.10,14.88]
;  cfluxerr=cflux*0.;sqrt(cflux)
  nc=n_elements(cflux)
  
  
;  ctrfact=[6.4816D-02,5.5728D-02,7.0833D-02,7.8771D-02,9.5722D-02,9.3041D-02] ;0.2-10.0
;  fluxfact=[2.1082D-12,1.8049D-12,2.1853D-12,2.2434D-12,2.6944D-12,2.1863D-12] ;0.2-10.0
  
;  ctrfact=[6.5956D-02,6.0116D-02,7.3690D-02,7.9711D-02,9.6882D-02,9.4284D-02] ;0.5-2.0
  
   ;fits with seg 2&3 separate SWIFT
;  sflux=[1.5688D-12,1.4755D-12,1.6325D-12,1.9058D-12,2.3493D-12,1.9816D-12]*1d13 ;0.5-2.0
;  norm=[1.972876D-03,3.663673D-03,2.389952D-03,2.885753D-03,3.414468D-03,2.887980D-03]
;  sfluxerr1=[2.728682D-04,1.16373d-3,3.135742D-04,3.324947D-04,4.490404D-04 ,4.489275D-04]/norm*sflux
;  sfluxerr2=[2.993254D-04,4.3627d-4,3.256472D-04,3.485428D-04,5.071296D-04 ,4.528656D-04]/norm*sflux
;  sage=[6494,6617,6619,6737,6811,6899]
  
  ;fits with seg 2&3 combined
;  sflux=[1.5093D-12,1.5305D-12,1.9665D-12,2.3672D-12,2.1460D-12]*1d13
;  norm=[2.119297D-03,2.397792D-03,2.934229D-03,3.509038D-03,3.013411D-03]
;  sfluxerr1=[2.660198D-04,3.911925D-04,2.979501D-04,4.423396D-04,5.360679D-04]/norm*sflux
;  sfluxerr2=[3.945744D-04,2.051651D-03,3.706245D-04,5.906553D-04,6.073311D-04]/norm*sflux
;  sage=[6494,6618,6737,6811,6899]
  
  ns=n_elements(sflux)
;  flfact=fluxfact/ctrfact
  
  
  flux=[rflux,cflux];,sflux]
  fluxerr=[rfluxerr,cfluxerr];,sfluxerr1]
  
;  cdate=[[1999,10,06],[2000,1,17],[2000,12,7],[2001,04,25],[2001,12,12],[2002,5,5],[2002,12,31],[2003,7,8],[2004,1,2],[2004,7,22],[2004,8,30],[2005,1,11],[2005,7,14]]
;  sdate=intarr(3,6)
;  for i=0,5 do begin
;     d=met2date_judy(swlc[i].met)
;     year=d[0]
;     doy=d[1]
;     ydn2md,year,doy,month,day
;     sdate[*,i]=[year,month,day]
;  endfor 
  
;  cnum=n_elements(cdate[0,*])

;  date=intarr(3,cnum+snum)
;  date[*,0:cnum-1]=cdate
;  date[*,cnum:*]=sdate
  
;  sn1987a_age,sage,date=sdate
;  stop
  age=[rage,cage];,sage]
  
  simpctable
  plot,age,flux,psym=3,xtitle='Days since SN',ytitle='0.5-2 keV Flux (ergs cm!U-2!N s!U-1!N)',xrange=[min(age),max(age)+10],yrange=[1d-15,max(flux+fluxerr)],charsize=1.5,/ylog,ytickf='loglabels';,title='SNR 1987A Lightcurve'
  
  plotsym,0,/fill
  oploterror,age[0:nr-1],rflux,rfluxerr,psym=6,/nohat;,color=!green,errcolor=!green
  oploterror,age[nr:nr+nc-1],cflux,cfluxerr,psym=8,/nohat;,color=!blue,errcolor=!blue
;  oploterror,age[nc:*],sflux,sfluxerr,psym=5,color=!red,errcolor=!red
;  oplot,age[nr+nc:*],sflux,psym=5,color=!red
  for i=0,ns-1 do oplot,[age[nr+nc+i],age[nr+nc+i]],[sflux[i]-sfluxerr1[i],sflux[i]+sfluxerr2[i]],color=!red
;  legend,['ROSAT (0.5-2 keV)','Chandra (0.5-2 keV)','Swift XRT (0.5-2 keV)'],/top,/left,psym=[6,8,5],color=[!green,!blue,!red],box=0,charsize=2
  legend,['ROSAT','Chandra'],/top,/left,psym=[6,8],box=0,charsize=1.5
;  f1=linfit(age[0:nr-1],rflux,measure_error=rfluxerr,yfit=yfit1)
;  oplot,age[0:nr-1],yfit1,color=!green
;  f2=linfit(age[nr:nr+nc-5],cflux[0:7],measure_error=cfluxerr[0:7],yfit=yfit2)
;  oplot,age[nr:nr+nc-1],age[nr:nr+nc-1]*f2[1]+f2[0],color=!blue

  if keyword_set(ps) then endplot
  stop
  return
end 
