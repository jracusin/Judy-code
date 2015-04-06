function host_extin,lam,Av,mw=mw,smc=smc,lmc=lmc

  if keyword_set(mw) then begin
     lambda=[0.047,0.08,0.22,9.7,18.0,25.];*1d-4
     a=[165.,14.,0.045,0.002,0.002,0.012]
     b=[90.,4.,-1.95,-1.95,-1.80,0.]
     n=[2.0,6.5,2.,2.,2.,2.]
     k=[2.89,0.31,0.16,0.31,0.28,0.76]
  endif 
  if keyword_set(lmc) then begin 
     lambda=[0.046,0.08,0.22,9.7,18.0,25.];*1d-4
     a=[175.,19.,0.023,0.005,0.006,0.02]
     b=[90.,5.5,-1.95,-1.95,-1.8,0]
     n=[2.0,4.5,2.,2.,2.,2.]
     k=[3.,0.56,0.08,0.77,0.86,1.26]
  endif 
  if keyword_set(smc) then begin 
     lambda=[0.042,0.08,0.22,9.7,18.0,25.];*1d-4
     a=[185.,27.,0.005,0.010,0.012,0.030]
     b=[90.,5.5,-1.95,-1.95,-1.8,0]
     n=[2.0,4.0,2.,2.,2.,2.]
     k=[2.89,0.91,0.02,1.55,1.72,1.89]
  endif 

  lamv=0.55 ;; microns
  xi_v=total(a/((lamv/lambda)^n+(lambda/lamv)^n+b))
  xi_lam=total(a/((lam/lambda)^n+(lambda/lam)^n+b))

  x=xi_v/xi_lam

  alam=av/x

;  nl=n_elements(lam)
;  xi=dblarr(nl)
;  for i=0,nl-1 do begin 
;     xit=a/((lam[i]/lambda)^n+(lambda/lam[i])^n+b)
;     xi[i]=total(xit)
;  endfor 

  return,alam
end 
pro filter_curves

  transfile='~/CALDB/data/swift/uvota/bcf/ftrans/swuftrans20041120v102.fits'
  colors=[!purple,!cyan,!salmon,!violet,!blue,!green,!p.color]
  lam_eff=[3501,4329,5402,2634,2030,2231,3471]
  lam_c=[3465,4392,5468,2600,1928,2246,3471]
  filter=['uu','bb','vv','w1','w2','m2','wh']
  c=1e-8
;  plot,[1600,8000]*c,[0,1],/nodata
  for i=0,6 do begin
     x=mrdfits(transfile,i+1)
     oplot,x.wavelength*c,x.transmis*0.05,color=colors[i]
;     oplot,[lam_eff[i],lam_eff[i]]*c,[0,1],line=2,color=colors[i]
;     oplot,[lam_c[i],lam_c[i]]*c,[0,1],line=1,color=colors[i]
     if i lt 6 then y=0.01 else y=0.04
     xyouts,lam_eff[i]*c,y,filter[i],color=colors[i]
  endfor 
return
end 

pro match_filters,filt2,ind,uvot=uvot,filt1=filt1

  filter=['uu','bb','vv','w1','m2','w2','wh']
  ofilter=['U','B','V','R','I','Z','J','H','K','Rc','Ic']
  filt1=[filter,ofilter]
  
  if keyword_set(uvot) then filt1=['u','b','v','uvw1','uvm2','uvw2','white']

  ind=intarr(n_elements(filt2))

  for i=0,n_elements(filt1)-1 do begin
     w=where(strtrim(filt2,2) eq strtrim(filt1[i],2),nw)
     if nw gt 0 then ind[w]=i
  endfor 
;  match,strtrim(filt1,2),strtrim(filt2,2),m1,m2 
;  ind=m1

  return
end 

pro calc_dust_corr,ra,dec,corr,filter=filter,gamma=gamma,noplot=noplot,lam_eff=lam_eff,ebv=ebv,dir=dir

  if n_params() eq 0 then begin
     print,'syntax - calc_dust_corr,ra,dec,corr'
     return
  end 
  
  if n_elements(dir) eq 0 then dir=''
;  if n_elements(ebv) eq 0 then begin 
     ofile=dir+'ned_extinct_'+ntostr(fix(ra))+'_'+ntostr(fix(dec))+'.txt'
     
     if not exist(ofile) then begin 
        url1 = '"http://nedwww.ipac.caltech.edu/cgi-bin/nph-calc?in_csys=Equatorial&in_equinox=J2000.0'
        url2 = '&obs_epoch=2000.0&lon='+ntostr(ra)+'d&lat='+ntostr(dec)+'d&pa=0.0&out_csys=Equatorial&out_equinox=J2000.0"'
        url=url1+url2
     
        com='wget -O '+ofile+' '+url
        spawn,com
     endif 

  ;;; LOOK UP E(B-V) FROM NED
     readcol,ofile,filter,lam,alam,delim=" '()",format='(a,f,f)',/silent
     readcol,ofile,line,format='(a)',delim='$',/silent
     line=line[19]
     spos=strpos(line,'E(B-V)')
     if spos ne -1 then ebv=strmid(line,spos+9,5)
;  endif 
  print,'E(B-V)=',ebv

;  a=indgen(n_elements(filter))
;  a=a[2:*]
;  filter=filter[a] & lam=lam[a] & alam=alam[a]*1.

  ;; DEFINE FILTER SET
  lam=lam*1e-4
;  lam_eff=[3501,4329,5402,2634,2231,2030,3471]*1d-8 ;; cm
  lam_eff=[3465,4392,5468,2600,2246,1928,3471]*1d-8 ;; cm
  filter=['uu','bb','vv','w1','m2','w2','wh']
  ofilter=['u','b','v','r','i','z','j','h','k','Rc','Ic']
  olam_eff=[0.36,0.44,0.545,0.64,0.79,0.91,1.26,1.6,2.22,0.64,0.79]*1d-4 ;; cm
  lam_eff=[lam_eff,olam_eff]
  filter=[filter,ofilter]

  nfilt=n_elements(filter)
  extin=dblarr(nfilt)

  ;; CALCULATE EXTINCTION (A_v)
  ;; from UVOT catalog paper
;  a=[0.9226,0.9994,1.0015,0.4346,0.0773,-0.0581]
;  b=[2.1019,1.0171,0.0126,5.3286,9.1784,8.4402]
  rv=3.1
;  extin=ebv*(a*rv+b)
;  extin=[extin,0.]

  ;; FUNCTION FROM CARDELLI ET AL (1989)
  x=1./(lam_eff*1d4) ;; microns
  a=dblarr(nfilt)
  b=a
  ;; IR
  w=where(x ge 0.3 and x lt 1.1,nw)
  if nw gt 0 then begin 
     a[w]=0.574*x[w]^1.61
     b[w]=-0.527*x[w]^1.61
  endif 
  ;; optical/NIR
  w=where(x ge 1.1 and x lt 3.3,nw)
  if nw gt 0 then begin 
     y=x[w]-1.82
     a[w]=[1.+0.17699*y-0.50447*y^2-0.02427*y^3+0.72085*y^4+0.019979*y^5-0.77530*y^6+0.32999*y^7]
     b[w]=[1.41338*y+2.28305*y^2+1.077233*y^3-5.38434*y^4-0.62251*y^5+5.30260*y^6-2.09002*y^7]
;     extin[w]=ebv*(a*rv+b)
  endif 
  ;; UV
  w=where(x ge 5.9 and x lt 8,nw)
  if nw gt 0 then begin
     fa=-0.04473*(x-5.9)^2-0.009779*(x-5.9)^3
     fb=0.213*(x-5.9)^2+0.1207*(x-5.9)^3
     a[w]=1.752-0.316*x[w]-0.104/((x[w]-4.67)^2+0.341)+fa[w]
     b[w]=-3.090+1.825*x[w]+1.206/((x[w]-4.62)^2.+0.263)+fb[w]
  endif
  w=where(x ge 3.3 and x lt 5.9,nw)
  if nw gt 0 then begin 
;     fa=intarr(n_elements(x))
;     fb=fa
     a[w]=1.752-0.316*x[w]-0.104/((x[w]-4.67)^2+0.341);+fa[w]
     b[w]=-3.090+1.825*x[w]+1.206/((x[w]-4.62)^2.+0.263);+fb[w]
  endif 
  extin=ebv*(a*rv+b)     

  if not keyword_set(noplot) then begin 
     lam=lam[2:*]
     alam=alam[2:*]
     plot,lam,alam,psym=1,/xlog,yrange=[0,0.2]
;     colprint,filter+' '+ntostr(lam)+' '+ntostr(alam)

;  p=fltarr(n_elements(alam),2)
;  p[*,0]=alam
;  p[*,1]=lam
;  a_uvot=interpol(alam,lam,lam_eff)  
;  a_uvot=interpolate(p,lam_eff,/missing)
     gamma=2.
     p=[1.,gamma]
     parinfo=parinfo_struct(2)
     parinfo.value=p
;     parinfo[1].fixed=1
;     newp=mpfitfun('pow',lam,alam,sqrt(alam),p,yfit=yfit,parinfo=parinfo)
;     print,newp
;     oplot,lam,yfit,line=2
;     a_uvot=pow(lam_eff,newp)
;     oplot,lam_eff,a_uvot,color=!red,psym=1
;  au2=spline(lam,alam,lam_eff)
;  oplot,lam_eff,au2,color=!blue,psym=1

     oplot,lam_eff,extin,psym=1,color=!green
;     oplot,lam_eff,,psym=1,color=!blue

     xyouts,lam_eff*1.1,extin,filter
     filter_curves
;     colprint,filter,lam_eff,extin,extin4

  endif 
  corr=extin  ;; A_lambda
;colprint,filter,lam_eff,x,a,b,extin
  return
end 
