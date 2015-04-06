function poisson,x,b

  y=x^b*exp(-x)/factorial(b)
return,y
end 

function prob_simp,b,s
  
  t=1e3
  ind=indgen(t)
  p=0d
;  for n=s,t-1 do begin
  n=s
  p0=1d
  p1=0.1d
  while abs(p0-p1) gt p*1d-15 and n lt t do begin 
     p0=p1
;     nfact=factorial(n*1d)
;     nfact=(factor(n*1d))
;     p1=exp(-b*1d)*b^n/exp(nfact)
     p1=poiprob(b,n)
     p=p+p1
;     print,n,p0,p1,p,p0-p1
;     print,n,p1,p
     n=n+1d
  endwhile

  return,p
end 

function prob_comp,b,s

  unc=0.10
  f=[1.,b,unc*b]
  t=1e3
  half=t/(t*0.2)/2.*b
  x=dindgen(t)/(t*0.2)*b+(b-half)
  w=where(x gt 0)
  x=x[w]
  t=n_elements(x)
  g=gaussian(x,f)
;  plot,x,g

  int1=0d & int2=0d
  for bb=0,t-1 do begin
     db=x[1]-x[0]
     if s gt 40 then func=gaussian(x[bb],f) else func=poisson(x[bb],b)
     int1=int1+db*func*prob_simp(x[bb],s)
     int2=int2+db*func
;     colprint,x[bb],int1[0],int2[0],int1[0]/int2[0]
  endfor 

  p=int1/int2

return,p
end 

pro orphan_gev_afterglows

  run='5000'
;  run='1000'
;  cd,'~/Fermi/Orphan_GeV_afterglows/'
;  file='Swift_GRBs_in_LAT.csv'

;  readcol,file,sgrb,trigtime,t90,ra,dec,err,format='(a,a,a,a,a,a)',delim=',',skip=1
;  ns=n_elements(sgrb)

  cd,'~/Glast/GANGSTER_NEW/Projects/Orphans/Results/'
  res=file_search('*/Bkg_Estimates/0.00_'+run+'.00/Results*txt')
  nres=n_elements(res)
  latgrbs=['080916C','090323','090328A','090510','090902B','090926A','091003']
  files=''
  kfiles=''

  plotsym,0,1,/fill
;  print,'GRB               BKG              SRC            SIGMA
;  PROB         POI_PROB'
  print,'GRB               BKG              SRC            PPROB_S           PSIG_S         PPROB_C           PSIG_C         '
  pprob_s=dblarr(nres)
  pprob_c=dblarr(nres)
  psig_s=dblarr(nres)
  psig_c=dblarr(nres)
  lat=intarr(nres)
  src=dblarr(nres)
  bkg=dblarr(nres)
  grbs=strarr(nres)
  for i=0,nres-1 do begin
     add=''
     readcol,res[i],bin,emin,emax,roi,bkg_bin,sig_bin,int_bkg,int_sig,exposure,skip=1,format='(i,l,l,d,d,d,d,d,d)',/silent     
     w0=where(int_sig eq 0,nw0)
     if nw0 ne 30 then begin 

        ee=(emax-emin)/2.+emin
        eerr=emax-ee
;        plot,[1e2,1e6],[1e-4,1e3],/nodata,title=res[i],xtitle='Energy',ytitle='Events/bin',/xsty,/ysty,/xlog,/ylog
;        oploterror,ee,bkg_bin,eerr,sqrt(bkg_bin),psym=3,/nohat,errcolor=!red
;        oploterror,ee,sig_bin,eerr,sqrt(sig_bin),psym=8,/nohat
        src[i]=max(int_sig)*1d
        bkg[i]=max(int_bkg)*1d

        tmp=str_sep(res[i],'_')
        grb=tmp[3]
        grbs[i]=grb
;        sigma=(src-bkg[i]*1d)/sqrt(src+bkg[i])*1d
;        prob=1.-(1.-gauss_pdf(sigma))*2d
;        pprob=poiprob(bkg[i],src)
;        if src[i]-bkg[i] gt 0 then begin 
           pprob_s[i]=prob_simp(bkg[i],src[i])
           if pprob_s[i] gt 1. then pprob_s[i]=1.
           psig_s[i]=gauss_cvf(pprob_s[i])
           pprob_c[i]=prob_comp(bkg[i],src[i])
           if pprob_c[i] gt 1. then pprob_c[i]=1.
           psig_c[i]=gauss_cvf(pprob_c[i])
;        endif else begin
;           pprob_s[i]=prob_simp(bkg[i],src[i])
;           if pprob_s[i] gt 1. then pprob_s[i]=1.
;           psig_s[i]=-gauss_cvf(pprob_s[i])
;           pprob_c[i]=prob_comp(bkg[i],src[i])
;           psig_c[i]=-gauss_cvf(pprob_c[i])
;        endelse 

        wlat=where(grb eq latgrbs,nlat)
        if nlat gt 0 then lat[i]=1
        if psig_s[i] gt 3. and pprob_s[i] gt 0 and src[i]-bkg[i] gt 0 or nlat gt 0 then begin ;add=' ******' else add=''
           if nlat gt 0 then add='***'
;           confidlev,bkg[i],src[i],0.9973,smin,smax,/noprint
;           psig=-gauss_cvf((1.+(1.-pprob))/2.)
;           if smin gt 0 then colprint,grb,bkg[i],src[i],sigma,prob,pprob,psig,add;smin,smax,add
           colprint,grb,bkg[i],src[i],pprob_s[i],psig_s[i],pprob_c[i],psig_c[i],add ;smin,smax,add
           if add eq '' then files=files+' '+grb+'/Bkg_Estimates/0.00_'+run+'.00/Results_'+grb+'_BOTH.png' else kfiles=kfiles+' '+grb+'/Bkg_Estimates/0.00_'+run+'.00/Results_'+grb+'_BOTH.png'
;           k=get_kbrd(10)
;           if k eq 's' then stop
        endif 
     endif 
  endfor 
;  com='tar cvfz orphan_'+run+'_candidates.tar.gz '+files
;  print,com
;  spawn,com

;  com='tar cvfz known_lat_grbs_'+run+'.tar.gz '+kfiles
;  print,com
;  spawn,com

  w=where(src ne 0)
  pprob_s=pprob_s[w]
  pprob_c=pprob_c[w]
  psig_s=psig_s[w]
  psig_c=psig_c[w]
  src=src[w]
  bkg=bkg[w]
  grbs=grbs[w]
  lat=lat[w]
  w=where(psig_s lt -100,nw)
  if nw gt 0 then psig_s[w]=-8
  w=where(psig_c lt -100,nw)
  if nw gt 0 then psig_c[w]=8
;  w=where(abs(psig_c) gt 100)
;  psig_c[w]=0.
  ws=where(lat eq 0)            ; and psig_s gt 0)
  wl=where(lat eq 1)
stop
  begplot,name='~/Fermi/Orphan_GeV_afterglows/significance_distributions_'+run+'.ps',/color
  !p.multi=[0,1,2]

  xrange=[-9,9]
  plothist,psig_s[ws],bin=0.2,xtitle='Sigma',xrange=xrange,/fill,/xsty
  plothist,psig_s[wl],bin=0.2,/over,color=!red,xrange=xrange,/xsty

  plothist,psig_c[ws],bin=0.2,xtitle='Sigma (w/ 15% sys)',xrange=xrange,/fill,/xsty
  plothist,psig_c[wl],bin=0.2,/over,color=!red,xrange=xrange,/xsty
  oplot,[3.,3.],[0,20],line=1
  !p.multi=0

  legend,['Swift','LAT'],box=0,textcolor=[!p.color,!red],/top,/right
  endplot
;;;add GBM?

  stop
;prob=1.-(1.-gauss_pdf(sigma))*2
  return
end 

pro asp_search
  cd,'~/Fermi/Orphan_GeV_afterglows/'
  file='Swift_GRBs_in_LAT.csv'

  readcol,file,sgrb,trigtime,t90,ra,dec,err,format='(a,a,a,a,a,a)',delim=',',skip=1
  ns=n_elements(sgrb)

  cd,'gifs/'
  gifs=file_search('*png')
  ngifs=n_elements(gifs)
  ftimes=strarr(ngifs)
  fgrb=strarr(ngifs)
  for i=0,ngifs-1 do begin 
     fstuff=str_sep(gifs[i],'_')
     ftimes[i]=fstuff[0]
     fstuff=str_sep(fstuff[1],'.')
     fgrb[i]=fstuff[0]
  endfor 

  ftimes=double(ftimes)
  s=sort(fgrb)
;  s=reverse(s)
;  colprint,'GRB'+fgrb[s],ftimes[s]
  cd,'..'
;  readcol,'ASP_lists/ASP_comb_lists.list',n,x,y,ara,adec,aposerr,snr,k,counts,sigc,bkg,sigbkg,/silent
  cd,'ASP_lists'
  asp=file_search('0*')
  nasp=n_elements(asp)
  ara=0d & adec=0d & asps='' & tstart=0d & tstop=0d
  for i=0,nasp-1 do begin
     aspfile=asp[i]+'/Filtered_evt_map_pgw_out_'+asp[i]+'.fits'
     gtifile=asp[i]+'/Filtered_evt_map_'+asp[i]+'.fits'
     fs=file_size(aspfile)
     if fs gt 8640 then begin 
        f=mrdfits(aspfile,1,/silent)
        nf=n_elements(f)
        ara=[ara,f.raj2000]
        adec=[adec,f.decj2000]
        asps=[asps,replicate(asp[i],nf)]
        g=mrdfits(gtifile,1,/silent)
;        tstart=[tstart,met2date(min(g.start))]
;        tstop=[tstop,met2date(max(g.stop))]
        tstart=[tstart,replicate(min(g.start),nf)]
        tstop=[tstop,replicate(max(g.stop),nf)]
     endif 
  endfor 
  ara=ara[1:*] & adec=adec[1:*] & asps=asps[1:*]
  tstart=tstart[1:*] & tstop=tstop[1:*]

  ras=dblarr(ns)
  decs=dblarr(ns)
  corr=0
  
  for i=0,ns-1 do begin

     ras[i]=ten(ra[i])*15.
     decs[i]=ten(dec[i])
     dist=separation(ras[i],decs[i],ara,adec)/3600.
     gcirc,2,ras[i],decs[i],ara,adec,dist2
     dist2=dist2/3600.
     w=where(dist2 lt 3. or dist lt 3.,nw)
     if nw gt 0 then begin 
;        print,sgrb[i]
        year='20'+strmid(sgrb[i],0,2)
        month=strmid(sgrb[i],2,2)
        day=strmid(sgrb[i],4,2)
        doy=ymd2dn(year,month,day)
        t0=date2met(year+'-'+ntostr(fix(doy))+'-'+trigtime[i])
        for j=0,nw-1 do begin 
           date=met2date_judy(tstart[w[j]])
           sdate=ntostr(year)+ntostr(doy)
           fdate=ntostr(fix(date[0]))+ntostr(fix(date[1]))
           sdate=sdate*1L
           fdate=fdate*1L
           if abs(sdate-fdate) le 1 and nw gt 0 then begin
;              colprint,sgrb[i],'  ',ras[i],'  ',decs[i],ara[w[j]],adec[w[j]],dist[w[j]],dist2[w[j]],asps[w[j]],met2date(tstart[w[j]]),met2date(tstop[w[j]])
              colprint,sgrb[i],'  ',dist2[w[j]],asps[w[j]],t0-tstart[w[j]],t0-tstop[w[j]],asps[w[j]],ara[w[j]],adec[w[j]]
              corr=[corr,w[j]]
           endif 
        endfor 
     endif 
  endfor 
  corr=corr[1:*]

  map_set,/aitoff,/grid
  oplot,ara,adec,psym=1
  oplot,ras,dec,psym=1,color=!red
  oplot,ara[corr],adec[corr],psym=1,color=!blue

stop

  return
end 
  
