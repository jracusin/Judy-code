@fit_functions
pro plot_uvotz

  grbstr=mrdfits('/Volumes/Apps_and_Docs/jracusin/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  readcol,'~/Fermi/LAT_lim_XRT/Swift_targid.txt',grb,targid,trig,t90,fluence,xrtdet,xflare,uvotdet,radiodet,irdet,optdet,format='(a,l,a,a,a,a,a,a,a,a,a)',delim='|'
  match,strtrim(grbstr.grb,2),strcompress(grb,/remove),m1,m2         
  y=where(uvotdet[m2] eq 'Y ' or uvotdet[m2] eq 'P ')
  n=where(uvotdet[m2] eq 'N ')
  u=where(uvotdet[m2] eq 'U ')

  begplot,name='~/Fermi/LAT_lim_XRT/uvot_z.eps',/land,/color
  !p.charsize=2.
  plothist,grbstr[m1].z,bin=0.5,xtitle='z',ytitle='N',/fill,fcolor=!grey80,color=!grey80
  plothist,grbstr[m1[y]].z,bin=0.5,/over,color=!red,/fill,fcolor=!red
  plothist,grbstr[m1[n]].z,bin=0.5,/over,color=!blue,thick=5,/fill,/fline,forient=45,fcolor=!blue
  plothist,grbstr[m1[u]].z,bin=0.5,/over,color=!green,thick=5,fcolor=!green,/fill
  oplot,[0,10],[0,0]
  legend,['All Swift w/ z','UVOT Detection (115)','UVOT Non-detection (50)','UVOT Unobserved'],box=0,/top,/right,textcolor=[!p.color,!red,!blue,!green]
  endplot

  stop
  return
end 
pro plot_samp

  gstr=mrdfits('~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
  
  !x.margin=[12,0]
  ;;; t90
  w=where(gstr.t90 ne 0)
  t90=gstr[w].t90
  begplot,name='~/Fermi/LAT_lim_XRT/t90.eps',/land,/encap,/color
  plothist,alog10(t90),bin=0.1,xtickname=['0.1','1','10','100'],xtitle='BAT T!L90!N (s)',ytitle='N',charsize=2
  oplot,alog10([2.,2.]),[0,15],line=2,color=!red
  xyouts,alog10(0.3),5,'short',charsize=2
  xyouts,alog10(5.),10,'long',charsize=2
  endplot
  ;;; bat_fluence
  w=where(gstr.bat_fluence ne 0)
  f=gstr[w].bat_fluence
  begplot,name='~/Fermi/LAT_lim_XRT/bat_fluence.eps',/land,/encap,/color
  plothist,alog10(f),bin=0.1,xtitle='BAT Fluence (15-150 keV) (erg cm!U-2!N)',ytitle='N',xrange=[-8,-3],xtickname='10!U'+ntostr(indgen(6)-8)+'!N',charsize=2
  oplot,alog10([1.4e-6,1.4e-6]),[0,100],line=2,color=!red
  endplot
  ;;; XPh_ind
;  p=gstr.xph_ind
;  begplot,name='~/Fermi/LAT_lim_XRT/bat_fluence.eps',/land,/encap,/color


stop
  return
end 
pro plot_results,ps=ps

  gstr=mrdfits('~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
  readcol,'~/Fermi/LAT_lim_XRT/splits/TS_scan_results.txt',split,nsamp,ts1,flux1,fluxerr1a,fluxerr1b,ts2,flux2,fluxerr2a,fluxerr2b,ts3,flux3,fluxerr3a,fluxerr3b,format='(a,i,f,f,f,f,f,f,f,f,f,f,f,f)',delim='(,) '

  nsplit=n_elements(split)/2.+0.5
  ngrb=n_elements(gstr)
  eng=[indgen(98)*0.1+0.3]
  engarr=fltarr(2,97)
  engarr[0,*]=eng[0:96]
  engarr[1,*]=eng[1:97]
  noe=n_elements(eng)
ind=0
  for i=0,nsplit-1 do begin 
     case i of
        0: begin
           w1=where(gstr.flux2 ne 0,nw1)
           nw2=0
           sp=['All GRBs','']
           splits='all'
        end
        1: begin
           w1=where(strtrim(gstr.dur,2) eq 'long',nw1)
           w2=where(gstr.dur eq 'short',nw2)
           sp=['long','short']
           splits='dur'
        end
        2: begin 
           w1=where(gstr.uvotdet eq 'Y',nw1)
           w2=where(gstr.uvotdet eq 'N',nw2)
           sp=['UVOT Detection','UVOT Non-Detection']
           splits='uvot'
        end
        3: begin 
           w1=where(gstr.xflare eq 'Y',nw1)
           w2=where(gstr.xflare eq 'N',nw2)
           sp=['XRT Flare','XRT No Flares']
           splits='flare'
        end
        4: begin 
           w1=where(gstr.bat_fluence gt 1.4e-6,nw1)
           w2=where(gstr.bat_fluence le 1.4e-6,nw2)
           sp=['Fluence > 1.4x10!U-6!N erg cm!U-2!N','Fluence < 1.4x10!U-6!N erg cm!U-2!N']
           splits='fluence'
        end
     endcase 

     fe1=fltarr(nw1,noe)
     if nw2 gt 0 then fe2=fltarr(nw2,noe) else fe2=0

     for j=0,nw1-1 do begin 
        norm=gstr[w1[j]].flux2/(intpow([0.3,10.0],[1.,gstr[w1[j]].xph_ind])*9.7)
        fe1[j,*]=pow(eng+0.05,[norm,gstr[w1[j]].xph_ind])
     endfor 
     for j=0,nw2-1 do begin 
        norm=gstr[w2[j]].flux2/(intpow([0.3,10.0],[1.,gstr[w2[j]].xph_ind])*9.7)
        fe2[j,*]=pow(eng+0.05,[norm,gstr[w2[j]].xph_ind])
     endfor 

;;; sum of fe1[j,*]*0.1=flux2
     fe=fltarr(6,noe) ;;0=median, 1=-1sig, 2=+1sig for split1 3=median, 4=-1sig, 5=+1 sig for split2
     for j=0,noe-1 do begin 
        flux=fe1[*,j]
        s=sort(flux)
        fe[0,j]=10^mean(alog10(flux));
;        fe[0,j]=median(flux)
        fe[1,j]=flux[s[round(nw1*0.16)]]
        fe[2,j]=flux[s[round(nw1*0.84)]]

        if nw2 gt 0 then begin 
           flux=fe2[*,j]
           s=sort(flux)
           fe[3,j]=10^mean(alog10(flux))
;           fe[3,j]=median(flux)
           fe[4,j]=flux[s[round(nw2*0.16)]]
           fe[5,j]=flux[s[round(nw2*0.84)]]
        endif 
     endfor 
;stop
     h=4.13566d-15*1d-3 ;; kev s
;     eng=eng/h
;     xrange=[1d16,1d27]
     xrange=[0.1,1d9]
     yrange=[1d-16,1d-10]

     if keyword_set(ps) then begplot,name='~/Fermi/LAT_lim_XRT/'+splits+'_sed.eps',/land,/color
;     ytitle='E F(E) [kev (erg cm!U-2!N s!U-1!N keV!U-1!N)]'
     ytitle=!tsym.nu+' F!L'+!tsym.nu+'!N (erg cm!U-2!N s!U-1!N)'
     plot,xrange,yrange,/xlog,/ylog,xtitle='E (keV)',ytitle=ytitle,/nodata,/xsty

     ;;; plot x-ray total and f(E) fluxes
;     oplot,[0.3,10]/h,replicate(median(gstr[w1].flux2),2)*[1.73,1.73],color=!green
     s=sort(gstr[w1].flux2)
     f1=gstr[w1[s[round(nw1*0.16)]]].flux2
     f2=gstr[w1[s[round(nw1*0.84)]]].flux2
;     oplot,[1.73,1.73]/h,[f1,f2]*[1.73,1.73],color=!green

     oplot,eng,fe[0,*]*eng
     oplot,eng,fe[1,*]*eng,line=2
     oplot,eng,fe[2,*]*eng,line=2

     if nw2 gt 0 then begin 
;        oplot,[0.3,10]/h,replicate(median(gstr[w2].flux2),2)*[1.,1.],color=!magenta
     
        oplot,eng,fe[3,*]*eng,color=!red
        oplot,eng,fe[4,*]*eng,line=2,color=!red
        oplot,eng,fe[5,*]*eng,line=2,color=!red
     endif 
     ;;; plot LAT 0.1-300, 0.1-1, and 1-300 GeV bands

     c=1d-6
     kev2erg=1.602e-12*1d3  ;;; 1 ev = 1.6e-12 erg ; 1kev = 1.6e-9 erg
     gev2erg=kev2erg*1d6
     ind=i*2
     en1=[0.1d6,300d6]
     en2=replicate(10.^mean(alog10([en1])),2)
     en3=[0.1d6,1d6]
     en4=replicate(10.^mean(alog10([en3])),2)
     en5=[1d6,300d6]
     en6=replicate(10.^mean(alog10([en5])),2)
     de1=en1[1]-en1[0]
     de3=en3[1]-en3[0]
     de5=en5[1]-en5[0]
;     norm=flux1[ind]*c/(intpow(en1,[1.,2.07],tnorm=1e6)*(en1[1]-en1[0]));*1d6^(2.07)

;     oplot,en1/h,pow(en2,[norm,2.07])*kev2erg*en2*en2,color=!green
;     oplot,en1/h,norm*flux1[ind]*kev2erg*en1*en1,color=!green

;     eng=[1.,1e3,1e6,1e9]
     eng2=(findgen(3000)*0.1+0.1)*1d6
;     oplot,en1/h,flux1[ind]*c*en2*en2*kev2erg^2
   
;     de=eng2[1]-eng2[0]
;goto,crap
;     plot,eng2,pow(eng2+de/2.,[norm,2.07])*de,/xlog,/ylog,xrange=[1,1e9],yrange=[1d-14,1d-6]
;     oplot,[1e6,1e6],[1d-25,1d5],line=2  
;     oplot,en1,[flux1[0],flux1[0]]*c,color=!green
;     oplot,[1,1d9],[1,1],line=2
;;; how do you translate normalization at 1 GeV to norm at 1 keV???
;crap:
;;; these should be equal if all is well, all is not
 ;    print,total(pow(eng2+de/2.,[norm,2.07])*(eng2[1]-eng2[0])),flux1[ind]*c

;stop
     de=eng2[1]-eng2[0]
     normg1=flux1[ind]*c/(intpow(en1,[1.,2.07],tnorm=1e6)*de1) ;; norm at 1 GeV
     norm1=normg1*1d6^2.07   ;;; norm at 1 keV
;     gfe1=intpow(en1,[norm1,2.07])*de1 
;     gfe1=pow(1.,[norm1,2.07])  ;; photons to kev conversion factor
     f1=pow(eng2+de/2.,[norm1,2.07])
;     eflux1=total(f*(eng2+de/2.)*kev2erg*de)
     eflux1=10^mean(alog10(f1*(eng2+de/2.)^2*kev2erg))
;     eflux1=intpow(en1,[norm1,2.07])*de1*(en1[0]*en1[1])/(de1)*sqrt(en1[0]*en1[1])*kev2erg

     normg3=flux2[ind]*c/(intpow(en3,[1.,2.07],tnorm=1e6)*de3)
     norm3=normg3*1d6^2.07
;     gfe3=intpow(en3,[norm3,2.07])*de3 
;     gfe3=pow(1.,[norm3,2.07])
     f3=pow(eng2[0:9]+de/2.,[norm3,2.07])
;     eflux3=total(f*(eng2+de/2.)*kev2erg*de)
     eflux3=10^mean(alog10(f3*(eng2[0:9]+de/2.)^2*kev2erg))
;     eflux3=intpow(en3,[norm3,2.07])*de3*(en3[0]*en3[1])/(de3)*sqrt(en3[0]*en3[1])
     
     normg5=flux3[ind]*c/(intpow(en5,[1.,2.07],tnorm=1e6)*de5)
     norm5=normg5*1d6^2.07
;     gfe5=intpow(en5,[norm5,2.07])*de5  
;     gfe5=pow(1.,[norm5,2.07])
     f5=pow(eng2[9:*]+de/2.,[norm5,2.07])
;     eflux5=total(f*(eng2+de/2.)*kev2erg*de)
;     eflux5=total(f5*eng2[9:*]*kev2erg*eng2[9:*])
     eflux5=10^mean(alog10(f5*(eng2[9:*]+de/2.)^2*kev2erg))
;     eflux5=intpow(en5,[norm5,2.07])*de5*(en5[0]*en5[1])/(de5)*sqrt(en5[0]*en5[1])

;     oplot,en1,[eflux1,eflux1],color=!green
;     oplot,en2,[eflux1*(1.+fluxerr1a[ind]/flux1[ind]),eflux1*(1.+fluxerr1b[ind]/flux1[ind])],color=!green
;     oplot,en2/h,[flux1[ind]+fluxerr1a[ind],flux1[ind]+fluxerr1b[ind]]*c*gfe1*en2*kev2erg,color=!green
     oplot,en3,[eflux3,eflux3]
     oplot,en4,[eflux3*(1.+fluxerr2a[ind]/flux2[ind]),eflux3*(1.+fluxerr2b[ind]/flux2[ind])]
;     oplot,en4/h,[flux2[ind]+fluxerr2a[ind],flux2[ind]+fluxerr2b[ind]]*c*gfe3*en4*kev2erg
     oplot,en5,[eflux5,eflux5]
     oplot,en6,[eflux5*(1.+fluxerr3a[ind]/flux3[ind]),eflux5*(1.+fluxerr3b[ind]/flux3[ind])]
;     oplot,en6/h,[flux3[ind]+fluxerr3a[ind],flux3[ind]+fluxerr3b[ind]]*c*gfe5*en6*kev2erg

;     oplot,en1/h,[flux1[ind],flux1[ind]]*c*gfe1*en2*kev2erg,color=!green
;     oplot,en2/h,[flux1[ind]+fluxerr1a[ind],flux1[ind]+fluxerr1b[ind]]*c*gfe1*en2*kev2erg,color=!green
;     oplot,en3/h,[flux2[ind],flux2[ind]]*c*gfe3*en4*kev2erg
;     oplot,en4/h,[flux2[ind]+fluxerr2a[ind],flux2[ind]+fluxerr2b[ind]]*c*gfe3*en4*kev2erg
;     oplot,en5/h,[flux3[ind],flux3[ind]]*c*gfe5*en6*kev2erg
;     oplot,en6/h,[flux3[ind]+fluxerr3a[ind],flux3[ind]+fluxerr3b[ind]]*c*gfe5*en6*kev2erg

     if nw2 gt 0 then begin
        ind=i*2+1
        oplot,en1/h,[flux1[ind],flux1[ind]]*c*en1/intpow(en1,[1.,2.07]),color=!magenta
        oplot,en2/h,[flux1[ind]+fluxerr1a[ind],flux1[ind]+fluxerr1b[ind]]*c*en2/intpow(en1,[1.,2.07]),color=!magenta

        oplot,en3/h,[flux2[ind],flux2[ind]]*c*en3/intpow(en3,[1.,2.07]),color=!red
        oplot,en4/h,[flux2[ind]+fluxerr2a[ind],flux2[ind]+fluxerr2b[ind]]*c*en4/intpow(en3,[1.,2.07]),color=!red

        oplot,en5/h,[flux3[ind],flux3[ind]]*c*en5/intpow(en5,[1.,2.07]),color=!red
        oplot,en6/h,[flux3[ind]+fluxerr3a[ind],flux3[ind]+fluxerr3b[ind]]*c*en6/intpow(en5,[1.,2.07]),color=!red
     endif 

   
     legend,sp,box=0,/bottom,/right,textcolor=[!p.color,!red]

     k=get_kbrd(10)
     if k eq 's' then stop
     if keyword_set(ps) then endplot
  endfor
  
     stop
return
end

pro plot_lat_coverage

  burst=mrdfits('~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
  n=n_elements(burst)
;  time=dblarr(n,1e3)
  t=lonarr(1e4+1L)
  t100=lonarr(100+1L)
  t300=lonarr(300+1L)
  t1000=lonarr(1000+1L)
  t3000=lonarr(3000+1L)
  t10000=lonarr(1e4+1L)
  x=lonarr(1e4+1L)
;  xnof=lonarr(1e4+1L)
;  n100=intarr(n)
;  n300=intarr(n)
;  n1000=intarr(n)
;  n3000=intarr(n)
;  n10000=intarr(n)
  for i=0,n-1 do begin
     gdir='~/GRBs/'+strtrim(burst[i].grb,2)+'/'
     lc=lcout2fits(gdir+'lc_newout_phil.txt')
     wl=where(burst[i].ltstart ne 0,nwl)
     for j=0,nwl-1 do begin 
        lts=round(burst[i].ltstart[j])
        ltp=round(burst[i].ltstop[j])
        if ltp gt 1e4 then ltp=1e4
        if lts lt 1e4 then t[lts:ltp]=t[lts:ltp]+1
;        if lts lt 100 then print,lts
        ;;; how many bursts were visible in first
        ;;; 100,300,1000,3000,10000 s as function of time bin
        

;        xts=round(burst[i].xtstart)
;        x[xts:*]=x[xts:*]+1.
     endfor 
     wno0=where(burst[i].ltstart ne 0)
     m=min(burst[i].ltstart[wno0])
     print,m
     if m le 100 then t100[round(m)]=t100[round(m)]+1
     if m le 300 then t300[round(m)]=t300[round(m)]+1
     if m le 1000 then t1000[round(m)]=t1000[round(m)]+1
     if m le 3000 then t3000[round(m)]=t3000[round(m)]+1
     if m le 10000 then t10000[round(m)]=t10000[round(m)]+1

     wx=where(lc.tstop lt 1d4,nwx)
     x0=lonarr(1e4+1L)
     for k=0,nwx-1 do begin 
        xts=round(lc[wx[k]].tstart)
        xtp=round(lc[wx[k]].tstop)
        x0[xts:xtp]=1
     endfor 
     x=x+x0
;        x[xts:xtp]=x[xts:xtp]+1
;     if exist(gdir+'flares_gtis.dat') then begin 
;        readcol,gdir+'flares_gtis.dat',fstart,fstop,format='(d,d)'
;        wf=where(fstart lt 1d4,nwf)
;        for k=0,nwf-1 do begin 
;           xnfts=round(fstart[k])
;           xnftp=round(fstop[k])
;           if xnftp gt 1d4 then xnftp=1d4
;           xnof[xnfts:xnftp]=xnof[xnfts:xnftp]-1
;        endfor 
;     endif 

  endfor 
  begplot,name='~/Fermi/LAT_lim_XRT/bursts_in_FoV.eps',/land,/color
  plot,lindgen(1e4),t,ytitle='Number of BAT GRBs',xtitle='Time since BAT Trigger (s)',yrange=[0,155],/ysty,/xlog,xrange=[1,1e4],charsize=2,psym=10
  oplot,lindgen(1d4),x,color=!red
  legend,['in LAT FoV','in XRT FoV'],box=0,/top,/left,textcolor=[!p.color,!red],charsize=2
  endplot
;  oplot,lindgen(1d4),xnof,color=!blue

stop
end

pro adjust_t,t,newt

  ot=t
  n=n_elements(t[0,*])
  nn=n_elements(newt)
  ;;; for each newt, add to t
  for k=0,nn-1 do begin 
     n=n_elements(t[0,*])
     w=where(newt[k] gt t[0,*] and newt[k] lt t[1,*],nw)
     if nw gt 0 then begin 
        ts=dblarr(2,n+1)
        tt=[[t[0,w],newt[k]],[newt[k],t[1,w]]]
        if w eq 0 and w ne n-1 then begin ;; w at beginning
           ts[*,0:1]=tt
           ts[*,2:*]=t[*,w+1:*]
        endif 
        if w ne 0 and w lt n-1 then begin ;; w in middle
           ts[*,0:w-1]=t[*,0:w-1]
           ts[*,w:w+1]=tt
           ts[*,w+2:*]=t[*,w+1:*]
        endif 
        if w ne 0 and w eq n-1 then begin ;; w at end
           ts[*,0:w-1]=t[*,0:w-1]
           ts[*,w:*]=tt
        endif
        if w eq 0 and w eq n-1 then ts=tt ;; only w
        t=ts
     endif 
  endfor 
;  print,ot
;  print,ts
;stop  
  return
end 

pro assemble_grb_list
;  readcol,'~/Fermi/LAT_lim_XRT/Swift_triggers.txt',met,tid,format='(l,l)'
  readcol,'~/Fermi/LAT_lim_XRT/Swift_targid.txt',grb,targid,trig,t90,fluence,xrtdet,xflare,uvotdet,radiodet,irdet,optdet,format='(a,l,a,a,a,a,a,a,a,a,a)',delim='|'
  grb=grb[403:661]
  grb=strcompress(grb,/remove)
  targid=targid[403:661]
  trig=trig[403:661]
  xrtdet=strtrim(xrtdet[403:661],2)
  uvotdet=strtrim(uvotdet[403:661],2)
  t90=t90[403:661]
  fluence=fluence[403:661]
  xflare=xflare[403:661]
  radiodet=radiodet[403:661]
  irdet=irdet[403:661]
  optdet=optdet[403:661]

  w=where(t90 eq ' ')
  t90[w]=0
  t90=t90*1.
  dur=strarr(n_elements(t90))
  short=where(t90 le 2.5 and t90 ne 0)
  long=where(t90 gt 2.5 or t90 eq 0.)
  dur[short]='short'
  dur[long]='long'


;  dont_match,targid,tid,dm1,dm2
;  notgrb=tid[dm2]
;  help,dm1,dm2

;  match,targid,tid,m1,m2
;  help,m1,m2
;  w=where(xrtdet[m1] ne 'Y')
;  badgrb=grb[m1[w]]
;  badgrbtid=tid[m2[w]]

  w=where((xrtdet eq 'Y') and grb ne 'GRB090510' and grb ne 'GRB100728A'); or xrtdet eq 'P'))
  x=where(xrtdet ne 'Y')

  grb=grb[w]
  targid=targid[w]
  xrtdet=xrtdet[w]
  uvotdet=uvotdet[w]
  dur=dur[w]
  xflare=xflare[w]
  fluence=fluence[w]
  trig=trig[w]
  t90=t90[w]
  radiodet=radiodet[w]
  irdet=irdet[w]
  optdet=optdet[w]

;  match,targid,tid,m1,m2
;  colprint,grb[m1],targid[m1],tid[m2],xrtdet[m1]
;  help,m1,m2

;  dont_match,targid,tid,dm1,dm2
;  help,dm1,dm2
  ;;;; investigate missing
;  tstart=fltarr(n_elements(dm1))
;  for i=0,n_elements(dm1)-2 do begin
;     file='~/GRBs/'+grb[dm1[i]]+'/lc_newout_phil.txt'
;     if not exist(file) then file='~/GRBs/'+strmid(grb[dm1[i]],0,9)+'/lc_newout_phil.txt'
;     if not exist(file) then file='~/GRBs/'+grb[dm1[i]]+'A/lc_newout_phil.txt'
;     if exist(file) then begin 
;        lc=lcout2fits(file)
;        tstart[i]=lc[0].tstart
;     endif 
;  endfor 
;  w=where(tstart lt 1d4 and tstart ne 0)

  n=n_elements(targid)
  tstart=fltarr(n)
  flares=strarr(n)
  trigtime=dblarr(n)
  for i=0,n-1 do begin
     file='~/GRBs/'+grb[i]+'/lc_newout_phil.txt'
     if not exist(file) then file='~/GRBs/'+strmid(grb[i],0,9)+'/lc_newout_phil.txt'
     if not exist(file) then file='~/GRBs/'+grb[i]+'A/lc_newout_phil.txt'
     if exist(file) then begin 
        lc=lcout2fits(file)
        tstart[i]=lc[0].tstart
     endif 
     if exist('~/GRBs/'+grb[i]+'/flares_gtis.dat') then flares[i]='Y' else flares[i]='N'
     trigtime[i]=date2met(trig[i],/fermi)
  endfor 
  w=where(tstart lt 1d4 and tstart ne 0)

  ;;; sample to give jim
  grb=grb[w]
  targid=targid[w]
  xrtdet=xrtdet[w]
  uvotdet=uvotdet[w]
  dur=dur[w]
  tstart=tstart[w]
  xflare=strtrim(xflare[w],2)
  fluence=fluence[w]
  flares=strtrim(flares[w],2)
  flare=strarr(n_elements(grb))
  x=where(xflare eq 'Y' or flares eq 'Y')
  flare[*]='N'
  flare[x]='Y'
  trigtime=trigtime[w]
  t90=t90[w]
  radiodet=radiodet[w]
  irdet=irdet[w]
  optdet=optdet[w]

  colprint,grb,targid,tstart,uvotdet,flare,dur,fluence

;  w=where((xflare eq 'N' and flares eq 'Y') or (xflare eq 'Y' and flares eq 'N'),nw)
;  help,w
;  colprint,grb[w],xflare[w],flares[w]
;  cd,'~/GRBs/'
;  for i=0,nw-1 do begin 
;     file=grb[w[i]]+'/lc_newout_phil.txt'
;     if exist(file) then plot_like_qdp,file=file
;     print,grb[w[i]],xflare[w[i]],flares[w[i]]
;     k=get_kbrd(10)
;     if k eq 's' then stop
;  endfor 
  
  writecol,'~/Fermi/LAT_lim_XRT/swift_sample.dat',grb,targid,tstart,uvotdet,flare,dur,fluence,$
           header='GRB        targid tstart udet xflare duration fluence'
 
  burst=create_struct('GRB','','targid',0L,'trigtime',0d,'xtstart',0d,$
                      'uvotdet','','radiodet','','irdet','','optdet','',$
                      'xflare','','dur','','t90',0d,'bat_fluence',0d,$
                      'xph_ind',0d,'xph_inderr',dblarr(2),'ltstart',dblarr(5),'ltstop',dblarr(5),$
                      'flux1',0d,'flux1err',0d,$
                      'flux2',0d,'flux2err',0d)
  n=n_elements(grb)
  burst=replicate(burst,n)
  burst.grb=grb
  burst.targid=targid
  burst.xtstart=tstart
  burst.uvotdet=uvotdet
  burst.radiodet=radiodet
  burst.irdet=irdet
  burst.optdet=optdet
  burst.xflare=flare
  burst.dur=dur
  burst.t90=t90
  burst.bat_fluence=fluence*1d
  burst.trigtime=trigtime
  
  mwrfits,burst,'~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',/create
stop
return
end 

pro fluxes_for_jim_lat,ps=ps

  !x.margin=[10,3]
  !p.charsize=2
  burst=mrdfits('~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',1)
  if burst[0].ltstart[0] eq 0 then begin 

  ;;; gather LAT time coverage intervals (up to 5 GTIs)
     for i=1,155 do begin
        lat=mrdfits('~/Fermi/LAT_lim_XRT/LAT_gtis.fits',i,hdr)
        tid=sxpar(hdr,'EXTNAME')
        w=where(burst.targid eq tid,nw)
        if nw eq 0 then stop
        ngti=n_elements(lat.start)
        burst[w].ltstart[0:ngti-1]=lat.start-burst[w].trigtime
        burst[w].ltstop[0:ngti-1]=lat.stop-burst[w].trigtime
     endfor 
     
     w=where(burst.ltstart[0] ne 0,n)
     burst=burst[w]
     
     mwrfits,burst,'~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',/create
  endif else n=n_elements(burst)
;  stop
  ratio=fltarr(n)
  tstart=dblarr(n)
  tstop=dblarr(n)
  diff=dblarr(n)
  redo=intarr(n)
  count=0
  cd,'~/GRBs/'
  w=where(burst.ltstart[0] lt 1,nw)
  if nw gt 0 then burst[w].ltstart[0]=1.
  for i=0,n-1 do begin
     gdir=''
     if exist(burst[i].grb) then gdir=burst[i].grb else begin
        if exist(strtrim(burst[i].grb,2)+'A') then begin
           burst[i].grb=strtrim(burst[i].grb,2)+'A' ;gdir=strtrim(burst[i].grb,2)+'A'
           gdir=burst[i].grb
        endif 
        sg=strmid(burst[i].grb,0,9)
        if exist(sg) then begin 
           gdir=sg
           burst[i].grb=sg
        endif 
     endelse
;     print,gdir
     gdir=strtrim(gdir,2)
     if gdir eq '' then stop
     cd,gdir
     print,burst[i].grb
     lc=lcout2fits('lc_newout_phil.txt')
     read_lcfit,'lc_fit_out_idl_int7.dat',pname,p
     sfile='UL_specfits.fits'
     if exist(sfile) then begin 
        spec=mrdfits(sfile,1)
        np=n_elements(p)
        if exist('flares_gtis.dat') then begin 
           readcol,'flares_gtis.dat',fstart,fstop,format='(d,d)'
           if fstart[0] gt lc[0].tstart then xstart=lc[0].tstart else begin 
              redo[i]=1
              xstart=fstop[0]
           endelse 
        endif else xstart=lc[0].tstart
;        tstop[i]=1e4
;        if tstart[i] lt tstop[i] then begin 
;           t=[0d,0d];[tstart[i],tstop[i]]
;           print,burst[i].ltstart
;           print,burst[i].ltstop
;           if burst[i].ltstart[0] gt tstart[i] then tstart[i]=burst[i].ltstart[0]
;;           if tstop[i] gt max(burst[i].ltstop,m) then
        tstop[i]=max(burst[i].ltstop)
        tstart[i]=burst[i].ltstart[0]
        wni=where(burst[i].ltstart ne 0,ni)
        t=dblarr(2,ni)
        if ni eq 1 then t=[tstart[i],tstop[i]] else $
           for k=0,ni-1 do t[*,k]=[burst[i].ltstart[k],burst[i].ltstop[k]]
;;         print,t
;;         if xstart gt t[0,0] then begin 
;;            if xstart lt t[1,0] then t[0,0]=xstart else begin 
;;               if xstart gt t[1,0] and xstart lt t[0,1] then begin 
;;                  t=t[*,1:*]
;;                  ni=ni-1
;;               endif else begin 
;;                  if xstart gt t[1,0] then begin 
;;                     if xstart gt t[0,1] and xstart lt t[1,1] then begin 
;;                        t=t[*,1:*]
;;                        t[0,0]=xstart
;;                        ni=ni-1
;;                     endif else begin 
;;                        if xstart gt t[1,1] and xstart lt t[0,2] then begin 
;;                           t=t[*,2:*]
;;                           ni=ni-2
;;                        endif else begin 
;;                           if xstart gt t[0,2] and xstart lt t[1,2] then begin 
;;                              t=t[*,2:*]
;;                              t[0,0]=xstart
;;                              ni=ni-2
;;                           endif 
;;                        endelse 
;;                     endelse 
;;                  endif
;;               endelse 
;;            endelse 
;;            diff[i]=xstart-burst[i].ltstart[0]
;;            count=count+1
;;            print
;;            print,xstart
;;            print,t
;;         endif 
;;         print,t[0,0]
        
        f2=0
        t2=dblarr(n_elements(t[0,*]))
        for z=0,n_elements(t[0,*])-1 do t2[z]=10^mean(alog10(t[*,z]))
        case np of
           2: begin 
;              f=intpow(t,p)
              f=pow(t2,p)
           end 
           4: begin 
              adjust_t,t,p[2]
;              f=intbknpow(t,p)
              f=bknpow(t2,p)
              tt=p[2]*2.
              n2=bknpow(tt,p)/pow(tt,[1.,p[3:*]])
              p2=[n2,p[3:*]]
              if p[1] gt p[3] then f2=pow(t2,p2);f2=intpow(t,p2)
           end 
           6: begin 
              adjust_t,t,[p[2],p[4]]
;              f=intbkn2pow(t,p)
              f=bkn2pow(t2,p)
              tt=p[2]*2.
              n2=bkn2pow(tt,p)/bknpow(tt,[1.,p[3:*]])
              p2=[n2,p[3:*]]
              if p[1] gt p[3] then f2=bknpow(t2,p2);f2=intbknpow(t,p2)
           end 
           8: begin
              adjust_t,t,[p[2],p[4],p[6]]
;              f=intbkn3pow(t,p)
              f=bkn3pow(t2,p)
              tt=p[2]*2.
              n2=bkn3pow(tt,p)/bkn2pow(tt,[1.,p[3:*]])
              p2=[n2,p[3:*]]
              if p[1] gt p[3] then f2=bkn2pow(t2,p2);f2=intbkn2pow(t,p2)
           end 
           10: begin 
              adjust_t,t,[p[2],p[4],p[6],p[8]]
;              f=intbkn4pow(t,p)
              f=intbkn4pow(t2,p)
              tt=p[2]*2.
              n2=bkn4pow(tt,p)/bkn3pow(tt,[1.,p[3:*]])
              p2=[n2,p[3:*]]
              if p[1] gt p[3] then f2=bkn3pow(t2,p2);f2=intbkn3pow(t,p2)
           end 
        endcase 
        ni=n_elements(t[0,*])
        print,t
        print,f
        print,f2
        tbin=t[1,*]-t[0,*]
;        f=total(f*tbin)/total(tbin)
;        f2=total(f2*tbin)/total(tbin)
        f=10^mean(alog10(f))
        f2=10^mean(alog10(f2))
        print,f2
        ts=[(findgen(10))*10.+1,(indgen(100)+1.)*100.]+burst[i].xtstart
        if f2[0] eq 0. then f2=f
        ratio[i]=spec[n_elements(spec)-1].unabs_cfratio
        if keyword_set(ps) then begplot,name='~/Fermi/LAT_lim_XRT/xrt_lcfits/'+strtrim(burst[i].grb,2)+'.eps',/land,/color
        plot,[1,1d4],[1d-14,1-8],/nodata,/xlog,/ylog,xrange=[1,1e4],title=burst[i].grb,/xsty,/ysty,yrange=[1d-14,1d-8],xtitle='Time since BAT trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)'
        for k=0,ni-1 do begin
           polyfill,[t[0,k],t[1,k],t[1,k],t[0,k],t[0,k]],[1d-14,1d-14,1d-8,1d-8,1d-14],color=!red
           oplot,[t[0,k],t[0,k]],[1d-15,1d10],line=1
           oplot,[t[1,k],t[1,k]],[1d-15,1d10],line=1
        endfor 

        if np ge 6 then begin
;           w=where(ts gt burst[i].xtstart)
;           ts=ts[w]
           if p[1] gt p[3] then begin
              oplot,ts,bkn2pow(ts,p)*ratio[i]
              oplot,ts,bknpow(ts,p2)*ratio[i],line=2
           endif else oplot,ts,bknpow(ts,p)*ratio[i]
        endif else begin
           if np eq 2 then oplot,ts,pow(ts,p)*ratio[i]
           if np eq 4 then begin 
              oplot,ts,bknpow(ts,p)*ratio[i]
              oplot,ts,pow(ts,p2)*ratio[i],line=2
           endif 
        endelse 

        if f[0] lt 1d-4 then f[*]=0
        if f2[0] lt 1d-4 then f2[*]=0
        
        oplot,[1,1e4],[f,f]*ratio[i],line=3
        oplot,[1,1e4],[f2,f2]*ratio[i],line=1,color=!green
        print,np,f,f2
        oploterror2,lc.time,lc.src_rate*ratio[i],[[lc.time-lc.tstart],[lc.tstop-lc.time]],[[lc.src_rate_err],[lc.src_rate_err]]*ratio[i],/nohat,psym=3
;        if i eq 19 or i eq 145 or i eq 154 then stop
        if not keyword_set(ps) then begin
           q=get_kbrd(10)
           if q eq 's' then stop
        endif 
;           stop
;        endif 
        if keyword_set(ps) then endplot
        burst[i].flux1=f*spec[n_elements(spec)-1].unabs_cfratio
        burst[i].flux2=f2*spec[n_elements(spec)-1].unabs_cfratio
        burst[i].xph_ind=spec[n_elements(spec)-1].phind
        burst[i].xph_inderr=spec[n_elements(spec)-1].phinderr
     endif  
;     endif 
     cd,'..'
  endfor 
  mwrfits,burst,'~/Fermi/LAT_lim_XRT/grb_flux_struct.fits',/create

  stop  ;;;; why are almost all flux2 = flux?????
  goto,skipcrap
  w=where(burst.flux1 ne 0 and burst.xtstart lt 1d4,nw)
  w0=where(burst.flux2 eq 0,nw0)
  if nw0 gt 0 then begin 
     badgrb=[badgrb,burst[w0].grb]
     badgrbtid=[badgrbtid,burst[w0].targid]
  endif 
  help,badgrb,notgrb,w
;  writecol,'~/Fermi/LAT_lim_XRT/badgrbs.txt',badgrb,badgrbtid
;  writecol,'~/Fermi/LAT_lim_XRT/notgrbs.txt',notgrb

  readcol,'~/Fermi/LAT_lim_XRT/Swift_list_otherobs.txt',grb,tid,ot1,ot2,ot3,ot4,delim='|', format='(a,l,a,a,a,a)'

  match,burst[w].targid,tid,m1,m2
  wo=where(strtrim(ot1[m2],2) eq 'Fermi-GBM' or strtrim(ot2[m2],2) eq 'Fermi-GBM' or strtrim(ot3[m2],2) eq 'Fermi-GBM' or strtrim(ot4[m2],2) eq 'Fermi-GBM')
  gbm=strarr(n_elements(flux))
  gbm[*]='N'
  gbm[w[m1[wo]]]='Y'
  
  writecol,'~/Fermi/LAT_lim_XRT/XRT_intflux_10ks.txt',burst[w].grbs,burst[w].targid,burst[w].flux1,burst[w].xtstart,burst[w].xtstop,burst[w].xph_ind,gbm[w],header='GRB         TID     flux        Tstart  Tstop   phInd    GBM'

  stop
  skipcrap:

  return
end 
