pro fermi_march_grbs,ps=ps

  dir='~/Fermi/March_GRBs/'
  cd,dir
  if keyword_set(ps) then begplot,name='afterglows.ps',/color

  grbs=['GRB090323','GRB090328']
  gamma=[1.96,1.82]
  colors=[!red,!blue,!green,!orange,!purple,!cyan,!magenta,!yellow,!grey50,!forestgreen]

  g=3631
  r=3631 ;; Jy
  i=3631
  z=3564.727 ;; Jy
  J=1560
  H=1040
  K=645
  RR=2941d ;; Jy

  flux0=[g,r,i,z,J,H,K,RR]
  sflux0=['g','r','i','z','J','H','K','R']
  zp=[17.89,19.11,18.34,17.49,16.82,17.35,20.29]
  szp=['v','b','u','w1','m2','w2','white']
  
;  flux0=[white,v0,b0,u0,uvw1,uvm2,uvw2,b,v,sdssr,r,sdssi,i,j,h,k]
;  sflux0=['white','v0','b0','u0','uvw1','uvw2','b','v','sdssr','r','sdssi','i','J','H','K']

;  r=2941d ;;; Jy
;  flux00=r;*1e3
  c=3d10            ;;cm/s
  eff_white=3471e-8 ;; cm
  fd_white=3.7d-17  ;; erg cm-2 s-1 ang-1
  eff_v=5402e-8
  fd_v=2.614e-16

  plotsym,1,2
  !p.multi=[0,1,2]
  for g=0,1 do begin 
     colors=colors[g:*]
     cd,grbs[g]
     readcol,'data_table.csv',telescope,band,time,terr,mag,magerr,format='(a,a,d,d,f,f)',delim=','
     flux00=fltarr(n_elements(band))
     flux00[*]=1.
     for b=0,n_elements(flux0)-1 do begin
        wb=where(band eq sflux0[b],nwb)
        if nwb gt 0 then flux00[wb]=flux0[b]
     endfor 

     flux=flux00*10^(-mag/2.5)
     fluxerr=flux00*(10^(-mag/2.5)-10^(-(mag+magerr)/2.5))
     wb=where(band eq '8.46GHz' or band eq '4.9GHz',nwb)
     if nwb gt 0 then begin
        flux[wb]=mag[wb]*1e-6
        fluxerr[wb]=magerr[wb]*1e-6
     endif 

     ufile='uvot_lc.dat'
     if exist(ufile) then begin 
        readcol,ufile,ufilt,utstart,utstop,uexp,usig,umag,umagerr,urate,uraterr,uflux,ufluxerr,format='(a,f,f,f,f,f,f,f,f,f,f)'
        urate[*]=1.
        uraterr[*]=ufluxerr/uflux
        ufact=uflux;/urate
        ufact=ufact*1e-3
        utime=(utstop-utstart)/2.+utstart
        uterr=(utstop-utstart)/2.
;        u=where(umagerr ne 3.)
        u=indgen(n_elements(ufilt))
        utime=utime[u] & urate=urate[u]
;UVW2     UVM2     UVW1       u        b        v       white
;Flux ratio   0.82796  0.81765  0.85168  0.89288  0.91288  0.93128   0.88182
;Magnitudes    0.205    0.219    0.174    0.123    0.099    0.077    0.137
;           ufact=ufact/0.85168  ;u
        ww=where(ufilt eq 'WHITE')
        wu=where(strtrim(ufilt,2) eq 'U')
        wv=where(strtrim(ufilt,2) eq 'V')
        ufact[ww]=ufact[ww]/0.88182     ;white
        ufact[wu]=ufact[wu]/0.85168     ;u
     endif else begin
        ufile='CATOnormalize.txt' 
        readcol,ufile,utime,uterr,urate,uraterr,format='(d,d,d,d)'
        ufact=replicate(fd_v*1e23*1e8*eff_v^2/c,n_elements(utime))

;        wu=where(utime-utime[1:*] gt 0)
;        ww=indgen(wu+1)
;        wu=indgen(n_elements(utime)-wu-1)+wu[0]+1
        wu=n_elements(utime)
        ww=indgen(wu)
        umagerr=fltarr(n_elements(utime))

        ;;; need extinction correction
        umag=umagerr
        umag=zp[0]-2.5*alog10(urate)
        av= 0.177716
        umag2=umag-av
        urate2=10.^((umag2-zp[0])/(-2.5))
;        urate=urate2

     endelse 
     

     bb=band(sort(band))
     u=uniq(bb)

     fact=1.;5e2
     readcol,'flux.qdp',xtime,xterr1,xterr0,xflux,xfluxerr,format='(d,d,d,d,d)',/silent
     xfluxd=flux2jy(xflux,gamma[g])
     xfluxderr=flux2jy(xflux+xfluxerr,gamma[g])-xfluxd
     xfluxd=xfluxd*fact
     xfluxderr=xfluxderr*fact

;  ploterror,lc.time,lc.src_rate,lc.src_rate_err,/nohat,/xlog,/ylog

     yrange=[1e-9,1e-3]
     plot,[3e4,2e6],yrange,/nodata,/xlog,/ysty,yrange=yrange,/ylog,xtitle='Time (s)',ytitle='Flux Density (Jy)',/xsty
     if exist(ufile) then begin 
        wnul=where(umagerr[ww] ne 3,nwul)
        if nwul gt 0 then oploterror,utime[ww[wnul]],urate[ww[wnul]]*ufact[ww[wnul]],uterr[ww[wnul]],uraterr[ww[wnul]]*ufact[ww[wnul]],/nohat,psym=4,color=!salmon,errcolor=!salmon
        wul=where(umagerr[ww] eq 3,nwul)
        if nwul gt 0 then oplot,utime[ww[wul]],urate[ww[wul]]*ufact[ww[wul]],psym=8,color=!salmon
;        wnul=where(umagerr[wu] ne 3,nwul)
;        if nwul gt 0 then oploterror,utime[wu[wnul]],urate[wu[wnul]]*ufact[wu[wnul]],uterr[wu[wnul]],uraterr[wu[wnul]]*ufact[ww[wnul]],/nohat,psym=4,color=!darkred,errcolor=!darkred
;        wul=where(umagerr[wu] eq 3,nwul)
;        if nwul gt 0 then oplot,utime[wu[wul]],urate[wu[wul]]*ufact[wu[wul]],psym=8,color=!darkred
;        if n_elements(wv) gt 0 then begin 
;           wul=where(umagerr[wv] eq 3,nwul)
;           if nwul gt 0 then plots,utime[wv],urate[wv]*ufact[wv],psym=8,color=!navyblue
;        endif 

     endif 
     for i=0,n_elements(u)-1 do begin
        f=where(band eq bb[u[i]],nf)
        plots,time[f],flux[f],psym=4,color=colors[i]
        for j=0,n_elements(nf)-1 do begin
           oplot,[time[f[j]]-terr[f[j]],time[f[j]]+terr[f[j]]],[flux[f[j]],flux[f[j]]],color=colors[i]
           oplot,[time[f[j]],time[f[j]]],[flux[f[j]]-fluxerr[f[j]],flux[f[j]]+fluxerr[f[j]]],color=colors[i]
        endfor 
        ul=where(fluxerr[f] lt 0,nul)
        if nul gt 0 then plots,time[f[ul]],flux[f[ul]],psym=8,color=colors[i]
     endfor 
     cd,'..'
     oploterror,xtime,xfluxd,xfluxderr,psym=3,/nohat
     for p=0,n_elements(xtime)-1 do oplot,[xtime[p]+xterr0[p],[xtime[p]+xterr1[p]]],[xfluxd[p],xfluxd[p]]
     wul=where(xfluxerr eq 0,nwul)
     if nwul gt 0 then oplot,xtime[wul],xfluxd[wul],psym=8

     if g eq 1 then legend,[bb[u],'white','u','X-ray'],textcolor=[colors[0:n_elements(u)-1],!salmon,!darkred,!p.color],/top,/left,box=0,charsize=1.
     if g eq 0 then legend,[bb[u],'white','u','v','X-ray'],textcolor=[colors[0:n_elements(u)-1],!salmon,!darkred,!navyblue,!p.color],/top,/left,box=0,charsize=1.

  endfor 

  !p.multi=0
  if keyword_set(ps) then endplot
  stop
  return
end 
