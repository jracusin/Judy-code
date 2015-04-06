function energy_angle,time,flux,z,nu=nu

  p=2.2 ;; don't have beta's!
  if n_elements(nu) eq 0 then nu=1.
  epse=0.2
  epsb=0.01
  t=86400. ;;1 day
  nuc=nu*1000.*8056.4*3e10 ;;; 1 kev*1000 ev/kev * 8056.4 cm-1/ev * 3e10 cm/s
  c1=1.4d-21 ;;cm^3/2
  c2=6.1d-5 ;;s^3/2 g^-1/2 cm^-1
  c3=6.9d39  ;;s^-3/2 g^-1/2 cm^-2
  dl=lumdist(z,h0=71,lambda=0.7)*1d6*3.08568025d18

  fd=flux2jy(flux,2.2)*1e-6
  y=c1*c3^0.5*c2^(-1.5)*epse^(-3.)*epsb^(-1.)*dl^(-2.)*nuc*time^2*flux^(-1.)
;  flux=fd
  eps=(p-2)/(p+2)
;  y=10d^(10*eps)
  fend=(c2*c3)^(-0.5)*c1^(-1.)*(dl^2.)/(1+z)*nuc*time*flux*y^eps
stop
  return,fend
end

pro lxtime,download=download
  
  readcol,'~/jetbreaks/grb_table_tid_z_05_09.txt',grb,tid,z,format='(a,a,a)'

  ngrb=n_elements(grb)

  cd,'~/GRBs'
  z=float(z)
  w=where(z ne 0,ngrb)
  grb=grb[w]
  tid=tid[w]
  z=z[w]

  if keyword_set(download) then begin 
     for i=0,ngrb-1 do begin
        dir='GRB'+grb[i]
        if not exist(dir) then spawn,'mkdir '+dir
        targid=str_sep(tid[i],'|')
        targid=targid[0]
;        zz=strsep(z[i],' ')
;        zz=zz[0]
        download_phil_lc,dir,tid=targid,/flux
     endfor 
  endif

;  yrange=[1d40,1d52]
  yrange=[1d64,1d72]
  plot,[1e1,1e7],yrange,/nodata,/xlog,/ylog,xtitle='Time (s)',ytitle='L!Lx!N (erg s!U-1!N)',xrange=[1e1,1e7],yrange=yrange,/xsty,/ysty
  for i=0,ngrb-1 do begin
     dir='GRB'+grb[i]
;     cd,dir
     if exist(dir+'/flux.qdp') then begin 
        readcol,dir+'/flux.qdp',time,terr1,terr0,flux,err
        dl=lumdist(z[i],h0=71,lambda=0.7)*1d6*3.08568025d18
        lx=4*!pi*dl^2*flux/(1.+z[i])  ;; cm2 erg/s/cm2
        lxerr=4*!pi*dl^2*err/(1.+z[i])
        
        fend=energy_angle(time,flux,z[i])
        fenderr=err/flux*fend

;        oploterror,time,lx,lxerr,psym=3,/nohat
;        for j=0,n_elements(lx)-1 do begin
;           oplot,[time[j]+terr0[j],time[j]+terr1[j]],[lx[j],lx[j]]
;           oplot,[time[j],time[j]],[lx[j]-lxerr[j],lx[j]+lxerr[j]]
;        endfor 
        oploterror,time,fend,fenderr,psym=3,/nohat
        for j=0,n_elements(fend)-1 do begin
           oplot,[time[j]+terr0[j],time[j]+terr1[j]],[fend[j],fend[j]]
           oplot,[time[j],time[j]],[fend[j]-fenderr[j],fend[j]+fenderr[j]]
        endfor 

     endif 
;     cd,'..'
  endfor 
stop
return
end 
