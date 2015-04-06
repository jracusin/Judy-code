pro calc_jet_stuff,cr,nsig=nsig
  
  u=uniq(cr.grb)
  nuniq=n_elements(u)
  tags=tag_names(cr)
  
  w=where(tags eq 'WHO_EISO',nw)
  if nw eq 0 then begin 
     add_tag,cr,'WHO_EISO','',cr2
     cr=cr2
  endif 
  
;  w=where(tags eq 'EISO',nw)
;  if nw eq 0 then begin 
;     add_tag,cr,'EISO',0.,cr2
;     cr=cr2
;  endif 
  
;  read_butler_eiso,bgrb,beiso,beisoerr,bz
;  read_liang_eiso,lgrb,leiso,lz
;  nliang=n_elements(lgrb)
;  match,lgrb,bgrb,m1,m2
;  grb=lgrb[m1]
;  z=lz[m1]
;  eiso=leiso[m1]
;  eta=replicate(1.,n_elements(m1))
;  w=where(bz gt 0)
;  dont_match,lgrb,bgrb[w],m1,m2
;  nbut=n_elements(m2)
;  grb=[grb,bgrb[w[m2]]]
;  z=[z,bz[w[m2]]]
;  eiso=[eiso,beiso[w[m2]]]
;  eta=[eta,replicate(0.05,n_elements(m2))]
;  who=[replicate('liang',nliang),replicate('butler',nbut)]
;  eta=1. ;;remove eta assumption because it's incorporated into Ekiso
  ;;;need to use Ek when available Eiso when not
  
  ;;get z from GSFC database
;  file='~/jetbreaks/grb_zs_0707.csv'
;  readcol,file,grb,tid,z,format='(a,a,f)',delim=','
;  match,strtrim(cr.grb,2),'GRB'+grb,m1,m2
;  cr[m1].z=z[m2]
  
  ;;calc Eiso from Taka's paper and assumptions
;  file='~/jetbreaks/sakamoto_data.csv'
;  readcol,file,grb,z,alphapl,alphaplerr,epeak,epeakerrp,epeakerrn,fluence,fluerr,obslow,obshigh,alphab,alphaberr,betab,betaberr,format='(a,f,f,f,f,f,f,f,f,f,f,f,f,f,f)';,delim=','
;  readcol,file,line,delim='|',format='(a)',skip=1
;  nl=n_elements(line)
;  grb=strarr(nl) & z=fltarr(nl) & alphapl=z & epeak=z & fluence=z 
;  obslow=z & obshigh=z & alphab=z & betab=z & eiso=dblarr(nl)
;  for i=0,n_elements(line)-1 do begin
;     sep=str_sep(line[i],',')
;     grb[i]=sep[0]
;     z[i]=sep[1]
;     alphapl[i]=sep[2]
;     epeak[i]=sep[4]
;     fluence[i]=sep[7]
;     obslow[i]=sep[9]
;     obshigh[i]=sep[10]
;     if n_elements(sep) gt 11 then begin 
;        alphab[i]=sep[11]
;        betab[i]=sep[13]
;     endif 
;  endfor 
  
;  for i=0,nl-1 do begin 
;     obsflu=fluence[i]*1e-8
;     obsband=[obslow[i],obshigh[i]]
;     
;     eiso[i]=alog10(calc_eiso_band(z[i],alphapl[i],obsflu,obsband=obsband,alpha=alphab[i],beta=betab[i],epeak=epeak[i]))
;  endfor 
  
;  read_butler_eiso,bgrb,beiso,beisoerr,bz
;  match,bgrb,grb,m1,m2
;  plot,alog10(beiso[m1]*1d52),eiso[m2],psym=1,/yno,/iso
;  oplot,[0,100],[0,100]
  
;  match,strtrim(cr.grb,2),'GRB'+grb,m1,m2
;  cr[m1].z=z[m2]
;  cr[m1].eiso=eiso[m2]
  
  grbstr=mrdfits(!mdata+'grb_info_z_epeak.fits',1)
;  match,strtrim(cr.grb,2),strtrim('GRB'+grbstr.grb),m1,m2
  
  q=0
  day=86400.
  for i=0,nuniq-1 do begin
     w=where(cr.grb eq cr[u[i]].grb,nw)
     wz=where('GRB'+strtrim(grbstr.grb,2) eq strtrim(cr[u[i]].grb,2),nwz)
     if nwz gt 0 then begin
        cr[w].z=grbstr[wz].z
        cr[w].eiso=grbstr[wz].eiso/1d52
        if grbstr[wz].shb eq 1 then cr[w].who_eiso='shb'
;        etas=eta[wz[0]]
;        cr[w].who_eiso=who[wz[0]]
     endif 
     
     
     j=w[nw-1]
     q=[q,j]
     zz=cr[j].z
;     if zz gt 0 then begin 
;        if zz eq 0. then zz=2.5
     if zz eq 0. then begin
        zz=2.3 ;;mean of long bursts
        if cr[w[0]].who_eiso eq 'shb' then zz=0.4 ;;;mean of SHB
     endif 
        
     eisos=cr[j].eiso*1d52
     if eisos eq 0. then eisos=3.7d52;1d53 ;;median of sample
     for k=0,nw-1 do $
        cr[w[k]].theta=jet_angle(cr[w[k]].tbreak/day,z=zz,eiso=eisos,xi=xi) ;,eta=etas)
;        print,zz,xi
     
     cr[w].theta_tstart=jet_angle(cr[j].tstart/day,z=zz,eiso=eisos) ;,eta=etas)
     cr[w].theta_stop=jet_angle(cr[j].tstop/day,z=zz,eiso=eisos)    ;,eta=etas)
     cr[w].theta_lastdet=jet_angle(cr[j].tlastdet/day,z=zz,eiso=eisos)    ;,eta=etas)
     cr[w].theta_lastpos=jet_angle(cr[j].tlastpos/day,z=zz,eiso=eisos)    ;,eta=etas)
     if cr[j].theta eq 0 and cr[j].seg0 ne 1 and nw gt 1 then stop
;     endif 
  endfor 
;  q=q[1:*]
;  w=where(cr[q].theta gt 0.)
;  wz=where(cr[q[w]].z gt 0.)
  
;  wnoz=where(cr[q[w]].z eq 0.)
;  !p.multi=[0,1,2]
;  plothist,[-1.,cr[q[w]].theta],xrange=[0,50]
;  plothist,[-1.,cr[q[w[wz]]].theta],/over,color=!red,xrange=[0,50]
;  plothist,[-1.,cr[q[w[wnoz]]].theta],/over,color=!blue,xrange=[0,50]
;  kstwop,cr[q[w[wz]]].theta,cr[q[w[wnoz]]].theta,d,prob,/plot
;  legend,['d='+sigfig(d,3),'prob='+sigfig(prob,3)],/bottom,/right,box=0
;  help,uniq(cr[w].grb)
  
  
  !p.multi=0
  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  mwrfits,cr,!mdata+'closure_relations_total'+signame+'.fits',/create
  
;  stop
  
  
  
  return
end 

;;;currently comparing all last breaks for those with z and without z
;;;running with jetbreak subset, can do comparison for only those with jet breaks
;;;NEED to do for limiting case of last data
