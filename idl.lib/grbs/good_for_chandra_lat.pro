@fit_functions
pro wrap_gfc,ps=ps

  cd,'~/GRBs/'
  grbs=file_search('GRB*/')
  ngrbs=n_elements(grbs)
  good=intarr(ngrbs)
  readcol,'~/Chandra/GRB_z.dat',sgrb,tid,t90,z,delim='|',format='(a,l,f,f)'
  sgrb=strcompress(sgrb,/remove)

  match,grbs,sgrb,m1,m2
  match,grbs+'A',sgrb,m1a,m2a
  m1=[m1,m1a]
  m2=[m2,m2a]
  s=sort(m1)
  m1=m1[s]
  m2=m2[s]
  
  wz=where(z[m2] ne 0 and t90[m2] gt 2.,ngrbs)
  grbs=grbs[m1[wz]]
  z=z[m2[wz]]
  t90=t90[m2[wz]]
  cf=dblarr(ngrbs)
  
  colprint,indgen(ngrbs),grbs
  print,'Start where?  g=?'
  g=0
  stop
  
  for i=g,ngrbs-1 do begin
     good[i]=good_for_chandra(grbs[i],z[i],cfratio=cfratio,ps=ps)
     cf[i]=cfratio
     if not keyword_set(ps) then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 
  wg=where(good eq 1)
  help,wg
  
  colprint,grbs[wg],z[wg],cf[wg]
stop
return
end 

function good_for_chandra_lat,grb,z,cfratio=cfratio,ps=ps,params=params,assumpmo=assumpmo,lcfit=lcfit

  ;;; before running - need to mkdir, download LC & SPEC, run fit_lc,
  ;;;                  add to GRB_z.dat
  ;;;need to input grb name, z
  ;;; reads in lc_fit_out_idl_int7.dat, UL_specfits.fits, lc_newout_phil.txt

;  cd,'~/GRBs/'
;  cd,grb
  
  if keyword_set(ps) then begplot,name=grb+'_Good_for_chandra_lat.ps',/land,/color
  good=0
  if not keyword_set(lcfit) then lcfit='lc_fit_out_idl_int9.dat'
  if not exist(lcfit) or not exist('UL_specfits.fits') then begin
     print,"can't find stuff" 
     return,good
  endif
;  if not exist(lcfit) then lcfit='lc_fit_out_idl_int7.dat'
  read_lcfit,lcfit,pnames,p,perror
  if n_elements(params) gt 0 then p=params
  np=n_elements(p)
  chandra=0
  if exist('UL_lc_chandra.fits') then chandra=1
  lc=lcout2fits(chandra=chandra)
  sfile='UL_specfits.fits'
  spec=mrdfits(sfile,1)
  gyr=strmid(grb,3,2)
  gmn=strmid(grb,5,2)
  gday=strmid(grb,7,2)
  gmet=date2met('20'+gyr+'-'+gmn+'-'+gday+'-00:00:00')
  days=round((today()-gmet)/86400.)
;  t=lc.time
  t=logarr(lc[0].tstart,max(lc.tstop),bin=0.1)
  ttd=days*86400d
  t15=ttd+15.*86400d
  t30=ttd+30.*86400d

  lastobs=max(lc.tstop)

;  case np of
;     0: return,good
;     2: mo='pow'
;     4: mo='bknpow'
;     6: mo='bkn2pow'
;     8: mo='bkn3pow'
;     10: mo='bkn4pow'
;  endcase 
  if n_elements(assumpmo) eq 0 then mo=fit_models(pnames,p,np,basemo=basemo) else mo=assumpmo
;  mo=basemo
  if np eq 4 then t=[t,p[2]]
  if np eq 6 then t=[t,p[2],p[4]]  
  if np eq 8 then t=[t,p[2],p[4],p[6]]
  if np eq 10 then t=[t,p[2],p[4],p[6],p[8]]
  t=[t,ttd,1e8]
  t=t[sort(t)]
  nt=n_elements(t)
  pmax=p[np-1]
  if n_elements(perror[0,*]) eq n_elements(p) then pmaxerr=perror[*,np-1] else pmaxerr=[0,0]
  if np ge 4 then $
     tmp=execute('nmax='+mo+'(p[np-2],p)/pow(p[np-2],[1.,pmax])') else nmax=p[0]

  cfratio=spec[n_elements(spec)-1].cfratio
  tmp=execute('f='+mo+'(t,p)*cfratio')
  tmp=execute('f15='+mo+'(t15,p)*cfratio')
  tmp=execute('f30='+mo+'(t30,p)*cfratio')
  tmp=execute('ftd='+mo+'(ttd,p)*cfratio')
  
  good=0
  print,f15,f30
  if (f15 ge 1e-14 and f30 ge 3e-15) or keyword_set(ps) then begin 
     good=1
     
;     plot,t,f,/xlog,/ylog,xrange=[10,1e8],yrange=[1d-16,1e-8],/xsty,/ysty
     xrange=[10,1e8]
     yrange=[1e-16,1e-5]
     plot,xrange,yrange,/xlog,/ylog,/nodata,xtitle='Time Since BAT Trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',title=grb+' (z='+ntostr(z,4)+')',xrange=xrange,yrange=yrange,/xsty,/ysty,xminor=9,yminor=9,yticks=11;v=10^(findgen(12)-16)
     oploterror,lc.time,lc.src_rate*cfratio,lc.src_rate_err*cfratio,psym=3,/nohat
     ul=where(lc.src_rate_err eq 0,nul)
     plotsym,1,3,thick=5
     if nul gt 0 then plots,lc[ul].time,lc[ul].src_rate*cfratio,psym=8
     for i=0,n_elements(lc)-1 do oplot,[lc[i].tstart,lc[i].tstop],[lc[i].src_rate,lc[i].src_rate]*cfratio
     oplot,t[0:nt-3],f[0:nt-3],color=!green
     oplot,t,f,color=!green,line=2

     oplot,[t15,t15],yrange,line=1
     oplot,[10,1e8],[f15,f15],line=1
     oplot,[t30,t30],yrange,line=1,color=!orange
     oplot,[10,1e8],[f30,f30],line=1,color=!orange
     if ttd lt 1e8 then begin 
        oplot,[ttd,ttd],yrange,line=1,color=!red
        oplot,[10,1e8],[ftd,ftd],line=1,color=!red
     endif 
     oplot,[10,1e8],[1e-15,1e-15],line=3,color=!magenta
     oplot,[10,1e8],[5e-15,5e-15],line=2,color=!cyan
;  legend,['10 days','30
;  days','Today'],textcolor=[!p.color,!blue,!red],box=0,/center,/left
     xyouts,t15*1.1,1d-10,'+15 days'
     xyouts,t30*1.1,1d-11,'+30 days',color=!orange
     if ttd lt 1e8 then xyouts,ttd*1.1,1d-9,'Today',color=!red
;     xyouts,20,8.e-15,'XRT limit @ >7 days'
;     xyouts,20,1.5e-15,'Chandra limit @ >30 days'
;     oplot,t,pow(t,[nmax,pmax])*cfratio,color=!purple,line=2
     nml=round(alog10(nmax*cfratio)-0.5)
     nm=nmax*cfratio/10.^nml
     if nml ne 0 then nmp=ntostr(nm,4)+'x10!U'+ntostr(fix(nml))+'!N' else nmp=ntostr(nm,4)
;;     tlim=(3.e-15/(nmax*cfratio))^(-1./pmax)
     tlim=(5d-15/(nmax*cfratio))^(-1./pmax)
     tlim2=(1d-15/(nmax*cfratio))^(-1./pmax)

     legend,['f(15d)='+numdec(f15,2,/sci,/idlplot),'f(30d)='+numdec(f30,2,/sci,/idlplot),'T(f=5x10!U-15!N)='+numdec(tlim/86400.,1)+' days','T(f=1x10!U-15!N)='+numdec(tlim2/86400.,1)+' days'],line=[1,1,2,3],color=[!p.color,!orange,!cyan,!magenta],/left,/center,box=0,position=[20,3e-6]

     legend,['Final Slope','Norm = '+nmp,!tsym.alpha+' = '+numdec(pmax,2)+' (-'+numdec(pmaxerr[0],2)+',+'+numdec(pmaxerr[1],2)+')'],box=0,/top,/right
;     legend,['T-T0 (f=5x10!U-15!N)='+numdec(tlim/1e6,2)+'x10!U6!N s = '+numdec(tlim/86400.,1)+' days'],/top,/left,box=0

  endif 

  print,grb
  print,'Last Observation @ '+ntostr(lastobs/86400.)+' days'
  print,'Final Decay index = '+ntostr(p[np-1])
  print,'Flux @ 15 days = '+ntostr(f15)+' erg cm-2 s-1'
  print,'Flux @ 30 days = '+ntostr(f30)+' erg cm-2 s-1'
  print,'Flux Today @ '+ntostr(days)+' days = '+ntostr(ftd)+' erg cm-2 s-1'
  print,'Today is '+ntostr(days)+' since the burst'

  if good then $
     print,'Good for Chandra @ 30 days!  z='+ntostr(z) $
  else print,'Not good for Chandra follow-up'
;  cd,'..'
  if keyword_set(ps) then begin
     endplot
     spawn,'ps2pdf '+grb+'_Good_for_chandra_lat.ps'
  endif 
stop
  return,good
end 
