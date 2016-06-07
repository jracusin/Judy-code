pro max_1e5s
  
  g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  w=where(g.tstart lt 1e5 and g.tlastdet gt 1e5,nw)
  g=g[w]
  
  f=dblarr(nw)
  for i=0,nw-1 do begin
     f[i]=call_function(strtrim(g[i].basemodel,2),1e5,g[i].p)*g[i].cfratio[n_elements(g[i].cfratio)-1]

  endfor 
  w=where(f ne 0 and f lt 1e8)
  f=f[w]
  g=g[w]

  begplot,name='~/stuff_for_people/Max/grb_flux_1e5s.ps',/landscape,/color
  plotloghist,f,xtitle='0.3-10 keV Flux (erg cm!U-2!Ns!U-1!N)',ytitle='N',xrange=[1e-15,1e-10],bin=0.1,charsize=2.
  print,median(f)
  oplot,[median(f),median(f)],[0,50],color=!red,line=2
  legend,['Median Flux = '+numdec(median(f),1,/idl,/sci)],box=0,/top,/left,textcolor=!red

  endplot
  ps2pdf,'~/stuff_for_people/Max/grb_flux_1e5s.ps'

stop
  return
end 

pro collect_grb_properties,grb

  cd,'~/GRBs'
  
  grbs=file_search('GRB*')
  ngrbs=n_elements(grbs)

  grb=create_struct('GRB','','trigtime',0d,'z',0.,'nflares',0,'np',0,'type','','xjb',0,$
                    'ndet',0,'tstart',0d,'tstop',0d,'tlastdet',0d,'t90',0.,'fluence',0.,$
                    'pnames',strarr(30),'p',fltarr(30),'perror',fltarr(2,30),$
                    'model','','basemodel','',$
                    'gbmname','','unabs_cfratio',0.,'cfratio',0.,'phind',0.)
  grb=replicate(grb,ngrbs)

  for i=0,ngrbs-1 do begin
     print,grbs[i]
     lcfitfile=grbs[i]+'/lc_fit_out_idl_int9.dat'
;     if not exist(lcfitfile) then lcfitfile=grbs[i]+'/lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin
        read_lcfit,lcfitfile,pnames,p,perror
        specfit='~/GRBs/'+strtrim(grbs[i])+'/UL_specfits.fits'
        if exist(specfit) then begin 
           spec=mrdfits(specfit,1)
           grb[i].unabs_cfratio=spec[n_elements(spec)-1].unabs_cfratio
           grb[i].cfratio=spec[n_elements(spec)-1].cfratio
           grb[i].phind=spec[n_elements(spec)-1].phind
        endif 
        if n_elements(pnames) gt 1 then begin 
           grb[i].p[0:n_elements(p)-1]=p
           grb[i].pnames[0:n_elements(p)-1]=pnames
           grb[i].perror[*,0:n_elements(p)-1]=perror
           mo=fit_models(pnames,p,np,nf,basemo=basemo)
           lc=lcout2fits(dir=grbs[i]);grbs[i]+'/lc_newout_phil.txt')
           grb[i].tstart=lc[0].tstart
           grb[i].tstop=lc[n_elements(lc)-1].tstop
           wdet=where(lc.src_rate_err ne 0,ndet)
           grb[i].ndet=ndet
           grb[i].tlastdet=lc[wdet[ndet-1]].time
           if strtrim(pnames[0],2) ne 'nofit' then begin 
              grb[i].model=mo
              grb[i].basemodel=basemo
              grb[i].nflares=nf
              grb[i].np=np
              grb[i].grb=grbs[i]
              case np of 
                 2: begin 
                    grb[i].type='SPL'
                 end
                 4: begin 
                    if p[1] gt p[3] then grb[i].type='I-II'
                    if p[1] lt p[3] then grb[i].type='II-III'
                 end
                 6: begin
                    if p[1] gt p[3] then grb[i].type='I-II-III'
                    if p[1] lt p[3] then grb[i].type='II-III-IV'
                 end
                 8: begin 
                    grb[i].type='I-II-III-IV'
                 end
                 10: begin
                    if p[1] lt p[3] then grb[i].type='0-I-II-III-IV'
                    if p[1] gt p[3] then grb[i].type='I-II-III-IV-V'
                 end 
              endcase 

              if grb[i].type eq 'I-II-III' or grb[i].type eq 'II-III' then begin 
                 alp=[0.28,1.09,1.89]
                 if np eq 6 then begin
                    a0=p[3]
                    a1=p[5]
                    a0err=perror[*,3]
                    a1err=perror[*,5]
                 endif 
                 if np eq 4 then begin
                    a0=p[1]
                    a1=p[3]
                    a0err=perror[*,1]
                    a1err=perror[*,3]
                 endif 
                                ;;; first seg diff II
                 if a0-alp[0] lt 0 then a02rr=a0err[1] else a02rr=a0err[0]

                                ;;; first seg diff III
                 if a0-alp[1] lt 0 then a03rr=a0err[1] else a03rr=a0err[0]

                                ;;; second seg diff III
                 if a1-alp[1] lt 0 then a13rr=a1err[1] else a13rr=a1err[0]

                                ;;; second seg diff IV
                 if a1-alp[2] lt 0 then a14rr=a1err[1] else a14rr=a1err[0]

                 d23=sqrt(((a0-alp[0])/a02rr)^2+((a1-alp[1])/a13rr)^2)
                 d24=sqrt(((a0-alp[0])/a02rr)^2+((a1-alp[2])/a14rr)^2)
                 d34=sqrt(((a0-alp[1])/a03rr)^2+((a1-alp[2])/a14rr)^2)

                 m=min([d23,d24,d34],wm)
                 
                 if wm eq 0 then type=23
                 if wm eq 1 then type=24
                 if wm eq 2 then type=34

                 if np eq 6 and type eq 24 then begin 
                    grb[i].type='I-II-IV'
                    grb[i].xjb=1
                 endif 
                 if np eq 6 and type eq 34 then begin 
                    grb[i].type='I-III-IV'
                    grb[i].xjb=1
                 endif 

;        if type eq 23 then begin 
;        endif 
                 if np eq 4 and (type eq 24 or type eq 34) then begin
                    grb[i].xjb=1
                    if type eq 24 then grb[i].type='II-IV'
                    if type eq 34 then begin 
                       grb[i].type='III-IV'
                    endif 
                 endif 
              endif 
              last_alpha=p[np-1]
              if grb[i].type eq 'SPL' and last_alpha gt 1.5 then begin
                 grb[i].xjb=2
              endif 
              if np ge 8 and strpos(grb[i].type,'-IV') ne -1 then grb[i].xjb=1

           endif
        endif
     endif 
  endfor 

;  sgrb=mrdfits('~/Swift/swiftgrb.fits',1)
  ;; only good through end of 2012

;  sgrb=mrdfits('~/Swift/jochen_z_list.fits',1)

;  match,strtrim(sgrb.grb,2),strtrim(grb.grb,2),m1,m2
;  grb[m2].z=sgrb[m1].z;redshift

  collect_grb_z,grb

  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  match,strtrim(bat.grb,2),strtrim(grb.grb,2),m1,m2
  grb[m2].t90=bat[m1].t90
  grb[m2].fluence=bat[m1].sbat_15_150
  grb[m2].trigtime=bat[m1].trigtime

  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  match,strtrim(bat.grb,2)+'A',strtrim(grb.grb,2),m1,m2
  if m2[0] ne -1 then begin 
     grb[m2].t90=bat[m1].t90
     grb[m2].fluence=bat[m1].sbat_15_150
     grb[m2].trigtime=bat[m1].trigtime
  endif 

  bat=mrdfits('~/jetbreaks/batcat.fits',1)
  match,strtrim(bat.grb,2),strtrim(grb.grb,2)+'A',m1,m2
  if m2[0] ne -1 then begin 
     grb[m2].t90=bat[m1].t90
     grb[m2].fluence=bat[m1].sbat_15_150
     grb[m2].trigtime=bat[m1].trigtime
  endif 
  ;;; need to catch GBM bursts that Swift caught later (not
  ;;; co-triggered) = LAT bursts, iPTF bursts.  collect manually?

  extragrbs=['GRB080916C','GRB090323','GRB090328A','GRB090531B','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB120624B','GRB120711A','GRB130305A','GRB130502B','GRB130504C','GRB130518A','GRB130606B','GRB130702A','GRB130907A','GRB131014A','GRB131108A','GRB131231A','GRB140102A','GRB140104B','GRB140323A','GRB140508A','GRB140606B','GRB140620A','GRB140623A']
  extragbmname=['GRB080916009','GRB090323002','GRB090328401','GRB090531775','GRB090902462','GRB090926181','GRB091003191','GRB100414097','GRB120624933','GRB120711115','GRB130305486','GRB130502327','GRB130504978','GRB130518580','GRB130606497','GRB130702004','GRB130907904','GRB131014215','GRB131108862','GRB131231198','GRB140102887','GRB140104731','GRB140323433','GRB140508128','GRB140606133','GRB140620219','GRB140623224']
  match,strtrim(grb.grb,2),strtrim(extragrbs,2),m1,m2
  grb[m1].gbmname=extragbmname[m2]

  w=where(grb.grb ne '',nw)
  grb=grb[w]

;;; match GBM
  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  s=sort(gbm.name)
  gbm=gbm[s]
  ng=n_elements(gbm)
  for i=0,ng-1 do begin 
;     v=date_conv(gbm[i].trigger_time+2400000.5d,'V')
;     utc=ntostr(v[0],4)+'-'+ntostr(fix(v[1]))+'-'+ntostr(v[2],2)+':'+ntostr(v[3],2)+':'+ntostr(v[4],4)
     utc=strsplit(gbm[i].trigger_time,' ',/ex)
     utc=utc[0]+'-'+utc[1]
     met=date2met(utc)
     gbm[i].trigger_time=met
  endfor 

  for i=0,nw-1 do begin
     d=abs(grb[i].trigtime-gbm.trigger_time)
     m=min(d,wm)
     if d[wm] lt 60 then grb[i].gbmname=gbm[wm].name
  endfor 

  wg=where(strtrim(grb.gbmname,2) ne '' and grb.trigtime eq 0.,nwg)
  match,strtrim(grb[wg].gbmname,2),strtrim(gbm.name,2),m1,m2
  grb[wg[m1]].trigtime=gbm[m2].trigger_time

  mwrfits,grb,'~/Swift/swift_grb_properties.fits',/create

  stop
  return
end 
  
