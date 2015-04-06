@fit_functions
pro neil_11flux,grb,flux,fluxerr,pow,powerr1,powerr2,nh,nherr1,nherr2,shb=shb
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  hrtt=3600.*11.  ;;11 hours in seconds
  tt='11 hr'
;  tt='3 ks'
;  hrtt=3000.
  
;  if not keyword_set(shb) then readcol,'~/stuff_for_people/neil/neil11.txt',grb,format='(a)' else $
;     readcol,'~/stuff_for_people/neil/neil11_shb.txt',grb,format='(a)'
;  match,dir,'GRB'+grb,m1,m2
;  dir=dir[m1]
  grb=dir
  ndir=n_elements(dir)
  
  lcfile='lc_fit_out_idl_int5.dat'
  fluxtt=dblarr(ndir) & cts=fluxtt & fluxfact=fluxtt & fluxerr=fluxtt & pow=fluxtt & powerr=dblarr(2,ndir) & rate=fluxtt & model_rate=fluxtt
  ul=strarr(ndir) & nh=fluxtt & nherr1=nh & nherr2=nh
  for i=0,ndir-1 do begin 
     cd,strtrim(dir[i],2)
     if exist(lcfile) then begin
        if numlines(lcfile) gt 1 then begin 
           print,dir[i]
           lc=lcout2fits(/phil)
           read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks,lc=lcstr
           wdet=where(lc.src_rate_err gt 0.)
           wnodet=where(lc.src_rate_err lt 0.,nwnodet)
           tstop=max(lc.tstop)
           tdetstop=max(lc[wdet].tstop)
           tdetstart=min(lc[wdet].tstart)
           if nwnodet gt 0 then begin 
              tnodetstop=max(lc[wnodet].tstop)
              tnodetstart=max(lc[wnodet].tstart)
           endif else begin
              tnodetstop=0. & tnodetstart=0.
           endelse 
;           if tstop gt hrtt then begin 
           if tstop gt 0. then begin 
              ul[i]=''; else ul[i]='   extrap '
           
              case breaks of
                 0: begin 
                    j=0
                    cts[i]=pow(hrtt,p)
                 end 
                 1: begin 
                    if hrtt lt lcstr.break1 then j=0 else j=1
                    cts[i]=bknpow(hrtt,p)
                 end 
                 2: begin 
                    if hrtt lt lcstr.break1 then j=0 
                    if hrtt gt lcstr.break1 and hrtt lt lcstr.break2 then j=1
                    if hrtt gt lcstr.break2 then j=2
                    cts[i]=bkn2pow(hrtt,p)
                 end 
                 3: begin 
                    if hrtt lt lcstr.break1 then j=0 
                    if hrtt gt lcstr.break1 and hrtt lt lcstr.break2 then j=1
                    if hrtt gt lcstr.break2 and hrtt lt lcstr.break3 then j=2
                    if hrtt gt lcstr.break3 then j=3
                    cts[i]=bkn3pow(hrtt,p)
                 end 
              endcase 
              
              if exist('spec') then begin 
                 read_specfit,specstr,dir='spec/' 
                 goodspec=1
                 if specstr[j].pow eq 0. then goodspec=0
              endif else goodspec=0
              
print,tnodetstart/3600.
              if tdetstop lt tnodetstop and tnodetstart lt hrtt and tnodetstop gt hrtt then begin
                 print,tstop/3600.,max(lc[wdet].tstop)/3600.
                 cts[i]=lc[n_elements(lc)-1].src_rate
                 uplim=1
                 ul[i]=ul[i]+' UL '

              endif else uplim=0
              
              if tstop lt hrtt then $
                 ul[i]=ul[i]+' <'+tt+' extrap'

;              if n_elements(specstr) gt j then begin 
              if goodspec then begin
;                 print,grb[i],specstr[j].dof
;                 if specstr[j].dof lt 5 and j gt 0 then j=j-1
;                 fluxfact[i]=specstr[j].unabs_flux/specstr[j].rate
                 model_rate[i]=specstr[j].model_rate
                 rate[i]=specstr[j].rate
                 if model_rate[i] eq 0 then model_rate[i]=specstr[j].rate
                 fluxfact[i]=specstr[j].unabs_flux/model_rate[i]
                 fluxtt[i]=cts[i]*fluxfact[i]
                 nh[i]=specstr[j].nh
                 nherr1[i]=specstr[j].nh_err[0]
                 nherr2[i]=specstr[j].nh_err[1]
                 err=dblarr(n_elements(p))
                 for k=0,n_elements(p)-1 do begin 
                    errs=perror[*,k]/p[k]
;                    err[k]=mean(errs)
;                    err[k]=sqrt(total(errs^2))
                    if perror[0,k] eq p[k] then err[k]=errs[1] else $
                       err[k]=min(errs)
                 endfor 
                 err=err[j*2+1]
;                 if err gt 100 then err=0.5
                 ferr=specstr[j].flux_err
                 w0=where(ferr ne 0.,nw0)
;                 fmean=mean(ferr[w0])
;                 fmean=sqrt(total(ferr[w0]^2))
                 fmean=min(ferr[w0])
                 if not uplim then $
                    fluxerr[i]=(fmean/specstr[j].flux)
;                    fluxerr[i]=sqrt((fmean/specstr[j].flux)^2+err^2)*fluxtt[i]
                 if finite(fluxfact[i]) eq 0 or fluxfact[i] eq 0. then stop
                 pow[i]=specstr[j].pow
                 powerr[*,i]=specstr[j].pow_err
;                 if i eq 8 then stop
              endif else begin
;                 fluxfact[i]=0.
                 if keyword_set(shb) then fluxfact[i]=6.14d-11  else $;;for mean SHB
                    fluxfact[i]=5.62e-11                              ;;for mean Long GRBs
                 fluxtt[i]=cts[i]*fluxfact[i]
                 pow[i]=0
                 powerr[i]=0
                 if not uplim and tstop gt hrtt then begin 
                    if perror[0] ne 0. then begin 
                       err=dblarr(n_elements(p))
                       for k=0,n_elements(p)-1 do begin 
                          errs=perror[*,k]/p[k]
                          if perror[0,k] eq p[k] then err[k]=errs[1] else $
                             err[k]=min(errs)
;                          err[k]=sqrt(total(errs^2))
;                          err[k]=mean(errs)
                       endfor 
                       err=err[j*2+1]
;                       if err gt 100 then err=0.5
;                       fluxerr[i]=sqrt(err^2+0.2^2)*fluxtt[i]
                       fluxerr[i]=0.3
                    endif
                 endif 
                 ul[i]=ul[i]+' assume'
              endelse 
              if not uplim and tstop gt hrtt then fluxerr[i]=sqrt(fluxerr[i]^2+err^2)*fluxtt[i]
              if tstop lt hrtt then fluxerr[i]=sqrt(fluxerr[i]^2+err^2+0.2^2)*fluxtt[i]
;              if not uplim then fluxerr[i]=sqrt(fluxerr[i]^2+err^2)*fluxtt[i]


              if tnodetstart gt hrtt and tdetstop lt hrtt or tdetstart gt hrtt then begin
                 cts[i]=0
                 fluxtt[i]=0
                 fluxerr[i]=0
                 if tdetstop lt hrtt then ul[i]=ul[i]+' no UL near '+tt
                 if tdetstart gt hrtt then ul[i]=ul[i]+' Started after '+tt
              endif 
           endif else begin
              print,'Stop before '+tt
              print,sigfig(tstop/60.,3)+' min'
              ul[i]=ul[i]+' <'+tt
;              stop
           endelse 
        endif else begin 
           print,'No lc fit'
           ul[i]=ul[i]+' no LC fit'
        endelse 
     endif else begin
        print,'No lc file'
        ul[i]=ul[i]+' no LC fit'
     endelse 
     cd,'..'   
  endfor 

;  match,dir,'GRB'+grb,m1,m2
  m1=indgen(n_elements(dir))
  w=where(fluxtt[m1] eq 0. or finite(fluxtt[m1]) eq 0.,nw)
  flux=fluxtt
  if nw gt 0 then begin 
     flux[m1[w]]=0.
     fluxerr[m1[w]]=0.
  endif 
  powerr1=powerr[0,*]
;  w3=where(powerr[0,*] eq 0)
;  powerr1[w3]=0.
  powerr2=powerr[1,*]
;  w3=where(powerr[1,*] eq 0)
;  powerr1[w3]=0.
  
;  print,'GRB               flux        fluxerr       pow      powerr1   powerr2    nH       nHerr1    nHerr2     UL'
  fflux=sigfig(flux[m1],4,/sci)
  wf0=where(flux[m1] eq 0.)
  fflux[wf0]='         '
  ffluxerr=sigfig(fluxerr[m1],4,/sci)
  wf0=where(fluxerr[m1] eq 0.)
  ffluxerr[wf0]='         '
  s='    '
  wlen=where(strlen(dir[m1]) eq 9)
  ss=strarr(n_elements(m1))
  ss[wlen]=' '
;  colprint,dir[m1]+s+ss,fflux+s,ffluxerr+s,numdec(pow[m1],3)+s,numdec(powerr1[m1],3)+s,numdec(powerr2[m1],3)+s,numdec(nh[m1],3)+s,numdec(nherr1[m1],3)+s,numdec(nherr2[m1],3)+s,ul[m1]
;  print,'GRB               flux        fluxerr ' 
;  colprint,
  writecol,'flux11hour.txt',strsame(dir[m1],14)+strsame(fflux,14)+strsame(ffluxerr,14)+strsame(ul[m1],14),header=strsame('GRB',14)+strsame('flux',14)+strsame('fluxerr',14)
  
;  colprint,dir[m1]+s+fflux+s+ffluxerr+s+ul[m1]
;  ploterror,indgen(n_elements(grb)),flux[m1],fluxerr[m1],psym=1,/nohat,/ylog,yrange=[1e-15,1e-10]
  
  print
  print
  print

;  colprint,grb[m1]+s,sigfig(flux[m1],4,/sci)+s,sigfig(fluxerr[m1],4,/sci)+s,sigfig(pow[m1],3)+s,sigfig(powerr1[m1],3)+s,sigfig(powerr2[m1],3)+s,sigfig(nh[m1],4)+s,sigfig(nherr1[m1],4)+s,sigfig(nherr2[m1],4)+s,ul[m1]
  
  
  stop
  
  return
end 
