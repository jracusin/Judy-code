@fit_functions
pro twocomp_results,noconsis=noconsis,ps=ps
  
  cd,!mdata
  cand=file_search('GRB*/twocomp/')
  spos=strpos(cand,'/')
  nc=n_elements(cand)
  for i=0,nc-1 do cand[i]=strmid(cand[i],0,spos[i])
  print,cand
  
  g=0
  stop
  openw,flun,'twocomp_table.tex',/get_lun
  printf,flun,'\colhead{GRB} & \colhead{Canonical} & \colhead{2-Comp} & \colhead{$t_{b,NJ}$} & \colhead{$t_{b,WJ}$} & \colhead{$E_{\gamma,iso}$} \\ '
  printf,flun,'\colhead{} & \colhead{class} & \colhead{class} & \colhead{(s)} & \colhead{(ks)} & \colhead{(erg)} \\'
  for i=g,nc-2 do begin
     print,i,cand[i]
     twocomp,cand[i],/skiplc,/skipspec,noconsis=noconsis,flun=flun,ps=ps
  endfor 
  
  close,flun
  free_lun,flun
  return
end 

pro find_twocomp_candidates,cand

  ;;;;shallow-steep-shallow-steep profile
  ;;;;phase II is alpha ~1
  ;;;;shallow-steep-shallow
  ;;;;steep I - shallow II (alpha~1)? though neither JB identifed
  cd,!mdata
  cr=mrdfits('closure_relations_total_2sig.fits',1)
  w123=where(cr.class eq 123,n123)
  w24=where(cr.class eq 24)
  w34=where(cr.class eq 34)
  w12=where(cr.class eq 12)
  latebr=0
  ;;; find shallow-steep-shallow-steep profile
  ;;; I-II-II with previous emission?
  cand=''
  g=uniq(cr[w123].grb)
  ng=n_elements(g)
  for i=0,ng-1 do begin 
;     w=where(cr.grb eq cr[w123[g[i]]].grb)
     grb=strtrim(cr[w123[g[i]]].grb,2)
     cd,grb
     ffile='flares_gtis.dat'
     if exist(ffile) then begin 
        read_lcfit,'lc_fit_out_idl_int9.dat',pname,p
        readcol,ffile,start,stop,format='(f,f)',/silent
        if stop[0] lt p[2] and n_elements(stop) eq 1 then begin
           lc=lcout2fits(/phil)
           wlc=where(lc.time lt stop[0],nlc)
           wlc2=where(bkn2pow(lc[wlc].time,p)-lc[wlc].src_rate gt 0,nwlc)
           print,nwlc,nlc,nwlc*1./nlc*1.
           if nwlc*1./nlc*1. gt 0.8 then cand=[cand,grb]
        endif 
     endif 
     cd,'..'
  endfor 
  cand=cand[1:*]
  print,cand
  
  ;;; phase II is alpha~1
  ww=[w123];,w24,w34]
  wtc2=where(cr[ww].seg2 eq 1 and cr[ww].alpha gt 0.8 and cr[ww].alpha lt 1.2)
  cand=[cand,cr[ww[wtc2]].grb]
  print,cr[ww[wtc2]].grb
  nc=n_elements(cand)
  
  ;;;;shallow-steep-shallow
  g=uniq(cr[w12].grb)
  ng=n_elements(g)
  for i=0,ng-1 do begin 
;     w=where(cr.grb eq cr[w123[g[i]]].grb)
     grb=strtrim(cr[w12[g[i]]].grb,2)
     cd,grb
     ffile='flares_gtis.dat'
     if exist(ffile) then begin 
        read_lcfit,'lc_fit_out_idl_int9.dat',pname,p
        readcol,ffile,start,stop,format='(f,f)'
        if stop[0] lt p[2] then begin
           lc=lcout2fits(/phil)
           wlc=where(lc.time lt stop[0],nlc)
           wlc2=where(bknpow(lc[wlc].time,p)-lc[wlc].src_rate gt 0,nwlc)
           print,nwlc,nlc,nwlc*1./nlc*1.
           if nwlc*1./nlc*1. gt 0.8 then cand=[cand,grb]
        endif 
     endif 
     cd,'..'
  endfor 
  print,cand[nc:*]
  nc=n_elements(cand)
  
  ;;;;steep I - shallow II (alpha~1)? though neither JB identifed
  w=where(cr[w12].seg2 eq 1 and cr[w12].alpha gt 0.8 and cr[ww].alpha lt 1.2)
  cand=[cand,cr[w12[w]].grb]
  print,cand[nc:*]
  print
  print
  cand=cand[sort(cand)]
  print,cand
  
  stop
return
end 

pro twocomp,grb,skiplc=skiplc,skipspec=skipspec,title=title,wtreg=wtreg,z=z,noconsis=noconsis,flun=flun,ps=ps
  !p.multi=0
  multiplot2,/reset,/default
  cd,!mdata+grb
       
  file='lc_newout_phil.txt'
  if not exist(file) then convert_phil2lcout
  if not exist('twocomp') then begin 
     spawn,'mkdir twocomp'
     spawn,'cp lc_fit_out_idl_int9.dat twocomp/'
     spawn,'cp lc_newout_phil.txt twocomp/'
     spawn,'cp lc_wrap3.par twocomp/'
  endif 
  
  cd,'twocomp'
;  if keyword_set(ps) then begplot,name=grb+'_twocomp_model_fits.eps',/encap
  name=grb+'_twocomp_model_fits.ps'

  print,grb
  read_lcfit,'lc_fit_out_idl_int9.dat',pname,oldp,perror
  file='lc_newout_phil.txt'
  oldlc=lcout2fits(/phil)
  int='1'

  
  if not keyword_set(skiplc) then begin 
  ;;;REMOVE FLARES, BUT MAYBE DIFFERENT THAN OLD FLARES
     plot_like_qdp,file=file
     newfile='lc_newout_noflares2.txt'
     ans='n'
     if exist(newfile) then begin
        print,'Use existing noflares filter? (y/n)'
        ans=get_kbrd(10)
     endif
     if ans eq 'y' then lc=lcout2fits(newfile,qdp=qdp)
     if ans eq 'n' then begin 
        print,'remove flares or p to skip? (y/n/p)'
        fl=get_kbrd(10)
        fit_noflares_again:
        if fl eq 'y' then begin
           fit_noflares2,file,newfile,/small,lc=lc,qdp=qdp,_extra=_extra,title=name,gtifile='flares_gtis2.dat'
        endif else begin 
           lc=oldlc
           newfile=file
           if exist(newfile) and newfile eq 'lc_newout_noflares2.txt' then begin 
              spawn,'rm '+newfile
              spawn,'rm flares_gtis2.dat'
           endif 
        endelse 
        if fl eq 's' then stop
     endif else fl=''
     lc=lcout2fits(newfile)
     print,newfile
     plot_like_qdp,file=newfile;,lc=lc
     print,'Flare removal (or not) is sufficient? (y/n)'
     ag=get_kbrd(10)
     if ag eq 'n' then begin
        fl='y'
        goto,fit_noflares_again
     endif 
     if ag eq 's' then stop
     
     doagain:
     plot_like_qdp,file=newfile
  ;;;USER INPUT TO GUESS PROFILE
     bt=0d
     !mouse.button=0

     while (!MOUSE.button NE 4) do begin
        
        print,'Click on estimate breaktime, or right click to continue'
        cursor,xxx,yyy,/wait,/change
        if !mouse.button ne 4 then begin 
           oplot,[xxx,xxx],[1e-15,1e4],color=!orange
           print,round(xxx)
           bt=[bt,xxx]
        endif
     endwhile
     breaks=n_elements(bt)-1
     if breaks gt 0 then bt=round(bt[1:*])
     
     time=lc.time
     err=lc.src_rate_err
     cts=lc.src_rate
     slope=0d
     mintime=ntostr(min(time))
     maxtime=ntostr(max(time))
     if breaks ge 1 then begin 
        for i=0,breaks do begin 
           if i lt breaks then begin
              if i ne 0 then time1=ntostr(bt[i-1]) else time1=mintime
              wtime2=ntostr(bt[i])
              if i lt breaks-1 then time2=ntostr(bt[i+1]) else time2=maxtime
           endif else begin 
              if breaks gt 0 then time1=ntostr(bt[i-1]) else time1=mintime
              wtime2=maxtime
           endelse 
           w=where(time ge time1-10. and time le wtime2+10. and err ne 0,nw)
           
           mw=max(w)+1
           w2=w
           if mw lt n_elements(time) then $
              w2=[w,mw]
        
           f=linfit(alog10(time[w]),alog10(cts[w]))
           oplot,time[w2],10^f[0]*time[w2]^f[1]
           print,time1,' ',wtime2
           sl=-f[1]
           slope=[slope,sl]
;      minslope=ntostr(sl-2)
;      maxslope=ntostr(sl+2)
           if i eq 0 then norm=10^f[0]*1^f[1]
        endfor 
        slope=slope[1:*]
     endif else norm=oldp[0]
     if not finite(norm) then norm=max(cts)
     
;     if breaks eq 1 then ppow=1 else ppow=0
;;;NEED TO ADD I-II CASE
     case breaks of
        0: begin
           pname=['norm1','alpha1','norm2','alpha2']
           p=[oldp[0],oldp[1],oldp[0]*0.1,oldp[3]]
           mo='pow_pow'
           intmo='int_pow_pow'
        end 
        1: begin
           print,'Early or Late JB? (e/l)?'
           k=get_kbrd(10)
           if k eq 'l' then begin 
              pname=['norm1','alpha1','norm2','alpha3','tb2','alpha4']
              p=[oldp[0],slope[0]+0.5,oldp[0]*1d-3,slope[0]-0.9,bt[0],slope[1]]
              latebr=1
           endif else begin 
              pname=['norm2','alpha3','norm1','alpha1','tb1','alpha2']
              p=[oldp[0]*1d-11,slope[1]-0.5,oldp[0]*1d-10,slope[0]-0.5,bt[0],slope[0]+0.5]
;              pname=['norm1','alpha1','tb1','alpha2','norm2','alpha4']
;              p=[oldp[0],slope[0],bt[0],slope[0]+0.9,oldp[0]*1d-3,slope[1]-0.5]
           endelse 
           mo='pow_bknpow'
           intmo='int_pow_bknpow'
        end 
        2: begin 
           pname=['norm1','alpha1','tb1','alpha2','norm2','alpha3','tb2','alpha4']
           p=[oldp[0],slope[0],bt[0],slope[1],oldp[0]*1d-3,0.5,bt[1],slope[2]]
           mo='double_bknpow'
           intmo='int_double_bknpow'   
        end 
     endcase 
     print,p
     np=n_elements(p)
     parinfo = parinfo_struct(np)
     parinfo.value=p
     parinfo.parname=pname
     parinfo[0].limits=[1e-5,0] ;;norm1
     parinfo[0].limited=[1,0]
     parinfo[1].limits=[0,0] ;;pow1
     parinfo[1].limited=[1,0]
;  parinfo[2].limits=[1000,0] ;;tb1
;  parinfo[2].limited=[1,0]
;  parinfo[3].limits=[1,0] ;;pow2
;  parinfo[3].limited=[1,0]
;  parinfo[4].limits=[0,0] ;;;norm2
;  parinfo[4].limited=[1,0]
;  parinfo[5].limits=[0,0] ;;pow3
;  parinfo[5].limited=[1,0]
;  parinfo[6].limits=[1e5,2e6] ;;tb2
;  parinfo[6].limited=[1,0]
;  parinfo[7].limits=[1.5,0] ;;pow4
;  parinfo[7].limited=[1,0]
     
     if breaks eq 2 then begin 
        parinfo[4].limits=[0,0]
        parinfo[4].limited=[1,0]
;     parinfo[5].limits=[-0.5,0.5]
;     parinfo[5].limited=[1,1]
        parinfo[6].limits=[bt[1]*0.1,bt[1]*3.]
        parinfo[6].limited=[1,1]
        parinfo[7].limits=[p[5],4]
        parinfo[7].limited=[1,1]
     endif 
     
     wn2=where(pname eq 'norm2',nwn2)
     if nwn2 gt 0 then begin 
        parinfo[wn2].limits=[1e-5,0]
        parinfo[wn2].limited=[1,0]
     endif 
     wn2=where(pname eq 'alpha3',nwn2)
     if nwn2 gt 0 then begin 
        parinfo[wn2].limits=[0,6]
        parinfo[wn2].limited=[1,1]
     endif 
     wn2=where(pname eq 'alpha4',nwn2)
     if nwn2 gt 0 then begin 
        parinfo[wn2].limits=[0,10]
        parinfo[wn2].limited=[1,1]
     endif 
     
     w=where(lc.src_rate_err gt 0,nw)
     tt=dblarr(2,nw)
     tt[0,*]=lc[w].tstart
     tt[1,*]=lc[w].tstop
     cts=lc[w].src_rate
     err=lc[w].src_rate_err
     exptime=lc[w].exptime
     corr_fact=lc[w].pu_corr
     srccounts=lc[w].src_counts
     backcounts=lc[w].tot_back_cts
     time=time[w]
     newp=mpfitfun(intmo,tt,cts,err,p,yfit=yfit,perror=perror,nprint=100,parinfo=parinfo,dof=dof)
     chisq=total(((yfit-cts)/err)^2)
     if breaks eq 1 then time=[time,newp[4]]
     if breaks eq 2 then time=[time,newp[2],newp[6]]
     time=time[sort(time)]
     tmp=execute('yfit='+mo+'(time,newp,f1,f2)')
     transtime=interpol(time,f1-f2,0.)
     print,transtime
     time=[time,transtime]
     time=time[sort(time)]
     tmp=execute('yfit='+mo+'(time,newp,f1,f2)')
     
     plot_like_qdp,file=newfile
     oplot,time,yfit,color=!green
     oplot,time,f1,line=1
     oplot,time,f2,line=2
     if breaks eq 2 then oplot,[newp[6],newp[6]],[1e-5,1e5],line=1,color=!yellow
     if breaks ge 1 then oplot,[newp[2],newp[2]],[1e-5,1e5],line=1,color=!yellow
     print,'Are you happy with this fit (y/n)? '
     k=get_kbrd(10)
     if k eq 'n' then goto,doagain
     if k eq 's' then stop
     
;  k=get_kbrd(10)
;  if k eq 's' then stop
     print,'Using Monte Carlo error method with 1000 simulations'
     lc_monte,time,tt,exptime,cts,err,corr_fact,srccounts,backcounts,newp,mo,pname,outperr,noplot=noplot,ps=ps,nsim=1000,nsig=2.,parinfo=parinfo
     perror=outperr
     ;;write output fit file
     ofile='lc_fit_out_twocomp_int'+int+'.dat'
     print,'writing out '+ofile
     openw,lun,ofile,/get_lun
     norm=newp[0]
     normerr=perror[*,0]
     for i=0,n_elements(newp)-1 do begin
        printf,lun,pname[i]+' '+ntostr(newp[i])+' '+ntostr(perror[0,i])+' '+ntostr(perror[1,i])
     endfor
;  printf,lun,'Norm '+ntostr(norm)+' '+ntostr(normerr[0])+' '+ntostr(normerr[1])
     printf,lun,'Chisq '+ntostr(chisq)
     printf,lun,'dof '+ntostr(dof)
     close,lun
     free_lun,lun
     
     p=newp
     oplot,[transtime,transtime],[1e-5,1e5],line=1,color=!blue
  endif else begin 
     ofile='lc_fit_out_twocomp_int'+int+'.dat'
     read_lcfit_twocomp,ofile,pname,p,perror,chisq,dof,breaks ;,lc=lc
  
     newp=p
     lc=lcout2fits(file)
     time=lc.time

     case breaks of
        0: begin 
           mo='pow_pow'
           intmo='int_pow_pow'
        end 
        1: begin    
           mo='pow_bknpow'
           intmo='int_pow_bknpow'
           time=[time,newp[4]]
        end
        2: begin 
           mo='double_bknpow'
           intmo='int_double_bknpow'
           time=[time,newp[2],newp[6]]
        end 
     endcase 
     time=time[sort(time)]
     tmp=execute('yfit='+mo+'(time,newp,f1,f2)')
     transtime=interpol(time,f1-f2,0.)
     time=[time,transtime]
     time=time[sort(time)]
     tmp=execute('yfit='+mo+'(time,newp,f1,f2)')
     if breaks eq 1 then begin 
        if newp[4]-transtime gt 0 then latebr=1
     endif 
     print,newp
;     erase
;     multiplot2,[1,breaks+2],/init
;     multiplot2
     plot_like_qdp,file=file,title=grb
     oplot,time,yfit,color=!green
     oplot,time,f1,line=1
     oplot,time,f2,line=2
     if breaks eq 2 then oplot,[newp[6],newp[6]],[1e-5,1e5],line=1,color=!yellow
     if breaks ge 1 then oplot,[newp[2],newp[2]],[1e-5,1e5],line=1,color=!yellow
     oplot,[transtime,transtime],[1e-5,1e5],line=1,color=!blue
  endelse 
  
  ;;;SPECTRAL FITS
  if not keyword_set(skipspec) then $
     fit_spec,file=ofile,transtime=transtime,/phil,wtreg=wtreg,z=z
 
  read_specfit,spec,dir='spec/',append='_2s'
  
  ;;;CR FITS
  case breaks of 
     0: arr=[1,3]
     1: arr=[1,3,5] 
     2: arr=[1,3,5,7]
  endcase 
  
  alpha=p[arr]
  alphaerr0=perror[0,arr]
  alphaerr1=perror[1,arr]
  
  gamma=spec.pow
  gammaerr0=spec.pow_err[0]
  gammaerr1=spec.pow_err[1]
  
  w=where(gamma eq 0.,nw)
  if nw gt 0 then begin
     gamma[w]=gamma[w-1]
     gammaerr0[w]=gammaerr0[w-1]
     gammaerr1[w]=gammaerr1[w-1]
     print,'SEGMENT NO SPEC FIT, USING PREVIOUS'
  endif 
     
  k=get_kbrd(10)
  
  if breaks ge 1 then plotcompat=1 else plotcompat=0
  plotcomm=['oplot,lc.time,'+mo+'(lc.time,['+ntostrarr(newp,',')+'],f1,f2)',$
           'oplot,lc.time,f1,line=1','oplot,lc.time,f2,line=2']
  
  
  fit_crs,alpha,alphaerr0,alphaerr1,gamma,gammaerr0,gammaerr1,plotevery=plotevery,name=name,/nocolor,skipfirst=skipfirst,/twocomp,latebr=latebr,title=grb,noconsis=noconsis,/plotlc,lc=lc,crstr=crstr,plotcompat=0,plotcomm=plotcomm,ps=ps
  
  case breaks of
     0: begin 
        canon='I-II'
        twocomp='e'
     end 
     1: begin
        if keyword_set(latebr) then begin 
           canon='I-II-II'
           twocomp='b'
        endif else begin 
           canon='0-I-II'
           twocomp='d'
        endelse 
     end 
     2: begin 
        canon='0-I-II-III'
        twocomp='a'
     end 
  endcase 
  cr=mrdfits(!mdata+'closure_relations_total_2sig.fits',1)
  w=where(strtrim(cr.grb,2) eq grb,nw)
  
  tags=tag_names(cr)
  crs=tags[6:57]
  crvals=fltarr(breaks+2,52)
  good=intarr(n_elements(crstr))
  good[*]=1
  for j=0,breaks+2-1 do begin 
     for i=6,57 do crvals[j,i-6]=crstr[j].(i)
     c=where(crvals[j,*] lt 1.,nc)
     if nc gt 0 then print,crs[c] else begin
        print,'NONE'
        good[j]=0
     endelse
  endfor 
  
  g=where(good eq 0,ng)
  if ng eq 0 then begin
     tstart=' & $< '+ntostr(round(cr[w[0]].tstart))+'$'
     tstop=' & $> '+ntostr(round(cr[w[nw-1]].tstop*1e-3))+'$'
     if breaks gt 0 then begin 
        btime=newp[arr[0:n_elements(arr)-2]+1]
        if breaks eq 2 then btimes=' & $'+ntostr(round(btime[0]))+'$ & $'+ntostr(round(btime[1]))+'$'
        if breaks eq 1 and keyword_set(latebr) then btimes=tstart+' & $'+ntostr(round(btime[1]))+'$'
        if breaks eq 1 and not keyword_set(latebr) then btimes='$'+ntostr(round(btime[0]*1d-3))+'$ & $'+tstop+'$'
     endif else btimes=tstart+tstop
     if cr[w[0]].eiso ne 0 then eiso='$'+numdec(alog10(cr[w[0]].eiso*1d52),2)+'$' else eiso=' -- '
     if n_elements(flun) gt 0 then printf,flun,grb,' & '+canon,' & '+twocomp,btimes,' & '+eiso+' \\'
     print,grb,' & '+canon,' & '+twocomp,btimes,' & '+eiso
  endif 
  ;;NEED CR FITS WITH REDEFINED CONSISTENCY CHECKS AND SEGMENT CR AVAILABILITY
;  if keyword_set(ps) then endplot
  
  return
end 
