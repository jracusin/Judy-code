pro lc_stats,tstart,tstop,exptime
  lcstat=mrdfits(!mdata+'lc_stats_z.fits',1)
  ngrb=n_elements(lcstat)
  tstart=dblarr(ngrb)
  tstop=dblarr(ngrb)
  exptime=dblarr(ngrb)
  cd,!mdata
  file='lc_newout.txt'
  for i=0,ngrb-1 do begin
     dir=strtrim(lcstat[i].grb)
     cd,dir
;     readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
     lc=lcout2fits(file)
     time=lc.time
     tstarted=lc.tstart
     tstoped=lc.tstop
     cts=lc.src_rate
     err=lc.src_rate_err
     type=lc.type
     bg=lc.tot_back_cts
     src=lc.src_counts
     sig=lc.det_sig
     expt=lc.exptime
     sigma=src/sqrt(src+bg*2)
     tstart[i]=tstarted[0]
     tstop[i]=tstoped[n_elements(time)-1]
     exptime[i]=total(expt)
     cd,'..'
  endfor
  return
end 

pro kstest_alpha,ps=ps

  if keyword_set(ps) then begin
     begplot,name='kstest_alpha.ps',/land
     psm=!tsym
  endif else psm=!vsym

;zlcstat=mrdfits(!mdata+'lc_stats_z.fits',1)
  lcstat=mrdfits(!mdata+'lc_stats_z.fits',1)
  zlcstat=lcstat

  w=where(lcstat.z eq 0)
  lcstat=lcstat[w]
  w0=where(lcstat.pow ne 0,nw0)
  pow=lcstat[w0].pow
  w1=where(lcstat.pow1 ne 0,nw1)
  pow1=lcstat[w1].pow1
  w2=where(lcstat.pow2 ne 0,nw2)
  pow2=lcstat[w2].pow2
  w3=where(lcstat.pow3 ne 0,nw3)
  pow3=lcstat[w3].pow3
  w4=where(lcstat.pow4 ne 0,nw4)
  pow4=lcstat[w4].pow4

  z=where(zlcstat.z ne 0)
  zlcstat=zlcstat[z]
  wz0=where(zlcstat.pow ne 0,nw0)
  zpow=zlcstat[wz0].pow
  wz1=where(zlcstat.pow1 ne 0,nw1)
  zpow1=zlcstat[wz1].pow1
  wz2=where(zlcstat.pow2 ne 0,nw2)
  zpow2=zlcstat[wz2].pow2
  wz3=where(zlcstat.pow3 ne 0,nw3)
  zpow3=zlcstat[wz3].pow3
  wz4=where(zlcstat.pow4 ne 0,nw4)
  zpow4=zlcstat[wz4].pow4

;;kstest each pow distribution
  !p.multi=[0,2,2]
  pks='p!LKS!N = '
  kstwop,pow,zpow,d,prob,/plot,xtitle='Only 1 '+psm.alpha,ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,pow1,zpow1,d,prob,/plot,xtitle=psm.alpha+'!L1!N',ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,pow2,zpow2,d,prob,/plot,xtitle=psm.alpha+'!L2!N',ytitle='p',xrange=[-2,3]
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,pow3,zpow3,d,prob,/plot,xtitle=psm.alpha+'!L3!N',ytitle='p'
  legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0
;kstwop,pow4,zpow4,d,prob,/plot,title='Pow4'
;legend,['d = '+ntostr(d),'prob = '+ntostr(prob)],/bottom,/right,box=0

  !p.multi=0

  if keyword_set(ps) then endplot

;;kstest on break times
  w1=where(lcstat.break1 ne 0)
  w2=where(lcstat.break2 ne 0)
  w3=where(lcstat.break3 ne 0)
  break1=lcstat[w1].break1
  break2=lcstat[w2].break2
  break3=lcstat[w3].break3

  wz1=where(zlcstat.break1 ne 0)
  wz2=where(zlcstat.break2 ne 0)
  wz3=where(zlcstat.break3 ne 0)
  zbreak1=zlcstat[wz1].break1
  zbreak2=zlcstat[wz2].break2
  zbreak3=zlcstat[wz3].break3

  if keyword_set(ps) then begplot,name='kstest_breaks.ps',/land else k=get_kbrd(10)
  !p.multi=[0,2,2]
  kstwop,break1,zbreak1,d,prob,/plot,xtitle='Break!L1!N',ytitle='p',charsize=1.3
  legend,['d = '+sigfig(d,1),pks+sigfig(prob,3)],/bottom,/right,box=0
  kstwop,break2,zbreak2,d,prob,/plot,xtitle='Break!L2!N',ytitle='p',charsize=1.3
  legend,['d = '+sigfig(d,2),pks+sigfig(prob,3)],/bottom,/right,box=0
;kstwop,break3,zbreak3,d,prob,/plot,xtitle='Break!L3!N',ytitle='p'
;legend,['d = '+sigfig(d,3),pks+sigfig(prob,3)],/bottom,/right,box=0

  !p.multi=0
  if keyword_set(ps) then endplot

  return
end 
pro postage_stamps

  lcstat=mrdfits(!mdata+'lc_stats.fits',1)

  begplot,name=!mdata+'lc_postage_stamps.ps',/land,/color
  !p.multi=[0,5,4]

  xm=!x.margin
  ym=!y.margin
;!x.margin=[5,1]
;!y.margin=[2,1]
  for i=0,n_elements(lcstat)-1 do begin
;for i=0,19 do begin 
     grb=strtrim(lcstat[i].grb,2)
     print,grb
;    cd,lcstat[i].grb
     breaks=lcstat[i].nbreaks
     case breaks of
        0: begin
           mo='pow'
           p=[lcstat[i].norm,lcstat[i].pow]
           perror=[lcstat[i].normerr,lcstat[i].powerr]
           pnames=['norm','pow']
        end 
        1: begin 
           mo='bknpow'
           if lcstat[i].pow1 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2]
              perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err]
           endif
           if lcstat[i].pow1 eq 0 and lcstat[i].pow2 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow2,lcstat[i].break2,lcstat[i].pow3]
              perror=[lcstat[i].normerr,lcstat[i].pow2err,lcstat[i].break2err,lcstat[i].pow3err]
           endif 
           if lcstat[i].pow1 eq 0 and lcstat[i].pow2 eq 0 and lcstat[i].pow3 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow3,lcstat[i].break3,lcstat[i].pow4]
              perror=[lcstat[i].normerr,lcstat[i].pow3err,lcstat[i].break3err,lcstat[i].pow4err]
           endif 
           pnames=['norm','pow1','break','pow2']
           w=where(p eq 0,nw)
           if nw gt 0 then stop
        end 
        2: begin 
           mo='bkn2pow'
           if lcstat[i].pow1 ne 0 then begin 
              p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2,$
                 lcstat[i].break2,lcstat[i].pow3]
              perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err,$
                      lcstat[i].break3err,lcstat[i].pow3err]
           endif else begin 
              p=[lcstat[i].norm,lcstat[i].pow2,lcstat[i].break2,lcstat[i].pow3,$
                 lcstat[i].break3,lcstat[i].pow4]
              perror=[lcstat[i].normerr,lcstat[i].pow2err,lcstat[i].break2err,lcstat[i].pow3err,$
                      lcstat[i].break3err,lcstat[i].pow4err]
           endelse 
           pnames=['norm','pow1','break1','pow2','break2','pow3']
        end 
        3: begin 
           mo='bkn3pow'
           p=[lcstat[i].norm,lcstat[i].pow1,lcstat[i].break1,lcstat[i].pow2,$
              lcstat[i].break2,lcstat[i].pow3,lcstat[i].break3,lcstat[i].pow4]
           perror=[lcstat[i].normerr,lcstat[i].pow1err,lcstat[i].break1err,lcstat[i].pow2err,$
                   lcstat[i].break3err,lcstat[i].pow3err,lcstat[i].break3err,lcstat[i].pow4err]
           pnames=['norm','pow1','break1','pow2','break2','pow3','break3','pow4']
        end 
        else: begin
           p=0
        end 
     endcase

     file=!mdata+grb+'/lc_newout.txt'
;     readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
     lc=lcout2fits(file)
     time=lc.time
     tstarted=lc.tstart
     tstoped=lc.tstop
     cts=lc.src_rate
     err=lc.src_rate_err
     type=lc.type
     bg=lc.tot_back_cts
     src=lc.src_counts
     sig=lc.det_sig
     expt=lc.exptime
     sigma=src/sqrt(src+bg*2)
     replot_xrt_lc,time,timerr,cts,err,file=file,title=grb,charsize=1
     if breaks ne -1 then plot_lcfit_results,p,perror,lcstat[i].chisq,lcstat[i].dof,time,breaks,leg,pnames,charsize=0.5,/noleg

;    cd,'..'
  endfor 
  !p.multi=0
  endplot

  !x.margin=xm
  !y.margin=ym
  return
end 
@fit_functions
pro plot_lcfit_results,newp,perror,chisq,dof,time,breaks,leg,pnames,charsize=charsize,noleg=noleg

  norm=newp[0]
;  normerr=perror[0]
  normerr=perror[*,0]
  pow1=newp[1]
;  pow1err=perror[1]
  pow1err=perror[*,1]
  case breaks of
     0: begin 
        t=time
        y=pow(t,newp)
        pnames='Pow1'
;        leg='Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3)
        leg='Pow1'+' = '+sigfig(pow1,3)+' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3)
     end
     1: begin 
        pow2=newp[3]
;        pow2err=perror[3]
        pow2err=perror[*,3]
        break1=newp[2]
;        break1err=perror[2]
        break1err=perror[*,2]
        w1=where(time gt 0 and time lt break1,nw1)
        w2=where(time ge break1)
        t=[time[w1],break1,time[w2]]
        y=bknpow(t,newp)
        pnames=['Pow1','Breaktime','Pow2']
;        leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
;             'Breaktime = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
;             'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3)]
        leg=['Pow1'+' = '+sigfig(pow1,3)+' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
             'Breaktime = '+ntostr(break1)+' !S!E+'+sigfig(break1err[1],3)+' !R!I-'+sigfig(break1err[0],3),$
             'Pow2'+' = '+sigfig(pow2,3)+' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3)]
        
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
     end
     2: begin 
        pow2=newp[3]
;        pow2err=perror[3]
        pow2err=perror[*,3]
        pow3=newp[5]
;        pow3err=perror[5]
        pow3err=perror[*,5]
        break1=newp[2]
;        break1err=perror[2]
        break1err=perror[*,2]
        break2=newp[4]
;        break2err=perror[4]
        break2err=perror[*,4]
        
        w1=where(time gt 0 and time lt break1,nw1)
        w2=where(time ge break1 and time lt break2,nw2)
        w3=where(time ge break2)
        t=[time[w1],break1,time[w2],break2,time[w3]]
        y=bkn2pow(t,newp)
        pnames=['Pow1','Breaktime1','Pow2','Breaktime2','Pow3']
;        leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
;             'Breaktime1 = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
;             'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3),$
;             'Breaktime2 = '+ntostr(break2)+' '+!tsym.plusminus+' '+ntostr(break2err),$
;             'Pow3'+' = '+sigfig(pow3,3)+' '+!tsym.plusminus+' '+sigfig(pow3err,3)]
        leg=['Pow1'+' = '+sigfig(pow1,3)+' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
             'Breaktime1 = '+ntostr(break1)+' !S!E+'+sigfig(break1err[1],3)+' !R!I-'+sigfig(break1err[0],3),$
             'Pow2'+' = '+sigfig(pow2,3)+' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3),$
             'Breaktime2 = '+ntostr(break2)+' !S!E+'+sigfig(break2err[1],3)+' !R!I-'+sigfig(break2err[0],3),$
             'Pow3'+' = '+sigfig(pow3,3)+' !S!E+'+sigfig(pow3err[1],3)+' !R!I-'+sigfig(pow3err[0],3)]
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
        oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2
        
     end
     3: begin 
        pow2=newp[3]
;        pow2err=perror[3]
        pow2err=perror[*,3]
        pow3=newp[5]
;        pow3err=perror[5]
        pow3err=perror[*,5]
        pow4=newp[7]
;        pow4err=perror[7]
        pow4err=perror[*,7]
        break1=newp[2]
;        break1err=perror[2]
        break1err=perror[*,2]
        break2=newp[4]
;        break2err=perror[4]
        break2err=perror[*,4]
        break3=newp[6]
;        break3err=perror[6]
        break3err=perror[*,6]

        w1=where(time ge 0 and time lt break1,nw1)
        w2=where(time ge break1 and time lt break2,nw2)
        w3=where(time ge break2 and time lt break3,nw3)
        w4=where(time ge break3,nw4)
        t=[time[w1],break1,time[w2],break2,time[w3],break3,time[w4]]
        y=bkn3pow(t,newp)
        pnames=['Pow1','Breaktime1','Pow2','Breaktime2','Pow3','Breaktime3','Pow4']
;        leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
;             'Breaktime1 = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
;             'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3),$
;             'Breaktime2 = '+ntostr(break2)+' '+!tsym.plusminus+' '+ntostr(break2err),$
;             'Pow3'+' = '+sigfig(pow3,3)+' '+!tsym.plusminus+' '+sigfig(pow3err,3),$
;             'Breaktime3 = '+ntostr(break3)+' '+!tsym.plusminus+' '+ntostr(break3err),$
;             'Pow4'+' = '+sigfig(pow4,3)+' '+!tsym.plusminus+' '+sigfig(pow4err,3)]
        leg=['Pow1'+' = '+sigfig(pow1,3)+' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
             'Breaktime1 = '+ntostr(break1)+' !S!E+'+sigfig(break1err[1],3)+' !R!I-'+sigfig(break1err[0],3),$
             'Pow2'+' = '+sigfig(pow2,3)+' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3),$
             'Breaktime2 = '+ntostr(break2)+' !S!E+'+sigfig(break2err[1],3)+' !R!I-'+sigfig(break2err[0],3),$
             'Pow3'+' = '+sigfig(pow3,3)+' !S!E+'+sigfig(pow3err[1],3)+' !R!I-'+sigfig(pow3err[0],3),$
             'Breaktime2 = '+ntostr(break3)+' !S!E+'+sigfig(break3err[1],3)+' !R!I-'+sigfig(break3err[0],3),$
             'Pow3'+' = '+sigfig(pow4,3)+' !S!E+'+sigfig(pow4err[1],3)+' !R!I-'+sigfig(pow4err[0],3)]
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
        oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2
        oplot,[break3,break3],[1e-8,1e5],color=!yellow,line=2

        
     end
  endcase 
  
  if norm gt 100 then sci=1 else sci=0
  leg=[leg,$
       'Norm = '+sigfig(norm,3,sci=sci)+' !S!E+'+sigfig(normerr[1],3,sci=sci)+' !R!I-'+sigfig(normerr[0],3,sci=sci),$
;       'Norm = '+ntostr(norm)+' '+!tsym.plusminus+' '+ntostr(normerr),$
       !tsym.chi+'!U2!N/dof = '+sigfig(chisq/dof,4),$
       'dof = '+ntostr(fix(dof))]

  oplot,t,y,color=!green,thick=1
;  oplot,time,yfit,color=!green
  if not keyword_set(noleg) then legend,leg,box=0,/top,/right,charsize=charsize


  return
end 
pro fit_lc_wrapper,newp=newp,dir=dir,mdir=mdir,siglim=siglim

  g=0
  if n_elements(mdir) eq 0 then mdir=!mdata
  cd,mdir
  if n_elements(dir) eq 0 then dir=file_search('GRB*')
  stop

  for i=g,n_elements(dir)-1 do begin 
     cd,dir[i]
     print,i
     print,dir[i]
     if exist('lc_newout.txt') then begin 
        slope=-999
        window,0
        nfl=''
        replot_xrt_lc,time,timerr,cts,err,file='lc_newout.txt',title=dir[i]
        file='lc_fit_out_idl.dat'
        go=0
        if exist('lc_newout_noflares.txt') and not exist('flares_gtis.dat') then go=1
        if exist(file) and not go then begin 
           read_lcfit,file,pname,newp,perror,chisq,dof,breaks
           if pname[0] ne 'nofit' then begin 
              plot_lcfit_results,newp,perror,chisq,dof,time,breaks,leg,pnames
              print,'Use previous fit? (y/n)'
              pfit='y'
              pfit=get_kbrd(10)
              if pfit eq 's' then stop
           endif else begin 
              pfit='n'
              nfl='p'
           endelse 
           
        endif else pfit='n'
        if pfit eq 'n' then begin 
           
           file='lc_newout_noflares.txt'
           ans='n'
           if exist(file) then begin
              print,'Use existing noflares filter? (y/n)'
              ans=get_kbrd(10)
           endif 
           if ans eq 'n' then begin 
              print,'remove flares or p to skip? (y/n/p)'
              fl=get_kbrd(10)
              if fl eq 'y' then begin
                 fit_noflares,/small ;,battime=battime
              endif else file='lc_newout.txt'
           endif else fl=''
           if fl[0] ne 'n' and fl[0] ne 'y' and nfl eq 'p' then fl='p'
           
           print,'type to continue (s to stop)'
           k=get_kbrd(10)
           if k eq 's' then stop
           if fl ne 'p' then begin 
              fit_lc2,file,title=dir[i],slope=slope,newp=newp,siglim=siglim
              print,'type to continue'
              k=get_kbrd(10)
              if k eq 's' then stop
           endif else begin
              openw,lun,'lc_fit_out_idl.dat',/get_lun
              printf,lun,'no fit'
              close,lun
              free_lun,lun
           endelse 
        endif else begin
           print,'type to continue'
           k=get_kbrd(10)
        endelse
;        plot_like_qdp,t,yfit,dir[i]
     endif 
     cd,mdir

  endfor 

  return
end 

@fit_functions

pro fit_pow_model,t,cts,terr,err,p,model,pnames,yfit,newp,perror,chisq,dof,weights,src,back,status=status

  np=n_elements(p)
  parinfo = parinfo_struct(np)

  parinfo.parname=pnames
  parinfo.value=p
  parinfo[0].limits=[0,0] ;;norm > 0
  parinfo[0].limited=[1,0]
  mint=min(t)*1.1
  maxt=max(t)*0.9
;;pow 1 limits > 0
  parinfo[1].limits=[0,0]
  parinfo[1].limited=[1,0]
  pmargin=2.


  
  case np of 
     4: begin                   ;bknpow
        ;;break limits
        parinfo[2].limits=[mint,maxt]
        parinfo[2].limited=[1,1]
        parinfo[2].mpminstep=1.
        ;;pow limits
        parinfo[3].limits=[p[3]-pmargin,p[3]+pmargin]
        parinfo[3].limited=[1,1]
     end
     6: begin                   ;bkn2pow
        ;;break limits
        parinfo[2].limits=[mint,p[4]]
        parinfo[4].limits=[p[2],maxt]
        parinfo[[2,4]].limited=[1,1]
        parinfo[[2,4]].mpminstep=1.
        ;;pow limits
        parinfo[3].limits=[p[3]-pmargin,p[3]+pmargin]
        parinfo[5].limits=[p[5]-pmargin,p[5]+pmargin]
        parinfo[[3,5]].limited=[1,1]
     end
     8: begin                   ;bkn3pow
        ;;break limits
        parinfo[2].limits=[mint,p[4]]
        parinfo[4].limits=[p[2],p[6]]
        parinfo[6].limits=[p[4],maxt]
        parinfo[[2,4,6]].limited=[1,1]
        parinfo[[2,4,6]].mpminstep=1.
        ;;pow limits
        parinfo[3].limits=[p[3]-pmargin,p[3]+pmargin]
        parinfo[5].limits=[p[5]-pmargin,p[5]+pmargin]
        parinfo[7].limits=[p[7]-pmargin,p[7]+pmargin]
        parinfo[[3,5,7]].limited=[1,1]
     end
     else: begin
     end 
  endcase 

;comberr=sqrt((terr/t)^2+(err/cts)^2)
  comberr=err
  weights=1.                    ;/err^2;comberr

  newp=mpfitfun(model,t,cts,err,p,parinfo=parinfo,$
                bestnorm=chisq,dof=dof,niter=niter,errmsg=errmsg,$
                perror=perror,yfit=yfit,status=status,nprint=10,$
                ftol=1e-15,xtol=1e-15,gtol=1e-25)


  print,status
  case status of
     0: print,'Improper input'
     1: print,'both actual and predicted relative reduction in sums of squares < FTOL'
     2: print,'relative error between 2 consecutive iterates is < XTOL'
     3: print,'conditions for STATUS = 1 & 2 both hold'
     4: print,'Cosine of angle between fvec and any column of J is || < GTOL'
     5: print,'Maximum iterations'
     6: print,'FTOL too small, no further reduction possible'
  endcase

  if status ne 0 then $
     perror=perror*sqrt(chisq/dof)
  
  
;weights=1./comberr^2
;chisq=total((cts-yfit)^2*abs(1./comberr^2))

  return
end 

pro plot_base_lc,w,_extra=_extra,ps=ps
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  if keyword_set(ps) then plotsym,0,0.7,/fill else plotsym,0,1.0,/fill
  
  if n_elements(yrange) eq 0 then yrange=[min(cts-err),max(cts+err)]
  winf=where(abs(err) gt 1e4,nwinf)
  if nwinf gt 0 then begin
      wninf=where(abs(err) lt 1e4)
      yrange=[min(cts[wninf]+err[wninf]),max(cts[wninf]+err[wninf])]
  endif 
  if yrange[0] lt 1e-4 then yrange[0]=1e-4

  xrange=[min(time-timerr),max(time+timerr)]
  
  plot,xrange,yrange,/nodata,xtitle='seconds since BAT trigger',ytitle='cts/s',/xlog,/ylog,yrange=yrange,_extra=_extra
  
  wt=where(type eq 0,nwt)
  if nwt gt 0 then $
     oploterror,time[wt],cts[wt],timerr[wt],err[wt],psym=3,/nohat;,color=!blue,errcolor=!blue
  
  pc=where(type eq 1,npc)
  if npc gt 0 then $
     oploterror,time[pc],cts[pc],timerr[pc],err[pc],psym=8,/nohat;,color=!red,errcolor=!red
  
  if n_elements(w) gt 0 then begin
     pc=where(type[w] eq 1,npc)
     wt=where(type[w] eq 0,nwt)
     color=!red
     if nwt gt 0 then $
        oploterror,time[w[wt]],cts[w[wt]],timerr[w[wt]],err[w[wt]],psym=3,/nohat,color=color,errcolor=color
     if npc gt 0 then $
        oploterror,time[w[pc]],cts[w[pc]],timerr[w[pc]],err[w[pc]],psym=8,/nohat,color=color,errcolor=color
  endif 
  
  return 
end 

pro fit_lc2,file,slope=slope,_extra=_extra,newp=newp,yfit=yfit,t=t,perror=perror,siglim=siglim
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
;  outfile='lc_out.dat'
  
  if n_elements(siglim) eq 0 then siglim=2.1
;  if exist('lc_newout_noflares.txt')
  if n_elements(file) eq 0 then file='lc_newout.txt'
;  if not exist(outfile) then lcout2xcm,file,outfile
  print,file
;  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  
  lc=lcout2fits(file)
  time=lc.time
  tstarted=lc.tstart
  tstoped=lc.tstop
  cts=lc.src_rate
  err=lc.src_rate_err
  type=lc.type
  bg=lc.tot_back_cts
  src=lc.src_counts
  sig=lc.det_sig
  expt=lc.exptime
  sigma=src/sqrt(src+bg*2)
  
  timerr=((time-tstarted)+(tstoped-time))/2.
  type=fix(type)
  w=where(cts gt 0 and finite(err) and sigma ge siglim)
  time=time[w] & timerr=timerr[w] & cts=cts[w] & err=err[w] & type=type[w]
  back=bg                       ;*expt

;  if cts[1]*expt[1] lt 12 then cash=1 else cash=0
  if src[1] lt 12 then cash=1 else cash=0
  if cash then begin
     print,'USING CSTATS IN XSPEC'
     fit_lc_xspec,newp,perror,chisq,dof,yfit,slope=1,/cash,/noplot
     status=1
     np=n_elements(newp)
     case np of
        2: breaks=0
        4: breaks=1
        6: breaks=2
        8: breaks=3
     endcase 
  endif else begin

     refit:
     plot_base_lc,_extra=_extra
;     plot_like_qdp,_extra=_extra
     
     bt=0d
     !mouse.button=0

     while (!MOUSE.button NE 4) do begin
        
        print,'Click on estimate breaktime, or right click to continue'
        cursor,xxx,yyy,/wait,/change
        if !mouse.button ne 4 then begin 
           oplot,[xxx,xxx],[1e-6,1e4],color=!yellow
           print,round(xxx)
           bt=[bt,xxx]
        endif
     endwhile
     breaks=n_elements(bt)-1
     if breaks gt 0 then bt=round(bt[1:*])

     slope=0d
     mintime=ntostr(min(time))
     maxtime=ntostr(max(time))
     for i=0,breaks do begin 
        if i lt breaks then begin
           if i ne 0 then time1=ntostr(bt[i-1]) else time1=mintime
           wtime2=ntostr(bt[i])
           if i lt breaks-1 then time2=ntostr(bt[i+1]) else time2=maxtime
        endif else begin 
           if breaks gt 0 then time1=ntostr(bt[i-1]) else time1=mintime
           wtime2=maxtime
        endelse 
        w=where(time ge time1-10. and time le wtime2+10. and sigma gt siglim,nw)
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

     if not finite(norm) then norm=max(cts)

;  norm=1e31;max(cts)*10.
     case breaks of
        0: begin
           mo='pow'
           p=[norm,slope]
           pnames=['norm','pow']
        end 
        1: begin 
           mo='bknpow'
           p=[norm,slope[0],bt,slope[1]]
           pnames=['norm','pow1','break','pow2']
        end 
        2: begin 
           mo='bkn2pow'
           p=[norm,slope[0],bt[0],slope[1],bt[1],slope[2]]
           pnames=['norm','pow1','break1','pow2','break2','pow3']
        end 
        3: begin 
           mo='bkn3pow'
           p=[norm,slope[0],bt[0],slope[1],bt[1],slope[2],bt[2],slope[3]]
           pnames=['norm','pow1','break1','pow2','break2','pow3','break3','pow4']
        end 
     endcase
     
     wul=where(sigma gt siglim,wul)
     fit_pow_model,time[wul],cts[wul],timerr[wul],err[wul],p,mo,pnames,yfit,newp,perror,chisq,dof,weights,src[wul],back[wul],status=status
  endelse 
  
  if status eq 0 then goto,refit
  
  perror0=dblarr(2,n_elements(perror))
  perror0[0,*]=perror
  perror0[1,*]=perror
  
  plot_lcfit_results,newp,perror0,chisq,dof,time,breaks,leg,pnames
  doagain='y'
  input,'Is this an acceptable fit? (y/n) ',doagain,'y'
  if doagain eq 's' then stop
  if doagain eq 'n' then begin
     slope=-999
     goto,refit
  endif 
  
  perror0=perror
  print
  print,'Calculating proper 1-sigma errors'
  conf_error,time,cts,err,newp,perror0,mo,perror2,bestfit,pvarunit,bestchisq,yfit ;,/doplot
  colprint,newp,perror0,perror2[0,*],perror2[1,*],bestfit,pvarunit,bestchisq
;  newp=bestfit*1.
;  for i=0,n_elements(newp)-1 do perror[i]=mean(perror2[*,i])
  perror=perror2
  chisq=total((cts-yfit)^2./err^2)
;  oplot,time,yfit,color=!red
  newp=newp*1d
  
  erase  
  plot_base_lc
  plot_lcfit_results,newp,perror,chisq,dof,time,breaks,leg,pnames
  

  t=time
  ;;write output ps file
  begplot,name='lc_fit_plot.ps',/landscape,/color
  plot_base_lc,/ps,_extra=_extra
;  plot_like_qdp,_extra=_extra
  oplot,time,yfit,color=!green
  legend,leg,box=0,/top,/right
  endplot
  
  ;;write output fit file
  openw,lun,'lc_fit_out_idl.dat',/get_lun
  norm=newp[0]
  normerr=perror[*,0]
  for i=0,n_elements(newp)-2 do begin
     j=i+1
     printf,lun,pnames[i]+' '+ntostr(newp[j])+' '+ntostr(perror[0,j])+' '+ntostr(perror[1,j])
  endfor
  printf,lun,'Norm '+ntostr(norm)+' '+ntostr(normerr[0])+' '+ntostr(normerr[1])
  printf,lun,'Chisq '+ntostr(chisq)
  printf,lun,'dof '+ntostr(dof)
  close,lun
  free_lun,lun
  
  return
end 

pro plot_lc2_results,ps=ps,withz=withz

zshift=1d
if keyword_set(ps) then psm=!tsym else psm=!vsym
if keyword_set(withz) then begin 
    lcstat=mrdfits(!mdata+'lc_stats_z.fits',1)
    lcstat=lcstat[where(lcstat.z ne 0)]
    zshift=(1.+lcstat.z)
    if keyword_set(ps) then begplot,name='fit_lc2_results_z.ps',/color
    outfile2='alpha_hist_z.ps'
    outfile3='break_times_z.ps'
    yrange1=[0,11]
    xrange2=[1,6]
    yrange2=[0,3]
endif else begin
    lcstat=mrdfits(!mdata+'lc_stats.fits',1) 
    if keyword_set(ps) then begplot,name='fit_lc2_results.ps',/color
    outfile2='alpha_hist.ps'
    outfile3='break_times.ps'
    yrange1=[0,25]
    xrange2=[2,6]
    yrange2=[0,6]
endelse 

w0=where(lcstat.pow ne 0,nw0)
pow=lcstat[w0].pow
w1=where(lcstat.pow1 ne 0,nw1)
pow1=lcstat[w1].pow1
w2=where(lcstat.pow2 ne 0,nw2)
pow2=lcstat[w2].pow2
w3=where(lcstat.pow3 ne 0,nw3)
pow3=lcstat[w3].pow3
w4=where(lcstat.pow4 ne 0,nw4)
pow4=lcstat[w4].pow4

wb1=where(lcstat.break1 ne 0,nb1)
break1=lcstat[wb1].break1/zshift
wb2=where(lcstat.break2 ne 0,nb2)
break2=lcstat[wb2].break2/zshift
wb3=where(lcstat.break3 ne 0,nb3)
break3=lcstat[wb3].break3/zshift

;window,0
!p.multi=[0,1,2]
bin=0.2
plothist,pow1,bin=bin,xtitle='Photon Index',xrange=[-1,4],yrange=yrange1
plothist,pow2,bin=bin,/overplot,color=!red
plothist,pow3,bin=bin,/overplot,color=!blue
plothist,pow4,bin=bin,/overplot,color=!orange
plothist,pow,bin=bin,/overplot,color=!green

;;potential 23 or 34 -- seem to be 23 not 34 
;w=where(lcstat.pow1 eq 0 and lcstat.pow2 ne 0 and lcstat.pow3 ne 0 and lcstat.pow4 eq 0)
;wp1=where(lcstat[w].break2 lt 4e4)
;wp2=where(lcstat[w].break2 gt 4e4)
;plothist,lcstat[w[wp1]].pow3,bin=bin,/over,color=!magenta
;plothist,lcstat[w[wp2]].pow3,bin=bin,/over,color=!cyan
;print,median(lcstat[w[wp1]].pow2),median(lcstat[w[wp2]].pow2)
;print,median(lcstat[w[wp1]].pow3),median(lcstat[w[wp2]].pow3)


legend,['Only 1 '+psm.Alpha+' ('+ntostr(nw0)+')',''+psm.Alpha+'1 ('+ntostr(nw1)+')',$
        ''+psm.Alpha+'2 ('+ntostr(nw2)+')',''+psm.Alpha+'3 ('+ntostr(nw3)+')',$
        ''+psm.Alpha+'4 ('+ntostr(nw4)+')'],$
       textcolor=[!green,!p.color,!red,!blue,!orange],box=0,/top,/right

;window,1
bin=.05
plothist,alog10(break1),bin=bin,xtitle='log Break times (s)',xrange=xrange2,yrange=yrange2
plothist,alog10(break2),bin=bin,/over,color=!magenta
plothist,alog10(break3),bin=bin,/over,color=!cyan
legend,['t!Lbreak1!N ('+ntostr(nb1)+')','t!Lbreak2!N ('+ntostr(nb2)+')','t!Lbreak3!N ('+ntostr(nb3)+')'],textcolor=[!p.color,!magenta,!cyan],$
       box=0,/top,/right

!p.multi=0
if keyword_set(ps) then endplot

;;;HISTOGRAMS WITH MULTIPLOT
if keyword_set(ps) then begplot,name=outfile2,/color else k=get_kbrd(10)
erase
xrange=[-2,6]
if keyword_set(withz) then yrange=[0,10] else yrange=[0,14]
bin=0.1
multiplot,[1,5],/init
multiplot
plot,xrange,yrange,/nodata
plothist,lcstat[w0].pow,bin=bin,color=!green,/overplot
legend,['Only 1 '+psm.alpha],/top,/right,box=0,textcolor=!green
multiplot
plot,xrange,yrange,/nodata
plothist,lcstat[w1].pow1,bin=bin,color=!p.color,/overplot
legend,[psm.alpha+'!L1!N'],/top,/right,box=0,textcolor=!p.color
multiplot
plot,xrange,yrange,/nodata
plothist,lcstat[w2].pow2,bin=bin,color=!red,/overplot
legend,[psm.alpha+'!L2!N'],/top,/right,box=0,textcolor=!red
multiplot
plot,xrange,yrange,/nodata
plothist,lcstat[w3].pow3,bin=bin,color=!blue,/overplot
legend,[psm.alpha+'!L3!N'],/top,/right,box=0,textcolor=!blue
multiplot
plot,xrange,yrange,/nodata,xtitle=psm.alpha
plothist,lcstat[w4].pow4,bin=bin,color=!orange,/overplot
legend,[psm.alpha+'!L4!N'],/top,/right,box=0,textcolor=!orange

multiplot,/reset

if keyword_set(ps) then endplot

;;multiplot breaktimes
if keyword_set(ps) then begplot,name=outfile3,/color else k=get_kbrd(10)
erase
xrange=xrange2
yrange=yrange2
;if keyword_set(withz) then yrange=[0,10] else yrange=[0,14]
bin=0.05
multiplot,[1,3],/init
multiplot
plot,xrange,yrange,/nodata
plothist,alog10(break1),bin=bin,color=!p.color,/overplot
legend,['t!Lbreak1!N'],/top,/right,box=0,textcolor=!p.color
multiplot
plot,xrange,yrange,/nodata
plothist,alog10(break2),bin=bin,color=!cyan,/overplot
legend,['t!Lbreak2!N'],/top,/right,box=0,textcolor=!p.color
multiplot
plot,xrange,yrange,/nodata,xtitle='log Break times (s)'
plothist,alog10(break3),bin=bin,color=!magenta,/overplot
legend,['t!Lbreak3!N'],/top,/right,box=0,textcolor=!p.color
multiplot

multiplot,/reset

if keyword_set(ps) then endplot

return
end 

pro fit_lc2_stats
  cd,!mdata
  dat=file_search('GRB*/lc_fit_out_idl.dat')
  ngrb=n_elements(dat)
  lcstat=replicate({grb:'',$
                    norm:0d,$
                    normerr:0d,$
                    pow:0d,$
                    powerr:0d,$
                    pow1:0d,$
                    pow1err:0d,$
                    break1:0d,$
                    break1err:0d,$
                    pow2:0d,$
                    pow2err:0d,$
                    break2:0d,$
                    break2err:0d,$
                    pow3:0d,$
                    pow3err:0d,$
                    break3:0d,$
                    break3err:0d,$
                    pow4:0d,$
                    pow4err:0d,$
                    chisq:0d,$
                    dof:0L,$
                    nbreaks:0 $
                   },ngrb)

  for i=0,ngrb-1 do begin 
     read_lcfit,dat[i],pname,p,perror,chisq,dof,breaks
     if pname[0] ne 'nofit' then begin 
        tmp=str_sep(dat[i],'/')
        lcstat[i].grb=tmp[0]
        lcstat[i].norm=p[0]
        lcstat[i].normerr=perror[0]
        lcstat[i].chisq=chisq
        lcstat[i].dof=dof
        lcstat[i].nbreaks=breaks
        pow1=p[1]
        pow1err=perror[1]

        case breaks of
           0: begin
              lcstat[i].pow=pow1
              lcstat[i].powerr=pow1err
           end
           1: begin
              pow2=p[3]
              pow2err=perror[3]
              break1=p[2]
              break1err=perror[2]
              if pow1 lt pow2 then begin
;                    if break1 lt 4e4 then begin  
                 lcstat[i].pow2=pow1
                 lcstat[i].pow2err=pow1err
                 lcstat[i].break2=break1
                 lcstat[i].break2err=break1err
                 lcstat[i].pow3=pow2
                 lcstat[i].pow3err=pow2err
;                    endif else begin 
;                        lcstat[i].pow3=pow1
;                        lcstat[i].pow3err=pow1err
;                        lcstat[i].break3=break1
;                        lcstat[i].break3err=break1err
                                ;                       lcstat[i].pow4=pow2
;                        lcstat[i].pow4err=pow2err
;                    endelse 
              endif else begin
                 lcstat[i].pow1=pow1
                 lcstat[i].pow1err=pow1err
                 lcstat[i].break1=break1
                 lcstat[i].break1err=break1err
                 lcstat[i].pow2=pow2
                 lcstat[i].pow2err=pow2err
              endelse 
           end 
           2: begin 
              pow2=p[3]
              pow2err=perror[3]
              pow3=p[5]
              pow3err=perror[5]
              break1=p[2]
              break1err=perror[2]
              break2=p[4]
              break2err=perror[4]
              if pow1 gt pow2 then begin 
                 lcstat[i].pow1=pow1
                 lcstat[i].pow1err=pow1err
                 lcstat[i].break1=break1
                 lcstat[i].break1err=break1err
                 lcstat[i].pow2=pow2
                 lcstat[i].pow2err=pow2err
                 lcstat[i].break2=break2
                 lcstat[i].break2err=break2err
                 lcstat[i].pow3=pow3
                 lcstat[i].pow3err=pow3err
              endif else begin
                 lcstat[i].pow2=pow1
                 lcstat[i].pow2err=pow1err
                 lcstat[i].break2=break1
                 lcstat[i].break2err=break1err
                 lcstat[i].pow3=pow2
                 lcstat[i].pow3err=pow2err
                 lcstat[i].break3=break2
                 lcstat[i].break3err=break3err
                 lcstat[i].pow4=pow3
                 lcstat[i].pow4err=pow4err
              endelse 
           end
           3: begin
              pow2=p[3]
              pow2err=perror[3]
              pow3=p[5]
              pow3err=perror[5]
              pow4=p[7]
              pow4err=perror[7]
              break1=p[2]
              break1err=perror[2]
              break2=p[4]
              break2err=perror[4]
              break3=p[6]
              break3err=perror[6]
              
              lcstat[i].pow1=pow1
              lcstat[i].pow1err=pow1err
              lcstat[i].break1=break1
              lcstat[i].break1err=break1err
              lcstat[i].pow2=pow2
              lcstat[i].pow2err=pow2err
              lcstat[i].break2=break2
              lcstat[i].break2err=break2err
              lcstat[i].pow3=pow3
              lcstat[i].pow3err=pow3err
              lcstat[i].break3=break3
              lcstat[i].break3err=break3err
              lcstat[i].pow4=pow4
              lcstat[i].pow4err=pow4err
           end 
        endcase 
     endif else begin 
        lcstat[i].nbreaks=-1
        tmp=str_sep(dat[i],'/')
        lcstat[i].grb=tmp[0]
     endelse 
  endfor

  mwrfits,lcstat,!mdata+'lc_stats.fits',/create

  return
end 

pro read_lcfit,file,pname,p,perror,chisq,dof,breaks
  if numlines(file) eq 1 then pname='nofit' else begin 
     readcol,file,name,col1,col2,col3,format='(a,d,d,d)',/silent
;     readcol,file,name,col1,col2,format='(a,d,d)',/silent
;     col3=col2
     readcol,file,name2,col,format='(a,d)',/silent
     n=n_elements(name)-2
     pname=['norm',name[0:n]]
     p=col1[0:n]
     perror1=col2[0:n]
     perror2=col3[0:n]
     norm=col1[n+1]
     normerr1=col2[n+1]
     normerr2=col3[n+1]
     p=[norm,p]
     perror1=[normerr1,perror1]
     perror2=[normerr2,perror2]
     perror=dblarr(2,n+2)
     perror[0,*]=perror1
     perror[1,*]=perror2
     chisq=col[n+2]
     dof=col[n+3]
     breaks=n/2

;    n=n_elements(name)
;    n2=n_elements(name2)
;    pname=['norm',name2[n:n2-4]]
;    p=col[n:n2-4]
;    perror=(col2[0:n-2]-col1[0:n-2])/2.
;    norm=col[n2-3]
;    normerr=(col2[n-1]-col1[n-1])/2.
;    p=[norm,p]
;    perror=[normerr,perror]
;    chisq=col[n2-2]
;    dof=col[n2-1]
;    breaks=n/2

  endelse 
  return
end 
