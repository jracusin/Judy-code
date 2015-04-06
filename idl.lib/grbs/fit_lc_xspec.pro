pro fit_xlc_wrapper

g=0
cd,!mdata
;spawn,'ls -d GRB* > dir.txt'
;readcol,'dir.txt',dir,format='(a)'
dir=file_search('GRB*')
stop
for i=g,n_elements(dir)-1 do begin 
    cd,dir[i]
    slope=-999
    print,i
    replot_xrt_lc,file='lc_newout.txt',title=dir[i]
    file='lc_newout_noflares.txt'
    ans='n'
    if exist(file) then begin
        print,'Use existing noflares filter? (y/n)'
        ans=get_kbrd(10)
    endif 
    if ans eq 'n' then begin 
        print,'remove flares? (y/n)'
        fl=get_kbrd(10)
        if fl eq 'y' then begin
            fit_noflares,/small
            xcm=1
        endif else file='lc_newout.txt'
    endif 
    print,'type to continue (s to stop)'
    k=get_kbrd(10)
    if k eq 's' then stop
    fit_lc_xspec,title=dir[i],slope=slope,file=file,xcm=xcm
    print,'type to continue'
    k=get_kbrd(10)
    if k eq 's' then stop
    cd,'..'
endfor 

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

pro fit_lc_xspec,p,perror,chisq,dof,yfit,file=file,slope=slope,_extra=_extra,xcm=xcm,cash=cash,noplot=noplot
  
  common fit_flares_common,time,timerr,cts,err,type,llun,fglun,ulnorm,ulpow,ulsigma,ulchisq,uldof,fall,rise
  
  outfile='lc_out.dat'
  
;  if exist('lc_newout_noflares.txt')
  if n_elements(file) eq 0 then file='lc_newout.txt'
;  if not exist(outfile) or keyword_set(xcm) then 
  lcout2xcm,file,outfile,/filt
  print,file
  readcol,file,time,tstarted,tstoped,cts,err,hard,harderr,expt,src,bg,sigma,exp,tot_ext_t,curr_ftype,grade0_ct,/silent
  timerr=((time-tstarted)+(tstoped-time))/2.
  type=fix(curr_ftype)
  w=where(cts gt 0)
  time=time[w] & timerr=timerr[w] & cts=cts[w] & err=err[w] & type=type[w]
  
  doagain='y'
  refit:
  
  window,0
  plot_base_lc,_extra=_extra
  
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
  
  case breaks of
     0: begin
        if n_elements(slope) eq 0 or slope eq -999 then slope=1d
        mo='pow'
     end 
     1: begin 
        if n_elements(slope) eq 0  or slope eq -999 then slope=[1,2]
        mo='bknpow'
     end 
     2: begin 
        if n_elements(slope) eq 0  or slope eq -999 then slope=[3,0,1]
        mo='bkn2pow'
     end 
     3: begin 
        if n_elements(slope) eq 0  or slope eq -999 then slope=[3,0,1,2]
        mo='bkn3pow'
     end 
  endcase
  

  openw,lun,'xlc.xcm',/get_lun
  printf,lun,'data xlc.pha'
  printf,lun,'query yes'
  printf,lun,'res xlc.rsp'
  printf,lun,'@xlcign'
  printf,lun,'cpd /ps'
  printf,lun,'setplot en'
  if keyword_set(cash) then printf,lun,'statistic cstat' else $
     printf,lun,'lmod bknthreepow'
  printf,lun
  printf,lun,'mo '+mo
  for i=0,5 do printf,lun,'1.0000'+ntostr(i+1)
  for i=0,2 do printf,lun
  mintime=ntostr(min(time))
  maxtime=ntostr(max(time))
;  minslope=ntostr(slope-2)
;  maxslope=ntostr(slope+2)
  for i=0,breaks do begin 
;      printf,lun,'newpar '+ntostr(i*2+1)+' '+ntostr(slope[i])+' '+minslope[i]+' '+minslope[i]+' '+maxslope[i]+' '+maxslope[i]
      if i lt breaks then begin
          if i ne 0 then time1=ntostr(bt[i-1]) else time1=mintime
          wtime2=ntostr(bt[i])
          if i lt breaks-1 then time2=ntostr(bt[i+1]) else time2=maxtime

          printf,lun,'newpar '+ntostr(i*2+2)+' '+ntostr(bt[i])+' '+time1+' '+time1+' '+time2+' '+time2
      endif else begin 
          if breaks gt 0 then time1=ntostr(bt[i-1]) else time1=mintime
          wtime2=maxtime
      endelse 
      w=where(time ge time1 and time le wtime2,nw)
      mw=max(w)+1
      w2=w
      if mw lt n_elements(time) then $
        w2=[w,mw]
      f=linfit(alog10(time[w]),alog10(cts[w]))
      oplot,time[w2],10^f[0]*time[w2]^f[1]
      print,time1,' ',wtime2
      sl=-f[1]
      minslope=ntostr(sl-2)
      maxslope=ntostr(sl+2)
      printf,lun,'newpar '+ntostr(i*2+1)+' '+ntostr(sl)+' '+minslope+' '+minslope+' '+maxslope+' '+maxslope
      
  endfor 
  
  printf,lun,'thaw 1-'+ntostr(breaks*2+2)
  printf,lun,'fit 1000'
  printf,lun,'fit 1000'
  printf,lun,'fit 1000'
;  printf,lun,'plot ld del'
;  printf,lun,'iplot ldata del'
  printf,lun,'setplot command label x "Seconds since BAT trigger"'
  printf,lun,'setplot command error x off 2'
  printf,lun,'setplot command line on 2'
;  printf,lun,'q'
  printf,lun,'plot ldata res'

  ;;output errors and fits
  printf,lun,'set fileid [open lc_fit_out.dat w]'

;  for i=0,breaks do begin 
;     if i lt breaks then errnames=['Pow_err'+ntostr(i+1),'Break_err'+ntostr(i+1)] else $
;        errnames=['Pow_err'+ntostr(i+1),'Norm_err']
;     for k=0,1 do begin 
;        printf,lun,'error stop 100,,maximum 15.0 '+ntostr(i*2+k+1)
;        j=ntostr(i*2+k+1)
;        printf,lun,'tclout error '+j
;        printf,lun,'set err'+j+' [string trim $xspec_tclout]'
;        printf,lun,'regsub -all { +} $err'+j+' { } cerr'+j+''
;        printf,lun,'set lerr'+j+' [split $cerr'+j+']'
;        printf,lun,'puts $fileid "'+errnames[k]+' [lindex $lerr'+j+' 0] [lindex $lerr'+j+' 1]"'
;     endfor 
; endfor

  printf,lun,'tclout param 1'
  printf,lun,'set par1 [string trim $xspec_tclout]'
  printf,lun,'regsub -all { +} $par1 { } cpar1'
  printf,lun,'set lpar1 [split $cpar1]'
  for p=0,1 do begin 
      for i=0,breaks do begin
          if i lt breaks then begin 
              names=['Pow'+ntostr(i+1),'Break'+ntostr(i+1)] 
              errnames=['Pow_err'+ntostr(i+1),'Break_err'+ntostr(i+1)]
          endif else begin 
              names=['Pow'+ntostr(i+1),'Norm']
              errnames=['Pow_err'+ntostr(i+1),'Norm_err']
          endelse
          if p eq 1 then begin  
              for k=0,1 do begin 
                  j=ntostr(i*2+k+1)
                  printf,lun,'tclout param '+j
                  printf,lun,'set par'+j+' [string trim $xspec_tclout]'
                  printf,lun,'regsub -all { +} $par'+j+' { } cpar'+j+''
                  printf,lun,'set lpar'+j+' [split $cpar'+j+']'
                  printf,lun,'puts $fileid "'+names[k]+' [lindex $lpar'+j+' 0]"'
              endfor 
          endif else begin 
              for k=0,1 do begin 
                  j=ntostr(i*2+k+1)
                  printf,lun,'error stop 100,,maximum 50.0 '+j
                  printf,lun,'tclout error '+j
                  printf,lun,'set err'+j+' [string trim $xspec_tclout]'
                  printf,lun,'regsub -all { +} $err'+j+' { } cerr'+j+''
                  printf,lun,'set lerr'+j+' [split $cerr'+j+']'
                  printf,lun,'puts $fileid "'+errnames[k]+' [lindex $lerr'+j+' 0] [lindex $lerr'+j+' 1]"'
              endfor 
          endelse 
      endfor 
  endfor 
  printf,lun,'tclout stat'
  printf,lun,'set stt [string trim $xspec_tclout]'
  printf,lun,'regsub -all { +} $stt { } cstt'
  printf,lun,'set lstt [split $cstt]'
  printf,lun,'puts $fileid "chisq [lindex $lstt 0]"'
  printf,lun,'tclout dof'
  printf,lun,'set df [string trim $xspec_tclout]'
  printf,lun,'regsub -all { +} $df { } cdf'
  printf,lun,'set ldf [split $cdf]'
  printf,lun,'puts $fileid "dof [lindex $ldf 0]"'
  
  printf,lun,'close $fileid'
  printf,lun,'show'
  if doagain eq 'y' then printf,lun,'exit'
  printf,lun
  close,lun
  free_lun,lun
  
  spawn,'xspec - xlc.xcm'
  
  spawn,'mv pgplot.ps fit_lc.ps'
  
  if numlines('lc_fit_out.dat') gt 0 then begin 
      readcol,'lc_fit_out.dat',param,value,format='(a,d)',/silent
      readcol,'lc_fit_out.dat',param,value1,value2,format='(a,d,d)',/silent

      errvals=(value2-value1)/2.
      case breaks of
          0: begin 
              norm=value[3]
              normerr=errvals[1]
              pow1=value[2]
              powerr=errvals[0]
              chisq=value[4]
              dof=value[5]
              p=[norm,pow1]
              perror=[normerr,powerr]
              y=norm*time^(-pow1)
              t=time
              leg='Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(powerr,3)
          end
          1: begin 
              norm=value[7]
              normerr=errvals[3]
              pow1=value[4]
              pow1err=errvals[0]
              pow2=value[6]
              pow2err=errvals[2]
              break1=value[5]
              break1err=errvals[1]
              chisq=value[8]
              dof=value[9]
              p=[norm,pow1,break1,pow2]
              perror=[normerr,pow1err,break1err,pow2err]
              w1=where(time gt 0 and time lt break1,nw1)
              w2=where(time gt break1)
              t=[time[w1],break1,time[w2]]
              y=[norm*t[0:nw1]^(-pow1),norm*break1^(pow2-pow1)*t[nw1+1:*]^(-pow2)]
              leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
                   'Breaktime = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
                   'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3)]
              oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
          end
          2: begin 
              norm=value[11]
              normerr=errvals[5]
              pow1=value[6]
              pow1err=errvals[0]
              pow2=value[8]
              pow2err=errvals[2]
              pow3=value[10]
              pow3err=errvals[4]
              break1=value[7]
              break1err=errvals[1]
              break2=value[9]
              break2err=errvals[3]
              chisq=value[12]
              dof=value[13]
              p=[norm,pow1,break1,pow2,break2,pow3]
              perror=[normerr,pow1err,break1err,pow2err,break2err,pow3err]
              w1=where(time gt 0 and time lt break1,nw1)
              w2=where(time gt break1 and time lt break2,nw2)
              w3=where(time gt break2)
              t=[time[w1],break1,time[w2],break2,time[w3]]
              y=[norm*t[0:nw1]^(-pow1),norm*break1^(pow2-pow1)*t[nw1+1:nw1+nw2]^(-pow2),$
                 norm*break1^(pow2-pow1)*break2^(pow3-pow2)*t[nw1+nw2+1:*]^(-pow3)]
              leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
                   'Breaktime1 = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
                   'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3),$
                   'Breaktime2 = '+ntostr(break2)+' '+!tsym.plusminus+' '+ntostr(break2err),$
                   'Pow3'+' = '+sigfig(pow3,3)+' '+!tsym.plusminus+' '+sigfig(pow3err,3)]
              oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
              oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2

          end 
          3: begin 
              norm=value[15]
              normerr=errvals[7]
              pow1=value[8]
              pow1err=errvals[0]
              pow2=value[10]
              pow2err=errvals[2]
              pow3=value[12]
              pow3err=errvals[4]
              pow4=value[14]
              pow4err=errvals[6]
              break1=value[9]
              break1err=errvals[1]
              break2=value[11]
              break2err=errvals[3]
              break3=value[13]
              break3err=errvals[5]
              chisq=value[16]
              dof=value[17]
              p=[norm,pow1,break1,pow2,break2,pow3,break3,pow4]
              perror=[normerr,pow1err,break1err,pow2err,break2err,pow3err,break3err,pow4err]
              w1=where(time gt 0 and time lt break1,nw1)
              w2=where(time gt break1 and time lt break2,nw2)
              w3=where(time gt break2 and time lt break3,nw3)
              w4=where(time gt break3,nw4)
              t=[time[w1],break1,time[w2],break2,time[w3],break3,time[w4]]
              y=[norm*t[0:nw1]^(-pow1),$
                 norm*break1^(pow2-pow1)*t[nw1+1:nw1+nw2]^(-pow2),$
                 norm*break1^(pow2-pow1)*break2^(pow3-pow2)*t[nw1+nw2+1:nw1+nw2+nw3]^(-pow3),$
                 norm*break1^(pow2-pow1)*break2^(pow3-pow2)*break3^(pow4-pow3)*t[nw1+nw2+nw3+1:*]^(-pow4)]
              leg=['Pow1'+' = '+sigfig(pow1,3)+' '+!tsym.plusminus+' '+sigfig(pow1err,3),$
                   'Breaktime1 = '+ntostr(break1)+' '+!tsym.plusminus+' '+ntostr(break1err),$
                   'Pow2'+' = '+sigfig(pow2,3)+' '+!tsym.plusminus+' '+sigfig(pow2err,3),$
                   'Breaktime2 = '+ntostr(break2)+' '+!tsym.plusminus+' '+ntostr(break2err),$
                   'Pow3'+' = '+sigfig(pow3,3)+' '+!tsym.plusminus+' '+sigfig(pow3err,3),$
                   'Breaktime3 = '+ntostr(break3)+' '+!tsym.plusminus+' '+ntostr(break3err),$
                   'Pow4'+' = '+sigfig(pow4,3)+' '+!tsym.plusminus+' '+sigfig(pow4err,3)]
              oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
              oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2
              oplot,[break3,break3],[1e-8,1e5],color=!yellow,line=2
         
          end
     
      endcase 
     
      leg=[leg,$
           'Norm = '+ntostr(norm)+' '+!tsym.plusminus+' '+ntostr(normerr),$
           !tsym.chi+'!U2!N/dof = '+sigfig(chisq/dof,4),$
           'dof = '+ntostr(fix(dof))]
      
      if not keyword_set(noplot) then begin 
         oplot,t,y,color=!green
         legend,leg,box=0,/top,/right
         
         doagain='y'
         input,'Is this an acceptable fit? (y/n) ',doagain,'y'
         if doagain eq 's' then stop
         if doagain eq 'n' then begin
            slope=-999
            goto,refit
         endif 

         ;;write output ps file
         begplot,name='lc_fit_plot.ps',/landscape,/color
         plot_base_lc,/ps,_extra=_extra
         oplot,t,y,color=!green
         legend,leg,box=0,/top,/right
         endplot
      endif 
  endif else print,'Fit no good ... skipping'
  yfit=y
  
  return
end 
