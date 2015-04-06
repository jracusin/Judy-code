@fit_functions
pro fix_crap
  
  cd,!mdata
  dir=file_search('GRB*')
  g=0
  ndir=n_elements(dir)
  stop
  
  for s=g,ndir-1 do begin 
     cd,dir[s]
     print,dir[s],s
     
     ;;;read lc_fit_out_idl_int2.dat
     ;;;run conf_error with proper delchi0
     ;;;re-write lc_fit_out
     file='lc_fit_out_idl_int4.dat'
     if exist(file) then begin 
        lcfile='lc_newout_noflares.txt'
        if not exist(lcfile) then lcfile='lc_newout_phil.txt'
        read_lcfit,file,pname,p,perror0,chisq,dof,breaks
        if pname[0] ne 'no fit' and pname[0] ne 'nofit' then begin 
           lc=lcout2fits(lcfile)
;           goto,skip
           perror1=fltarr(n_elements(p))
           for i=0,n_elements(p)-1 do begin
              wf=where(finite(perror0[*,i]) and perror0[*,i] gt 0.)
              perror1[i]=min(perror0[wf,i])
           endfor 
           case breaks of
              0: begin
                 mo='intpow'
                 pmin0=[0.,-10.]
                 log=[1,0]
              end 
              1: begin
                 mo='intbknpow'
                 pmin0=[0.,-10.,0,-10.]
                 log=[1,0,1,0]
              end 
              2: begin
                 mo='intbkn2pow'
                 pmin0=[0.,-10.,0,-10.,0.,-10.]
                 log=[1,0,1,0,1,0]
              end 
              3: begin
                 mo='intbkn3pow'
                 pmin0=[0.,-10.,0,-10.,0.,-10.,0.,-10.]
                 log=[1,0,1,0,1,0,1,0]
              end 
           endcase 
           cts=lc.src_rate
           err=lc.src_rate_err
           time=lc.time
           timerr=fltarr(2,n_elements(time))
           timerr[0,*]=time-lc.tstart
           timerr[1,*]=lc.tstop-time
           w=where(cts gt 0 and finite(err) and err ne 0)
           time=time[w] & timerr=timerr[*,w] & cts=cts[w] & err=err[w]
           
           tt=dblarr(2,n_elements(time))
           tt[0,*]=time-timerr[0,*]
           tt[1,*]=time+timerr[1,*]
           
;           delchi0=chisqr_cvf(0.1,n_elements(p)-1)
           ;;; change n params of interest to 1 if single PL, and 3 if others
;           if n_elements(p) eq 2 then delchi0=chisqr_cvf(0.1,1) else delchi0=chisqr_cvf(0.1,3)
           delchi0=chisqr_cvf(0.1,1)
           print,delchi0
           colprint,pname,p,perror1
           
           begplot,name='lc_fit_err_plots.ps',/land
           conf_error,tt,cts,err,p*1.,perror1*1.,mo,perror,bestfit,pvarunit,bestchisq,yfit,log=log,pmin0=pmin0,delchi0=delchi0,/doplot
           endplot
           
           ;;write output fit file
           openw,lun,'lc_fit_out_idl_int6.dat',/get_lun
           norm=p[0]
           normerr=perror[*,0]
           pname=pname[1:*]
           for i=0,n_elements(p)-2 do begin
              j=i+1
              printf,lun,pname[i]+' '+ntostr(p[j])+' '+ntostr(perror[0,j])+' '+ntostr(perror[1,j])
           endfor
           printf,lun,'Norm '+ntostr(norm)+' '+ntostr(normerr[0])+' '+ntostr(normerr[1])
           printf,lun,'Chisq '+ntostr(chisq)
           printf,lun,'dof '+ntostr(fix(dof))
           close,lun
           free_lun,lun
;           skip:
        endif else spawn,'cp lc_fit_out_idl_int5.dat lc_fit_out_idl_int6.dat' 
     endif  
;     if exist('spec') then begin 
;        read_specfit,spec,dir='spec'
;        w=where(spec.nev lt 60 and spec.nev2 lt 60,nw)
;        if nw gt 0 then stop
;     endif else print,'No specfit'
     
;;      xrtdir=file_search('00*-xrt')
;;      pccl=file_search(xrtdir+'/sw*pcw*po_cl.evt')
;;      pos=strpos(pccl,'w3')
;;      w=where(pos eq -1)
;;      pccl=pccl[w]
;;      ndir=strmid(xrtdir,9,2)
;;      npc=strmid(pccl,9,2)
;;      dont_match,ndir,npc,m1,m2
;;      if n_elements(pccl) ne n_elements(xrtdir) then begin
;;         for i=0,max([ndir*1,npc*1]) do begin
;;            w1=where(ndir*1 eq i,nw1)
;;            w2=where(npc*1 eq i,nw2)
;;            p=''
;;            if nw1 gt 0 then p=xrtdir[w1]+' '
;;            if nw2 gt 0 then p=p+pccl[w2]
;;            print,p
;;         endfor 
;; ;        colprint,xrtdir[m1],pccl[m2]
;;         stop
;;      endif 
     
     cd,'..'
  endfor 
  
  
;;      lcfile='lc_newout_noflares.txt'
;;      if not exist(lcfile) then lcfile='lc_newout.txt'
;;      fitfile='lc_fit_out_idl_int.dat'
  
;;      if exist(lcfile) and exist(fitfile) then begin

;;         if numlines(fitfile) gt 2 then begin 
;;            lc=lcout2fits(lcfile)
;;            wneg=where(lc.src_rate lt 0.,nwneg)
;;            if nwneg gt 0 then begin
;;               print,'negative counts rate'
;;               stop
;;            endif 
;;            read_lcfit,fitfile,pnames,newp,perror,chisq,dof,breaks
;;            case breaks of
;;               0: begin
;;                  mo='intpow'
;;                  pmin0=[0.,-10.]
;;                  log=[1,0]
;;               end 
;;               1: begin 
;;                  mo='intbknpow'
;;                  pmin0=[0.,-10.,0,-10.]
;;                  log=[1,0,1,0]
;;               end 
;;               2: begin 
;;                  mo='intbkn2pow'
;;                  pmin0=[0.,-10.,0,-10.,0.,-10.]
;;                  log=[1,0,1,0,1,0]
;;               end 
;;               3: begin 
;;                  mo='intbkn3pow'
;;                  pmin0=[0.,-10.,0,-10.,0.,-10.,0.,-10.]
;;                  log=[1,0,1,0,1,0,1,0]
;;               end 
;;            endcase 
;;            wdet=where(lc.src_rate_err gt 0,nwdet)
;; ;           timerr=(lc.tstop-lc.tstart)/2.
;; ;           fit_pow_model,lc[wdet].time,lc[wdet].src_rate,timerr[wdet],lc[wdet].src_rate_err,p,mo,pnames,yfit,newp,perror,chisq,dof,weights,lc[wdet].src_counts,lc[wdet].tot_back_cts,status=status
  
;;            w=where(finite(perror) eq 0 or perror lt 0,nw)
;;            if nw gt 0 then perror[w]=0.;newp[w]*0.5
;;            perror0=fltarr(n_elements(newp))
;;            for i=0,n_elements(newp)-1 do begin
;;               w=where(perror[*,i] ne 0.,nw)
;;               if nw eq 2 then perror0[i]=min(perror[*,i])
;;               if nw eq 1 then perror0[i]=perror[w,i]
;;            endfor 
  
;;            tt=dblarr(2,nwdet)
;;            tt[0,*]=lc[wdet].tstart    ;time-timerr
;;            tt[1,*]=lc[wdet].tstop     ; time+timerr

;;            conf_error,tt,lc[wdet].src_rate,lc[wdet].src_rate_err,newp,perror0,mo,perror2,bestfit,pvarunit,bestchisq,yfit,pmin0=pmin0,log=log;,/doplot
;; ;           print,perror
;; ;           print,perror2
;;            perror=perror2
;;            openw,lun,'lc_fit_out_idl_int2.dat',/get_lun
;;            norm=newp[0]
;;            normerr=perror[*,0]
;;            for j=1,n_elements(newp)-1 do begin
;; ;              j=i+1
;;               printf,lun,pnames[j]+' '+ntostr(newp[j])+' '+ntostr(perror[0,j])+' '+ntostr(perror[1,j])
;;            endfor
;;            printf,lun,'Norm '+ntostr(norm)+' '+ntostr(normerr[0])+' '+ntostr(normerr[1])
;;            printf,lun,'Chisq '+ntostr(chisq)
;;            printf,lun,'dof '+ntostr(fix(dof))
;;            close,lun
;;            free_lun,lun
;;         endif 
;;      endif 
;;      cd,'..'
;;   endfor 
  
  
;;   done=file_search('GRB*/lc_newout.txt')
;;   donedir=strarr(n_elements(done))
;;   ready=file_search('GRB*/lc_wrap3.par')
;;   readydir=strarr(n_elements(ready))
;;   for i=0,n_elements(done)-1 do begin 
;;      ddir=str_sep(done[i],'/')
;;      donedir[i]=ddir[0]
;;   endfor 
;;   for i=0,n_elements(ready)-1 do begin
;;      rdir=str_sep(ready[i],'/')
;;      readydir[i]=rdir[0]
;;   endfor 
;;   print,ndir,n_elements(done),n_elements(ready)
  
;;   csvfile='new_grbs.csv'
;;   readcol,!mdata+csvfile,grbname,tid,battime,wt,format='(a,l,d,i)',delim=','
  
;;   dont_match,strtrim(donedir,2),'GRB'+grbname,m1,m2
;;   print,'NOT DONE'
;;   help,m1,m2
;;   print,m2,grbname[m2]
  
;;   print,'NOT READY'
;;   dont_match,strtrim(readydir,2),'GRB'+grbname,m1,m2
  
;;   help,m1,m2
;;   stop
;  print,m2,grbname[m2]
  
;  stop
;  openw,lun,'crap.txt',/get_lun
;;;  for i=g,ndir-1 do begin
;;;     cd,dir[i]
;;;     print,dir[i],i
;;;     subdir=file_search('00*')
;;;     s=strpos(subdir,'xrt')
;;;     w=where(s eq -1,nw)
;;;     if nw gt 0 then begin 
;;;        subdir=subdir[w]
  
;;;        f=0
;;;        for j=0,n_elements(subdir)-1 do begin
;;;           xdir=subdir[j]+'-xrt'
;;;           if not exist(xdir) then f=[f,j]
;;;        endfor 
;;;        if n_elements(f) gt 1 then begin
;;;           f=f[1:*]
;;;           run_xrtpipeline,w=f
;;;        endif 
;;;     endif 
;;     year='20'+strmid(dir[i],3,2)
;;     month=strmid(dir[i],5,2)
;;     subdir=file_search('00*')
;     s=strpos(subdir,'xrt')
;     w=where(s eq -1)
;     subdir=subdir[w]
;;     tid=strmid(subdir[0],2,6)*1L
;;     get_sdc_data,year,month,tid
;     if not exist(subdir[0]+'/xrt') then begin
;        print,' no xrt dir?'
;        stop
;        spawn,'rm '+subdir[0]
;     endif 
;;     newdir=file_search('00*')
;;     if n_elements(subdir) ne n_elements(newdir) then printf,lun,dir[i],tid
;     xrt=file_search('00*-xrt')
;     nxrt=n_elements(xrt)
;     lastxrt=xrt[nxrt-1]
;     lastseg=strmid(lastxrt,9,2)
;     tid=strmid(lastxrt,2,6)*1L
;     if tid lt 100000 then nxrt=nxrt+1
;     if lastseg ne nxrt-1 and lastseg ne '00' then begin
;        print,dir[i],i
;        print,xrt
;        spawn,'ls'
;        stop
;     endif 
;;;     cd,'..'
;;;  endfor 
;;;  close,/all
  
  return
end 
