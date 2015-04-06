pro plot_plot_lc,time,cts,error,a,sigma,_extra=_extra,noplot=noplot,nofit=nofit
  
  if not keyword_set(nofit) then begin 
     A=[10000.,-1.0]
     weights=1./error^2
     fit=mpcurvefit(time,cts,weights,A,SIGMA,FUNCTION_NAME='pwr',/noder,parinfo=parinfo,chisq=chisq)
  endif 
  
  if not keyword_set(noplot) then begin 
     defsymbols
     ploterror,time,cts,error,psym=1,/xlog,/ylog,xrange=[100,max(time)],yrange=[1e-8,max(cts)],xtitle='T-T0 (s)',ytitle='Rate (cts s!U-1!N)',_extra=_extra
     fit=a[0]*time^a[1]
     oplot,time,fit
     legend,[!tsym.alpha+' = '+ntostr(round(a[1]*100.)/100.,5)+!tsym.plusminus+ntostr(round(sigma[1]*100.)/100.,4)],/top,/right,box=0
     
  endif
  
;  print,a
  return
end 

pro plot_lc,fname,x_src,y_src,rad_src,x_back,y_back,rad_back,ct_per_bin,btime,time,cts,error,_extra=_extra,noplot=noplot ;,grade_sel (518,453,486,518,20,588,602,40,20)

if n_elements(fname) gt 1 then begin
    data_events=mrdfits(fname(0),1,hd_evt)
    data_gti=mrdfits(fname(0),2,hd_gti)
;make arrays from structures
    data_gti_start=data_gti.start
    data_gti_stop=data_gti.stop
    data_events_x=data_events.x
    data_events_y=data_events.y
    data_events_time=data_events.time
    src_ind=where(sqrt((data_events_x-x_src(0))^2.+(data_events_y-y_src(0))^2.) lt rad_src) 
    data_src_x=data_events_x(src_ind)
    data_src_y=data_events_y(src_ind)
    data_src_time=data_events_time(src_ind)
    
    back_ind=where(sqrt((data_events_x-x_back(0))^2.+(data_events_y-y_back(0))^2.) lt rad_back) 
    data_back_x=data_events_x(back_ind)
    data_back_y=data_events_y(back_ind)
    data_back_time=data_events_time(back_ind)

    for n=1,n_elements(fname)-1 do begin
        data_events_new=mrdfits(fname(n),1,hd_evt)
        data_gti_new=mrdfits(fname(n),2,hd_gti)
;        data_events=[data_events,data_events_new]
;        data_gti=[data_gti,data_gti_new]
        data_gti_start=[data_gti_start,data_gti_new.start]
        data_gti_stop=[data_gti_stop,data_gti_new.stop]
;;        data_events_x=[data_events_x,data_events_new.x]
;;        data_events_y=[data_events_y,data_events_new.y]
;;        data_events_time=[data_events_time,data_events_new.time]

;;        src_ind=where(sqrt((data_events_x-x_src(n))^2.+(data_events_y-y_src(n))^2.) lt rad_src) 
;;        data_src_x=[data_src_x,data_events_x(src_ind)]
;;        data_src_y=[data_src_y,data_events_y(src_ind)]
;;        data_src_time=[data_src_time,data_events_time(src_ind)]
    
;;        back_ind=where(sqrt((data_events_x-x_back(n))^2.+(data_events_y-y_back(n))^2.) lt rad_back) 
;;        data_back_x=[data_back_x,data_events_x(back_ind)]
;;        data_back_y=[data_back_y,data_events_y(back_ind)]
;;        data_back_time=[data_back_time,data_events_time(back_ind)]

        data_events_x_new=data_events_new.x
        data_events_y_new=data_events_new.y
        data_events_time_new=data_events_new.time
        src_ind=where(sqrt((data_events_x_new-x_src(n))^2.+(data_events_y_new-y_src(n))^2.) lt rad_src)
        data_src_x=[data_src_x,data_events_x_new(src_ind)]
        data_src_y=[data_src_y,data_events_y_new(src_ind)]
        data_src_time=[data_src_time,data_events_time_new(src_ind)]
        back_ind=where(sqrt((data_events_x_new-x_back(n))^2.+(data_events_y_new-y_back(n))^2.) lt rad_back)
        data_back_x=[data_back_x,data_events_x_new(back_ind)]
        data_back_y=[data_back_y,data_events_y_new(back_ind)]
        data_back_time=[data_back_time,data_events_time_new(back_ind)]
        

;        print,'ev_x is ',n_elements(data_events_x)
    endfor
endif

if n_elements(fname) eq 1 then begin
    data_events=mrdfits(fname,1,hd_evt)
    data_gti=mrdfits(fname,2,hd_gti)
    data_gti_start=data_gti.start
    data_gti_stop=data_gti.stop
    data_events_x=data_events.x
    data_events_y=data_events.y
    data_events_time=data_events.time
    src_ind=where(sqrt((data_events_x-x_src)^2.+(data_events_y-y_src)^2.) lt rad_src) 
    data_src_x=data_events_x(src_ind)
    data_src_y=data_events_y(src_ind)
    data_src_time=data_events_time(src_ind)
    
    back_ind=where(sqrt((data_events_x-x_back)^2.+(data_events_y-y_back)^2.) lt rad_back) 
    data_back_x=data_events_x(back_ind)
    data_back_y=data_events_y(back_ind)
    data_back_time=data_events_time(back_ind)

endif

;src_ind=where(sqrt((data_events_x-x_src)^2.+(data_events_y-y_src)^2.) lt rad_src) 
;data_src_x=data_events_x(src_ind)
;data_src_y=data_events_y(src_ind)
;data_src_time=data_events_time(src_ind)

;back_ind=where(sqrt((data_events_x-x_back)^2.+(data_events_y-y_back)^2.) lt rad_back) 
;data_back_x=data_events_x(back_ind)
;data_back_y=data_events_y(back_ind)
;data_back_time=data_events_time(back_ind)

data_src_rebin=dblarr(n_elements(data_src_x),2)
err_bin=dblarr(n_elements(data_src_x))

n=0.d
rebin_ctr=0.d
while n lt n_elements(data_src_x) do begin
;print,n
    cts_in_bin=0.
    run_time=0.d
    avg_time=0.d
    start_time_bin=data_src_time(n)
    start_gti_ind=where(data_gti_start le data_src_time(n) and data_gti_stop ge data_src_time(n))
    while (cts_in_bin lt ct_per_bin and n lt n_elements(data_src_x)) do begin
;;        curr_gti_ind=where(data_gti_start le data_src_time(n) and data_gti_stop ge data_src_time(n))
;;        curr_gti_start=data_gti_start(curr_gti_ind)
;;        curr_gti_stop=data_gti_stop(curr_gti_ind)
;        run_time=run_time+(data_src_time(n)-start_time_bin)
;;        if curr_gti_ind ne start_gti_ind then begin; calculate deadtime
;;            dead_time=0.d
;;            for m=start_gti_ind(0),curr_gti_ind(0)-1 do dead_time=dead_time+(data_gti_start(m+1)-data_gti_stop(m))
;;        endif
;;        if curr_gti_ind eq start_gti_ind then dead_time=0.d
;        run_time=run_time-dead_time
        avg_time=avg_time+data_src_time(n)
;        print,data_src_x(n),data_src_y(n),data_src_time(n)-1.4d8,avg_time
        n=n+1.d
        cts_in_bin=cts_in_bin+1.d
;        print,n
    endwhile
    stop_time_bin=data_src_time(n-1)
    curr_gti_ind=where(data_gti_start le data_src_time(n-1) and data_gti_stop ge data_src_time(n-1))
    curr_gti_start=data_gti_start(curr_gti_ind)
    curr_gti_stop=data_gti_stop(curr_gti_ind)
;        run_time=run_time+(data_src_time(n)-start_time_bin)
    if curr_gti_ind ne start_gti_ind then begin ; calculate deadtime
        dead_time=0.d
;        print,'start_gti is ',start_gti_ind(0),' curr_gti_ind is ',curr_gti_ind(0)-1
        for m=start_gti_ind(0),curr_gti_ind(0)-1 do begin
            dead_time=dead_time+(data_gti_start(m+1)-data_gti_stop(m))
;            print,'deadtime is ',dead_time
        endfor
    endif
    if curr_gti_ind eq start_gti_ind then dead_time=0.d
                                ;get background
    w=where(data_back_time gt start_time_bin and data_back_time lt stop_time_bin,nw)
    asab=(rad_src^2./rad_back^2.)
    back_cts_bin=asab*nw
    print,nw
;    back_cts_bin=(rad_src^2./rad_back^2.)*n_elements(where(data_back_time gt start_time_bin and data_back_time lt stop_time_bin))

    run_time=run_time+(data_src_time(n-1)-start_time_bin)
    run_time=run_time-dead_time
;    print,'cts is ',cts_in_bin,' deadtime is ',dead_time,' runtime is ',run_time,' back cts is ',back_cts_bin
;    if cts_in_bin gt ct_per_bin/2. then begin
;        print,'in'
	;do data correction to data ctrate
        data_src_rebin(rebin_ctr,*)=[avg_time/cts_in_bin,(cts_in_bin-back_cts_bin)/run_time]
        
        err_bin[rebin_ctr]=sqrt(cts_in_bin+asab*back_cts_bin)/run_time
        rebin_ctr=rebin_ctr+1.d
        print,start_time_bin-btime,data_src_time(n-1)-btime,run_time,back_cts_bin,cts_in_bin,format='(i,i,f10.2,f10.3,i)'
;    endif
endwhile
data_src_rebin=data_src_rebin(where(data_src_rebin(*,0) ne 0.),*)
;cbin=cbin[where(data_src_rebin(*,0) ne 0.)]
;err=sqrt(ct_per_bin)/ct_per_bin
;err=sqrt(cbin)/cbin
time=data_src_rebin(*,0)-btime
cts=data_src_rebin(*,1)
error=err_bin
;error=err*cts
w=where(cts ne 0)
time=time[w]
cts=cts[w]
error=error[w]


if not keyword_set(noplot) then plot_plot_lc,time,cts,error

;set_plot,'x'
;set_plot,'ps'
;device,filename='xlc.ps'
goto,jump
!p.multi=[0,1,2]
plot,data_src_rebin(*,0)-btime,data_src_rebin(*,1),psym=1,/xlog,/ylog,xtitle='seconds since BAT trigger',ytitle='cts/s'

;print,'err is ',err
for a=0,n_elements(data_src_rebin(*,0))-1 do oplot,[data_src_rebin(a,0)-btime,data_src_rebin(a,0)-btime],[data_src_rebin(a,1)-err*data_src_rebin(a,1),data_src_rebin(a,1)+err*data_src_rebin(a,1)]
;device,/close

parinfo = replicate({value:0.D, fixed:0, limited:[0,0],limits:[0.D,0], step:0.D }, 2)
parinfo.value=a

dat_ind=where(data_src_rebin(*,0)-btime gt 50000.d)
A=[10000.,-1.0]
;weights=1./dat(*,4)
;weights=weights/max(weights)
;weights=fltarr(n_elements(data_src_rebin(*,0)))
weights=fltarr(n_elements(dat_ind))
weights(*)=1.
fit=mpcurvefit(data_src_rebin(dat_ind,0)-btime,data_src_rebin(dat_ind,1),weights,A,SIGMA,FUNCTION_NAME='pwr',/noder,parinfo=parinfo,chisq=chisq)

;oplot,dat(*,1),dat(*,3),psym=1
;for n=0,n_elements(data_src_rebin(*,0))-1 do oplot,[dat(n,1),dat(n,1)],[dat(n,3)+dat(n,4),dat(n,3)-dat(n,4)]
oplot,data_src_rebin(dat_ind,0)-btime,fit
oplot,[1.d,1.d7],[A(0),A(0)*1.d7^A(1)]

;print,A
ind_ind=fltarr(100)
ind_ind(*)=-99.
ign_out=''
read,ign_out,prompt='ignore outliers?'
if ign_out eq 'y' then read,n_out,prompt='how many good datapoints?'
ind_ind=fltarr(n_out)
if ign_out eq 'y' then read,ind_ind,prompt='name points to keep'

!p.multi=[0,1,2]
if ign_out eq 'y' then begin
    dat_ind=where(data_src_rebin(*,0)-btime gt 50000.d)
    dat_ind=dat_ind(ind_ind)
    A=[10000.,-1.0]
;weights=1./dat(*,4)
;weights=weights/max(weights)
;weights=fltarr(n_elements(data_src_rebin(*,0)))
    weights=fltarr(n_elements(dat_ind))
    weights(*)=1.
    fit=mpcurvefit(data_src_rebin(dat_ind,0)-btime,data_src_rebin(dat_ind,1),weights,A,SIGMA,FUNCTION_NAME='pwr',/noder,parinfo=parinfo,chisq=chisq)
    plot,data_src_rebin(*,0)-btime,data_src_rebin(*,1),psym=1,/xlog,/ylog
    err=sqrt(ct_per_bin)/ct_per_bin
;    print,'err is ',err
    for b=0,n_elements(data_src_rebin(*,0))-1 do oplot,[data_src_rebin(b,0)-btime,data_src_rebin(b,0)-btime],[data_src_rebin(b,1)-err*data_src_rebin(b,1),data_src_rebin(b,1)+err*data_src_rebin(b,1)]
    
    oplot,data_src_rebin(dat_ind,0)-btime,fit
    oplot,[1.d,1.d7],[A(0),A(0)*1.d7^A(1)]
;    print,A
    
 endif

plot,data_src_rebin(*,0)-btime,abs(data_src_rebin(*,1)-A(0)*(data_src_rebin(*,0)-btime)^A(1)),psym=1,/xlog,/ylog

jump:

end

