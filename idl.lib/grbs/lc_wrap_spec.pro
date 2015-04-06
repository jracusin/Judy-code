pro lc_wrap,parfname,bin=bin,lorella=lorella,debug=debug,srcrad=srcrad,sper=sper
;LC_wrap,['../../GRB060124_test/sw00178750000xwtw2po_cl.evt','../../GRB060124_test/sw00178750000xpcw2po_cl.evt','../../GRB060124_test/sw00178750005xpcw2po_cl.evt'],'05:08:27.26','69:44:26.42','05:07:43.631','69:43:31.30',159810892.8d,bin=[100,100,100,250]

;LC_wrap,['sw00116116000xwtw2po_cl.evt','sw00116116000xpcw4po_cl.evt','sw00116116001xpcw4po_cl.evt'],'09:30:10.046','16:59:46.98','09:30:31.087','17:03:28.51',136718739.97d
;LC_wrap,['sw00116116000xwtw2po_cl.evt','sw00116116000xpcw4po_cl.evt'],'09:30:10.046','16:59:46.98','09:30:31.087','17:03:28.51',136718739.97d
;LC_wrap,['wt_orb1.evt','sw00211117991xpcw2po_cl.evt'],'21:31:44.9','02:53:08.5',323.05052,2.934253,169956680.d

;READ THE INPUT PARAM FILE TO GET FILENAMES AND POSITIONS
lc_readpars,parfname,flist,ra,dec,raback,decback,trig_time

;DETERMINE WHICH FILES ARE WT AND WHICH ARE PC
ftype=intarr(n_elements(flist))
for n=0,n_elements(flist)-1 do begin
    dattmp=mrdfits(flist(n),0,hd)
    mode=sxpar(hd,'DATAMODE')
    if mode eq 'PHOTON  ' then ftype(n)=1
    if mode eq 'WINDOWED' then ftype(n)=0
endfor
;stop
;CONCATENATE THE FILELIST AND GENERATE EVT, GTI, AND BADPIX STRUCT'S
concat_evt_060222,flist,evt,gti,bad,sper=sper
;stop
;CONVERT INPUT RA,DEC TO DEGREES IF INPUT AS SEXIGESIMAL
;postest=size(ra)
;if postest(1) eq 7 then begin
spos=strpos(ra,':')
if spos[0] ne -1 then begin 
   convert_from_sexigesimal,ra,dec,ra_deg,dec_deg
   ra=ra_deg
   dec=dec_deg
endif
;postest=size(raback)
;if postest(1) eq 7 then begin
bpos=strpos(raback,':')
if bpos[0] ne -1 then begin 
   convert_from_sexigesimal,raback,decback,ra_deg,dec_deg
   raback=ra_deg
   decback=dec_deg
endif

;FIND THE DETECTOR AND BACKGROUND POSITION OF THE SOURCE RA AND DEC IN EACH CORRESPONDING
;FILE
src_skyxs=intarr(n_elements(flist))
src_skyys=intarr(n_elements(flist))
back_skyxs=intarr(n_elements(flist))
back_skyys=intarr(n_elements(flist))

for n=0,n_elements(flist)-1 do begin
    if ftype(n) eq 0 then begin
        find_wt_srcdetpos,flist(n),ra,dec,x_int,y_int,pa,ra_int,dec_int
        find_wt_srcdetpos,flist(n),raback,decback,x_intback,y_intback,paback,ra_intback,dec_intback
        print,flist(n),x_int,y_int,pa,ra_int,dec_int,x_intback,y_intback,paback,ra_intback,dec_intback
;        stop
    endif
    if ftype(n) eq 1 then begin
        find_pc_srcdetpos,flist(n),ra,dec,x_int,y_int,pa
        find_pc_srcdetpos,flist(n),raback,decback,x_intback,y_intback,paback
        print,flist(n),x_int,y_int,pa,x_intback,y_intback,paback
    endif
    src_skyxs(n)=x_int
    src_skyys(n)=y_int
    back_skyxs(n)=x_intback
    back_skyys(n)=y_intback
endfor
;stop
if n_elements(flist) gt 1 then if (strmid(flist(0),14,2) eq 'wt' and  strmid(flist(0),0,13) eq strmid(flist(1),0,13) and strmid(flist(1),14,2) eq 'pc') then begin
    src_skyxs(0)=src_skyxs(1)
    src_skyys(0)=src_skyys(1)
;    back_skyxs(0)=back_skyxs(1)
;    back_skyys(0)=back_skyys(1)
endif

;DETERMINE THE START AND STOP TIMES OF EACH ORBIT IN THE INPUT FILES
psf_corr_ctr=0L
;for n=0,n_elements(flist)-1 do begin
;    det_orbit,flist(n),start_orb_arr,stop_orb_arr
    det_orbit_gti,gti,ftype,start_orb_arr,stop_orb_arr,orbit_fnum
;stop
    psf_corr_arr=fltarr(n_elements(start_orb_arr))
    psf_corr_back_arr=fltarr(n_elements(start_orb_arr))
;    if n gt 0 then begin
;        psf_corr_arr_old=psf_corr_arr
;        psf_corr_arr=fltarr(n_elements(psf_corr_arr_old)+n_elements(start_orb_arr))
;        psf_corr_arr(0:n_elements(psf_corr_arr_old)-1)=psf_corr_arr_old
;    endif
    for m=0,n_elements(start_orb_arr)-1 do begin
        print,'start/stop:',start_orb_arr(m),stop_orb_arr(m),m,' of ',n_elements(start_orb_arr),orbit_fnum(m)
;        if m eq 24 then print,'start/stop:',start_orb_arr(m),stop_orb_arr(m),m,' of ',n_elements(start_orb_arr),orbit_fnum(m)-1
        ind=where(evt.time ge start_orb_arr(m) and evt.time le stop_orb_arr(m)); and evt.ftype eq ftype(n))
	if keyword_set(debug) then ind=where(evt.time ge start_orb_arr(m) and evt.time le stop_orb_arr(m) and evt.x ne -1 and evt.y ne -1)
;CALCULATE THE DETX,DETY CENTROID OF THE SKY POSITION FOR THIS ORBIT
;stop
        if n_elements(ind) gt 4 then begin
            find_src_detpos,src_skyxs(orbit_fnum(m)),src_skyys(orbit_fnum(m)),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;            if m eq 24 then find_src_detpos,src_skyxs(orbit_fnum(m)-1),src_skyys(orbit_fnum(m)-1),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;CALCULATE A PSF GRID (100x100) ASSUMING ENERGY=2keV and OFFAXIS ANGLE=0.
            model_psf,2.,0.,ef
;DETERMINE THE POSITION OF THE BADPIXELS/COLUMNS WRT THE PSF AND
;CALCULATE THE CORRESPONDING CORRECTION FACTOR
            modify_psf_wbadpix,ef,detxcrf,detycrf,psfout,psf_corr_fact,ftype(orbit_fnum(m))
            spawn,'mv psf_corr_diagnostic.ps psf_corr_diagnostic_orb'+strtrim(string(m),2)+'.ps'
            print,psf_corr_fact,detxcrf,detycrf
        endif
        if n_elements(ind) le 4 then psf_corr_fact=1.; IF NOT ENOUGH EVENTS IN ORBIT FOR A FIT THEN ASSUME PSF_CORR=1.0
        if n_elements(ind) le 4 then print,psf_corr_fact
        psf_corr_arr(psf_corr_ctr)=psf_corr_fact
        if ind(0) ne -1 then evt(ind).fact1=psf_corr_fact
;        psf_corr_ctr=psf_corr_ctr+1L

;CALCULATE THE DETX,DETY CENTROID OF THE BACKGROUND POSITION FOR THIS ORBIT
;stop
        if n_elements(ind) gt 4 then begin
            find_src_detpos,back_skyxs(orbit_fnum(m)),back_skyys(orbit_fnum(m)),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;            if m eq 24 then find_src_detpos,back_skyxs(orbit_fnum(m)-1),back_skyys(orbit_fnum(m)-1),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;CALCULATE A PSF GRID (100x100) ASSUMING ENERGY=2keV and OFFAXIS ANGLE=0.
;;            model_psf,2.,0.,ef
            ef=fltarr(2001,2001) ;NO POINT SOURCE SO FLATTEN OUT THE MODEL OF THE PSF
            ef(*)=1.
;DETERMINE THE POSITION OF THE BADPIXELS/COLUMNS WRT THE PSF AND
;CALCULATE THE CORRESPONDING CORRECTION FACTOR
            modify_psf_wbadpix,ef,detxcrf,detycrf,psfout,psf_corr_fact,ftype(orbit_fnum(m))
            spawn,'mv psf_corr_diagnostic.ps psf_corr_diagnostic_back_orb'+strtrim(string(m),2)+'.ps'
            print,psf_corr_fact,detxcrf,detycrf
        endif
        if n_elements(ind) le 4 then psf_corr_fact=1.; IF NOT ENOUGH EVENTS IN ORBIT FOR A FIT THEN ASSUME PSF_CORR=1.0
        if n_elements(ind) le 4 then print,psf_corr_fact
        psf_corr_back_arr(psf_corr_ctr)=psf_corr_fact
        if ind(0) ne -1 then evt(ind).fact1=psf_corr_fact
        psf_corr_ctr=psf_corr_ctr+1L
    endfor
;stop
;endfor

;evt_ind_wt=where(evt.ftype eq 0)
;evt_ind_pc=where(evt.ftype eq 1)

;BUILD THE EVT INDEXES FOR THE SOURCE DATA

;evt_dist=sqrt((evt.x-src_skyxs(0))^2+(evt.y-src_skyys(0))^2)
;evt_ind_wt=where(evt.ftype eq 0 and evt.fnum eq 0 and evt_dist lt 30.)
;evt_ind_pc=where(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30.)

evt_ind_pc=0.
evt_ind_wt=0.
for fctr=0,n_elements(flist)-1 do begin
    evt_dist=sqrt((evt.x-src_skyxs(fctr))^2+(evt.y-src_skyys(fctr))^2)
    evt_ind_wt_tmp=where(evt.ftype eq 0 and evt.fnum eq fctr and evt_dist lt 20.);30.)
    evt_ind_pc_tmp=where(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30.)
    if evt_ind_wt_tmp(0) ne -1 then evt_ind_wt=[evt_ind_wt,evt_ind_wt_tmp]
    if evt_ind_pc_tmp(0) ne -1 then evt_ind_pc=[evt_ind_pc,evt_ind_pc_tmp]
endfor
if n_elements(evt_ind_pc) gt 1 then evt_ind_pc=evt_ind_pc(1:*) else evt_ind_pc=-1
if n_elements(evt_ind_wt) gt 1 then evt_ind_wt=evt_ind_wt(1:*) else evt_ind_wt=-1



;BUILD THE EVT INDEXES FOR THE BACKGROUND DATA

;evt_dist_back=sqrt((evt.x-back_skyxs(0))^2+(evt.y-back_skyys(0))^2)
;evt_ind_wt_back=where(evt.ftype eq 0 and evt.fnum eq 0 and evt_dist_back lt 30.)
;evt_ind_pc_back=where(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist_back lt 30.)
evt_ind_pc_back=0.
evt_ind_wt_back=0.
for fctr=0,n_elements(flist)-1 do begin
    evt_dist_back=sqrt((evt.x-back_skyxs(fctr))^2+(evt.y-back_skyys(fctr))^2)
    evt_ind_wt_back_tmp=where(evt.ftype eq 0 and evt.fnum eq fctr and evt_dist_back lt 20.);30.)
    evt_ind_pc_back_tmp=where(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist_back lt 30.)
    if evt_ind_wt_back_tmp(0) ne -1 then evt_ind_wt_back=[evt_ind_wt_back,evt_ind_wt_back_tmp]
    if evt_ind_pc_back_tmp(0) ne -1 then evt_ind_pc_back=[evt_ind_pc_back,evt_ind_pc_back_tmp]
endfor
if n_elements(evt_ind_pc_back) gt 1 then evt_ind_pc_back=evt_ind_pc_back(1:*) else evt_ind_pc_back=-1
if n_elements(evt_ind_wt_back) gt 1 then evt_ind_wt_back=evt_ind_wt_back(1:*) else evt_ind_wt_back=-1



;CHECK TO SEE IF THERE ARE ANY -1 VALUES IN THE SOURCE INDEX ARRAYS AND IF SO
;THEN REMOVE THEM
;if evt_ind_pc(0) ne -1 then if not(n_elements(evt_ind_pc) eq 1 and evt_ind_pc(0) eq -1) then while evt_ind_pc(n_elements(evt_ind_pc)-1) eq -1 do evt_ind_pc=evt_ind_pc(0:n_elements(evt_ind_pc)-2)
;if evt_ind_wt(0) ne -1 then if not(n_elements(evt_ind_wt) eq 1 and evt_ind_wt(0) eq -1) then while evt_ind_wt(n_elements(evt_ind_wt)-1) eq -1 do evt_ind_wt=evt_ind_wt(0:n_elements(evt_ind_wt)-2)
;if n_elements(evt_ind_pc) gt 1 then while evt_ind_pc(n_elements(evt_ind_pc)-1) eq -1 do if n_elements(evt_ind_pc) gt 1 then evt_ind_pc=evt_ind_pc(0:n_elements(evt_ind_pc)-2)
;if n_elements(evt_ind_wt) gt 1 then while evt_ind_wt(n_elements(evt_ind_wt)-1) eq -1 do if n_elements(evt_ind_wt) gt 1 then evt_ind_wt=evt_ind_wt(0:n_elements(evt_ind_wt)-2)
;clean_pc_test=where(evt_ind_pc eq -1) & if clean_pc_test(0) ne -1 then evt_ind_pc=evt_ind_pc(where(evt_ind_pc ne -1))
;clean_wt_test=where(evt_ind_wt eq -1) & if clean_wt_test(0) ne -1 then evt_ind_wt=evt_ind_wt(where(evt_ind_wt ne -1))

if 0 then begin
n=0L
mone_ctr=0L
if evt_ind_pc(0) ne -1 then if not(n_elements(evt_ind_pc) eq 1 and evt_ind_pc(0) eq -1) then begin
    while n lt n_elements(evt_ind_pc) do begin
        if evt_ind_pc(n) eq -1 then begin
            for m=n,n_elements(evt_ind_pc)-2 do begin
                print,n,m,m+1, evt_ind_pc(m), evt_ind_pc(m+1)
                evt_ind_pc(m)=evt_ind_pc(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    evt_ind_pc=evt_ind_pc(0:n-1-mone_ctr)
endif
n=0L
mone_ctr=0L
if evt_ind_wt(0) ne -1 then if not(n_elements(evt_ind_wt) eq 1 and evt_ind_wt(0) eq -1) then begin
    while n lt n_elements(evt_ind_wt) do begin
        if evt_ind_wt(n) eq -1 then begin
            for m=n,n_elements(evt_ind_wt)-2 do begin
                print,n,m,m+1, evt_ind_wt(m), evt_ind_wt(m+1)
                evt_ind_wt(m)=evt_ind_wt(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    evt_ind_wt=evt_ind_wt(0:n-1-mone_ctr)
endif
endif

;CHECK TO SEE IF THERE ARE ANY -1 VALUES IN THE BACKGROUND INDEX ARRAYS AND IF SO
;THEN REMOVE THEM
;if evt_ind_pc_back(0) ne -1 then if not(n_elements(evt_ind_pc_back) eq 1 and evt_ind_pc_back(0) eq -1) then while evt_ind_pc_back(n_elements(evt_ind_pc_back)-1) eq -1 do evt_ind_pc_back=evt_ind_pc_back(0:n_elements(evt_ind_pc_back)-2)
;stop
;if evt_ind_wt_back(0) ne -1 then if not(n_elements(evt_ind_wt_back) eq 1 and evt_ind_wt_back(0) eq -1) then while evt_ind_wt_back(n_elements(evt_ind_wt_back)-1) eq -1 do evt_ind_wt_back=evt_ind_wt_back(0:n_elements(evt_ind_wt_back)-2)
;if n_elements(evt_ind_pc_back) gt 1 then while evt_ind_pc_back(n_elements(evt_ind_pc_back)-1) eq -1 do if n_elements(evt_ind_pc_back) gt 1 then evt_ind_pc_back=evt_ind_pc_back(0:n_elements(evt_ind_pc_back)-2)
;if n_elements(evt_ind_wt_back) gt 1 then while evt_ind_wt_back(n_elements(evt_ind_wt_back)-1) eq -1 do if n_elements(evt_ind_wt_back) gt 1 then evt_ind_wt_back=evt_ind_wt_back(0:n_elements(evt_ind_wt_back)-2)
;clean_pc_test=where(evt_ind_pc eq -1) & if clean_pc_test(0) ne -1 then evt_ind_pc=evt_ind_pc(where(evt_ind_pc ne -1))
;clean_wt_test=where(evt_ind_wt eq -1) & if clean_wt_test(0) ne -1 then evt_ind_wt=evt_ind_wt(where(evt_ind_wt ne -1))
if 0 then begin
n=0L
mone_ctr=0L
if evt_ind_pc_back(0) ne -1 then if not(n_elements(evt_ind_pc_back) eq 1 and evt_ind_pc_back(0) eq -1) then begin
    while n lt n_elements(evt_ind_pc_back) do begin
        if evt_ind_pc_back(n) eq -1 then begin
            for m=n,n_elements(evt_ind_pc_back)-2 do begin
                print,n,m,m+1, evt_ind_pc_back(m), evt_ind_pc_back(m+1)
                evt_ind_pc_back(m)=evt_ind_pc_back(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    evt_ind_pc_back=evt_ind_pc_back(0:n-1-mone_ctr)
endif
n=0L
mone_ctr=0L
if evt_ind_wt_back(0) ne -1 then if not(n_elements(evt_ind_wt_back) eq 1 and evt_ind_wt_back(0) eq -1) then begin
    while n lt n_elements(evt_ind_wt_back) do begin
        if evt_ind_wt_back(n) eq -1 then begin
            for m=n,n_elements(evt_ind_wt_back)-2 do begin
                print,n,m,m+1, evt_ind_wt_back(m), evt_ind_wt_back(m+1)
                evt_ind_wt_back(m)=evt_ind_wt_back(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    evt_ind_wt_back=evt_ind_wt_back(0:n-1-mone_ctr)
endif
endif

;if not(keyword_set(lorella)) then begin
    if n_elements(evt_ind_wt) gt 1 then bin_by_rules,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bin=bin
    if n_elements(evt_ind_pc) gt 1 then bin_by_rules,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bin=bin
;endif
;if keyword_set(lorella) then begin
;    if n_elements(evt_ind_wt) gt 1 then bin_by_rules_lorella,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bin=bin
;    if n_elements(evt_ind_pc) gt 1 then bin_by_rules_lorella,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bin=bin
;endif

set_plot,'x'
window,0
;CT_RATE_ARR_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PILEUP,PSF_CORR,ERROR]
if n_elements(evt_ind_wt) gt 1 then begin
    plot,ct_rate_arr_wt(0,*)-trig_time,ct_rate_arr_wt(1,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
    if n_elements(evt_ind_pc) gt 1 then oplot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*),psym=3
endif

if n_elements(evt_ind_pc) gt 1 then plot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if n_elements(evt_ind_pc) gt 1 then ct_rate_arr_pc_no_puc=ct_rate_arr_pc

if n_elements(evt_ind_pc) gt 1 then begin;PILEUP LOOP
;MAKE NOTE OF TIME RANGES IN PC WHERE COUNT RATE IS ABOVE THE PILEUP
;THRESHOLD SO WE CAN GO BACK AND RECALC THE LIGHTCURVE CORRECTING FOR
;PILEUP
    PU_thresh1=0.5              ;CTS/S
    PU_thresh2=2.0
    PU_thresh3=5.0
    PU_thresh4=10.
    PU_thresh5=25.
    PU_thresh6=70.
    
;    PU_rad1=1.                  ;PIXELS
    PU_rad1=3.
    PU_rad2=3.
    PU_rad3=5.
    PU_rad4=7.
;    PU_rad5=10.
    PU_rad=13.
    PU_rad6=15.
    
;    PU_fact1=1.1
    PU_fact1=1.76
    PU_facg2=1.76
    PU_fact3=2.66
    PU_fact4=3.69
;    PU_fact5=5.38
    PU_fact=7.20
    PU_fact6=8.46

    ;;MODS SHOULD BEGIN HERE
    
    pu_indm7=where(ct_rate_arr_pc(1,*) le 5e-4)
    pu_indm6=where(ct_rate_arr_pc(1,*) gt 5e-4 and ct_rate_arr_pc(1,*) le 1e-3)
    pu_indm5=where(ct_rate_arr_pc(1,*) gt 1e-3 and ct_rate_arr_pc(1,*) le 5e-3)
    pu_indm4=where(ct_rate_arr_pc(1,*) gt 5e-3 and ct_rate_arr_pc(1,*) le 1e-2)
    pu_indm3=where(ct_rate_arr_pc(1,*) gt 1e-2 and ct_rate_arr_pc(1,*) le 5e-2)
    pu_indm2=where(ct_rate_arr_pc(1,*) gt 5e-2 and ct_rate_arr_pc(1,*) le 0.1)
    pu_indm1=where(ct_rate_arr_pc(1,*) gt 0.1 and ct_rate_arr_pc(1,*) le 0.5)

    pu_ind1=where(ct_rate_arr_pc(1,*) gt 0.5 and ct_rate_arr_pc(1,*) le 2.)
    pu_ind2=where(ct_rate_arr_pc(1,*) gt 2.0 and ct_rate_arr_pc(1,*) le 5.)
    pu_ind3=where(ct_rate_arr_pc(1,*) gt 5.0 and ct_rate_arr_pc(1,*) le 10.)
    pu_ind4=where(ct_rate_arr_pc(1,*) gt 10.0 and ct_rate_arr_pc(1,*) le 25.)
    pu_ind5=where(ct_rate_arr_pc(1,*) gt 25.0 and ct_rate_arr_pc(1,*) le 70.)
    pu_ind6=where(ct_rate_arr_pc(1,*) gt 70.0)

    if pu_ind6(0) ne -1 then begin
        for n=0,n_elements(pu_ind6)-1 do oplot,[ct_rate_arr_pc(8,pu_ind6(n)),ct_rate_arr_pc(9,pu_ind6(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind6)-1 do xyouts,ct_rate_arr_pc(8,pu_ind6(n))-trig_time,2.5,'6'
        for n=0,n_elements(pu_ind6)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind6(n)) and evt.time le ct_rate_arr_pc(9,pu_ind6(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=6
            if ind(0) ne -1 then  evt(ind).fact2=8.46
            if ind(0) ne -1 then  evt(ind).fact3=1.333
        endfor
    endif
    if pu_ind5(0) ne -1 then begin
        for n=0,n_elements(pu_ind5)-1 do oplot,[ct_rate_arr_pc(8,pu_ind5(n)),ct_rate_arr_pc(9,pu_ind5(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind5)-1 do xyouts,ct_rate_arr_pc(8,pu_ind5(n))-trig_time,2.5,'5'
        for n=0,n_elements(pu_ind5)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind5(n)) and evt.time le ct_rate_arr_pc(9,pu_ind5(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=5
;            if ind(0) ne -1 then  evt(ind).fact2=5.38
            if ind(0) ne -1 then  evt(ind).fact2=7.20
            if ind(0) ne -1 then  evt(ind).fact3=1.125
        endfor
    endif
    if pu_ind4(0) ne -1 then begin
        for n=0,n_elements(pu_ind4)-1 do oplot,[ct_rate_arr_pc(8,pu_ind4(n)),ct_rate_arr_pc(9,pu_ind4(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind4)-1 do xyouts,ct_rate_arr_pc(8,pu_ind4(n))-trig_time,2.5,'4'
        for n=0,n_elements(pu_ind4)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind4(n)) and evt.time le ct_rate_arr_pc(9,pu_ind4(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=4
            if ind(0) ne -1 then  evt(ind).fact2=3.69
            if ind(0) ne -1 then  evt(ind).fact3=1.06

        endfor
    endif
    if pu_ind3(0) ne -1 then begin
        for n=0,n_elements(pu_ind3)-1 do oplot,[ct_rate_arr_pc(8,pu_ind3(n)),ct_rate_arr_pc(9,pu_ind3(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind3)-1 do xyouts,ct_rate_arr_pc(8,pu_ind3(n))-trig_time,2.5,'3'
        for n=0,n_elements(pu_ind3)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind3(n)) and evt.time le ct_rate_arr_pc(9,pu_ind3(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=3
            if ind(0) ne -1 then  evt(ind).fact2=2.66
            if ind(0) ne -1 then  evt(ind).fact3=1.03

        endfor
    endif
    if pu_ind2(0) ne -1 then begin
        for n=0,n_elements(pu_ind2)-1 do oplot,[ct_rate_arr_pc(8,pu_ind2(n)),ct_rate_arr_pc(9,pu_ind2(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind2)-1 do xyouts,ct_rate_arr_pc(8,pu_ind2(n))-trig_time,2.5,'2'
        for n=0,n_elements(pu_ind2)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind2(n)) and evt.time le ct_rate_arr_pc(9,pu_ind2(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=2
            if ind(0) ne -1 then  evt(ind).fact2=1.76
            if ind(0) ne -1 then  evt(ind).fact3=1.01

        endfor
    endif
    if pu_ind1(0) ne -1 then begin
        for n=0,n_elements(pu_ind1)-1 do oplot,[ct_rate_arr_pc(8,pu_ind1(n)),ct_rate_arr_pc(9,pu_ind1(n))]-trig_time,[2.,2.]
        for n=0,n_elements(pu_ind1)-1 do xyouts,ct_rate_arr_pc(8,pu_ind1(n))-trig_time,2.5,'1'
        for n=0,n_elements(pu_ind1)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind1(n)) and evt.time le ct_rate_arr_pc(9,pu_ind1(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=1
;            if ind(0) ne -1 then  evt(ind).fact2=1.1
            if ind(0) ne -1 then  evt(ind).fact2=1.76
            if ind(0) ne -1 then  evt(ind).fact3=1.001

        endfor
    endif
    if pu_indm1(0) ne -1 then begin
        for n=0,n_elements(pu_indm1)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm1(n)) and evt.time le ct_rate_arr_pc(9,pu_indm1(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-1
            if ind(0) ne -1 then  evt(ind).fact2=1.07
            if ind(0) ne -1 then  evt(ind).fact3=1.44

        endfor
    endif
    if pu_indm2(0) ne -1 then begin
        for n=0,n_elements(pu_indm2)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm2(n)) and evt.time le ct_rate_arr_pc(9,pu_indm2(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-2
            if ind(0) ne -1 then  evt(ind).fact2=1.09
            if ind(0) ne -1 then  evt(ind).fact3=2.25

        endfor
    endif
    if pu_indm3(0) ne -1 then begin
        for n=0,n_elements(pu_indm3)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm3(n)) and evt.time le ct_rate_arr_pc(9,pu_indm3(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-3
            if ind(0) ne -1 then  evt(ind).fact2=1.134
            if ind(0) ne -1 then  evt(ind).fact3=4.

        endfor
    endif
    if pu_indm4(0) ne -1 then begin
        for n=0,n_elements(pu_indm4)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm4(n)) and evt.time le ct_rate_arr_pc(9,pu_indm4(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-4
            if ind(0) ne -1 then  evt(ind).fact2=1.18
            if ind(0) ne -1 then  evt(ind).fact3=6.25

        endfor
    endif
    if pu_indm5(0) ne -1 then begin
        for n=0,n_elements(pu_indm5)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm5(n)) and evt.time le ct_rate_arr_pc(9,pu_indm5(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-5
            if ind(0) ne -1 then  evt(ind).fact2=1.26
            if ind(0) ne -1 then  evt(ind).fact3=11.11

        endfor
    endif
    if pu_indm6(0) ne -1 then begin
        for n=0,n_elements(pu_indm6)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm6(n)) and evt.time le ct_rate_arr_pc(9,pu_indm6(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-6
            if ind(0) ne -1 then  evt(ind).fact2=1.37
            if ind(0) ne -1 then  evt(ind).fact3=18.4

        endfor
    endif
    if pu_indm7(0) ne -1 then begin
        for n=0,n_elements(pu_indm7)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm7(n)) and evt.time le ct_rate_arr_pc(9,pu_indm7(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-7
            if ind(0) ne -1 then  evt(ind).fact2=1.603
            if ind(0) ne -1 then  evt(ind).fact3=36.

        endfor
    endif
;stop
    evt_dist=sqrt((evt.x-src_skyxs(0))^2+(evt.y-src_skyys(0))^2)
;evt_ind_wt=where(evt.ftype eq 0 and evt.fnum eq 0 and evt_dist lt 30.)
    
    critm7=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 5. and evt_dist gt 0. and evt.pileup eq -7);< 5e-4
    critm6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 7. and evt_dist gt 0. and evt.pileup eq -6);5e-4 < < 1e-3
    critm5=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 9. and evt_dist gt 0. and evt.pileup eq -5);1e-3 < < 5e-3
    critm4=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 12. and evt_dist gt 0. and evt.pileup eq -4);5e-3 < < 1e-2
    critm3=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 15. and evt_dist gt 0. and evt.pileup eq -3);1e-2 < < 5e-2
    critm2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 20. and evt_dist gt 0. and evt.pileup eq -2);5e-2 < < 0.1
    critm1=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 25. and evt_dist gt 0. and evt.pileup eq -1);0.1 < < 0.5

    crit1=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt.pileup eq 0)
;    crit2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 1. and evt.pileup eq 1)
    crit2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 3. and evt.pileup eq 1)
    crit3=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 3. and evt.pileup eq 2)
    crit4=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 5. and evt.pileup eq 3)
    crit5=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 7. and evt.pileup eq 4)
;    crit6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 10. and evt.pileup eq 5)
    crit6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 13. and evt.pileup eq 5)
    crit7=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30. and evt_dist gt 15. and evt.pileup eq 6)
    evt_ind_pc=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)

    evt_ind_pc=0.
;stop
    for fctr=0,n_elements(flist)-1 do begin
        evt_dist=sqrt((evt.x-src_skyxs(fctr))^2+(evt.y-src_skyys(fctr))^2)
;;    evt_ind_wt_tmp=where(evt.ftype eq 0 and evt.fnum eq fctr and evt_dist lt 30.)
        critm7=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 5. and evt_dist gt 0. and evt.pileup eq -7) ;< 5e-4
        critm6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 7. and evt_dist gt 0. and evt.pileup eq -6) ;5e-4 < < 1e-3
        critm5=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 9. and evt_dist gt 0. and evt.pileup eq -5) ;1e-3 < < 5e-3
        critm4=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 12. and evt_dist gt 0. and evt.pileup eq -4) ;5e-3 < < 1e-2
        critm3=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 15. and evt_dist gt 0. and evt.pileup eq -3) ;1e-2 < < 5e-2
        critm2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 20. and evt_dist gt 0. and evt.pileup eq -2) ;5e-2 < < 0.1
        critm1=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 25. and evt_dist gt 0. and evt.pileup eq -1) ;0.1 < < 0.5
        
        crit1=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt.pileup eq 0)
;        crit2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 1. and evt.pileup eq 1)
        crit2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 3. and evt.pileup eq 1)
        crit3=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 3. and evt.pileup eq 2)
        crit4=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 5. and evt.pileup eq 3)
        crit5=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 7. and evt.pileup eq 4)
;        crit6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 10. and evt.pileup eq 5)
        crit6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 13. and evt.pileup eq 5)
        crit7=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30. and evt_dist gt 15. and evt.pileup eq 6)
        evt_ind_pc_tmp=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)
;;    evt_ind_wt=[evt_ind_wt,evt_ind_wt_tmp]
        if evt_ind_pc_tmp(0) ne -1 then evt_ind_pc=[evt_ind_pc,evt_ind_pc_tmp]
;    stop
    endfor
    if n_elements(evt_ind_pc) gt 1 then evt_ind_pc=evt_ind_pc(1:*) else evt_ind_pc=-1
print,'end mods'
;stop
    ;;MODS SHOULD END HERE
    
;CHECK TO SEE IF THERE ARE ANY -1 VALUES IN THE INDEX ARRAYS AND IF SO
;THEN REMOVE THEM
;    if evt_ind_pc(0) ne -1 then while evt_ind_pc(n_elements(evt_ind_pc)-1) eq -1 do evt_ind_pc=evt_ind_pc(0:n_elements(evt_ind_pc)-2)
;    if evt_ind_wt(0) ne -1 then while evt_ind_wt(n_elements(evt_ind_wt)-1) eq -1 do evt_ind_wt=evt_ind_wt(0:n_elements(evt_ind_wt)-2)
;if n_elements(evt_ind_pc) gt 1 then while evt_ind_pc(n_elements(evt_ind_pc)-1) eq -1 do if n_elements(evt_ind_pc) gt 1 then evt_ind_pc=evt_ind_pc(0:n_elements(evt_ind_pc)-2)
;if n_elements(evt_ind_wt) gt 1 then while evt_ind_wt(n_elements(evt_ind_wt)-1) eq -1 do if n_elements(evt_ind_wt) gt 1 then evt_ind_wt=evt_ind_wt(0:n_elements(evt_ind_wt)-2)
;clean_pc_test=where(evt_ind_pc eq -1) & if clean_pc_test(0) ne -1 then evt_ind_pc=evt_ind_pc(where(evt_ind_pc ne -1))
;clean_wt_test=where(evt_ind_wt eq -1) & if clean_wt_test(0) ne -1 then evt_ind_wt=evt_ind_wt(where(evt_ind_wt ne -1))    
if 0 then begin
    n=0L
    mone_ctr=0L
    if evt_ind_pc(0) ne -1 then while n lt n_elements(evt_ind_pc) do begin
        if evt_ind_pc(n) eq -1 then begin
            for m=n,n_elements(evt_ind_pc)-2 do begin
                print,n,m,m+1, evt_ind_pc(m), evt_ind_pc(m+1)
                evt_ind_pc(m)=evt_ind_pc(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    if evt_ind_pc(0) ne -1 then evt_ind_pc=evt_ind_pc(0:n-1-mone_ctr)
    n=0L
    mone_ctr=0L
    if evt_ind_wt(0) ne -1 then while n lt n_elements(evt_ind_wt) do begin
        if evt_ind_wt(n) eq -1 then begin
            for m=n,n_elements(evt_ind_wt)-2 do begin
                print,n,m,m+1, evt_ind_wt(m), evt_ind_wt(m+1)
                evt_ind_wt(m)=evt_ind_wt(m+1)
            endfor
            n=n-1L
            mone_ctr=mone_ctr+1L
        endif
        n=n+1L
    endwhile
    if evt_ind_wt(0) ne -1 then evt_ind_wt=evt_ind_wt(0:n-1-mone_ctr)
endif
;stop

;if not(keyword_set(lorella)) then begin
    if n_elements(evt_ind_wt) gt 1 then bin_by_rules,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bin=bin
    if n_elements(evt_ind_pc) gt 1 then bin_by_rules,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bin=bin
;endif
;if keyword_set(lorella)) then begin
;    if n_elements(evt_ind_wt) gt 1 then bin_by_rules_lorella,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bin=bin
;    if n_elements(evt_ind_pc) gt 1 then bin_by_rules_lorella,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bin=bin
;endif

    set_plot,'x'
    window,1
endif;PILEUP LOOP

;CALCULATE THE AVERAGE PILEUP CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then pileup_fact=total(evt(evt_ind_pc(ind)).fact2)/n_elements(ind)
    if ind(0) eq -1 then pileup_fact=1.
    ct_rate_arr_pc(10,n)=pileup_fact
endfor
if n_elements(ct_rate_arr_wt) gt 1 then for n=0,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    ind=where(evt(evt_ind_wt).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt).ftype eq 0)
    if ind(0) ne -1 then pileup_fact=total(evt(evt_ind_wt(ind)).fact2)/n_elements(ind)
if ind(0) eq -1 then pileup_fact=1.
    ct_rate_arr_wt(10,n)=pileup_fact
endfor

;;CHECK;;
;CALCULATE THE AVERAGE PSF CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc(ind)).fact1)/n_elements(ind)
    if ind(0) eq -1 then psf_corr_fact=1.
    ct_rate_arr_pc(11,n)=psf_corr_fact
endfor
if n_elements(ct_rate_arr_wt) gt 1 then for n=0,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    ind=where(evt(evt_ind_wt).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt).ftype eq 0)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt(ind)).fact1)/n_elements(ind)
    if ind(0) eq -1 then psf_corr_fact=1.
    ct_rate_arr_wt(11,n)=psf_corr_fact
endfor

;CALCULATE THE AVERAGE FACT3 FACTOR OF ALL EVENTS THAT GO INTO EACH
;BIN AND FILL IN THE APPROPRIATE FACTOR TO THE CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then fact3_tot=total(evt(evt_ind_pc(ind)).fact3)/n_elements(ind)
    if ind(0) eq -1 then fact3_tot=1.
    ct_rate_arr_pc(16,n)=fact3_tot
endfor
if n_elements(ct_rate_arr_wt) gt 1 then for n=0,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    ind=where(evt(evt_ind_wt).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt).ftype eq 0)
    if ind(0) ne -1 then fact3_tot=total(evt(evt_ind_wt(ind)).fact3)/n_elements(ind)
    if ind(0) eq -1 then fact3_tot=1.
    ct_rate_arr_wt(16,n)=fact3_tot
endfor

;stop
;CALCULATE THE ERRORS
if n_elements(ct_rate_arr_wt) gt 1 then for n=0,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    for nn=2,5 do begin
        error=sqrt(ct_rate_arr_wt(nn,n))/ct_rate_arr_wt(6,n)
        ct_rate_arr_wt(10+nn,n)=error*ct_rate_arr_wt(10,n)*ct_rate_arr_wt(11,n)
    endfor
endfor
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    for nn=2,5 do begin
        error=sqrt(ct_rate_arr_pc(nn,n))/ct_rate_arr_pc(6,n)
        ct_rate_arr_pc(10+nn,n)=error*ct_rate_arr_pc(10,n)*ct_rate_arr_pc(11,n)
    endfor
endfor
;;CHECK;;
!p.multi=[0,1,1]

;CT_RATE_ARR_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,EBIN_CTS1,EBIN_CTS2,EBIN_CTS3,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PUC,PSFCORR,ERROR,FACT3]
if n_elements(ct_rate_arr_wt) gt 1 then plot,ct_rate_arr_wt(0,*)-trig_time,ct_rate_arr_wt(1,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if n_elements(ct_rate_arr_pc) gt 1 then oplot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*)*ct_rate_arr_pc(10,*),psym=3
if n_elements(ct_rate_arr_wt) gt 1 then for n=0,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    oplot,[ct_rate_arr_wt(0,n)-trig_time,ct_rate_arr_wt(0,n)-trig_time],[ct_rate_arr_wt(1,n)-ct_rate_arr_wt(12,n),ct_rate_arr_wt(1,n)+ct_rate_arr_wt(12,n)]
endfor
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_pc(0,n)-trig_time,ct_rate_arr_pc(0,n)-trig_time],[ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)-ct_rate_arr_pc(12,n),ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)+ct_rate_arr_pc(12,n)]
endfor

if n_elements(ct_rate_arr_pc) gt 1 then plot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*)*ct_rate_arr_pc(10,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_pc(0,n)-trig_time,ct_rate_arr_pc(0,n)-trig_time],[ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)-ct_rate_arr_pc(12,n),ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)+ct_rate_arr_pc(12,n)]
endfor
;stop

if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_wt) gt 1 then calc_backgr,evt(evt_ind_wt_back),ct_rate_arr_wt,0,ftype,gti,ct_rate_arr_back_wt
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_pc) gt 1 then calc_backgr,evt(evt_ind_pc_back),ct_rate_arr_pc,1,ftype,gti,ct_rate_arr_back_pc

;CALCULATE THE AVERAGE PSF CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH **BACKGROUND** BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
;THE TOTAL BACKGR FIELDS
    ind=where(evt(evt_ind_pc_back).time ge ct_rate_arr_back_pc(20,n) and evt(evt_ind_pc_back).time le ct_rate_arr_back_pc(21,n) and evt(evt_ind_pc_back).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_pc(24,n)=psf_corr_fact
    ct_rate_arr_back_pc(17:19,n)=ct_rate_arr_back_pc(17:19,n)*ct_rate_arr_back_pc(24,n)
;THE 0.2-1.0 BACKGR FIELDS
    ind=where(evt(evt_ind_pc_back).time ge ct_rate_arr_back_pc(28,n) and evt(evt_ind_pc_back).time le ct_rate_arr_back_pc(29,n) and evt(evt_ind_pc_back).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_pc(32,n)=psf_corr_fact
    ct_rate_arr_back_pc(25:27,n)=ct_rate_arr_back_pc(25:27,n)*ct_rate_arr_back_pc(32,n)
;THE 1.0-2.0 BACKGR FIELDS
    ind=where(evt(evt_ind_pc_back).time ge ct_rate_arr_back_pc(36,n) and evt(evt_ind_pc_back).time le ct_rate_arr_back_pc(37,n) and evt(evt_ind_pc_back).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_pc(40,n)=psf_corr_fact
    ct_rate_arr_back_pc(33:35,n)=ct_rate_arr_back_pc(33:35,n)*ct_rate_arr_back_pc(40,n)
;THE 2.0-10.0 BACKGR FIELDS
    ind=where(evt(evt_ind_pc_back).time ge ct_rate_arr_back_pc(44,n) and evt(evt_ind_pc_back).time le ct_rate_arr_back_pc(45,n) and evt(evt_ind_pc_back).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_pc(48,n)=psf_corr_fact
    ct_rate_arr_back_pc(41:43,n)=ct_rate_arr_back_pc(41:43,n)*ct_rate_arr_back_pc(48,n)
endfor
if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
;THE TOTAL BACKGR FILES
    ind=where(evt(evt_ind_wt_back).time ge ct_rate_arr_back_wt(20,n) and evt(evt_ind_wt_back).time le ct_rate_arr_back_wt(21,n) and evt(evt_ind_wt_back).ftype eq 0)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_wt(24,n)=psf_corr_fact
    ct_rate_arr_back_wt(17:19,n)=ct_rate_arr_back_wt(17:19,n)*ct_rate_arr_back_wt(24,n)
;THE 0.2-1.0 BACKGR FILES
    ind=where(evt(evt_ind_wt_back).time ge ct_rate_arr_back_wt(28,n) and evt(evt_ind_wt_back).time le ct_rate_arr_back_wt(29,n) and evt(evt_ind_wt_back).ftype eq 0)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_wt(32,n)=psf_corr_fact
    ct_rate_arr_back_wt(25:27,n)=ct_rate_arr_back_wt(25:27,n)*ct_rate_arr_back_wt(32,n)
;THE 1.0-2.0 BACKGR FILES
    ind=where(evt(evt_ind_wt_back).time ge ct_rate_arr_back_wt(36,n) and evt(evt_ind_wt_back).time le ct_rate_arr_back_wt(37,n) and evt(evt_ind_wt_back).ftype eq 0)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_wt(40,n)=psf_corr_fact
    ct_rate_arr_back_wt(33:35,n)=ct_rate_arr_back_wt(33:35,n)*ct_rate_arr_back_wt(40,n)
;THE 2.0-10.0 BACKGR FILES
    ind=where(evt(evt_ind_wt_back).time ge ct_rate_arr_back_wt(44,n) and evt(evt_ind_wt_back).time le ct_rate_arr_back_wt(45,n) and evt(evt_ind_wt_back).ftype eq 0)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt_back(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
    ct_rate_arr_back_wt(48,n)=psf_corr_fact
    ct_rate_arr_back_wt(41:43,n)=ct_rate_arr_back_wt(41:43,n)*ct_rate_arr_back_wt(48,n)
endfor


;stop

;CT_RATE_ARR_BACK_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PUC,PSF_CORR,ERROR,BACKGR_CTS,BACKGR_RATE,BACKGR_ERROR_RATE,BACKGR_STARTT,BACKGR_STOPT,BACKGR_DEAD,BACKGR_TRUE_DUR,EMPTY_PREV_EXTRA_SRCTIME,EMPTY_LATER_EXTRA_SRCTIME,EMPTY_PREV_EXTRA_BACKTIME,EMPTY_LATER_EXTRA_BACKTIME]
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_wt(8,*)) gt 1 and n_elements(ct_rate_arr_wt) gt 1 then det_extratime,gti,ftype,ct_rate_arr_wt(8,*),ct_rate_arr_wt(9,*),extratime_arr_wt,0
;stop
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_pc(8,*)) gt 1 and n_elements(ct_rate_arr_pc) gt 1 then det_extratime,gti,ftype,ct_rate_arr_pc(8,*),ct_rate_arr_pc(9,*),extratime_arr_pc,1
;stop

if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_back_wt(20,*)) gt 1 and n_elements(ct_rate_arr_wt) gt 1 then det_extratime_back,evt(evt_ind_wt_back),gti,ftype,ct_rate_arr_back_wt(20,*),ct_rate_arr_back_wt(21,*),back_extratime_arr_wt_tot,0
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_back_pc(20,*)) gt 1 and n_elements(ct_rate_arr_pc) gt 1 then det_extratime_back,evt(evt_ind_pc_back),gti,ftype,ct_rate_arr_back_pc(20,*),ct_rate_arr_back_pc(21,*),back_extratime_arr_pc_tot,1
if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_back_wt(28,*)) gt 1 and n_elements(ct_rate_arr_wt) gt 1 then det_extratime_back,evt(evt_ind_wt_back),gti,ftype,ct_rate_arr_back_wt(28,*),ct_rate_arr_back_wt(29,*),back_extratime_arr_wt_1,0
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_back_pc(28,*)) gt 1 and n_elements(ct_rate_arr_pc) gt 1 then det_extratime_back,evt(evt_ind_pc_back),gti,ftype,ct_rate_arr_back_pc(28,*),ct_rate_arr_back_pc(29,*),back_extratime_arr_pc_1,1
if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_back_wt(36,*)) gt 1 and n_elements(ct_rate_arr_wt) gt 1 then det_extratime_back,evt(evt_ind_wt_back),gti,ftype,ct_rate_arr_back_wt(36,*),ct_rate_arr_back_wt(37,*),back_extratime_arr_wt_2,0
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_back_pc(36,*)) gt 1 and n_elements(ct_rate_arr_pc) gt 1 then det_extratime_back,evt(evt_ind_pc_back),gti,ftype,ct_rate_arr_back_pc(36,*),ct_rate_arr_back_pc(37,*),back_extratime_arr_pc_2,1
if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_back_wt(44,*)) gt 1 and n_elements(ct_rate_arr_wt) gt 1 then det_extratime_back,evt(evt_ind_wt_back),gti,ftype,ct_rate_arr_back_wt(44,*),ct_rate_arr_back_wt(45,*),back_extratime_arr_wt_3,0
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_back_pc(44,*)) gt 1 and n_elements(ct_rate_arr_pc) gt 1 then det_extratime_back,evt(evt_ind_pc_back),gti,ftype,ct_rate_arr_back_pc(44,*),ct_rate_arr_back_pc(45,*),back_extratime_arr_pc_3,1

;NOW ASSIGN HALF THE EXTRA TIME OF EACH GAP TO THE PREVIOUS BIN AND
;HALF TO THE SUBSEQUENT BIN
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    if n gt 0 then ct_rate_arr_back_pc(49,n)=0.5*extratime_arr_pc(n-1);PREVIOUS EXTRA TIME FOR SRC BIN
    if n gt 0 then ct_rate_arr_back_pc(51,n)=0.5*back_extratime_arr_pc_tot(n-1); PREVIOUS EXTRA TIME FOR TOT BACKGR BIN
    if n gt 0 then ct_rate_arr_back_pc(53,n)=0.5*back_extratime_arr_pc_1(n-1); PREVIOUS EXTRA TIME FOR 0.2-1.0 BACKGR BIN
    if n gt 0 then ct_rate_arr_back_pc(55,n)=0.5*back_extratime_arr_pc_2(n-1) ; PREVIOUS EXTRA TIME FOR 1.0-2.0 BACKGR BIN
    if n gt 0 then ct_rate_arr_back_pc(57,n)=0.5*back_extratime_arr_pc_3(n-1) ; PREVIOUS EXTRA TIME FOR 2.0-10 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_pc(0,*))-1 then ct_rate_arr_back_pc(50,n)=0.5*extratime_arr_pc(n); LATTER EXTRA TIME FOR SRC BIN
    if n lt n_elements(ct_rate_arr_back_pc(0,*))-1 then ct_rate_arr_back_pc(52,n)=0.5*back_extratime_arr_pc_tot(n); LATTER EXTRA TIME FOR TOT BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_pc(0,*))-1 then ct_rate_arr_back_pc(54,n)=0.5*back_extratime_arr_pc_1(n); LATTER EXTRA TIME FOR 0.2-1.0 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_pc(0,*))-1 then ct_rate_arr_back_pc(56,n)=0.5*back_extratime_arr_pc_2(n) ; LATTER EXTRA TIME FOR 1.0-2.0 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_pc(0,*))-1 then ct_rate_arr_back_pc(58,n)=0.5*back_extratime_arr_pc_3(n) ; LATTER EXTRA TIME FOR 2.0-10 BACKGR BIN
endfor
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
    if n gt 0 then ct_rate_arr_back_wt(49,n)=0.5*extratime_arr_wt(n-1) ;PREVIOUS EXTRA TIME FOR SRC BIN
    if n gt 0 then ct_rate_arr_back_wt(51,n)=0.5*back_extratime_arr_wt_tot(n-1); PREVIOUS EXTRA TIME FOR TOT BACKGR BIN
    if n gt 0 then ct_rate_arr_back_wt(53,n)=0.5*back_extratime_arr_wt_1(n-1); PREVIOUS EXTRA TIME FOR 0.2-1.0 BACKGR BIN
    if n gt 0 then ct_rate_arr_back_wt(55,n)=0.5*back_extratime_arr_wt_2(n-1) ; PREVIOUS EXTRA TIME FOR 1.0-2.0 BACKGR BIN
    if n gt 0 then ct_rate_arr_back_wt(57,n)=0.5*back_extratime_arr_wt_3(n-1) ; PREVIOUS EXTRA TIME FOR 2.0-10 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_wt(0,*))-1 then ct_rate_arr_back_wt(50,n)=0.5*extratime_arr_wt(n); LATTER EXTRA TIME FOR SRC BIN
    if n lt n_elements(ct_rate_arr_back_wt(0,*))-1 then ct_rate_arr_back_wt(52,n)=0.5*back_extratime_arr_wt_tot(n); LATTER EXTRA TIME FOR TOT BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_wt(0,*))-1 then ct_rate_arr_back_wt(54,n)=0.5*back_extratime_arr_wt_1(n); LATTER EXTRA TIME FOR 0.2-1.0 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_wt(0,*))-1 then ct_rate_arr_back_wt(56,n)=0.5*back_extratime_arr_wt_2(n) ; LATTER EXTRA TIME FOR 1.0-2.0 BACKGR BIN
    if n lt n_elements(ct_rate_arr_back_wt(0,*))-1 then ct_rate_arr_back_wt(58,n)=0.5*back_extratime_arr_wt_3(n) ; LATTER EXTRA TIME FOR 2.0-10 BACKGR BIN
endfor
;stop
print,'ALMOST DONE NOW'
;stop

;NOW ADJUST THE SRC AND BACKGR CT RATES FOR THE EXTRA TIME

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    ct_rate_arr_back_pc(1,n)=ct_rate_arr_back_pc(2,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n));SRC ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_pc(18,n)=ct_rate_arr_back_pc(17,n)/(ct_rate_arr_back_pc(23,n)+ct_rate_arr_back_pc(51,n)+ct_rate_arr_back_pc(52,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_pc(26,n)=ct_rate_arr_back_pc(25,n)/(ct_rate_arr_back_pc(31,n)+ct_rate_arr_back_pc(53,n)+ct_rate_arr_back_pc(54,n)) ;TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_pc(34,n)=ct_rate_arr_back_pc(33,n)/(ct_rate_arr_back_pc(39,n)+ct_rate_arr_back_pc(55,n)+ct_rate_arr_back_pc(56,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_pc(42,n)=ct_rate_arr_back_pc(41,n)/(ct_rate_arr_back_pc(47,n)+ct_rate_arr_back_pc(57,n)+ct_rate_arr_back_pc(58,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
endfor
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
    ct_rate_arr_back_wt(1,n)=ct_rate_arr_back_wt(2,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n));SRC ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_wt(18,n)=ct_rate_arr_back_wt(17,n)/(ct_rate_arr_back_wt(23,n)+ct_rate_arr_back_wt(51,n)+ct_rate_arr_back_wt(52,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_wt(26,n)=ct_rate_arr_back_wt(25,n)/(ct_rate_arr_back_wt(31,n)+ct_rate_arr_back_wt(53,n)+ct_rate_arr_back_wt(54,n)) ;TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_wt(34,n)=ct_rate_arr_back_wt(33,n)/(ct_rate_arr_back_wt(39,n)+ct_rate_arr_back_wt(55,n)+ct_rate_arr_back_wt(56,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    ct_rate_arr_back_wt(42,n)=ct_rate_arr_back_wt(41,n)/(ct_rate_arr_back_wt(47,n)+ct_rate_arr_back_wt(57,n)+ct_rate_arr_back_wt(58,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
endfor


!p.multi=[0,1,1]
;NOW PLOT SRC-BACKGR LC
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then plot,ct_rate_arr_back_pc(0,*)-trig_time,ct_rate_arr_back_pc(1,*)*ct_rate_arr_back_pc(10,*)*ct_rate_arr_back_pc(11,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then plot,ct_rate_arr_back_wt(0,*)-trig_time,ct_rate_arr_back_wt(1,*)*ct_rate_arr_back_wt(10,*)*ct_rate_arr_back_wt(11,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(0,n)-trig_time],[ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(12,n),ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)+ct_rate_arr_back_pc(12,n)]
endfor

if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
    oplot,[ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(0,n)-trig_time],[ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(12,n),ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)+ct_rate_arr_back_wt(12,n)]
endfor

;if n_elements(ct_rate_arr_back_pc) gt 1 then plot,ct_rate_arr_back_pc(0,*)-trig_time,ct_rate_arr_back_pc(18,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
;if n_elements(ct_rate_arr_back_wt) gt 1 then plot,ct_rate_arr_back_wt(0,*)-trig_time,ct_rate_arr_back_wt(18,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]

;if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
;    oplot,[ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(0,n)-trig_time],[ct_rate_arr_back_pc(18,n)-ct_rate_arr_back_pc(19,n),ct_rate_arr_back_pc(18,n)+ct_rate_arr_back_pc(19,n)]
;endfor
;if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
;    oplot,[ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(0,n)-trig_time],[ct_rate_arr_back_wt(18,n)-ct_rate_arr_back_wt(19,n),ct_rate_arr_back_wt(18,n)+ct_rate_arr_back_wt(19,n)]
;endfor

;if n_elements(ct_rate_arr_back_pc) gt 1 then plot,ct_rate_arr_back_pc(0,*)-trig_time,ct_rate_arr_back_pc(1,*)*ct_rate_arr_back_pc(10,*)*ct_rate_arr_back_pc(11,*)-ct_rate_arr_back_pc(18,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
;if n_elements(ct_rate_arr_back_wt) gt 1 then plot,ct_rate_arr_back_wt(0,*)-trig_time,ct_rate_arr_back_wt(1,*)*ct_rate_arr_back_wt(10,*)*ct_rate_arr_back_wt(11,*)-ct_rate_arr_back_wt(18,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]

;if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
;    oplot,[ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(0,n)-trig_time],[ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(18,n)-ct_rate_arr_back_pc(19,n),ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(18,n)+ct_rate_arr_back_pc(19,n)]
;endfor
;if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
;    oplot,[ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(0,n)-trig_time],[ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(18,n)-ct_rate_arr_back_wt(19,n),ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(18,n)+ct_rate_arr_back_wt(19,n)]
;endfor

;if n_elements(ct_rate_arr_back_pc) gt 1 and n_elements(ct_rate_arr_back_wt) gt 1 then begin
;    if ct_rate_arr_back_pc(0,0) lt ct_rate_arr_back_wt(0,0) then print,'first 100 cts is ',ct_rate_arr_back_pc(1:4,0),' ',ct_rate_arr_back_pc(8,0)-trig_time,' ',ct_rate_arr_back_pc(9,0)-trig_time
;    if ct_rate_arr_back_pc(0,0) ge ct_rate_arr_back_wt(0,0) then print,'first 100 cts is ',ct_rate_arr_back_wt(1:4,0),' ',ct_rate_arr_back_wt(8,0)-trig_time,' ',ct_rate_arr_back_wt(9,0)-trig_time
;endif else begin
;    if n_elements(ct_rate_arr_back_pc) gt 1 then print,'first 100 cts is ',ct_rate_arr_back_pc(1:4,0),' ',ct_rate_arr_back_pc(8,0)-trig_time,' ',ct_rate_arr_back_pc(9,0)-trig_time
;    if n_elements(ct_rate_arr_back_wt) gt 1 then print,'first 100 cts is ',ct_rate_arr_back_wt(1:4,0),' ',ct_rate_arr_back_wt(8,0)-trig_time,' ',ct_rate_arr_back_wt(9,0)-trig_time
;endelse

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then ind_pc=where(ct_rate_arr_back_pc(0,*)-trig_time-300. eq min(ct_rate_arr_back_pc(0,*)-trig_time-300.))
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then ind_wt=where(ct_rate_arr_back_wt(0,*)-trig_time-300. eq min(ct_rate_arr_back_wt(0,*)-trig_time-300.))

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 and n_elements(ct_rate_arr_back_wt) gt 1 then begin
    if abs(ct_rate_arr_back_pc(0,ind_pc)-trig_time-300.) lt abs(ct_rate_arr_back_wt(0,ind_wt)-trig_time-300.) then print,'300s cts is ',ct_rate_arr_back_pc(1:4,ind_pc),' ',ct_rate_arr_back_pc(8,ind_pc)-trig_time,' ',ct_rate_arr_back_pc(9,ind_pc)-trig_time
    if abs(ct_rate_arr_back_pc(0,ind_pc)-trig_time-300.) ge abs(ct_rate_arr_back_wt(0,ind_wt)-trig_time-300.) then print,'300s cts is ',ct_rate_arr_back_wt(1:4,ind_wt),' ',ct_rate_arr_back_wt(8,ind_wt)-trig_time,' ',ct_rate_arr_back_wt(9,ind_wt)-trig_time
endif else begin
    if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then print,'300s cts is ',ct_rate_arr_back_pc(1:4,ind_pc),' ',ct_rate_arr_back_pc(8,ind_pc)-trig_time,' ',ct_rate_arr_back_pc(9,ind_pc)-trig_time
    if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then print,'300s cts is ',ct_rate_arr_back_wt(1:4,ind_wt),' ',ct_rate_arr_back_wt(8,ind_wt)-trig_time,' ',ct_rate_arr_back_wt(9,ind_wt)-trig_time
endelse
;stop

;CT_RATE_ARR_BACK_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PUC,PSF_CORR,ERROR,BACKGR_CTS,BACKGR_RATE,BACKGR_ERROR_RATE,BACKGR_STARTT,BACKGR_STOPT,BACKGR_DEAD,BACKGR_TRUE_DUR,EMPTY_PREV_EXTRA_SRCTIME,EMPTY_LATER_EXTRA_SRCTIME,EMPTY_PREV_EXTRA_BACKTIME,EMPTY_LATER_EXTRA_BACKTIME]
openw,lunout,'lc_newout.txt',/get_lun
printf,lunout,'junk1'
printf,lunout,'junk2'

if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
    hardness1=ct_rate_arr_back_wt(4,n)/ct_rate_arr_back_wt(3,n)
    hardness2=ct_rate_arr_back_wt(5,n)/ct_rate_arr_back_wt(4,n)
;    error_in_hardness=sqrt(ct_rate_arr_back_wt(4,n)+ct_rate_arr_back_wt(5,n));NOT RIGHT
    err_soft=sqrt(ct_rate_arr_back_wt(3,n)+ct_rate_arr_back_wt(25,n))
    err_mid=sqrt(ct_rate_arr_back_wt(4,n)+ct_rate_arr_back_wt(33,n))
    err_hard=sqrt(ct_rate_arr_back_wt(5,n)+ct_rate_arr_back_wt(41,n))
    error_in_hardness1=sqrt(hardness1^2.*(err_soft^2./ct_rate_arr_back_wt(3,n)^2.+err_mid^2./ct_rate_arr_back_wt(4,n)^2.))
    error_in_hardness2=sqrt(hardness2^2.*(err_mid^2./ct_rate_arr_back_wt(4,n)^2.+err_hard^2./ct_rate_arr_back_wt(5,n)^2.))
    error_in_hardness=sqrt(error_in_hardness1^2.+error_in_hardness2^2.)

    det_sig=sqrt(ct_rate_arr_back_wt(2,n))
;    rate=ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(18,n)
    rate=(ct_rate_arr_back_wt(1,n)-ct_rate_arr_back_wt(18,n)/ct_rate_arr_back_wt(16,n))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(18,n)
;    rate1=(ct_rate_arr_back_wt(3,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(26,n)
;    rate2=(ct_rate_arr_back_wt(4,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(34,n)
;    rate3=(ct_rate_arr_back_wt(5,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(42,n)
    rate1=((ct_rate_arr_back_wt(3,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(26,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(26,n)
    rate2=((ct_rate_arr_back_wt(4,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(34,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(34,n)
    rate3=((ct_rate_arr_back_wt(5,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(42,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(42,n)
    printf,lunout,format='(24d16.4)',ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(8,n)-trig_time,ct_rate_arr_back_wt(9,n)-trig_time,rate,ct_rate_arr_back_wt(12,n),hardness1*hardness2+hardness2,error_in_hardness,ct_rate_arr_back_wt(6,n),ct_rate_arr_back_wt(2,n),ct_rate_arr_back_wt(18,n),det_sig,ct_rate_arr_back_wt(11,n),-999,0,rate1,rate2,rate3,ct_rate_arr_back_wt(13,n),ct_rate_arr_back_wt(14,n),ct_rate_arr_back_wt(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2
    
endfor
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    hardness1=ct_rate_arr_back_pc(4,n)/ct_rate_arr_back_pc(3,n)
    hardness2=ct_rate_arr_back_pc(5,n)/ct_rate_arr_back_pc(4,n)
;    error_in_hardness=sqrt(ct_rate_arr_back_pc(4,n)+ct_rate_arr_back_pc(5,n));NOT RIGHT
    err_soft=sqrt(ct_rate_arr_back_pc(3,n)+ct_rate_arr_back_pc(25,n))
    err_mid=sqrt(ct_rate_arr_back_pc(4,n)+ct_rate_arr_back_pc(33,n))
    err_hard=sqrt(ct_rate_arr_back_pc(5,n)+ct_rate_arr_back_pc(41,n))
    error_in_hardness1=sqrt(hardness1^2.*(err_soft^2./ct_rate_arr_back_pc(3,n)^2.+err_mid^2./ct_rate_arr_back_pc(4,n)^2.))
    error_in_hardness2=sqrt(hardness2^2.*(err_mid^2./ct_rate_arr_back_pc(4,n)^2.+err_hard^2./ct_rate_arr_back_pc(5,n)^2.))
    error_in_hardness=sqrt(error_in_hardness1^2.+error_in_hardness2^2.)
    det_sig=sqrt(ct_rate_arr_back_pc(2,n))
;    rate=ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(18,n)
    rate=(ct_rate_arr_back_pc(1,n)-ct_rate_arr_back_pc(18,n)/ct_rate_arr_back_pc(16,n))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n);-ct_rate_arr_back_pc(18,n)
;    rate1=(ct_rate_arr_back_pc(3,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(25,n)
;    rate2=(ct_rate_arr_back_pc(4,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(34,n)
;    rate3=(ct_rate_arr_back_pc(5,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(42,n)
    rate1=((ct_rate_arr_back_pc(3,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(26,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(26,n)
    rate2=((ct_rate_arr_back_pc(4,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(34,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(34,n)
    rate3=((ct_rate_arr_back_pc(5,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(42,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(42,n)
    printf,lunout,format='(24d16.4)',ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(8,n)-trig_time,ct_rate_arr_back_pc(9,n)-trig_time,rate,ct_rate_arr_back_pc(12,n),hardness1*hardness2+hardness2,error_in_hardness,ct_rate_arr_back_pc(6,n),ct_rate_arr_back_pc(2,n),ct_rate_arr_back_pc(18,n),det_sig,ct_rate_arr_back_pc(11,n),-999,1,rate1,rate2,rate3,ct_rate_arr_back_pc(13,n),ct_rate_arr_back_pc(14,n),ct_rate_arr_back_pc(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2
endfor
free_lun,lunout

if keyword_set(lorella) then begin
openr,lunin,'lc_newout.txt',/get_lun
line=parse_str(lunin)
line=parse_str(lunin)
ctr=0l
if bin(0) eq 100 and bin(1) eq 100 and bin(2) eq 100 and bin(3) eq 100 then openw,lun_lorella,'lorella_100.txt',/get_lun
if bin(0) eq 25 and bin(1) eq 25 and bin(2) eq 25 and bin(3) eq 25 then openw,lun_lorella,'lorella_25.txt',/get_lun

printf,format='(14a16)',lun_lorella,' ','time','rate','rate_err','rate1','rate2','rate3','rate1_err','rate2_err','rate3_err','hard1','hard2','hard1_err','hard2_err' 
while not eof(lunin) do begin
    line=parse_str(lunin)
    time_temp=0.d
    rate_temp=0.d
    rate_err_temp=0.d
    rate1_temp=0.d
    rate2_temp=0.d
    rate3_temp=0.d
    rate1_err_temp=0.d
    rate2_err_temp=0.d
    rate3_err_temp=0.d
    hard1_temp=0.d
    hard2_temp=0.d
    hard1_err_temp=0.d
    hard2_err_temp=0.d
    while ctr lt 4 do begin
        time_temp=time_temp+float(line(0))
        rate_temp=rate_temp+float(line(3))
        rate_err_temp=rate_err_temp+float(line(4))
        rate1_temp=rate1_temp+float(line(14))
        rate2_temp=rate2_temp+float(line(15))
        rate3_temp=rate3_temp+float(line(16))
        rate1_err_temp=rate1_err_temp+float(line(17))
        rate2_err_temp=rate2_err_temp+float(line(18))
        rate3_err_temp=rate3_err_temp+float(line(19))
        hard1_temp=hard1_temp+float(line(20))
        hard2_temp=hard2_temp+float(line(21))
        hard1_err_temp=hard1_err_temp+float(line(22))
        hard2_err_temp=hard2_err_temp+float(line(23))

        line=parse_str(lunin)
        oldline1=line
        if ctr gt 0 then oldline2=oldline1
        ctr=ctr+1l
    endwhile
    if ctr eq 4 then begin
        time=time_temp/4.
        rate=rate_temp/4.
        rate_err=rate_err_temp/4.
        rate1=rate1_temp/4.
        rate2=rate2_temp/4.
        rate3=rate3_temp/4.
        rate1_err=rate1_err_temp/4.
        rate2_err=rate2_err_temp/4.
        rate3_err=rate3_err_temp/4.
        hard1=hard1_temp/4.
        hard2=hard2_temp/4.
        hard1_err=hard1_err_temp/4.
        hard2_err=hard2_err_temp/4.
        print,'100cts vals are ',time,' ',rate,' ',rate_err,' ',rate1,' ',rate2,' ',rate3,' ',rate1_err,' ',rate2_err,' ',rate3_err,' ',hard1,' ',hard2,' ',hard1_err,' ',hard2_err
        printf,lun_lorella,format='(a17,13f16.5)','100cts vals are ',time,rate,rate_err,rate1,rate2,rate3,rate1_err,rate2_err,rate3_err,hard1,hard2,hard1_err,hard2_err
    endif
    ctr=ctr+1l
endwhile
free_lun,lunin
openr,lunin,'lc_newout.txt',/get_lun
line=parse_str(lunin)
line=parse_str(lunin)
ctr=0l
while not eof(lunin) do begin
    line=parse_str(lunin)
    time_temp=0.d
    rate_temp=0.d
    rate_err_temp=0.d
    rate1_temp=0.d
    rate2_temp=0.d
    rate3_temp=0.d
    rate1_err_temp=0.d
    rate2_err_temp=0.d
    rate3_err_temp=0.d
    hard1_temp=0.d
    hard2_temp=0.d
    hard1_err_temp=0.d
    hard2_err_temp=0.d
;    if ctr eq 4 then print,'100cts time is ',time
    if ctr gt 1 then if line(0) gt 300. and oldline1(0) le 300. then begin
        nextline=parse_str(lunin)
        time=(1.*nextline(0)+1.*line(0)+1.*oldline1(0)+1.*oldline2(0))/4.
        rate=(1.*nextline(3)+1.*line(3)+1.*oldline1(3)+1.*oldline2(3))/4.
        rate_err=(1.*nextline(4)+1.*line(4)+1.*oldline1(4)+1.*oldline2(4))/4.
        rate1=(1.*nextline(14)+1.*line(14)+1.*oldline1(14)+1.*oldline2(14))/4.
        rate2=(1.*nextline(15)+1.*line(15)+1.*oldline1(15)+1.*oldline2(15))/4.
        rate3=(1.*nextline(16)+1.*line(16)+1.*oldline1(16)+1.*oldline2(16))/4.
        rate1_err=(1.*nextline(17)+1.*line(17)+1.*oldline1(17)+1.*oldline2(17))/4.
        rate2_err=(1.*nextline(18)+1.*line(18)+1.*oldline1(18)+1.*oldline2(18))/4.
        rate3_err=(1.*nextline(19)+1.*line(19)+1.*oldline1(19)+1.*oldline2(19))/4.
        hard1=(1.*nextline(20)+1.*line(20)+1.*oldline1(20)+1.*oldline2(20))/4.
        hard2=(1.*nextline(21)+1.*line(21)+1.*oldline1(21)+1.*oldline2(21))/4.
        hard1_err=(1.*nextline(22)+1.*line(22)+1.*oldline1(22)+1.*oldline2(22))/4.
        hard2_err=(1.*nextline(23)+1.*line(23)+1.*oldline1(23)+1.*oldline2(23))/4.
        
        print,'300s vals are ',time,' ',rate,' ',rate_err,' ',rate1,' ',rate2,' ',rate3,' ',rate1_err,' ',rate2_err,' ',rate3_err,' ',hard1,' ',hard2,' ',hard1_err,' ',hard2_err
        printf,format='(a16,13f16.5)',lun_lorella,'300s vals are ',time,rate,rate_err,rate1,rate2,rate3,rate1_err,rate2_err,rate3_err,hard1,hard2,hard1_err,hard2_err
                                ;       stop
    endif
    oldline1=line
    if ctr gt 0 then oldline2=oldline1
    ctr=ctr+1l
endwhile
free_lun,lun_lorella
endif
print,'FINISHED'
end
