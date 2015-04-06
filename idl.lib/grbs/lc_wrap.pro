@/bulk/axiom/morris/Mod_LC_wrap/read_acsfiles.pro
@/bulk/axiom/morris/Mod_LC_wrap/lc_readpars.pro
@/bulk/axiom/morris/Mod_LC_wrap/modify_psf_wbadpix.pro
@/bulk/axiom/morris/Mod_LC_wrap/find_src_detpos.pro
@/bulk/axiom/morris/Mod_LC_wrap/det_orbit_gti.pro
@/bulk/axiom/morris/Mod_LC_wrap/bin_by_rules_mod2.pro
@/bulk/axiom/morris/Mod_LC_wrap/concat_evt_060222.pro
@/bulk/axiom/morris/Mod_LC_wrap/calc_backgr.pro

pro lc_wrap,parfname,bin=bin,lorella=lorella,debug=debug,diag=diag,srcrad=srcrad,sing=sing,ms64=ms64,siglim=siglim,wt_noback=wt_noback
;LC_wrap,['../../GRB060124_test/sw00178750000xwtw2po_cl.evt','../../GRB060124_test/sw00178750000xpcw2po_cl.evt','../../GRB060124_test/sw00178750005xpcw2po_cl.evt'],'05:08:27.26','69:44:26.42','05:07:43.631','69:43:31.30',159810892.8d,bin=[100,100,100,250]

if keyword_set(bin) then begin
    if bin(0) gt bin(1) or bin(1) gt bin(2) or bin(2) gt bin(3) then begin
        print,'ERROR - binning info should be monotonically increasing: [hard_low, soft_low, soft_high, hard_high]; Exiting'
        return
    endif
endif

if not(keyword_set(siglim)) then siglim=3. 
thunds_time=300.

;LC_wrap,['sw00116116000xwtw2po_cl.evt','sw00116116000xpcw4po_cl.evt','sw00116116001xpcw4po_cl.evt'],'09:30:10.046','16:59:46.98','09:30:31.087','17:03:28.51',136718739.97d
;LC_wrap,['sw00116116000xwtw2po_cl.evt','sw00116116000xpcw4po_cl.evt'],'09:30:10.046','16:59:46.98','09:30:31.087','17:03:28.51',136718739.97d
;LC_wrap,['wt_orb1.evt','sw00211117991xpcw2po_cl.evt'],'21:31:44.9','02:53:08.5',323.05052,2.934253,169956680.d

;READ THE INPUT PARAM FILE TO GET FILENAMES AND POSITIONS
lc_readpars,parfname,flist,ra,dec,raback,decback,trig_time,acsflist,bins

;DETERMINE WHICH FILES ARE WT AND WHICH ARE PC
ftype=intarr(n_elements(flist))
for n=0l,n_elements(flist)-1 do begin
    dattmp=mrdfits(flist(n),0,hd)
    mode=sxpar(hd,'DATAMODE')
    if mode eq 'PHOTON  ' then ftype(n)=1
    if mode eq 'WINDOWED' then ftype(n)=0
endfor
;stop
;CONCATENATE THE FILELIST AND GENERATE EVT, GTI, AND BADPIX STRUCT'S
concat_evt_060222,flist,evt,gti,bad,sing=sing
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

for n=0l,n_elements(flist)-1 do begin
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

;DETERMINE THE ENDTIME OF ALL SLEWS DURING THE ACS FILES 
read_acsfiles,acsflist,slew_endtimes
;stop

;DETERMINE THE START AND STOP TIMES OF EACH ORBIT IN THE INPUT FILES
psf_corr_ctr=0L
;for n=0l,n_elements(flist)-1 do begin
;    det_orbit,flist(n),start_orb_arr,stop_orb_arr
    det_orbit_gti,gti,ftype,start_orb_arr,stop_orb_arr,orbit_fnum,slew_endtimes
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
            find_src_detpos,src_skyxs(orbit_fnum(m)),src_skyys(orbit_fnum(m)),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf,diag=diag
            if keyword_set(diag) then spawn,'mv sky2det.diagnostic.ps sky2det.diagnostic'+strtrim(string(m),2)+'.ps'
;            if m eq 24 then find_src_detpos,src_skyxs(orbit_fnum(m)-1),src_skyys(orbit_fnum(m)-1),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;CALCULATE A PSF GRID (100x100) ASSUMING ENERGY=2keV and OFFAXIS ANGLE=0.
            model_psf,2.,0.,ef
;DETERMINE THE POSITION OF THE BADPIXELS/COLUMNS WRT THE PSF AND
;CALCULATE THE CORRESPONDING CORRECTION FACTOR
;            if m eq 6 then detxcrf=detxcrf+3.
;if start_orb_arr(m) gt 167691984.000 then stop
            modify_psf_wbadpix,ef,detxcrf,detycrf,psfout,psf_corr_fact,ftype(orbit_fnum(m)),diag=diag
;stop
            if exist('psf_corr_diagnostic.ps') then $
               spawn,'mv psf_corr_diagnostic.ps psf_corr_diagnostic_orb'+strtrim(string(m),2)+'.ps'
            print,psf_corr_fact,detxcrf,detycrf
        endif
        if n_elements(ind) le 4 then psf_corr_fact=1.; IF NOT ENOUGH EVENTS IN ORBIT FOR A FIT THEN ASSUME PSF_CORR=1.0
        if n_elements(ind) le 4 then print,psf_corr_fact
        psf_corr_arr(psf_corr_ctr)=psf_corr_fact
        ind_src=where(evt.time ge start_orb_arr(m) and evt.time le stop_orb_arr(m) and sqrt((evt.x-src_skyxs(orbit_fnum(m)))^2.+(evt.y-src_skyys(orbit_fnum(m)))^2.) lt 30.)
        if ind_src(0) ne -1 then evt(ind_src).fact1=psf_corr_fact
;;        if ind(0) ne -1 then evt(ind).fact1=psf_corr_fact
;        psf_corr_ctr=psf_corr_ctr+1L
        print,n_elements(ind),start_orb_arr(m)-trig_time
;        stop

;CALCULATE THE DETX,DETY CENTROID OF THE BACKGROUND POSITION FOR THIS ORBIT
;stop
        if n_elements(ind) gt 4 then begin
            find_src_detpos,back_skyxs(orbit_fnum(m)),back_skyys(orbit_fnum(m)),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf,diag=diag
            if keyword_set(diag) then spawn,'mv sky2det.diagnostic.ps sky2det_back.diagnostic'+strtrim(string(m),2)+'.ps'
;            if m eq 24 then find_src_detpos,back_skyxs(orbit_fnum(m)-1),back_skyys(orbit_fnum(m)-1),evt(ind).x,evt(ind).y,evt(ind).detx,evt(ind).dety,detxcrf,detycrf
;CALCULATE A PSF GRID (100x100) ASSUMING ENERGY=2keV and OFFAXIS ANGLE=0.
;;            model_psf,2.,0.,ef
            ef=fltarr(2001,2001) ;NO POINT SOURCE SO FLATTEN OUT THE MODEL OF THE PSF
            ef(*)=1.
;DETERMINE THE POSITION OF THE BADPIXELS/COLUMNS WRT THE PSF AND
;CALCULATE THE CORRESPONDING CORRECTION FACTOR
            modify_psf_wbadpix,ef,detxcrf,detycrf,psfout,psf_corr_fact,ftype(orbit_fnum(m)),diag=diag
            spawn,'mv psf_corr_diagnostic.ps psf_corr_diagnostic_back_orb'+strtrim(string(m),2)+'.ps'
            print,psf_corr_fact,detxcrf,detycrf
        endif
        if n_elements(ind) le 4 then psf_corr_fact=1.; IF NOT ENOUGH EVENTS IN ORBIT FOR A FIT THEN ASSUME PSF_CORR=1.0
        if n_elements(ind) le 4 then print,psf_corr_fact
        psf_corr_back_arr(psf_corr_ctr)=psf_corr_fact
        ind_bck=where(evt.time ge start_orb_arr(m) and evt.time le stop_orb_arr(m) and sqrt((evt.x-back_skyxs(orbit_fnum(m)))^2.+(evt.y-back_skyys(orbit_fnum(m)))^2.) lt 40.)
        if ind_bck(0) ne -1 then evt(ind_bck).fact1=psf_corr_fact
;;        if ind(0) ne -1 then evt(ind).fact1=psf_corr_fact
        psf_corr_ctr=psf_corr_ctr+1L
    endfor
;stop
;endfor

;evt_ind_wt=where(evt.ftype eq 0)
;evt_ind_pc=where(evt.ftype eq 1)

;BUILD THE EVT INDEXES FOR THE SOURCE DATA

;evt_dist=sqrt((evt.x-src_skyxs(0))^2.+(evt.y-src_skyys(0))^2.)
;evt_ind_wt=where(evt.ftype eq 0 and evt.fnum eq 0 and evt_dist lt 30.)
;evt_ind_pc=where(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt 30.)

evt_ind_pc=0.
evt_ind_wt=0.
for fctr=0,n_elements(flist)-1 do begin
    evt_dist=sqrt((evt.x-src_skyxs(fctr))^2.+(evt.y-src_skyys(fctr))^2.)
    evt_ind_wt_tmp=where(evt.ftype eq 0 and evt.fnum eq fctr and evt_dist lt 20.);30.)
    evt_ind_pc_tmp=where(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt 30.)
    if evt_ind_wt_tmp(0) ne -1 then evt_ind_wt=[evt_ind_wt,evt_ind_wt_tmp]
    if evt_ind_pc_tmp(0) ne -1 then evt_ind_pc=[evt_ind_pc,evt_ind_pc_tmp]
endfor
if n_elements(evt_ind_pc) gt 1 then evt_ind_pc=evt_ind_pc(1:*) else evt_ind_pc=-1
if n_elements(evt_ind_wt) gt 1 then evt_ind_wt=evt_ind_wt(1:*) else evt_ind_wt=-1



;BUILD THE EVT INDEXES FOR THE BACKGROUND DATA

;evt_dist_back=sqrt((evt.x-back_skyxs(0))^2.+(evt.y-back_skyys(0))^2.)
;evt_ind_wt_back=where(evt.ftype eq 0 and evt.fnum eq 0 and evt_dist_back lt 30.)
;evt_ind_pc_back=where(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist_back lt 30.)
evt_ind_pc_back=0.
evt_ind_wt_back=0.
for fctr=0,n_elements(flist)-1 do begin
    evt_dist_back=sqrt((evt.x-back_skyxs(fctr))^2.+(evt.y-back_skyys(fctr))^2.)
    evt_ind_wt_back_tmp=where(evt.ftype eq 0 and evt.fnum eq fctr and evt_dist_back lt 20.);30.)
    evt_ind_pc_back_tmp=where(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist_back lt 40.);30.)
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
;stop
if evt_ind_wt(0) ne -1 then lcstartt=double(evt(evt_ind_wt(0)).time)
if evt_ind_wt(0) ne -1 then lcstopt=double(evt(evt_ind_wt(n_elements(evt_ind_wt)-1)).time)
if evt_ind_wt(0) ne -1 then elem=ceil((lcstopt-lcstartt)/0.064)
openw,lc64ms_lunout,'64ms_lc.txt',/get_lun
openw,lc64ms_lunoutb,'64ms_lc_b.txt',/get_lun
if evt_ind_wt(0) ne -1 then printf,lc64ms_lunout,elem
if evt_ind_wt(0) ne -1 then printf,lc64ms_lunoutb,elem

if keyword_set(ms64) then begin
for aa=lcstartt,lcstopt,0.064d do begin & print,aa,format='(f16.4)' & printf,lc64ms_lunout,n_elements(where(evt(evt_ind_wt).time gt aa and evt(evt_ind_wt).time le aa+0.064)),format='(i)' & printf,lc64ms_lunoutb,aa,n_elements(where(evt(evt_ind_wt).time gt aa and evt(evt_ind_wt).time le aa+0.064)),format='(f16.4,i)' & endfor 
;for aa=lcstartt,lcstopt,0.064d do begin & printf,lc64ms_lunout,n_elements(where(evt(evt_ind_wt).time gt aa and evt(evt_ind_wt).time le aa+0.064)),format='(i)' & endfor 
endif

free_lun,lc64ms_lunoutb
free_lun,lc64ms_lunout
;stop
pu_class_wt=intarr(n_elements(start_orb_arr))
pu_class_wt(*)=40
;;;;stop
if n_elements(evt_ind_wt) gt 1 then bin_by_rules,evt(evt_ind_wt),evt(evt_ind_wt_back),pu_class_wt,gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bins,time_sigma_arr_wt,bin=bin
pu_class_pc=intarr(n_elements(start_orb_arr))
pu_class_pc(*)=20
if keyword_set(srcrad) then pu_class_pc(*)=srcrad
;;;;stop
if n_elements(evt_ind_pc) gt 1 then bin_by_rules,evt(evt_ind_pc),evt(evt_ind_pc_back),pu_class_pc,gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bins,time_sigma_arr_pc,bin=bin
;stop
;endif
;if keyword_set(lorella) then begin
;    if n_elements(evt_ind_wt) gt 1 then bin_by_rules_lorella,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bins,bin=bin
;    if n_elements(evt_ind_pc) gt 1 then bin_by_rules_lorella,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bins,bin=bin
;endif
;stop

set_plot,'x'
if 0 then begin
window,0
;CT_RATE_ARR_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PILEUP,PSF_CORR,ERROR]
if n_elements(evt_ind_wt) gt 1 then begin
;    stop
fakeflag=0
for n=0l,n_elements(gti)-1 do begin
    gti_evt_ind=where(evt(evt_ind_wt).time ge gti(n).start and evt(evt_ind_wt).time le gti(n).stop)
;    stop
    if gti_evt_ind(0) ne -1 then temprate=1.*n_elements(gti_evt_ind)/(gti(n).stop-gti(n).start) else temprate=0.
    temptime=(gti(n).stop+gti(n).start)/2.
    if n_elements(gti_evt_ind) gt 1 then begin
;    if gti_evt_ind(0) ne -1 then begin
;stop
        fakeburst,trig_time,temprate,temptime,-6.,gti(n).start,gti(n).stop,d_evt_times,evt_times 
        if fakeflag eq 0 then tot_d_evt_times=d_evt_times else tot_d_evt_times=[tot_d_evt_times,d_evt_times]
;get the FFT of the true event arrival times in this GTI
        last_evt_time=evt(evt_ind_wt(gti_evt_ind(n_elements(gti_evt_ind)-1))).time
;stop
        testarr=dblarr(long((last_evt_time-trig_time)/1.78e-3))
        testind=long(double(evt(evt_ind_wt(gti_evt_ind)).time-trig_time)/1.78d-3)
        for m=0,n_elements(testind)-1 do testarr(testind(m)-1)=testarr(testind(m)-1)+1.d
        testarr=testarr(testind(0):*)
        gti_fft=abs(fft(testarr,-1))
        freq_ind=indgen(double(n_elements(gti_fft)),/long) 
        freq=freq_ind/(double(n_elements(gti_fft))*0.00178)
        plot,/ylog,freq,gti_fft,psym=3,/xlog,xrange=[1e-3,1333]
        print,linfit(alog10(freq(1:100)),alog10(gti_fft(1:100)),yfit=y)
        oplot,freq(1:100),10.^y
;stop
;        gti_fft=fft(evt(evt_ind_wt(gti_evt_ind)).time)
;stop
        d_evttime=dblarr(n_elements(gti_evt_ind)-1)
        for m=1,n_elements(d_evttime)-1 do begin
            d_evttime(m)=evt(evt_ind_wt(gti_evt_ind(m))).time-evt(evt_ind_wt(gti_evt_ind(m-1))).time
        endfor
        if fakeflag eq 0 then begin
            tot_d_evttime=d_evttime
            fakeflag=1
        endif else begin
            tot_d_evttime=[tot_d_evttime,d_evttime]
        endelse
    endif
endfor
;stop
    plot,ct_rate_arr_wt(0,*)-trig_time,ct_rate_arr_wt(1,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
    if n_elements(evt_ind_pc) gt 1 then oplot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*),psym=3
endif
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
    PU_rad5=13.
    PU_rad6=15.
    
;    PU_fact1=1.1
    PU_fact1=1.76
    PU_fact2=1.76
    PU_fact3=2.66
    PU_fact4=3.69
;    PU_fact5=5.38
    PU_fact5=7.20
    PU_fact6=8.46

    ;;MODS SHOULD BEGIN HERE
;    stop
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

    
    pu_class1=intarr(n_elements(ct_rate_arr_pc(0,*)))
    if pu_indm7(0) ne -1 then pu_class1(pu_indm7)=5
    if pu_indm6(0) ne -1 then pu_class1(pu_indm6)=7
    if pu_indm5(0) ne -1 then pu_class1(pu_indm5)=9
    if pu_indm4(0) ne -1 then pu_class1(pu_indm4)=12
    if pu_indm3(0) ne -1 then pu_class1(pu_indm3)=15
    if pu_indm2(0) ne -1 then pu_class1(pu_indm2)=20
    if pu_indm1(0) ne -1 then pu_class1(pu_indm1)=25
    if pu_ind1(0) ne -1 then pu_class1(pu_ind1)=30
    if pu_ind2(0) ne -1 then pu_class1(pu_ind2)=30
    if pu_ind3(0) ne -1 then pu_class1(pu_ind3)=30
    if pu_ind4(0) ne -1 then pu_class1(pu_ind4)=30
    if pu_ind5(0) ne -1 then pu_class1(pu_ind5)=30
    if pu_ind6(0) ne -1 then pu_class1(pu_ind6)=30

    if not(keyword_set(srcrad)) then begin
    for nn=0,n_elements(start_orb_arr)-1 do begin
        ind=where(start_orb_arr(nn) lt ct_rate_arr_pc(9,*) and stop_orb_arr(nn) gt ct_rate_arr_pc(8,*))
        if ind(0) ne -1 then pu_class_pc(nn)=mean(pu_class1(ind))
        if ind(0) eq -1 and nn gt 0 then pu_class_pc(nn)=pu_class_pc(nn-1) 
        if ind(0) eq -1 and nn eq 0 then pu_class_pc(nn)=20.
     endfor
 endif
   

    if pu_ind6(0) ne -1 then begin
        for n=0l,n_elements(pu_ind6)-1 do oplot,[ct_rate_arr_pc(8,pu_ind6(n)),ct_rate_arr_pc(9,pu_ind6(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind6)-1 do xyouts,ct_rate_arr_pc(8,pu_ind6(n))-trig_time,2.5,'6'
        for n=0l,n_elements(pu_ind6)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind6(n)) and evt.time le ct_rate_arr_pc(9,pu_ind6(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=6
            if ind(0) ne -1 then  evt(ind).fact2=8.46;MULTIPLY UP BY THIS FACTOR TO ACCOUNT FOR THE HOLE IN THE MIDDLE OF THE PILEUP CORRECTION
            if ind(0) ne -1 then  evt(ind).fact3=1.333;MULTIPLY BACKGROUND UP BY THIS FACTOR TO ACCOUNT FOR THE MISSING BACKGROUND AREA (FLAT - NOT PSF SHAPED)
        endfor
    endif
    if pu_ind5(0) ne -1 then begin
        for n=0l,n_elements(pu_ind5)-1 do oplot,[ct_rate_arr_pc(8,pu_ind5(n)),ct_rate_arr_pc(9,pu_ind5(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind5)-1 do xyouts,ct_rate_arr_pc(8,pu_ind5(n))-trig_time,2.5,'5'
        for n=0l,n_elements(pu_ind5)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind5(n)) and evt.time le ct_rate_arr_pc(9,pu_ind5(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=5
;            if ind(0) ne -1 then  evt(ind).fact2=5.38
            if ind(0) ne -1 then  evt(ind).fact2=7.20
            if ind(0) ne -1 then  evt(ind).fact3=1.125
        endfor
    endif
    if pu_ind4(0) ne -1 then begin
        for n=0l,n_elements(pu_ind4)-1 do oplot,[ct_rate_arr_pc(8,pu_ind4(n)),ct_rate_arr_pc(9,pu_ind4(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind4)-1 do xyouts,ct_rate_arr_pc(8,pu_ind4(n))-trig_time,2.5,'4'
        for n=0l,n_elements(pu_ind4)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind4(n)) and evt.time le ct_rate_arr_pc(9,pu_ind4(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=4
            if ind(0) ne -1 then  evt(ind).fact2=3.69
            if ind(0) ne -1 then  evt(ind).fact3=1.06

        endfor
    endif
    if pu_ind3(0) ne -1 then begin
        for n=0l,n_elements(pu_ind3)-1 do oplot,[ct_rate_arr_pc(8,pu_ind3(n)),ct_rate_arr_pc(9,pu_ind3(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind3)-1 do xyouts,ct_rate_arr_pc(8,pu_ind3(n))-trig_time,2.5,'3'
        for n=0l,n_elements(pu_ind3)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind3(n)) and evt.time le ct_rate_arr_pc(9,pu_ind3(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=3
            if ind(0) ne -1 then  evt(ind).fact2=2.66
            if ind(0) ne -1 then  evt(ind).fact3=1.03

        endfor
    endif
    if pu_ind2(0) ne -1 then begin
        for n=0l,n_elements(pu_ind2)-1 do oplot,[ct_rate_arr_pc(8,pu_ind2(n)),ct_rate_arr_pc(9,pu_ind2(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind2)-1 do xyouts,ct_rate_arr_pc(8,pu_ind2(n))-trig_time,2.5,'2'
        for n=0l,n_elements(pu_ind2)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind2(n)) and evt.time le ct_rate_arr_pc(9,pu_ind2(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=2
            if ind(0) ne -1 then  evt(ind).fact2=1.76
            if ind(0) ne -1 then  evt(ind).fact3=1.01

        endfor
    endif
    if pu_ind1(0) ne -1 then begin
        for n=0l,n_elements(pu_ind1)-1 do oplot,[ct_rate_arr_pc(8,pu_ind1(n)),ct_rate_arr_pc(9,pu_ind1(n))]-trig_time,[2.,2.]
        for n=0l,n_elements(pu_ind1)-1 do xyouts,ct_rate_arr_pc(8,pu_ind1(n))-trig_time,2.5,'1'
        for n=0l,n_elements(pu_ind1)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_ind1(n)) and evt.time le ct_rate_arr_pc(9,pu_ind1(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=1
;            if ind(0) ne -1 then  evt(ind).fact2=1.1
            if ind(0) ne -1 then  evt(ind).fact2=1.76
            if ind(0) ne -1 then  evt(ind).fact3=1.001

        endfor
    endif
    if pu_indm1(0) ne -1 then begin
        for n=0l,n_elements(pu_indm1)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm1(n)) and evt.time le ct_rate_arr_pc(9,pu_indm1(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-1
            if ind(0) ne -1 then  evt(ind).fact2=1.07
            if ind(0) ne -1 then  evt(ind).fact3=1.44

        endfor
    endif
    if pu_indm2(0) ne -1 then begin
        for n=0l,n_elements(pu_indm2)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm2(n)) and evt.time le ct_rate_arr_pc(9,pu_indm2(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-2
            if ind(0) ne -1 then  evt(ind).fact2=1.09
            if ind(0) ne -1 then  evt(ind).fact3=2.25

        endfor
    endif
    if pu_indm3(0) ne -1 then begin
        for n=0l,n_elements(pu_indm3)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm3(n)) and evt.time le ct_rate_arr_pc(9,pu_indm3(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-3
            if ind(0) ne -1 then  evt(ind).fact2=1.134
            if ind(0) ne -1 then  evt(ind).fact3=4.

        endfor
    endif
    if pu_indm4(0) ne -1 then begin
        for n=0l,n_elements(pu_indm4)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm4(n)) and evt.time le ct_rate_arr_pc(9,pu_indm4(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-4
            if ind(0) ne -1 then  evt(ind).fact2=1.18
            if ind(0) ne -1 then  evt(ind).fact3=6.25

        endfor
    endif
    if pu_indm5(0) ne -1 then begin
        for n=0l,n_elements(pu_indm5)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm5(n)) and evt.time le ct_rate_arr_pc(9,pu_indm5(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-5
            if ind(0) ne -1 then  evt(ind).fact2=1.26
            if ind(0) ne -1 then  evt(ind).fact3=11.11

        endfor
    endif
    if pu_indm6(0) ne -1 then begin
        for n=0l,n_elements(pu_indm6)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm6(n)) and evt.time le ct_rate_arr_pc(9,pu_indm6(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-6
            if ind(0) ne -1 then  evt(ind).fact2=1.37
            if ind(0) ne -1 then  evt(ind).fact3=18.4

        endfor
    endif
    if pu_indm7(0) ne -1 then begin
        for n=0l,n_elements(pu_indm7)-1 do begin
            ind=where(evt.time ge ct_rate_arr_pc(8,pu_indm7(n)) and evt.time le ct_rate_arr_pc(9,pu_indm7(n)) and evt.ftype eq 1)
            if ind(0) ne -1 then  evt(ind).pileup=-7
            if ind(0) ne -1 then  evt(ind).fact2=1.603
            if ind(0) ne -1 then  evt(ind).fact3=36.

        endfor
    endif
;stop
    evt_dist=sqrt((evt.x-src_skyxs(0))^2.+(evt.y-src_skyys(0))^2.)
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

    if keyword_set(srcrad) then begin
        critm7=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -7) ;< 5e-4
        critm6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -6) ;5e-4 < < 1e-3
        critm5=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -5) ;1e-3 < < 5e-3
        critm4=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -4) ;5e-3 < < 1e-2
        critm3=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -3) ;1e-2 < < 5e-2
        critm2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -2) ;5e-2 < < 0.1
        critm1=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -1) ;0.1 < < 0.5
        
        crit1=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt.pileup eq 0)
;    crit2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 1. and evt.pileup eq 1)
        crit2=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 3. and evt.pileup eq 1)
        crit3=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 3. and evt.pileup eq 2)
        crit4=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 5. and evt.pileup eq 3)
        crit5=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 7. and evt.pileup eq 4)
;    crit6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 10. and evt.pileup eq 5)
        crit6=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 13. and evt.pileup eq 5)
        crit7=(evt.ftype eq 1 and evt.fnum eq 0 and evt_dist lt srcrad and evt_dist gt 15. and evt.pileup eq 6)
    endif

    ;;;;FOR CLAUDIO
    if keyword_set(srcrad) then begin
       if srcrad ge 0. and srcrad lt 5. then begin
          evt.fact2=1.603
          evt.fact3=36.
       endif
       if srcrad ge 5. and srcrad lt 7. then begin
          evt.fact2=1.37
          evt.fact3=18.4
       endif
       if srcrad ge 7. and srcrad lt 9. then begin
          evt.fact2=1.25
          evt.fact3=11.11
       endif
       if srcrad ge 9. and srcrad lt 12. then begin
          evt.fact2=1.18
          evt.fact3=6.25
       endif
       if srcrad ge 12. and srcrad lt 15. then begin
          evt.fact2=1.134
          evt.fact3=4.
       endif
       if srcrad ge 15. and srcrad lt 20. then begin
          evt.fact2=1.09
          evt.fact3=2.25
       endif
       if srcrad ge 20. and srcrad lt 25. then begin
          evt.fact2=1.07
          evt.fact3=1.44
       endif
       if srcrad ge 25. and srcrad lt 30. then begin
          evt.fact2=1.76
          evt.fact3=1.01
       endif
       if srcrad ge 30. then begin
          evt.fact2=1.76
          evt.fact3=1.001
       endif
    endif
    ;;;;END FOR CLAUDIO

;    evt_ind_pc=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)
    evt_ind_pc=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)

    evt_ind_pc=0.
;stop
    for fctr=0,n_elements(flist)-1 do begin
        print,'sky and skyy or file ',fctr,' is ',src_skyxs(fctr),' ',src_skyys(fctr)
;        stop
        evt_dist=sqrt((evt.x-src_skyxs(fctr))^2.+(evt.y-src_skyys(fctr))^2.)
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

        if keyword_set(srcrad) then begin
            critm7=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -7) ;< 5e-4
            critm6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -6) ;5e-4 < < 1e-3
            critm5=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -5) ;1e-3 < < 5e-3
            critm4=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -4) ;5e-3 < < 1e-2
            critm3=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -3) ;1e-2 < < 5e-2
            critm2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -2) ;5e-2 < < 0.1
            critm1=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 0. and evt.pileup eq -1) ;0.1 < < 0.5
            
            crit1=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt.pileup eq 0)
;        crit2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 1. and evt.pileup eq 1)
            crit2=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 3. and evt.pileup eq 1)
            crit3=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 3. and evt.pileup eq 2)
            crit4=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 5. and evt.pileup eq 3)
            crit5=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 7. and evt.pileup eq 4)
;        crit6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 10. and evt.pileup eq 5)
            crit6=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 13. and evt.pileup eq 5)
            crit7=(evt.ftype eq 1 and evt.fnum eq fctr and evt_dist lt srcrad and evt_dist gt 15. and evt.pileup eq 6)
        endif
	;stop

;        evt_ind_pc_tmp=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)
        evt_ind_pc_tmp=where(critm7 or critm6 or critm5 or critm4 or critm3 or critm2 or critm1 or crit2 or crit3 or crit4 or crit5 or crit6 or crit7)
;;    evt_ind_wt=[evt_ind_wt,evt_ind_wt_tmp]
        if evt_ind_pc_tmp(0) ne -1 then evt_ind_pc=[evt_ind_pc,evt_ind_pc_tmp]
;    stop
    endfor
;stop
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
pu_class_wt=intarr(n_elements(start_orb_arr))
pu_class_wt(*)=40
;;;;stop
    if n_elements(evt_ind_wt) gt 1 then bin_by_rules,evt(evt_ind_wt),evt(evt_ind_wt_back),pu_class_wt,gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bins,time_sigma_arr_wt,bin=bin
;;;;stop
    if n_elements(evt_ind_pc) gt 1 then bin_by_rules,evt(evt_ind_pc),evt(evt_ind_pc_back),pu_class_pc,gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bins,time_sigma_arr_pc,bin=bin
;stop
;endif
;stop
if 0 then begin
fakeflag=0
for n=0l,n_elements(gti)-1 do begin
    gti_evt_ind=where(evt(evt_ind_pc).time ge gti(n).start and evt(evt_ind_pc).time le gti(n).stop)
    if gti_evt_ind(0) ne -1 then temprate=1.*n_elements(gti_evt_ind)/(gti(n).stop-gti(n).start) else temprate=0.
    temptime=(gti(n).stop+gti(n).start)/2.
    if n_elements(gti_evt_ind) gt 1 then begin
;    if gti_evt_ind(0) ne -1 then begin
;stop
        fakeburst,trig_time,temprate,temptime,-0.1,gti(n).start,gti(n).stop,d_evt_times,evt_times 
        if fakeflag eq 0 then tot_d_evt_times=d_evt_times else tot_d_evt_times=[tot_d_evt_times,d_evt_times]
;get the FFT of the true event arrival times in this GTI

;get the FFT of the true event arrival times in this GTI
        last_evt_time=evt(evt_ind_pc(gti_evt_ind(n_elements(gti_evt_ind)-1))).time
;stop
        testarr=dblarr(long((last_evt_time-trig_time)/2.5))
        testind=long(double(evt(evt_ind_pc(gti_evt_ind)).time-trig_time)/2.5)
        for m=0,n_elements(testind)-1 do testarr(testind(m)-1)=testarr(testind(m)-1)+1.d
        testarr=testarr(testind(0):*)
        gti_fft=abs(fft(testarr,-1))
        freq_ind=indgen(double(n_elements(gti_fft)),/long) 
        freq=freq_ind/(double(n_elements(gti_fft))*2.5)
        plot,/ylog,freq,gti_fft,psym=3,/xlog,xrange=[1e-3,1000]
        print,linfit(alog10(freq(1:100)),alog10(gti_fft(1:100)),yfit=y)
        oplot,freq(1:100),10.^y
;stop

        gti_fft=fft(evt(evt_ind_pc(gti_evt_ind)).time)
;stop
        d_evttime=dblarr(n_elements(gti_evt_ind)-1)
        for m=1,n_elements(d_evttime)-1 do begin
            d_evttime(m)=evt(evt_ind_pc(gti_evt_ind(m))).time-evt(evt_ind_pc(gti_evt_ind(m-1))).time
        endfor
        if fakeflag eq 0 then begin
            tot_d_evttime=d_evttime
            fakeflag=1
        endif else begin
            tot_d_evttime=[tot_d_evttime,d_evttime]
        endelse
    endif
endfor
endif
;stop

;if keyword_set(lorella)) then begin
;    if n_elements(evt_ind_wt) gt 1 then bin_by_rules_lorella,evt(evt_ind_wt),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_wt,1,trig_time,bins,bin=bin
;    if n_elements(evt_ind_pc) gt 1 then bin_by_rules_lorella,evt(evt_ind_pc),gti,ftype,start_orb_arr,stop_orb_arr,ct_rate_arr_pc,0,trig_time,bins,bin=bin
;endif
;stop
    set_plot,'x'
    window,1
endif;PILEUP LOOP

;CALCULATE THE AVERAGE PILEUP CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0.d,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then pileup_fact=total(evt(evt_ind_pc(ind)).fact2)/n_elements(ind)
    if ind(0) eq -1 then pileup_fact=1.
    ct_rate_arr_pc(10,n)=pileup_fact
 endfor
;stop
if n_elements(ct_rate_arr_wt) gt 1 then for n=0.d,n_elements(ct_rate_arr_wt(0,*))-1 do begin;0.d,n_elements(ct_rate_arr_wt(0,*))-1 do begin
   print,n
;   tenthway=long(n_elements(evt_ind_wt)/20.)
;;;	tenthway=long(n_elements(evt_ind_wt)/10.)

;   fourthway=long(n_elements(evt_ind_wt)/4.)
;fourthbins=long(n_elements(ct_rate_arr_wt(0,*))/4.)
;   bins_arr=lonarr(20)
;;;	bins_arr=lonarr(10)
;   for a=1,19 do bins_arr(a-1)=where(ct_rate_arr_wt(8,*) lt evt(evt_ind_wt(a*tenthway)).time and ct_rate_arr_wt(9,*) ge evt(evt_ind_wt(a*tenthway)).time)
;;;for a=1,9 do bins_arr(a-1)=where(ct_rate_arr_wt(8,*) lt evt(evt_ind_wt(a*tenthway)).time and ct_rate_arr_wt(9,*) ge evt(evt_ind_wt(a*tenthway)).time)

;  if n lt bins_arr(0) then ind=where(evt(evt_ind_wt(0:tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(0:tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(0:tenthway-1)).ftype eq 0) else if n ge bins_arr(0) and n lt bins_arr(1) then ind=where(evt(evt_ind_wt(tenthway:2*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).ftype eq 0) else if n ge bins_arr(1) and n lt bins_arr(2) then ind=where(evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).ftype eq 0) else if n ge bins_arr(2) and n lt bins_arr(3) then ind=where(evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).ftype eq 0) else if n ge bins_arr(3) and n lt bins_arr(4) then ind=where(evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).ftype eq 0) else if n ge bins_arr(4) and n lt bins_arr(5) then ind=where(evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).ftype eq 0) else if n ge bins_arr(5) and n lt bins_arr(6) then ind=where(evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).ftype eq 0) else if n ge bins_arr(6) and n lt bins_arr(7) then ind=where(evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).ftype eq 0) else if n ge bins_arr(7) and n lt bins_arr(8) then ind=where(evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).ftype eq 0) else if n ge bins_arr(8) and n lt bins_arr(9) then ind=where(evt(evt_ind_wt(9*tenthway:10*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(9*tenthway:10*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(9*tenthway:10*tenthway-1)).ftype eq 0) else if n ge bins_arr(9) and n lt bins_arr(10) then ind=where(evt(evt_ind_wt(10*tenthway:11*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(10*tenthway:11*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(10*tenthway:11*tenthway-1)).ftype eq 0) else if n ge bins_arr(10) and n lt bins_arr(11) then ind=where(evt(evt_ind_wt(11*tenthway:12*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(11*tenthway:12*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(11*tenthway:12*tenthway-1)).ftype eq 0) else if n ge bins_arr(11) and n lt bins_arr(12) then ind=where(evt(evt_ind_wt(12*tenthway:13*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(12*tenthway:13*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(12*tenthway:13*tenthway-1)).ftype eq 0) else if n ge bins_arr(12) and n lt bins_arr(13) then ind=where(evt(evt_ind_wt(13*tenthway:14*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(13*tenthway:14*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(13*tenthway:14*tenthway-1)).ftype eq 0) else if n ge bins_arr(13) and n lt bins_arr(14) then ind=where(evt(evt_ind_wt(14*tenthway:15*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(14*tenthway:15*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(14*tenthway:15*tenthway-1)).ftype eq 0) else if n ge bins_arr(14) and n lt bins_arr(15) then ind=where(evt(evt_ind_wt(15*tenthway:16*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(15*tenthway:16*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(15*tenthway:16*tenthway-1)).ftype eq 0) else if n ge bins_arr(15) and n lt bins_arr(16) then ind=where(evt(evt_ind_wt(16*tenthway:17*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(16*tenthway:17*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(16*tenthway:17*tenthway-1)).ftype eq 0) else if n ge bins_arr(16) and n lt bins_arr(17) then ind=where(evt(evt_ind_wt(17*tenthway:18*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(17*tenthway:18*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(17*tenthway:18*tenthway-1)).ftype eq 0) else if n ge bins_arr(17) and n lt bins_arr(18) then ind=where(evt(evt_ind_wt(18*tenthway:19*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(18*tenthway:19*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(18*tenthway:19*tenthway-1)).ftype eq 0) else if n ge bins_arr(18) then ind=where(evt(evt_ind_wt(19*tenthway:*)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(19*tenthway:*)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(19*tenthway:*)).ftype eq 0)
;;;  if n lt bins_arr(0) then ind=where(evt(evt_ind_wt(0:tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(0:tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(0:tenthway-1)).ftype eq 0) else if n ge bins_arr(0) and n lt bins_arr(1) then ind=where(evt(evt_ind_wt(tenthway:2*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).ftype eq 0) else if n ge bins_arr(1) and n lt bins_arr(2) then ind=where(evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).ftype eq 0) else if n ge bins_arr(2) and n lt bins_arr(3) then ind=where(evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).ftype eq 0) else if n ge bins_arr(3) and n lt bins_arr(4) then ind=where(evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).ftype eq 0) else if n ge bins_arr(4) and n lt bins_arr(5) then ind=where(evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).ftype eq 0) else if n ge bins_arr(5) and n lt bins_arr(6) then ind=where(evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).ftype eq 0) else if n ge bins_arr(6) and n lt bins_arr(7) then ind=where(evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).ftype eq 0) else if n ge bins_arr(7) and n lt bins_arr(8) then ind=where(evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).ftype eq 0) else if n ge bins_arr(8) then ind=where(evt(evt_ind_wt(9*tenthway:*)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(9*tenthway:*)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(9*tenthway:*)).ftype eq 0)

;;;  test=where(evt(evt_ind_wt(ind)).fact2 ne 1.) 


;;;   if ind(0) ne -1 and test(0) ne -1 then pileup_fact=total(evt(evt_ind_wt(ind)).fact2)/n_elements(ind) else pileup_fact=1.
;   if ind(0) eq -1 then pileup_fact=1.
;;;   ct_rate_arr_wt(10,n)=pileup_fact
   ct_rate_arr_wt(10,n)=1.
endfor

;;CHECK;;
;CALCULATE THE AVERAGE PSF CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0.d,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_pc(ind)).fact1)/n_elements(ind)
    if ind(0) eq -1 then psf_corr_fact=1.
    ct_rate_arr_pc(11,n)=psf_corr_fact
;if n ge 80 then stop
endfor
if n_elements(ct_rate_arr_wt) gt 1 then for n=0.d,n_elements(ct_rate_arr_wt(0,*))-1 do begin

;    ind=where(evt(evt_ind_wt).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt).ftype eq 0)
;    if ind(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt(ind)).fact1)/n_elements(ind)
;    if ind(0) eq -1 then psf_corr_fact=1.
;    ct_rate_arr_wt(11,n)=psf_corr_fact
   ct_rate_arr_wt(11,n)=1.
;;FIX;;!!

;   tenthway=long(n_elements(evt_ind_wt)/20.)
	tenthway=long(n_elements(evt_ind_wt)/10.)

;   fourthway=long(n_elements(evt_ind_wt)/4.)
;fourthbins=long(n_elements(ct_rate_arr_wt(0,*))/4.)
;   bins_arr=lonarr(20)
	bins_arr=lonarr(10)
;   for a=1,19 do bins_arr(a-1)=where(ct_rate_arr_wt(8,*) lt evt(evt_ind_wt(a*tenthway)).time and ct_rate_arr_wt(9,*) ge evt(evt_ind_wt(a*tenthway)).time)
for a=1,9 do bins_arr(a-1)=where(ct_rate_arr_wt(8,*) lt evt(evt_ind_wt(a*tenthway)).time and ct_rate_arr_wt(9,*) ge evt(evt_ind_wt(a*tenthway)).time)

;  if n lt bins_arr(0) then ind=where(evt(evt_ind_wt(0:tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(0:tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(0:tenthway-1)).ftype eq 0) else if n ge bins_arr(0) and n lt bins_arr(1) then ind=where(evt(evt_ind_wt(tenthway:2*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).ftype eq 0) else if n ge bins_arr(1) and n lt bins_arr(2) then ind=where(evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).ftype eq 0) else if n ge bins_arr(2) and n lt bins_arr(3) then ind=where(evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).ftype eq 0) else if n ge bins_arr(3) and n lt bins_arr(4) then ind=where(evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).ftype eq 0) else if n ge bins_arr(4) and n lt bins_arr(5) then ind=where(evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).ftype eq 0) else if n ge bins_arr(5) and n lt bins_arr(6) then ind=where(evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).ftype eq 0) else if n ge bins_arr(6) and n lt bins_arr(7) then ind=where(evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).ftype eq 0) else if n ge bins_arr(7) and n lt bins_arr(8) then ind=where(evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).ftype eq 0) else if n ge bins_arr(8) and n lt bins_arr(9) then ind=where(evt(evt_ind_wt(9*tenthway:10*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(9*tenthway:10*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(9*tenthway:10*tenthway-1)).ftype eq 0) else if n ge bins_arr(9) and n lt bins_arr(10) then ind=where(evt(evt_ind_wt(10*tenthway:11*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(10*tenthway:11*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(10*tenthway:11*tenthway-1)).ftype eq 0) else if n ge bins_arr(10) and n lt bins_arr(11) then ind=where(evt(evt_ind_wt(11*tenthway:12*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(11*tenthway:12*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(11*tenthway:12*tenthway-1)).ftype eq 0) else if n ge bins_arr(11) and n lt bins_arr(12) then ind=where(evt(evt_ind_wt(12*tenthway:13*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(12*tenthway:13*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(12*tenthway:13*tenthway-1)).ftype eq 0) else if n ge bins_arr(12) and n lt bins_arr(13) then ind=where(evt(evt_ind_wt(13*tenthway:14*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(13*tenthway:14*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(13*tenthway:14*tenthway-1)).ftype eq 0) else if n ge bins_arr(13) and n lt bins_arr(14) then ind=where(evt(evt_ind_wt(14*tenthway:15*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(14*tenthway:15*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(14*tenthway:15*tenthway-1)).ftype eq 0) else if n ge bins_arr(14) and n lt bins_arr(15) then ind=where(evt(evt_ind_wt(15*tenthway:16*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(15*tenthway:16*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(15*tenthway:16*tenthway-1)).ftype eq 0) else if n ge bins_arr(15) and n lt bins_arr(16) then ind=where(evt(evt_ind_wt(16*tenthway:17*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(16*tenthway:17*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(16*tenthway:17*tenthway-1)).ftype eq 0) else if n ge bins_arr(16) and n lt bins_arr(17) then ind=where(evt(evt_ind_wt(17*tenthway:18*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(17*tenthway:18*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(17*tenthway:18*tenthway-1)).ftype eq 0) else if n ge bins_arr(17) and n lt bins_arr(18) then ind=where(evt(evt_ind_wt(18*tenthway:19*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(18*tenthway:19*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(18*tenthway:19*tenthway-1)).ftype eq 0) else if n ge bins_arr(18) then ind=where(evt(evt_ind_wt(19*tenthway:*)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(19*tenthway:*)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(19*tenthway:*)).ftype eq 0)
;if n gt 10180 then stop
  if n lt bins_arr(0) then ind=where(evt(evt_ind_wt(0:tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(0:tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(0:tenthway-1)).ftype eq 0) else if n ge bins_arr(0) and n lt bins_arr(1) then ind=1.*tenthway+where(evt(evt_ind_wt(tenthway:2*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(tenthway:2*tenthway-1)).ftype eq 0) else if n ge bins_arr(1) and n lt bins_arr(2) then ind=2.*tenthway+where(evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(2*tenthway:3*tenthway-1)).ftype eq 0) else if n ge bins_arr(2) and n lt bins_arr(3) then ind=3.*tenthway+where(evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(3*tenthway:4*tenthway-1)).ftype eq 0) else if n ge bins_arr(3) and n lt bins_arr(4) then ind=4.*tenthway+where(evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(4*tenthway:5*tenthway-1)).ftype eq 0) else if n ge bins_arr(4) and n lt bins_arr(5) then ind=5.*tenthway+where(evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(5*tenthway:6*tenthway-1)).ftype eq 0) else if n ge bins_arr(5) and n lt bins_arr(6) then ind=6.*tenthway+where(evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(6*tenthway:7*tenthway-1)).ftype eq 0) else if n ge bins_arr(6) and n lt bins_arr(7) then ind=7.*tenthway+where(evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(7*tenthway:8*tenthway-1)).ftype eq 0) else if n ge bins_arr(7) and n lt bins_arr(8) then ind=8.*tenthway+where(evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(8*tenthway:9*tenthway-1)).ftype eq 0) else if n ge bins_arr(8) then ind=9.*tenthway+where(evt(evt_ind_wt(9*tenthway:*)).time ge ct_rate_arr_wt(8,n) and evt(evt_ind_wt(9*tenthway:*)).time le ct_rate_arr_wt(9,n) and evt(evt_ind_wt(9*tenthway:*)).ftype eq 0)

  test=where(evt(evt_ind_wt(ind)).fact1 ne 1.) 
;test2=where(evt(evt_ind_wt(ind)).fact1 ge 2.)
;if test2(0) ne -1 then stop

   if ind(0) ne -1 and test(0) ne -1 then psf_corr_fact=total(evt(evt_ind_wt(ind)).fact1)/n_elements(ind) else psf_corr_fact=1.
;   if ind(0) eq -1 then pileup_fact=1.
   ct_rate_arr_wt(11,n)=psf_corr_fact
   print,n
endfor

;CALCULATE THE AVERAGE FACT3 FACTOR OF ALL EVENTS THAT GO INTO EACH
;BIN AND FILL IN THE APPROPRIATE FACTOR TO THE CT_RATE_ARR_PC ARRAY
if n_elements(ct_rate_arr_pc) gt 1 then for n=0.d,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    ind=where(evt(evt_ind_pc).time ge ct_rate_arr_pc(8,n) and evt(evt_ind_pc).time le ct_rate_arr_pc(9,n) and evt(evt_ind_pc).ftype eq 1)
    if ind(0) ne -1 then fact3_tot=total(evt(evt_ind_pc(ind)).fact3)/n_elements(ind)
    if ind(0) eq -1 then fact3_tot=1.
    ct_rate_arr_pc(16,n)=fact3_tot
endfor

if n_elements(ct_rate_arr_wt) gt 1 then for n=0.d,n_elements(ct_rate_arr_wt(0,*))-1 do begin
;    ind=where(evt(evt_ind_wt).time ge ct_rate_arr_wt(8,n) and
;    evt(evt_ind_wt).time le ct_rate_arr_wt(9,n) and
;    evt(evt_ind_wt).ftype eq 0)
;    if ind(0) ne -1 then fact3_tot=total(evt(evt_ind_wt(ind)).fact3)/n_elements(ind)
;    if ind(0) eq -1 then fact3_tot=1.
;    ct_rate_arr_wt(16,n)=fact3_tot
   ct_rate_arr_wt(16,n)=1.
endfor

;stop
if not(keyword_set(wt_noback)) then if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_wt) gt 0 then calc_backgr,evt(evt_ind_wt_back),ct_rate_arr_wt,0,ftype,gti,ct_rate_arr_back_wt,pu_class_wt,start_orb_arr,stop_orb_arr,srcrad_arr_wt
if keyword_set(wt_noback) then begin
   if evt_ind_wt_back(0) ne -1 then ct_rate_arr_back_wt=dblarr(59,n_elements(ct_rate_arr_wt(0,*))) ; ADD BACKGROUND AND ERROR AND BACK_START_TIME AND BACK_STOP_TIME IN BACKGROUND TO ARRAY
   if evt_ind_wt_back(0) ne -1 then ct_rate_arr_back_wt(0:16,*)=ct_rate_arr_wt
   if evt_ind_wt_back(0) ne -1 then srcrad_arr_wt=fltarr(n_elements(ct_rate_arr_wt(0,*)))
endif
;stop

;srcrad_arr_wt(*)=0.
;stop
if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_pc) gt 1 then calc_backgr,evt(evt_ind_pc_back),ct_rate_arr_pc,1,ftype,gti,ct_rate_arr_back_pc,pu_class_pc,start_orb_arr,stop_orb_arr,srcrad_arr_pc
;stop

;stop
;CALCULATE THE ERRORS
if n_elements(ct_rate_arr_wt) gt 1 then lim_wt=intarr(n_elements(ct_rate_arr_wt(0,*)))
if n_elements(ct_rate_arr_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_wt(0,*))-1 do begin
;;stop
    for nn=2,2 do begin
	scl_rat=srcrad_arr_wt(n)^2./40.^2.
;        error=sqrt(ct_rate_arr_wt(nn,n)+ct_rate_arr_back_wt(17+((nn-2)*8),n)*scl_rat^2.)/ct_rate_arr_wt(6,n)
        error=sqrt(ct_rate_arr_wt(nn,n)+ct_rate_arr_back_wt(18+((nn-2)*8),n)*ct_rate_arr_wt(6,n))/ct_rate_arr_wt(6,n)
;        if error ge ct_rate_arr_wt(nn-1,n)/siglim then stop
;;stop
        if error ge ct_rate_arr_wt(nn-1,n)/siglim then begin
;stop
;;            if ct_rate_arr_back_wt(nn,n) gt ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2. then confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(nn,n),99.73,smin,smax
;;	    if ct_rate_arr_back_wt(nn,n) le ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2. then confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.*1.001,99.73,smin,smax
           confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(nn,n),99.73,smin,smax
	    if smin eq 0. then begin
              ; stop
               ct_rate_arr_back_wt(10+nn,n)=-1.*ct_rate_arr_back_wt(nn,n)
               ct_rate_arr_back_wt(nn,n)=smax
;;;NEW;;;		ct_rate_arr_back_wt(17+((nn-2)*8),n)=smax*40.^2./srcrad_arr_wt(n)^2.
		lim_wt(n)=1
                error=0.
	endif
	if smin gt 0. then begin
;;		if ct_rate_arr_back_wt(nn,n) gt ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2. then confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(nn,n),68.3,smin,smax
;;		if ct_rate_arr_back_wt(nn,n) le ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2. then confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.*1.001,68.3,smin,smax
           confidlev,ct_rate_arr_back_wt(17+((nn-2)*8),n)*srcrad_arr_wt(n)^2./40.^2.,ct_rate_arr_back_wt(nn,n),68.3,smin,smax
           error=(ct_rate_arr_back_wt(nn,n)-smin)/ct_rate_arr_wt(6,n)
	endif
        endif
        if error ne 0. then ct_rate_arr_back_wt(10+nn,n)=error*ct_rate_arr_wt(10,n)*ct_rate_arr_wt(11,n)
    endfor
endfor
if n_elements(ct_rate_arr_pc) gt 1 then lim_pc=intarr(n_elements(ct_rate_arr_pc(0,*)))
;if n_elements(ct_rate_arr_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_pc(0,*))-1 do begin
n=0l
if n_elements(ct_rate_arr_pc) gt 1 then test_ctr=n_elements(ct_rate_arr_pc(0,*))
;stop
if n_elements(ct_rate_arr_pc) gt 1 then while n lt test_ctr do begin
print,'n is ',n
;;   stop
    for nn=2,2 do begin
	scl_rat=srcrad_arr_pc(n)^2./40.^2.
;        error=sqrt(ct_rate_arr_pc(nn,n)+ct_rate_arr_back_pc(17+((nn-2)*8),n)*scl_rat^2.)/ct_rate_arr_pc(6,n)
        error=sqrt(ct_rate_arr_pc(nn,n)+ct_rate_arr_back_pc(18+((nn-2)*8),n)*ct_rate_arr_pc(6,n))/ct_rate_arr_pc(6,n)
;;	stop
	smin=1.
        if error ge ct_rate_arr_pc(nn-1,n)/siglim then begin
;           stop
;;           if ct_rate_arr_back_pc(nn,n) gt ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2. then confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(nn,n),99.73,smin,smax
;;           if ct_rate_arr_back_pc(nn,n) le ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2. then confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.*1.001,99.73,smin,smax
           confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(nn,n),99.73,smin,smax
           if smin eq 0. then begin
		if n eq n_elements(ct_rate_arr_pc(0,*))-1 then begin
             ; stop
              ct_rate_arr_back_pc(10+nn,n)=-1.*ct_rate_arr_back_pc(nn,n)
              ct_rate_arr_back_pc(nn,n)=smax
;;;NEW;;;              ct_rate_arr_back_pc(17+((nn-2)*8),n)=smax*40.^2./srcrad_arr_pc(n)^2.
              lim_pc(n)=1
              error=0.
           endif
		if n lt n_elements(ct_rate_arr_pc(0,*))-1 then begin
			ct_rate_arr_back_pc(9,n)=ct_rate_arr_back_pc(9,n+1)
			ct_rate_arr_back_pc(0,n)=(ct_rate_arr_back_pc(8,n)+ct_rate_arr_back_pc(9,n))/2.
			ct_rate_arr_back_pc(12,n)=(ct_rate_arr_back_pc(12,n)+ct_rate_arr_back_pc(12,n+1))/2.
			ct_rate_arr_back_pc(6,n)=ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(6,n+1)
			ct_rate_arr_back_pc(2,n)=ct_rate_arr_back_pc(2,n)+ct_rate_arr_back_pc(2,n+1)
			ct_rate_arr_back_pc(18,n)=(ct_rate_arr_back_pc(18,n)+ct_rate_arr_back_pc(18,n+1))/2.
			ct_rate_arr_back_pc(11,n)=(ct_rate_arr_back_pc(11,n)+ct_rate_arr_back_pc(11,n+1))/2.	
			ct_rate_arr_back_pc(1,n)=(ct_rate_arr_back_pc(1,n)+ct_rate_arr_back_pc(1,n+1))/2.
			ct_rate_arr_back_pc(18,n)=(ct_rate_arr_back_pc(18,n)+ct_rate_arr_back_pc(18,n+1))/2.
			ct_rate_arr_back_pc(16,n)=(ct_rate_arr_back_pc(16,n)+ct_rate_arr_back_pc(16,n+1))/2.
			ct_rate_arr_back_pc(10,n)=(ct_rate_arr_back_pc(10,n)+ct_rate_arr_back_pc(10,n+1))/2.
			if n_elements(ct_rate_arr_pc(0,*))-1 gt n+1 then for mm=n+1,n_elements(ct_rate_arr_pc(0,*))-2 do ct_rate_arr_pc(*,mm)=ct_rate_arr_pc(*,mm+1)
                        if n_elements(ct_rate_arr_back_pc(0,*))-1 gt n+1 then for mm=n+1,n_elements(ct_rate_arr_back_pc(0,*))-2 do ct_rate_arr_back_pc(*,mm)=ct_rate_arr_back_pc(*,mm+1)
			n_ele_arr=n_elements(ct_rate_arr_pc(0,*))
                        ct_rate_arr_pc=ct_rate_arr_pc(*,0:n_ele_arr-2)
                        n_ele_arr=n_elements(ct_rate_arr_back_pc(0,*))
                        ct_rate_arr_back_pc=ct_rate_arr_back_pc(*,0:n_ele_arr-2)
			n=n-1
		endif
				
	endif
           if smin gt 0. then begin
;;              if ct_rate_arr_back_pc(nn,n) gt ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2. then confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(nn,n),68.3,smin,smax
;;              if ct_rate_arr_back_pc(nn,n) le ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2. then confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.*1.001,68.3,smin,smax
              confidlev,ct_rate_arr_back_pc(17+((nn-2)*8),n)*srcrad_arr_pc(n)^2./40.^2.,ct_rate_arr_back_pc(nn,n),68.3,smin,smax
              error=(ct_rate_arr_back_pc(nn,n)-smin)/ct_rate_arr_pc(6,n)
           endif
;stop
        endif
;stop
        smin_test=size(smin)
        if error ne 0. then begin
           if smin_test(1) ne 0 then if smin ne 0. then ct_rate_arr_back_pc(10+nn,n)=error*ct_rate_arr_pc(10,n)*ct_rate_arr_pc(11,n)
           if smin_test(1) eq 0 then ct_rate_arr_back_pc(10+nn,n)=error*ct_rate_arr_pc(10,n)*ct_rate_arr_pc(11,n)
        endif
;stop
     endfor
	test_ctr=n_elements(ct_rate_arr_pc(0,*))
	n=n+1l
endwhile
; endfor
;;CHECK;;
!p.multi=[0,1,1]
;stop
;CT_RATE_ARR_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,EBIN_CTS1,EBIN_CTS2,EBIN_CTS3,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PUC,PSFCORR,ERROR,FACT3]
if n_elements(ct_rate_arr_wt) gt 1 then plot,ct_rate_arr_wt(0,*)-trig_time,ct_rate_arr_wt(1,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if n_elements(ct_rate_arr_pc) gt 1 then oplot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*)*ct_rate_arr_pc(10,*),psym=3
if n_elements(ct_rate_arr_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_wt(0,*))-1 do begin
    oplot,[ct_rate_arr_wt(0,n)-trig_time,ct_rate_arr_wt(0,n)-trig_time],[ct_rate_arr_wt(1,n)-ct_rate_arr_wt(12,n),ct_rate_arr_wt(1,n)+ct_rate_arr_wt(12,n)]
endfor
if n_elements(ct_rate_arr_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_pc(0,n)-trig_time,ct_rate_arr_pc(0,n)-trig_time],[ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)-ct_rate_arr_pc(12,n),ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)+ct_rate_arr_pc(12,n)]
endfor

if n_elements(ct_rate_arr_pc) gt 1 then plot,ct_rate_arr_pc(0,*)-trig_time,ct_rate_arr_pc(1,*)*ct_rate_arr_pc(10,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if n_elements(ct_rate_arr_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_pc(0,n)-trig_time,ct_rate_arr_pc(0,n)-trig_time],[ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)-ct_rate_arr_pc(12,n),ct_rate_arr_pc(1,n)*ct_rate_arr_pc(10,n)+ct_rate_arr_pc(12,n)]
endfor
;stop
;stop
;if evt_ind_wt_back(0) ne -1 then if n_elements(ct_rate_arr_wt) gt 1 then calc_backgr,evt(evt_ind_wt_back),ct_rate_arr_wt,0,ftype,gti,ct_rate_arr_back_wt,pu_class_wt,start_orb_arr,stop_orb_arr,srcrad_arr_wt
;stop
;;;NEW TEST )052307
;if evt_ind_pc_back(0) ne -1 then if n_elements(ct_rate_arr_pc) gt 1 then calc_backgr,evt(evt_ind_pc_back),ct_rate_arr_pc,1,ftype,gti,ct_rate_arr_back_pc,pu_class_pc,start_orb_arr,stop_orb_arr,srcrad_arr_pc
;END NEW TEST



if evt_ind_wt_back(0) eq -1 and n_elements(ct_rate_arr_wt) gt 0 then begin
    ct_rate_arr_back_wt=dblarr(59,n_elements(ct_rate_arr_wt(0,*))) ; ADD BACKGROUND AND ERROR AND BACK_START_TIME AND BACK_STOP_TIME IN BACKGROUND TO ARRAY
    ct_rate_arr_back_wt(0:16,*)=ct_rate_arr_wt
    ct_rate_arr_back_wt(18,*)=0.05
    ct_rate_arr_back_wt(26,*)=0.05
    ct_rate_arr_back_wt(34,*)=0.05
    ct_rate_arr_back_wt(42,*)=0.05

    ct_rate_arr_back_wt(24,*)=1.
    ct_rate_arr_back_wt(32,*)=1.
    ct_rate_arr_back_wt(40,*)=1.
    ct_rate_arr_back_wt(48,*)=1.
endif
if evt_ind_pc_back(0) eq -1 and n_elements(ct_rate_arr_pc) gt 0 then begin
    ct_rate_arr_back_pc=dblarr(59,n_elements(ct_rate_arr_pc(0,*))) ; ADD BACKGROUND AND ERROR AND BACK_START_TIME AND BACK_STOP_TIME IN BACKGROUND TO ARRAY
    ct_rate_arr_back_pc(0:16,*)=ct_rate_arr_pc
;stop
    ct_rate_arr_back_pc(18,*)=0.05
    ct_rate_arr_back_pc(26,*)=0.05
    ct_rate_arr_back_pc(34,*)=0.05
    ct_rate_arr_back_pc(42,*)=0.05

    ct_rate_arr_back_pc(24,*)=1.
    ct_rate_arr_back_pc(32,*)=1.
    ct_rate_arr_back_pc(40,*)=1.
    ct_rate_arr_back_pc(48,*)=1.
    
endif

;stop

;stop
;CALCULATE THE AVERAGE PSF CORRECTION FACTOR OF ALL EVENTS THAT GO
;INTO EACH **BACKGROUND** BIN AND FILL IN THE APPROPRIATE FACTOR TO THE
;CT_RATE_ARR_PC ARRAY
;stop
if n_elements(ct_rate_arr_pc) gt 1 and evt_ind_pc_back(0) ne -1 then for n=0l,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
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
if n_elements(ct_rate_arr_back_wt) gt 1 and evt_ind_wt_back(0) ne -1 then for n=0l,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
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
;stop
;NOW ASSIGN HALF THE EXTRA TIME OF EACH GAP TO THE PREVIOUS BIN AND
;HALF TO THE SUBSEQUENT BIN
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
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
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
print,n
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
;stop
;NOW ADJUST THE SRC AND BACKGR CT RATES FOR THE EXTRA TIME

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    ct_rate_arr_back_pc(1,n)=ct_rate_arr_back_pc(2,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n));SRC ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_pc(18,*)) gt 1 then ct_rate_arr_back_pc(18,n)=ct_rate_arr_back_pc(17,n)*srcrad_arr_pc(n)^2./40.^2./(ct_rate_arr_back_pc(23,n)+ct_rate_arr_back_pc(51,n)+ct_rate_arr_back_pc(52,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_pc(26,*)) gt 1 then ct_rate_arr_back_pc(26,n)=ct_rate_arr_back_pc(25,n)/(ct_rate_arr_back_pc(31,n)+ct_rate_arr_back_pc(53,n)+ct_rate_arr_back_pc(54,n)) ;TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_pc(34,*)) gt 1 then ct_rate_arr_back_pc(34,n)=ct_rate_arr_back_pc(33,n)/(ct_rate_arr_back_pc(39,n)+ct_rate_arr_back_pc(55,n)+ct_rate_arr_back_pc(56,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_pc(42,*)) gt 1 then ct_rate_arr_back_pc(42,n)=ct_rate_arr_back_pc(41,n)/(ct_rate_arr_back_pc(47,n)+ct_rate_arr_back_pc(57,n)+ct_rate_arr_back_pc(58,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
endfor
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
   print,n,n
    ct_rate_arr_back_wt(1,n)=ct_rate_arr_back_wt(2,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n));SRC ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_wt(18,*)) gt 1 and ct_rate_arr_back_wt(23,n) gt 0. then ct_rate_arr_back_wt(18,n)=ct_rate_arr_back_wt(17,n)*srcrad_arr_wt(n)^2./40.^2./(ct_rate_arr_back_wt(23,n)+ct_rate_arr_back_wt(51,n)+ct_rate_arr_back_wt(52,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_wt(26,*)) gt 1 and ct_rate_arr_back_wt(31,n) gt 0. then ct_rate_arr_back_wt(26,n)=ct_rate_arr_back_wt(25,n)/(ct_rate_arr_back_wt(31,n)+ct_rate_arr_back_wt(53,n)+ct_rate_arr_back_wt(54,n)) ;TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_wt(34,*)) gt 1 and ct_rate_arr_back_wt(39,n) gt 0. then ct_rate_arr_back_wt(34,n)=ct_rate_arr_back_wt(33,n)/(ct_rate_arr_back_wt(39,n)+ct_rate_arr_back_wt(55,n)+ct_rate_arr_back_wt(56,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
    if n_elements(ct_rate_arr_back_wt(42,*)) gt 1 and ct_rate_arr_back_wt(47,n) gt 0. then ct_rate_arr_back_wt(42,n)=ct_rate_arr_back_wt(41,n)/(ct_rate_arr_back_wt(47,n)+ct_rate_arr_back_wt(57,n)+ct_rate_arr_back_wt(58,n));TOT BACKGR ADJUSTED FOR EXTRA TIME
endfor
;stop

!p.multi=[0,1,1]
;NOW PLOT SRC-BACKGR LC
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then plot,ct_rate_arr_back_pc(0,*)-trig_time,ct_rate_arr_back_pc(1,*)*ct_rate_arr_back_pc(10,*)*ct_rate_arr_back_pc(11,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then plot,ct_rate_arr_back_wt(0,*)-trig_time,ct_rate_arr_back_wt(1,*)*ct_rate_arr_back_wt(10,*)*ct_rate_arr_back_wt(11,*),psym=3,/ylog,yrange=[0.001,1000.],/xlog,xrange=[10.,1e6]

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    oplot,[ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(0,n)-trig_time],[ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(12,n),ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)+ct_rate_arr_back_pc(12,n)]
endfor

if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
    oplot,[ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(0,n)-trig_time],[ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(12,n),ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)+ct_rate_arr_back_wt(12,n)]
endfor


if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then ind_pc=where(ct_rate_arr_back_pc(0,*)-trig_time-thunds_time eq min(ct_rate_arr_back_pc(0,*)-trig_time-thunds_time))
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then ind_wt=where(ct_rate_arr_back_wt(0,*)-trig_time-thunds_time eq min(ct_rate_arr_back_wt(0,*)-trig_time-thunds_time))

if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 and n_elements(ct_rate_arr_back_wt) gt 1 then begin
    if abs(ct_rate_arr_back_pc(0,ind_pc)-trig_time-thunds_time) lt abs(ct_rate_arr_back_wt(0,ind_wt)-trig_time-thunds_time) then print,'300s cts is ',ct_rate_arr_back_pc(1:4,ind_pc),' ',ct_rate_arr_back_pc(8,ind_pc)-trig_time,' ',ct_rate_arr_back_pc(9,ind_pc)-trig_time
    if abs(ct_rate_arr_back_pc(0,ind_pc)-trig_time-thunds_time) ge abs(ct_rate_arr_back_wt(0,ind_wt)-trig_time-thunds_time) then print,'300s cts is ',ct_rate_arr_back_wt(1:4,ind_wt),' ',ct_rate_arr_back_wt(8,ind_wt)-trig_time,' ',ct_rate_arr_back_wt(9,ind_wt)-trig_time
endif else begin
    if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then print,'300s cts is ',ct_rate_arr_back_pc(1:4,ind_pc),' ',ct_rate_arr_back_pc(8,ind_pc)-trig_time,' ',ct_rate_arr_back_pc(9,ind_pc)-trig_time
    if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then print,'300s cts is ',ct_rate_arr_back_wt(1:4,ind_wt),' ',ct_rate_arr_back_wt(8,ind_wt)-trig_time,' ',ct_rate_arr_back_wt(9,ind_wt)-trig_time
endelse
;stop

;CT_RATE_ARR_BACK_PC CONSISTS OF [BIN_TIME,RATE,TOT_COUNTS,TRUE_DURATION,DEADTIME,BIN_START,BIN_STOP,PUC,PSF_CORR,ERROR,BACKGR_CTS,BACKGR_RATE,BACKGR_ERROR_RATE,BACKGR_STARTT,BACKGR_STOPT,BACKGR_DEAD,BACKGR_TRUE_DUR,EMPTY_PREV_EXTRA_SRCTIME,EMPTY_LATER_EXTRA_SRCTIME,EMPTY_PREV_EXTRA_BACKTIME,EMPTY_LATER_EXTRA_BACKTIME]
openw,lunout,'lc_newout.txt',/get_lun
;stop
printf,lunout,'time    t_start    t_stop   src_rate   src_rate_err   tot_hard   tot_hard_err   exptime    src_counts  back_ctrate   det_sig   psf_corr   junk   junk   rate1   rate2   rate3  rate1_err   rate2_err    rate3_err   hard1   hard2   hard1_err   hard2_err src_rate   back_rate   back_area_corr   pu_corr    psf_corr  somejunk  e1_cts  e2_cts  e3_cts  somejunk somejunk somejunk tot_back_cts  hard_ratio_1  hard_ratio_2  hard_ratio_err1  hard_ratio_err2 mean_time sigma_time'
printf,lunout,'junk2'
;stop
if evt_ind_wt(0) ne -1 then if n_elements(ct_rate_arr_back_wt) gt 1 then for n=0l,n_elements(ct_rate_arr_back_wt(0,*))-1 do begin
   print,n,n,n
    hardness1=ct_rate_arr_back_wt(4,n)/ct_rate_arr_back_wt(3,n)
    hardness2=ct_rate_arr_back_wt(5,n)/ct_rate_arr_back_wt(4,n)
;    error_in_hardness=sqrt(ct_rate_arr_back_wt(4,n)+ct_rate_arr_back_wt(5,n));NOT RIGHT
    err_soft=sqrt(ct_rate_arr_back_wt(3,n)+ct_rate_arr_back_wt(25,n))
    err_mid=sqrt(ct_rate_arr_back_wt(4,n)+ct_rate_arr_back_wt(33,n))
    err_hard=sqrt(ct_rate_arr_back_wt(5,n)+ct_rate_arr_back_wt(41,n))
    error_in_hardness1=sqrt(hardness1^2.*(err_soft^2./ct_rate_arr_back_wt(3,n)^2.+err_mid^2./ct_rate_arr_back_wt(4,n)^2.))
    error_in_hardness2=sqrt(hardness2^2.*(err_mid^2./ct_rate_arr_back_wt(4,n)^2.+err_hard^2./ct_rate_arr_back_wt(5,n)^2.))
    tot_hard=(ct_rate_arr_back_wt(5,n)+ct_rate_arr_back_wt(4,n))/ct_rate_arr_back_wt(3,n)
    tot_harderr=sqrt(tot_hard^2.*(1./(ct_rate_arr_back_wt(5,n)+ct_rate_arr_back_wt(4,n))+1./(ct_rate_arr_back_wt(3,n))))
    error_in_hardness=sqrt(error_in_hardness1^2.+error_in_hardness2^2.)

;    det_sig=sqrt(ct_rate_arr_back_wt(2,n))
    det_sig=ct_rate_arr_back_wt(1,n)/ct_rate_arr_back_wt(12,n)
    det_sig=ct_rate_arr_back_wt(2,n)/sqrt(ct_rate_arr_back_wt(2,n)+2.*ct_rate_arr_back_wt(18,n)*ct_rate_arr_wt(6,n))
;    rate=ct_rate_arr_back_wt(1,n)*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(18,n)
;    rate=(ct_rate_arr_back_wt(1,n)-ct_rate_arr_back_wt(18,n)/ct_rate_arr_back_wt(16,n))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(18,n)
    rate=(ct_rate_arr_back_wt(1,n))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(18,n)
;    rate1=(ct_rate_arr_back_wt(3,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(26,n)
;    rate2=(ct_rate_arr_back_wt(4,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(34,n)
;    rate3=(ct_rate_arr_back_wt(5,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n)-ct_rate_arr_back_wt(42,n)
    rate1=((ct_rate_arr_back_wt(3,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(26,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(26,n)
    rate2=((ct_rate_arr_back_wt(4,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(34,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(34,n)
    rate3=((ct_rate_arr_back_wt(5,n)/(ct_rate_arr_back_wt(6,n)+ct_rate_arr_back_wt(49,n)+ct_rate_arr_back_wt(50,n)))-(ct_rate_arr_back_wt(42,n)/ct_rate_arr_back_wt(16,n)))*ct_rate_arr_back_wt(10,n)*ct_rate_arr_back_wt(11,n);-ct_rate_arr_back_wt(42,n)
;    printf,lunout,format='(40d16.4)',ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(8,n)-trig_time,ct_rate_arr_back_wt(9,n)-trig_time,rate,ct_rate_arr_back_wt(12,n),hardness1*hardness2+hardness1,error_in_hardness,ct_rate_arr_back_wt(6,n),ct_rate_arr_back_wt(2,n),ct_rate_arr_back_wt(18,n),det_sig,ct_rate_arr_back_wt(11,n),-999,0,rate1,rate2,rate3,ct_rate_arr_back_wt(13,n),ct_rate_arr_back_wt(14,n),ct_rate_arr_back_wt(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2,ct_rate_arr_back_wt(1,n),ct_rate_arr_back_wt(18,n),ct_rate_arr_back_wt(16,n),ct_rate_arr_back_wt(10,n),ct_rate_arr_back_wt(11,n),ct_rate_arr_back_wt(3,n),ct_rate_arr_back_wt(4,n),ct_rate_arr_back_wt(5,n),ct_rate_arr_back_wt(26,n),ct_rate_arr_back_wt(34,n),ct_rate_arr_back_wt(42,n)
;    if ct_rate_arr_back_wt(2,n) eq ct_rate_arr_back_wt(17,n) then begin
;    	rate=ct_rate_arr_back_wt(2,n)/ct_rate_arr_back_wt(16,n)
;	ct_rate_arr_back_wt(12,n)=rate
;endif	
	if lim_wt(n) eq 1 then ct_rate_arr_back_wt(12,n)=rate
	if lim_wt(n) eq 1 then det_sig=1.
        cts1=ct_rate_arr_back_wt(3,n)
        cts2=ct_rate_arr_back_wt(4,n)
        cts3=ct_rate_arr_back_wt(5,n)
        hard_rate_err1=sqrt(((cts2+cts1)/(cts2-cts1)^2.+(cts2+cts1)/(cts2+cts1)^2.)*((cts2-cts1)/(cts2+cts1))^2.)
        hard_rate_err2=sqrt(((cts3+cts2)/(cts3-cts2)^2.+(cts3+cts2)/(cts3+cts2)^2.)*((cts3-cts2)/(cts3+cts2))^2.)
      printf,lunout,format='(43d17.5)',ct_rate_arr_back_wt(0,n)-trig_time,ct_rate_arr_back_wt(8,n)-trig_time,ct_rate_arr_back_wt(9,n)-trig_time,rate,ct_rate_arr_back_wt(12,n),tot_hard,tot_harderr,ct_rate_arr_back_wt(6,n),ct_rate_arr_back_wt(2,n),ct_rate_arr_back_wt(18,n),det_sig,ct_rate_arr_back_wt(11,n),-999,0,rate1,rate2,rate3,ct_rate_arr_back_wt(13,n),ct_rate_arr_back_wt(14,n),ct_rate_arr_back_wt(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2,ct_rate_arr_back_wt(1,n),ct_rate_arr_back_wt(18,n),ct_rate_arr_back_wt(16,n),ct_rate_arr_back_wt(10,n),ct_rate_arr_back_wt(11,n),ct_rate_arr_back_wt(3,n),ct_rate_arr_back_wt(4,n),ct_rate_arr_back_wt(5,n),ct_rate_arr_back_wt(26,n),ct_rate_arr_back_wt(34,n),ct_rate_arr_back_wt(42,n),ct_rate_arr_back_wt(18,n)*ct_rate_arr_wt(6,n),(rate2-rate1)/(rate2+rate1),(rate3-rate2)/(rate3+rate2),hard_rate_err1,hard_rate_err2,time_sigma_arr_wt(0,n)-trig_time,time_sigma_arr_wt(1,n)
;stop
    
endfor
if evt_ind_pc(0) ne -1 then if n_elements(ct_rate_arr_back_pc) gt 1 then for n=0l,n_elements(ct_rate_arr_back_pc(0,*))-1 do begin
    hardness1=ct_rate_arr_back_pc(4,n)/ct_rate_arr_back_pc(3,n)
    hardness2=ct_rate_arr_back_pc(5,n)/ct_rate_arr_back_pc(4,n)
;    error_in_hardness=sqrt(ct_rate_arr_back_pc(4,n)+ct_rate_arr_back_pc(5,n));NOT RIGHT
    err_soft=sqrt(ct_rate_arr_back_pc(3,n)+ct_rate_arr_back_pc(25,n))
    err_mid=sqrt(ct_rate_arr_back_pc(4,n)+ct_rate_arr_back_pc(33,n))
    err_hard=sqrt(ct_rate_arr_back_pc(5,n)+ct_rate_arr_back_pc(41,n))
    error_in_hardness1=sqrt(hardness1^2.*(err_soft^2./ct_rate_arr_back_pc(3,n)^2.+err_mid^2./ct_rate_arr_back_pc(4,n)^2.))
    error_in_hardness2=sqrt(hardness2^2.*(err_mid^2./ct_rate_arr_back_pc(4,n)^2.+err_hard^2./ct_rate_arr_back_pc(5,n)^2.))
    tot_hard=(ct_rate_arr_back_pc(5,n)+ct_rate_arr_back_pc(4,n))/ct_rate_arr_back_pc(3,n)
    tot_harderr=sqrt(tot_hard^2.*(1./(ct_rate_arr_back_pc(5,n)+ct_rate_arr_back_pc(4,n))+1./(ct_rate_arr_back_pc(3,n))))
    error_in_hardness=sqrt(error_in_hardness1^2.+error_in_hardness2^2.)
;    det_sig=sqrt(ct_rate_arr_back_pc(2,n))
    det_sig=ct_rate_arr_back_pc(1,n)/ct_rate_arr_back_pc(12,n)
    det_sig=ct_rate_arr_back_pc(2,n)/sqrt(ct_rate_arr_back_pc(2,n)+2.*ct_rate_arr_back_pc(18,n)*ct_rate_arr_pc(6,n))

;    rate=ct_rate_arr_back_pc(1,n)*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(18,n)
;    rate=(ct_rate_arr_back_pc(1,n)-ct_rate_arr_back_pc(18,n)/ct_rate_arr_back_pc(16,n))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n);-ct_rate_arr_back_pc(18,n)
    rate=(ct_rate_arr_back_pc(1,n))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n);-ct_rate_arr_back_pc(18,n)
;	if ct_rate_arr_back_pc(2,n) eq ct_rate_arr_back_pc(17,n) then begin
;	        rate=ct_rate_arr_back_pc(2,n)/ct_rate_arr_back_pc(16,n)
;		        ct_rate_arr_back_pc(12,n)=rate
;			endif

;;;;    rate=(ct_rate_arr_back_pc(1,n)-ct_rate_arr_back_pc(18,n)/ct_rate_arr_back_pc(16,n))*ct_rate_arr_back_pc(10,n);*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(18,n)
;    rate1=(ct_rate_arr_back_pc(3,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(25,n)
;    rate2=(ct_rate_arr_back_pc(4,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(34,n)
;    rate3=(ct_rate_arr_back_pc(5,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n)-ct_rate_arr_back_pc(42,n)
    rate1=((ct_rate_arr_back_pc(3,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(26,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(26,n)
    rate2=((ct_rate_arr_back_pc(4,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(34,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(34,n)
    rate3=((ct_rate_arr_back_pc(5,n)/(ct_rate_arr_back_pc(6,n)+ct_rate_arr_back_pc(49,n)+ct_rate_arr_back_pc(50,n)))-(ct_rate_arr_back_pc(42,n)/ct_rate_arr_back_pc(16,n)))*ct_rate_arr_back_pc(10,n)*ct_rate_arr_back_pc(11,n) ;-ct_rate_arr_back_pc(42,n)
;    printf,lunout,format='(40d16.4)',ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(8,n)-trig_time,ct_rate_arr_back_pc(9,n)-trig_time,rate,ct_rate_arr_back_pc(12,n),hardness1*hardness2+hardness1,error_in_hardness,ct_rate_arr_back_pc(6,n),ct_rate_arr_back_pc(2,n),ct_rate_arr_back_pc(18,n),det_sig,ct_rate_arr_back_pc(11,n),-999,1,rate1,rate2,rate3,ct_rate_arr_back_pc(13,n),ct_rate_arr_back_pc(14,n),ct_rate_arr_back_pc(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2,ct_rate_arr_back_pc(1,n),ct_rate_arr_back_pc(18,n),ct_rate_arr_back_pc(16,n),ct_rate_arr_back_pc(10,n),ct_rate_arr_back_pc(11,n),ct_rate_arr_back_pc(3,n),ct_rate_arr_back_pc(4,n),ct_rate_arr_back_pc(5,n),ct_rate_arr_back_pc(26,n),ct_rate_arr_back_pc(34,n),ct_rate_arr_back_pc(42,n)
;    if lim_pc(n) eq 1 then ct_rate_arr_back_pc(12,n)=rate
    if lim_pc(n) eq 1 then det_sig=1.
    cts1=ct_rate_arr_back_pc(3,n)
    cts2=ct_rate_arr_back_pc(4,n)
    cts3=ct_rate_arr_back_pc(5,n)
    hard_rate_err1=sqrt(((cts2+cts1)/(cts2-cts1)^2.+(cts2+cts1)/(cts2+cts1)^2.)*((cts2-cts1)/(cts2+cts1))^2.)
    hard_rate_err2=sqrt(((cts3+cts2)/(cts3-cts2)^2.+(cts3+cts2)/(cts3+cts2)^2.)*((cts3-cts2)/(cts3+cts2))^2.)
    ;stop
    printf,lunout,format='(43d17.5)',ct_rate_arr_back_pc(0,n)-trig_time,ct_rate_arr_back_pc(8,n)-trig_time,ct_rate_arr_back_pc(9,n)-trig_time,rate,ct_rate_arr_back_pc(12,n),tot_hard,tot_harderr,ct_rate_arr_back_pc(6,n),ct_rate_arr_back_pc(2,n),ct_rate_arr_back_pc(18,n),det_sig,ct_rate_arr_back_pc(11,n),-999,1,rate1,rate2,rate3,ct_rate_arr_back_pc(13,n),ct_rate_arr_back_pc(14,n),ct_rate_arr_back_pc(15,n),hardness1,hardness2,error_in_hardness1,error_in_hardness2,ct_rate_arr_back_pc(1,n),ct_rate_arr_back_pc(18,n),ct_rate_arr_back_pc(16,n),ct_rate_arr_back_pc(10,n),ct_rate_arr_back_pc(11,n),ct_rate_arr_back_pc(3,n),ct_rate_arr_back_pc(4,n),ct_rate_arr_back_pc(5,n),ct_rate_arr_back_pc(26,n),ct_rate_arr_back_pc(34,n),ct_rate_arr_back_pc(42,n),ct_rate_arr_back_pc(18,n)*ct_rate_arr_pc(6,n),(rate2-rate1)/(rate2+rate1),(rate3-rate2)/(rate3+rate2),hard_rate_err1,hard_rate_err2,time_sigma_arr_pc(0,n)-trig_time,time_sigma_arr_pc(1,n)
    
endfor
free_lun,lunout

if keyword_set(lorella) then begin
openr,lunin,'lc_newout.txt',/get_lun
line=parse_str(lunin)
line=parse_str(lunin)
ctr=0l
if bin(0) eq 100 and bin(1) eq 100 and bin(2) eq 100 and bin(3) eq 100 then openw,lun_lorella,'lorella_100.txt',/get_lun
if bin(0) eq 25 and bin(1) eq 25 and bin(2) eq 25 and bin(3) eq 25 then openw,lun_lorella,'lorella_25.txt',/get_lun
if bin(0) eq 10 and bin(1) eq 10 and bin(2) eq 10 and bin(3) eq 10 then openw,lun_lorella,'lorella_10.txt',/get_lun

printf,format='(16a16)',lun_lorella,' ','time','tstart','tstop','rate','rate_err','rate1','rate2','rate3','rate1_err','rate2_err','rate3_err','hard1','hard2','hard1_err','hard2_err' 
while not eof(lunin) do begin
    line=parse_str(lunin)
    time_temp=0.d
    time_start=0.d
    time_stop=0.d
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
        time_start=time_start+float(line(1))
        time_stop=time_stop+float(line(2))
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
        time_start=time_start/4.
        time_stop=time_stop/4.
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
    time_start=0.d
    time_stop=0.d
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
    if ctr gt 1 then if line(0) gt thunds_time and oldline1(0) le thunds_time then begin
        nextline=parse_str(lunin)
        time=(1.*nextline(0)+1.*line(0)+1.*oldline1(0)+1.*oldline2(0))/4.
        time_start=(1.*nextline(1)+1.*line(1)+1.*oldline1(1)+1.*oldline2(1))/4.
        time_stop=(1.*nextline(2)+1.*line(2)+1.*oldline1(2)+1.*oldline2(2))/4.

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
        printf,format='(a16,15f16.5)',lun_lorella,'300s vals are ',time,time_start,time_stop,rate,rate_err,rate1,rate2,rate3,rate1_err,rate2_err,rate3_err,hard1,hard2,hard1_err,hard2_err
                                ;       stop
    endif
    oldline1=line
    if ctr gt 0 then oldline2=oldline1
    ctr=ctr+1l
endwhile
free_lun,lun_lorella
endif
print,'FINISHED'
;stop
end
