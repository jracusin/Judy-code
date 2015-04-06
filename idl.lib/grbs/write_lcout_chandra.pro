pro write_lcout_chandra
  
  
  ;;GRB 050724
  
;  tstart=[560000,205920,214612,225551,238353,248605,1867280d]
;  dur=[47109,8270,9610,12546,13065,6465,43218d]
;  flux=[1.3,5.8,4.93,3.76,3.62,5.14,0.312]*1e-14
;  fluxerr=[0.75,1.08,0.93,0.72,0.69,1.16,0.12]*1e-14
;  xrtfluxtocounts=2.39992d-13/0.00347119
;  tstop=tstart+dur
;  time=tstart+dur/2.
  
  ;;GRB 051221A
;  tstart=[130.9,136.3,141.8,148.2,154.8,384.1,393.6,404.0,1311.9,1726.7,2247.8]*1e3
;  tstop=[136.3,141.8,148.2,154.8,161.1,393.6,404.0,414.3,1329.8,1751.3,2296.3]*1e3
;  time=(tstop-tstart)/2.+tstart
;  flux=[1.85,1.79,1.56,1.51,1.44,.515,.469,.449,.0464,0.032,0.0157]*1e-13
;  fluxerr=[0.19,0.18,0.16,0.15,0.15,0.075,0.068,0.067,0.00165,0.00118,0.0061]*1e-13
;  xrtfluxtocounts=9.01517e-14/0.00175511;4.06749e-14/0.000980877
  
  ;;GRB 060729
  tstart=[1.9844789e7,2.5107449e7,2.8983989e7,4.4430569e7,5.5396589e7]
  tstop=[1.9942889e7,2.5148609e7,2.9046929e7,4.5306629e7,5.5742969e7]
;  dur=[27690,40268.]
;  tstop=tstart+dur
;  time=tstart+dur/2.
  time=(tstop-tstart)/2.+tstart
  flux=[3.93,2.67,2.07,0.69,0.26]*1e-15
  fluxerr=[1.28,0.83,0.72,0.35,0.18]*1e-15
  xrtfluxtocounts=3.65782e-14/0.00051999200;3.85842e-13/0.00729732
  
  ;;GRB050525A LRPD from wiki not chandra
;  time=[130.000,134.000,138.000,142.000,146.000,150.000,154.000,158.000,162.000,166.000,170.000,174.000,178.000,182.000,186.000,190.000,194.000,198.000,202.000,206.000,210.000,214.000,218.000,222.000,226.000,230.000,236.000,244.000,252.000,264.000]
;  timerr=[2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,2.00000,4.00000,4.00000,4.00000,8.00000]
;  tstart=time-timerr/2.
;  tstop=time+timerr/2.
;  flux=[1.2700000e-09,1.1600000e-09,1.1900000e-09,1.1200000e-09,9.1600000e-10,9.0400000e-10,9.5000000e-10,9.1000000e-10,9.2200000e-10,8.0600000e-10,7.8900000e-10,8.0600000e-10,8.1200000e-10,8.9900000e-10,7.7800000e-10,6.6800000e-10,7.8300000e-10,6.8000000e-10,7.1400000e-10,7.0800000e-10,5.7000000e-10,6.1100000e-10,6.9100000e-10,6.2800000e-10,5.9300000e-10,6.7400000e-10,5.7300000e-10,5.4700000e-10,4.7500000e-10,4.9500000e-10]
;  fluxerr=[9.2200000e-11,8.8900000e-11,8.9800000e-11,8.7400000e-11,8.0400000e-11,8.0000000e-11,8.1700000e-11,8.0200000e-11,8.0600000e-11,7.6400000e-11,7.5800000e-11,7.6400000e-11,7.6600000e-11,7.9800000e-11,7.5300000e-11,7.1000000e-11,7.5500000e-11,7.1500000e-11,7.2900000e-11,7.2600000e-11,6.6900000e-11,6.8600000e-11,7.1900000e-11,6.9400000e-11,6.7900000e-11,7.1200000e-11,4.7400000e-11,4.6600000e-11,4.4300000e-11,3.1800000e-11]
;  xrtfluxtocounts=3.00265e-13/0.00365407
  
  ;;GRB050408 adding 1st orbit PC data that won't process through pipeline from phil's
;  time=[2598.978943,2665.673034,2733.447644,2796.553899,2864.897001]
;  tstart=time-[43.250860,37.233371,34.803661,30.212936,33.366338]
;  tstop=time+[29.460760,30.463649,27.878739,34.976804,46.867122]
;  cxo_counts=[0.337126,0.362311,0.410642,0.376246,0.397284]
;  cxo_err=[0.075517,0.081087,0.089760,0.084205,0.078020]
  
  cxo_counts=flux/xrtfluxtocounts
  cxo_err=fluxerr/xrtfluxtocounts
  
  s=sort(time)
  openw,lun,'lc_newout_chandra.txt',/get_lun
  
;  printf,lun,'time    t_start    t_stop   src_rate   src_rate_err   tot_hard   tot_hard_err   exptime    src_counts  back_ctrate   det_sig   psf_corr   junk   junk   rate1   rate2   rate3  rate1_err   rate2_err    rate3_err   hard1   hard2   hard1_err   hard2_err src_rate   back_rate   back_area_corr   pu_corr    psf_corr  somejunk  e1_cts  e2_cts  e3_cts  somejunk somejunk somejunk tot_back_cts  hard_ratio_1  hard_ratio_2  hard_ratio_err1  hard_ratio_err2 mean_time sigma_time'
  sp='          '
  for i=0,n_elements(time)-1 do begin 
     printf,lun,ntostr(time[s[i]])+sp+ntostr(tstart[s[i]])+sp+ntostr(tstop[s[i]])+sp+ntostr(cxo_counts[s[i]])+sp+ntostr(cxo_err[s[i]])+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(-999.)+sp+ntostr(1.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)+sp+ntostr(0.)
     
  endfor 
  
  close,lun
  free_lun,lun
  
  return
end 
