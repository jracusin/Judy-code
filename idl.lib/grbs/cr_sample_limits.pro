pro cr_sample_limits
  
  cd,!mdata
  
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  
  cts_stop=fltarr(ndir) & cts_break=fltarr(ndir) & cts_lastdet=fltarr(ndir)
  tstop=fltarr(ndir) & tbreak=fltarr(ndir) & tlastdet=fltarr(ndir) & tstart=fltarr(ndir)
  times=['stop','break','lastdet']
  for i=0,ndir-1 do begin ;;loop over grbs
     print,dir[i]
     cd,dir[i]
     lcfitfile='lc_fit_out_idl_int2.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pname,p,perror,chisq,dof,breaks;,lc=lc
        lc=lcout2fits()
        tstop[i]=max(lc.tstop)
        tstart[i]=min(lc.tstart)
        if breaks gt 0 then tbreak[i]=p[(breaks-1)*2+2] else tbreak[i]=0.
        wdet=where(lc.src_rate_err gt 0.,nwdet)
        tlastdet[i]=lc[wdet[nwdet-1]].time
        case breaks of 
           0: model='pow'
           1: model='bknpow'
           2: model='bkn2pow'
           3: model='bkn3pow'
        endcase 
        
        ;;;loop over times of interest
        for j=0,n_elements(times)-1 do $
           tmp=execute('cts_'+times[j]+'[i]='+model+'(t'+times[j]+'[i],p)')
     endif 
     cd,'..'

  endfor 
  
  !p.multi=[0,2,2]
  plot,tstop,cts_stop,psym=1,/xlog,xrange=[100,1e8],/ylog,yrange=[1e-5,10],xtitle='t!L'+times[0],ytitle='Count Rate (t!L'+times[0]+'!N)'
  plot,tbreak,cts_break,psym=1,/xlog,xrange=[100,1e8],/ylog,yrange=[1e-5,10],xtitle='t!L'+times[1],ytitle='Count Rate (t!L'+times[1]+'!N)'
  plot,tlastdet,cts_lastdet,psym=1,/xlog,xrange=[100,1e8],/ylog,yrange=[1e-5,10],xtitle='t!L'+times[2],ytitle='Count Rate (t!L'+times[2]+'!N)'
  !p.multi=0
  
  ;;WANT TO KNOW COUNT RATE & FLUX AT SPECIFIC TIMES (TSTOP,LAST TBREAK, LAST DET?)
  stop
  
  return
end 
