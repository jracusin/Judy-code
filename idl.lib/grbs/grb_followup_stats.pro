pro grb_followup_stats
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  
  tstop=dblarr(ndir) & ctr=tstop
  for i=0,ndir-1 do begin
     cd,dir[i]
     print,dir[i]
     if exist('lc_newout_xrt.txt') then lcfile='lc_newout_xrt.txt' else lcfile='lc_newout.txt'
     if exist(lcfile) then begin
        
        lc=lcout2fits(lcfile)
        tstop[i]=max(lc.tstop)
        w=where(lc.src_rate_err gt 0,nw)
        if nw gt 0 then ctr[i]=lc[w[nw-1]].src_rate
        
     endif 
     cd,'..'
  endfor 
  w=where(tstop gt 0 and ctr gt 0)
  tstop=tstop[w]
  dir=dir[w]
  ctr=ctr[w]
  
  b1=tstop[0:30]  ;;;Jan-June 2005
  b2=tstop[31:82]  ;;;July-Dec 2005
  b3=tstop[83:133]  ;;;Jan-June 2006
  b4=tstop[134:182]  ;;;July-Dec 2006
  b5=tstop[183:*]    ;;;Jan-June 2007
  
  c1=ctr[0:30]   ;;;Jan-June 2005
  c2=ctr[31:82]  ;;;July-Dec 2005
  c3=ctr[83:133]  ;;;Jan-June 2006
  c4=ctr[134:182]  ;;;July-Dec 2006
  c5=ctr[183:*]    ;;;Jan-June 2007
  
  begplot,name=!mdata+'grb_followup_times.ps',/land
  erase
  multiplot,[1,5],/init
  multiplot
  plothist,alog10(b1),bin=0.1,xrange=[3,8],yrange=[0,10]
  legend,['Jan-June 2005'],/top,/right,box=0
    multiplot
  plothist,alog10(b2),bin=0.1,xrange=[3,8],yrange=[0,10]
  legend,['July-Dec 2005'],/top,/right,box=0
    multiplot
  plothist,alog10(b3),bin=0.1,xrange=[3,8],yrange=[0,10],ytitle='N'
  legend,['Jan-June 2006'],/top,/right,box=0
    multiplot
  plothist,alog10(b4),bin=0.1,xrange=[3,8],yrange=[0,10]
  legend,['July-Dec 2006'],/top,/right,box=0
  multiplot
  plothist,alog10(b5),bin=0.1,xtitle='log Tstop (s)',xrange=[3,8],yrange=[0,10]
  legend,['Jan-June 2007'],/top,/right,box=0
  multiplot,/reset
  endplot
  
  begplot,name=!mdata+'grb_followup_ctrate.ps',/land
  erase
  multiplot,[1,5],/init 
  multiplot
  plothist,alog10(c1),bin=0.1,xrange=[-4,0],yrange=[0,8]
  legend,['Jan-June 2005'],/top,/right,box=0
    multiplot
  plothist,alog10(c2),bin=0.1,xrange=[-4,0],yrange=[0,8]
  legend,['July-Dec 2005'],/top,/right,box=0
    multiplot
  plothist,alog10(c3),bin=0.1,xrange=[-4,0],yrange=[0,8],ytitle='N'
  legend,['Jan-June 2006'],/top,/right,box=0
    multiplot
  plothist,alog10(c4),bin=0.1,xrange=[-4,0],yrange=[0,8]
  legend,['July-Dec 2006'],/top,/right,box=0
  multiplot
  plothist,alog10(c5),bin=0.1,xtitle='log Count Rate (0.3-10.0 keV) (s!U-1!N)',xrange=[-4,0],yrange=[0,8]
  legend,['Jan-June 2007'],/top,/right,box=0
  multiplot,/reset
  endplot
  
  
  stop
  
  return
end 
