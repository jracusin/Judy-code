pro search_tte,trigtime

  ;;; download ctte files
  ;;; plot them around trig time
  ;;; overplot line for trig time

  cd,'~/Fermi/GBM_Data/CTTE'

  files=file_search('*tte*')

  nfiles=n_elements(files)
  xrange=[-10,10.]
  bin=1.

  !p.multi=[0,2,7]
  for i=0,nfiles-1 do begin
     g=mrdfits(files[i],2,hdr)
     tstart=sxpar(hdr,'TSTART')
     g.time=g.time+tstart-trigtime
     w=where(g.time ge xrange[0] and g.time le xrange[1])

     plothist,g[w].time,x,y,/noplot,bin=bin
     plot,x,y,psym=10,xrange=xrange,title=files[i],xtitle='T-T!L0!N',yrange=median(y)+[-200,200]*bin*2.,charsize=1.5
     oplot,[0,0],[-2000,2000],color=!red,line=2
     if i gt 1 then sum=y
     if i gt 2 then sum=sum+y
     
  endfor 

  !p.multi=0

stop
end 

