pro select_uvot_lcs

  dir='/Volumes/Apps_and_Docs/jracusin/Fermi/Swift_pop_study/new/FINAL_LC'
  cd,dir
  gdir=['BAT','GBM','LAT']
  colors=[!p.color,!grey50,!grey20]
  lcolors=[!green,!orange,!cyan,!magenta,!purple] ;,!yellow]
  plot,[10,1e6],[1e-4,1e3],/nodata,/xlog,/ylog,xtitle='Time (s)',ytitle='Count Rate'
  plotsym,0,0.8,/fill
  
  readcol,'~/Fermi/Swift_pop_study/sample_stats.csv',dgrb,whose,dz,dur,t90,format='(a,a,f,a,a)',/silent


  tstop=dblarr(69)
  tstart=tstop
  ndet=intarr(69)
  grb=strarr(69)
  ngrb=[58,11,5]
  bat=indgen(ngrb[0])
  gbm=indgen(ngrb[1])+ngrb[0]
  lat=indgen(ngrb[2])+ngrb[0]+ngrb[1]
  j=0
  for k=0,2 do begin
     cd,gdir[k]
     gfiles=file_search('GRB*txt')
     ngrbs=n_elements(gfiles)
     for i=0,ngrbs-1 do begin
        readcol,gfiles[i],time,terr,rate,raterr,/silent
        w=where(rate gt 0 and rate/raterr gt 1,nw)
        ndet[j]=nw
;        print,gfiles[i],nw,min(time[w]),max(time[w])
        if k le 1 then color=colors[k] else color=lcolors[i]
        tstop[j]=max(time[w]+terr[w])
        tstart[j]=min(time[w]-terr[w])
        spos=strpos(gfiles[i],'.txt')
        grb[j]=strmid(gfiles[i],0,spos)
        wz=where(grb[j] eq dgrb)
        z=dz[wz[0]]
;        if tstop[j] lt 1e5 then color=!yellow
        oploterror,time[w]/(1.+z),rate[w],terr[w]/(1.+z),raterr[w],/nohat,psym=8,errcolor=color,color=color

        j++
     endfor 
     cd,'..'
  endfor 
stop
 plothist,ndet,bin=2
 plothist,ndet[bat],bin=2,/over,color=!red
 plothist,ndet[gbm],bin=2,/over,color=!grey20
 plothist,ndet[lat],bin=2,/over,color=!cyan 

 readcol,'BAT/Peak_U_cr.txt',peak,ptime,pgrbs,format='(f,f,a)'
 pgrb=pgrbs
 for i=0,n_elements(peak)-1 do begin
    spos=strpos(pgrbs[i],'.tx')
    pgrb[i]=strmid(pgrbs[i],0,spos)
 endfor 
; w=where(tstop lt 86400.)
 match,pgrb,grb,m1,m2

; colprint,pgrb[m1],peak[m1]


stop
return
end 
