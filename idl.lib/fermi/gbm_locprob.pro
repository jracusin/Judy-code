pro read_gbm,file,probmap,ra,dec,center=center

  probmap=mrdfits(file,1,hdr)
  cra=sxpar(hdr,'CRVAL1')
  cdec=sxpar(hdr,'CRVAL2')
  if n_elements(center) gt 0 then begin 
     ra=center[0]
     dec=center[1]
  endif 
  pix=sxpar(hdr,'CDELT1')
  xmap=fltarr(512,512)
  ymap=fltarr(512,512)
  xmap[0,0]=cra-pix*512/2.
  ymap[0,0]=cdec-pix*512/2.
  for i=0,511 do begin
     xmap[i,*]=xmap[0,0]+pix*i
     ymap[*,i]=ymap[0,0]+pix*i
  endfor 

  return
end 
pro opt_gbm

  ;;; how many sets of 19 Swift tiles would cover GBM 1,2,3 sigma?
  ;; r=1.41524 deg for 19 tiles (green inner circle)
  rad=1.41524
  ntiles=2000
  dir='~/Fermi/gbmtrig/'
  cd,dir
  locprob=file_search('glg_locprob_all*fit')

  grb=strmid(locprob,18,9)
  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  match,strtrim(gbm.name,2),'GRB'+strtrim(grb,2),m1,m2
  grb=grb[m2]
  locprob=locprob[m2]
  xrtrad=23.6/2./60.

  results=create_struct('locprob','','ntiles68',0,'ntiles95',0,'ntiles99',0.,'p1000',0.,'p2000',0.)
  results=replicate(results,n_elements(locprob))

  pnum=intarr(n_elements(locprob))
  for m=0,n_elements(locprob)-1 do begin 
     !p.multi=[0,1,2]

     probmap=mrdfits(locprob[m],1,hdr)
     
;  cra=sxpar(hdr,'CRVAL1')
;  cdec=sxpar(hdr,'CRVAL2')
     cra=0.
     cdec=0.
     pix=sxpar(hdr,'CDELT1')
     xmap=fltarr(512,512)
     ymap=fltarr(512,512)
     xmap[0,0]=cra-pix*512/2.
     ymap[0,0]=cdec-pix*512/2.
     for i=0,511 do begin
        xmap[i,*]=xmap[0,0]+pix*i
        ymap[*,i]=ymap[0,0]+pix*i
     endfor 

     amap=probmap
     pmap=probmap
     prob=fltarr(ntiles)
     
     begplot,name='~/Swift/Tiling/gbm/'+grb[m]+'_2000tiles.ps',/color,font='helvetica'
     map_set,/aitoff,/grid,limit=[cra-20.,cdec-20,cra+20,cdec+20]
     contour,probmap,xmap,ymap,levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)],/iso,/overplot

;     g=20
;     pgrid1=fltarr(g+1,g+1)
;     xx=fltarr(g+1,g+1) & yy=fltarr(g+1,g+1)
;     for i=0,g do begin
;        xx[i,*]=(i-g/2.)/2.
;        yy[*,i]=(i-g/2.)/2.
;     endfor 
;     radone=0.
;     decdone=0.

     nneigh=7
     for j=0,ntiles-1 do begin 
        if j eq 0 then begin 
           maxpix=max(amap,mmax)
           cra=xmap[mmax]
           cdec=ymap[mmax]
           radone=cra
           decdone=cdec
        endif        

        if j ge 1 then begin 
           if j eq 1 then begin 
              xrt_tiling,nneigh,xx,yy,center=[radone,decdone],/noplot
              dxx=xx
              dyy=yy
              plist=fltarr(n_elements(xx))
              wnear=where(xmap gt cra-1 and xmap lt cra+1 and ymap gt cdec-1 and ymap lt cdec+1)
              for i=0,n_elements(xx)-1 do begin 
;                 skycircle,xx[i],yy[i],xrtrad,x,y,n=30
;                 w=where_inside(x,y,xmap[wnear],ymap[wnear])
                 dist=separation(xmap[wnear],ymap[wnear],cra,cdec)/3600.
                 w=where(dist le xrtrad)
                 w=wnear[w]
                 plist[i]=total(amap[w])/total(probmap)
;                 oplot,x,y,color=!green
              endfor 
              dplist=plist

           endif else begin 
              rd=ntostr(dxx,5)+'_'+ntostr(dyy,5)
              rd0=rem_dup(rd)
              dxx=dxx[rd0]
              dyy=dyy[rd0]
              dplist=dplist[rd0]

              match=match_2d(dxx,dyy,radone,decdone,3./60.)
              wm=where(match eq -1,nwm)
              xx=dxx[wm]
              yy=dyy[wm]
              plist=dplist[wm]
              for i=0,nwm-1 do begin
                 skycircle,xx[i],yy[i],xrtrad,x,y,n=30
;                 oplot,x,y,color=!green
              endfor 
           endelse 
           
;;            wnear=where(xmap gt cra-3 and xmap lt cra+3 and ymap gt cdec-3 and ymap lt cdec+3)
;;            plist=fltarr(n_elements(xx))

;;            for i=0,n_elements(xx)-1 do begin 
;;               dist=separation(xmap[wnear],ymap[wnear],xx[i],yy[i])/3600.
;;               w=where(dist le xrtrad)

;;               skycircle,xx[i],yy[i],xrtrad,x,y,n=30
;;               w0=where_inside(x,y,xmap[wnear],ymap[wnear])
;;               w0=wnear[w0]
;;               w=wnear[w]
;;               plist[i]=total(amap[w])/total(probmap)
;; stop
;;            endfor 

           ;; skip to here
           sp=sort(plist)
           maxgrid=max(plist,mplist)
           cra=xx[mplist]
           cdec=yy[mplist]
           radone=[radone,cra]
           decdone=[decdone,cdec]
           xrt_tiling,nneigh,xx,yy,center=[cra,cdec],/noplot
           wnear=where(xmap gt cra-1 and xmap lt cra+1 and ymap gt cdec-1 and ymap lt cdec+1)
           pl=fltarr(n_elements(xx))
           for i=0,n_elements(xx)-1 do begin 
;              skycircle,xx[i],yy[i],xrtrad,x,y,n=30
;              w=where_inside(x,y,xmap[wnear],ymap[wnear])
              dist=separation(xmap[wnear],ymap[wnear],cra,cdec)/3600.
              w=where(dist le xrtrad)
              w=wnear[w]
              pl[i]=total(amap[w])/total(probmap)
;              oplot,x,y,color=!green
           endfor 
           dxx=[dxx,xx]
           dyy=[dyy,yy]
           dplist=[dplist,pl]
        endif 
        wnear=where(xmap gt cra-1 and xmap lt cra+1 and ymap gt cdec-1 and ymap lt cdec+1)
        dist=separation(xmap[wnear],ymap[wnear],cra,cdec)/3600.
        w0=where(dist le xrtrad*0.8)
        w=where(dist le xrtrad)
        w0=wnear[w0]
        w=wnear[w]

        ;; for i=0,g,1 do begin ;;; jiggle grid by 1 deg to find new max
        ;;    for o=0,g,1 do begin
        ;;       wnear=where(xmap gt cra-5 and xmap lt cra+5 and ymap gt cdec-5 and ymap lt cdec+5)
        ;;       dist=separation(xmap[wnear],ymap[wnear],cra+xx[i,o],cdec+yy[i,o])/3600.
        ;;       w=where(dist le rad)
        ;;       w=wnear[w]
        ;;       pgrid1[i,o]=total(amap[w])/total(probmap)
        ;;    endfor 
        ;; endfor 
;        maxgrid=max(pgrid1,mgrid1)
;        cra=xmap[mmax]+xx[mgrid1]
;        cdec=ymap[mmax]+yy[mgrid1]
;        dist=separation(xmap,ymap,cra,cdec)/3600.
;        w=where(dist le rad)

        pmap[w]=0
        prob[j]=1.-total(pmap)/total(probmap)

;        prob[j]=total(amap[w])/total(amap)

        amap[w0]=0

        skycircle,cra,cdec,xrtrad,x,y,n=30
        oplot,x,y,color=!red,psym=3
        c=cumulative(prob)
        wc=where(c gt 0.68)
;stop
;        if wc[0] ne -1 then j=ntiles-1
;        k=get_kbrd(10)
;        if k eq 's' then stop

     endfor 
;stop
     plot,prob,ytitle='Cumulative Enclosed Probability',xtitle='N tiles'
;     c=cumulative(prob)
     results[m].locprob=locprob[m]
     g=where(prob gt 0.68)
     results[m].ntiles68=g[0]
     g=where(prob gt 0.95)
     results[m].ntiles95=g[0]
     g=where(prob gt 0.99)
     results[m].ntiles99=g[0]
     results[m].p1000=prob[999]
     results[m].p2000=prob[1999]

;     print,prob[g[0]],g[0]
;     print,prob[999]
     !p.multi=0

;     print,c[g[0]],g[0]
;     print,c[999]
     pnum[m]=g[0]
;     k=get_kbrd(10)
;     if k eq 's' then stop
     endplot
     spawn,'ps2pdf ~/Swift/Tiling/gbm/'+grb[m]+'_2000tiles.ps
;     ~/Swift/Tiling/gbm/'+grb[m]+'_19tiles.pdf'
     mwrfits,results,'~/Swift/Tiling/gbm/tiles_probs.fits',/create
  endfor 



;2  begplot,name='~/Swift/Tiling/gbm_19tiles_68p.ps',/land,/color,font='helvetica'
;  plothist,pnum,xtitle='# of 19 tile patterns to cover 68% GBM loc prob',ytitle='N',/fill
;  endplot
;  spawn,'ps2pdf ~/Swift/Tiling/gbm_19tiles_68p.ps ~/Swift/Tiling/gbm_19tiles_68p.pdf'
;  stop
  return
end 

pro locprob,ptot,noplot=noplot

  cd,'~/Fermi/gbmtrig/'
  locprob=file_search('~/Fermi/gbmtrig/glg_locprob_all*fit')
  grb=strmid(locprob,48,9)

  gbm=mrdfits('~/Fermi/GBM_GRB_Catalog.fits',1)
  match,strtrim(gbm.name,2),'GRB'+strtrim(grb,2),m1,m2

  grb=grb[m2]
  locprob=locprob[m2]
  n=n_elements(locprob)

  exposure=2880.
  ntiles=[1,4,7,19,37,61,91]
  xrtrad=23.6/60.
  if n_elements(ptot) eq 0 then begin 
     ptot=fltarr(n,n_elements(ntiles))

     for j=0,n-1 do begin 
        probmap=mrdfits(locprob[j],1,hdr)
        cra=sxpar(hdr,'CRVAL1')
        cdec=sxpar(hdr,'CRVAL2')
        pix=sxpar(hdr,'CDELT1')
        xmap=fltarr(512,512)
        ymap=fltarr(512,512)
        expmap1=fltarr(512,512)
        expmap4=fltarr(512,512)
        expmap7=fltarr(512,512)
        expmap19=fltarr(512,512)
        expmap37=fltarr(512,512)
        expmap61=fltarr(512,512)
        expmap91=fltarr(512,512)
        xmap[0,0]=cra-pix*512/2.
        ymap[0,0]=cdec-pix*512/2.
        for i=0,511 do begin
           xmap[i,*]=xmap[0,0]+pix*i
           ymap[*,i]=ymap[0,0]+pix*i
        endfor 
;     r=random_pdist(probmap,1000L,xrand=xrand,yrand=yrand,xarr=xarr,yarr=yarr);,arr=arr
        center=[cra,cdec]
        
        if not keyword_set(noplot) then begplot,name='~/Swift/Tiling/gbm/'+grb[j]+'_gbm_xrt_zoom.ps',/land,/color,font='helvetica'
        xrt_tiling,ntiles[6],x,y,rad,center=center,noplot=noplot
        for i=0,ntiles[6]-1 do begin 
           dist=separation(xmap,ymap,x[i],y[i])/3600.
           w=where(dist le xrtrad)
           if i lt ntiles[0] then expmap1[w]=expmap1[w]+1.
           if i lt ntiles[1] then expmap4[w]=expmap4[w]+1.
           if i lt ntiles[2] then expmap7[w]=expmap7[w]+1.
           if i lt ntiles[3] then expmap19[w]=expmap19[w]+1.
           if i lt ntiles[4] then expmap37[w]=expmap37[w]+1.
           if i lt ntiles[5] then expmap61[w]=expmap61[w]+1.
           if i lt ntiles[6] then expmap91[w]=expmap91[w]+1.
        endfor 
        if not keyword_set(noplot) then contour,probmap,xmap,ymap,/iso,/overplot,levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)]
        w=where(expmap1 ge 1.)
        ptot[j,0]=total(probmap[w])/total(probmap)
        w=where(expmap4 ge 1.)
        ptot[j,1]=total(probmap[w])/total(probmap)
        w=where(expmap7 ge 1.)
        ptot[j,2]=total(probmap[w])/total(probmap)
        w=where(expmap19 ge 1.)
        ptot[j,3]=total(probmap[w])/total(probmap)
        w=where(expmap37 ge 1.)
        ptot[j,4]=total(probmap[w])/total(probmap)
        w=where(expmap61 ge 1.)
        ptot[j,5]=total(probmap[w])/total(probmap)
        w=where(expmap91 ge 1.)
        ptot[j,6]=total(probmap[w])/total(probmap)

        if not keyword_set(noplot) then begin
           legend,ntostr(ntiles)+' tiles: p='+numdec(ptot[j,*],3),/top,/left,box=0
           endplot
           spawn,'ps2pdf ~/Swift/Tiling/gbm/'+grb[j]+'_gbm_xrt_zoom.ps ~/Swift/Tiling/gbm/'+grb[j]+'_gbm_xrt_zoom.pdf'
        endif 

;     xp=xmap[xrand,yrand]
;     yp=ymap[xrand,yrand]
        
;     oplot,xp,yp,psym=1,symsize=0.5,color=!magenta

;     k=get_kbrd(10)
;     if k eq 's' then stop
     endfor 
  endif 

;  color=[!blue,!red,!green,!magenta,!cyan,!orange,!yellow]
  ymax=[150,120,80,60,30,25,20]
  begplot,name='~/Swift/Tiling/GBM_enclosed_prob.ps',/color,font='helvetica'
  multiplot,[1,7],/init
  for t=0,6 do begin
     if t eq 6 then xtitle='Enclosed GBM Probability in # XRT Tiles' else xtitle=''
     if t eq 0 then title='GBM' else title=''
     multiplot
     plot,[0,0.5],[0,20],/nodata,xtitle=xtitle,ytitle='N',xrange=[0,1],yrange=[0,ymax[t]],/xsty,/ysty,charsize=1.5,title=title
     plothist,ptot[*,t],bin=0.02,x,y,fcolor=!blue,/fill,/overplot
     legend,ntostr(ntiles[t])+' Tiles',box=0,/top,/right,charsize=1.5
  endfor 
  multiplot,/reset,/default
;  legend,ntostr(ntiles)+' Tiles',textcolor=color,box=0,/top,/right
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/GBM_enclosed_prob.ps ~/Swift/Tiling/GBM_enclosed_prob.pdf'
  stop

  return
end 

pro get_gbm_locprob

  cd,'~/Fermi/gbmtrig/'
  spawn,'wget -O gbmtrig.txt "http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/2015/"'

  readcol,'gbmtrig.txt',lines,format='(a)',delim='$'
  n=n_elements(lines)
  g=8 ;;; lines that bursts start on
stop
  for i=g,n-1 do begin
     s=strsplit(lines[i],'>/',/ex)
     trig=s[5]
     com='wget "http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/2015/'+trig+'/current/glg_locprob_all_'+trig+'_v01.fit"'
     print,com
     spawn,com
     if not exist(trig+'/current/glg_locprob_all_'+trig+'_v01.fit') then begin 
        com='wget "http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/2015/'+trig+'/current/glg_locprob_all_'+trig+'_v02.fit"'
        print,com
        spawn,com
     endif 
     if not exist(trig+'/current/glg_locprob_all_'+trig+'_v02.fit') then begin 
        com='wget "http://heasarc.gsfc.nasa.gov/FTP/fermi/data/gbm/triggers/2015/'+trig+'/current/glg_locprob_all_'+trig+'_v03.fit"'
        print,com
        spawn,com
     endif 
  endfor 

stop
end
