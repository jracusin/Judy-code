pro plot_probs

  gbm=mrdfits('~/Swift/Tiling/gbm/tiles_probs.fits',1)
  ligo=mrdfits('~/Swift/Tiling/ligo/tiles_probs3det.fits',1)
  w=where(ligo.p1000 gt 0); and ligo.p1000 lt 1. and ligo.p2000 lt 1.)
  ligo=ligo[w]
  help,gbm,ligo

  begplot,name='~/Swift/Tiling/gbm/prob_ntiles.ps',font='helvetica'
  multiplot,[1,2],/init
  multiplot
  plothist,gbm.p1000,bin=0.05,ytitle='N',xrange=[0,1],/fill
  legend,'1000 Tiles',box=0,/top,/left
  multiplot
  plothist,gbm.p2000,bin=0.05,ytitle='N',xtitle='Enclosed Probability in N tiles',xrange=[0,1],/fill
  legend,'2000 Tiles',box=0,/top,/left
  multiplot,/reset,/default
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/gbm/prob_ntiles.ps ~/Swift/Tiling/gbm/prob_ntiles.pdf'

  begplot,name='~/Swift/Tiling/ligo/prob_ntiles.ps',font='helvetica'
  multiplot,[1,2],/init
  multiplot
  plothist,ligo.p1000-0.04,bin=0.05,ytitle='N',xrange=[0,1],/fill,yrange=[0,12]
  legend,'1000 Tiles',box=0,/top,/left
  multiplot
  plothist,ligo.p2000-0.04,bin=0.05,ytitle='N',xtitle='Enclosed Probability in N tiles',xrange=[0,1],/fill,yrange=[0,15]
  legend,'2000 Tiles',box=0,/top,/left
  multiplot,/reset,/default
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/ligo/prob_ntiles.ps ~/Swift/Tiling/ligo/prob_ntiles.pdf'


stop
  return
end 

pro optimize_tiling,files,outname,outdir,pngfiles=pngfiles,ntiles=ntiles,noplot=noplot

;  if n_elements(ntiles) eq 0 then 
  ntiles=2000.
  xrtrad=23.6/2./60.
  results=create_struct('file','','ntiles68',0,'ntiles95',0,'ntiles99',0.,'p1000',0.,'p2000',0.)
  results=replicate(results,n_elements(files))

  for u=0,n_elements(files)-1 do begin 
     read_fits_map,outdir+files[u],hmap,nside=nside
     if n_elements(pngfiles) eq 0 then mollview,files[u],/silent,/nobar,titleplot=' ',colt=20,coord='C',png=outdir+outname+'.png'
     
     pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
     xmap=360.-phi*!radeg
     ymap=(0.5*!pi-theta)*!radeg

     probmap=hmap

     amap=probmap
     pmap=probmap
     prob=fltarr(ntiles)

     maxpix=max(amap,mmax)
     cra=xmap[mmax]
     cdec=ymap[mmax]

     if not keyword_set(noplot) then begin 
        im=image(outdir+pngfiles[u],margin=0,image_dimensions=[800,510])
        mp=map('Mollweide',label_show=0,grid_longitude=30,grid_latitude=30,linestyle=1,thick=2,margin=[0.008,0,0.008,0],/current) ;,xrange=[cra-60,cra+60])
     endif 
     radone=0.
     decdone=0.

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

           if max(plist) eq 0 then j=ntiles-1
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


        pmap[w]=0
        prob[j]=1.-total(pmap)/total(probmap)

        amap[w0]=0

        skycircle,cra,cdec,xrtrad,x,y,n=30
;           oplot,x,y,color=!red
        if not keyword_set(noplot) then begin 
           p2=plot(x,y,color='red',/current,/overplot)
        endif 
        wc=where(prob gt 0.999)
        if wc[0] ne -1 then j=ntiles-1        

     endfor 
     
     if not keyword_set(noplot) then begin 
        im.save,outdir+outname[u]+'_xrt_2000tiles.png'
        im.close
     endif 

     begplot,name=outdir+outname[u]+'_xrt_2000tiles_prob.ps',/land
     plot,prob,ytitle='Cumulative Enclosed Probability',xtitle='N tiles'
     results[u].file=files[u]
     g=where(prob gt 0.68)
     results[u].ntiles68=g[0]
     g=where(prob gt 0.95)
     results[u].ntiles95=g[0]
     g=where(prob gt 0.99)
     results[u].ntiles99=g[0]
     results[u].p1000=prob[999]
     results[u].p2000=prob[1999]
     endplot
     spawn,'ps2pdf '+outdir+outname[u]+'_xrt_2000tiles_prob.ps '+outdir+outname[u]+'_xrt_2000tiles_prob.pdf'

     mwrfits,results,outdir+'tiles_probs'+outname+'.fits',/create

  endfor
 
  return
end 


pro ligo_gbm
  outdir='~/Swift/Tiling/'
  ldir='~/iLobster/simulations/LIGO_sims/2016_fits/'
  ligofiles=file_search(ldir+'*/bayestar*fits')
  nligo=n_elements(ligofiles)

;  ligofile='~/iLobster/simulations/LIGO_sims/2016_fits/303684/bayestar.fits'
;  detfile='~/iLobster/simulations/LIGO_sims/2016_fits/303684_lin.png'
  
  dir='~/Fermi/gbmtrig/'
  gbmfiles=file_search(dir+'glg_locprob_all*fit')
  ngbm=n_elements(gbmfiles)

  outdir='~/Swift/Tiling/ligo_gbm/sim_same_center/'

  nsim=1000
;  done=file_search(outdir+'tiles_probsligo_gbm_*.fits')
  done=file_search(outdir+'ligo_gbm_healpix_map_*.fits')
  if done[0] ne '' then begin 
     dd=intarr(n_elements(done))
     for i=0,n_elements(done)-1 do begin
        d=strsplit(done[i],'_.',/ex)
        dd[i]=d[7]
     endfor
  endif else dd=0
  md=max(dd)
;  nest=0 & ring=0
  for s=md+1,nsim-1 do begin
     time=systime(1)
     l=randomu(seed,1.)*nligo
     g=randomu(seed,1.)*ngbm
     ligofile=ligofiles[l]
     gbmfile=gbmfiles[g]

     read_fits_map,ligofile,hmap,nside=nside
     print,nside
     pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
     lra=360.-phi*!radeg
     ldec=(0.5*!pi-theta)*!radeg
;     nest=1
;     if max(ldec) gt 1000 then begin 
;        pix2ang_ring,nside,lindgen(n_elements(hmap)),theta,phi
;        lra=360.-phi*!radeg
;        ldec=(0.5*!pi-theta)*!radeg
;        ring=1
;        nest=0
;     endif 
;     print,nest,ring
     maxpix=max(hmap,mmax)
     cra=lra[mmax]
     cdec=ldec[mmax]

     ;;; GBM Map
     probmap=mrdfits(gbmfile,1,hdr)
     pix=sxpar(hdr,'CDELT1')
     smap=size(probmap)
     xmap=fltarr(smap[1],smap[2])
     ymap=fltarr(smap[1],smap[2])
     xmap[0,0]=cra-pix*smap[1]/2.
     ymap[0,0]=cdec-pix*smap[2]/2.
     for i=0,smap[1]-1 do xmap[i,*]=xmap[0,0]+pix*i
     for i=0,smap[2]-1 do ymap[*,i]=ymap[0,0]+pix*i
     gra=xmap
     gdec=ymap
     phi=(360.-gra)/!radeg
     theta=-(gdec/!radeg-0.5*!pi)
     ang2pix_nest, nside, theta, phi, ipnest 
     gmap=hmap
     gmap[*]=0.

     gmap[ipnest]=probmap
     gmap=gmap/total(gmap)

     map=gmap*hmap
     map=map/total(map)

     mollview,ligofile,coord='C',colt=20,png=outdir+'ligo_healpix_map_'+ntostr(s)+'.png',window=-1

     write_fits_map,outdir+'gbm_healpix_map_'+ntostr(s)+'.fits',gmap,coordsys='C',ordering='nest'
     mollview,outdir+'gbm_healpix_map_'+ntostr(s)+'.fits',coord='C',colt=20,png=outdir+'gbm_healpix_map_'+ntostr(s)+'.png',window=-1

     write_fits_map,outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.fits',map,coordsys='C',ordering='nest'
     mollview,outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.fits',coord='C',colt=20,png=outdir+'ligo_gbm_healpix_map_'+ntostr(s)+'.png',window=-1

;     optimize_tiling,'ligo_gbm_healpix_map_'+ntostr(s)+'.fits','ligo_gbm_'+ntostr(s),outdir,pngfile='ligo_gbm_healpix_map_'+ntostr(s)+'.png',/noplot
     ptime,systime(1)-time
stop
  endfor 

  stop
  return
end 

pro opt_ligo
  ;;; code not finished or working

  ;;; how many sets of 19 Swift tiles would cover LIGO 1,2,3 sigma?
  ;; r=1.41524 deg for 19 tiles (green inner circle)

  simpctable
  ntiles=2000
  xrtrad=23.6/2./60.

  cd,'~/iLobster/simulations/LIGO_sims/2016_fits/'
  files=file_search('*/bayestar*fits')
  dir=''
  readcol,'2016_coinc.csv',eid,sid,network,snr,snrh,snrl,snrv,mass1,mass2,a50,a90,searched,la50,la90,lasearched,format='(l,l,a,f,f,f,f,f,f,f,f,f,f,f)',delim=',',/preserve

  w2=where((strtrim(network,2) eq 'HL' or strtrim(network,2) eq 'HV' or strtrim(network,2) eq 'LV')) ; and snr ge 14 and snr le 15) ; and a50 gt 500 and a50 lt 1000)
  w3=where(strtrim(network,2) eq 'HLV') ; and snr ge 14 and snr le 15); and a50 lt 500 and a90 lt 500)

  det2=eid[w2]
  det3=eid[w3]
  det2file=ntostr(det2)+'_lin.png'
  det3file=ntostr(det3)+'_lin.png'

  for d=0,1 do begin 
     case d of 
        0: begin
           ndet='3'
           det=det3
           detfile=det3file
        end
        1: begin
           ndet='2'
           det=det2
           detfile=det2file
        end 
     endcase 

     results=create_struct('det','','ntiles68',0,'ntiles95',0,'ntiles99',0.,'p1000',0.,'p2000',0.)
     results=replicate(results,n_elements(det))

     for u=1,n_elements(det)-1 do begin 
        w1=where(eid eq det[u])  
        w=where(files eq ntostr(eid[w1[0]])+'/bayestar.fits')
        print,files[w]
        read_fits_map,files[w],hmap,nside=nside
;        healpix_to_image,hmap,map2,scale=1,transfer=1,coord=5,proj=1,size=2
;        wm2=where(map2 le 0)
;        map2[wm2]=0
        pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
        xmap=360.-phi*!radeg
        ymap=(0.5*!pi-theta)*!radeg

        pnum=intarr(n_elements(det))
        probmap=hmap
;        probmap=mrdfits(detfile[j],1,hdr)

        amap=probmap
        pmap=probmap
        prob=fltarr(ntiles)

        maxpix=max(amap,mmax)
        cra=xmap[mmax]
        cdec=ymap[mmax]

        im=image(detfile[u],margin=0,image_dimensions=[800,510])
        mp=map('Mollweide',label_show=0,grid_longitude=30,grid_latitude=30,title=ndet+' GW Detector Localization',linestyle=1,thick=2,margin=[0.008,0,0.008,0],/current);,xrange=[cra-60,cra+60])
;        print,cra[0]-30.,cdec[0]-30,cra[0]+30,cdec[0]+30
;        !p.multi=[0,1,2]
;        rdis,map2

;        map_set,/aitoff,/grid;,limit=[cdec[0]-50.,cra[0]-50,cdec[0]+50,cra[0]+50]
;        rdis,map2
;        contour,map2,/overplot

        radone=0.
        decdone=0.

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


           pmap[w]=0
           prob[j]=1.-total(pmap)/total(probmap)

;        prob[j]=total(amap[w])/total(amap)

           amap[w0]=0

           skycircle,cra,cdec,xrtrad,x,y,n=30
;           oplot,x,y,color=!red
           p2=plot(x,y,color='red',/current,/overplot)
           c=cumulative(prob)
           wc=where(c gt 0.68)
;           if wc[0] ne -1 then j=ntiles-1
           if j mod 100 eq 0 then print,j
        endfor 

        im.save,'~/Swift/Tiling/ligo/'+ntostr(det[u])+'_ligo_xrt_'+ndet+'det_2000tiles.png'
        im.close

        begplot,name='~/Swift/Tiling/ligo/'+ntostr(det[u])+'_ligo_xrt_'+ndet+'det_2000tiles_prob.ps',/land,font='helvetica'
        plot,prob,ytitle='Cumulative Enclosed Probability',xtitle='N tiles',charsize=2
        results[u].det=det[u]
        g=where(prob gt 0.68)
        results[u].ntiles68=g[0]
        g=where(prob gt 0.95)
        results[u].ntiles95=g[0]
        g=where(prob gt 0.99)
        results[u].ntiles99=g[0]
        results[u].p1000=prob[999]
        results[u].p2000=prob[1999]
        endplot
        spawn,'ps2pdf ~/Swift/Tiling/ligo/'+ntostr(det[u])+'_ligo_xrt_'+ndet+'det_2000tiles_prob.ps ~/Swift/Tiling/ligo/'+ntostr(det[u])+'_ligo_xrt_'+ndet+'det_2000tiles_prob.pdf'

        pnum[u]=g[0]
;        k=get_kbrd(10)
;        if k eq 's' then stop
        mwrfits,results,'~/Swift/Tiling/ligo/tiles_probs'+ndet+'det.fits',/create

     endfor 
  endfor 

  stop
  return
end 

pro xrt_tile_ligo,ptot2,ptot3,noplot=noplot

  cd,'~/iLobster/simulations/LIGO_sims/2016_fits/'
  files=file_search('*/bayestar*fits')
  dir=''
  readcol,'2016_coinc.csv',eid,sid,network,snr,snrh,snrl,snrv,mass1,mass2,a50,a90,searched,la50,la90,lasearched,format='(l,l,a,f,f,f,f,f,f,f,f,f,f,f)',delim=',',/preserve

  w2=where((strtrim(network,2) eq 'HL' or strtrim(network,2) eq 'HV' or strtrim(network,2) eq 'LV')) ; and snr ge 14 and snr le 15) ; and a50 gt 500 and a50 lt 1000)
  w3=where(strtrim(network,2) eq 'HLV') ; and snr ge 14 and snr le 15); and a50 lt 500 and a90 lt 500)

  det2=eid[w2]
  det3=eid[w3]
  print,'2det'
  colprint,eid[w2],network[w2],snr[w2],a50[w2],a90[w2]
  print,'3det'
  colprint,eid[w3],network[w3],snr[w3],a50[w3],a90[w3]

  det2file=ntostr(det2)+'_lin.png'
  outdet2file=ntostr(det2)+'_ligo_xrt_2det.png'
  det3file=ntostr(det3)+'_lin.png'
  outdet3file=ntostr(det3)+'_ligo_xrt_3det.png'

  ntiles=[1,4,7,19,37,61,91]
  xrtrad=23.6/60.

  if n_elements(ptot2) eq 0 then begin 
     for d=0,1 do begin 
        case d of 
           0: begin
              ndet='3'
              det=det3
              detfile=det3file
              outdetfile=outdet3file
           end
           1: begin
              ptot3=ptot
              ndet='2'
              det=det2
              detfile=det2file
              outdetfile=outdet2file
           end 
        endcase 
        ptot=fltarr(n_elements(det),n_elements(ntiles))
        
        for j=0,n_elements(det)-1 do begin 
           print,outdetfile[j]
           w1=where(eid eq det[j])  
           w=where(files eq ntostr(eid[w1[0]])+'/bayestar.fits')
           if not exist(detfile[j]) then mollview,files[w],map_out=map1,/silent,/nobar,titleplot=' ',colt=20,coord='C',png=dir+det[j]+'.png'

           read_fits_map,files[w],hmap,nside=nside
           pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
           ra=360.-phi*!radeg
           dec=(0.5*!pi-theta)*!radeg
           expmap1=fltarr(nside*6.,nside*2.)
           expmap4=fltarr(nside*6.,nside*2.)
           expmap7=fltarr(nside*6.,nside*2.)
           expmap19=fltarr(nside*6.,nside*2.)
           expmap37=fltarr(nside*6.,nside*2.)
           expmap61=fltarr(nside*6.,nside*2.)
           expmap91=fltarr(nside*6.,nside*2.)

           if not keyword_set(noplot) then begin 
              im=image(detfile[j],margin=0,image_dimensions=[800,510])
              m=map('Mollweide',label_show=0,grid_longitude=30,grid_latitude=30,/current,title=ndet+' GW Detector Localization',linestyle=1,thick=2,margin=[0.008,0,0.008,0])
           endif 
           probmap=hmap

           maxpix=max(probmap,mmax)
           tilex=ra[mmax]
           tiley=dec[mmax]
           print,tilex,tiley

           center=[tilex,tiley]
           xrt_tiling,ntiles[6],x,y,rad,rad2,center=center,/noplot
;        skycircle,center[0],center[1],rad-g/2.,cx,cy
;        p=plot(cx,cy,color='green')
;        pmap=reform(probmap,nside*6.,nside*2.)
;        xx=reform(ra,nside*6.,nside*2.)
;        yy=reform(dec,nside*6.,nside*2.)
;        c=contour(pmap,xx,yy,/overplot,/current);levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)])
;        contour,pmap,xx,yy,levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)]
           for i=0,ntiles[6]-1 do begin 
              skycircle,x[i],y[i],xrtrad,xx,yy,n=360.
              if not keyword_set(noplot) then p=plot(xx,yy,/overplot,/current)
              near=where(ra gt x[i]-10 and ra lt x[i]+10 and dec gt y[i]-10 and dec lt y[i]+10)
              dist=separation(ra[near],dec[near],x[i],y[i])/3600.
              w=where(dist le xrtrad)
              w=near[w]
              if i lt ntiles[0] then expmap1[w]=expmap1[w]+1.
              if i lt ntiles[1] then expmap4[w]=expmap4[w]+1.
              if i lt ntiles[2] then expmap7[w]=expmap7[w]+1.
              if i lt ntiles[3] then expmap19[w]=expmap19[w]+1.
              if i lt ntiles[4] then expmap37[w]=expmap37[w]+1.
              if i lt ntiles[5] then expmap61[w]=expmap61[w]+1.
              if i lt ntiles[6] then expmap91[w]=expmap91[w]+1.
           endfor 
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
           print,ptot[j,*]
           if not keyword_set(noplot) then begin 
              skycircle,center[0],center[1],rad,cx,cy,n=360.
              p=plot(cx,cy,color='red',/over,/current)
              skycircle,center[0],center[1],rad2,cx,cy,n=360.
              p=plot(cx,cy,color='green',/over,/current)
;              legend,ntostr(ntiles)+' tiles: p='+numdec(ptot[j,*],3),/top,/left,box=0
              t=text(0.1,0.7,ntostr(ntiles)+' tiles: p='+numdec(ptot[j,*],3),/overplot,/current,font_size=10)
              im.save,'~/Swift/Tiling/ligo/'+outdetfile[j]
              im.refresh
              im.close
;              key=get_kbrd(10)
;              if key eq 's' then stop
           endif 
        endfor 

     endfor 
     ptot2=ptot
  endif 

  ymax=[300,300,250,200,150,100,100]
  begplot,name='~/Swift/Tiling/LIGO_enclosed_prob3.ps',/color,font='helvetica'
  multiplot,[1,7],/init
  for t=0,6 do begin
     if t eq 6 then xtitle='Enclosed LIGO Probability in # XRT Tiles' else xtitle=''
     if t eq 0 then title='3 GW Detectors' else title=''
     multiplot
     plot,[0,0.9],[0,20],/nodata,xtitle=xtitle,ytitle='N',xrange=[0,1],yrange=[0,ymax[t]],/xsty,/ysty,charsize=1.5,title=title
     plothist,ptot3[*,t],bin=0.01,x,y,fcolor=!blue,/fill,/overplot
     legend,ntostr(ntiles[t])+' Tiles',box=0,/top,/right,charsize=1.5
  endfor 
  multiplot,/reset,/default
;  legend,ntostr(ntiles)+' Tiles',textcolor=color,box=0,/top,/right
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/LIGO_enclosed_prob3.ps ~/Swift/Tiling/LIGO_enclosed_prob3.pdf'


  ymax=[120,100,100,80,60,40,40]
  begplot,name='~/Swift/Tiling/LIGO_enclosed_prob2.ps',/color,font='helvetica'
  multiplot,[1,7],/init
  for t=0,6 do begin
     if t eq 6 then xtitle='Enclosed LIGO Probability in # XRT Tiles' else xtitle=''
     if t eq 0 then title='2 GW Detectors' else title=''
     multiplot
     plot,[0,0.9],[0,20],/nodata,xtitle=xtitle,ytitle='N',xrange=[0,1],yrange=[0,ymax[t]],/xsty,/ysty,charsize=1.5,title=title
     plothist,ptot2[*,t],bin=0.01,x,y,fcolor=!blue,/fill,/overplot
     legend,ntostr(ntiles[t])+' Tiles',box=0,/top,/right,charsize=1.5
  endfor 
  multiplot,/reset,/default
;  legend,ntostr(ntiles)+' Tiles',textcolor=color,box=0,/top,/right
  endplot
  spawn,'ps2pdf ~/Swift/Tiling/LIGO_enclosed_prob2.ps ~/Swift/Tiling/LIGO_enclosed_prob2.pdf'
  stop
  
  return
end

pro tiling,arr=arr

  exposure=2880.
  overhead=20.
  ntiles=91
  tiledur=exposure/ntiles-overhead

  probmap=mrdfits('glg_locprob_all_bn140916234_v01.fit',1,hdr)
  cra=sxpar(hdr,'CRVAL1')
  cdec=sxpar(hdr,'CRVAL2')
  pix=sxpar(hdr,'CDELT1')
  xmap=fltarr(512,512)
  ymap=fltarr(512,512)
  xmap[0,0]=cra-pix*512/2.
  ymap[0,0]=cdec-pix*512/2.
  for i=0,511 do begin
     xmap[i,*]=xmap[0,0]+pix*i
     ymap[*,i]=ymap[0,0]+pix*i
  endfor 

  r=random_pdist(probmap,1000L,arr=arr,xrand=xrand,yrand=yrand,xarr=xarr,yarr=yarr)
  center=[cra,cdec]

  xrt_tiling,ntiles,x,y,rad,center=center

  contour,probmap,xmap,ymap,/iso,/overplot,levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)]
  xp=xmap[xrand,yrand]
  yp=ymap[xrand,yrand]

  oplot,xp,yp,psym=1

  stop

return
end 

;;; need either a single example or distribution of loc contours
;;; need to account for background in determining if point is
;;; detection (flux should be okay)
