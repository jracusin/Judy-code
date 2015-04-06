pro make_grid,pmap,xmap,ymap

  np=1e5                         ;;; number of points
  npgen=round(randomu(seed,np)*np)  ;; uniform random draw from np, np times
  lat=reverse(acos((npgen/(np+1.)*2.-((np-1.)/(np+1.))))*!radeg-90.)
  lon=randomu(seed,np)*360.
  
  plotsym,0,0.2
  map_set,/grid,/hammer,latdel=30,londel=30
  oplot,lon,lat,psym=8

  radone=lon
  decdone=lat
  xrtrad=23.6/2./60.
  plist=fltarr(np)
  for i=0L,np-1 do begin 
     wnear=where(xmap gt radone[i]-1 and xmap lt radone[i]+1 and ymap gt decdone[i]-1 and ymap lt decdone[i]+1)
     dist=separation(xmap[wnear],ymap[wnear],radone[i],decdone[i])/3600.
     w=where(dist le xrtrad) ;;; pixels all within xrtrad
     w=wnear[w]
     plist[i]=total(pmap[w])/total(pmap) ;;; prob in each tile
  endfor      

  s=reverse(sort(plist))
  n=200.
  for i=0,n-1 do begin 
     skycircle,radone[s[i]],decdone[s[i]],xrtrad,x,y,n=30
     oplot,x,y,color=!red
  endfor 

  stop
return
end 

pro neighbor_tiles,radone,decdone,ranew,decnew

  n=n_elements(radone)
  ranew=0.
  decnew=0.
  for i=0,n-1 do begin 
     tiles7,x,y,xcen=radone[i],ycen=decdone[i]
     ranew=[ranew,x]
     decnew=[decnew,y]
  endfor 
  ranew=ranew[1:*]
  decnew=decnew[1:*]
  coords=numdec(ranew,2)+'_'+numdec(decnew,2)
  r=rem_dup(coords)
  ranew=ranew[r]
  decnew=decnew[r]
  return
end 
pro tiles7,x,y,xcen=xcen,ycen=ycen

  r=23.6/2. ;; xrtrad
  xrtrad=r
  g=r*sqrt(3.)
  g2=r*sqrt(2.)
  x=[0,-g,-g*0.5,g*0.5,g,g*0.5,-g*0.5]
  y=[0,0,-g2,-g2,0,g2,g2]
  if n_elements(xcen) eq 0 then xcen=0
  if n_elements(ycen) eq 0 then ycen=0
  x=x/60.+xcen
  y=y/60.+ycen

  return
end 
pro xrt_tiling_alg

  ;;; make list of tiles within some XX prob contour (1sig, 2sig?)
  ;;; which ones are currently visible
  ;;; which tile is most probable
  ;;; which tile is in next most probable bin
  ;;; which tile is closest - need to figure out trade off between
  ;;;                         prob and nearness

  ;; read in pmap, define ramap and decmap 
  ;; define full set of tiles
  ;;;;;; 1) start with max pmap
  ;;;;;; 2) then neighbor of already done tiles with next highest
  ;;;;;; enclosed prob - make list of already done and check against
  ;;;;;; 3) build up grid
  ;;;;;; 4) optimize order is another problem

  dir='~/Fermi/gbmtrig/'
  cd,dir
  locprob=file_search('glg_locprob_all*fit')

  r=23.6/2./60. ;; xrtrad
  xrtrad=r
  sig=0.68
  prob=0.

  pmap=mrdfits(locprob[3],1,hdr)
  cra=50.
  cdec=30.
  pix=sxpar(hdr,'CDELT1')
  xmap=fltarr(512,512)
  ymap=fltarr(512,512)
  xmap[0,0]=cra-pix*512/2.
  ymap[0,0]=cdec-pix*512/2.
  for i=0,511 do begin
     xmap[i,*]=xmap[0,0]+pix*i
     ymap[*,i]=ymap[0,0]+pix*i
  endfor 

  amap=pmap

  make_grid,pmap,xmap,ymap
stop

  rad=20.
  map_set,/aitoff,/grid,limit=[cra-rad,cdec-rad,cra+rad,cdec+rad],label=10,londel=10,latdel=10
  contour,pmap,xmap,ymap,levels=[1.-sigprob(3.),1.-sigprob(2.),1.-sigprob(1.)],/iso,/overplot

  n=100
;  radone=fltarr(n)
;  decdone=fltarr(n)
  plist=fltarr(n)

  ;;; start with tile centered on most probable pixel
  maxpix=max(amap,mmax)
  cra=xmap[mmax]  ;; ra,dec of that tile center
  cdec=ymap[mmax]
  radone=cra ;;; start list of tiles that are already done
  decdone=cdec
  wnear=where(xmap gt cra-1 and xmap lt cra+1 and ymap gt cdec-1 and ymap lt cdec+1)
  dist=separation(xmap[wnear],ymap[wnear],cra,cdec)/3600.
  w=where(dist le xrtrad) ;;; pixels all within xrtrad
  w=wnear[w]
  plist[0]=total(amap[w])/total(pmap) ;;; prob in each tile
  w0=where(dist le xrtrad*0.8)  ;; set prob to 0 within 0.8*xrtrad
  w0=wnear[w0]
  amap[w0]=0
  skycircle,cra,cdec,r,x,y,n=30
  oplot,x,y,color=!green

  ranew0=radone
  decnew0=decdone
  for i=1,100 do begin 
     ;;; neighbor tiles
     neighbor_tiles,ranew0,decnew0,ranew,decnew
     ranew0=ranew
     decnew0=decnew
     radone=[radone,ranew]
     decdone=[decdone,decnew]
     for j=0,n_elements(radone)-1 do begin 
        skycircle,radone[j],decdone[j],r,x,y,n=30
        oplot,x,y,color=!green
     endfor 
  endfor 

  plist=fltarr(n_elements(radone))
  for i=0,n_elements(radone)-1 do begin 
     wnear=where(xmap gt radone[i]-1 and xmap lt radone[i]+1 and ymap gt decdone[i]-1 and ymap lt decdone[i]+1)
     dist=separation(xmap[wnear],ymap[wnear],radone[i],decdone[i])/3600.
     w=where(dist le xrtrad) ;;; pixels all within xrtrad
     w=wnear[w]
     plist[i]=total(pmap[w])/total(pmap) ;;; prob in each tile
  endfor 
  help,radone,decdone
  s=sort(plist)  ;;; sort probs, grab top 200

  n=200
  for i=0,n-1 do begin
     
     skycircle,radone[s[i]],decdone[s[i]],r,x,y,n=30
     oplot,x,y,color=!red
  endfor 

  
;;; algs - build set of tiles based on proximity,prob, & viewability
;;;        for each contour
;;;      - preset grid - then calc probs in each tile, and select
;;;        subset and order based on proximity & viewability
;;;          - could also shift grid to match center at most probable pix
;;;      - random grid - calc prob in each random tile, and select N
;;;        most probable
;;;      - prob weighted random grid - but want to reduce overlap?

  stop
return
end
