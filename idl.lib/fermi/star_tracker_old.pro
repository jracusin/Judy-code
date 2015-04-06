pro dump_stcat,cra,cdec,rad,outfile=outfile
  file='~/Fermi/Spacecraft/star_tracker/GLASTCAT010111_FLIGHT.txt'

  readcol,file,catnum,x,y,z,mag,eff,qual,delim=' ',format='(d,d,d,d,d,d,a)',/silent
  n=n_elements(catnum)
  ind=indgen(n)+1

  ra=atan(y,x)*!radeg
  dec=asin(z)*!radeg
  epoch=intarr(n)
  epoch[*]=2000
 
  if n_params() eq 0 then stop
  dist=separation(ra,dec,cra,cdec)/3600.

  w=where(dist le rad)
  if n_elements(outfile) eq 0 then outfile='~/Fermi/Spacecraft/star_tracker/GLASTCAT.dat'
  writecol,outfile,ind[w],ra[w],dec[w],mag[w]
return
end 

pro star_tracker,xmn,xmx,ymn,ymx,ps=ps

  dir='~/Fermi/Spacecraft/star_tracker/'
  imfiles=dir+['ccd1w.fits','ccd2w.fits','ccd3w.fits']
  nim=n_elements(imfiles)

  pix0=create_struct('x',0,'y',0,'val',0,'sig',0.,'wim',0,'bkg',0.)
  pix=replicate(pix0,1000)

  begplot,name=dir+'hot_warm_pixels.eps',/land,/color,font='helvetica'
  plotsym,0,1
  val=70  ;;; star threshold - but what about hot pixels rather than warm?
;  thresh=3.
  thresh=8.

  x=intarr(512,512)
  y=x
  for i=0,511 do begin
     x[i,*]=i
     y[*,i]=i
  endfor 

  ims=intarr(512,512,nim) & ims2=ims
  sigs=fltarr(512,512,nim)
  !p.multi=[0,2,2]
  for i=0,nim-1 do begin
     
     im=mrdfits(imfiles[i],0,hdr)
     if i le 1 then begin
        im=rotate(im,7)
     endif
;     im=im[2:509,2:509]
     ims[*,*,i]=im

     plothist,im,/ylog,xtitle='Pixel values',title='Image '+ntostr(i),charsize=1
     oplot,[val,val],[0.1,1e6],color=!red,line=2
     legend,['Stars? >'+ntostr(val)],/top,/right,box=0,textcolor=!red

     w=where(im gt val,nw)
     print,nw

     ;;; match stars in glastcat
     cra=fxpar(hdr,'CRVAL1')
     cdec=fxpar(hdr,'CRVAL2')
     rad=15
     outfile='~/Fermi/Spacecraft/star_tracker/GLASTCAT'+ntostr(i)+'dat'
     if not exist(outfile) then $
        dump_stcat,cra,cdec,rad,outfile=outfile
     readcol,outfile,ind,ra,dec,mag,format=('i,f,f,f')
     adxy,hdr,ra,dec,gx,gy
     wu=where(gx ge 0 and gx lt 512 and gy ge 0 and gy lt 512,ng)
     gx=gx[wu]
     if i le 1 then gy=511-gy[wu] else gy=gy[wu]
;     w=0
;     for u=0,ng-1 do begin
;        dist=sqrt((x-gx[u])^2+(y-gy[u])^2)
;        mindist=min(dist,md)
;        if mindist lt 2. then w=[w,md]
;     endfor 
;     w=w[1:*]
;     nw=n_elements(w)
;     print,nw
     ;;; where hot pixels
     hp=0 & mind=0.
     for u=0,nw-1 do begin
        dist=sqrt((x[w[u]]-gx)^2+(y[w[u]]-gy)^2)
        mindist=min(dist,md)
        if mindist gt 5 then begin
           hp=[hp,w[u]]
           mind=[mind,mindist]
        endif 
     endfor 
     nhp=n_elements(hp)
     if nhp gt 1 then begin 
        nhp=nhp-1
        hp=hp[1:*]
        mind=mind[1:*]
        print,ntostr(nhp)+' Hot Pixels found'
        print,'    x     y     val'
        for p=0,nhp-1 do $
           print,x[hp[p]],y[hp[p]],im[x[hp[p]],y[hp[p]]]
     endif else print,'No Hot Pixels found'

     rdis,im,low=0,high=100,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,charsize=1
     oplot,x[w],y[w],psym=8,color=!red
     oplot,gx,gy,psym=8,symsize=1.5;,color=!blue
     if nhp gt 1 then plots,x[hp],y[hp],psym=2,color=!magenta
    xyouts,520,450,'GLAST ST CAT',charsize=1
    xyouts,520,400,'pixels > '+ntostr(val),color=!red,charsize=1
    xyouts,520,350,'pixels > '+ntostr(val),color=!magenta,charsize=1
    xyouts,520,310,'  not near stars',color=!magenta,charsize=1
    xyouts,520,270,'  (hot pixels?)',color=!magenta,charsize=1
;     legend,'stars',box=0,/bottom,/right,textcolor=!red

     for j=0,nw-1 do begin  ;;; remove 3x3 box around each pixel above threshold
;        xa=[-2,-1,0,1,2,-2,-1,0,1,2,-2,-1,0,1,2,-2,-1,0,1,2,-2,-1,0,1,2]
;        ya=[-2,-2,-2,-2,-2,-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1,2,2,2,2,2]
        xa=[-1,0,1,-1,0,1,-1,0,1]
        ya=[-1,-1,-1,0,0,0,1,1,1]
        xw=x[w[j]]
        yw=y[w[j]]
        im[xw+xa,yw+ya]=0
     endfor 

     ims2[*,*,i]=im
     q=where(im ne 0) ;;; not stars
     plothist,im[q],/ylog,xtitle='Pixel values',charsize=1
     m=mean(im[q])
     std=stddev(im[q])
     print,m,std

     sig=(im-m)/sqrt(im)
;     z=where(sig[q] gt thresh,nz)  ;;; pixels > n sigma
     z=where(im[q] gt m+thresh,nz)
     n0=where(pix.x eq 0)
     n0=n0[0]
     pix[n0:n0+nz-1].x=x[q[z]]
     pix[n0:n0+nz-1].y=y[q[z]]
     pix[n0:n0+nz-1].val=im[q[z]]
     pix[n0:n0+nz-1].sig=sig[q[z]]
     pix[n0:n0+nz-1].wim=i
     pix[n0:n0+nz-1].bkg=m

     sigs[*,*,i]=sig
     
;     cx=[cx,x[q[z]]]
;     cy=[cy,y[q[z]]]
;     cval=[cval,im[q[z]]]
;     csig=[csig,sig[q[z]]]
;     wim=[wim,i]

     rdis,im,low=0,high=100,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,xtitle='x',ytitle='y',charsize=1
     oplot,x[w],y[w],psym=8,color=!red
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor 
  !p.multi=0

  w0=where(pix.x ne 0)
  pix0=pix[w0]

  ;;; combine list of warm pixels to collect values for each non-duplicate
  pix=create_struct('x',0,'y',0,'val',intarr(nim),'sig',fltarr(nim),'bkg',0.,'nwarm',0)
  pix=replicate(pix,1000)

  s=rem_dup(pix0.x*pix0.y)
  ns=n_elements(s)
  for i=0,ns-1 do begin
     ws=where(pix0.x eq pix0[s[i]].x and pix0.y eq pix0[s[i]].y,nws)
     wim=pix0[ws].wim

     pix[i].x=pix0[ws[0]].x
     pix[i].y=pix0[ws[0]].y
     pix[i].val=ims[pix[i].x,pix[i].y,*]
     pix[i].sig=sigs[pix[i].x,pix[i].y,*]
     pix[i].bkg=pix0[i].bkg
     pix[i].nwarm=nws

  endfor 

  w0=where(pix.nwarm ne 0)
  pix=pix[w0]

  w=where(pix.nwarm ge 2,nw)
  help,w

  !p.multi=[0,nim,1]
  !p.charsize=1
  for i=0,nw-1 do begin

     xmn=pix[w[i]].x-10>0
     ymn=pix[w[i]].y-10>0
     xmx=pix[w[i]].x+10<511
     ymx=pix[w[i]].y+10<511

     for j=0,nim-1 do begin
        if pix[w[i]].val[j] gt thresh+pix[w[i]].bkg then color=!red else color=!green
        rdis,ims[*,*,j],low=0,high=val+10,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,title=ntostr(pix[w[i]].x)+', '+ntostr(pix[w[i]].y),xtitle='x',ytitle='y',charsize=1.5
        oplot,pix.x,pix.y,psym=8,color=color,symsize=2
        legend,['pix val='+ntostr(pix[w[i]].val[j]),'sig='+ntostr(pix[w[i]].sig[j],4)],/bottom,/left,textcolor=color,box=0
     endfor 
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor   
  
  for j=0,nim-1 do begin
     wup=where(pix[w].val[j] ge thresh+pix[w].bkg)
     wdown=where(pix[w].val[j] lt thresh+pix[w].bkg,nwdown)
     rdis,ims[*,*,j],low=0,high=val,title='Image '+ntostr(j),xtitle='x',ytitle='y',charsize=1.5
     plots,pix[w[wup]].x,pix[w[wup]].y,psym=8,color=!red
     if nwdown gt 0 then plots,pix[w[wdown]].x,pix[w[wdown]].y,psym=8,color=!green
     legend,['> '+ntostr(thresh,1),'< '+ntostr(thresh,1)],textcolor=[!red,!green],box=0,/top,/left

  endfor 
 
  !p.multi=0
  colprint,pix[w].x,pix[w].y,pix[w].sig[0],pix[w].sig[1],pix[w].sig[2]

  endplot
  spawn,'ps2pdf '+dir+'hot_warm_pixels.eps'

  stop
  return
end 
;;; loop through each image and find pixels above background
;;; cross-correlate to get pixels that appear more than once
;;; loop again to plot each candidate
