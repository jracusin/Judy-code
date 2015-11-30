pro aitoff_maps,redo=redo,gal=gal

  cd,'~/Fermi/constellations/'

  add=['c0_0','c0_180']
  if keyword_set(gal) then add='gal_'+add
  imfile='~/Fermi/constellations/'+add+'_aitoff_map.png'

  if not exist(imfile[0]) or keyword_set(redo) then begin 
     read_fits_map,'~/Fermi/constellations/healpix_diffuse_bin28.fits',hmap,hdr,nside=nside
     ;; coords for each healpix pixel
     pix2ang_ring,nside,lindgen(n_elements(hmap)),theta,phi
     naxis=512.
     
     crval1=[0,180]
     crval2=[0,0]
     cdelt=[0.67,0.67]
     ctype=['AIT','AIT']
     rev=[1,1]

     for j=0,1 do begin 
        gmap=fltarr(naxis,naxis)
        ghdr=headfits('~/Fermi/constellations/empty_map.fits')
        sxaddpar,ghdr,'CRPIX1',0.5*(naxis-1.)
        sxaddpar,ghdr,'CRPIX2',0.5*(naxis-1.)
        sxaddpar,ghdr,'CDELT1',cdelt[j]
        sxaddpar,ghdr,'CDELT2',cdelt[j]
        if keyword_set(gal) then begin
           sxaddpar,ghdr,'CTYPE1','GLON-'+ctype[j]
           sxaddpar,ghdr,'CTYPE2','GLAT-'+ctype[j]
        endif else begin
           sxaddpar,ghdr,'CTYPE1','RA---'+ctype[j]
           sxaddpar,ghdr,'CTYPE2','DEC--'+ctype[j]
        endelse 
        sxaddpar,ghdr,'CRVAL1',crval1[j]
        sxaddpar,ghdr,'CRVAL2',crval2[j]
        
        x=fltarr(naxis,naxis)
        y=fltarr(naxis,naxis)
        for i=0,naxis-1 do begin
           x[i,*]=findgen(naxis)
           y[*,i]=findgen(naxis)
        endfor 
        
        extast,ghdr,astr
        xy2ad,x,y,astr,ra,dec
        
        w=where(dec le 0 or dec ge 0)
;     if j eq 0 then w=where(dec le 0)
;     if j eq 1 then w=where(dec ge 0)
;     if j eq 2 then w=where(abs(dec) le 30)
;     if j eq 3 then w=where(abs(dec) le 30)

        if not keyword_set(gal) then euler,ra,dec,glon,glat,1 else begin
           glon=ra
           glat=dec
        endelse 

        print,minmax(glon),minmax(glat)
        phi=glon*!dtor          ;(360.-gra)/!radeg
        theta=0.5*!pi-glat*!dtor
        w0=where(theta lt 0)
        theta[w0]=0.

        ang2pix_ring, nside, theta[w], phi[w], ipring
        gmap[w]=hmap[ipring]
        
        w=where(gmap eq 0)
        gmap[w]=1e-17
        
        if j ge 2 then land=1 else land=0
        mwrfits,gmap,add[j]+'_aitoff_map.fits',/create
        begplot,name=add[j]+'_aitoff_map.ps',land=land
        rdis,rotate(gmap,rev[j]),/log,low=-15.5,high=-11.0,/nolabels,/noframe
        endplot
        spawn,'ps2pdf '+add[j]+'_aitoff_map.ps '+add[j]+'_aitoff_map.pdf'
        spawn,'convert '+add[j]+'_aitoff_map.pdf '+add[j]+'_aitoff_map.png'
     endfor      
  endif 

if keyword_set(gal) then stop

  cat=mrdfits('~/Fermi/gll_psc_v14.fit',1)
  ra=cat.raj2000
  dec=cat.dej2000
  flux=cat.flux1000
  flux=alog10(flux)
  bin=0.2
  plothist,flux,x,y,bin=bin,/noplot
  cra=[0,180]
  cdec=[0,0]
  color=[replicate('gainsboro',2),replicate('light gray',3),$
         replicate('silver',3),replicate('dark gray',3),$
         replicate('gray',3),replicate('dim gray',3),$
         replicate('dim gray',3),replicate('black',3),$
         replicate('black',3),$
         replicate('black',2)]


;  limit=[[-30,0,30,360],[-30,-180,30,180]]
  cl=[0,180]
  position=[[0.05,0.05,0.95,0.95],[0.05,0.05,0.95,0.95]]
;  off=[[-0.01,-0.04,0,0.01],
  off=[[0.0,-0.04,0.02,0.02],[0.0,-0.04,0.01,0.02]]
  xrange=[[125,480],[130,480]]
  for j=0,1 do begin 
     im=image(imfile[j],/clip,xrange=xrange[*,j],yrange=[300,500],$
              position=position[*,j])
     pos=position[*,j]+off[*,j]
     m=map('hammer',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=cl[j],$
           color='grey',position=pos,/hide)
    
     p=plot(replicate(179,181)-cl[j],findgen(181)-90,thick=2,/current,/overplot)
     p=plot(replicate(181,181)-cl[j],findgen(181)-90,thick=2,/current,/overplot)
     for i=0,n_elements(x)-1 do begin 
        dist=separation(ra,dec,cra[j],cdec[j])/3600.
        q=where(flux gt x[i]-bin/2. and flux le x[i]+bin/2.,nq)
        if nq gt 0 then begin 
           s=sort(flux[q])
           q=q[s]
           sym=0              
           size=0.02+0.02*i
           if i ge 20 then sym=3
           p=plot(-ra[q],dec[q],symbol='o',sym_size=size,/current,/overplot,sym_color='white',color[i],/sym_filled,sym_fill_color=color[i],line='none',sym_thick=0.3)

        endif 
     endfor
     
     im.save,'~/Fermi/constellations/fermi_constellations_'+add[j]+'.ps'
     im.save,'~/Fermi/constellations/fermi_constellations_'+add[j]+'.png',resolution=8192
     im.close
  endfor 

stop
  return
end

pro diffuse

  read_fits_map,'~/Fermi/constellations/healpix_diffuse_bin28.fits',hmap,hdr,nside=nside
  ;; coords for each healpix pixel
  pix2ang_ring,nside,lindgen(n_elements(hmap)),theta,phi
  naxis=512.
;  mwrfits,gmap,'~/Fermi/constellations/empty_map.fits',/create

  crval1=[0,0,180,0]
  crval2=[-90,90,0,0]
  cdelt=[0.5,0.5,0.67,0.67]
  add=['south','north','eq','eq2']
  ctype=['STG','STG','MER','MER']
  rev=[3,3,1,1]
  for j=0,3 do begin 
     gmap=fltarr(naxis,naxis)
     ghdr=headfits('~/Fermi/constellations/empty_map.fits')
     sxaddpar,ghdr,'CRPIX1',0.5*(naxis-1.)
     sxaddpar,ghdr,'CRPIX2',0.5*(naxis-1.)
     sxaddpar,ghdr,'CDELT1',cdelt[j]
     sxaddpar,ghdr,'CDELT2',cdelt[j]
     sxaddpar,ghdr,'CTYPE1','RA---'+ctype[j]
     sxaddpar,ghdr,'CTYPE2','DEC--'+ctype[j]
     sxaddpar,ghdr,'CRVAL1',crval1[j]
     sxaddpar,ghdr,'CRVAL2',crval2[j]
 
     x=fltarr(naxis,naxis)
     y=fltarr(naxis,naxis)
     for i=0,naxis-1 do begin
        x[i,*]=findgen(naxis)
        y[*,i]=findgen(naxis)
     endfor 
     
     extast,ghdr,astr
     xy2ad,x,y,astr,ra,dec
     
     if j eq 0 then w=where(dec le 0)
     if j eq 1 then w=where(dec ge 0)
     if j eq 2 then w=where(abs(dec) le 30)
     if j eq 3 then w=where(abs(dec) le 30)

     euler,ra,dec,glon,glat,1

     print,minmax(glon),minmax(glat)
     phi=glon*!dtor             ;(360.-gra)/!radeg
     theta=0.5*!pi-glat*!dtor
     w0=where(theta lt 0)
     theta[w0]=0.

     ang2pix_ring, nside, theta[w], phi[w], ipring
     gmap[w]=hmap[ipring]
     
     w=where(gmap eq 0)
     gmap[w]=1e-17
;     if j ge 2 then begin 
;        gmap[w]=1e-15
;     endif 
     
     if j ge 2 then land=1 else land=0
     mwrfits,gmap,add[j]+'_stereo_map.fits',/create
     begplot,name=add[j]+'_stereo_map.ps',land=land
     rdis,rotate(reverse(gmap),rev[j]),/log,low=-15.5,high=-11.0,/nolabels,/noframe
     endplot
     spawn,'ps2pdf '+add[j]+'_stereo_map.ps '+add[j]+'_stereo_map.pdf'
     spawn,'convert '+add[j]+'_stereo_map.pdf '+add[j]+'_stereo_map.png'
  endfor      

  return
end 

pro write_conmaps,ra,dec,flux,x,bin

  ;; north pole, south pole, 
  cra=[0,180,180,0];90,180,270]
  cdec=[90,-90,0,0];0,0,0]
  rad=91
  rarad=[180,180,rad,rad,rad,rad]
  decrad=replicate(90,6);[rad,rad,rad,rad,rad,rad]

  ralow=cra-rarad
  rahigh=cra+rarad
  declow=cdec-decrad
  dechigh=cdec+decrad
  title=['North Pole (RA=0, Dec=90)','South Pole (RA=0, Dec=-90)','Spring Sky (RA=0, Dec=0)','Summer Sky (RA=90, Dec=0)','Autumn Sky (RA=180, Dec=0)','Winter Sky (RA=270, Dec=0)']
;  subtitle=['(RA=0, Dec=90)','(RA=0, Dec=-90)','(RA=0, Dec=0)','(RA=90, Dec=0)','(RA=180, Dec=0)','(RA=270, Dec=0)']
;  nx=n_elements(x)

;  cx=1-(normalize(x)*0.9+0.05)

;  cx=round((findgen(nx)+1)*(100./nx))
;  color=(220-22)*cx
;stop

  add='_v7'
  imfile=['~/Fermi/constellations/north_stereo_map.png','~/Fermi/constellations/south_stereo_map.png','~/Fermi/constellations/eq_stereo_map.png','~/Fermi/constellations/eq2_stereo_map.png']
  ns=['_north','_south','_eq']
;  begplot,name='~/Fermi/constellations/fermi_constellations'+add+'.ps',font='helvetica',/land
  ;; color=[replicate(!grey90,2),replicate(!grey80,3),$
  ;;        replicate(!grey70,3),replicate(!grey60,3),$
  ;;        replicate(!grey50,3),replicate(!grey40,3),$
  ;;        replicate(!grey30,3),replicate(!grey20,3),$
  ;;        replicate(!grey10,3),$
  ;;        replicate(!black,2)]
  color=[replicate('gainsboro',2),replicate('light gray',3),$
         replicate('silver',3),replicate('dark gray',3),$
         replicate('gray',3),replicate('dim gray',3),$
         replicate('dim gray',3),replicate('black',3),$
         replicate('black',3),$
         replicate('black',2)]
  
  position=[[0.05,0.45,0.5,0.95],[0.5,0.45,0.95,0.95],[0.2,0.25,0.8,0.45],[0.2,0.05,0.8,0.25]]

; !p.multi=[0,2,1]
; multiplot,[2,1],/init
; !x.margin=[0.1,0.1]
; !y.margin=[0.1,0.1]
  for j=0,1 do begin 
;     multiplot
;     !p.clip=1
;     if j eq 0 then begin 
;        im=read_png('~/Fermi/constellations/north_stereo_map.png')
;        tvscl,im
     if j eq 1 then current=1 else current=0
     im=image(imfile[j],/clip,xrange=[115,495],yrange=[230,560],current=current,position=position[*,j])
     m=map('stereographic',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=cra[j],center_latitude=cdec[j],$
           color='grey',limit=[declow[j],ralow[j],dechigh[j],rahigh[j]],/hide,$
           position=position[*,j])
     
;     endif 
;     im=mrdfits(ns[j]+'_stereo_map.fits',0)
;     rdis,rotate(reverse(im),3),/log,color=!white,/nocenter;,/nolabels,/noframe
;     tvscl,alog10(rotate(reverse(im),3))
     ;; map_set, /stereographic, /noerase,$
     ;;          /ISOTROPIC, cdec[j],cra[j],reverse=3,/advance,$
     ;;          limit=[declow[j],rahigh[j],dechigh[j],ralow[j]],/noborder,clip=1;,title=title[j]
     ;; map_grid,londel=30,latdel=30,color=!grey70
;     if j eq 1 then oplot,findgen(37)*10.,replicate(-0.1,37),line=1,color=!grey70
     print,[declow[j],ralow[j],dechigh[j],rahigh[j]],title[j]
     for i=0,n_elements(x)-1 do begin 
        dist=separation(ra,dec,cra[j],cdec[j])/3600.
        q=where(flux gt x[i]-bin/2. and flux le x[i]+bin/2. and dist le 90.,nq)
        if nq gt 0 then begin 
           s=sort(flux[q])
           q=q[s]
           sym=0              
           size=0.02+0.02*i
           if i ge 20 then sym=3
;          for k=0,nq-1 do begin 
           ;;    plotsym,sym,size,/fill
           ;;    plots,ra[q[k]],dec[q[k]],psym=8,color=color[i]
           ;;    plotsym,sym,size,color=!white,thick=1
           ;;    plots,ra[q[k]],dec[q[k]],psym=8 ;,color=color[i]
;          endfor 
           p=plot(180+ra[q],dec[q],symbol='o',sym_size=size,/current,/overplot,sym_color='white',color[i],/sym_filled,sym_fill_color=color[i],line='none',sym_thick=0.3)

        endif 
     endfor
;     eqra=findgen(181)*2.
;     eqdec=fltarr(181)
;     oplot,eqra,eqdec,color=!salmon;,thick=0.5 ;; celestial equator
;     euler,eqra,eqdec,glon,glat,2
;     oplot,glon,glat,color=!lightblue;,thick=0.5 ;; galactic plane
;     euler,eqra,eqdec,elon,elat,4
;     oplot,elon,elat,color=!lightgreen;,thick=0.5 ;; ecliptic

;k=get_kbrd(10)
;     im.save,'~/Fermi/constellations/fermi_constellations'+ns[j]+add+'.png'
;     im.close
  endfor 
;  !p.multi=0
;  multiplot,/reset,/default
;;; EQUATORIAL STRIP
;  !y.margin=[10,10]
;  plot,[360,0],[-30,30],/nodata,/iso,color=!white,xrange=[360,0],yrange=[-30,30],/xsty,/ysty

  limit=[[-30,0,30,360],[-30,-180,30,180]]
  cl=[180,0]
  for j=2,3 do begin 
     im=image(imfile[j],/clip,xrange=[190,605],$;[120,490],$ ;[130,480],
              yrange=[250,365],$;[346,448],$
              current=current,position=position[*,j])
     pos=position[*,j]+[0,-0.005,0,0.00]
     m=map('mercator',label_show=0,/current,linestyle=1,$
           thick=2,center_longitude=cl[j-2],$
           color='grey',limit=limit[*,j-2],position=pos,/hide)
;  ra2=ra
     if j eq 2 then begin 
        ra2=ra;+40
        w=where(ra2 gt 360)
        ra2[w]=ra2[w]-360.
        w=where(ra2 lt 0)
        ra2[w]=ra2[w]+360.
     endif else begin
        ra2=ra;-30.
        w=where(ra2 gt 180)
        ra2[w]=ra2[w]-360.
        w=where(ra2 lt -180)
        ra2[w]=ra2[w]+360.
     endelse 
     for i=0,n_elements(x)-1 do begin 
        q=where(flux gt x[i]-bin/2. and flux le x[i]+bin/2. and abs(dec) le 30,nq)
        if nq gt 0 then begin 
           s=sort(flux[q])
           q=q[s]
           sym=0              
           size=0.02+0.02*i
           p=plot(ra2[q],dec[q],symbol='o',sym_size=size,/current,/overplot,sym_color='white',/sym_filled,sym_fill_color=color[i],line='none',sym_thick=0.3)
;           p2=plot(cl,[0.,0.],symbol='+',sym_color='red',/current,/overplot,linestyle='none')

        endif 
     endfor 
  endfor 
  im.save,'~/Fermi/constellations/fermi_constellations'+add+'.ps'
  im.save,'~/Fermi/constellations/fermi_constellations'+add+'.png'
;  im.close

;  endplot
;  spawn,'ps2pdf ~/Fermi/constellations/fermi_constellations'+add+'.ps ~/Fermi/constellations/fermi_constellations'+add+'.pdf'

  return
end 

pro fermi_constellations

;  !p.background=!white
;  !p.color=!black
  cat=mrdfits('~/Fermi/gll_psc_v14.fit',1)
;  readcol,'~/Fermi/3FGL_list_for_Judy.txt',source,ra,dec,sig,flux100,flux100err,flux1000,flux1000err,format='(a,f,f,f,d,d,d,d)'
;  cat=mrdfits('~/Fermi/gll_psc_v06.fit',1)
  ra=cat.raj2000
  dec=cat.dej2000
  flux=cat.flux1000
  flux=alog10(flux)
  bin=0.2
  plothist,flux,x,y,bin=bin,/noplot
;  oplot,[-8,-8],minmax(y)

  write_conmaps,ra,dec,flux,x,bin

  return
end 
