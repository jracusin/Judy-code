pro make_images_for_paper,ps=ps,dir=dir
  
  if keyword_set(ps) then begplot,name='sn1987a_images.eps',/encaps,font='helvetica'
  !p.multi=[0,4,5]
  !x.margin=[1,1]
  !y.margin=[1,1]
  
  low=0
  high=0
  sn1987a_age,age,date=date
  
  ;;;REMOVE LETG OBS
  w=where(date[0,*] eq 2004 and date[1,*] eq 8)
  good=intarr(n_elements(age))
  good[w]=1
  w=where(good eq 0,nw)
  date2=intarr(3,nw)
  date2[0,*]=date[0,w]
  date2[1,*]=date[1,w]
  date2[2,*]=date[2,w]

  xoff=10
  yoff=5
  if n_elements(dir) eq 0 then dir=''
;  imfiles=file_search(dir+'sn1987a*_300-8000_cr.fits')
  imfiles=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr_psu.fits','sn1987a21A_300-8000_cr_psu.fits']
;  im11pos=strpos(imfiles,'a11')
;  w=where(im11pos eq -1)
;  imfiles=imfiles[w]

  nim=n_elements(imfiles)
  nf=intarr(nim)
  for i=0,nim-1 do begin
     pos=strpos(imfiles[i],'_300')
     num=strmid(imfiles[i],pos[n_elements(pos)-1]-2,2)
     pos2=strpos(num,'a')
     if pos2 ne -1 then nf[i]=strmid(num,1,1) else nf[i]=num

  endfor
  print,nf

;  imfiles=imfiles[sort(nf)]
;  date=date[*,sort(nf)]
  
  for i=0,nim-1 do begin 
     im=imfiles[i]
;     im=mrdfits(dir+'sn1987a'+ntostr(i+1)+'_300-8000_cr.fits',0,hdr,/silent)
     im=mrdfits(imfiles[i],0,hdr,/silent)
     
     time=sxpar(hdr,'EXPOSURE')
;     if i eq 0 then im=im*6.  ;FIDDLED WITH JUST TO GET CONTRAST FOR FIGURE
;     if i eq 10 then im=im*8.
     print,time
     fim=im;*total(im)/time
;     print,total(im),total(im)/time
;     lo=min(fim)
;     low=[low,lo]
;     hi=max(fim)
;     high=[high,hi]
     
     print,min(fim),max(fim)
;     low=-3.7990803e-13;-5.41807e-10;-2.5685432e-14
;     high=1.0830593e-05;0.00940662;2.1443753e-07
;     low=-2.5685432e-14
;     high=2.1443753e-07
     low=-5e-4;-1.53232e-10
     high=0.00940662*0.3
     rdis,fim,/silent,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '] ;,high=high,low=low
;     plot,[0,1],/nodata,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' ']
;     tvscl,fim,i
     mm=date2[1,i]
     if mm lt 10 then mm='0'+ntostr(mm) else mm=ntostr(mm)
     legend,[ntostr(date2[0,i])+'-'+mm],position=[-15,190],box=0,charsize=1
     if i eq 0 then begin 
        oplot,[175+xoff,175+xoff],[10+yoff,40+yoff]
        oplot,[170+xoff,175+xoff],[35+yoff,41+yoff]
        oplot,[175+xoff,180+xoff],[41+yoff,35+yoff]
        oplot,[145+xoff,175+xoff],[10+yoff,10+yoff]
        oplot,[144+xoff,150+xoff],[9+yoff,15+yoff]
        oplot,[144+xoff,150+xoff],[9+yoff,5+yoff]
        xyouts,170+xoff,45+yoff,'N',charsize=0.7
        xyouts,127+xoff,5+yoff,'E',charsize=0.7
        oplot,[72,138],[45,45]
        xyouts,75,30,'1 arcsec',charsize=0.7
     endif 
  endfor
;  low=min(low[1:*])
;  high=max(high[1:*])
;  print,low,high
  
  if keyword_set(ps) then endplot
  !p.multi=0
  stop  
  
  ;;;;specific model fits
  if keyword_set(ps) then begplot,name='fits_to_im12.eps',/land,/encaps,ysize=3,font='helvetica'
;  mdir=['ring','ptsrc','lobes']
  mdir=['ring','bilat','lobes']
  title=['a','b','c','d','e']
;  n=indgen(nim)+1
  n=[1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21]
;  n=[7,8,9,10]
  
  !p.multi=[0,5,1]
  for t=0,n_elements(n)-1 do begin
     i=n[t]-1
     print,i+1
     for j=0,2 do begin
;        im=mrdfits(dir+'sn1987a'+ntostr(i+1)+'_300-8000_cr.fits',0,hdr,/silent)
        im=mrdfits(imfiles[t],0,hdr,/silent)
        ofile=mdir[j]+'/new_sn1987a'+ntostr(i+1)+'_fitim.fits'
        if j eq 0 then begin 
           newim=mrdfits(ofile,0,/silent)
           rdis,im,/silent,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '],xtitle=title[0]
           rdis,newim,/silent,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '],xtitle=title[1]
        endif 
        fitim=mrdfits(ofile,1,/silent)
        rdis,fitim,/silent,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '],xtitle=title[j+2]
     endfor 
     k=get_kbrd(10)
  endfor 
  !p.multi=0
  if keyword_set(ps) then endplot
  
  return
end 
