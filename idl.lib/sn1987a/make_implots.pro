PRO make_implots,ps=ps
  
  dir=!data+'sn1987a/image_analysis/'
;  subdir='2008_02_18/'
  subdir=''
;  append='_free'
  append=''
  orig=file_search(dir+'sn1987a*300-8000_cr.fits')
  ind=[ntostr([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26])]
;  n=n_elements(ind)
  files=['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr.fits','sn1987a21A_300-8000_cr.fits','sn1987a22a_300-8000_cr.fits','sn1987a22b_300-8000_cr.fits','sn1987a23a_300-8000_cr.fits','sn1987a23b_300-8000_cr.fits','sn1987a23c_300-8000_cr.fits','sn1987a24_300-8000_cr.fits']
  n=n_elements(files)
;  n=n_elements(orig)
  cs=3
  if keyword_set(ps) then begplot,name='~/SNR87A/fits_to_im.eps',/encap,font='helvetica',/land

;  !x.margin=[3,0]
;  !y.margin=[3,0]
;  multiplot2,[3,2],/init
  g=0
  stop
  if g ne 0 then n=g+1
  print,'what g?'
;position=[0.1,0.1,0.1,0.1]
  for i=g,n-1 do begin
     !p.multi=[0,3,2]
;  erase
;     orig=dir+'sn1987a'+ind[i]+'_300-8000_cr_psu.fits'
;     if not exist(orig) then 
;     orig=dir+'sn1987a'+ind[i]+'_300-8000_cr.fits'
     orig=dir+files[i]
     print,orig
     im=mrdfits(orig,0)
     rdis,im,/nolabel,/nocenter
;plot,indgen(10),color=!white,xticks=1,yticks=1
     legend,'a',box=0,/top,/left
     ring=dir+subdir+'ring'+append+'/new_sn1987a'+ind[i]+'_fitim.fits'
     if exist(ring) then begin 
        newim=mrdfits(ring,0)
        rdis,newim,/nolabel,/nocenter
; plot,indgen(10),color=!white,xticks=1,yticks=1
       legend,'b',box=0,/top,/left
        rim=mrdfits(ring,1)
        plot,indgen(10),color=!white,xticks=1,yticks=1
        rdis,rim,/nolabel,/nocenter
;plot,indgen(10),color=!white,xticks=1,yticks=1

        legend,'c',box=0,/top,/left
     endif else plot,indgen(10),color=!white,xticks=1,yticks=1

     bilat=dir+subdir+'bilat'+append+'/new_sn1987a'+ind[i]+'_fitim.fits'
     if exist(bilat) then begin 
        bim=mrdfits(bilat,1)
        rdis,bim,/nolabel,/nocenter
;plot,indgen(10),color=!white,xticks=1,yticks=1
        legend,'d',box=0,/top,/left
     endif else plot,indgen(10),color=!white,xticks=1,yticks=1
;     ptsrc=dir+subdir+'ptsrc/new_sn1987a'+ind[i]+'_fitim.fits'
;     pim=mrdfits(ptsrc,1)
;;     multiplot2
;     rdis,pim,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1
;     legend,'e',box=0,/top,/left
     lobes=dir+subdir+'lobes'+append+'/new_sn1987a'+ind[i]+'_fitim.fits'
     if exist(lobes) then begin 
        lim=mrdfits(lobes,1)
        rdis,lim,/nolabel,/nocenter
;plot,indgen(10),color=!white,xticks=1,yticks=1
        legend,'e',box=0,/top,/left
     endif else plot,indgen(10),color=!white,xticks=1,yticks=1
;     plot,indgen(10),color=!white,xticks=1,yticks=1
     print,ind[i]
     if not keyword_set(ps) then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 

  endfor 


;  multiplot2,/reset
;  multiplot2,/default
  if keyword_set(ps) then endplot
  !p.multi=0
  return
END 

  

