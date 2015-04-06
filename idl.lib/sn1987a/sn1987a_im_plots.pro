pro sn1987a_im_plots,ps=ps
  
  dir='/Volumes/Firewire1/racusin/sn1987a/image_analysis/'
  files=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20_300-8000_cr.fits','sn1987a21_300-8000_cr.fits','sn1987a22a_300-8000_cr.fits','sn1987a22b_300-8000_cr.fits','sn1987a23a_300-8000_cr.fits','sn1987a23b_300-8000_cr.fits','sn1987a23c_300-8000_cr.fits','sn1987a24_300-8000_cr.fits']
  nn=n_elements(files)
  
  if keyword_set(ps) then begplot,name='~/SNR87A/sn1987a_images.ps';,/land
  !x.margin=[0.5,0.5]
  !y.margin=[0.5,0.5]
  !p.multi=[0,5,6]
  sn1987a_age,ages,date=date
  
;  im=mrdfits(files[0],0,/silent)
;  low=max(im)
;  im=mrdfits(files[nn-1],0,/silent)
;  high=max(im)
  for i=0,nn-1 do begin 
     im=mrdfits(files[i],0,/silent)
     rdis,im,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent;,low=low,high=high
     d=ntostr(date[0,i])+'-'+ntostr(date[1,i])
     xyouts,10,170,d,charsize=1.
;     if i eq 0 then begin
;        oplot,[50,150]

  endfor
  if keyword_set(ps) then endplot
  
  !p.multi=0
  
  
  

;;   for i=1,15 do begin

;;   !x.margin=[1,1]
;;   !y.margin=[0.5,0.5]
;;      print,i
;;      w=ntostr(i)
;;      im=mrdfits('sn1987a'+w+'_300-8000_cr.fits',/silent)
;;      deproject_image,im,newim
;;      ring=mrdfits('ring/new_sn1987a'+w+'_fitim.fits',1,/silent)
;;      pt=mrdfits('pointsrc4_nolim/new_sn1987a'+w+'_fitim.fits',1,/silent)
;;      tang=mrdfits('newmod4_lim/new_sn1987a'+w+'_fitim.fits',1,/silent)
;;      !p.multi=[0,5,1]
;;      rdis,im,title='Original image',xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent
;;      rdis,newim,title='De-projected image',xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent
;;      rdis,ring,title='Ring model fit',xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent
;;      rdis,pt,title='Point source model fit',xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent
;;      rdis,tang,title='Lobes model fit',xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,/silent
;;      !p.multi=0
;;      k=get_kbrd(10)
;;   endfor 

  
  return
end 
     
