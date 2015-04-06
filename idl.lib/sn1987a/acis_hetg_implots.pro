pro acis_hetg_implots
  
  cd,'~/sn1987a/image_analysis'
  
  im20a=mrdfits('sn1987a20A_300-8000_cr_psu.fits')
  im20h=mrdfits('sn1987a20H_300-8000_cr_psu.fits')
  im21a=mrdfits('sn1987a21A_300-8000_cr_psu.fits')
  im21h=mrdfits('sn1987a21H_300-8000_cr_psu.fits')
  
  begplot,name='acis_hetg_images.ps'
  !x.margin=[1,1]
  !y.margin=[0.5,0.5]
  !p.multi=[0,2,2]
  rdis,im20a,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='ACIS S3 - Obs 20'
  rdis,im20h,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='HETG - Obs 20'
  rdis,im21a,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='ACIS S3 - Obs 21'
  rdis,im21h,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='HETG - Obs 21'

  endplot

  n=5
  begplot,name='acis_hetg_contours.ps'
  contour,im20a,nlevel=n,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='ACIS S3 - Obs 20',/iso
  contour,im20h,nlevel=n,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='HETG - Obs 20',/iso
  contour,im21a,nlevel=n,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='ACIS S3 - Obs 21',/iso
  contour,im21h,nlevel=n,xtickname=[' ',' '],ytickname=[' ',' '],xticks=1,yticks=1,title='HETG - Obs 21',/iso

  endplot
  !p.multi=0  
  stop
  return
end 
