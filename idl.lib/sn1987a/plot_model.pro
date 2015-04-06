pro plot_model,ps=ps,compare=compare
  
  !x.margin=[0.5,0.5]
  !y.margin=[2,2]
  if keyword_set(ps) then begplot,name='composite_images.ps'
  
  files=['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits']
  fpars=mrdfits('sn1987a_fpars.fits',1)
  
  for i=0,14 do begin
     
     f=fpars[i]
     a=[f.a0,f.a1,f.x0,f.y0,f.sigx,f.sigy,f.b1,f.r0,f.theta0,f.sigr,f.sigt,f.c0,f.c1,f.phi0,f.counts,f.mu0,f.sigt2,f.b2]
     sn1987a_model,x,a
;     im=mrdfits(files[i],0)
;     rdis,im,low=min(im),high=max(im)
     print,f.chisq,f.counts
     
;     if not keyword_set(ps) then k=get_kbrd(10)
     
  endfor
  
  if keyword_set(ps) then endplot
  
  if keyword_set(compare) then begin 
     !x.margin=[0.5,0.5]
     !y.margin=[0.5,0.5]
     
     if keyword_set(ps) then begplot,name='compare_model_im.ps'
     !p.multi=[0,6,8]
     for i=0,5 do plot,[0,1],[0,0.2],/nodata,/iso,color=!white
     
     sn1987a_age,ages,date=date
     month=month_cnv(date[1,*],/short)
     dates=month+'-'+ntostr(date[2,*])+'-'+ntostr(date[0,*])
     
     for i=0,13 do begin 
;        im=mrdfits(files[i],0)
        im=mrdfits('sn1987a'+ntostr(i+1)+'_fitim.fits',0)
        fim=mrdfits('sn1987a'+ntostr(i+1)+'_fitim.fits',1)
        plot,[0,1],[0,1],/iso,/nodata,color=!white,xticks=1,xtickname=[' ',' ',' '],yticks=1,ytickname=[' ',' ',' ']
        legend,[dates[i]],box=0,charsize=0.9,/top,/left

        if i le 1 then title='Real Image' else title=''
        rdis,im,title=title,xticks=1,xtickname=[' ',' ',' '],yticks=1,ytickname=[' ',' ',' ']
        if i le 1 then title='Modeled Image' else title=''
        rdis,fim,title=title,xticks=1,xtickname=[' ',' ',' '],yticks=1,ytickname=[' ',' ',' ']
        
     endfor 

     if keyword_set(ps) then endplot
  endif 
  
  return
end 
