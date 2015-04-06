pro new_sn1987a_image_analysis, files, counts, lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat,all=all

  nf=n_elements(files)

  for k=0,nf-1 do begin 
     file=files[k]
     count=counts[k]

     if keyword_set(all) then n=3 else n=0
     for i=0,n do begin 
        if keyword_set(all) then begin
           ring=0 & lobes=0 & ptsrc=0 & bilat=0
           case i of
              0: ring=1
              1: lobes=1
              2: ptsrc=1
              3: bilat=1
           endcase
        endif

        if keyword_set(ring) then mo='ring'
        if keyword_set(lobes) then mo='lobes'
        if keyword_set(ptsrc) then mo='ptsrc'
        if keyword_set(bilat) then mo='bilat'
        add='_'+mo
        sdir=mo+'/'

        if not exist(sdir) then spawn,'mkdir '+sdir
        nn=1
        fpars=create_struct('a0',0d,'a0err',0d,'a1',0d,'a1err',0d,$
                            'x0',0d,'x0err',0d,'y0',0d,'y0err',0d,$
                            'sigr',0d,'sigrerr',dblarr(2),'r0',0d,'r0err',dblarr(2),$
                            's0',0d,'s0err',0d,'theta0',0d,'theta0err',0d,$
                            'sigt0',0d,'sigt0err',0d,'sigr0',0d,'sigr0err',0d,$
                            's1',0d,'s1err',0d,'theta1',0d,'theta1err',0d,$
                            'sigt1',0d,'sigt1err',0d,'sigr1',0d,'sigr1err',0d,$
                            's2',0d,'s2err',0d,'theta2',0d,'theta2err',0d,$
                            'sigt2',0d,'sigt2err',0d,'sigr2',0d,'sigr2err',0d,$
                            's3',0d,'s3err',0d,'theta3',0d,'theta3err',0d,$
                            'sigt3',0d,'sigt3err',0d,'sigr3',0d,'sigr3err',0d,$
                            'counts',0d,'iter',0,'chisq',0d,'dof',0d)
        
        npars=n_tags(fpars)
        !p.multi=[0,1,3]
        
        im=mrdfits(file)
        im=im-min(im)
        im=im/max(im) ;;;LOSE FLUX INFO
        im=im/total(im)*count
        deproject_image,im,newim
        
        print,total(im),total(newim)
        
        new_sn1987a_fit,newim,a,sigma,chisq,dof,iter,fitim,perror,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat
        rdis,im
        rdis,newim
        rdis,fitim

        for j=0,npars-3,2 do fpars.(j)=a[j/2]
        
        for j=1,npars-2,2 do fpars.(j)=sigma[(j-1)/2]
        r0=a[5]
        r0err=perror[*,5]
        fpars.r0err=r0err
        fpars.sigrerr=perror[*,4]
        fpars.iter=iter
        fpars.chisq=chisq
        fpars.dof=dof

;  ofile='new_sn1987a'+add+'_fitim.fits'
        root=str_sep(file,'.fits')
        ofile=root[0]+add+'_fitim.fits'
        mwrfits,newim,sdir+ofile,/create
        mwrfits,fitim,sdir+ofile
        diff=newim-fitim
        mwrfits,diff,sdir+ofile

        fparfile=root[0]+add+'_fpars.fits'
        mwrfits,fpars,sdir+fparfile,/create
        print,r0,r0err
        
        !p.multi=0
     endfor 
  endfor 
  return
end

pro new_sn1987a_image_analysis_standard,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat,all=all,hardsoft=hardsoft
  
  if not keyword_set(lobes) and not keyword_set(ring) and not keyword_set(ptsrc) and not keyword_set(bilat) and not keyword_set(all) and not keyword_set(hardsoft) then begin 
     print,'Need to specify model (lobes, ring, ptsrc, bilat, all, hardsoft)'
     return
  endif 
  
  if keyword_set(all) then doall=3 else doall=0
  for aa=0,doall do begin
     if keyword_set(all) then begin 
        ring=0 & ptsrc=0 & bilat=0 & lobes=0
        case aa of
           0: ring=1
           1: ptsrc=1
           2: bilat=1
           3: lobes=1
        endcase 
     endif 
     
     if keyword_set(ring) then sdir='ring/'
     if keyword_set(ptsrc) then sdir='ptsrc/'
     if keyword_set(lobes) then sdir='lobes/'
     if keyword_set(bilat) then sdir='bilat/'
;     if keyword_set(hardsoft) then sdir='hardsoft/'+sdir
     if not exist(sdir) then spawn,'mkdir '+sdir

     if not keyword_set(hardsoft) then begin 
        n=ntostr([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21])
        n=[n,'22a','22b','23a','23b','23c','24']
        
;  dir='~jracusin/data/sn1987a/image_analysis/'
;  dir='/bulk/shadow/racusin/sn1987a/image_analysis/'
;     dir='/Volumes/Firewire1/racusin/sn1987a/image_analysis/'
;        dir=!data+'/sn1987a/image_analysis/'
        dir='~/SNR87A/image_analysis/'
        cd,dir
        files=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr.fits','sn1987a21A_300-8000_cr.fits','sn1987a22a_300-8000_cr.fits','sn1987a22b_300-8000_cr.fits','sn1987a23a_300-8000_cr.fits','sn1987a23b_300-8000_cr.fits','sn1987a23c_300-8000_cr.fits','sn1987a24_300-8000_cr.fits']
     endif else begin 
        n=['16m','16h','16s','17m','17h','17s','19m','19h','19s','20m','20h','20s']
        dir='/Volumes/Firewire1/racusin/sn1987a/hardsoft_images/'
        cd,dir
        files=dir+['sn1987a16_1500-8000.fits','sn1987a16_2000-8000.fits','sn1987a16_300-800.fits','sn1987a17_1500-8000.fits','sn1987a17_2000-8000.fits','sn1987a17_300-800.fits','sn1987a19_1500-8000.fits','sn1987a19_2000-8000.fits','sn1987a19_300-800.fits','sn1987a20A_1500-8000_psu.fits','sn1987a20A_2000-8000_psu.fits','sn1987a20A_300-800_psu.fits']
;        bilat=1
     endelse

;     files=dir+['sn1987a1_300-8000_cr.fits','sn1987a2_300-8000_cr.fits','sn1987a3_300-8000_cr.fits','sn1987a4_300-8000_cr.fits','sn1987a5_300-8000_cr.fits','sn1987a6_300-8000_cr.fits','sn1987a7_300-8000_cr.fits','sn1987a8_300-8000_cr.fits','sn1987a9_300-8000_cr.fits','sn1987a10_300-8000_cr.fits','sn1987a11_300-8000_cr.fits','sn1987a12_300-8000_cr.fits','sn1987a13_300-8000_cr.fits','sn1987a14_300-8000_cr.fits','sn1987a15_300-8000_cr.fits','sn1987a16_300-8000_cr.fits','sn1987a17_300-8000_cr.fits','sn1987a19_300-8000_cr.fits','sn1987a20A_300-8000_cr.fits','sn1987a20A_300-8000_cr_psu.fits','sn1987a20H_300-8000_cr.fits','sn1987a20H_300-8000_cr_psu.fits','sn1987a21A_300-8000_cr.fits','sn1987a21A_300-8000_cr_psu.fits','sn1987a21H_300-8000_cr.fits','sn1987a21H_300-8000_cr_psu.fits']
     nn=n_elements(files)
     
     fpars=create_struct('a0',0d,'a0err',0d,'a1',0d,'a1err',0d,$
                         'x0',0d,'x0err',0d,'y0',0d,'y0err',0d,$
                         'sigr',0d,'sigrerr',dblarr(2),'r0',0d,'r0err',dblarr(2),$
                         's0',0d,'s0err',0d,'theta0',0d,'theta0err',0d,$
                         'sigt0',0d,'sigt0err',0d,'sigr0',0d,'sigr0err',0d,$
                         's1',0d,'s1err',0d,'theta1',0d,'theta1err',0d,$
                         'sigt1',0d,'sigt1err',0d,'sigr1',0d,'sigr1err',0d,$
                         's2',0d,'s2err',0d,'theta2',0d,'theta2err',0d,$
                         'sigt2',0d,'sigt2err',0d,'sigr2',0d,'sigr2err',0d,$
                         's3',0d,'s3err',0d,'theta3',0d,'theta3err',0d,$
                         'sigt3',0d,'sigt3err',0d,'sigr3',0d,'sigr3err',0d,$
                         'counts',0d,'iter',0,'chisq',0d,'dof',0d)
     
     counts=[690,607,9030,1800,6226,6427,9277,9668,11856,17979,16557,24939,27048,30940,30870,32798,27945,12008,12119,9204,3537,4970,2663,3103,3566,8361]*1d
     if keyword_set(hardsoft) then counts=[5841,2410,8148,5258,2170,6429,2110,828,2739,2247,916,2721]*1d

     npars=n_tags(fpars)
     fpars=replicate(fpars,nn)
     r0=dblarr(nn) & r0err=dblarr(2,nn)
     !p.multi=[0,4,3]
     g=0
     print,'set g'
     colprint,indgen(nn),n,files
     stop
     if g ne 0 then begin 
        fpars0=mrdfits(sdir+'new_sn1987a_fpars.fits',1)
        struct_assign,fpars0,fpars
     endif 
        
     for i=g,nn-1 do begin
        print,i
        im=mrdfits(files[i])
        im=im-min(im)
        im=im/max(im) ;;;LOSE FLUX INFO
        im=im/total(im)*counts[i]
        deproject_image,im,newim
        
        print,total(im),total(newim)
;     sn1987a_fit,newim,a,fitim,chisq,iter,sigma,r0errs,i,stack=stack

        new_sn1987a_fit,newim,a,sigma,chisq,dof,iter,fitim,perror,lobes=lobes,ring=ring,ptsrc=ptsrc,bilat=bilat
;     !p.multi=0
        rdis,im
        rdis,newim
        rdis,fitim
        
;     for j=0,n_elements(a)-1 do fpars[i].(j)=a[j]
        for j=0,npars-3,2 do fpars[i].(j)=a[j/2]
        
;     dof=200.^2-22.
;     csigma=sigma*sqrt(chisq/dof)

        for j=1,npars-2,2 do fpars[i].(j)=sigma[(j-1)/2]
        r0[i]=a[5]
        r0err[*,i]=perror[*,5]
        fpars[i].r0err=r0err[*,i]
        fpars[i].sigrerr=perror[*,4]
        fpars[i].iter=iter
        fpars[i].chisq=chisq
        fpars[i].dof=dof
        
        ofile='new_sn1987a'+ntostr(n[i])+'_fitim.fits'
        mwrfits,newim,sdir+ofile,/create
        mwrfits,fitim,sdir+ofile
        diff=newim-fitim
        mwrfits,diff,sdir+ofile
        
        mwrfits,fpars,sdir+'new_sn1987a_fpars.fits',/create
;stop
     endfor 
     print,r0,r0err
     
     !p.multi=0
  endfor 
  return
end 
