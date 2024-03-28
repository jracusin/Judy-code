pro plot_st_trends

  begplot,name='~/Fermi/Spacecraft/star_tracker/hot_pix_trends.ps',/color,font='helvetica',/land
  !p.multi=[0,3,2]
  !x.margin=[10,3]
  !y.margin=[10,2]
  colors=[!red,!green,!blue,!cyan,!magenta,!orange,!purple,!pink,!forestgreen,!salmon,!dodgerblue,!violet,!darkred]
  colors=[colors,colors,colors]

  l1=mrdfits('~/Fermi/Spacecraft/star_tracker/ST1_warm_pixel_list.fits',1)  
  l3=mrdfits('~/Fermi/Spacecraft/star_tracker/ST3_warm_pixel_list.fits',1)  

  n1=n_elements(l1[0].date)
  min1=fltarr(n1)
  max1=fltarr(n1)
  mean1=fltarr(n1)
  for i=0,n1-1 do begin
    min1[i]=min(l1.loc_bkg[i])
    max1[i]=max(l1.loc_bkg[i])
    mean1[i]=median(l1.loc_bkg[i])
  endfor 

  n3=n_elements(l3[0].date)
  min3=fltarr(n3)
  max3=fltarr(n3)
  mean3=fltarr(n3)
  for i=0,n3-1 do begin
    min3[i]=min(l3.loc_bkg[i])
    max3[i]=max(l3.loc_bkg[i])
    mean3[i]=median(l3.loc_bkg[i])
  endfor

  !p.multi=0
  plot,[2013,round(l1[0].date[i]+0.5)],[0,70],/nodata,xtitle='Date',ytitle='Local Background Level',/xsty,/ysty
  for i=0,n1-1 do oplot,[l1[0].date[i],l1[0].date[i]],[min1[i],max1[i]],color=!blue
  oplot,l1[0].date,mean1,color=!blue,psym=1
  oplot,minmax(l1.date),[median(l1.loc_bkg),median(l1.loc_bkg)],color=!blue,line=2

  for i=0,n3-1 do oplot,[l3[0].date[i],l3[0].date[i]],[min3[i],max3[i]],color=!red
  oplot,l3[0].date,mean3,color=!red,psym=1
  oplot,minmax(l3.date),[median(l3.loc_bkg),median(l3.loc_bkg)],color=!red,line=2

  legend,['ST1','ST3'],psym=[1,1],color=[!blue,!red],textcolor=[!blue,!red]

  endplot
  ps2pdf,'~/Fermi/Spacecraft/star_tracker/hot_pix_trends.ps

  return
  end 

pro plot_st_results

  begplot,name='~/Fermi/Spacecraft/star_tracker/hot_pix_analysis.ps',/color,font='helvetica',/land
  !p.multi=[0,3,2]
  !x.margin=[10,3]
  !y.margin=[10,2]
  colors=[!red,!green,!blue,!cyan,!magenta,!orange,!purple,!pink,!forestgreen,!salmon,!dodgerblue,!violet,!darkred]
  colors=[colors,colors,colors]

  plotsym,8,0.3,/fill
  for st=1,3,2 do begin ;;; loop over 3 star trackers
     imfiles=file_search('~/Fermi/Spacecraft/star_tracker/STimages/*ST'+ntostr(st)+'*CCD*dump*.fits*')
     wgood=where(strpos(imfiles,'bad') eq -1)
     color=colors[wgood]

     pixf=mrdfits('ST'+ntostr(st)+'_warm_pixel_list.fits',1,/silent)
     np=n_elements(pixf)
     ni=n_elements(pixf[0].comp)
     ;;; plot positions of suspect pixels
     plot,[0,512],[0,512],psym=1,xrange=[0,511],yrange=[0,511],/xsty,/ysty,xtitle='x',ytitle='y',/iso,title='ST #'+ntostr(st),/nodata

     for i=0,ni-1 do begin 
;        plot,pixf.x,pixf.y,psym=1,xrange=[0,511],yrange=[0,511],/xsty,/ysty,xtitle='x',ytitle='y',/iso,title='ST'+ntostr(st)
        w8=where(pixf.comp[i] ge 8,nw8)
        if nw8 gt 1 then oplot,pixf[w8].x,pixf[w8].y,psym=8,color=color[i]
     endfor 


     n8=intarr(ni)
     ;;; plot histogram of pixel values for suspect pixels
     plot,[0,50],[0,30],/nodata,xtitle='Counts above bkg',title='ST #'+ntostr(st),ytitle='# pixels > bkg'
     for i=0,ni-1 do begin 
        plothist2,pixf.comp[i],xhist,yhist,/over,color=color[i],bin=1
        w8=where(pixf.comp[i] ge 8,nw8)
        n8[i]=nw8
     endfor 
     oplot,[8,8],[0,100],line=2
     legend,strmid(pixf[0].wim,9,15),textcolor=color[0:ni-1],box=0,/top,/right,charsize=0.6

     ;;; plot # of pixels >8+bkg with im
     if st eq 1 then xrange3=[2013,round(max(pixf.date)+0.5)]
;     plot,[0,ni+1],yrange=[0,max(n8)],xtitle='IM #',ytitle='# pixels > 8+bkg',xrange=xrange3,/nodata,title='ST #'+ntostr(st),/xsty
;     for i=0,ni-1 do plots,i+1,n8[i],psym=5,color=color[i]
;     oplot,indgen(ni)+1,n8,line=2
     plot,xrange3,yrange=[0,max(n8)],xtitle='Image Year',ytitle='# pixels > 8+bkg',xrange=xrange3,/nodata,title='ST #'+ntostr(st)
     for i=0,ni-1 do plots,pixf.date[i],n8[i],psym=5,color=color[i]
     oplot,pixf.date,n8,line=2
     print,n8
     print,'Pixels currently warm (x,y,val above background,# times warm): '+ntostr(n_elements(w8))
     ncomp=n_elements(pixf[w8[0]].comp)
     for i=0,n_elements(w8)-1 do print,pixf[w8[i]].x,pixf[w8[i]].y,pixf[w8[i]].comp[ncomp-1],n_elements(where(pixf[w8[i]].comp gt 8))
  endfor 

  print,'POSSIBLE PIXEL CLUSTERS'
  for st=1,3,2 do begin ;;; loop over 3 star trackers
     print,'ST#',st
     pixf=mrdfits('ST'+ntostr(st)+'_warm_pixel_list.fits',1,/silent)
     np=n_elements(pixf)

     for p=0,np-1 do begin
        dist=sqrt((pixf.x*1.-pixf[p].x)^2+(pixf.y*1.-pixf[p].y)^2)
        w=where(dist lt 5,nw)
        if nw gt 1 then print,pixf[p].x,pixf[p].y,dist[w]
     endfor 
  endfor 
  
  !p.multi=0
;  if not keyword_set(ps) then begin 
;     k=get_kbrd(10)
;     if k eq 's' then stop
;  endif 

  thresh=8.
  val=70.
  plotsym,0,1
  !x.margin=[1,0]
  !y.margin=[2,2]
  ;;; loop through each pixel
  for st=1,3,2 do begin ;;; loop over 3 star trackers
     print,'Star Tracker #',st
     pixf=mrdfits('~/Fermi/Spacecraft/star_tracker/ST'+ntostr(st)+'_warm_pixel_list.fits',1,/silent)
;     np=n_elements(pixf)
     wp=where(pixf.nwarm gt 4,np) 
     pixf=pixf[wp]
;;; plot only those pixels which have been hot >4 times
     ni=n_elements(pixf[0].comp)
;     !p.multi=[0,ni+2,4]
;     !p.multi=[0,round(ni/2+1),4]
      !p.multi=[0,round(ni/3+1),3]
;     multiplot,[ni+1,4],/init

     ims=intarr(512,512,ni)
     for i=0,ni-1 do begin

        im=mrdfits(strtrim(pixf[0].wim[i],2),/silent)
        sim=size(im)
        if sim[0] eq 3 then im=im[*,*,0]
        if st eq 1 and i le 1 then im=rotate(im,7)
        ims[*,*,i]=im
     endfor 
     for p=0,np-1 do begin
        xmn=pixf[p].x-10>0
        ymn=pixf[p].y-10>0
        xmx=pixf[p].x+10<511
        ymx=pixf[p].y+10<511
        
        for i=0,ni-1 do begin
;           multiplot
           if i eq 0 then begin 
              xtitle='x'
              ytitle='y'
              ytickname=''
           endif else begin
;              xtitle=''
              ytitle=''
              ytickname=replicate(' ',5)
           endelse 
           rdis,ims[*,*,i],low=0,high=val,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,charsize=1,/silent,xtitle=xtitle,ytitle=ytitle,ytickname=ytickname        ;'IM'+ntostr(i)+': '+ntostr(pixf[p].x)+', '+ntostr(pixf[p].y)
;           oplot,pixf.x,pixf.y,psym=8,color=color,symsize=2
           h=where(pixf.comp[i] ge thresh and pixf.x ge xmn and pixf.x le xmx and pixf.y ge ymn and pixf.y le ymx,nh)
           w=where(pixf.comp[i] lt thresh and pixf.x ge xmn and pixf.x le xmx and pixf.y ge ymn and pixf.y le ymx,nw)
           if nh gt 0 then plots,pixf[h].x,pixf[h].y,psym=8,color=!red,symsize=2
           if nw gt 0 then plots,pixf[w].x,pixf[w].y,psym=8,color=!green,symsize=2
           xyouts,xmn+3,ymx+1,strmid(pixf[p].wim[i],9,15),charsize=0.5
           xyouts,xmn+1,ymn+5,'pix val='+ntostr(pixf[p].val[i]),charsize=0.5
           xyouts,xmn+1,ymn+3,'loc bkg='+ntostr(sigfig(pixf[p].loc_bkg[i])),charsize=0.5
           xyouts,xmn+1,ymn+1,'>bkg='+ntostr(sigfig(pixf[p].comp[i],3)),charsize=0.5
           legend,ntostr(pixf[p].x)+', '+ntostr(pixf[p].y),/top,/left,box=0,charsize=0.5
        endfor
        ;;; make plot of pixel value as function of time
;        multiplot
        plot,indgen(10),xtickname=replicate(' ',10),ytickname=replicate(' ',10),/nodata,color=!white
        if ni mod 2 eq 1 then plot,indgen(10),xtickname=replicate(' ',10),ytickname=replicate(' ',10),/nodata,color=!white
;        plot,indgen(ni)+1,pixf[p].comp,psym=5,xrange=[0,ni+1],yrange=[0,max(pixf[p].comp)+5],charsize=1,ymargin=[4,4],symsize=0.5,xtitle='IM #',ytitle='>bkg',xmargin=[-20,0]
        plot,pixf[p].date,pixf[p].comp,psym=5,xrange=xrange3,yrange=[0,max(pixf[p].comp)+5],charsize=1,ymargin=[4,4],symsize=0.5,xtitle='Image Year',ytitle='>bkg',xmargin=[-15,0]
;        oplot,[0,ni+1],[8,8],color=!orange,line=1
        oplot,xrange3,[8,8],color=!orange,line=1


;        if not keyword_set(ps) then begin 
;           k=get_kbrd(10)
;           if k eq 's' then stop
;        endif 
     endfor 
;     multiplot,/reset,/default
  endfor
  
  !p.multi=0

;  if keyword_set(ps) then begin 
     endplot
     spawn,'ps2pdf ~/Fermi/Spacecraft/star_tracker/hot_pix_analysis.ps ~/Fermi/Spacecraft/star_tracker/hot_pix_analysis.pdf'
;  endif 
stop
return
end 

pro star_tracker_analysis

  dir='~/Fermi/Spacecraft/star_tracker/'
  cd,dir
  pix0=create_struct('ST',0,'date',0.,'wim','','x',0,'y',0,'val',0,'sig',0.,$
                     'comp',0.,'ccd_bkg',0.,'loc_bkg',0.)
  pix=replicate(pix0,1000)
  thresh=8.     ;;; pixels above background
  starthresh=70 ;;; above this value are only stars
  xab0=indgen(17)-8 & xab=xab0
  for i=0,15 do xab=[xab,xab0]
  yab=0
  for i=0,16 do yab=[yab,replicate(i-8,17)]
  yab=yab[1:*]

  x=intarr(512,512)
  y=x
  for i=0,511 do begin
     x[i,*]=i
     y[*,i]=i
  endfor 

  for st=1,3 do begin ;;; loop over 3 star trackers

     imfiles=file_search('STimages/*ST'+ntostr(st)+'*CCD*dump*.fits')
     date=fltarr(n_elements(imfiles))
     nim=n_elements(imfiles)
     ims=intarr(512,512,nim)  ;;; original images
     ims2=ims                 ;;; images after stars subtracted
     sigs=fltarr(512,512,nim) ;;; sigma above background
     pix=replicate(pix0,5000) ;;; new pix structure for each ST

     if imfiles[0] ne '' then begin 
        for i=0,nim-1 do begin ;;; loop over image dumps from each ST

           im=mrdfits(imfiles[i],0,hdr,/silent)
           d=strsplit(imfiles[i],'/-',/ex)
           date[i]=2000.+d[1]+d[2]/365.

           sim=size(im)
           if sim[0] eq 3 then im=im[*,*,0]
           if st eq 1 and i le 1 then im=rotate(im,7)

           ims[*,*,i]=im
           
           w=where(im gt starthresh,nw) 

           for j=0,nw-1 do begin  ;;; remove 3x3 box around each pixel above threshold
              xa=[-1,0,1,-1,0,1,-1,0,1]
              ya=[-1,-1,-1,0,0,0,1,1,1]
              xw=x[w[j]]
              yw=y[w[j]]
              im[xw+xa,yw+ya]=0
           endfor 

           ims2[*,*,i]=im ;;; save new image with bright stars removed

           q=where(im ne 0) ;;; not stars
           m=mean(im[q])    ;;; mean background over whole detector
           sig=(im-m)/sqrt(im)
           sigs[*,*,i]=sig
           z=where(im[q] gt m+thresh-1,nz) ;;; reduce thresh compared to whole CCD background

        ;;; then compare 16x16 box for local background - really 17x17
           for j=0,nz-1 do begin 
              xw=x[q[z[j]]]
              yw=y[q[z[j]]]
              bpix=ims[xw+xab,yw+yab,i]
              wb0=where(bpix ne 0 and xw+xab ge 0 and xw+xab lt 512 and yw+yab ge 0 and yw+yab lt 512)
              mloc=mean(bpix[wb0])
              if im[q[z[j]]] ge mloc+thresh then begin 
                 n0=where(pix.x eq 0) ;;; add to bad pixel list as we go through images for each ST
                 n0=n0[0]
                 pix[n0].x=x[q[z[j]]]
                 pix[n0].y=y[q[z[j]]]
                 pix[n0].val=im[q[z[j]]]
                 pix[n0].sig=sig[q[z[j]]]
                 pix[n0].comp=im[q[z[j]]]-mloc
                 pix[n0].wim=imfiles[i]
                 pix[n0].ccd_bkg=m
                 pix[n0].loc_bkg=mloc
              endif 
           endfor ;;; loop over hot pixel candidates
        endfor  ;;; loop over each image
        w0=where(pix.x ne 0)
        pix=pix[w0]

        ;;; combine list of warm pixels to collect values for each
        ;;; non-duplicate
        pixf=create_struct('ST',0,'date',fltarr(nim),'wim',strarr(nim),'x',0,'y',0,$
                           'val',intarr(nim),'sig',fltarr(nim),$
                           'comp',fltarr(nim),'ccd_bkg',fltarr(nim),'loc_bkg',fltarr(nim),$
                           'nwarm',0)
        xstr=ntostr(pix.x)
        wstr=where(pix.x lt 100)
        xstr[wstr]='0'+xstr[wstr]
        wstr=where(pix.x lt 10)
        xstr[wstr]='0'+xstr[wstr]
        ystr=ntostr(pix.y)
        wstr=where(pix.y lt 100)
        ystr[wstr]='0'+ystr[wstr]
        wstr=where(pix.y lt 10)
        ystr[wstr]='0'+ystr[wstr]

        s=rem_dup(xstr+ystr)
        ns=n_elements(s)
        pixf=replicate(pixf,ns)
        mloc=fltarr(nim)

        for i=0,ns-1 do begin ;;; loop over each warm pixel candidate
           ws=where(pix.x eq pix[s[i]].x and pix.y eq pix[s[i]].y,nws)
           if nws gt 2 then begin ;;; warm pixel must be warm >2 times 
                ;; changed from 1->2 on 1/14/14
              pixf[i].x=pix[ws[0]].x
              pixf[i].y=pix[ws[0]].y
              pixf[i].val=ims[pixf[i].x,pixf[i].y,*]
              pixf[i].sig=sigs[pixf[i].x,pixf[i].y,*]
              xw=pix[ws[0]].x
              yw=pix[ws[0]].y
              for j=0,nim-1 do begin
                 bpix=ims2[xw+xab,yw+yab,j]
                 wb0=where(bpix ne 0 and xw+xab ge 0 and xw+xab lt 512 and yw+yab ge 0 and yw+yab lt 512 and xab ne 0 and yab ne 0)
                 mloc[j]=mean(bpix[wb0])
              endfor 
              pixf[i].comp=pixf[i].val-mloc
              pixf[i].ccd_bkg=pix[ws[0]].ccd_bkg
              pixf[i].loc_bkg=mloc
              pixf[i].nwarm=nws
              pixf[i].st=st
              pixf[i].wim=imfiles
              pixf[i].date=date
           endif 
        endfor  ;;; loop over each warm pixel candidate
        wn0=where(pixf.x ne 0)
        pixf=pixf[wn0]
        mwrfits,pixf,'ST'+ntostr(st)+'_warm_pixel_list.fits',/create
        help,pixf
     endif
  endfor ;;; loop over each star tracker

  return
end 
