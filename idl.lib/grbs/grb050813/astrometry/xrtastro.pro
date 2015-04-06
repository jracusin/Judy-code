
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function xrtpsfr, r
  
  ;; PSF parameters
  gfrac=0.584d
  gsig =2.823d
  lsig =3.645d
  
  ;; PSF from my fits to the bright source observation
  psf = gfrac*gaussian(r,sig=gsig) + $
        (1d - gfrac)*lorentzian(r,sig=lsig)
  
  ;; Normalize to r=20pix
  psf = psf/11.056061d
  
  ;; Zero out larger radii
  if somewhere(r gt 20) then begin
     psf[where(r gt 20)]=0d
  endif
  
  return, psf
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function wcentroid, xyin, xin, yin, tin, $
    niter=_niter, alpha=_alpha, dbt=_dbt, $
    itx=itx, ity=ity, doplot=doplot

  if n_elements(_niter) gt 0 then niter=_niter else niter=5
  if n_elements(_alpha) gt 0 then alpha=_alpha else alpha=-1.3
  if n_elements(_dbt) gt 0   then dbt=_dbt     else dbt=125.0
  
  xyout=xyin
  itx=[xyin[0]]
  ity=[xyin[1]]
  
  for i=0,niter-1 do begin $
     wcr=sqrt((xin-xyout[0])^2+(yin-xyout[1])^2) & $
     wts=(dbt+tin)^alpha*xrtpsfr(wcr) & $
     ;; weighted_mean takes uncertainties, not weights
     xyout[0]=weighted_mean(xin,1/sqrt(wts)) & $
     xyout[1]=weighted_mean(yin,1/sqrt(wts)) & $
     push,itx,xyout[0] & $
     push,ity,xyout[1]
  endfor

  if keyword_set(doplot) then begin
     c=define_colors()
     ;; make plot
     set_isoxy,min(xin),max(xin),min(yin),max(yin)
     npts=n_elements(xin)
     plot,xin+0.5*randomu(seed,npts),yin+0.5*randomu(seed,npts),psym=4
     oplot,[xyin[0]],[xyin[1]],psym=4,color=c.red,thick=3
     oplot,itx,ity,psym=-4,color=c.blue
     ;; cleanup
     set_isoxy,0
     if !d.name eq 'X' then wait,0.5
  endif
  
  return, xyout
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function wcentroidu, xyin, xin, yin, tin, $
         cl=_cl, nmc=_nmc, niter=_niter, alpha=_alpha, dbt=_dbt, $
         mcx=mcx, mcy=mcy, seed=seed, doplot=doplot
  
  if n_elements(_cl) gt 0    then cl=_cl       else cl=0.90
  if n_elements(_nmc) gt 0   then nmc=_nmc     else nmc=1000
  if n_elements(_niter) gt 0 then niter=_niter else niter=5
  if n_elements(_alpha) gt 0 then alpha=_alpha else alpha=-1.3
  if n_elements(_dbt) gt 0   then dbt=_dbt     else dbt=125.0
  
  x0=xyin[0]
  y0=xyin[1]
  nevt=n_elements(xin)
  
  ;; Perform the simulations
  mcx=fltarr(nmc) & mcy=fltarr(nmc)
  
  for i=0l,nmc-1 do begin $
     iix=long(randomu(seed,nevt)*nevt) & $
     xyout=wcentroid(xyin,xin[iix],yin[iix],tin[iix], $
                     niter=niter,alpha=alpha,dbt=dbt)
     mcx[i]=xyout[0] & $
     mcy[i]=xyout[1]
  endfor
  
  ;; Find the radius of appropriate confidence level
  mcdist=sqrt((mcx-x0)^2 + (mcy-y0)^2)
  ixs=sort(mcdist)
  ixcrit=cl*nmc
  runc=mcdist[ixs[1+long(ixcrit)]]
  
  if keyword_set(doplot) then begin
     c=define_colors()
     cirx=cos(2*!pi*findgen(25)/24.0)
     ciry=sin(2*!pi*findgen(25)/24.0)
     ;; make plot
     set_isoxy,x0-2*runc,x0+2*runc,y0-2*runc,y0+2*runc
     plot,mcx,mcy,psym=4
     oplot,[xyin[0]],[xyin[1]],psym=4,color=c.red,thick=3
     oplot,x0+runc*cirx,y0+runc*ciry,color=c.blue
     ;; cleanup
     set_isoxy,0
     if !d.name eq 'X' then wait,0.5
  endif
  
  return, runc
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XRT pixel size in arcsec
xrtpix=2.357

;; Read in data
;file='grb_im.fits'
;file='grb_im2.fits'

file='grb_im_cl.fits'
fgrb='xrt000.img'
if n_elements(xd) eq 0 then begin
   xd=readfits(file,xh)
   gd=readfits(fgrb,gh)
   common onepar, opf,opx,opy,opu
endif

;; Original afterglow coordinates from Judy
x0=501.79 & y0=485.44

;; Original serendipitous source coordinates from Judy
;; (celldetect from ximage)

xpx=418.00 & xpy=276.48
push,xpx,370.55 & push,xpy,355.97
push,xpx,502.35 & push,xpy,298.81
push,xpx,388.30 & push,xpy,539.98
push,xpx,460.58 & push,xpy,528.61
push,xpx,442.00 & push,xpy,565.54
push,xpx,583.57 & push,xpy,482.76
push,xpx,503.07 & push,xpy,718.77

;; Number of tie objects
ntie=n_elements(xpx)

;; Convert to image coordinates, IDL style
if n_elements(tiexf) ne ntie then begin
   ;; first iteration (start from judy's positions)
   tiex=xpx-244.0-1.0
   tiey=xpy-244.0-1.0
endif else begin
   ;; second & subsequent iterations
   tiex=tiexf
   tiey=tieyf
endelse

;; Size of the cutout-box
hbox=12

;; Make plots?
doplot=1

;; Output array variables
tiexf=fltarr(ntie)
tieyf=fltarr(ntie)
tieru=fltarr(ntie)

for itie=0,ntie-1 do begin
   ;; Construct the "event list"
   evx=[0.0] & evy=[0.0] & evt=[0.0]
   for xix=tiex[itie]-hbox,tiex[itie]+hbox do begin
      for yix=tiey[itie]-hbox,tiey[itie]+hbox do begin
         if xd[xix,yix] gt 0 then begin
            evx=[evx,replicate(xix,xd[xix,yix])]
            evy=[evy,replicate(yix,xd[xix,yix])]
            evt=[evt,replicate(1.0,xd[xix,yix])]
         endif
      endfor
   endfor
   evx=evx[1:*] & evy=evy[1:*] & evt=evt[1:*]
   ;; Centroid the tie object
   xyin=[tiex[itie],tiey[itie]]
   xyout=wcentroid(xyin,evx,evy,evt, $
                   niter=7,alpha=0,dbt=0.0,itx=itx,ity=ity, $
                   doplot=doplot)
   tiexf[itie]=xyout[0]
   tieyf[itie]=xyout[1]
   xyin=[tiexf[itie],tieyf[itie]]
   ;; Uncertainty in the centroid
   runc=wcentroidu(xyin,evx,evy,evt, $
                   cl=0.90,nmc=1000,niter=7,alpha=0, $
                   dbt=0.0,doplot=doplot)
   tieru[itie]=runc
endfor

;; Output best-fit coordinates for tie objects

for i=0,ntie-1 do begin $
   print,tiexf[i]+1,tieyf[i]+1,tieru[i], $
       format='("image;circle(",f8.3,",",f8.3,",",f6.3,") # color=red'+$
              ' text={S'+pstr(i+1)+'}")'
endfor

;; image;circle( 171.309,  32.701, 1.646) # color=red text={S1}
;; image;circle( 132.050, 115.083, 2.254) # color=red text={S2}
;; image;circle( 258.660,  52.170, 1.749) # color=red text={S3}
;; image;circle( 147.141, 296.724, 0.994) # color=red text={S4}
;; image;circle( 216.583, 284.688, 0.999) # color=red text={S5}
;; image;circle( 197.720, 320.986, 1.439) # color=red text={S6}
;; image;circle( 338.506, 238.095, 0.771) # color=red text={S7}
;; image;circle( 259.149, 477.933, 1.374) # color=red text={S8}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; match:  s3 -> 1011-0253348
;;         s4 -> 1012-0249391
;;         s6 -> 1013-0251474
;;         s7 -> sex_388 from DSS image

ix=[2,3,5,6]
xoxf=tiexf[ix] & xoyf=tieyf[ix] & xoru=tieru[ix]
nfit=n_elements(xoxf)

;; Pixel coordinates of optical counterparts, in IDL style
optx=[259.06,149.24,200.48,340.10] - 1.0
opty=[ 52.38,297.41,321.35,237.12] - 1.0

offx=optx-xoxf & offy=opty-xoyf

;; Plot of the offsets

cirx=cos(2*!pi*findgen(25)/24.0)
ciry=sin(2*!pi*findgen(25)/24.0)
     
offrx=max(offx)-min(offx)
offry=max(offy)-min(offy)
set_isoxy,min(offx)-offrx,max(offx)+offrx, $
          min(offy)-offry,max(offy)+offry
plot,offx,offy,psym=4
oplote,offx,offy,xoru,xoru,psym=3
oplot,!x.crange,[0,0],line=1
oplot,[0,0],!y.crange,line=1
for i=0,nfit-1 do begin $
   oplot,offx[i]+xoru[i]*cirx,offy[i]+xoru[i]*ciry & $
   xyouts,offx[i]+offrx/20,offy[i]+offrx/20,pstr(i+1),charsize=1.5, $
       color=c.red
endfor

;; Calculate correction as the weighted mean

xcorr=weighted_mean(offx,xoru,xcorru)
ycorr=weighted_mean(offy,xoru,ycorru)

oplot,[xcorr],[ycorr],psym=4,thick=3,color=c.blue
oplote,[xcorr],[ycorr],[xcorru],[ycorru],psym=3,thick=2,color=c.blue
oplot,xcorr+xcorru*cirx,ycorr+ycorru*ciry,thick=2,color=c.blue

print,xcorr,xcorru
print,ycorr,ycorru
print,sqrt(xcorru^2 + ycorru^2)*xrtpix

;;      1.78926     0.534191
;;    -0.199769     0.534191
;;      1.78546
      
;; Output /corrected/ best-fit coordinates for tie objects

print,'Applying correction:'

for i=0,ntie-1 do begin $
   print,tiexf[i]+1+xcorr,tieyf[i]+1+ycorr,tieru[i], $
       format='("image;circle(",f8.3,",",f8.3,",",f6.3,") # color=red'+$
              ' text={S'+pstr(i+1)+'}")'
endfor

;; Applying correction:
;; image;circle( 173.098,  32.501, 1.646) # color=red text={S1}
;; image;circle( 133.839, 114.883, 2.254) # color=red text={S2}
;; image;circle( 260.449,  51.971, 1.749) # color=red text={S3}
;; image;circle( 148.930, 296.524, 0.994) # color=red text={S4}
;; image;circle( 218.372, 284.488, 0.999) # color=red text={S5}
;; image;circle( 199.509, 320.786, 1.439) # color=red text={S6}
;; image;circle( 340.296, 237.895, 0.771) # color=red text={S7}
;; image;circle( 260.939, 477.733, 1.374) # color=red text={S8}

;;;;;;;;;;;;;;;;;;;;

set_isoxy,0
if !d.name eq 'X' then wait,1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

xyin=[x0,y0]-1

;; Construct the "event list"
evx=[0.0] & evy=[0.0] & evt=[0.0]
for xix=xyin[0]-hbox,xyin[0]+hbox do begin $
   for yix=xyin[1]-hbox,xyin[1]+hbox do begin $
      if gd[xix,yix] gt 0 then begin $
         evx=[evx,replicate(xix,gd[xix,yix])] & $
         evy=[evy,replicate(yix,gd[xix,yix])] & $
         evt=[evt,replicate(1.0,gd[xix,yix])]
      endif
   endfor
endfor
evx=evx[1:*] & evy=evy[1:*] & evt=evt[1:*]

;; Centroid the afterglow
xyout=wcentroid(xyin,evx,evy,evt, $
                niter=7,alpha=0,dbt=0.0,itx=itx,ity=ity, $
                doplot=doplot)
gxf=xyout[0]
gyf=xyout[1]
xyin=[gxf,gyf]
;; Uncertainty in the centroid
gru=wcentroidu(xyin,evx,evy,evt, $
               cl=0.90,nmc=1000,niter=7,alpha=0, $
               dbt=0.0,doplot=doplot)

gru2=sqrt(gru^2 + xcorru*ycorru)

;; Corrected afterglow position
print,'Corrected GRB position (uncertainy includes correction uncertainty):'

print,gxf+xcorr+1,gyf+ycorr+1,gru2, $
   format='("image;circle(",f8.3,",",f8.3,",",f6.3,") # color=blue'+$
   ' text={GRB}")'

;; image;circle( 502.028, 483.928, 1.298) # color=blue text={GRB}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end

