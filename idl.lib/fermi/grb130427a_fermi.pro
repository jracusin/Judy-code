pro shift_lc

  lc=lcout2fits(/phil)

  lc.time=lc.time-51

  fit_lc,lc=lc

stop
return
end

pro dan_plot,dra,ddec,fwhm,exp

Hist1D_Binsize = 0.2
Hist2D_Binsize = 0.2
xbin = hist1d(dRa,binsize=Hist1D_Binsize)
ybin = hist1d(dDec,binsize=Hist1D_Binsize)
;print,xbin,ybin
z0=hist2d(dRa,dDec,BINSIZE1=Hist2D_Binsize,BINSIZE2=Hist2D_Binsize, OBIN1=xbin, OBIN2=ybin)
POLYFILL, !X.WINDOW([0,1,1,0]), !Y.WINDOW([0,0,1,1]), /NORM, color=0

psf = PSF_GAUSSIAN( Npixel=31, FWHM=[fwhm,fwhm], /NORMAL )
image = convolve_dan(z0,psf)

image=image/exp
print,max(image),exp
image[0,0]=1.5e-4
;w=where(image eq 0,nw)
;if nw gt 0 then image[w]=min(image)
;image=image-0.96*0.5

;!p.color=!black
w=where(image eq 0)
s=size(image)
;rdis,image,/iso,low=1e-6,high=1e-2;,xmn=s[1]*0.2,xmx=s[1]*0.8,ymn=s[2]*0.2,ymx=s[2]*0.8;,/log;,low=-8,high=-3
;tvimage,image
CONTOUR, image, xbin, ybin, NLEVELS=200, /fill, xrange=[-10,10], yrange=[-10,10], xstyle=4, ystyle=4, xtitle=textoidl('\delta Ra'), ytitle=textoidl('\delta Dec'), charsize=1.25, Position=[0.1, 0.1, 0.90, 0.9], /noerase,/iso;, Title='Background'
;COLORBAR, NCOLORS=254, POSITION=[0.92,0.10,0.94,0.90] , MINRANGE=150, MAXRANGE=254, title=textoidl('Number of Events'), Divisions=2, ticknames=['0','1','2'], thick=0.50, format='(F10.2)', CHARSIZE = 0.90, /vertical, /right, /top
;print,dra,ddec,xbin,ybin

return
end 

pro grb130427a_fermi

  cd,'~/Desktop/GRB130427A/movie/'
;  ph=mrdfits('L130502120131DB652F7F90_PH00.fits',1,hdr)
;  ph=mrdfits('racusin-AstroServer-00000-ft1.fits',1)
  ph=mrdfits('racusin-AstroServer-00001-ft1.fits',1,hdr)
  gti=mrdfits('racusin-AstroServer-00001-ft1.fits',2)

  trigtime=388741629d
  gti.start=gti.start-trigtime
  gti.stop=gti.stop-trigtime
  ph.time=ph.time-trigtime
  w=where(ph.theta lt 70 and ph.zenith_angle lt 100)
  ph=ph[w]

;  ev=ph[w].event_class



;; [16133     1     4     0     0]
;;  [16135     1     4     0     0]
;;  [16159     1     4     8    16]
;;  [32517     1     4     0     0]
;;  [32519     1     4     0     0]
;;  [32525     1     4     8     0]
;;  [32527     1     4     8     0]
;;  [32541     1     4     8    16]
;;  [32543     1     4     8    16]
;;  [65285     1     4     0     0]
spawn,'convert -delay 80.0 -loop 100 -dispose previous grb130427_movie*ps grb130427a_movie.gif'

stop

return
end 
