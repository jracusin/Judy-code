pro disp_ims
  im1=mrdfits('ccd1.fits')
  im2=mrdfits('ccd2.fits')
  im3=mrdfits('ccd3.fits')

;  w1=where(im1 le 12)
;  im1[w1]=0
;  w2=where(im2 le 12)
;  im2[w2]=0
;  w3=where(im2 le 12)
;  im3[w3]=0

  !p.multi=[0,3,1]
  !p.charsize=2
;  xmn=420
;  xmx=460
;  ymn=440
;  ymx=480
  
  xmn=0
  xmx=511
  ymn=0
  ymx=511

  im1=rotate(im1,7)*1.
  im2=rotate(im2,7)*1.
  im3=im3*1.
  plotsym,0,2

  x=intarr(512,512)
  y=x
  for i=0,511 do begin
     x[i,*]=i
     y[*,i]=i
  endfor 
  val=30

  r12=im1/im2
  w12=where(im1 ne 0 and im2 ne 0)
  q12=where(r12[w12] gt 0.8 and r12[w12] lt 1.2 and im1[w12] gt val and im2[w12] gt val)
  q12=w12[q12]
  help,q12

  r13=im1/im3
  w13=where(im1 ne 0 and im3 ne 0)
  q13=where(r13[w13] gt 0.8 and r13[w13] lt 1.2 and im1[w13] gt val and im2[w13] gt val)
  q13=w13[q13]
  help,q13

  r23=im2/im3
  w23=where(im2 ne 0 and im3 ne 0)
  q23=where(r23[w23] gt 0.8 and r23[w23] lt 1.2 and im2[w23] gt val and im2[w23] gt val)
  q23=w23[q23]
  help,q23

  match,q12,q13,m1,m2
  help,m1,m2
  match,q12,q23,m1,m2
  help,m1,m2

  for i=0,2 do begin
     case i of
        0: im=im1
        1: im=im2
        2: im=im3
     endcase 
        
     rdis,im,low=0,high=100,xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx
;     oplot,x[q12],y[q12],psym=2,color=!red
;     oplot,x[q13],y[q13],psym=1,color=!yellow
;     oplot,x[q23],y[q23],psym=1,color=!blue

     oplot,x[q12[m1]],y[q12[m1]],psym=8,color=!orange

;     if i le 1 then xyouts,x[q12],y[q12],numdec(r12[q12],2),color=!red,charsize=1
;     xyouts,x[q13],y[q13],numdec(r13[q13],2),color=!yellow,charsize=1
;     if i ne 0 then xyouts,x[q23],y[q23],numdec(r23[q23],2),color=!blue,charsize=1
  endfor 
stop
  k=get_kbrd(10)
  n=n_elements(m1)
;  w=where(x[q12[m1]] gt 440 and y[q12[m1]] gt 450 and x[q12[m1]] lt 460,n)
;  m1=m1[w]
  for i=0,n-1 do begin
     j=q12[m1[i]]
     print,x[j],y[j]
     rdis,im1,low=0,high=100,xmn=(x[j]-50)>0,xmx=(x[j]+50)<511,ymn=(y[j]-50)>0,ymx=(y[j]+50)<511
     oplot,x[q12[m1]],y[q12[m1]],psym=8,color=!orange
     xyouts,x[q12[m1]],y[q12[m1]],numdec(r12[q12[m1]],2)+', '+numdec(im1[q12[m1]],0),color=!yellow,charsize=1

     rdis,im2,low=0,high=100,xmn=(x[j]-50)>0,xmx=(x[j]+50)<511,ymn=(y[j]-50)>0,ymx=(y[j]+50)<511
     oplot,x[q12[m1]],y[q12[m1]],psym=8,color=!orange
     xyouts,x[q12[m1]],y[q12[m1]],numdec(r13[q12[m1]],2)+', '+numdec(im2[q12[m1]],0),color=!yellow,charsize=1

     rdis,im3,low=0,high=100,xmn=(x[j]-50)>0,xmx=(x[j]+50)<511,ymn=(y[j]-50)>0,ymx=(y[j]+50)<511
     oplot,x[q12[m1]],y[q12[m1]],psym=8,color=!orange
     xyouts,x[q12[m1]],y[q12[m1]],numdec(r23[q12[m1]],2)+', '+numdec(im3[q12[m1]],0),color=!yellow,charsize=1

     
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor 



  !p.multi=0

stop
return
end

pro st_astrometry,image,cra,cdec,dx,dy

  rad=6.
;  if not exist('GLASTCAT.dat') then $
;  star_tracker,cra,cdec,rad

  for i=2,2 do begin 
     fnum=ntostr(i+1)
     readcol,'~/Fermi/Spacecraft/star_tracker/GLASTCAT.dat',ind,ra,dec,mag,format=('i,f,f,f')
;  if not exist('coord.cat') then $
     spawn,'sex -c machine.config ccd'+fnum+'.fits'
     spawn,'cp coord.cat coord3.cat'
     spawn,'sort -rnk3,3 coord'+fnum+'.cat | head -20 > coordf'+fnum+'.cat'
     readcol,'coordf'+fnum+'.cat',num,f1,f2,f3,x,y,flag
     write_regfile,'coord'+fnum+'.reg',x,y,3.,/det,color='!blue'
     readcol,'stars'+fnum+'.dat',xx,yy,ras,decs
     im=mrdfits('ccd'+fnum+'.fits',0,hdr)
     starast,ras,decs,xx-1.5,yy-1,cd,hdr=hdr
     mwrfits,im,'ccd'+fnum+'w.fits',hdr,/create
  endfor 
stop
;  im=intarr(512,512)
;  im[x,y]=1

;  s=reverse(sort(f2))
;  colprint,f2[s],x[s],y[s]
  x=x-256
  y=y-256

  pixscale=58.6454/3600. ;;; deg/pix

  w=where(f2 gt 100,nw)
  w2=where(mag le 8.)
  theta=45*!dtor
  r=sqrt(x[w]^2+y[w]^2)*pixscale
  t0=atan(y[w]/x[w])

  q=where(x[w] gt 0)
  r[q]=-r[q]
  x2=r*cos(theta+t0)+cra
  y2=-r*sin(theta+t0)+cdec
  
;  x2=x[w]*(-pixscale)+cra
;  y2=y[w]*pixscale+cdec
  plot,x2,y2,psym=2,/iso,xrange=[cra+rad,cra-rad],yrange=[cdec-rad,cdec+rad],/xsty
  oplot,ra[w2],dec[w2],psym=1,color=!red
readcol,'stars1.dat',xx,yy,ras,decs
oplot,ras,decs,psym=4,color=!green 

stop

;starast,ra[i],dec[i],xx[i],yy[i],cd,hdr=hdr
return
end 
