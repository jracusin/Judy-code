pro ligo2radec,ligofile,ra,dec

  read_fits_map,ligofile,hmap,nside=nside
  print,nside
  pix2ang_nest,nside,lindgen(n_elements(hmap)),theta,phi
  lra=360.-phi*!radeg
  ldec=(0.5*!pi-theta)*!radeg

  v=10^(dindgen(100)/100.-6.)
  n=lonarr(100)
  p=fltarr(100)
  for i=0,n_elements(v)-1 do begin
     w=where(hmap gt v[i],nw)
     n[i]=nw
     p[i]=total(hmap[w])
  endfor 

  m=min(abs(p-0.9),wm)
  print,v[wm],n[wm],p[wm]
  w=where(hmap gt v[wm],nw)

  hmap=hmap[w]
  lra=lra[w]
  ldec=ldec[w]

;  w=where(lra gt 200 and lra lt 201 and ldec gt 65 and ldec lt 66,nw)
;  hmap=hmap[w]
;  lra=lra[w]
;  ldec=ldec[w]
;  plot,lra,ldec,psym=3,/yno,/iso

  dec=0
  ra=0
  for i=0,359 do begin 
     w=where(lra gt i and lra lt i+1,nw)
     if nw gt 0 then begin 
        dec=[dec,min(ldec[w]),max(ldec[w])]
        ra=[ra,i,i]
     endif 
  endfor 

  ra=ra[1:*]
  dec=dec[1:*]
;  oplot,ra,dec,psym=1,color=!red

  ;; hist2d,lra,ldec,hist,[0,360],[-90,90],180,180,xbins=xbins,ybins=ybins
  ;; w=where(hist gt 5 and hist lt 50)
  ;; x=fltarr(180,180)
  ;; for i=0,179 do x[*,i]=xbins
  ;; y=fltarr(180,180)
  ;; for i=0,179 do y[i,*]=ybins
  ;; oplot,x[w],y[w],color=!red,psym=1

;; stop
;;   ra=0.
;;   dec=0.
;; ;  done=intarr(nw)
;;   for i=0,nw-1 do begin
;; ;     if done[i] eq 0 then begin 
;;         d=separation(lra[i],ldec[i],lra,ldec)/3600.
;;         q=where(d lt 0.1 and d gt 0,nq)
;;         if nq le 1 then begin 
;;            ra=[ra,lra[i]]
;;            dec=[dec,ldec[i]]
;;         endif 
;;         if i mod 1000 eq 0 then print,i
;; ;        done[q]=1
;; ;     endif 
;;   endfor 

;;   ra=ra[1:*]
;;   dec=dec[1:*]
;;   simpctable
;;   oplot,ra,dec,psym=3,color=!red


;stop
  return
end 
