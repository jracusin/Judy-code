pro check_posta,quicklook=quicklook
spawn,'ls -lrt *postagestamp.*.fits* > fff'
readcol,'fff',c1,c2,c3,c4,c5,c6,c7,c8,file,format='(a,a,a,a,a,a,a,a,a)' 
spawn,'rm fff' 
nfil=n_elements(file)
media=fltarr(nfil)
sigma=fltarr(nfil)

lo_lim=125.	;Ka low limit
up_lim=150.	; Ka upper limit
sbin=1

;SPAWN,'ls -lrt xrt*jun*/*/*im_image*fits*  > tttim '
;spawn,'wc tttim   >  wwcc2'
;readcol,'wwcc2',nnn, format='(a)'

if nfil[0] gt 0  then begin

   wfile=strpos(file,'.fits')
   wfile=where(wfile gt 0)
   file=file[wfile]
for i=0,nfil-1 do begin

   posta=readfits(file[i],hdr)
   hh=histogram(posta,min=lo_lim,max=up_lim,bin=sbin)    
   hx=lo_lim+sbin/2.+findgen((up_lim-lo_lim)/sbin+1)*sbin
   hf=gaussfit(hx,hh,res,nterms=3)
   if not keyword_set(quicklook) then begin
      plot,hx,hh,psym=10,xr=[lo_lim,up_lim],/xst
      oplot,hx,hf,li=5,color=177,thick=3   
   endif 
;   str=' '  & read,str		
;   ima=readfits(image[i], hdr)
;   temp=sxpar(hdr,'T_CCD')
   media[i]=res[1]
   sigma[i]=res[2]
   print,'mean:  ', res[1]
   print,'stddev: ',res[2]
;   print, 'temperature:  ',temp


endfor

openw,1,'check_posta.xls'
for i=0,nfil-1 do  printf,1,file[i],media[i],sigma[i], $
    format='(a50,", ",f14.2,", ",f14.2)'
close,1

endif else begin
	print, "no frames of this type"
endelse


end
