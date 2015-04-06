pro find_sources,grbra,grbdec,newgrbra,newgrbdec,suffix=suffix,compra=compra,compdec=compdec
  
  ;;once wavdetect is installed at swift, run it via
  ;wavdetect combined_image_seg012.fits source_list.fits source_cell.fits recon_image.fits norm_back.fits
  
  if n_elements(suffix) eq 0 then suffix=''
  src=mrdfits('source_list'+suffix+'.fits',1)
  src=src[where(src.src_significance gt 3)]
  openw,lun,'src_list'+suffix+'.reg',/get_lun
  printf,lun,'# Region file format: DS9 version 3.0'
  printf,lun,'global color=green font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  for i=0,n_elements(src)-1 do $
     printf,lun,'fk5;ellipse('+ntostr(src[i].ra)+','+ntostr(src[i].dec)+','+ntostr(src[i].ra_err*10.)+','+ntostr(src[i].dec_err*10.)+',0)'
  close,lun
  free_lun,lun
  
  readcol,'sdss.csv',objid,run,rerun,camcol,field,obj,type,ra,dec,u,g,r,i,z,Err_u,Err_g,Err_r,Err_i,Err_z,format='(a,l,i,i,L,i,i,d,d,d,d,d,d,d,d,d,d,d,d)'
  openw,lun,'sdss'+suffix+'.reg',/get_lun
  printf,lun,'# Region file format: DS9 version 3.0'
  printf,lun,'global color=red font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  for i=0,n_elements(ra)-1 do printf,lun,'fk5;circle('+ntostr(ra[i])+','+ntostr(dec[i])+',5")'
  close,lun
  free_lun,lun
  
  ;; do source matching
  xdist=0d &  ydist=0d & x=0d & y=0d & xra=0d & xdec=0d & windex=0
  mra=0d & mdec=0d & sig=0d & xdisterr=0d & ydisterr=0d & index=0
  for i=0,n_elements(src)-1 do begin
     xraa=src[i].ra
     xdeca=src[i].dec
     dist=calc_coord_offset(xraa,xdeca,ra,dec)
     mindist=min(dist,w)
     if mindist lt 5. then begin
        print,'Source match: '+ntostr(xra)+' '+ntostr(xdec)+' - '+ntostr(ra[w])+' '+ntostr(dec[w])
        xd=(xraa-ra[w])*3600. ;convert to arcsec
        yd=(xdeca-dec[w])*3600.*cos(dec[w]*!dtor) ;ditto
        print,xd,yd
        xdist=[xdist,xd]
        ydist=[ydist,yd]
        mra=[mra,ra[w]]
        mdec=[mdec,dec[w]]
        x=[x,src[i].x]
        y=[y,src[i].y]
        sig=[sig,src[i].src_significance]
        xdisterr=[xdisterr,src[i].x_err]
        ydisterr=[ydisterr,src[i].y_err]
        xra=[xra,xraa]
        xdec=[xdec,xdeca]
        index=[index,i]
        windex=[windex,w]
     endif 
     
  endfor 
  xdist=xdist[1:*]
  ydist=ydist[1:*]
  mra=mra[1:*]
  mdec=mdec[1:*]
  sig=sig[1:*]
  xdisterr=xdisterr[1:*]
  ydisterr=ydisterr[1:*]
  x=x[1:*]
  y=y[1:*]
  xra=xra[1:*]
  xdec=xdec[1:*]
  index=index[1:*]
  windex=windex[1:*]
  
  openw,lun,'match'+suffix+'.reg',/get_lun
  printf,lun,'# Region file format: DS9 version 3.0'
  printf,lun,'global color=yellow font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  for i=0,n_elements(mra)-1 do printf,lun,'fk5;circle('+ntostr(mra[i])+','+ntostr(mdec[i])+',10")'
  close,lun
  free_lun,lun
  
  colprint,ntostr(mra)+' '+ntostr(mdec)
  
;  print,mean(xdist),mean(ydist)
  print
  raoff=weighted_mean(xdist,xdisterr)
  decoff=weighted_mean(ydist,ydisterr)
  newgrbra=grbra-raoff/3600d
  newgrbdec=grbdec-decoff/3600d
  print,'ra/dec/total offsets: ',raoff,decoff,calc_coord_offset(grbra,grbdec,newgrbra,newgrbdec)
  radec,newgrbra,newgrbdec,ihr, imin, xsec, ideg, imn, xsc
  print,'new coords: ',newgrbra,newgrbdec,ihr, imin, xsec, ideg, imn, xsc
  if n_elements(compra) gt 0 then begin
     off=calc_coord_offset(newgrbra,newgrbdec,compra,compdec)
     print,'comp offset: ',off
  endif 
  
  print
  print,'using only positive offsets'
  w=where(xdist gt 0 and ydist gt 0)
  raoff=weighted_mean(xdist[w],xdisterr[w])
  decoff=weighted_mean(ydist[w],ydisterr[w])
  newgrbra=grbra-raoff/3600d
  newgrbdec=grbdec-decoff/3600d
  print,'ra/dec/total offsets: ',raoff,decoff,calc_coord_offset(grbra,grbdec,newgrbra,newgrbdec)
  radec,newgrbra,newgrbdec,ihr, imin, xsec, ideg, imn, xsc
  print,'new coords: ',newgrbra,newgrbdec,ihr, imin, xsec, ideg, imn, xsc
    if n_elements(compra) gt 0 then begin
     off=calc_coord_offset(newgrbra,newgrbdec,compra,compdec)
     print,'comp offset: ',off
  endif 
  
;  im=mrdfits('combined_image_seg0123_pha40.fits',0,hdr)
;  astromit,x*1.,y*1.,mra*1.,mdec*1.,hdr,3.5
  
  stop
  return
end 
