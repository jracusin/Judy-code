pro fit_boresight,ra,dec,roll,boresight=boresight,notam=notam
  
  file='~/boresight/grb_coord_offsets.csv'
  
  readcol,file,pass,ldp,detx,dety,ora,odec,delim=',',format='(a,l,d,d,d,d)'
  
  n=n_elements(pass)
  dir='/bulk/yankees2/xrt/pass_data/pass_'
  odir='/bulk/yankees/xrt/pass_data/pass_'
  
  if n_elements(ra) eq 0 then begin 
     ra=dblarr(n)
     dec=ra
     roll=ra
     
     for i=0,n-1 do begin
        
        fdir=dir+pass[i]
        if not exist(fdir) then fdir=odir+pass[i]
        files=file_search(fdir+'/pass*science*'+ntostr(ldp[i])+'*pc*fits.gz')
        if n_elements(files) gt 1 then spawn,'gunzip '+fdir+'/pass*'+ntostr(ldp[i])+'*pc*gz'
        
        grb_pos_diff,detx[i],dety[i],ldp[i],mra,mdec,mrahms,mdecdms,/noplot,boresight=boresight,msc=msc,dir=fdir+'/',notam=notam
        ra[i]=mra
        dec[i]=mdec
        roll[i]=msc[2]
        
     endfor 
  endif 
  
  w=indgen(n)
  w1=w[0:4]
  w2=w[5:12]
  w3=w[13:20]
  dra=(ora-ra)*3600.*cos(odec*!dtor)
  ddec=(odec-dec)*3600.
  simpctable
;  !p.multi=[0,1,2]
  if keyword_set(notam) then tamyn='   without tam' else tamyn='   with tam'
  plot,[-10,10],[-10,10],/nodata,psym=1,/iso,/ynozero,xtitle='d ra',ytitle='d dec',title='Boresight '+ntostr(boresight[0],5)+','+ntostr(boresight[1],5)+tamyn
  oplot,dra[w1],ddec[w1],psym=1,color=!purple
  oplot,dra[w2],ddec[w2],psym=1,color=!red
  oplot,dra[w3],ddec[w3],psym=1,color=!cyan
  oplot,[-100,100],[0,0]
  oplot,[0,0],[-100,100]
  tvcircle,3,0,0,/data
  tvcircle,6,0,0,/data
  legend,['Feb-Mar','Apr-May','Jun-Jul'],textcolor=[!purple,!red,!cyan],box=0,/right,/top,charsize=1
  
  y=dra*cos(-roll*!dtor)+ddec*sin(-roll*!dtor)
  z=ddec*cos(-roll*!dtor)-dra*sin(-roll*!dtor)
  plot,[-10,10],[-10,10],psym=1,/iso,/ynozero,xtitle='d Y',ytitle='d Z',title='Boresight '+ntostr(boresight[0],5)+','+ntostr(boresight[1],5)+tamyn
  oplot,y[w1],z[w1],psym=1,color=!purple
  oplot,y[w2],z[w2],psym=1,color=!red
  oplot,y[w3],z[w3],psym=1,color=!cyan
  oplot,[-100,100],[0,0]
  oplot,[0,0],[-100,100]
  tvcircle,3,0,0,/data
  tvcircle,6,0,0,/data
;  !p.multi=0
  return
end 
