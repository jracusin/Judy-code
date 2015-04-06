pro find_source,evtfile,ra,dec,snr,noxrtcentroid=noxrtcentroid
  
  evt=mrdfits(evtfile,1,hdr)
  
  rao=sxpar(hdr,'RA_OBJ')
  deco=sxpar(hdr,'DEC_OBJ')
  
  if not keyword_set(noxrtcentroid) then begin 
     spawn,'xrtcentroid calcpos=yes clobber=yes outfile=centroid outdir=./ totalint=10 unit=COUNTS boxra='+ntostr(rao)+' boxdec='+ntostr(deco)+' boxradius=3 infile='+evtfile+'  interactive=no'
  endif else begin 
     spawn,'ximage @/home/jracusin/bin/find_sources.xco '+evtfile
  endelse 
  
  tmp=str_sep(evtfile,'.evt')
  detfile=tmp[0]+'.det'
  
  readcol,detfile,n,cnterr,x,y,corr,rah,ram,ras,dech,decm,decs,rad,hbox,prob,snr,format='(i,a,d,d,i,i,i,d,i,i,d,i,i,d,d)',comment='!',delim=' '
  
  m=max(snr,w)
  
  
  hms2radec,rah,ram,ras,dech,decm,decs,ra,dec
  print,rah[w],ram[w],ras[w],dech[w],decm[w],decs[w]
  
  stop
  return  
end 
