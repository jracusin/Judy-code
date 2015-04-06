pro compare_centroid_positions,calfile,dir,cal=cal
  
;  filein = pickfile(title='Select XRT Postage Stamp file: ', $
;                    filter='*postage*', /must_exist)
  
  if n_elements(dir) eq 0 then dir=''
  filein=findfile(dir+'*_postagestamp.0*.postage_stamp.fits')
  
  
  if n_elements(calfile) eq 0 then calfile='~/ACSCAL.txt'  ;;need specific
  
  if n_elements(cal) eq 0 then begin 
     cal=create_struct('objname','','targetid',0L,'ra',0D,'dec',0D)
     objnames=''
     targetid=0L
     ra=0D
     dec=0D
     n=0
     openr,clun,calfile,/get_lun
     line=''
     while not eof(clun) do begin 
        readf,clun,line
        chunks=str_sep(line,' ')
        w=where(chunks ne '') 
        chunks=chunks[w]
        objnames=[objnames,chunks[0]]
        targetid=[targetid,chunks[1]]
        ra=[dec,chunks[2]]
        dec=[dec,chunks[3]]
        n=n+1
     endwhile
     cal=replicate(cal,n)
     cal.objname=objnames[1:*]
     cal.targetid=targetid[1:*]
     cal.ra=ra[1:*]
     cal.dec=dec[1:*]
     close,clun
     free_lun,clun
  endif 
  
  
  targid=0L
  sc_ra=0D
  sc_dec=0D
  grb_ra=0D
  grb_dec=0D
  for i=0,n_elements(filein)-1 do begin 
     ps=mrdfits(filein[i],0,hdr,/silent)
     
     targid=[targid,sxpar(hdr,'TARGETID')]
     sc_ra=[sc_ra,sxpar(hdr,'SC_RA')]
     sc_dec=[sc_dec,sxpar(hdr,'SC_DEC')]
     grb_ra=[grb_ra,sxpar(hdr,'GRB_RA')]
     grb_dec=[grb_dec,sxpar(hdr,'GRB_DEC')]
     
  endfor 
  
  targid=targid[1:*]
  sc_ra=sc_ra[1:*]
  sc_dec=sc_dec[1:*]
  grb_ra=grb_ra[1:*]
  grb_dec=grb_dec[1:*]
  
  rtarg=targid[rem_dup(targid)]
  ntarg=n_elements(rtarg)
  print,'Number of targets = '+ntostr(ntarg)
  for i=0,ntarg-1 do begin 
     w=where(cal.targetid eq rtarg[i],nw)
     q=where(targid eq rtarg[i],nq)
     print,'Number of observations = '+ntostr(nq)
     
     if nw gt 0 then begin 
        del_scra=(sc_ra[q]-cal[w].ra)*3600.
        del_scdec=(sc_dec[q]-cal[w].dec)*3600.
        del_grbra=(grb_ra[q]-cal[w].ra)*3600.
        del_grbdec=(grb_dec[q]-cal[w].dec)*3600.
        
        m_scra=mean(sc_ra[q])
        m_scdec=mean(sc_dec[q])
        m_grbra=mean(grb_ra[q])
        m_grbdec=mean(grb_dec[q])
        
        s_scra=stddev(sc_ra[q])*3600.
        s_scdec=stddev(sc_dec[q])*3600.
        s_grbra=stddev(grb_ra[q])*3600.
        s_grbdec=stddev(grb_dec[q])*3600.
     
        moff_scra=mean(del_scra)
        moff_scdec=mean(del_scdec)
        moff_grbra=mean(del_grbra)
        moff_grbdec=mean(del_grbdec)
     

        print
        print,'*******************************************************************'
        print,'Object True Position: '
        print,'                 (ra,dec) = ',cal[w].ra,' ',cal[w].dec
        print,'*******************************************************************'
        print,'Spacecraft pointing: '
        print,'                 <ra,dec> = ',m_scra,' ',m_scdec
        print,'         (sig_ra,sig_dec) = ',s_scra,'"',s_scdec,'"'
        print,'Offset from SC pointing: '
        print,'             <d_ra,d_dec> = ',moff_scra,'"',moff_scdec,'"'
        print,'*******************************************************************'
        print,'XRT Position: '
        print,'                 <ra,dec> = ',m_grbra,' ',m_grbdec
        print,'         (sig_ra,sig_dec) = ',s_grbra,'"',s_grbdec,'"'
        print,'Offset from XRT Position: '
        print,'             <d_ra,d_dec> = ',moff_grbra,'"',moff_grbdec,'"'
        print,'*******************************************************************'
     
     endif 
  endfor 
     
;  stop
  return
end 
