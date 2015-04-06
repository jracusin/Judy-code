pro grb_pos_diff,detx,dety,ldp,mra,mdec,mrahms,mdecdms,dir=dir,tam=tam,frame1=frame1,noplot=noplot,msc=msc,boresight=boresight
  
  if n_params() eq 0 then begin
     print,'syntax - grb_pos_diff,detx,dety,ldp,ra,dec,mrahms,mdechms,dir=dir,msc=msc,/tam'
     return
  endif 
  
  if keyword_set(tam) then notam=0 else notam=1
  
  read_frames,fr,ldp=ntostr(ldp),dir=dir
  w=where(fr.obs_mode eq 'POINTING' and fr.tstart gt min(fr.tstart)+100)
  fr=fr[w]
  
  if n_elements(frame1) gt 0 then begin
     w=where(fr.frame_no gt frame1)
     fr=fr[w]
  endif 
  
  nframe=n_elements(fr)
  ra=dblarr(nframe) & dec=ra 
  
  for i=0,nframe-1 do begin
     
     coordinates,detx,dety,fr[i].ra,fr[i].dec,fr[i].roll,fr[i].tam_x1,fr[i].tam_y1,fr[i].tam_x2,fr[i].tam_y2,rahms,decdms,tra,tdec,notam=notam,boresight=boresight
     ra[i]=tra
     dec[i]=tdec
  endfor 
  
  mra=median(ra)
  mdec=median(dec)
  mtmp=min(sqrt((fr.ra-median(fr.ra))^2+(fr.dec-median(fr.dec))^2),w)
;  w=where(fr.ra eq median(fr.ra) and fr.dec eq median(fr.dec))
  w=w[0]
  msc=[fr[w].ra,fr[w].dec,fr[w].roll,fr[w].tam_x1,fr[w].tam_y1,fr[w].tam_x2,fr[w].tam_y2]
  
;  rah=mra/15D
;  irah=fix(rah)
;  iram=fix((rah-irah)*60.)
;  iras=((rah-irah)*60.-fix((rah-irah)*60.))*60.
  deghms,mra,irah,iram,iras
  mrahms=[irah,iram,iras]
;  idec=fix(mdec)
;  idecm=fix(abs(mdec-idec)*60.)
;  idecs=(abs(mdec-idec)*60.-fix(abs(mdec-idec)*60.))*60.
  degdms,mdec,idec,idecm,idecs
  mdecdms=[idec,idecm,idecs]
  print,'Median RA/Dec: '+ntostr(mra)+' '+ntostr(mdec)
  print,'               '+ntostr(fix(mrahms[0]))+' '+ntostr(fix(mrahms[1]))+' '+ntostr(mrahms[2],4)
  print,'               '+ntostr(fix(mdecdms[0]))+' '+ntostr(fix(mdecdms[1]))+' '+ntostr(mdecdms[2],4)
  
  offset=calc_coord_offset(ra,dec,mra,mdec,dra=dra,ddec=ddec)
  
;   if not keyword_set(noplot) then begin
;      begplot,name='grb_positions.ps'
;      plot,dra,ddec,/yno,xtitle='ra',ytitle='dec'
     
;      erase
;      multiplot,[1,7],/init
;      multiplot
;      time=fr.tstart-min(fr.tstart)
;      plot,time,offset,ytitle='offset'
;      multiplot
;      plot,time,dra,ytitle=!tsym.delta_cap+' ra'
;      multiplot
;      plot,time,ddec,ytitle=!tsym.delta_cap+' dec'  
;      multiplot
;      plot,time,fr.tam_x1,psym=1,/yno,ytitle='Tam X1'
;      multiplot
;      plot,time,fr.tam_x2,psym=1,/yno,ytitle='Tam Y1'
;      multiplot
;      plot,time,fr.tam_y1,psym=1,/yno,ytitle='Tam Y1'
;      multiplot
;      plot,time,fr.tam_y2,psym=1,/yno,ytitle='Tam Y2',xtitle='t-t!L0!N (s)'
;      multiplot,/reset
;      endplot
;   endif 
  
  return
end 
  
  
