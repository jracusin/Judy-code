pro run_xrtpipeline,w=w,teldef=teldef,indir=indir,start=start,$
                    pntra=pntra,pntdec=pntdec,pntroll=pntroll,$
                    datamode=datamode,nopnt=nopnt,srcra=srcra,srcdec=srcdec,$
                    angdist=angdist,noexpo=noexpo
  
  if n_elements(indir) eq 0 then indir='./'
  indirs=file_search(indir+'00*')
  done=strpos(indirs,'-xrt')
  d=where(done eq -1,nd)
  if nd gt 0 then indirs=indirs[d]
  ms=strpos(indirs,'_o')
  wms=where(ms eq -1,nms)
  if nms gt 0 then indirs=indirs[wms]
  if n_elements(srcra) eq 0 then begin
     srcra='OBJECT'
     srcdec='OBJECT'
  endif else begin
     srcra=ntostr(srcra)
     srcdec=ntostr(srcdec)
  endelse 
  
  colprint,indgen(n_elements(indirs)),indirs
stop
  if n_elements(w) ne 0 then s=0 else s=1
  if n_elements(w) eq 0 then w=indgen(n_elements(indirs))

  nw=n_elements(w)
  dm=strarr(nw)
      
  if keyword_set(datamode) then begin
      if nw gt 1 then dm[s:*]=' datamode=pc'
  endif
  if keyword_set(angdist) then adist=' gtiexpr="ang_dist<0.175"' else adist=''
  
  if n_elements(pntra) eq 0 and not keyword_set(nopnt) then begin 
     f0=file_search('00*0-xrt')
     if f0[0] eq '' then f0=file_search('00*1-xrt')
     if f0[0] ne '' then begin
        f1=file_search(f0[0]+'/sw*pc*po*cl.evt')
        hdr=headfits(f1[0])
        pntra=sxpar(hdr,'RA_PNT')
        pntdec=sxpar(hdr,'DEC_PNT')
        pntroll=sxpar(hdr,'PA_PNT')
     endif 
  endif 
  
  if not keyword_set(noexpo) then expo=' createexpomap=yes useexpomap=yes' else expo=''
  for j=0,nw-1 do begin 
     
     if exist(indirs[w[j]]+'/xrt') then begin 
     tmpdir=str_sep(indirs[w[j]],indir)
     dir=tmpdir[n_elements(tmpdir)-1]
     
     lpos=strpos(dir,'0')
     ldir=strmid(dir,lpos,11)
     log='xrtpipeline_'+ldir+'.log'
     if n_elements(pntra) ne 0 then pnt=' pntra='+ntostr(pntra)+' pntdec='+ntostr(pntdec)+' pntroll='+ntostr(pntroll) else pnt=''
     
;     if keyword_set(teldef) then com='xrtpipeline indir='+indirs[w[j]]+' outdir=./'+dir+'-xrt steminputs=sw'+dir+' srcra=OBJECT srcdec=OBJECT createexpomap=yes useexpomap=yes teldef=/bulk/pkg/caldb/data/swift/xrt/bcf/teldef/swx20060402v001.teldef clobber=yes > '+log else $
     com='xrtpipeline indir='+indirs[w[j]]+' outdir=./'+dir+'-xrt steminputs=sw'+dir+' srcra='+srcra+' srcdec='+srcdec+' '+expo+pnt+dm[j]+adist+' clobber=yes > '+log
    
     print,com
     spawn,com
     
     if j eq 0 then begin 
        if n_elements(pntra) eq 0 and not keyword_set(nopnt) then begin 
           f0=indirs[w[j]]
           f1=file_search(f0[0]+'-xrt/sw*pc*po*cl.evt')
           hdr=headfits(f1[0])
           pntra=sxpar(hdr,'RA_PNT')
           pntdec=sxpar(hdr,'DEC_PNT')
           pntroll=sxpar(hdr,'PA_PNT')
        endif 
     endif 
  endif 
  endfor 
  
  return
end 
