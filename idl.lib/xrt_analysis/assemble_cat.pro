pro assemble_cat,outstr,catfile,outfile,mode=mode,ccdtmin=ccdtmin,ccdtmax=ccdtmax,dir=dir,outdir=outdir
  
  if n_params() eq 0 then begin
     print,'syntax - assemble_cat,outstr,catfile,outfile,mode=mode,ccdtmin=ccdtmin,ccdtmax=ccdtmax,outdir=outdir,dir=dir'
     return
  endif
  
  if n_elements(dir) eq 0 then dir='/bulk/yankees2/xrt/pass_data/'
  if n_elements(catfile) eq 0 then catfile='big.cat'
  if n_elements(outfile) eq 0 then outfile='obj.txt'
  if n_elements(mode) eq 0 then mode='PC' else mode=strupcase(mode)
  if n_elements(ccdtmax) eq 0 then ccdtmax=-45.
  if n_elements(ccdtmin) eq 0 then ccdtmin=-80.
 
  w=where(outstr.settled eq 1 and strtrim(outstr.mode,2) eq mode and outstr.ccdt le ccdtmax and outstr.ccdt ge ccdtmin,nw)
  
  file=strarr(nw)
  
  catbegin=strmid(catfile,0,strpos(catfile,'.cat'))
  rd=rem_dup(outstr[w].timeline)
  tl=outstr[w[rd]].timeline
  norbit=n_elements(rd)
  
  for j=0,norbit-1 do begin
     cfile=catbegin+'_'+mode+ntostr(j+1)+'.cat'
     q=where(outstr[w].timeline eq tl[j],nq)
     q=w[q]
     
     openw,lun,cfile,/get_lun
     for i=0,nq-1 do begin
        spos=strpos(outstr[q[i]].timeline,'science')
        file[i]=strmid(outstr[q[i]].timeline,0,spos+9)+'_LDP'+ntostr(outstr[q[i]].ldp)+'.frame'+ntostr(outstr[q[i]].frame)+'.'+strlowcase(mode)+'.events.fits'
        
        if mode eq 'WT' then begin
           if file_size(file[i]) gt 1e4 then begin 
              if n_elements(outdir) gt 0 then begin 
                 bpos=46;strpos(outstr[q[i]].timeline,'pass')
                 
              endif else begin 
                 bpos=0
                 outdir=''
              endelse 
              
              ofile=outdir+strmid(outstr[q[i]].timeline,bpos,spos+9)+'_LDP'+ntostr(outstr[q[i]].ldp)+'.frame'+ntostr(outstr[q[i]].frame)+'.'+strlowcase(mode)+'.events.out.fits'
              print,ofile
              if n_elements(k) eq 0 then k=''
              if k ne 'y' then $
                 read,k,prompt='Convert WT to event files? (y/n)  '
              if k eq 'y' then wt2evt,file[i],ofile
              if exist(ofile) then printf,lun,ofile
           endif 
        endif else printf,lun,file[i]
     
     endfor 
     close,lun
     free_lun,lun
  endfor 
  
  uldp=outstr[rem_dup(outstr.ldp)].ldp
  nuldp=n_elements(uldp)
  openw,olun,outfile,/get_lun
  printf,olun,'ldp   targetid   modes     min ccdt   max ccdt   frame start   frame stop   slewagain'
  for i=0,nuldp-1 do begin
     w=where(outstr.ldp eq uldp[i],nw)
     modes=outstr[w].mode
     modes=modes(rem_dup(modes))
     mode=''
     wshort=where(strlen(modes) eq 2,nwshort)
     if nwshort gt 0 then modes[wshort]=modes[wshort]+'  '
     wshort=where(strlen(modes) eq 3,nwshort)
     if nwshort gt 0 then modes[wshort]=modes[wshort]+' '
     for j=0,n_elements(modes)-1 do mode=mode+' '+modes[j]
     output=ntostr(uldp[i])+'    '+ntostr(outstr[w[0]].targetid)+'    '+mode+'     '+ntostr(min(outstr[w].ccdt),5)+'       '+ntostr(max(outstr[w].ccdt),5)
     if nw gt 1 then begin 
;        slew=outstr[w].slew
        set=outstr[w].settled
;        ws=where(slew eq 1 and set eq 0)
        ws=where(set eq 0)
        wset=where(set eq 1,nwset)
        if nwset gt 1 then begin 
           wsagain=where(ws gt wset[0],nws)
           if nws gt 0 then slewagain=1 else slewagain=0
           output=output+'         '+ntostr(min(outstr[w[wset]].frame))+'           '+ntostr(max(outstr[w[wset]].frame))+'          '+ntostr(slewagain)
        endif 
     endif 
     printf,olun,output
  endfor 
  close,/all
  
  return
end 
  
     
