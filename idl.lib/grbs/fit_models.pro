function fit_models,pnames,p,np,nf,file=file,addbreak=addbreak,basemo=basemo,rembreak=rembreak,breaks=breaks
  
  if n_params() eq 0 then begin
     print,'syntax - mo=fit_models(pnames,p,np,nf,file=file,addbreak=addbreak,basemo=basemo,rembreak=rembreak,breaks=breaks)'
     return,''
  end 

  if n_elements(file) ne 0 then read_lcfit,file,pnames,p
  if strtrim(pnames[0],2) eq 'nofit' then return,'no fit'
  w=where(strtrim(pnames,2) ne '')
  pnames=pnames[w]
  p=p[w]

  np=n_elements(p)
  wf=where(strpos(pnames,'gnorm') ne -1,nf)
  wc=where(strtrim(pnames,2) eq 'c',nc)
  if nc gt 0 then np=np-1
  mo=''
  case np of
     2: mo='pow'
     4: mo='bknpow'
     5: mo='gauss1_pow'
     6: mo='bkn2pow'
     7: mo='gauss1_bknpow'
     8: begin 
        mo='bkn3pow'
        if nf eq 2 then mo='gauss2_pow' 
     end 
     9: mo='gauss1_bkn2pow'
     10: begin
        mo='bkn4pow'
        if nf eq 2 then mo='gauss2_bknpow'
     end
     11: begin
        if nf eq 3 then mo='gauss3_pow'
        if nf eq 1 then mo='gauss1_bkn3pow'
     end
     12: mo='gauss2_bkn2pow'
     13: begin
        if nf eq 3 then mo='gauss3_bknpow'
        if nf eq 1 then mo='gauss1_bkn4pow'
     end 
     14: begin 
        if nf eq 4 then mo='gauss4_pow'
        if nf eq 2 then mo='gauss2_bkn3pow'
     end 
     15: mo='gauss3_bkn2pow'
     16: begin 
        if nf eq 4 then mo='gauss4_bknpow'
        if nf eq 2 then mo='gauss2_bkn4pow'
     end 
     17: begin 
        if nf eq 5 then mo='gauss5_pow'
        if nf eq 3 then mo='gauss3_bkn3pow'
     end 
     18: mo='gauss4_bkn2pow'
     19: begin 
        if nf eq 5 then mo='gauss5_bknpow'
        if nf eq 3 then mo='gauss3_bkn4pow'
     end 
     20: begin 
        if nf eq 6 then mo='gauss6_pow'
        if nf eq 4 then mo='gauss4_bkn3pow'
     end 
     21: mo='gauss5_bkn2pow'
     22: begin 
        if nf eq 6 then mo='gauss6_bknpow'
        if nf eq 4 then mo='gauss4_bkn4pow'
     end 
     23: begin 
        if nf eq 7 then mo='gauss7_pow'
        if nf eq 5 then mo='gauss5_bkn3pow'
     end 
     24: mo='gauss6_bkn2pow'
     25: begin 
        if nf eq 7 then mo='gauss7_bknpow'
        if nf eq 5 then mo='gauss5_bkn4pow'
     end 
     26: mo='gauss6_bkn3pow'
     27: mo='gauss7_bkn2pow'
     28: mo='gauss6_bkn4pow'
     29: mo='gauss7_bkn3pow'
     31: mo='gauss7_bkn4pow'
     else: mo='nofit'
  endcase 
  np=np-nf*3

  mos=strsplit(mo,'_',/ex)
  premo=''
  if n_elements(mos) eq 2 then begin 
     premo=mos[0]+'_'
     basemo=mos[1] 
  endif else basemo=mos[0]

  if keyword_set(addbreak) then begin
     if basemo eq 'pow' then basemo='bknpow' else begin 
        bmo=strsplit(mo,'bkn',/ex)
        n=strmid(bmo[n_elements(bmo)-1],0,1)
        if n ne 'p' then n=ntostr(n+1) else n='2'
        basemo='bkn'+n+'pow'
     endelse 
     mo=premo+basemo
  endif 

  if keyword_set(rembreak) then begin
     if basemo eq 'bknpow' then basemo='pow' else begin
        if basemo eq 'pow' then begin
           print,'Cannot remove break'
           basemo='pow'
        endif else begin 
           bmo=strsplit(mo,'bkn',/ex)
           n=strmid(bmo[n_elements(bmo)-1],0,1)
           if n ne 'p' then n=ntostr(n-1) else n=''
           if n eq '1' then n=''
           basemo='bkn'+n+'pow'
        endelse 
     endelse
     mo=premo+basemo
  endif 

  if mo ne 'nofit' then begin 
     case basemo of 
        'pow': breaks=0
        'bknpow': breaks=2
        'bkn2pow': breaks=[2,4]
        'bkn3pow': breaks=[2,4,6]
        'bkn4pow': breaks=[2,4,6,8]
        'bkn5pow': breaks=[2,4,6,8,10]
     endcase 
  endif 


return,mo
end 
