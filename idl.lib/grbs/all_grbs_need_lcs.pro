pro are_they_done
  cd,!mdata
  dir=file_search('GRB*/')
  lcfile=file_search('GRB*/lc_wrap3.par')
  donefile1=file_search('GRB*/GRB*lc.gif')
  donefile2=file_search('GRB*/lc_fit_out_idl.dat')
  
  lcdir=strarr(n_elements(lcfile))
  for i=0,n_elements(lcfile)-1 do begin
     tmp=str_sep(lcfile[i],'/')
     lcdir[i]=tmp[0]
  endfor 
  
  donedir1=strarr(n_elements(donefile1))
  for i=0,n_elements(donefile1)-1 do begin
     tmp=str_sep(donefile1[i],'/')
     donedir1[i]=tmp[0]
  endfor 
  
  donedir2=strarr(n_elements(donefile2))
  for i=0,n_elements(donefile2)-1 do begin
     tmp=str_sep(donefile2[i],'/')
     donedir2[i]=tmp[0]
  endfor 
  
  help,donefile1
  help,donefile2
  dont_match,lcdir,donedir1,m1,m2
  dont_match,lcdir,donedir2,m1b,m2b
  if n_elements(m1) gt 1 then begin 
     match,dir,lcdir[m1],mm1,mm2
     print,'Make LC'
     colprint,lcdir[m1],mm1
  endif 

  if n_elements(m1b) gt 1 then begin 
     match,dir,lcdir[m1b],mm1b,mm2b
     print,'Fit LC'
     colprint,lcdir[m1b],mm1b
  endif 
  stop
  return
end 

pro all_grbs_need_lcs,g,nw,nolcwrap=nolcwrap
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  
  if n_params() eq 0 then begin 
     g=0
     nw=ndir-1
  endif 
  
  stop  
  
  print,g,nw
 
  for i=g,nw do begin 
;     print,dir[i]
     cd,dir[i]
     parfile='lc_wrap3.par'
     outfile='lc_newout.txt'
     
     if exist(parfile) then begin 
;        lc=lcout2fits(outfile)
;        w10=where(lc.src_counts lt 15.,nw10)
;        if nw10 gt 2 then print,ntostr(i)+' '+dir[i]+' '+'bin'
        
        if not keyword_set(nolcwrap) then lc_wrap,parfile
        psname=dir[i]+'_lc.ps'
        gifname=dir[i]+'_lc.gif'
        plot_like_qdp,name=dir[i],/ps,pname=psname
        spawn,'convert -rotate 270 '+psname+' '+gifname
     
     endif 
     cd,!mdata
  endfor 
  
  return
end 
