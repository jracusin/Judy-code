pro alpha_time
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  hr11=3600.*11.  ;;11 hours in seconds
  hr2=2e3         ;;2 ks
  
  alpha11=dblarr(ndir) & alphaerr11=dblarr(2,ndir)
  alpha2=dblarr(ndir) & alphaerr2=dblarr(2,ndir)

  for i=0,ndir-1 do begin 
     cd,dir[i]
     print,dir[i]
     lcfile='lc_fit_out_idl_int3.dat'     
     if not exist(lcfile) then lcfile='lc_fit_out_idl_int2.dat'
     file='lc_newout_noflares.txt'
     if not exist(file) then file='lc_newout.txt'
     if exist(lcfile) and exist(file) then begin 
        lc=lcout2fits(file)
        tstart=min(lc.tstart)
        tstop=max(lc.tstop)
        read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks
        if n_elements(pname) gt 1 then begin 
           for k=0,1 do begin
              if k eq 0 then hrtt=hr11
              if k eq 1 then hrtt=hr2
              if hrtt gt tstart and hrtt lt tstop then begin 
                 case breaks of
                    0: j=1
                    1: begin
                       if hrtt le p[2] then j=1
                       if hrtt gt p[2] then j=3
                    end 
                    2: begin
                       if hrtt le p[2] then j=1
                       if hrtt gt p[2] and hrtt le p[4] then j=3
                       if hrtt gt p[4] then j=5
                    end 
                    3: begin
                       if hrtt le p[2] then j=1
                       if hrtt gt p[2] and hrtt le p[4] then j=3
                       if hrtt gt p[4] and hrtt le p[6] then j=5
                       if hrtt gt p[6] then j=7
                    end 
                 endcase 
                 if k eq 0 then begin 
                    alpha11[i]=p[j]
                    alphaerr11[*,i]=perror[*,j]
                    winf=where(finite(alphaerr11[*,i]) eq 0,nwinf)
                    if nwinf gt 0 then begin
                       lcfile='lc_fit_out_idl_int2.dat'
                       read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks
                       alphaerr11[winf,i]=perror[winf,j]
                    endif 
                 endif else begin 
                    alpha2[i]=p[j]
                    alphaerr2[*,i]=perror[*,j]
                    winf=where(finite(alphaerr2[*,i]) eq 0,nwinf)
                    if nwinf gt 0 then begin
                       lcfile='lc_fit_out_idl_int2.dat'
                       read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks
                       alphaerr2[winf,i]=perror[winf,j]
                    endif 
                 endelse 
              endif 
           endfor 
        endif 
     endif
     cd,'..'
  endfor 

  
  w=where(alpha11 ne 0 or alpha2 ne 0,nw)
  colprint,dir[w],alpha11[w],alphaerr11[0,w],alphaerr11[1,w],alpha2[w],alphaerr2[0,w],alphaerr2[1,w]
  
  !p.multi=[0,1,2]
  plot,indgen(nw),alpha11[w],psym=5
  for i=0,nw-1 do oplot,[i,i],[alpha11[w[i]]-alphaerr11[0,w[i]],alpha11[w[i]]+alphaerr11[1,w[i]]]
  
  plot,indgen(nw),alpha2[w],psym=5
  for i=0,nw-1 do oplot,[i,i],[alpha2[w[i]]-alphaerr2[0,w[i]],alpha2[w[i]]+alphaerr2[1,w[i]]]
  !p.multi=0
  
  stop
  return
end 
           
