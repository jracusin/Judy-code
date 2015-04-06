pro read_lcfit,file,pname,p,perror,chisq,dof,breaks,lc=lc,flares=flares,np=np,nf=nf
  
  if n_params() eq 0 then begin 
     print,'syntax - read_lcfit,file,pname,p,perror,chisq,dof,breaks,lc=lc'
     return
  endif 
  
  if numlines(file) eq 1 then begin 
     pname='nofit'
     return
  endif 

  readcol,file,name,col1,col2,col3,format='(a,d,d,d)',/silent
  readcol,file,name2,col,format='(a,d)',/silent
;  pname=name
  n=n_elements(name)-1
  nf=0
;  pname=['norm',name[0:n]]
  if name[0] eq 'Norm' or name[0] eq 'norm' or name[0] eq 'gnorm1' then begin 
     p=col1[1:n]
     perror1=col2[1:n]
     perror2=col3[1:n]
     norm=col1[0]               ;[n+1]
     normerr1=col2[0]           ;[n+1]
     normerr2=col3[0]           ;[n+1]
     nname=name[0]
     npname=name[1:*]
  endif else begin

     p=col1[0:n-1]
     perror1=col2[0:n-1]
     perror2=col3[0:n-1]
     norm=col1[n]
     normerr1=col2[n]
     normerr2=col3[n]
     nname=name[n]
     npname=name[0:n-1]
  endelse
  
  pname=[nname,npname]
  f=where(npname eq 'gnorm1',nwf)
  if nwf gt 0 then begin 
     nf=(n-f[0])/3
     nt=n+1
     n=n-nf*3
  endif else nt=n+1
  p=[norm,p]
  perror1=[normerr1,perror1]
  perror2=[normerr2,perror2]
  wnan=where(finite(perror1,/nan),nwnan)
  if nwnan gt 0 then perror1[wnan]=0.
  wnan=where(finite(perror2,/nan),nwnan)
  if nwnan gt 0 then perror2[wnan]=0.

  perror=dblarr(2,nt)
  perror[0,*]=perror1
  perror[1,*]=perror2
  chisq=col[nt]
  dof=col[nt+1]
  breaks=n/2

  if where(name eq 'c') ne -1 then begin
     breaks=breaks-1
     nc=1
  endif else nc=0

  d2=dblarr(2)
  lc=create_struct($
     'norm',0d,$
     'normerr',d2,$
     'pow',0d,$
     'powerr',d2,$
     'pow1',0d,$
     'pow1err',d2,$
     'break1',0d,$
     'break1err',d2,$
     'pow2',0d,$
     'pow2err',d2,$
     'break2',0d,$
     'break2err',d2,$
     'pow3',0d,$
     'pow3err',d2,$
     'break3',0d,$
     'break3err',d2,$
     'pow4',0d,$
     'pow4err',d2,$
     'break4',0d,$
     'break4err',d2,$
     'pow5',0d,$
     'pow5err',d2,$
     'chisq',0d,$
     'dof',0L,$
     'nbreaks',0)
  
  lc.norm=p[0]
  lc.normerr=[perror1[0],perror2[0]]
  lc.nbreaks=breaks
  lc.chisq=chisq
  lc.dof=dof
  case breaks of
     0: begin
        lc.pow=p[1]
        lc.powerr=[perror1[1],perror2[1]]
     end 
     1: begin
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.break1=p[2]
        lc.break1err=[perror1[2],perror2[2]]
        lc.pow2=p[3]
        lc.pow2err=[perror1[3],perror2[3]]
     end
     2: begin 
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.break1=p[2]
        lc.break1err=[perror1[2],perror2[2]]
        lc.pow2=p[3]
        lc.pow2err=[perror1[3],perror2[3]]
        lc.break2=p[4]
        lc.break2err=[perror1[4],perror2[4]]
        lc.pow3=p[5]
        lc.pow3err=[perror1[5],perror2[5]]
     end 
     3: begin 
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.break1=p[2]
        lc.break1err=[perror1[2],perror2[2]]
        lc.pow2=p[3]
        lc.pow2err=[perror1[3],perror2[3]]
        lc.break2=p[4]
        lc.break2err=[perror1[4],perror2[4]]
        lc.pow3=p[5]
        lc.pow3err=[perror1[5],perror2[5]]
        lc.break3=p[6]
        lc.break3err=[perror1[6],perror2[6]]
     end 
     4: begin 
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.break1=p[2]
        lc.break1err=[perror1[2],perror2[2]]
        lc.pow2=p[3]
        lc.pow2err=[perror1[3],perror2[3]]
        lc.break2=p[4]
        lc.break2err=[perror1[4],perror2[4]]
        lc.pow3=p[5]
        lc.pow3err=[perror1[5],perror2[5]]
        lc.break3=p[6]
        lc.break3err=[perror1[6],perror2[6]]
        lc.pow4=p[7]
        lc.pow4err=[perror1[7],perror2[7]]
        lc.break4=p[6]
        lc.break4err=[perror1[8],perror2[8]]

     end 

  endcase 

  if nf gt 0 then begin 
     d2=dblarr(2)
     flares=create_struct('gnorm',0d,'gnormerr',d2,$
                          'gcenter',0d,'gcentererr',d2,'gwidth',0d,'gwidtherr',d2)
     flares=replicate(flares,nf)

     for i=0,nf-1 do begin 
        flares[i].gnorm=p[n+i*3+1]
        flares[i].gcenter=p[n+i*3+2]
        flares[i].gwidth=p[n+i*3+3]
     endfor 
  endif
  np=n_elements(p)-nf*3-nc

  return
end 
