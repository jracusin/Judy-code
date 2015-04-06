pro read_lcfit_twocomp,file,pname,p,perror,chisq,dof,breaks,lc=lc
  
  if n_params() eq 0 then begin 
     print,'syntax - read_lcfit_twocomp,file,pname,p,perror,chisq,dof,breaks,lc=lc'
     return
  endif 
  
  if numlines(file) eq 1 then begin 
     pname='nofit'
     return
  endif 

  readcol,file,name,col1,col2,col3,format='(a,d,d,d)',/silent
  readcol,file,name2,col,format='(a,d)',/silent
  n=n_elements(name)-1
;  pname=['norm',name[0:n]]
  pname=name
  p=col1[0:n]
  perror1=col2[0:n]
  perror2=col3[0:n]
;  norm=col1[n+1]
;  normerr1=col2[n+1]
;  normerr2=col3[n+1]
;  p=[norm,p]
;  perror1=[normerr1,perror1]
;  perror2=[normerr2,perror2]
  perror=dblarr(2,n+1)
  perror[0,*]=perror1
  perror[1,*]=perror2
  chisq=col[n+1]
  dof=col[n+2]
  breaks=n/2-1

  
  d2=dblarr(2)
  lc=create_struct($
     'norm1',0d,$
     'norm1err',d2,$
     'pow',0d,$
     'powerr',d2,$
     'pow1',0d,$
     'pow1err',d2,$
     'break1',0d,$
     'break1err',d2,$
     'pow2',0d,$
     'pow2err',d2,$
     'norm2',0d,$
     'norm2err',d2,$
     'pow3',0d,$
     'pow3err',d2,$
     'break2',0d,$
     'break2err',d2,$
     'pow4',0d,$
     'pow4err',d2,$
     'chisq',0d,$
     'dof',0L,$
     'nbreaks',0)
  
  lc.norm1=p[0]
  lc.norm1err=[perror1[0],perror2[0]]
  lc.nbreaks=breaks
  lc.chisq=chisq
  lc.dof=dof
  case breaks of
     0: begin
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.norm2=p[2]
        lc.norm2err=[perror1[2],perror2[2]]
        lc.pow3=p[3]
        lc.pow3err=[perror1[3],perror2[3]]        
     end 
     1: begin
        wa2=where(pname eq 'alpha2',nwa2)
        if nwa2 eq 0 then begin 
           lc.pow1=p[1]
           lc.pow1err=[perror1[1],perror2[1]]
           lc.norm2=p[2]
           lc.norm2err=[perror1[2],perror2[2]]
           lc.pow3=p[3]
           lc.pow3err=[perror1[3],perror2[3]]
           lc.break2=p[4]
           lc.break2err=[perror1[4],perror2[4]]
           lc.pow4=p[5]
           lc.pow4err=[perror1[5],perror2[5]]
        endif else begin
           lc.norm2=p[0]
           lc.norm2err=[perror1[0],perror2[0]]
           lc.pow3=p[1]
           lc.pow3err=[perror1[1],perror2[1]]
           lc.norm1=p[2]
           lc.norm1err=[perror1[2],perror2[2]]
           lc.pow1=p[3]
           lc.pow1err=[perror1[3],perror2[3]]
           lc.break1=p[4]
           lc.break1err=[perror1[4],perror2[4]]
           lc.pow2=p[5]
           lc.pow2err=[perror1[5],perror2[5]]
        endelse 
     end
     2: begin 
        lc.pow1=p[1]
        lc.pow1err=[perror1[1],perror2[1]]
        lc.break1=p[2]
        lc.break1err=[perror1[2],perror2[2]]        
        lc.pow2=p[3]
        lc.pow2err=[perror1[3],perror2[3]]
        lc.norm2=p[4]
        lc.norm2err=[perror1[4],perror2[4]]
        lc.pow3=p[5]
        lc.pow3err=[perror1[5],perror2[5]]
        lc.break2=p[6]
        lc.break2err=[perror1[6],perror2[6]]
        lc.pow4=p[7]
        lc.pow4err=[perror1[7],perror2[7]]
     end 
  endcase 
  
  
  return
end 
