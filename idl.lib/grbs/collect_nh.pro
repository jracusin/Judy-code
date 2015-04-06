pro collect_nh
  
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  c=','
  
  openw,lun,'grb_all_nh.csv',/get_lun
  printf,lun,'GRB, nHgal, nH1, nH1err-, nH1err+, nH2, nH2err-, nH2err+, nH3, nH3err-, nH3err+, nH4, nH4err-, nH4err+'
  for i=0,ndir-1 do begin
     cd,dir[i]
     if strlen(dir[i]) eq 9 then grb=dir[i]+' ' else grb=dir[i]
     lcfile='lc_fit_out_idl_int2.dat'
     if exist(lcfile) then begin 
        read_lcfit,lcfile,pname,p,perror,chisqs,dof,breaks,lc=lc
        if exist('spec/seg1.dat') then begin
           if numlines('spec/seg1.dat') gt 2 then begin 
              read_specfit,spec,dir='spec'
;           print,n_elements(spec),breaks+1
              spec=spec[0:breaks]
              readcol,'spec/nhgal.out',blah1,format='(a)',delim='&',/silent
              blah1=blah1[n_elements(blah1)-1]
              blah1=str_sep(blah1,' ')
              galnh=blah1[n_elements(blah1)-1]*1e-22
              nh=''
              for j=0,breaks do $
                 nh=nh+ntostr(spec[j].nh)+c+ntostr(spec[j].nh_err[0])+c+ntostr(spec[j].nh_err[1])+c
              printf,lun,grb+c+ntostr(galnh)+c+nh
           endif 
        endif
     endif 
     cd,'..'
  endfor
  close,lun
  free_lun,lun
  
  return
end 
