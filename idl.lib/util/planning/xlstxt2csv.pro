pro xlstxt2csv,outcsv,infile=infile,data=data
  
  @pass1_path_init
  
  if n_elements(infile) ne 0 then file=infile else $
     file=pickfile(filter='*.txt')
  
  if n_params() eq 0 then outcsv=file+'.csv'          ;temp filename 
  
  openw,tlun,outcsv,/get_lun
  
  printf,tlun,'target_name, type, target_ID, segment, FoM, RA, dec, roll, BATmode, XRTmode, UVOTmode, duration, comment, priority, SSmin, SSmax,'
  
  temp=mrdfits(calib_xls_path,1)  ;needs to be set pointing to dir
  data=read_ascii(file,temp=temp)
  
  nobj=n_elements(data.field01)
  
  nseg=0
  ssmin1=60
  ssmax1=3000
  
  for i=0,nobj-1 do begin 
     
     if data.field01[i] ne '' then begin 
        target_id=data.field01[i]+','
        type=data.field12[i]+','
        rast=data.field02[i]
        decst=data.field03[i]
        mpoint=data.field04[i]
        raoff=data.field05[i]
        decoff=data.field06[i]
        batmode='0x0000,'       ;will this always be the same?
        xrtmode='0x0000,'
        uvotmode='0x0000,'      ;ok for now, eventually get from Pat?
        if data.field11[i] lt ssmin1/1.e3 then data.field11[i]=ssmin1/1.e3
        duration=data.field11[i]+','  
        priority=data.field14[i]+','
        if priority eq ',' then priority='0,'
        fom='0,'           ;need to get real value from someone?
        c1a=strsplit(data.field20[i],',',/extract)
        c1b=strsplit(strjoin(c1a+' '),'"',/extract)
        c1=c1b[0]
        c2a=strsplit(data.field21[i],',',/extract)
        c2b=strsplit(strjoin(c2a+' '),'"',/extract)
        c2=c2b[0]
        c3a=strsplit(data.field22[i],',',/extract)
        c3b=strsplit(strjoin(c3a+' '),'"',/extract)
        c3=c3b[0]
        comment=c1+' '+c2+' '+c3+','
        target_name=data.field19[i]+','
        if i gt 0 then wseg=where(data.field01[0:i-1] eq data.field01[i],nseg)
        seg=nseg+1
        segment=ntostr(seg)+','
        roll='0,'               ;filler value, updated in tako
        ssmin=ntostr(ssmin1)+',' ;arbitrary but also set by tako.init
        ssmax=ntostr(ssmax1)+',' 
        ra=strsplit(rast,/extract)
        dec=strsplit(decst,/extract)
        if n_elements(ra) eq 3 then begin
           hms2radec,ra[0],ra[1],ra[2],dec[0],dec[1],dec[2],ratmp,dectmp
           if mpoint eq 'Yes' then begin
              coord_offset,ratmp,dectmp,raoff/60.,decoff/60.,newra,newdec
              ratmp=newra
              dectmp=newdec
           endif 
           ra=ntostr(ratmp)+','
           dec=ntostr(dectmp)+','
;           ra=ra*15.
;           ra=ntostr(tenv(ra[0],ra[1],ra[2]))+','
;           dec=strsplit(decst,/extract)
;           dec=ntostr(tenv(dec[0],dec[1],dec[2]))+','
        
           printf,tlun,target_name+type+target_id+segment+fom+ra+dec+roll+$
              batmode+xrtmode+uvotmode+duration+comment+priority+ssmin+ssmax
        endif 
     endif 
  endfor 
  close,/all
  
  return
end
