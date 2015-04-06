pro short_grb_jetbreaks,ogrb,ps=ps,go=go

  dir='~/stuff_for_people/Nora/'
  readcol,dir+'shortgrb.csv',stuff,format='(a)',delim='#'
  nstuff=n_elements(stuff)
  grb=strarr(nstuff) & mission=grb & ee=grb
  for i=0,nstuff-1 do begin
     chunks=str_sep(stuff[i],',')
     grb[i]=chunks[0]
     if strmid(grb[i],0,1)*1. gt 1 then grb[i]='0'+grb[i]
     mission[i]=chunks[1]
     ee[i]=chunks[2]
  endfor 
     
  grbs='GRB'+grb
  ngrbs=n_elements(grbs)

  sgrb=mrdfits('~/Swift/swiftgrb.fits',1)
  if keyword_set(ps) then begin 
     begplot,name=dir+'short_grbs_jbfits.ps',/color
     !x.margin=[12,8]
     !y.margin=[10,10]
  endif 
  
  if n_elements(ogrb) eq 0 then begin 
     ogrb=create_struct('grb','','z',0.,'type','','jb',0,'tbreak',0d,'tbreaklim',0,'alpha',0.,'alphaerr',fltarr(2),'mission','','ee','')
     ogrb=replicate(ogrb,ngrbs)

     cd,'~/GRBs/'
     
     for i=0,ngrbs-1 do begin
        if not exist(grbs[i]) and exist(grbs[i]+'A') then grbs[i]=grbs[i]+'A'
        ogrb[i].grb=grbs[i]
        ogrb[i].mission=mission[i]
        if strtrim(ee[i],2) eq 'EE' then ogrb[i].ee='yes' else ogrb[i].ee='no'
        if exist(grbs[i]) then begin
           cd,grbs[i]
           
           lcfile='lc_fit_out_idl_int8.dat'
           if not exist(lcfile) then lcfile='lc_fit_out_idl_int7.dat'

           if exist(lcfile) then begin 
              if numlines(lcfile) gt 1 then begin 
                 w=where(strcompress(sgrb.name,/remove) eq strtrim(grbs[i],2))
                 ogrb[i].z=sgrb[w].redshift
                 type='' & jb=0 & tbreak=0 & tbreaklim=0
                 find_jetbreak,grbs[i],type,jb,tbreak,tbreaklim,alpha,alphaerr,ps=ps,go=go
                 ogrb[i].type=type
                 ogrb[i].jb=jb
                 ogrb[i].tbreak=tbreak
                 ogrb[i].tbreaklim=tbreaklim
                 na=n_elements(alpha)
                 ogrb[i].alpha=alpha[na-1]
                 ogrb[i].alphaerr=alphaerr[*,na-1]
                 colprint,grbs[i],sgrb[w].redshift,type,jb,tbreak,tbreaklim
              endif else begin
                 print,'no fit '+grbs[i]
                 ogrb[i].type='no fit'
              endelse 
           endif else begin 
              print,'no fit '+grbs[i]
              ogrb[i].type='no fit'
           endelse 
           cd,'~/GRBs/'
        endif else begin
           print,'no '+grbs[i]
           ogrb[i].type='no LC'
        endelse 
     endfor 
  endif 

  if keyword_set(ps) then begin
     endplot
     spawn,'convert '+dir+'short_grbs_jbfits.ps '+dir+'short_grbs_jbfits.pdf'
  endif 

  lim=strarr(ngrbs)
  lim[*]=' '
  w=where(ogrb.tbreaklim eq 1)
  lim[w]='>'
  w=where(ogrb.tbreaklim eq 2)
  lim[w]='<'

  jb=strarr(ngrbs)
  w=where(ogrb.jb eq 1)
  jb[w]='yes       '
  w=where(ogrb.jb eq 2)
  jb[w]='post-jb   '
  w=where(ogrb.jb eq -1)
  jb[w]='pre-jb    '
  w=where(ogrb.jb eq 0)
  jb[w]='unknown   '

  print
  print
  print
  colprint,'Name          ','z       ','EE      ','Type     ','Jet Break  ','Break Time    ','alpha'
  colprint,strsame(ogrb.grb,12),strsame(ntostr(ogrb.z,5),10),strsame(ogrb.ee,5),strsame(ogrb.type,12),jb,lim+ntostr(ogrb.tbreak),'   '+ntostr(ogrb.alpha,4)+'[+'+ntostr(ogrb.alphaerr[1,*],4)+',-'+ntostr(ogrb.alphaerr[0,*],4)+']'
  stop
  return
end
