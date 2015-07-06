pro read_perley,p,s

;  spawn,'wget http://www.astro.caltech.edu/grbox/grboxtxt.php?starttime=700101&endtime=191231&sort=time&reverse=y&showindex=y&showt90=y&showra=y&showdec=y&showz=y&comments=y&xor=y&otobs=y&hostobs=y&ref=y&observatory=t&obsdate=2014-11-18&posfmt=sexc&xrtpos=best&format=txt -O ~/Swift/redshifts/grbs_perley_z.txt'
  readcol,'~/Swift/redshifts/grbs_perley_z.txt',grb,t90,ra,dec,z,format='(a,f,a,a,f)',/silent

  p=create_struct('grb','','z',0.,'ref',strarr(3))
  p=replicate(p,n_elements(grb))
  p.grb=grb
  p.z=z
  
  cd,'~/Swift/redshifts/'
  if n_elements(s) eq 0 then s=read_xml('grboxtxt.xml')
  t=tag_names(s.grbs)
  grbs='' & refs='' & z=''
  for i=0,n_elements(t)-1 do begin 
     tmp=execute('t2=tag_names(s.grbs.'+t[i]+'.redshift)')
     w=where(strpos(t2,'REF') ne -1,nw)
     if nw gt 0 then begin 
        tmp=execute('n=n_elements(s.grbs.'+t[i]+'.redshift.ref)')
        tmp=execute('n2=n_elements(s.grbs.'+t[i]+'.index)')
        if n2 ne n then $
           tmp=execute('grbs=[grbs,replicate(s.grbs.'+t[i]+'.index,n)]') else $
              tmp=execute('grbs=[grbs,s.grbs.'+t[i]+'.index]')
        tmp=execute('refs=[refs,s.grbs.'+t[i]+'.redshift.ref]')
        tmp=execute('z=[z,s.grbs.'+t[i]+'.redshift.z._text]')
     endif 
  endfor 

  grbs=grbs[1:*]
  refs=refs[1:*]
  z=z[1:*]

  match,strtrim(p.grb,2),strtrim(grbs,2),m1,m2
  for i=0,n_elements(m1)-1 do begin
     w=where(strtrim(grbs,2) eq strtrim(p[m1[i]].grb,2),nw)
     p[m1[i]].ref[0:nw-1]=refs[w]
  endfor 

  ;;; fixing wrong GCN ref
  q=where(p.grb eq '131108A')
  p[q].ref[0]='GCN15470'

  ;;; missing GCN ref
  q=where(p.grb eq '070506')
  p[q].ref[0]='GCN6379'
  q=where(p.grb eq '100424A')
  p[q].ref[0]='GCN14291'
  
  ;;; fixing wrong or missing ADS ref
  q=where(p.grb eq '080916C')
  p[q].ref[0]='ADS/2009A%26A...498...89G'
  q=where(p.grb eq '080207')
  p[q].ref[0]='ADS/2012ApJ...756..187H'
  q=where(p.grb eq '081109')
  p[q].ref[0]='ADS/2011AA...534A.108K'

  ;;; fixing arXiv -> published
  q=where(p.ref[0] eq 'ADS/2013arXiv1301.5903P')
  p[q].ref[0]='ADS/2013ApJ...778..128P'
  q=where(p.ref[0] eq 'arXiv/1409.5791')
  p[q].ref[0]='ADS/2015MNRAS.446.3911S'
  q=where(p.ref[0] eq 'arXiv/1302.2352')
  p[q].ref[0]='ADS/2014ApJ...781...13L'

  q=where(strtrim(p.ref[0],2) eq 'astro-ph/0803.4017')
  p[q].ref[0]='ADS/2008ApJ...681..453J'

  ;;; ADDING PHOTO-Zs
  o=p[0]
  o=replicate(o,6)
  o.grb=['060510A','090401B','100805A','081230','111215A','111225A']
  o.z=[1.2,3.1,1.85,2.03,2.06,0.297]
  o[0:2].ref[0]='ADS/2012MNRAS.426L..86O'
  o[3].ref[0]='ADS/2011AA...526A.153K'
  o[4].ref[0]='ADS/2015MNRAS.446.4116V'
  o[5].ref[0]='GCN16079'
  concat_structs,p,o,newp
  p=newp

  ;;; missed z (trouble reading in txt file or just missed)
  o=p[0]
  o=replicate(o,4)
  o.grb=['090102','130925A','080123','080905A']
  o.z=[1.547,0.35,0.495,0.1218]
  o.ref[0]=['GCN8766','GCN15249','ADS/2010ApJ...725.1202L','ADS/2010MNRAS.408..383R']
  concat_structs,p,o,newp
  p=newp

  mwrfits,p,'~/Swift/redshifts/grbs_perley_z.fits',/create

  return
end 

pro read_jochen,jg

  ;;; save latest html source code for http://www.mpe.mpg.de/~jcg/grbgen.html
  file='~/Swift/redshifts/grb_z_greiner.html'
  spawn,'wget http://www.mpe.mpg.de/~jcg/grbgen.html -O '+file

  openr,lun,file,/get_lun
  for i=0,45 do line=readline(lun,skip=1)
  grb='' & instr=''
  ipn=0 & xa=0 & ot=0 & ra=0 & z=0. & shb=0 & xrf=0
  i=0
  while not(eof(lun)) and i lt 1200 do begin   
     line=readline(lun)   
     line=readline(lun,delim='>')
;     print,line
;     help,line
;     if n_elements(line) eq 6 then begin
     g=strsplit(line[3],/extract,'<')
     shb0=strpos(g[0],'S')
     xrf0=strpos(g[0],'X')
     g=strsplit(line[3],/extract,'<SX')
     if shb0 ne -1 then shb=[shb,1] else shb=[shb,0]
     if xrf0 ne -1 then xrf=[xrf,1] else xrf=[xrf,0]
;     g=strmid(g[0],0,7)
     grb=[grb,g[0]]

     ;;; S=short X=XRT - need to save
;     endif 
     line=readline(lun,delim='>') ;; position - don't care
     line=readline(lun,delim='>') ;; rest of position - still don't care
     line=readline(lun,delim='>') ;; pos error - ditto
;     line=readline(lun,delim='>') ;; something
     line=readline(lun,delim='>') ;; Instrument
     s=strsplit(line[1],/extract,'<')
     instr=[instr,s[0]]
     line=readline(lun,delim='>') ;; IPN
     s=strpos(line[1],'y')
     if s eq -1 then ipn=[ipn,0] else ipn=[ipn,1]
     line=readline(lun,delim='>') ;; XA
     s=strpos(line[1],'y')
     if s eq -1 then xa=[xa,0] else xa=[xa,1]
     line=readline(lun,delim='>') ;; OT
     s=strpos(line[1],'y')
     if s eq -1 then ot=[ot,0] else ot=[ot,1]
     line=readline(lun,delim='>') ;; RA
     s=strpos(line[1],'y')
     if s eq -1 then ra=[ra,0] else ra=[ra,1]
     line=readline(lun,delim='>') ;; IAUC - don't care
     line=readline(lun,delim='>') ;; z
     s1=strsplit(line[1],/extract,'<?()')
;     if strpos(line[1],'?') ne -1 then stop
     s2=strpos(s1[0],'&')
     if s2 eq -1 then z=[z,s1[0]*1.] else z=[z,-1]
     i=i+1
;     colprint,grb,ipn,xa,ot,ra,z
  endwhile
  close,lun
  free_lun,lun

  jg=create_struct('grb','','ipn',0,'xa',0,'ot',0,'ra',0,'shb',0,'xrf',0,'z',0.)
  n=n_elements(grb)-2
  jg=replicate(jg,n)
  jg.grb='GRB'+grb[2:*]
  jg.ipn=ipn[2:*]
  jg.xa=xa[2:*]
  jg.ot=ot[2:*]
  jg.ra=ra[2:*]
  jg.shb=shb[2:*]
  jg.xrf=xrf[2:*]
  jg.z=z[2:*]

  mwrfits,jg,'~/Swift/redshifts/jochen_z_list.fits',/create

;  stop
return
end

pro collect_grb_z,g,grabnew=grabnew

  if n_elements(g) eq 0 then g=mrdfits('~/Swift/swift_grb_properties.fits',1)

  if keyword_set(grabnew) then begin
     read_jochen,j
     read_perley,p
  endif else begin 
     j=mrdfits('~/Swift/redshifts/jochen_z_list.fits',1) ;; Jochen Greiner's list
     p=mrdfits('~/Swift/redshifts/grbs_perley_z.fits',1) ;; Dan Perley's list
  endelse 

  ;;; Sam photo-z from Oates et al. 2012
  slist=['GRB060510A','GRB090401B','GRB100805A']
  sz=[1.2,3.1,1.85]

  match,strtrim(g.grb,2),strtrim(j.grb,2),m1,m2
  if m1[0] ne -1 then g[m1].z=j[m2].z
  match,strtrim(g.grb,2),strtrim(j.grb,2)+'A',m1,m2
  if m2[0] ne -1 then g[m1].z=j[m2].z

  match,strtrim(g.grb,2),strtrim(p.grb,2),m1,m2
  if m1[0] ne -1 then g[m1].z=p[m2].z
  match,strtrim(g.grb,2),strtrim(p.grb,2)+'A',m1,m2
  if m2[0] ne -1 then g[m1].z=p[m2].z

  match,strtrim(g.grb,2),strtrim(slist,2),m1,m2
  if m1[0] ne -1 then g[m1].z=sz[m2]
  match,strtrim(g.grb,2),strtrim(slist,2)+'A',m1,m2
  if m2[0] ne -1 then g[m1].z=sz[m2]

  ;; remove questionable redshifts
  q=['GRB121011A','GRB060923C','GRB050713A','GRB060805A','GRB060807','GRB070223','GRB070224','GRB070330','GRB080205','GRB080212','GRB080307','GRB080319A','GRB080320','GRB080523','GRB080703','GRB121212A']
  match,strtrim(g.grb,2),strtrim(q,2),m1,m2
  g[m1].z=0.


  mwrfits,g,'~/Swift/swift_grb_properties.fits',/create

;;   bat=mrdfits('~/jetbreaks/batcat.fits',1) ;; parsed from Davide's list???

;;   sgrb=mrdfits('~/Swift/swiftgrb.fits',1)  ;;; Davide's list


;;   readcol,'~/stuff_for_people/Nino/Xray_sample.dat',ngrb,z,f11,f24,format='(a,f,f,f)' ;; Nino's list

;;   ;; if batcat up to date, comprehensize list of bat bursts
;;   ;; (misses other's followed-up ...


;;   grb=create_struct('grb','','z',0.)
;;   grb=replicate(grb,n_elements(bat))

;;   grb.grb=bat.grb

;;   match,strtrim(sgrb.name,2),strtrim(grb.grb,2),m1,m2
;;   grb[m2].z=sgrb[m1].redshift
;; stop
;;   w=where(z ne 999)
;;   match,strtrim(ngrb[w],2),strtrim(grb.grb,2),m1,m2

;;   grb[m2].z=z[w[m1]]


;; stop
  return
end
