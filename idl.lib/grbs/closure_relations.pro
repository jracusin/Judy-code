@fit_functions
@type_jbcr
pro filter_jb_properties,cr,pewter,iron,nsig=nsig
  
  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  jbs=[pewter,iron]
  tags=tag_names(cr)
  nwj=26
  wj=indgen(nwj)+6
  wfit=intarr(nwj)
  for i=0,n_elements(jbs)-1 do begin

     for j=0,nwj-1 do if cr[jbs[i]].(wj[j]) lt nsig then wfit[j]=1
     w=where(wfit ne 0,nw) ;;where non-jb fits
     for j=0,nw-1 do tmp=execute('cr[jbs[i]].('+ntostr(wj[w[j]])+')=90.')
     wcr=where(cr.grb eq cr[jbs[i]].grb)
     file=strtrim(cr[jbs[i]].grb)+'/qstr'+signame+'.fits'
     print,cr[jbs[i]].grb
     qstr=mrdfits(file,1)
     pfile=strtrim(cr[jbs[i]].grb)+'/pstr'+signame+'.fits'
     pstr=mrdfits(pfile,1)
     if not exist(file) then stop
     cr_consistency_check,cr[wcr],elim,qstr,pstr,nsig=nsig     
;     which_cr_results,cr,cr[jbs[i]].grb
;     k=get_kbrd(10)
  endfor
  type_jb,cr,jbs,nsig=nsig
  
  return
end

pro cr_fake_compare,nsig=nsig
  
  if n_elements(nsig) eq 0 then nsig=3.
  cr_paper_results,cr,gold,silver,iron,pewter,w0,w23,w123,w4,w3g,nsig=nsig,/nofilter
;;; should we assume that probable bursts can only have their jb relations, but then how to compare to mock???  
  u=uniq(cr[[w0,w23,w123,w4]].grb)
;  nreal=n_elements(u)*1.
  nreal=[n_elements(uniq(cr[w4].grb)),n_elements(uniq(cr[w123].grb)),n_elements(uniq(cr[w23].grb)),n_elements(uniq(cr[w0].grb))]
;  nreal=n_elements(uniq(cr[w4].grb))
  type_jb,cr,[gold,silver,pewter,iron],nonly=nonly0,/silent,nsig=nsig
;  type_jb,cr,gold,nonly=nonly0,/silent,nsig=nsig
  type_jb,cr,w4[w3g],nonly=nonly0a,/silent,nsig=nsig
  nonly0[4]=nonly0[4]+nonly0a[4]
  
  fakelc,cr[w4],'w1234',dir='w1234/',nonly=nonly1,nsig=nsig;,/silent
  fakelc,cr[w123],'w123',dir='w123/',nonly=nonly2,/silent,nsig=nsig
  fakelc,cr[w23],'w23',dir='w23/',nonly=nonly3,/silent,nsig=nsig
  fakelc,cr[w0],'w0',dir='w0/',nonly=nonly4,/silent,nsig=nsig
  
  
  print,'             # JB        Uni          Uni-sp      Uni-n-s     JB+EI        Struc       ISM         Wind        p<2         p>2         nu2          nu3'
  print,'real ',nonly0[0:11]
  print
  print,'f1234',nonly1[0:11]
  print,'f123 ',nonly2[0:11]
  print,'f23  ',nonly3[0:11]
  print,'f0   ',nonly4[0:11]
  print
;  nfake=4000.
  nfake=1000.
  print,nreal,nfake
;  njb=total([nonly1[0],nonly2[0],nonly3[0],nonly4[0]])
;  nuni=total([nonly1[1],nonly2[1],nonly3[1],nonly4[1]])
;  nunisp=total([nonly1[2],nonly2[2],nonly3[2],nonly4[2]])
;  nuninosp=total([nonly1[3],nonly2[3],nonly3[3],nonly4[3]])
;  njbei=total([nonly1[4],nonly2[4],nonly3[4],nonly4[4]])
;  nstruc=total([nonly1[5],nonly2[5],nonly3[5],nonly4[5]])
;  nism=total([nonly1[6],nonly2[6],nonly3[6],nonly4[6]])
;  nwind=total([nonly1[7],nonly2[7],nonly3[7],nonly4[7]])
;  np12=total([nonly1[8],nonly2[8],nonly3[8],nonly4[8]])
;  npg2=total([nonly1[9],nonly2[9],nonly3[9],nonly4[9]])
;  nnu2=total([nonly1[10],nonly2[10],nonly3[10],nonly4[10]])
;  nnu3=total([nonly1[11],nonly2[11],nonly3[11],nonly4[11]])
;  nonly=[nonlyjb,nonlynorm,nonlyspread,nonlynospread,nonlyeinorm,nonlystruc,nonlyism,nonlywind,nonlyp12,nonlyp2,nonlynu2,nonlynu3,nonlysph,nnotjb]  
 
;  print,'expect        '+ntostrarr([njb,nuni,nunisp,nuninosp,njbei,nstruc,nism,nwind,np12,npg2,nnu2,nnu3]/nfake*nreal,'       ',5)
;  print,'+/-           '+ntostrarr(sqrt([njb,nuni,nunisp,nuninosp,njbei,nstruc,nism,nwind,np12,npg2,nnu2,nnu3])/nfake*nreal,'       ',5)
  
  out='' & outerr='' & sp='       '
  for i=0,11 do begin
     n=[nonly1[i],nonly2[i],nonly3[i],nonly4[i]]
     nerr=total(sqrt(n)/nfake*nreal)
     nval=total(n/nfake*nreal)
     if i eq 11 then sp=''
     out=out+ntostr(nval,5)+sp
     outerr=outerr+ntostr(nerr,5)+sp
  endfor 
  print,'expect        '+out
  print,'+/-           '+outerr
  
  stop
  return
end 

pro fakelc,crreal,name,cr,dir=dir,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
  
  if n_params() lt 2 then begin
     print,'syntax fakelc,crreal,name'
     return
  endif 
  
  if n_elements(nsig) eq 0 then nsig=3.
  if n_elements(dir) eq 0 then dir='' else begin
     cdir=''
     if nsig eq 2. then signame='_2sig' else begin
        signame='_3sig'
        cdir='conf90/'
     endelse 
     totfile='simulations/'+cdir+'cr_sim_total_'+name+signame+'.fits'
     if exist(totfile) then cr=mrdfits(totfile,1)
  endelse 
  names=name+'_'

  w0=where(crreal.seg0 eq 1,nw0)
  w1=where(crreal.seg1 eq 1,nw1)
  w2=where(crreal.seg2 eq 1,nw2)
  w3=where(crreal.seg3 eq 1,nw3)
  w4=where(crreal.seg4 eq 1,nw4)
  
  if n_elements(cr) eq 0 then begin      
     n=1000
     num=indgen(n)
     numstr=ntostr(num)
     w=where(num lt 10)
     numstr[w]='0'+numstr[w]
     w=where(num lt 100)
     numstr[w]='0'+numstr[w]
     grb='GRB000'+numstr
     jstart=1
     if nw0 eq 0 then begin 
        if nw1 gt 0 then begin ;;;works for 1-2-3-4, 1-2-3
           random_signif,crreal,w1,w1,crfake1,nsig=nsig
           random_signif,crreal,w2,[w1,w2],crfake2,nsig=nsig
           crfake1.grb=grb
        endif else begin
           random_signif,crreal,w2,w2,crfake2,nsig=nsig  ;;works for 2-3-4 or 2-3
           jstart=2
        endelse 
        random_signif,crreal,w3,[w2,w3],crfake3,nsig=nsig
        if nw4 gt 0 then begin 
           random_signif,crreal,w4,[w3,w4],crfake4,nsig=nsig
           crfake4.grb=grb
        endif
        crfake2.grb=grb
        crfake3.grb=grb
     endif else begin  ;;works if 0
        random_signif,crreal,w0,w0,crfake0,nsig=nsig
        crfake0.grb=grb
        jstart=0
     endelse 
     
     if nw0 eq 0 then begin 
        if nw1 gt 0 then concat_structs,crfake1,crfake2,crtmp0 else crtmp0=crfake2
        if nw4 gt 0 then concat_structs,crfake3,crfake4,crtmp1 else crtmp1=crfake3
        concat_structs,crtmp0,crtmp1,cr
        crtmp0=0 & crtmp1=0
     endif else cr=crfake0
     
;     k=get_kbrd(10)
;     if k eq 's' then stop
     cd,!mdata
;  make_crstruct,n,crfake
;  x=alog10(findgen(100)+1)
;  t=(max(x)-reverse(x))*1e6+100.
     x=10.^((findgen(100)+1.)/50.)
     t=[x[0:49]*1e2,x[50:69]*1e3,x[70:99]*1e4]
     for i=0,n-1 do begin 
        fname='simulations/'+dir+'cr_sim_'+names+ntostr(i)+signame+'.fits'
        w=where(cr.grb eq grb[i],nw)
        crstr=cr[w]
        which_alpha,cr[w].alpha,cr[w].alphaerr,j,posa,delalp,delalperr
        delalp=[0,delalp]
        delalperr=[[0,0],[delalperr]]
        print,cr[w].alpha
        print,cr[w].alphaerr
        print,cr[w].beta
        print,cr[w].betaerr
        if nw1 gt 0 and nw4 gt 0 then begin  ;;works if 1-2-3-4
           !p.multi=[0,1,5]
           p=[1e5,cr[w[0]].alpha,1000.,cr[w[1]].alpha,1e4,cr[w[2]].alpha,1e5,cr[w[3]].alpha]
           plot,t,bkn3pow(t,p),/xlog,/ylog,title=ntostr(n)
           oplot,[1000.,1000.],[1e-7,1e6],line=2 ;;break 1
           oplot,[1e5,1e5],[1e-7,1e6],line=2     ;;break 3
        endif 
        if nw1 eq 0 and nw4 eq 0 and nw0 eq 0 then begin ;;works if 2-3 (or 1-2)
           !p.multi=[0,1,3]
           p=[1e5,cr[w[0]].alpha,1e4,cr[w[1]].alpha]
           plot,t,bknpow(t,p),/xlog,/ylog,title=ntostr(n)
        endif 
        if nw1 gt 0 and nw4 eq 0 and nw0 eq 0 then begin ;;works if 1-2-3
           !p.multi=[0,1,4]
           p=[1e5,cr[w[0]].alpha,1000.,cr[w[1]].alpha,1e4,cr[w[2]].alpha]
           plot,t,bkn2pow(t,p),/xlog,/ylog,title=ntostr(n)
           oplot,[1000.,1000.],[1e-7,1e6],line=2 ;;break 1
        endif 
        if nw0 gt 0 then begin ;;works if 0
           !p.multi=[0,1,2]
           p=[1e5,cr[w[0]].alpha]
           plot,t,pow(t,p),/xlog,/ylog,title=ntostr(n)
        endif else begin 
           oplot,[1e4,1e4],[1e-5,1e3],line=2 ;;break 2
        endelse 

        for j=0,nw-1 do begin 
           
           which_closure_relation,cr[w[j]].alpha,cr[w[j]].alphaerr[0],cr[w[j]].alphaerr[1],cr[w[j]].beta+1.,cr[w[j]].betaerr[0],cr[w[j]].betaerr[1],crels,posa[*,j],delalp[j],[delalperr[j],delalperr[j]],gg,chisq=chisq,qstr=qstr0,pstr=pstr0,nsig=nsig,plotevery=plotevery
           
           if j gt 0 then begin
              concat_structs,qstr,qstr0,qstrcomb
              qstr=qstrcomb
              concat_structs,pstr,pstr0,pstrcomb
              pstr=pstrcomb
           endif else begin
              qstr=qstr0
              pstr=pstr0
           endelse 
           
           oplot,[0,0],[0,60],line=2
;        stop
;        if n_elements(posa[j,*]) eq 2 then posa=posa[0]
;        s=size(posa)
;        if s[0] eq 2 then posa=
           com=execute('crstr[j].seg'+ntostr(j+jstart)+'=1')
           
;        for pa=1,4 do begin 
;           wposa=where(posa eq pa,nw)
;           if n_elements(posa) ne 4 then begin 
;              if nw eq 1 then com=execute('crstr[j].seg'+ntostr(pa)+'=1') 
;           endif else cr[j].seg0=1
;        endfor 
           if gg[0] ne -1 then $
              for c=0,n_elements(gg)-1 do  com=execute('crstr[j].(gg[c]+6)=sqrt(chisq[gg[c]])')     
        endfor 
        elim=1
        while elim gt 0 do cr_consistency_check,crstr,elim,qstr,pstr,nsig=nsig
;        cr_consistency_check,crstr
;        cr_consistency_check,crstr ;;run twice to adapt to already removed possibilities

        mwrfits,crstr,fname,/create
;     k=get_kbrd(10)
;     if k eq 's' then stop
        !p.multi=0
     endfor 
     !p.multi=0
     
     cat_crstructs,cr,nsig=nsig,fname='simulations/'+dir+'cr_sim_'+names+'*'+signame+'*',outname='simulations/cr_sim_total_'+name+signame+'.fits'
     
  endif 
  fw0=where(cr.seg0 eq 1,nw0)
  fw1=where(cr.seg1 eq 1,nw1)
  fw2=where(cr.seg2 eq 1,nw2)
  fw3=where(cr.seg3 eq 1,nw3)
  fw4=where(cr.seg4 eq 1,nw4)
  print
;stop  
  tnames=tag_names(cr)
  ntags=n_elements(tnames)
  ncrs=ntags-6-32
  crnames=tnames[6:ncrs+6]
  n=n_elements(cr)
  nreal=n_elements(crreal)*1.
  rand=strarr(n)
  res=strarr(n)
  rcon=strarr(n)
  nsig0=1.  
  if nw0 gt 0 then begin 
     print,'Single PL'
     type_jb,cr,fw0,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
     print
     ;;compare real and random

     for i=0,ncrs-1 do begin
        com=execute('wcons=where(cr[fw0].(i+6) le nsig0 and cr[fw0].(i+6) ne 100.,nwcons)')
        com=execute('wrealcons=where(crreal[w0].(i+6) le nsig0,nwrealcons)')
        rand[i]=ntostr(nwcons)+'/'+ntostr(n)+'*'+ntostr(fix(nreal))
        res[i]=numdec(nwcons*1./n*nreal,2)
        rcon[i]=ntostr(nwrealcons)
     endfor 
     w=where(rcon*1. gt 0 and res*1. gt 0)
     colprint,crnames[w]+'     '+rand[w]+'    '+res[w]+'   '+rcon[w]
  endif else begin 
     if nw1 gt 0 then begin 
        print,'region I'
        type_jb,cr,fw1,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
        ;;compare real and random
        for i=0,ncrs-1 do begin
           com=execute('wcons=where(cr[fw1].(i+6) le nsig0 and cr[fw1].(i+6) ne 100.,nwcons)')
           com=execute('wrealcons=where(crreal[w1].(i+6) le nsig0,nwrealcons)')
           rand[i]=ntostr(nwcons)+'/'+ntostr(n)+'*'+ntostr(fix(nreal))
           res[i]=numdec(nwcons*1./n*nreal,2)
           rcon[i]=ntostr(nwrealcons)
        endfor 
        w=where(rcon*1. gt 0 and res*1. gt 0)
        colprint,crnames[w]+'     '+rand[w]+'    '+res[w]+'   '+rcon[w]
     endif 
     print
     print,'region II'
     type_jb,cr,fw2,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
     ;;compare real and random
     for i=0,ncrs-1 do begin
        com=execute('wcons=where(cr[fw2].(i+6) le nsig0,nwcons)')
        com=execute('wrealcons=where(crreal[w2].(i+6) le nsig0 and cr[fw2].(i+6) ne 100.,nwrealcons)')
        rand[i]=ntostr(nwcons)+'/'+ntostr(n)+'*'+ntostr(fix(nreal))
        res[i]=numdec(nwcons*1./n*nreal,2)
        rcon[i]=ntostr(nwrealcons)
     endfor 
     w=where(rcon*1. gt 0 and res*1. gt 0)
     colprint,crnames[w]+'     '+rand[w]+'    '+res[w]+'   '+rcon[w]
     print
     print,'region III'
     type_jb,cr,fw3,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
     nonlyei=nonly[4]
     ;;compare real and random
     for i=0,ncrs-1 do begin
        com=execute('wcons=where(cr[fw3].(i+6) le nsig0,nwcons)')
        com=execute('wrealcons=where(crreal[w3].(i+6) le nsig0 and cr[fw3].(i+6) ne 100.,nwrealcons)')
        rand[i]=ntostr(nwcons)+'/'+ntostr(n)+'*'+ntostr(fix(nreal))
        res[i]=numdec(nwcons*1./n*nreal,2)
        rcon[i]=ntostr(nwrealcons)
     endfor 
     w=where(rcon*1. gt 0 and res*1. gt 0)
     colprint,crnames[w]+'     '+rand[w]+'    '+res[w]+'   '+rcon[w]
     print
     if nw4 gt 0 then begin 
        print,'region IV'
        type_jb,cr,fw4,wonlyspread=wonlyspread,wonlynospread=wonlynospread,wonlynorm=wonlynorm,wonlyeinorm=wonlyeinorm,wonlystruc=wonlystruc,wonlyjb=wonlyjb,silent=silent,wnotjb=wnotjb,wsph=wsph,wonlysph=wonlysph,wonlyism=wonlyism,wonlywind=wonlywind,wonlyp12=wonlyp12,wonlypg2=wonlypg2,wonlynu2=wonlynu2,wonlynu3=wonlynu3,nonly=nonly,nsig=nsig
        nonly[4]=nonlyei
        ;;compare real and random
        for i=0,ncrs-1 do begin
           com=execute('wcons=where(cr[fw4].(i+6) le nsig0,nwcons)')
           com=execute('wrealcons=where(crreal[w4].(i+6) le nsig0 and cr[fw4].(i+6) ne 100.,nwrealcons)')
           rand[i]=ntostr(nwcons)+'/'+ntostr(n)+'*'+ntostr(fix(nreal))
           res[i]=numdec(nwcons*1./n*nreal,2)
           rcon[i]=ntostr(nwrealcons)
        endfor 
        w=where(rcon*1. gt 0 and res*1. gt 0)
        colprint,crnames[w]+'     '+rand[w]+'    '+res[w]+'   '+rcon[w]
     endif 
  endelse 
  
;  stop
  return
end 

pro random_signif,cr,wq,wa,crfake,nsig=nsig
  
  if n_elements(nsig) eq 0 then nsig=3.
  ;;create grid of alpha's & beta's based on real data and mean alperr & betaerr and see frac that fits CRs or family of CRs (type_jb tests)
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  
  if n_elements(cr) eq 0 then cr=mrdfits(!mdata+'closure_relations_total'+signame+'.fits',1)
  n=1000  ;;;number to fake
  if n_elements(wq) eq 0 then wq=indgen(n_elements(cr))
  if n_elements(crfake) eq 0 then begin 
     make_crstruct,n,crfake

     !p.multi=[0,2,3]
     ;;start by doing all regions together
     ;;get random alphas
     print,'alpha: ',minmax(cr[wq].alpha),minmax(cr[wq].alphaerr)
     print,'beta: ',minmax(cr[wq].beta),minmax(cr[wq].betaerr)
     plothist,cr[wq].alpha,x,y,bin=0.1;,yrange=[0,50]
     g=gaussfit(x,y,a,estimates=[max(y),mean(cr[wq].alpha),stddev(cr[wq].alpha)],nterm=3)
     oplot,x,g,color=!green
     genrand,g,x,n,alprand,/double
     plothist,alprand,/over,color=!red,bin=0.05;,yrange=[0,50]
     
     ;;get random beta's
     plothist,cr[wq].beta,x,y,bin=0.1;,yrange=[0,50]
     g=gaussfit(x,y,b,estimates=[max(y),mean(cr[wq].beta),stddev(cr[wq].beta)],nterm=3)
     oplot,x,g,color=!green
     genrand,g,x,n,betrand,/double
     plothist,betrand,/over,color=!red,bin=0.05;,yrange=[0,50]

     ;;get random delta alphas
     uniq=uniq(cr[wa].grb)
     nuniq=n_elements(uniq)
     delalp=0. & delalperr=0.
     for i=0,nuniq-1 do begin
        w=where(cr[wa].grb eq cr[wa[uniq[i]]].grb and cr[wa].alphaerr[0] gt 0. and cr[wa].alphaerr[1] gt 0.,nw)
        if nw gt 1 then begin 
           dalp=cr[wa[w[1:*]]].alpha-cr[wa[w[0:nw-2]]].alpha
           dalperrs=[sqrt(total(cr[wa[w]].alphaerr[0]^2)),sqrt(total(cr[wa[w]].alphaerr[1]^2))]
           delalp=[delalp,dalp]
           delalperr=[delalperr,dalperrs]
        endif 
     endfor 
     if n_elements(delalp) gt 1 then begin 
        delalp=delalp[1:*]
        delalperr=delalperr[1:*]

        print,'delalp: ',minmax(delalp),minmax(delalperr)
        
        plothist,delalp,x,y,bin=0.1;,yrange=[0,50]
        g=gaussfit(x,y,aa,estimates=[max(y),mean(delalp),stddev(delalp)],nterm=3)
        oplot,x,g,color=!green
        genrand,g,x,n,dalprand,/double
        plothist,dalprand,/over,color=!red,bin=0.03;,yrange=[0,50]

        plothist,delalperr,x,y,bin=0.01;,yrange=[0,50]
        g=gaussfit(x,y,aaerr,estimates=[max(y),mean(delalperr),stddev(delalperr)],nterm=3)
        oplot,x,g,color=!green
        genrand,g,x,n,dalpranderr,/double
        plothist,dalpranderr,/over,color=!red,bin=0.03;,yrange=[0,50]
     endif else begin
        dalprand=fltarr(n)
        dalpranderr=fltarr(n)
        print,'No delalp'
     endelse
                                ;  delalperr=median(delalperr[1:*])
;     delalperr=stddev(delalp)
;     delalperr=aa[2]
;     delalperr=[delalperr,delalperr]
     
;  alphaerr=median(cr.alphaerr)
;  betaerr=median(cr.betaerr)
;     alphaerr=stddev(cr[wq].alpha)
;     betaerr=stddev(cr[wq].beta)
     
     w=where(cr[wq].alphaerr[0] gt 0. and cr[wq].alphaerr[1] gt 0.)
     plothist,cr[wq[w]].alphaerr,x,y,bin=0.1;,yrange=[0,50]
     g=gaussfit(x,y,aerr,estimates=[max(y),mean(cr[wq[w]].alphaerr),stddev(cr[wq[w]].alphaerr)],nterm=3)
     oplot,x,g,color=!green
     genrand,g,x,n,alphaerr,/double
     plothist,alphaerr,/over,color=!red,bin=0.03;,yrange=[0,50]
     
     w=where(cr[wa].betaerr[0] gt 0. and cr[wa].betaerr[1] gt 0.)
     plothist,cr[wa[w]].betaerr,x,y,bin=0.1;,yrange=[0,50]
     g=gaussfit(x,y,berr,estimates=[max(y),mean(cr[wa[w]].betaerr),stddev(cr[wa[w]].betaerr)],nterm=3)
     oplot,x,g,color=!green
     genrand,g,x,n,betaerr,/double
     plothist,betaerr,/over,color=!red,bin=0.03;,yrange=[0,50]
     
;     alphaerr=a[2]
;     betaerr=b[2]
     
     crfake.alpha=alprand
     crfake.beta=betrand
     crfake.alphaerr[0]=alphaerr
     crfake.alphaerr[1]=alphaerr
     crfake.betaerr[0]=betaerr
     crfake.betaerr[1]=betaerr
     
     
;;      goto,jump
;;      k=get_kbrd(10)
;;      if k eq 's' then stop
;;      posa=[1,2,3,4]
;;      !p.multi=0

;;      for i=0,n-1 do begin 
;; ;        print,alprand[i],alphaerr[i]
;; ;        print,betrand[i],betaerr[i]
;; ;        print,dalprand[i],dalpranderr[i]
;;         which_closure_relation,alprand[i],alphaerr[i],alphaerr[i],betrand[i]+1.,betaerr[i],betaerr[i],crels,posa,dalprand[i],[dalpranderr[i],dalpranderr[i]],gg,chisq=chisq ;,/noplot ;,answer=answer
;;         oplot,[0,0],[0,60],line=2
;; ;        k=get_kbrd(10)
;;         if k eq 's' then stop
;;         if gg[0] ne -1 then $
;;            for c=0,n_elements(gg)-1 do  com=execute('crfake[i].(gg[c]+6)=sqrt(chisq[gg[c]])')     
;;         ;;now need to collect random results
;;      endfor   
;;      jump:
     
  endif 
  
;;   type_jb,crfake,indgen(n)
;;   stop
  
;;   goto,dontdo
;;   nr=n_elements(wq)
;;   z=dblarr(nr)
;;   tnames=tag_names(cr)
;;   ntags=n_elements(tnames)
;;   ncrs=ntags-6-26  
;;   p=dblarr(ncrs)
;;   for i=0,ncrs-1 do begin
;;      com=execute('chisq=(crfake.(i+6))^2.')
;;      std=stddev(chisq)
;; ;     std=stddev([-chisq,chisq])
;;      mu=0.
;; ;     mu=mean(chisq)
;;      real=dblarr(nr)
;;      for j=0,nr-1 do begin 
;;         com=execute('real[j]=cr[wq[j]].(i+6)^2')
;; ;        com=execute('print,cr[wq[j]].(i+6)^2')
;;         if real[j] ne 1e4 then z[j]=abs(real[j]-mu)/std
;;      endfor 
;;      w=where(z ne 0. and real ne 1e4,nw)
;;      if nw gt 0 then begin 
;;         mz=median(z[w])
;;         pj=gaussint(z)-gaussint(-z)
;;         p[i]=gaussint(mz)-gaussint(-mz)
;;         plothist,[-1,chisq],title=tnames[i+6],bin=0.5,xrange=[0,100];max(chisq)]
;;         plothist,[-1,real[w]],/over,color=!red,bin=0.5,xrange=[0,100];max(chisq)]
;;         print,p[i]
;;         stop
;;      endif else print,'No real fit '+tnames[i+6]
;;   endfor 
;;   plothist,chisq
;;   dontdo:
                                ;  which_closure_relation,a,alow,aup,g,glow,gup,cr,posa,delalp,delalperr,gg,ps=ps,injection=injection,answer=answer
;  type_jb,crfake,indgen(n)
  
  !p.multi=0
;  stop
  return
end 

pro type_reg,cr,w,w4,w23,w123,w0
  
  match,w,w4,m4a,m4b
  match,w,w23,m23a,m23b
  match,w,w123,m123a,m123b
  match,w,w0,m0a,m0b
  
  if m4a[0] ne -1 then nm4=n_elements(m4a) else nm4=0
  if m23a[0] ne -1 then nm23=n_elements(m23a) else nm23=0
  if m123a[0] ne -1 then nm123=n_elements(m123a) else nm123=0
  if m0a[0] ne -1 then nm0=n_elements(m0a) else nm0=0
  print,'I-II-III-IV/II-III-IV: '+ntostr(nm4)
  print,'II-III/III-IV: '+ntostr(nm23)
  print,'I-II-III: '+ntostr(nm123)
  print,'single pl: '+ntostr(nm0)
  
  
  return
end 
  
pro maybe_jet_break,cr,w,q,display=display,nsig=nsig,phil=phil
  
  nsig0=1.
  if n_elements(nsig) eq 0 then nsig=3.
  cd,!mdata
  nseg=n_elements(cr)
  jetbreak=intarr(nseg)
  ;;grab only cr tags
  tags=tag_names(cr)
  tags=tags[6:n_elements(tags)-33]
  ncr=n_elements(tags)
  jtmp=strpos(tags,'JET')
  wj=where(jtmp eq 0,nwj)
;  wj=wj[0:nwj-2]
  for j=0,nseg-1 do begin
     bin=intarr(ncr)
     for i=0,ncr-1 do tmp=execute('bin[i]=cr[j].('+ntostr(i+6)+')')
     jet=bin[wj]
;     w=where(jet eq 1,nw)
     w=where(jet le nsig0,nw)
     if nw gt 0 then cr[j].jetbreak=1
  endfor 
  
  u=uniq(cr.grb)
  nuniq=n_elements(u)
  jetbreak=intarr(nuniq)
  q=intarr(nuniq)
;  w=where(cr[u].jetbreak gt 0,nuniq)
;  u=u[w]
  
  alpha=fltarr(nuniq)
  tbreak=alpha
  count=0
  for i=0,nuniq-1 do begin 
     w=where(cr.grb eq cr[u[i]].grb,nw)
     q[i]=w[nw-1]
;     jetbreak[i]=total(cr[w].jetbreak)
     jetbreak[i]=cr[q[i]].jetbreak
     cr[w].jetbreak=0
;     tbreak[i]=max(cr[w].tbreak)  ;;NOTHING IS EVER DONE WITH TBREAK
     alpha[i]=cr[q[i]].alpha
;     jj=where(cr[w].jetbreak eq 1,njj)
;     if njj gt 0 then alpha[i]=cr[w[jj]].alpha
     if jetbreak[i] gt 0 then begin
        print,cr[u[i]].grb+' '+ntostrarr(cr[w].alpha)+' '+ntostr(jetbreak[i])
        count=count+1
        if nw eq 4 or (nw eq 3 and cr[w[0]].seg1 eq 0) then cr[w].jetbreak=4
        if nw eq 3 and cr[w[0]].seg1 eq 1 then cr[w].jetbreak=2
        if nw eq 2 and cr[w[0]].seg2 eq 1 then cr[w].jetbreak=3
        if nw eq 1 then cr[w].jetbreak=1
     endif 
     if jetbreak[i] gt 0 and keyword_set(display) then begin 
        plot_cr,cr,w,nsig=nsig0,phil=phil
        print,cr[w].tbreak
        k=get_kbrd(10)
       if k eq 's' then stop
     endif 
  endfor 
;  q=q[1:*]  
  
  ;;if jetbreak eq 4 then jb seg 4 (I-II-III-IV or II-III-IV)
  ;;if jetbreak eq 3 then II-III or III-IV ambiguity
  ;;if jetbreak eq 2 then jb seg 3
  ;;if jetbreak eq 1 then one seg consistent with jcr
  
  
  w=where(cr[q].jetbreak gt 3,nw)
;  stop

  
  w=q[w]
;  colprint,cr[w].grb,cr[w].alpha,cr[w].jetbreak
    
  ;;;slopes very shallow for fitting jet breaks, see which jet CRs actually fit

;  stop
  
  return
end 

pro which_cr_results,cr,grbs,nsig=nsig,psc=psc,phil=phil,plotevery=plotevery,incaption=incaption,segment=segment
  
  cd,!mdata
  nsig0=1.
  if n_elements(nsig) eq 0 then nsig=3.
  ngrb=n_elements(grbs)
  for t=0,ngrb-1 do begin
     grb=grbs[t]
     g=where(strtrim(grb,2) eq strtrim(cr.grb,2),n)
     print,g
     tnames=tag_names(cr)
     ntags=n_elements(tnames)
     ncrs=ntags-6-32
     print,grb
     bin=fltarr(ncrs)
     
     for i=0,n-1 do begin
        if cr[g[i]].seg0 eq 1 then seg=0
        if cr[g[i]].seg1 eq 1 then seg=1
        if cr[g[i]].seg2 eq 1 then seg=2
        if cr[g[i]].seg3 eq 1 then seg=3
        if cr[g[i]].seg4 eq 1 then seg=4
        print,'Segment '+ntostr(seg)+' consistent with:'
        for j=0,ncrs-1 do tmp=execute('bin[j]=cr[g[i]].('+ntostr(j+6)+')')
        w=where(bin le nsig0,nw)
        if nw gt 0 then print,tnames[w+6]
     endfor 
     plot_cr,cr,g,nsig=nsig,psc=psc,phil=phil,plotevery=plotevery,incaption=incaption,segment=segment
;     stop
     if ngrb gt 1 then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 

  return
end 
  
pro plot_cr,crs,ind,nsig=nsig,psc=psc,phil=phil,plotevery=plotevery,incaption=incaption,segment=segment
  
  nsig0=1.
  if n_elements(nsig) eq 0 then nsig=3.
  if n_params() eq 2 then grbs=crs[ind[uniq(crs[ind].grb)]].grb else ind=indgen(n_elements(crs))
  ngrb=n_elements(grbs)
  
  for t=0,ngrb-1 do begin
     wg=where(crs.grb eq grbs[t],nwg)
     cr=crs[wg]
  
     rlc=0
     ncrs=n_tags(cr)-6-32
     breaks=n_elements(cr)-1
     dir=cr[0].grb
     cd,strtrim(dir,2)
;     fit_lc,/justplot,pmulti=[0,1,breaks+3],name=dir,charsize=1.5,lc=rlc
     erase
     multiplot2,/init,[1,1+nwg]
;     multiplot2
     if keyword_set(phil) then lcoutfile='lc_newout_phil.txt'
     rlc=lcout2fits(lcoutfile)
;     plot_like_qdp,xtitle='',name=grbs[t],lc=rlc,/noxaxis
     if keyword_set(psc) then begin
        nocolor=1
        noleg=1
     endif 
     multiplot2,ydowngap=0.05
     fit_lc,/justplot,title=grbs[t],charsize=1,lc=rlc,ytitle='Count Rate',noleg=noleg,nocolor=nocolor,/noinit,/noxaxis,phil=phil,/nohard
     
     wrlc=where(rlc.tot_hard gt 0 and finite(rlc.tot_hard))  
;  !y.margin=[3,1]
;     multiplot2,ydowngap=0.08
;     ploterror,rlc[wrlc].time,rlc[wrlc].tot_hard,rlc[wrlc].tot_hard_err,psym=3,/nohat,xtitle='Time since BAT trigger (s)',ytitle='hardness ratio',charsize=1.,/xlog,/ylog,yrange=minmax(rlc[wrlc].tot_hard),xtickname=replicate('',10) ;[min(rlc[wrlc].tot_hard-rlc[wrlc].tot_hard_err),max(rlc[wrlc].tot_hard+rlc[wrlc].tot_hard_err)]
     
;     for r=0,n_elements(rlc[wrlc])-1 do oplot,[rlc[wrlc[r]].tstart,rlc[wrlc[r]].tstop],[rlc[wrlc[r]].tot_hard,rlc[wrlc[r]].tot_hard]
     which_alpha,cr.alpha,cr.alphaerr,j,posa,delalp,delalperr
     delalp=[0,delalp]
     delalperr=[[0,0],[delalperr]]
;     multiplot2
     if n_elements(segment) eq 0 then begin 
        seg=['I','II','III','IV']
        if cr[0].seg1 eq 0 then seg=seg[1:*]
     endif else seg=segment
     for j=0,breaks do begin 
        bin=fltarr(ncrs)
        for i=0,ncrs-1 do tmp=execute('bin[i]=cr[j].('+ntostr(i+6)+')')
        wsig=where(bin le nsig0,nwsig)
        answer=intarr(ncrs)
        if nwsig gt 0 then answer[wsig]=1
        multiplot2,ydowngap=0
        if j eq breaks then xtitle=!tsym.psi_cap+' = '+!tsym.alpha+' - f('+!tsym.beta+')' else xtitle=''
        which_closure_relation,cr[j].alpha,cr[j].alphaerr[0],cr[j].alphaerr[1],cr[j].beta+1.,cr[j].betaerr[0],cr[j].betaerr[1],crsp,posa[*,j],delalp[j],delalperr[*,j],gg,psc=psc,answer=answer,qstr=qstr,pstr=pstr,nsig=nsig,xtitle=xtitle,charsize=1.,segment=seg[j],nocolor=nocolor,plotevery=plotevery,incaption=incaption
     endfor
     multiplot2,/reset
     multiplot2,/default
     
     cd,'..'

     if ngrb gt 1 then begin 
        k=get_kbrd(10)
        if k eq 's' then stop
     endif 
  endfor 
  
  return
end 

function qconsistent,q1,q2,q1err,q2err,nsig=nsig
  
  dq=q1-q2
  nsig0=1.
  if dq gt 0 then begin
     if dq-nsig0*q1err[0]-nsig0*q2err[1] le 0 then return,1 else return,0
  endif else begin
     if dq+nsig0*q1err[1]+nsig0*q2err[0] ge 0 then return,1 else return,0
  endelse 

end 

pro cr_consistency_check,cr,elim,qstr,pstr,nsig=nsig,onlycsm=onlycsm,nonu=nonu

  nsig0=1.
  if n_elements(nsig) eq 0 then nsig=3.
  elim=0
  ;;;MAKES ASSUMPTION THAT WIND OR ISM IS CONSTANT PER GRB
  
  crname=['HighLat','HighLat2',$
;      'ISMs1a','ISMs1b','ISMs1ai',$
          'ISMs2a','ISMs2b','ISMs2ai',$
          'ISMs3a','ISMs3b','ISMs3ai',$
;      'WINDs1a','WINDs1b','WINDs1ai',$
          'WINDs2a','WINDs2b','WINDs2ai',$
          'WINDs3a','WINDs3b','WINDs3ai',$
;      'ISMf1a','ISMf1b','ISMf1ai',$
          'ISMf2a','ISMf2b','ISMf2ai',$
          'ISMf3a','ISMf3b','ISMf3ai',$
;      'WINDf1a','WINDf1b','WINDf1ai',$
          'WINDf2a','WINDf2b','WINDf2ai',$
          'WINDf3a','WINDf3b','WINDf3ai',$
;      'JETs1a','JETs1b','JETs1i',$
          'JETs2a','JETs2b','JETs2ai',$  ;;uniform jet (spreading)
          'JETs3a','JETs3b','JETs3ai',$
          'JETsISM2a','JETsISM2b','JETsISM2ai',$ ;;uniform jet (non-spreading)
          'JETsISM3a','JETsISM3b','JETsISM3ai',$
          'JETsWIND2a','JETsWIND2b','JETsWIND2ai',$
          'JETsWIND3a','JETsWIND3b','JETsWIND3ai',$
          'JETsoism2a','JETsoism2b',$ ;;structured outflow
          'JETsoism3a','JETsoism3b',$
          'JETsowind2a','JETsowind2b',$
          'JETsowind3a','JETsowind3b'] 
;          'JETthism2','JETthism3',$
;          'JETthwind2','JETthwind3'] ;;top-hat jet
  
  model=[3,3,$   ;;HighLAT 
         1,1,1,$ ;;ISMs2
         1,1,1,$ ;;ISMs3
         2,2,2,$ ;;WINDs2
         2,2,2,$ ;;WINDs3
         1,1,1,$ ;;ISMf2
         1,1,1,$ ;;ISMf3
         2,2,2,$ ;;WINDf2
         2,2,2,$ ;;WINDf3
         3,3,3,$ ;;JET2
         3,3,3,$ ;;JET3
         1,1,1,$ ;;JETsISM2
         1,1,1,$ ;;JETsISM3
         2,2,2,$ ;;JETsWIND2
         2,2,2,$ ;;JETsWIND3
         1,1,$   ;;DEL ALPHA SO ISM
         1,1,$
         2,2,$
         2,2]   ;;DEL ALPHA S0 WIND
  
  ;;ASSUMES THAT ONLY F->S TRANSITION NOT S->F
  sf=[3,3,$   ;;HighLAT 
      1,1,1,$ ;;ISMs2
      1,1,1,$ ;;ISMs3
      1,1,1,$ ;;WINDs2
      1,1,1,$ ;;WINDs3
      2,2,2,$ ;;ISMf2
      2,2,2,$ ;;ISMf3
      2,2,2,$ ;;WINDf2
      2,2,2,$ ;;WINDf3
      1,1,1,$ ;;JET2
      1,1,1,$ ;;JET3
      1,1,1,$ ;;JETsISM2
      1,1,1,$ ;;JETsISM3
      1,1,1,$ ;;JETsWIND2
      1,1,1,$ ;;JETsWIND3
      1,1,$   ;;DEL ALPHA SO ISM2
      1,1,$   ;;DEL ALPHA SO ISM3
      1,1,$   ;;DEL ALPHA S0 WIND2
      1,1]    ;;DEL ALPHA S0 WIND2
  
  nu=[0,0,$   ;;HighLAT 
      2,2,2,$ ;;ISMs2
      3,3,3,$ ;;ISMs3
      2,2,2,$ ;;WINDs2
      3,3,3,$ ;;WINDs3
      2,2,2,$ ;;ISMf2
      3,3,3,$ ;;ISMf3
      2,2,2,$ ;;WINDf2
      3,3,3,$ ;;WINDf3
      2,2,2,$ ;;JET2
      3,3,3,$ ;;JET3
      2,2,2,$ ;;JETsISM2
      3,3,3,$ ;;JETsISM3
      2,2,2,$ ;;JETsWIND2
      3,3,3,$ ;;JETsWIND3
      2,2,$   ;;DEL ALPHA SO ISM
      3,3,$   ;;DEL ALPHA SO ISM
      2,2,$   ;;DEL ALPHA S0 WIND
      3,3]    ;;DEL ALPHA S0 WIND
  
;  p=[0,0,$
;     1,2,3,$  ;;ISMs2
;     1,2,3,$  ;;ISMs3
;     1,2,3,$  ;;WINDs2
;     1,2,3,$  ;;WINDs3
;     1,2,3,$  ;;ISMf2
;     1,2,3,$  ;;ISMf3
;     1,2,3,$  ;;WINDf2
;     1,2,3,$  ;;WINDf3
;     1,2,3,$  ;;JET2
;     1,2,3,$  ;;JET3
;     1,2,3,$  ;;JETsISM2
;     1,2,3,$  ;;JETsISM3
;     1,2,3,$  ;;JETsWIND2
;     1,2,3,$  ;;JETsWIND3
;     1,2,$    ;;DEL ALPHA SO ISM
;     1,2,$    ;;DEL ALPHA SO ISM
;     1,2,$    ;;DEL ALPHA S0 WIND
;     1,2]     ;;DEL ALPHA S0 WIND
  
  n=n_elements(cr)
  ncrs=n_elements(crname)
  a1=intarr(n) & a2=a1 & a3=a1 & a4=a1 & a5=a1 & a6=a1 ;& a7=a1 & a8=a1
;  nwp2=0 & nwp12=0
  for j=0,n-1 do begin 
     
     bin=fltarr(ncrs)
     for i=0,ncrs-1 do tmp=execute('bin[i]=cr[j].('+ntostr(i+6)+')')
     if cr[j].seg1 eq 1 then bin[0:1]=1
     w=where(bin le nsig0,nw)
     if nw gt 0 then begin 
        mfits=model[w]
        sffits=sf[w]
        nufits=nu[w]
;        pfits=p[w]
        wa1=where(mfits eq 1 or mfits eq 3,na1)   ;;ISM models
        wa2=where(mfits eq 2 or mfits eq 3,na2)   ;;Wind models
        wa3=where(sffits eq 1 or sffits eq 3,na3) ;;slow cooling models
        wa4=where(sffits eq 2 or sffits eq 3,na4) ;;fast colling models
        wa5=where(nufits eq 2 or nufits eq 0,na5) ;;nu2
        wa6=where(nufits eq 3 or nufits eq 0,na6) ;;nu3
;        wa7=where(pfits eq 1,na7)                 ;;p>2
;        wa8=where(pfits eq 2,na8)                 ;;1<p<2
        print,'Model fits: ',mfits
        print,'slow/fast:  ',sffits
        print,'nu2/nu3:    ',nufits
;        print,'p>2 / 1<p<2 ',pfits
     endif else begin 
        na1=0 & na2=0 & na3=0 & na4=0 & na5=0 & na6=0 ;& na7=0 & na8=0
     endelse 
     if na1 gt 0 then a1[j]=1
     if na2 gt 0 then a2[j]=1
     if na3 gt 0 then a3[j]=1
     if na4 gt 0 then a4[j]=1
     if na5 gt 0 then a5[j]=1
     if na6 gt 0 then a6[j]=1
;     if na7 gt 0 then a7[j]=1
;     if na8 gt 0 then a8[j]=1
  endfor 
;  print,a1,a2,a3,a4,a5,a6
  ;;;HOW TO CHECK FOR ONLY F->S NO S->F?
  w1=where(a1 eq 0,nw1)                  ;;1=ISM
  w2=where(a2 eq 0,nw2)                  ;;2=WIND
  w3=where(a3 eq 0,nw3)                  ;;3=SLOW
  w4=where(a4 eq 0,nw4)                  ;;4=FAST
  w34=where(a3 eq 1 and a4 eq 1,nw34)    ;;SLOW OR FAST
  w5=where(a5 eq 0,nw5)                  ;;5=NU2
  w6=where(a6 eq 0,nw6)                  ;;6=NU3
;  w7=where(a7 eq 0,nw7)                  ;;7=p2
;  w8=where(a8 eq 0,nw8)                  ;;8=p12
  csm=0
  if nw1 eq 0 then csm=1
  if nw2 eq 0 then csm=2
  if nw1 eq 0 and nw2 eq 0 then csm=3
  if csm eq 0 then csm=3
  
  nus=0
  if nw5 eq 0 then nus=2
  if nw6 eq 0 then nus=3
  if nw5 eq 0 and nw6 eq 0 then nus=4
  if nus eq 0 then nus=4
  
  cool=intarr(n)
  cool[*]=3
;  nus=intarr(n)
  pp=0
  if n gt 1 then begin 
     wslow=where(a4 eq 0 and a3 eq 1,nws)                             ;;where only slow
     if nws gt 0 then if max(wslow) lt n-1 then a4[max(wslow)+1:*]=0  ;;then following fast bad 
     wfast=where(a4 eq 1 and a3 eq 0,nwf)                             ;;where only fast
     if nwf gt 0 then if min(wfast) gt 0 then a3[0:min(wfast)-1]=0    ;;then preceding slow bad
     w3=where(a3 eq 0,nw3)                                            ;;where not slow is fast
     w4=where(a4 eq 0,nw4)                                            ;;where not fast is slow
     w34=where(a3 eq 1 and a4 eq 1,nw34)                              ;;where slow and fast

;     wnu2=where(a5 eq 1 and a6 eq 0,nwnu2)                            ;;where definitely 2
;     if nwnu2 gt 0 then if min(wnu2) gt 0 then a6[0:min(wnu2)-1]=0    ;;then preceding not 3
;     wnu3=where(a5 eq 0 and a6 eq 1,nwnu3)                            ;;where definitely 3
;     if nwnu3 gt 0 then if max(wnu3) lt n-1 then a5[max(wnu3)+1:*]=0  ;;then following not 2
;     w5=where(a5 eq 0,nw5)                                            ;;where not 2 is 3
;     w6=where(a6 eq 0,nw6)                                            ;;where not 3 is 2
     
;     wp2=where(a7 eq 1 and a8 eq 0,nwp2) ;; where definitely p>2
;     wp12=where(a8 eq 1 and a7 eq 0,nwp12) ;; where definitely 1<p<2
;     wpno=where(a7 eq 1 and a8 eq 1,nwno) ;; where both
;     if nwp2 gt 0 then pp=1
;     if nwp12 gt 0 then pp=2
;     if nwp2 eq 0 and nwp12 eq 0 and nwno gt 0 then pp=3
;     print,'pp = ',pp
;     print,'p result ',nwp2,nwp12,nwno
  endif
;  print
;  print,a1,a2,a3,a4,a5,a6
;  print
  if nw3 gt 0 then cool[w3]=2
  if nw4 gt 0 then cool[w4]=1
  if nw34 gt 0 then cool[w34]=3
;  if nw5 gt 0 then nus[w5]=3
;  if nw6 gt 0 then nus[w6]=2
  nsig0=1.
  whighlat1=where(cr.highlat lt nsig0,nwhighlat1)
  whighlat2=where(cr.highlat eq 100.,nwhighlat2)
  if n gt 2 and nwhighlat2 lt n-1 then begin ;; if can see more than 2 seg then first must be high lat
     cr[1:*].highlat=100.
     print,'eliminate: HIGHLAT 2+'
     elim=elim+1
  endif
  if n eq 2 and nwhighlat2 lt n-1 then begin 
     if cr[0].seg1 eq 1 then begin ;;if 1-2
        cr[1].highlat=100.
        print,'eliminate: HIGHLAT in II'
        elim=elim+1
     endif else begin ;;if 2-3
        ;;attempting to only allow naked GRBs to fit highlat in II because it's really a I
        if cr[1].highlat lt nsig0 and cr[1].tbreak lt 1e3 then begin ;;if maybe prompt+1
           cr[0].highlat=100.
           print,'eliminate: HIGHLAT in possible prompt+1'
           elim=elim+1
        endif else begin ;;if really 2-3 or 3-4 no highlat possible
           
           cr.highlat=100.
           print,'eliminate: HIGHLAT in II&III'
           elim=elim+1
        endelse 
     endelse 
  endif 
  
  for j=0,n-1 do begin
     
     bin=fltarr(ncrs)
     for i=0,ncrs-1 do tmp=execute('bin[i]=cr[j].('+ntostr(i+6)+')')
     if cr[j].seg1 eq 1 then bin[0:1]=1
     w=where(bin le nsig0,nw)
     if nw gt 0 then begin 
        ism=where(model[w] eq 1,nism)
        wind=where(model[w] eq 2,nwind)
        slow=where(sf[w] eq 1,nslow)
        fast=where(sf[w] eq 2,nfast)
        nu2=where(nu[w] eq 2,nnu2)
        nu3=where(nu[w] eq 3,nnu3)
;        pp1=where(p[w] eq 1,npp1)
;        pp2=where(p[w] eq 2,npp2)

        if csm eq 1 then begin 
           for i=0,nwind-1 do begin
              tmp=execute('cr[j].('+ntostr(w[wind[i]]+6)+')=100.') ;cr[j].('+ntostr(w[wind[i]]+6)+')-2')
              print,'eliminate: '+ntostr(j+1)+' '+crname[w[wind[i]]]+' WIND'
              elim=elim+1
           endfor
           cr.csm=1 
        endif
        if csm eq 2 then begin 
           for i=0,nism-1 do begin 
;              tmp=execute('cr[j].(w['+ntostr(ism[i])+']+6)=cr[j].(w['+ntostr(ism[i]+6)+']+6)-2')
              tmp=execute('cr[j].('+ntostr(w[ism[i]]+6)+')=100.') ;cr[j].('+ntostr(w[ism[i]]+6)+')-2')
              print,'eliminate: '+ntostr(j+1)+' '+crname[w[ism[i]]]+' ISM'
              elim=elim+1
           endfor 
           cr.csm=2
        endif
        if csm eq 3 then cr[j].csm=3

        if not keyword_set(onlycsm) then begin   
           cr[j].cool=cool[j]
           if cool[j] eq 1 and nfast gt 0 then begin 
              for i=0,nfast-1 do begin
;              tmp=execute('cr[j].(w['+ntostr(fast[i])+']+6)=cr[j].(w['+ntostr(fast[i])+']+6)-3')
                 tmp=execute('cr[j].('+ntostr(w[fast[i]]+6)+')=100.') ;cr[j].('+ntostr(w[fast[i]]+6)+')-3')
                 print,'eliminate: '+ntostr(j+1)+' '+crname[w[fast[i]]]+' fast cool'
                 elim=elim+1
              endfor 
           endif 
           if cool[j] eq 2 and nslow gt 0 then begin 
              for i=0,nslow-1 do begin 
;              tmp=execute('cr[j].(w['+ntostr(slow[i])+']+6)=cr[j].(w['+ntostr(slow[i])+']+6)-3')
                 tmp=execute('cr[j].('+ntostr(w[slow[i]]+6)+')=100.') ;cr[j].('+ntostr(w[slow[i]]+6)+')-3')
                 print,'eliminate: '+ntostr(j)+' '+crname[w[slow[i]]]+' slow cool'
                 elim=elim+1
              endfor 
           endif 
           if not keyword_set(nonu) then begin 
              if nus eq 2 then begin
                 for i=0,nnu3-1 do begin
                    tmp=execute('cr[j].('+ntostr(w[nu3[i]]+6)+')=100.') ;cr[j].('+ntostr(w[nu3[i]]+6)+')-4')
                    print,'eliminate: '+ntostr(j+1)+' '+crname[w[nu3[i]]]+' nu2'
                    elim=elim+1
                 endfor 
                 cr.nu=2
              endif 
              if  nus eq 3 then begin
                 for i=0,nnu2-1 do begin 
                    tmp=execute('cr[j].('+ntostr(w[nu2[i]]+6)+')=100.') ;cr[j].('+ntostr(w[nu2[i]]+6)+')-4')
                    print,'eliminate: '+ntostr(j+1)+' '+crname[w[nu2[i]]]+' nu3'
                    elim=elim+1
                 endfor 
                 cr.nu=3
              endif 
              if nus eq 4 then cr[j].nu=4
           endif 
;        cr[j].nu=nus[j]
;        if nus[j] eq 2 and nnu3 gt 0 then begin
;           for i=0,nnu3-1 do begin 
;;              tmp=execute('cr[j].(w['+ntostr(nu3[i])+']+6)=cr[j].(w['+ntostr(nu3[i])+']+6)-4')
;              tmp=execute('cr[j].('+ntostr(w[nu3[i]]+6)+')=cr[j].('+ntostr(w[nu3[i]]+6)+')-4')
;              print,'eliminate: '+ntostr(j+1)+' '+crname[w[nu3[i]]]
;           endfor
;        endif 
;        if nus[j] eq 3 and nnu2 gt 0 then begin 
;           for i=0,nnu2-1 do begin 
;;              tmp=execute('cr[j].(w['+ntostr(nu2[i])+']+6)=cr[j].(w['+ntostr(nu2[i])+']+6)-4')
;              tmp=execute('cr[j].('+ntostr(w[nu2[i]]+6)+')=cr[j].('+ntostr(w[nu2[i]]+6)+')-4')
;              print,'eliminate: '+ntostr(j+1)+' '+crname[w[nu2[i]]]
;           endfor
;        endif
           
        ;;;CANNOT EMLIMINATE BASED ON P BECAUSE FULL SET OF P<2 RELATIONS ARE NOT INCLUDED
;        if pp eq 1 and npp2 gt 0 then begin
;           for i=0,npp2-1 do begin
;              tmp=execute('cr[j].('+ntostr(w[pp2[i]]+6)+')=100.')
;              print,'eliminate: '+ntostr(j+1)+' '+crname[w[pp2[i]]]+' p>2'
;              elim=elim+1
;           endfor 
;        endif 
;        if pp eq 2 and npp1 gt 0 then begin
;           for i=0,npp1-1 do begin
;              tmp=execute('cr[j].('+ntostr(w[pp1[i]]+6)+')=100.')
;              print,'eliminate: '+ntostr(j+1)+' '+crname[w[pp1[i]]]+' 1<p<2'
;              elim=elim+1
;           endfor 
;        endif 
           
           
        ;;;jetbreak spreading
           if n gt 1 and j eq n-1 then begin 
              if cr[j].jets2a le nsig0 and (cr[j-1].isms2a gt nsig0 and cr[j-1].ismf2a gt nsig0 and $
                                            cr[j-1].winds2a gt nsig0 and cr[j-1].windf2a gt nsig0 and $
                                            cr[j-1].jets2ai gt nsig0 and $
                                            cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0) then begin 
                 cr[j].jets2a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETS2a - same jet ab-ei'
                 elim=elim+1
              endif 
              if cr[j].jets3a le nsig0 and (cr[j-1].isms3a gt nsig0 and cr[j-1].ismf3a gt nsig0 and $
                                            cr[j-1].winds3a gt nsig0 and cr[j-1].windf3a gt nsig0 and $
                                            cr[j-1].jets3ai gt nsig0 and $
                                            cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0) then begin 
                 cr[j].jets3a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETS3a - same jet ab-ei'
                 elim=elim+1
              endif 
              if cr[j].jets2b le nsig0 and (cr[j-1].isms2b gt nsig0 and cr[j-1].ismf2b gt nsig0 and $
                                            cr[j-1].winds2b gt nsig0 and cr[j-1].windf2b gt nsig0 and $
                                            cr[j-1].jets2ai gt nsig0 and $
                                            cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0) then begin 
                 cr[j].jets2b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETS2b - same jet ab-ei'
                 elim=elim+1
              endif 
              if cr[j].jets3b le nsig0 and (cr[j-1].isms3b gt nsig0 and cr[j-1].ismf3b gt nsig0 and $
                                            cr[j-1].winds3b gt nsig0 and cr[j-1].windf3b gt nsig0 and $
                                            cr[j-1].jets3ai gt nsig0 and $
                                            cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0) then begin 
                 cr[j].jets3b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETS3b - same jet ab-ei'
                 elim=elim+1
              endif 
           endif 
           
           
         ;;;;;jetbreak non-spreading must have jet model & previous seg model same
           if n gt 1 and j eq n-1 then begin
              if cr[j].jetsism2a le nsig0 and (cr[j-1].isms2a gt nsig0 and cr[j-1].ismf2a gt nsig0 and $
                                               cr[j-1].isms2b gt nsig0 and cr[j-1].ismf2b gt nsig0 and $
                                               cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0 and $
                                               cr[j-1].jetsism2ai gt nsig0) then begin 
                 cr[j].jetsism2a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSISM2a - not all ISM2a'
                 elim=elim+1
              endif 
              if cr[j].jetsism2b le nsig0 and (cr[j-1].isms2a gt nsig0 and cr[j-1].ismf2a gt nsig0 and $
                                               cr[j-1].isms2b gt nsig0 and cr[j-1].ismf2b gt nsig0 and $
                                               cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0 and $
                                               cr[j-1].jetsism2ai gt nsig0) then begin 
                 cr[j].jetsism2b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSISM2b - not all ISM2b'
                 elim=elim+1
              endif
              if cr[j].jetsism3a le nsig0 and (cr[j-1].isms3a gt nsig0 and cr[j-1].ismf3a gt nsig0 and $
                                               cr[j-1].isms3b gt nsig0 and cr[j-1].ismf3b gt nsig0 and $
                                               cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0 and $
                                               cr[j-1].jetsism3ai gt nsig0) then begin 
                 cr[j].jetsism3a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSISM3a - not all ISM3a'
                 elim=elim+1
              endif 
              if cr[j].jetsism3b le nsig0 and (cr[j-1].isms3a gt nsig0 and cr[j-1].ismf3a gt nsig0 and $
                                               cr[j-1].isms3b gt nsig0 and cr[j-1].ismf3b gt nsig0 and $
                                               cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0 and $
                                               cr[j-1].jetsism3ai gt nsig0) then begin 
                 cr[j].jetsism3b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSISM3b - not all ISM3b'
                 elim=elim+1
              endif 
              if cr[j].jetswind2a le nsig0 and (cr[j-1].winds2a gt nsig0 and cr[j-1].windf2a gt nsig0 and $
                                                cr[j-1].winds2b gt nsig0 and cr[j-1].windf2b gt nsig0 and $
                                                cr[j-1].winds2ai gt nsig0 and cr[j-1].windf2ai gt nsig0 and $
                                                cr[j-1].jetswind2ai gt nsig0) then begin 
                 cr[j].jetswind2a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSWIND2a - not all Wind2a'
                 elim=elim+1
              endif 
              if cr[j].jetswind2b le nsig0 and (cr[j-1].winds2a gt nsig0 and cr[j-1].windf2a gt nsig0 and $
                                                cr[j-1].winds2b gt nsig0 and cr[j-1].windf2b gt nsig0 and $
                                                cr[j-1].winds2ai gt nsig0 and cr[j-1].windf2ai gt nsig0 and $
                                                cr[j-1].jetswind2ai gt nsig0) then begin 
                 cr[j].jetswind2b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSWIND2b - not all Wind2b'
                 elim=elim+1
              endif 
              if cr[j].jetswind3a le nsig0 and (cr[j-1].winds3a gt nsig0 and cr[j-1].windf3a gt nsig0 and $
                                                cr[j-1].winds3b gt nsig0 and cr[j-1].windf3b gt nsig0 and $
                                                cr[j-1].winds3ai gt nsig0 and cr[j-1].windf3ai gt nsig0 and $
                                                cr[j-1].jetswind3ai gt nsig0) then begin 
                 cr[j].jetswind3a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSWIND3a - not all Wind3a'
                 elim=elim+1
              endif 
              if cr[j].jetswind3b le nsig0 and (cr[j-1].winds3a gt nsig0 and cr[j-1].windf3a gt nsig0 and $
                                                cr[j-1].winds3b gt nsig0 and cr[j-1].windf3b gt nsig0 and $
                                                cr[j-1].winds3ai gt nsig0 and cr[j-1].windf3ai gt nsig0 and $
                                                cr[j-1].jetswind3ai gt nsig0) then begin 
                 cr[j].jetswind3b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSWIND3b - not all Wind3b'
                 elim=elim+1
              endif 
              if cr[j].jetsoism2a le nsig0 and (cr[j-1].isms2a gt nsig0 and cr[j-1].ismf2a gt nsig0 and $
                                                cr[j-1].isms2b gt nsig0 and cr[j-1].ismf2b gt nsig0 $and $
                                                $ ;cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0 and $
                                                $ ;cr[j-1].jetsism2ai gt nsig0
                                               ) then begin 
                 cr[j].jetsoism2a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOISM2a - not all ISM2'
                 elim=elim+1
              endif 
              if cr[j].jetsoism2b le nsig0 and (cr[j-1].isms2a gt nsig0 and cr[j-1].ismf2a gt nsig0 and $
                                                cr[j-1].isms2b gt nsig0 and cr[j-1].ismf2b gt nsig0 $ ;and $
                                                $ ;cr[j-1].isms2ai gt nsig0 and cr[j-1].ismf2ai gt nsig0 and $
                                                $ ;cr[j-1].jetsism2ai gt nsig0
                                               ) then begin 
                 cr[j].jetsoism2b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOISM2b - not all ISM2'
                 elim=elim+1
              endif 
              if cr[j].jetsoism3a le nsig0 and (cr[j-1].isms3a gt nsig0 and cr[j-1].ismf3a gt nsig0 and $
                                                cr[j-1].isms3b gt nsig0 and cr[j-1].ismf3b gt nsig0 $ ;and $
                                                $ ;cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0 and $                                           
                                                $ ;cr[j-1].jetsism3ai gt nsig0
                                               ) then begin 
                 cr[j].jetsoism3a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOISM3a - not all ISM3'
                 elim=elim+1
              endif 
              if cr[j].jetsoism3b le nsig0 and (cr[j-1].isms3a gt nsig0 and cr[j-1].ismf3a gt nsig0 and $
                                                cr[j-1].isms3b gt nsig0 and cr[j-1].ismf3b gt nsig0 $ ;and $
                                                $ ;cr[j-1].isms3ai gt nsig0 and cr[j-1].ismf3ai gt nsig0 and $                                           
                                                $ ;cr[j-1].jetsism3ai gt nsig0
                                               ) then begin 
                 cr[j].jetsoism3b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOISM3b - not all ISM3'
                 elim=elim+1
              endif 
              if cr[j].jetsowind2a le nsig0 and (cr[j-1].winds2a gt nsig0 and cr[j-1].windf2a gt nsig0 and $
                                                 cr[j-1].winds2b gt nsig0 and cr[j-1].windf2b gt nsig0 $ ;and $
                                                 $ ;cr[j-1].winds2ai gt nsig0 and cr[j-1].windf2ai gt nsig0 and $                                           
                                                 $ ;cr[j-1].jetswind2ai gt nsig0
                                                ) then begin 
                 cr[j].jetsowind2a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOWIND2a - not all Wind2'
                 elim=elim+1
              endif 
              if cr[j].jetsowind2b le nsig0 and (cr[j-1].winds2a gt nsig0 and cr[j-1].windf2a gt nsig0 and $
                                                 cr[j-1].winds2b gt nsig0 and cr[j-1].windf2b gt nsig0 $ ;and $
                                                 $ ;cr[j-1].winds2ai gt nsig0 and cr[j-1].windf2ai gt nsig0 and $                                           
                                                 $ ;cr[j-1].jetswind2ai gt nsig0
                                                ) then begin 
                 cr[j].jetsowind2b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOWIND2b - not all Wind2'
                 elim=elim+1
              endif 
              if cr[j].jetsowind3a le nsig0 and (cr[j-1].winds3a gt nsig0 and cr[j-1].windf3a gt nsig0 and $
                                                 cr[j-1].winds3b gt nsig0 and cr[j-1].windf3b gt nsig0 $ ;and $
                                                 $ ;cr[j-1].winds3ai gt nsig0 and cr[j-1].windf3ai gt nsig0 and $                                           
                                                 $ ;cr[j-1].jetswind3ai gt nsig0
                                                ) then begin 
                 cr[j].jetsowind3a=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOWIND3a - not all Wind3'
                 elim=elim+1
              endif 
              if cr[j].jetsowind3b le nsig0 and (cr[j-1].winds3a gt nsig0 and cr[j-1].windf3a gt nsig0 and $
                                                 cr[j-1].winds3b gt nsig0 and cr[j-1].windf3b gt nsig0 $ ;and $ 
                                                 $ ;cr[j-1].winds3ai gt nsig0 and cr[j-1].windf3ai gt nsig0 and $                                           
                                                 $ ;cr[j-1].jetswind3ai gt nsig0
                                                ) then begin 
                 cr[j].jetsowind3b=100.
                 print,'eliminate: '+ntostr(j+1)+' JETSOWIND3b - not all Wind3'
                 elim=elim+1
              endif 
           endif 
           
           ;;ENERGY INJECTION + JET BREAK!
           if (n eq 2 and j ge n-1) or (n gt 2 and j ge n-2) then begin ;;is there another seg before j
              if cr[j].jets2ai le nsig0 and ($
                 (cr[j-1].isms2ai gt nsig0 or qconsistent(qstr[j].jets2ai,qstr[j-1].isms2ai,qstr[j].jets2aierr,qstr[j-1].isms2aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].ismf2ai gt nsig0 or qconsistent(qstr[j].jets2ai,qstr[j-1].ismf2ai,qstr[j].jets2aierr,qstr[j-1].ismf2aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].winds2ai gt nsig0 or qconsistent(qstr[j].jets2ai,qstr[j-1].winds2ai,qstr[j].jets2aierr,qstr[j-1].winds2aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].winds2ai gt nsig0 or qconsistent(qstr[j].jets2ai,qstr[j-1].windf2ai,qstr[j].jets2aierr,qstr[j-1].windf2aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jets2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].jets3ai le nsig0 and ($
                 (cr[j-1].isms3ai gt nsig0 or qconsistent(qstr[j].jets3ai,qstr[j-1].isms3ai,qstr[j].jets3aierr,qstr[j-1].isms3aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].ismf3ai gt nsig0 or qconsistent(qstr[j].jets3ai,qstr[j-1].ismf3ai,qstr[j].jets3aierr,qstr[j-1].ismf3aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].winds3ai gt nsig0 or qconsistent(qstr[j].jets3ai,qstr[j-1].winds3ai,qstr[j].jets3aierr,qstr[j-1].winds3aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].winds3ai gt nsig0 or qconsistent(qstr[j].jets3ai,qstr[j-1].windf3ai,qstr[j].jets3aierr,qstr[j-1].windf3aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jets3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs3ai qcon'
                 elim=elim+1
              endif 
              if cr[j].jetsism2ai le nsig0 and ($
                 (cr[j-1].isms2ai gt nsig0 or qconsistent(qstr[j].jetsism2ai,qstr[j-1].isms2ai,qstr[j].jetsism2aierr,qstr[j-1].isms2aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].ismf2ai gt nsig0 or qconsistent(qstr[j].jetsism2ai,qstr[j-1].ismf2ai,qstr[j].jetsism2aierr,qstr[j-1].ismf2aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jetsism2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsism2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].jetsism3ai le nsig0 and ($
                 (cr[j-1].isms3ai gt nsig0 or qconsistent(qstr[j].jetsism3ai,qstr[j-1].isms3ai,qstr[j].jetsism3aierr,qstr[j-1].isms3aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].ismf3ai gt nsig0 or qconsistent(qstr[j].jetsism3ai,qstr[j-1].ismf3ai,qstr[j].jetsism3aierr,qstr[j-1].ismf3aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jetsism3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsism3ai qcon'
                 elim=elim+1
              endif
              if cr[j].jetswind2ai le nsig0 and ($
                 (cr[j-1].winds2ai gt nsig0 or qconsistent(qstr[j].jetswind2ai,qstr[j-1].winds2ai,qstr[j].jetswind2aierr,qstr[j-1].winds2aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].windf2ai gt nsig0 or qconsistent(qstr[j].jetswind2ai,qstr[j-1].windf2ai,qstr[j].jetswind2aierr,qstr[j-1].windf2aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jetswind2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETswind2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].jetswind3ai le nsig0 and ($
                 (cr[j-1].winds3ai gt nsig0 or qconsistent(qstr[j].jetswind3ai,qstr[j-1].winds3ai,qstr[j].jetswind3aierr,qstr[j-1].winds3aierr,nsig=nsig0) eq 0) and $
                 (cr[j-1].windf3ai gt nsig0 or qconsistent(qstr[j].jetswind3ai,qstr[j-1].windf3ai,qstr[j].jetswind3aierr,qstr[j-1].windf3aierr,nsig=nsig0) eq 0)) then begin 
                 cr[j].jetswind3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETswind3ai qcon'
                 elim=elim+1
              endif 
           endif 
        ;;;MORE JET + EI, IF JET+EI THEN NEXT MODEL MUST BE CONSISTENT JET W/ NU, SPREAD, ENVIRON
           if j lt n-1 and n ge 2 then begin 
              ;; spreading jets
              if cr[j].jets2ai le nsig0 and ($
                 cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0) then begin 
                 cr[j].jets2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs2ai same jet'
                 elim=elim+1
              endif 
              if cr[j].jets3ai le nsig0 and ($
                 cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0) then begin 
                 cr[j].jets3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs3ai same jet'
                 elim=elim+1
              endif 
              ;; non-spreading jets
              if cr[j].jetsism2ai le nsig0 and ($
                 cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0) then begin 
                 cr[j].jetsism2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsISM2ai same jet'
                 elim=elim+1
              endif 
              if cr[j].jetsism3ai le nsig0 and ($
                 cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0) then begin 
                 cr[j].jetsism3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsISM3ai same jet'
                 elim=elim+1
              endif 
              if cr[j].jetswind2ai le nsig0 and ($
                 cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0) then begin 
                 cr[j].jetswind2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsWIND2ai same jet'
                 elim=elim+1
              endif 
              if cr[j].jetswind3ai le nsig0 and ($
                 cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0) then begin 
                 cr[j].jetswind3ai=100.0
                 print,'eliminate: '+ntostr(j+1)+' JETsWIND3ai same jet'
                 elim=elim+1
              endif 
           endif 
           
           
           ;;IF N+EI GOES ONLY TO EI, THEN QCONSISTENT
           if j lt n-1 and n ge 2 then begin 
           ;;; slow cooling
              if cr[j].isms2ai le nsig0 and ($
                 cr[j+1].isms2a gt nsig0 and cr[j+1].isms2b gt nsig0 and $
                 cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0 and cr[j+1].jetsoism2a gt nsig0 and $
                 (((cr[j+1].jets2ai le nsig0 and qconsistent(qstr[j].isms2ai,qstr[j+1].jets2ai,qstr[j].isms2aierr,qstr[j+1].jets2aierr,nsig=nsig0) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetsism2ai le nsig0 and qconsistent(qstr[j].isms2ai,qstr[j+1].jetsism2ai,qstr[j].isms2aierr,qstr[j+1].jetsism2aierr,nsig=nsig0) eq 0) or cr[j+1].jetsism2ai gt nsig0))) then begin 
                 cr[j].isms2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].isms3ai le nsig0 and ($
                 cr[j+1].isms3a gt nsig0 and cr[j+1].isms3b gt nsig0 and $
                 cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and cr[j+1].jetsoism3a gt nsig0 and $
                 (((cr[j+1].jets3ai le nsig0 and qconsistent(qstr[j].isms3ai,qstr[j+1].jets3ai,qstr[j].isms3aierr,qstr[j+1].jets3aierr,nsig=nsig0) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetsism3ai le nsig0 and qconsistent(qstr[j].isms3ai,qstr[j+1].jetsism3ai,qstr[j].isms3aierr,qstr[j+1].jetsism3aierr,nsig=nsig0) eq 0) or cr[j+1].jetsism3ai gt nsig0))) then begin 
                 cr[j].isms3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3ai qcon'
                 elim=elim+1
              endif 
              if cr[j].winds2ai le nsig0 and ($
                 cr[j+1].winds2a gt nsig0 and cr[j+1].winds2b gt nsig0 and $
                 cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and cr[j+1].jetsowind2a gt nsig0 and $
                 (((cr[j+1].jets2ai le nsig0 and qconsistent(qstr[j].winds2ai,qstr[j+1].jets2ai,qstr[j].winds2aierr,qstr[j+1].jets2aierr,nsig=nsig0) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetswind2ai le nsig0 and qconsistent(qstr[j].winds2ai,qstr[j+1].jetswind2ai,qstr[j].winds2aierr,qstr[j+1].jetswind2aierr,nsig=nsig0) eq 0) or cr[j+1].jetswind2ai gt nsig0))) then begin 
                 cr[j].winds2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].winds3ai le nsig0 and ($
                 cr[j+1].winds3a gt nsig0 and cr[j+1].winds3b gt nsig0 and $
                 cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and cr[j+1].jetsowind3a gt nsig0 and $
                 (((cr[j+1].jets3ai le nsig0 and qconsistent(qstr[j].winds3ai,qstr[j+1].jets3ai,qstr[j].winds3aierr,qstr[j+1].jets3aierr,nsig=nsig0) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetswind3ai le nsig0 and qconsistent(qstr[j].winds3ai,qstr[j+1].jetswind3ai,qstr[j].winds3aierr,qstr[j+1].jetswind3aierr,nsig=nsig0) eq 0) or cr[j+1].jetswind3ai gt nsig0))) then begin 
                 cr[j].winds3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3ai qcon'
                 elim=elim+1
              endif 
           ;;; fast cooling
              if cr[j].ismf2ai le nsig0 and ($
                 cr[j+1].isms2a gt nsig0 and cr[j+1].ismf2a gt nsig0 and $
                 cr[j+1].isms2b gt nsig0 and cr[j+1].ismf2b gt 0 and $
                 cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0 and cr[j+1].jetsoism2a gt nsig0 and $
                 (((cr[j+1].jets2ai le nsig0 and qconsistent(qstr[j].ismf2ai,qstr[j+1].jets2ai,qstr[j].ismf2aierr,qstr[j+1].jets2aierr,nsig=nsig0) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetsism2ai le nsig0 and qconsistent(qstr[j].ismf2ai,qstr[j+1].jetsism2ai,qstr[j].ismf2aierr,qstr[j+1].jetsism2aierr,nsig=nsig0) eq 0) or cr[j+1].jetsism2ai gt nsig0))) then begin 
                 cr[j].ismf2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].ismf3ai le nsig0 and ($
                 cr[j+1].isms3a gt nsig0 and cr[j+1].ismf3a gt nsig0 and $
                 cr[j+1].isms3b gt nsig0 and cr[j+1].ismf3b gt 0 and $
                 cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and cr[j+1].jetsoism3a gt nsig0 and $
                 (((cr[j+1].jets3ai le nsig0 and qconsistent(qstr[j].ismf3ai,qstr[j+1].jets3ai,qstr[j].ismf3aierr,qstr[j+1].jets3aierr,nsig=nsig0) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetsism3ai le nsig0 and qconsistent(qstr[j].ismf3ai,qstr[j+1].jetsism3ai,qstr[j].ismf3aierr,qstr[j+1].jetsism3aierr,nsig=nsig0) eq 0) or cr[j+1].jetsism3ai gt nsig0))) then begin 
                 cr[j].ismf3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3ai qcon'
                 elim=elim+1
              endif 
              if cr[j].windf2ai le nsig0 and ($
                 cr[j+1].winds2a gt nsig0 and cr[j+1].windf2a gt nsig0 and $
                 cr[j+1].winds2b gt nsig0 and cr[j+1].windf2b gt 0 and $
                 cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and cr[j+1].jetsowind2a gt nsig0 and $
                 (((cr[j+1].jets2ai le nsig0 and qconsistent(qstr[j].windf2ai,qstr[j+1].jets2ai,qstr[j].windf2aierr,qstr[j+1].jets2aierr,nsig=nsig0) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetswind2ai le nsig0 and qconsistent(qstr[j].windf2ai,qstr[j+1].jetswind2ai,qstr[j].windf2aierr,qstr[j+1].jetswind2aierr,nsig=nsig0) eq 0) or cr[j+1].jetswind2ai gt nsig0))) then begin 
                 cr[j].windf2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf2ai qcon'
                 elim=elim+1
              endif 
              if cr[j].windf3ai le nsig0 and ($
                 cr[j+1].winds3a gt nsig0 and cr[j+1].windf3a gt nsig0 and $
                 cr[j+1].winds3b gt nsig0 and cr[j+1].windf3b gt 0 and $
                 cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and cr[j+1].jetsowind3a gt nsig0 and $
                 (((cr[j+1].jets3ai le nsig0 and qconsistent(qstr[j].windf3ai,qstr[j+1].jets3ai,qstr[j].windf3aierr,qstr[j+1].jets3aierr,nsig=nsig0) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetswind3ai le nsig0 and qconsistent(qstr[j].windf3ai,qstr[j+1].jetswind3ai,qstr[j].windf3aierr,qstr[j+1].jetswind3aierr,nsig=nsig0) eq 0) or cr[j+1].jetswind3ai gt nsig0))) then begin 
                 cr[j].windf3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3ai qcon'
                 elim=elim+1
              endif 
           endif 
           
           ;;ENERGY INJECTION + NORMAL
           ;;CAN GO DIRECTLY TO JB
           if n gt 1 and n-j gt 1 then begin  ;;is there another seg after j
              ;;slow (with only slow after)
              if cr[j].isms2ai le nsig0 and $
                 (cr[j+1].isms2a gt nsig0 and cr[j+1].isms2b gt nsig0 and $
                  cr[j+1].jets2ai gt nsig0 and cr[j+1].jetsism2ai gt nsig0 $ ;and $
;               cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
;               cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0 and $
;               cr[j+1].jetsoism2a gt nsig0
                 ) then begin
                 cr[j].isms2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2ai - not all ISM2'
                 elim=elim+1
              endif 
              if cr[j].winds2ai le nsig0 and $
                 (cr[j+1].winds2a gt nsig0 and cr[j+1].winds2b gt nsig0 and $
                  cr[j+1].jets2ai gt nsig0 and cr[j+1].jetswind2ai gt nsig0 $ ;and $
;               cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
;               cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and $
;               cr[j+1].jetsowind2a gt nsig0
                 ) then begin
                 cr[j].winds2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2ai - not all Wind2'
                 elim=elim+1
              endif 
              if cr[j].isms3ai le nsig0 and $
                 (cr[j+1].isms3a gt nsig0 and cr[j+1].isms3b gt nsig0 and $
                  cr[j+1].jets3ai gt nsig0 and cr[j+1].jetsism3ai gt nsig0 $ ;and $
;               cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
;               cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and $
;               cr[j+1].jetsoism3a gt nsig0
                 ) then begin
                 cr[j].isms3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3ai - not all ISM3'
                 elim=elim+1
              endif 
              if cr[j].winds3ai le nsig0 and $
                 (cr[j+1].winds3a gt nsig0 and cr[j+1].winds3b gt nsig0 and $
                  cr[j+1].jets3ai gt nsig0 and cr[j+1].jetswind3ai gt nsig0 $ ;and $
;               cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
;               cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and $
;               cr[j+1].jetsowind3a gt nsig0
                 ) then begin
                 cr[j].winds3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3ai - not all Wind3'
                 elim=elim+1
              endif 
              ;;fast (with fast or slow after)
              if cr[j].ismf2ai le nsig0 and $
                 (cr[j+1].isms2a gt nsig0 and cr[j+1].isms2b gt nsig0 and $
                  cr[j+1].ismf2a gt nsig0 and cr[j+1].ismf2b gt nsig0 and $
                  cr[j+1].jets2ai gt nsig0 and cr[j+1].jetsism2ai gt nsig0 $ ;and $
;               cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
;               cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0 and $
;               cr[j+1].jetsoism2a gt nsig0
                 ) then begin
                 cr[j].ismf2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf2ai - not all fast bf slow'
                 elim=elim+1
              endif 
              if cr[j].windf2ai le nsig0 and $
                 (cr[j+1].winds2a gt nsig0 and cr[j+1].winds2b gt nsig0 and $
                  cr[j+1].windf2a gt nsig0 and cr[j+1].windf2b gt nsig0 and $
                  cr[j+1].jets2ai gt nsig0 and cr[j+1].jetswind2ai gt nsig0 $ ;and $
                                ;              cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
                                ;              cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and $
                                ;              cr[j+1].jetsowind2a gt nsig0
                 ) then begin
                 cr[j].windf2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf2ai - not all fast bf slow'
                 elim=elim+1
              endif 
              if cr[j].ismf3ai le nsig0 and $
                 (cr[j+1].isms3a gt nsig0 and cr[j+1].isms3b gt nsig0 and $
                  cr[j+1].ismf3a gt nsig0 and cr[j+1].ismf3b gt nsig0 and $
                  cr[j+1].jets3ai gt nsig0 and cr[j+1].jetsism3ai gt nsig0 $ ;and $
;               cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
;               cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and $
;               cr[j+1].jetsoism3a gt nsig0
                 ) then begin
                 cr[j].ismf3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3ai - not all fast bf slow'
                 elim=elim+1
              endif 
              if cr[j].windf3ai le nsig0 and $
                 (cr[j+1].winds3a gt nsig0 and cr[j+1].winds3b gt nsig0 and $
                  cr[j+1].windf3a gt nsig0 and cr[j+1].windf3b gt nsig0 and $
                  cr[j+1].jets3ai gt nsig0 and cr[j+1].jetswind3ai gt nsig0 $ ;and $
;               cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
;               cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and $
;               cr[j+1].jetsowind3a gt nsig0
                 ) then begin
                 cr[j].windf3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3ai - not all fast bf slow'
                 elim=elim+1
              endif 
              
           ;;;Normal decay must be followed by normal JB
              if cr[j].isms2a le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
                  cr[j+1].jetsism2a gt nsig0 and  cr[j+1].jetsism2b gt nsig0 and $
                  cr[j+1].jetsoism2a gt nsig0 and cr[j+1].jetsoism2b gt nsig0) then begin
                 cr[j].isms2a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2a - not IE cutoff'
                 elim=elim+1
              endif
              if cr[j].isms2b le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and $
                  cr[j+1].jets2b gt nsig0 and cr[j+1].jetsism2a gt nsig0 and $
                  cr[j+1].jetsism2b gt nsig0 and cr[j+1].jetsoism2a gt nsig0 and $
                  cr[j+1].jetsoism2b gt nsig0) then begin
                 cr[j].isms2b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].isms3a le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
                  cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and $
                  cr[j+1].jetsoism3a gt nsig0 and cr[j+1].jetsoism3b gt nsig0) then begin
                 cr[j].isms3a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].isms3b le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and $
                  cr[j+1].jets3b gt nsig0 and cr[j+1].jetsism3a gt nsig0 and $
                  cr[j+1].jetsism3b gt nsig0 and cr[j+1].jetsoism3a gt nsig0 and $ 
                  cr[j+1].jetsoism3b gt nsig0) then begin
                 cr[j].isms3b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].ismf2a le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
                  cr[j+1].jetsism2a gt nsig0 and cr[j+1].jetsism2b gt nsig0 and $
                  cr[j+1].jetsoism2a gt nsig0 and cr[j+1].jetsoism2b gt nsig0) then begin
                 cr[j].ismf2a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf2a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].ismf2b le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and $
                  cr[j+1].jets2b gt nsig0 and cr[j+1].jetsism2a gt nsig0 and $
                  cr[j+1].jetsism2b gt nsig0 and cr[j+1].jetsoism2a gt nsig0 and $
                  cr[j+1].jetsoism2b gt nsig0) then begin
                 cr[j].ismf2b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf2b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].ismf3a le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
                  cr[j+1].jetsism3a gt nsig0 and cr[j+1].jetsism3b gt nsig0 and $
                  cr[j+1].jetsoism3a gt nsig0 and cr[j+1].jetsoism3b gt nsig0) then begin
                 cr[j].ismf3a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].ismf3b le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and $
                  cr[j+1].jets3b gt nsig0 and cr[j+1].jetsism3a gt nsig0 and $
                  cr[j+1].jetsism3b gt nsig0 and cr[j+1].jetsoism3a gt nsig0 and $
                  cr[j+1].jetsoism3b gt nsig0) then begin
                 cr[j].ismf3b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].winds2a le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
                  cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and $
                  cr[j+1].jetsowind2a gt nsig0 and cr[j+1].jetsowind2b gt nsig0) then begin
                 cr[j].winds2a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].winds2b le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and $
                  cr[j+1].jets2b gt nsig0 and cr[j+1].jetswind2a gt nsig0 and $
                  cr[j+1].jetswind2b gt nsig0 and cr[j+1].jetsowind2a gt nsig0 and $
                  cr[j+1].jetsowind2b gt nsig0) then begin
                 cr[j].winds2b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].winds3a le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
                  cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and $
                  cr[j+1].jetsowind3a gt nsig0 and cr[j+1].jetsowind3b gt nsig0) then begin
                 cr[j].winds3a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].winds3b le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and $
                  cr[j+1].jets3b gt nsig0 and cr[j+1].jetswind3a gt nsig0 and $
                  cr[j+1].jetswind3b gt nsig0 and cr[j+1].jetsowind3a gt nsig0 and $
                  cr[j+1].jetsowind3b gt nsig0) then begin
                 cr[j].winds3b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].windf2a le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and cr[j+1].jets2b gt nsig0 and $
                  cr[j+1].jetswind2a gt nsig0 and cr[j+1].jetswind2b gt nsig0 and $
                  cr[j+1].jetsowind2a gt nsig0 and cr[j+1].jetsowind2b gt nsig0) then begin
                 cr[j].windf2a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf2a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].windf2b le nsig0 and $
                 (cr[j+1].jets2a gt nsig0 and $
                  cr[j+1].jets2b gt nsig0 and cr[j+1].jetswind2a gt nsig0 and $
                  cr[j+1].jetswind2b gt nsig0 and cr[j+1].jetsowind2a gt nsig0 and $
                  cr[j+1].jetsowind2b gt nsig0) then begin
                 cr[j].windf2b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf2b - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].windf3a le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and cr[j+1].jets3b gt nsig0 and $
                  cr[j+1].jetswind3a gt nsig0 and cr[j+1].jetswind3b gt nsig0 and $
                  cr[j+1].jetsowind3a gt nsig0 and cr[j+1].jetsowind3b gt nsig0) then begin
                 cr[j].windf3a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3a - not IE cutoff'
                 elim=elim+1
              endif 
              if cr[j].windf3b le nsig0 and $
                 (cr[j+1].jets3a gt nsig0 and $
                  cr[j+1].jets3b gt nsig0 and cr[j+1].jetswind3a gt nsig0 and $
                  cr[j+1].jetswind3b gt nsig0 and cr[j+1].jetsowind3a gt nsig0 and $
                  cr[j+1].jetsowind3b gt nsig0) then begin
                 cr[j].windf3b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3b - not IE cutoff'
                 elim=elim+1
              endif 
           endif   
           
           ;;P CONSISTENT
           
           ncr=52
           if n gt 1 and j lt n-1 then begin
              ;;where p=2b+1
              ;; get compatible relations
              ;; check if p is consistent for each, if none consistent throw out
           ;;;NO FAST NU2
           ;;;CASES OF III-IV NO EI
              if cr[j].isms2a le nsig0 and ($
                 (((cr[j+1].jets2a le nsig0 and qconsistent(pstr[j].isms2a,pstr[j+1].jets2a,pstr[j].isms2aerr,pstr[j+1].jets2aerr,nsig=nsig0) eq 0) or cr[j+1].jets2a gt nsig0) and ((cr[j+1].jetsism2a le nsig0 and qconsistent(pstr[j].isms2a,pstr[j+1].jetsism2a,pstr[j].isms2aerr,pstr[j+1].jetsism2aerr,nsig=nsig0) eq 0) or cr[j+1].jetsism2a gt nsig0) and ((cr[j+1].jetsoism2a le nsig0 and qconsistent(pstr[j].isms2a,pstr[j+1].jetsoism2a,pstr[j].isms2aerr,pstr[j+1].jetsoism2aerr,nsig=nsig0) eq 0) or cr[j+1].jetsoism2a gt nsig0))) then begin 
                 cr[j].isms2a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2a pcon'
                 elim=elim+1
              endif 
              if cr[j].isms3a le nsig0 and ($
                 (((cr[j+1].jets3a le nsig0 and qconsistent(pstr[j].isms3a,pstr[j+1].jets3a,pstr[j].isms3aerr,pstr[j+1].jets3aerr,nsig=nsig0) eq 0) or cr[j+1].jets3a gt nsig0) and ((cr[j+1].jetsism3a le nsig0 and qconsistent(pstr[j].isms3a,pstr[j+1].jetsism3a,pstr[j].isms3aerr,pstr[j+1].jetsism3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsism3a gt nsig0) and ((cr[j+1].jetsoism3a le nsig0 and qconsistent(pstr[j].isms3a,pstr[j+1].jetsoism3a,pstr[j].isms3aerr,pstr[j+1].jetsoism3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsoism3a gt nsig0))) then begin 
                 cr[j].isms3a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3a pcon'
                 elim=elim+1
              endif 
              if cr[j].isms2b le nsig0 and ($
                 (((cr[j+1].jets2b le nsig0 and qconsistent(pstr[j].isms2b,pstr[j+1].jets2b,pstr[j].isms2berr,pstr[j+1].jets2berr,nsig=nsig0) eq 0) or cr[j+1].jets2b gt nsig0) and ((cr[j+1].jetsism2b le nsig0 and qconsistent(pstr[j].isms2b,pstr[j+1].jetsism2b,pstr[j].isms2berr,pstr[j+1].jetsism2berr,nsig=nsig0) eq 0) or cr[j+1].jetsism2b gt nsig0) and ((cr[j+1].jetsoism2a le nsig0 and qconsistent(pstr[j].isms2b,pstr[j+1].jetsoism2a,pstr[j].isms2berr,pstr[j+1].jetsoism2aerr,nsig=nsig0) eq 0) or cr[j+1].jetsoism2a gt nsig0))) then begin 
                 cr[j].isms2b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2b pcon'
                 elim=elim+1
              endif 
              if cr[j].isms3b le nsig0 and ($
                 (((cr[j+1].jets3b le nsig0 and qconsistent(pstr[j].isms3b,pstr[j+1].jets3b,pstr[j].isms3berr,pstr[j+1].jets3berr,nsig=nsig0) eq 0) or cr[j+1].jets3b gt nsig0) and ((cr[j+1].jetsism3b le nsig0 and qconsistent(pstr[j].isms3b,pstr[j+1].jetsism3b,pstr[j].isms3berr,pstr[j+1].jetsism3berr,nsig=nsig0) eq 0) or cr[j+1].jetsism3b gt nsig0) and ((cr[j+1].jetsoism3a le nsig0 and qconsistent(pstr[j].isms3b,pstr[j+1].jetsoism3a,pstr[j].isms3berr,pstr[j+1].jetsoism3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsoism3a gt nsig0))) then begin 
                 cr[j].isms3b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3b pcon'
                 elim=elim+1
              endif 
              if cr[j].ismf3a le nsig0 and ($
                 (((cr[j+1].jets3a le nsig0 and qconsistent(pstr[j].ismf3a,pstr[j+1].jets3a,pstr[j].ismf3aerr,pstr[j+1].jets3aerr,nsig=nsig0) eq 0) or cr[j+1].jets3a gt nsig0) and ((cr[j+1].jetsism3a le nsig0 and qconsistent(pstr[j].ismf3a,pstr[j+1].jetsism3a,pstr[j].ismf3aerr,pstr[j+1].jetsism3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsism3a gt nsig0))) then begin 
                 cr[j].ismf3a=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3a pcon'
                 elim=elim+1
              endif 
              if cr[j].ismf3b le nsig0 and ($
                 (((cr[j+1].jets3b le nsig0 and qconsistent(pstr[j].ismf3b,pstr[j+1].jets3b,pstr[j].ismf3berr,pstr[j+1].jets3berr,nsig=nsig0) eq 0) or cr[j+1].jets3b gt nsig0) and ((cr[j+1].jetsism3b le nsig0 and qconsistent(pstr[j].ismf3b,pstr[j+1].jetsism3b,pstr[j].ismf3berr,pstr[j+1].jetsism3berr,nsig=nsig0) eq 0) or cr[j+1].jetsism3b gt nsig0))) then begin 
                 cr[j].ismf3b=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMf3b pcon'
                 elim=elim+1
              endif 
              if cr[j].winds2a le nsig0 and ($
                 (((cr[j+1].jets2a le nsig0 and qconsistent(pstr[j].winds2a,pstr[j+1].jets2a,pstr[j].winds2aerr,pstr[j+1].jets2aerr,nsig=nsig0) eq 0) or cr[j+1].jets2a gt nsig0) and ((cr[j+1].jetswind2a le nsig0 and qconsistent(pstr[j].winds2a,pstr[j+1].jetswind2a,pstr[j].winds2aerr,pstr[j+1].jetswind2aerr,nsig=nsig0) eq 0) or cr[j+1].jetswind2a gt nsig0) and ((cr[j+1].jetsowind2a le nsig0 and qconsistent(pstr[j].winds2a,pstr[j+1].jetsowind2a,pstr[j].winds2aerr,pstr[j+1].jetsowind2aerr,nsig=nsig0) eq 0) or cr[j+1].jetsowind2a gt nsig0))) then begin 
                 cr[j].winds2a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2a pcon'
                 elim=elim+1
              endif 
              if cr[j].winds3a le nsig0 and ($
                 (((cr[j+1].jets3a le nsig0 and qconsistent(pstr[j].winds3a,pstr[j+1].jets3a,pstr[j].winds3aerr,pstr[j+1].jets3aerr,nsig=nsig0) eq 0) or cr[j+1].jets3a gt nsig0) and ((cr[j+1].jetswind3a le nsig0 and qconsistent(pstr[j].winds3a,pstr[j+1].jetswind3a,pstr[j].winds3aerr,pstr[j+1].jetswind3aerr,nsig=nsig0) eq 0) or cr[j+1].jetswind3a gt nsig0) and ((cr[j+1].jetsowind3a le nsig0 and qconsistent(pstr[j].winds3a,pstr[j+1].jetsowind3a,pstr[j].winds3aerr,pstr[j+1].jetsowind3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsowind3a gt nsig0))) then begin 
                 cr[j].winds3a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3a pcon'
                 elim=elim+1
              endif 
              if cr[j].winds2b le nsig0 and ($
                 (((cr[j+1].jets2b le nsig0 and qconsistent(pstr[j].winds2b,pstr[j+1].jets2b,pstr[j].winds2berr,pstr[j+1].jets2berr,nsig=nsig0) eq 0) or cr[j+1].jets2b gt nsig0) and ((cr[j+1].jetswind2b le nsig0 and qconsistent(pstr[j].winds2b,pstr[j+1].jetswind2b,pstr[j].winds2berr,pstr[j+1].jetswind2berr,nsig=nsig0) eq 0) or cr[j+1].jetswind2b gt nsig0) and ((cr[j+1].jetsowind2a le nsig0 and qconsistent(pstr[j].winds2b,pstr[j+1].jetsowind2a,pstr[j].winds2berr,pstr[j+1].jetsowind2aerr,nsig=nsig0) eq 0) or cr[j+1].jetsowind2a gt nsig0))) then begin 
                 cr[j].winds2b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2b pcon'
                 elim=elim+1
              endif 
              if cr[j].winds3b le nsig0 and ($
                 (((cr[j+1].jets3b le nsig0 and qconsistent(pstr[j].winds3b,pstr[j+1].jets3b,pstr[j].winds3berr,pstr[j+1].jets3berr,nsig=nsig0) eq 0) or cr[j+1].jets3b gt nsig0) and ((cr[j+1].jetswind3b le nsig0 and qconsistent(pstr[j].winds3b,pstr[j+1].jetswind3b,pstr[j].winds3berr,pstr[j+1].jetswind3berr,nsig=nsig0) eq 0) or cr[j+1].jetswind3b gt nsig0) and ((cr[j+1].jetsowind3a le nsig0 and qconsistent(pstr[j].winds3b,pstr[j+1].jetsowind3a,pstr[j].winds3berr,pstr[j+1].jetsowind3aerr,nsig=nsig0) eq 0) or cr[j+1].jetsowind3a gt nsig0))) then begin 
                 cr[j].winds3b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3b pcon'
                 elim=elim+1
              endif 
              if cr[j].windf3a le nsig0 and ($
                 (((cr[j+1].jets3a le nsig0 and qconsistent(pstr[j].windf3a,pstr[j+1].jets3a,pstr[j].windf3aerr,pstr[j+1].jets3aerr,nsig=nsig0) eq 0) or cr[j+1].jets3a gt nsig0) and ((cr[j+1].jetswind3a le nsig0 and qconsistent(pstr[j].windf3a,pstr[j+1].jetswind3a,pstr[j].windf3aerr,pstr[j+1].jetswind3aerr,nsig=nsig0) eq 0) or cr[j+1].jetswind3a gt nsig0))) then begin 
                 cr[j].windf3a=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3a pcon'
                 elim=elim+1
              endif 
              if cr[j].windf3b le nsig0 and ($
                 (((cr[j+1].jets3b le nsig0 and qconsistent(pstr[j].windf3b,pstr[j+1].jets3b,pstr[j].windf3berr,pstr[j+1].jets3berr,nsig=nsig0) eq 0) or cr[j+1].jets3b gt nsig0) and ((cr[j+1].jetswind3b le nsig0 and qconsistent(pstr[j].windf3b,pstr[j+1].jetswind3b,pstr[j].windf3berr,pstr[j+1].jetswind3berr,nsig=nsig0) eq 0) or cr[j+1].jetswind3b gt nsig0))) then begin 
                 cr[j].windf3b=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDf3b pcon'
                 elim=elim+1
              endif 
;           goto,skippcon
           ;;;NOW FOR CASES WITH EI III-IV
              if cr[j].jets2ai le nsig0 and ($
                 (((cr[j+1].jets2a le nsig0 and qconsistent(pstr[j].jets2ai,pstr[j+1].jets2a,pstr[j].jets2aierr,pstr[j+1].jets2aerr) eq 0) or cr[j+1].jets2a gt nsig0) and ((cr[j+1].jets2b le nsig0 and qconsistent(pstr[j].jets2ai,pstr[j+1].jets2b,pstr[j].jets2aierr,pstr[j+1].jets2berr) eq 0) or cr[j+1].jets2b gt nsig0))) then begin 
                 cr[j].jets2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs2ai pcon'
                 elim=elim+1
              endif 
              if cr[j].jets3ai le nsig0 and ($
                 (((cr[j+1].jets3a le nsig0 and qconsistent(pstr[j].jets3ai,pstr[j+1].jets3a,pstr[j].jets3aierr,pstr[j+1].jets3aerr) eq 0) or cr[j+1].jets3a gt nsig0) and ((cr[j+1].jets3b le nsig0 and qconsistent(pstr[j].jets3ai,pstr[j+1].jets3b,pstr[j].jets3aierr,pstr[j+1].jets3berr) eq 0) or cr[j+1].jets3b gt nsig0))) then begin 
                 cr[j].jets3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETs3ai pcon'
                 elim=elim+1
              endif 
              if cr[j].jetsism2ai le nsig0 and ($
                 (((cr[j+1].jetsism2a le nsig0 and qconsistent(pstr[j].jetsism2ai,pstr[j+1].jetsism2a,pstr[j].jetsism2aierr,pstr[j+1].jetsism2aerr) eq 0) or cr[j+1].jetsism2a gt nsig0) and ((cr[j+1].jetsism2b le nsig0 and qconsistent(pstr[j].jetsism2ai,pstr[j+1].jetsism2b,pstr[j].jetsism2aierr,pstr[j+1].jetsism2berr) eq 0) or cr[j+1].jetsism2b gt nsig0))) then begin 
                 cr[j].jetsism2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsISM2ai pcon'
                 elim=elim+1
              endif 
              if cr[j].jetsism3ai le nsig0 and ($
                 (((cr[j+1].jetsism3a le nsig0 and qconsistent(pstr[j].jetsism3ai,pstr[j+1].jetsism3a,pstr[j].jetsism3aierr,pstr[j+1].jetsism3aerr) eq 0) or cr[j+1].jetsism3a gt nsig0) and ((cr[j+1].jetsism3b le nsig0 and qconsistent(pstr[j].jetsism3ai,pstr[j+1].jetsism3b,pstr[j].jetsism3aierr,pstr[j+1].jetsism3berr) eq 0) or cr[j+1].jetsism3b gt nsig0))) then begin 
                 cr[j].jetsism3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsISM3ai pcon'
                 elim=elim+1
              endif 
              if cr[j].jetswind2ai le nsig0 and ($
                 (((cr[j+1].jetswind2a le nsig0 and qconsistent(pstr[j].jetswind2ai,pstr[j+1].jetswind2a,pstr[j].jetswind2aierr,pstr[j+1].jetswind2aerr) eq 0) or cr[j+1].jetswind2a gt nsig0) and ((cr[j+1].jetswind2b le nsig0 and qconsistent(pstr[j].jetswind2ai,pstr[j+1].jetswind2b,pstr[j].jetswind2aierr,pstr[j+1].jetswind2berr) eq 0) or cr[j+1].jetswind2b gt nsig0))) then begin 
                 cr[j].jetswind2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsWIND2ai pcon'
                 elim=elim+1
              endif 
              if cr[j].jetswind3ai le nsig0 and ($
                 (((cr[j+1].jetswind3a le nsig0 and qconsistent(pstr[j].jetswind3ai,pstr[j+1].jetswind3a,pstr[j].jetswind3aierr,pstr[j+1].jetswind3aerr) eq 0) or cr[j+1].jetswind3a gt nsig0) and ((cr[j+1].jetswind3b le nsig0 and qconsistent(pstr[j].jetswind3ai,pstr[j+1].jetswind3b,pstr[j].jetswind3aierr,pstr[j+1].jetswind3berr) eq 0) or cr[j+1].jetswind3b gt nsig0))) then begin 
                 cr[j].jetswind3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' JETsWIND3ai pcon'
                 elim=elim+1
              endif 
           ;;;CASES OF II-III

              if cr[j].isms2ai le nsig0 and ($
                 (((cr[j+1].isms2a le nsig0 and qconsistent(pstr[j].isms2ai,pstr[j+1].isms2a,pstr[j].isms2aierr,pstr[j+1].isms2aerr) eq 0) or cr[j+1].isms2a gt nsig0) and ((cr[j+1].isms2b le nsig0 and qconsistent(pstr[j].isms2ai,pstr[j+1].isms2b,pstr[j].isms2aierr,pstr[j+1].isms2berr) eq 0) or cr[j+1].isms2b gt nsig0) and ((cr[j+1].jets2ai le nsig0 and qconsistent(pstr[j].isms2ai,pstr[j+1].jets2ai,pstr[j].isms2aierr,pstr[j+1].jets2aierr) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetsism2ai le nsig0 and qconsistent(pstr[j].isms2ai,pstr[j+1].jetsism2ai,pstr[j].isms2aierr,pstr[j+1].jetsism2aierr) eq 0) or cr[j+1].jetsism2ai gt nsig0))) then begin 
                 cr[j].isms2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs2ai pcon'
                 elim=elim+1
              endif 
              if cr[j].isms3ai le nsig0 and ($
                 (((cr[j+1].isms3a le nsig0 and qconsistent(pstr[j].isms3ai,pstr[j+1].isms3a,pstr[j].isms3aierr,pstr[j].isms3aerr) eq 0) or cr[j+1].isms3a gt nsig0) and ((cr[j+1].isms3b le nsig0 and qconsistent(pstr[j].isms3ai,pstr[j+1].isms3b,pstr[j].isms3aierr,pstr[j].isms3berr) eq 0) or cr[j+1].isms3b gt nsig0) and ((cr[j+1].jets3ai le nsig0 and qconsistent(pstr[j].isms3ai,pstr[j+1].jets3ai,pstr[j].isms3aierr,pstr[j].jets3aierr) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetsism3ai le nsig0 and qconsistent(pstr[j].isms3ai,pstr[j+1].jetsism3ai,pstr[j].isms3aierr,pstr[j].jetsism3aierr) eq 0) or cr[j+1].jetsism3ai gt nsig0))) then begin 
                 cr[j].isms3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' ISMs3ai pcon'
                 elim=elim+1
              endif 
              if cr[j].winds2ai le nsig0 and ($
                 (((cr[j+1].winds2a le nsig0 and qconsistent(pstr[j].winds2ai,pstr[j+1].winds2a,pstr[j].winds2aierr,pstr[j+1].winds2aerr) eq 0) or cr[j+1].winds2a gt nsig0) and ((cr[j+1].winds2b le nsig0 and qconsistent(pstr[j].winds2ai,pstr[j+1].winds2b,pstr[j].winds2aierr,pstr[j+1].winds2berr) eq 0) or cr[j+1].winds2b gt nsig0) and ((cr[j+1].jets2ai le nsig0 and qconsistent(pstr[j].winds2ai,pstr[j+1].jets2ai,pstr[j].winds2aierr,pstr[j+1].jets2aierr) eq 0) or cr[j+1].jets2ai gt nsig0) and ((cr[j+1].jetswind2ai le nsig0 and qconsistent(pstr[j].winds2ai,pstr[j+1].jetswind2ai,pstr[j].winds2aierr,pstr[j+1].jetswind2aierr) eq 0) or cr[j+1].jetswind2ai gt nsig0))) then begin 
                 cr[j].winds2ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs2ai pcon'
                 elim=elim+1
              endif 
              if cr[j].winds3ai le nsig0 and ($
                 (((cr[j+1].winds3a le nsig0 and qconsistent(pstr[j].winds3ai,pstr[j+1].winds3a,pstr[j].winds3aierr,pstr[j+1].winds3aerr) eq 0) or cr[j+1].winds3a gt nsig0) and ((cr[j+1].winds3b le nsig0 and qconsistent(pstr[j].winds3ai,pstr[j+1].winds3b,pstr[j].winds3aierr,pstr[j+1].winds3berr) eq 0) or cr[j+1].winds3b gt nsig0) and ((cr[j+1].jets3ai le nsig0 and qconsistent(pstr[j].winds3ai,pstr[j+1].jets3ai,pstr[j].winds3aierr,pstr[j+1].jets3aierr) eq 0) or cr[j+1].jets3ai gt nsig0) and ((cr[j+1].jetswind3ai le nsig0 and qconsistent(pstr[j].winds3ai,pstr[j+1].jetswind3ai,pstr[j].winds3aierr,pstr[j+1].jetswind3aierr) eq 0) or cr[j+1].jetswind3ai gt nsig0))) then begin 
                 cr[j].winds3ai=100.
                 print,'eliminate: '+ntostr(j+1)+' WINDs3ai pcon'
                 elim=elim+1
              endif
              skippcon:
           endif         
        endif                   ;else begin 
;           stop
;           cwind=where(cr.csm eq 2,ncwind)
;           if ncwind gt 0 then begin
;              cr.csm=2
;              elim=elim+1
;           endif 
        
;           cism=where(cr.csm eq 1,ncism)
;           if ncism gt 0 then begin
;              cr.csm=1
;              elim=elim+1
;           endif 
        
;        endelse 
        
     endif 
  endfor    
  
;  print,csm
;  print,cool
;  print,nus
  
  return
end 
  
pro cr_census,cr,nsig=nsig
  
  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  if n_params() eq 0 then cr=mrdfits(!mdata+'closure_relations_total'+signame+'.fits',1)
  u=uniq(cr.grb)
  for i=0,n_elements(u)-1 do begin
     w=where(cr.grb eq cr[u[i]].grb,nw)
     if nw eq 4 then print,cr[u[i]].grb
  endfor 
  
  return
end

pro cr_stats,cr,psc=psc,nsig=nsig
  
  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  if n_elements(cr) eq 0 then cr=mrdfits(!mdata+'closure_relations_total'+signame+'.fits',1)
  if keyword_set(psc) then begin 
     sym=!tsym
     begplot,name=!mdata+'closure_relations_summary.ps',/color,/land
  endif  else sym=!vsym
  nu=sym.nu
  nu2=nu+'!L2!N'
  nu3=nu+'!L3!N'
  ntags=n_tags(cr)-32-6  ;;;if add tags subtract 1
  tn=tag_names(cr)
  numfit=intarr(ntags)
  
  crname=['HighLat','HighLat2',$
          'ISMs2a','ISMs2b','ISMs2ai',$
          'ISMs3a','ISMs3b','ISMs3ai',$
          'WINDs2a','WINDs2b','WINDs2ai',$
          'WINDs3a','WINDs3b','WINDs3ai',$
          'ISMf2a','ISMf2b','ISMf2ai',$
          'ISMf3a','ISMf3b','ISMf3ai',$
          'WINDf2a','WINDf2b','WINDf2ai',$
          'WINDf3a','WINDf3b','WINDf3ai',$
          'JETs2a','JETs2b','JETs2ai',$
          'JETs3a','JETs3b','JETs3ai',$
          'JETsISM2a','JETsISM2b','JETsISM2ai',$ ;;uniform jet (non-spreading)
          'JETsISM3a','JETsISM3b','JETsISM3ai',$
          'JETsWIND2a','JETsWIND2b','JETsWIND2ai',$
          'JETsWIND3a','JETsWIND3b','JETsWIND3ai',$
          'JETsoism2a','JETsoism2b',$
          'JETsoism3a','JETsoism3b',$
          'JETsowind2a','JETsowind2b',$
          'JETsowind3a','JETsowind3b'] ;;structured outflow
  
  c=[1,2,$   ;;HighLAT 
     3,4,5,$ ;;ISMs2
     6,7,8,$ ;;ISMs3
     3,4,5,$ ;;WINDs2
     6,7,8,$ ;;WINDs3
     3,4,5,$ ;;ISMf2
     6,7,8,$ ;;ISMf3
     3,4,5,$ ;;WINDf2
     6,7,8,$ ;;WINDf3
     3,4,5,$ ;;JET2
     6,7,8,$ ;;JET3
     3,4,5,$ ;;JETsISM2
     6,7,8,$ ;;JETsISM3
     3,4,5,$ ;;JETsWIND2
     6,7,8,$ ;;JETsWIND3
     3,4,$   ;;DEL ALP SO ISM
     6,7,$
     3,4,$
     6,7] ;;SO WIND
  color=[!p.color,!magenta,!yellow,!green,!cyan,!blue,!purple,!red,!orange,!grey20,!pink]
;  color=color[c]
;  !p.multi=[0,1,5]
  erase
  multiplot,[1,5],/init
  leg=['Single PL','I','II','III','IV']
;  lines=[-2,3,12,21,30,39,47.5,56]+1   ;;;ALL MESSED UP NOW
  lines=[0,3.5,9.5,15.5,21.5,27.5,33.5,45.5,50.5]
  modclass=['','High Lat','ISM slow','Wind slow','ISM fast','Wind fast','Jet spread','Jet no spread','Structured Jet']
  
  nsig0=1.
  for s=0,4 do begin 
     if s eq 4 then xtitle='Closure Relation' else xtitle=''
     com=execute('ws=where(cr.seg'+ntostr(s)+' eq 1,nws)')
     if nws gt 0 then begin 
        for i=6,ntags-1 do begin 
           
           com=execute('w=where(cr[ws].(i) le nsig0,nw)')
           numfit[i-6]=nw
;           print,tn[i],nw
        endfor 
     endif 
     multiplot
     if s eq 4 then yrange=[0,20] else yrange=minmax(numfit)
     plot,indgen(ntags)+2,numfit,psym=10,charsize=1,/nodata,xtitle=xtitle,ytitle='N',xrange=[0,ntags-3],yrange=yrange
     legend,leg[s],box=0,/top,/right,font=0,charsize=1,margin=0
     if nws gt 0 then begin 
        for clr=1,8 do begin 
           tmpnumfit=intarr(ntags)
           w=where(c eq clr)
           tmpnumfit[w]=numfit[w]
           oplot,indgen(ntags)+2,tmpnumfit,color=color[clr],psym=10
           if tmpnumfit[0] gt 0 then begin 
              oplot,[0.5,0.5]+1,[0,tmpnumfit[0]],color=color[clr]
              oplot,[0.5,1]+1,[tmpnumfit[0],tmpnumfit[0]],color=color[clr]
           endif 
        endfor 
     endif 
     oplot,indgen(ntags)+2,intarr(ntags),psym=10,color=!p.color
     yheight=round(max(numfit)/10.+0.1)*10.*0.85
;     print,yheight,max(numfit)
     if yheight gt max(numfit) then yheight=max(numfit)-2
     if yheight eq 0 then yheight=0.8
     
     for l=1,n_elements(lines)-1 do begin 
        if l lt n_elements(lines)-1 then oplot,[lines[l],lines[l]],[0,200],line=2
        if  ((s eq 1 or s eq 2) and (l ge 6 and l le 8)) or ((s eq 2 or s eq 3 or s eq 4) and l eq 1) then blah=1 else xyouts,lines[l-1]+1,yheight,modclass[l],charsize=1
     endfor 
     
     if s eq 0 then begin
        legend,['',nu2+' p>2',nu2+' 1<p<2',nu2+' p>2 + EI',nu3+' p>2',nu3+' 1<p<2',nu3+' p>2 + EI'],textcolor=[color[0],color[3],color[4],color[5],color[6],color[7],color[8]],box=0,/top,/right,charsize=1.
     endif 
  endfor 
  multiplot,/reset
;  !p.multi=0
  if keyword_set(psc) then endplot
  
  return
end 

pro cat_crstructs,cr,fname=fname,outname=outname,nsig=nsig
  
  cd,!mdata
  if n_elements(nsig) eq 0 then nsig=3.
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  if n_elements(fname) eq 0 then fname='GRB*/closure_relations'+signame+'.fits'
  crfiles=file_search(fname)
  nw=n_elements(crfiles)
  cr0=mrdfits(crfiles[0],1)
  for i=1,nw-1 do begin 
     cr1=mrdfits(crfiles[i],1)
     concat_structs,cr0,cr1,cr
     colprint,cr1.grb
     cr0=cr
  endfor 
  if n_elements(outname) eq 0 then outname='closure_relations_total'+signame+'.fits'
  
  mwrfits,cr,outname,/create
  
  return
end 

pro make_qstruct,n,qstr
  qstr=create_struct('ISMs2ai',0.,'ISMs2aierr',fltarr(2),$
                      'ISMs3ai',0.,'ISMs3aierr',fltarr(2),$
                      'ISMf2ai',0.,'ISMf2aierr',fltarr(2),$
                      'ISMf3ai',0.,'ISMf3aierr',fltarr(2),$
                      'WINDs2ai',0.,'WINDs2aierr',fltarr(2),$
                      'WINDs3ai',0.,'WINDs3aierr',fltarr(2),$
                      'WINDf2ai',0.,'WINDf2aierr',fltarr(2),$
                      'WINDf3ai',0.,'WINDf3aierr',fltarr(2),$
                      'JETs2ai',0.,'JETs2aierr',fltarr(2),$
                      'JETs3ai',0.,'JETs3aierr',fltarr(2),$
                      'JETsISM2ai',0.,'JETsISM2aierr',fltarr(2),$
                      'JETsISM3ai',0.,'JETsISM3aierr',fltarr(2),$
                      'JETsWIND2ai',0.,'JETsWIND2aierr',fltarr(2),$
                      'JETsWIND3ai',0.,'JETsWIND3aierr',fltarr(2))
  return
end 

pro make_pstruct,n,pstr
  pstr=create_struct('ISMs2a',0.,'ISMs2aerr',fltarr(2),$
                     'ISMs2b',0.,'ISMs2berr',fltarr(2),$
                     'ISMs2ai',0.,'ISMs2aierr',fltarr(2),$
                     'ISMs3a',0.,'ISMs3aerr',fltarr(2),$
                     'ISMs3b',0.,'ISMs3berr',fltarr(2),$
                     'ISMs3ai',0.,'ISMs3aierr',fltarr(2),$
;                      'ISMf2ai',0.,'ISMf2aierr',fltarr(2),$
                     'ISMf3a',0.,'ISMf3aerr',fltarr(2),$
                     'ISMf3b',0.,'ISMf3berr',fltarr(2),$
                     'ISMf3ai',0.,'ISMf3aierr',fltarr(2),$
                     'WINDs2a',0.,'WINDs2aerr',fltarr(2),$
                     'WINDs2b',0.,'WINDs2berr',fltarr(2),$
                     'WINDs2ai',0.,'WINDs2aierr',fltarr(2),$
                     'WINDs3a',0.,'WINDs3aerr',fltarr(2),$
                     'WINDs3b',0.,'WINDs3berr',fltarr(2),$
                     'WINDs3ai',0.,'WINDs3aierr',fltarr(2),$
;                      'WINDf2ai',0.,'WINDf2aierr',fltarr(2),$
                     'WINDf3a',0.,'WINDf3aerr',fltarr(2),$
                     'WINDf3b',0.,'WINDf3berr',fltarr(2),$
                     'WINDf3ai',0.,'WINDf3aierr',fltarr(2),$
                     'JETs2a',0.,'JETs2aerr',fltarr(2),$
                     'JETs2b',0.,'JETs2berr',fltarr(2),$
                     'JETs2ai',0.,'JETs2aierr',fltarr(2),$
                     'JETs3a',0.,'JETs3aerr',fltarr(2),$
                     'JETs3b',0.,'JETs3berr',fltarr(2),$
                     'JETs3ai',0.,'JETs3aierr',fltarr(2),$
                     'JETsISM2a',0.,'JETsISM2aerr',fltarr(2),$
                     'JETsISM2b',0.,'JETsISM2berr',fltarr(2),$
                     'JETsISM2ai',0.,'JETsISM2aierr',fltarr(2),$
                     'JETsISM3a',0.,'JETsISM3aerr',fltarr(2),$
                     'JETsISM3b',0.,'JETsISM3berr',fltarr(2),$
                     'JETsISM3ai',0.,'JETsISM3aierr',fltarr(2),$
                     'JETsWIND2a',0.,'JETsWIND2aerr',fltarr(2),$
                     'JETsWIND2b',0.,'JETsWIND2berr',fltarr(2),$                     
                     'JETsWIND2ai',0.,'JETsWIND2aierr',fltarr(2),$
                     'JETsWIND3a',0.,'JETsWIND3aerr',fltarr(2),$
                     'JETsWIND3b',0.,'JETsWIND3berr',fltarr(2),$                     
                     'JETsWIND3ai',0.,'JETsWIND3aierr',fltarr(2),$
                     'JETsoISM2a',0.,'JETsoISM2aerr',fltarr(2),$
                     'JETsoISM3a',0.,'JETsoISM3aerr',fltarr(2),$
                     'JETsoWIND2a',0.,'JETsoWIND2aerr',fltarr(2),$
                     'JETsoWIND3a',0.,'JETsoWIND3aerr',fltarr(2))
  
  return
end 

pro make_crstruct,n,crstr
  crstr=create_struct('GRB','',$
                      'Seg0',0,'Seg1',0,'Seg2',0,'Seg3',0,'Seg4',0,$
                      'HighLat',100.,'HighLat2',100.,$
                      'ISMs2a',100.,'ISMs2b',100.,'ISMs2ai',100.,$
                      'ISMs3a',100.,'ISMs3b',100.,'ISMs3ai',100.,$
                      'WINDs2a',100.,'WINDs2b',100.,'WINDs2ai',100.,$
                      'WINDs3a',100.,'WINDs3b',100.,'WINDs3ai',100.,$
                      'ISMf2a',100.,'ISMf2b',100.,'ISMf2ai',100.,$
                      'ISMf3a',100.,'ISMf3b',100.,'ISMf3ai',100.,$
                      'WINDf2a',100.,'WINDf2b',100.,'WINDf2ai',100.,$
                      'WINDf3a',100.,'WINDf3b',100.,'WINDf3ai',100.,$
                      'JETs2a',100.,'JETs2b',100.,'JETs2ai',100.,$
                      'JETs3a',100.,'JETs3b',100.,'JETs3ai',100.,$
                      'JETsISM2a',100.,'JETsISM2b',100.,'JETsISM2ai',100.,$  ;;uniform spreading jet
                      'JETsISM3a',100.,'JETsISM3b',100.,'JETsISM3ai',100.,$
                      'JETsWIND2a',100.,'JETsWIND2b',100.,'JETsWIND2ai',100.,$
                      'JETsWIND3a',100.,'JETsWIND3b',100.,'JETsWIND3ai',100.,$
                      'JETsoism2a',100.,'JETsoism2b',100.,$  ;;structured outflow
                      'JETsoism3a',100.,'JETsoism3b',100.,$
                      'JETsowind2a',100.,'JETsowind2b',100.,$ 
                      'JETsowind3a',100.,'JETsowind3b',100.,$ ;;structured outflow
                      'CSM',0,$
                      'COOL',0,$
                      'NU',0,$
                      'JETBREAK',0,$
                      'ALPHA',0.,'ALPHAERR',fltarr(2),$
                      'BETA',0.,'BETAERR',fltarr(2),$
                      'TBREAK',0.,'TBREAKERR',fltarr(2),$
                      'TSTART',0.,'TSTOP',0.,'TLASTDET',0.,'TLASTPOS',0.,$
                      'CTR_TSTOP',0.,'CTR_TBREAK',0.,'CTR_TLASTDET',0.,'CTR_TLASTPOS',0.,'CTR_T1DAY',0.,$
                      'Z',0.,'THETA',0.,'THETA_TSTART',0.,'THETA_STOP',0.,'THETA_LASTDET',0.,'THETA_LASTPOS',0.,$
                      'EISO',0.,'EISOERR',fltarr(2),'WHO_EISO',' ','COMPLETE',0,'CLASS',0,$
                      'FLUX',0d,'RATE',0d)
                      
  crstr=replicate(crstr,n)
  return
end 
pro which_alpha,alphas,alphaserr,j,posa,delalp,delalperr,twocomp=twocomp,latebr=latebr
  
  breaks=n_elements(alphas)-1
  case breaks of
     0: begin 
        posa=[[1,2,3,4]]
        delalp=0.
        delalperr=[0.,0.]
     end
     1: begin
        if not keyword_set(twocomp) then begin 
           if alphas[0] gt alphas[1] then posa=[[1],[2]] else $
;           posa=[[2],[3]]           
                 posa=[[2,3],[3,4]]
           delalp=alphas[1]-alphas[0]
           delalperr=[sqrt(total(alphaserr[0,0:1]^2)),sqrt(total(alphaserr[1,0:1]^2))]
        endif else begin
           posa=[[4],[3]]
           delalp=0.
           delalperr=[0.,0.]
        endelse 
     end
     2: begin
        if not keyword_set(twocomp) then begin 
           if alphas[0] gt alphas[1] then posa=[[1],[2],[3]] else $
              posa=[[2],[3],[4]]
           delalp=[alphas[1]-alphas[0],alphas[2]-alphas[1]]
           delalperr=[[sqrt(total(alphaserr[0,0:1]^2)),sqrt(total(alphaserr[1,0:1]^2))],[sqrt(total(alphaserr[0,1:2]^2)),sqrt(total(alphaserr[1,1:2]^2))]]
        endif else begin 
;           if keyword_set(latebr) then begin 
              posa=[[3,4],[0,3],[0,4]]
              delalp=[0.,alphas[2]-alphas[1]]
              delalperr=[[0.,0.],[sqrt(total(alphaserr[0,1:2]^2)),sqrt(total(alphaserr[1,1:2]^2))]]
;           endif else begin 
;              posa=[[0,3],[0,4],[3,4]]
;              delalp=[alphas[1]-alphas[0],0.]
;              delalperr=[[sqrt(total(alphaserr[0,0:1]^2)),sqrt(total(alphaserr[1,0:1]^2))],[0.,0.]]
;           endelse 
        endelse 
     end
     3: begin 
        if not keyword_set(twocomp) then begin 
           posa=[[1],[2],[3],[4]]
           delalp=[alphas[1]-alphas[0],alphas[2]-alphas[1],alphas[3]-alphas[2]]
           delalperr=[[sqrt(total(alphaserr[0,0:1]^2)),sqrt(total(alphaserr[1,0:1]^2))],[sqrt(total(alphaserr[0,1:2]^2)),sqrt(total(alphaserr[1,1:2]^2))],[sqrt(total(alphaserr[0,2:3]^2)),sqrt(total(alphaserr[1,2:3]^2))]]
        endif else begin 
           posa=[[3],[4],[3],[4]]
           delalp=[alphas[1]-alphas[0],0.,alphas[3]-alphas[2]]
           delalperr=[[sqrt(total(alphaserr[0,0:1]^2)),sqrt(total(alphaserr[1,0:1]^2))],[0.,0.],[sqrt(total(alphaserr[0,2:3]^2)),sqrt(total(alphaserr[1,2:3]^2))]]   
        endelse            
     end
endcase 
  
  return
end

pro which_closure_relation,a,alow,aup,g,glow,gup,cr,posa,delalp,delalperr,gg,psc=psc,injection=injection,answer=answer,noplot=noplot,chisq=chisq,qstr=qstr,pstr=pstr,nsig=nsig,xtitle=xtitle,charsize=charsize,segment=segment,nocolor=nocolor,plotevery=plotevery,incaption=incaption,title=title,twocomp=twocomp;,crval=crval,errvallow=errvallow,errvalup=errvalup
  
  if n_elements(nsig) eq 0 then nsig=3.
  b=g-1.
  blow=glow
  bup=gup
;  blow=glow-1
;  bup=gup-1
  
  cr=['HighLat','HighLat2',$
      'ISMs2a','ISMs2b','ISMs2ai',$
      'ISMs3a','ISMs3b','ISMs3ai',$
      'WINDs2a','WINDs2b','WINDs2ai',$
      'WINDs3a','WINDs3b','WINDs3ai',$
      'ISMf2a','ISMf2b','ISMf2ai',$
      'ISMf3a','ISMf3b','ISMf3ai',$
      'WINDf2a','WINDf2b','WINDf2ai',$
      'WINDf3a','WINDf3b','WINDf3ai',$
      'JETs2a','JETs2b','JETs2ai',$  ;;uniform jet with lateral expansion ISM or Wind
      'JETs3a','JETs3b','JETs3ai',$
      'JETsISM2a','JETsISM2b','JETsISM2ai',$ ;;uniform jet (non-spreading)
      'JETsISM3a','JETsISM3b','JETsISM3ai',$
      'JETsWIND2a','JETsWIND2b','JETsWIND2ai',$
      'JETsWIND3a','JETsWIND3b','JETsWIND3ai',$
      'JETsoISM2a','JETsoISM2b',$  ;;structured outflow
      'JETsoISM3a','JETsoISM3b',$
      'JETsoWIND2a','JETsoWIND2b',$ 
      'JETsoWIND3a','JETsoWIND3b'] ;;structured outflow  
  
  n=n_elements(cr)
  chisq=dblarr(n)
  chisq[*]=100.
  if n_elements(answer) gt 0 then know=1 else begin 
     know=0
     answer=intarr(n)
     answer[*]=1
  endelse 
  
  b2=2.*b
  b2p1=2.*b+1
  
  p=[0,0,$            ;;HighLAT 
     b2p1,b2p1,b2p1,$ ;;ISMs2
     b2,b2,b2,$       ;;ISMs3
     b2p1,b2p1,b2p1,$ ;;WINDs2
     b2,b2,b2,$       ;;WINDs3
     0,0,0,$          ;;ISMf2
     b2,b2,b2,$       ;;ISMf3
     0,0,0,$          ;;WINDf2
     b2,b2,b2,$       ;;WINDf3
     b2p1,b2p1,b2p1,$ ;;JET2
     b2,b2,b2,$       ;;JET3
     b2p1,b2p1,b2p1,$         ;;JETsISM2
     b2,b2,b2,$             ;;JETsISM3
     b2p1,b2p1,b2p1,$         ;;JETsWIND2
     b2,b2,b2,$             ;;JETsWIND3
     b2p1,b2p1,$         ;;DEL ALPHA S0 ISM2
     b2,b2,$              ;;DEL ALPHA S0 ISM3
     b2p1,b2p1,$          ;;DEL ALPHA S0 WIND2
     b2,b2]               ;;DEL ALPHA S0 WIND3
  
;  berr=mean([blow,bup]);2.*(abs(blow)+abs(bup))/2.
  perr=dblarr(2,n)
  perr[0,*]=2.*blow
  perr[1,*]=2.*bup
;  perr[*]=2.*berr
  w=where(p eq 0)
 ; perr[*,w]=100.
  pp='(p='+numdec(p,2)+'!S!L-'+numdec(perr[0,*],2)+'!R!U+'+numdec(perr[1,*],2)+'!N'
  ppt='($p='+numdec(p,2)+'_{-'+numdec(perr[0,*],2)+'}^{+'+numdec(perr[1,*],2)+'}'
  pp[w]=''
  ppt[w]=''
  
  c=[1,2,$   ;;HighLAT 
     3,4,5,$    ;;ISMs2
     6,7,8,$   ;;ISMs3
     3,4,5,$    ;;WINDs2
     6,7,8,$   ;;WINDs3
     3,4,5,$   ;;ISMf2
     6,7,8,$  ;;ISMf3
     3,4,5,$ ;;WINDf2
     6,7,8,$ ;;WINDf3
     3,4,5,$    ;;JET2
     6,7,8,$   ;;JET3
     3,4,5,$ ;;JETsISM2
     6,7,8,$ ;;JETsISM3
     3,4,5,$ ;;JETsWIND2
     6,7,8,$ ;;JETsWIND3
     3,4,$ ;;DEL ALP SO ISM
     6,7,$
     3,4,$
     6,7] ;;SO WIND
  
  if not keyword_set(nocolor) then $
     color=[!p.color,!magenta,!yellow,!green,!cyan,!blue,!purple,!red,!orange,!grey20,!pink] else color=replicate(!p.color,11)
  color=color[c]
  
  bb=[0,0,$   ;;HighLAT 
      1,2,1,$ ;;ISMs2
      1,2,1,$ ;;ISMs3
      1,2,1,$ ;;WINDs2
      1,2,1,$ ;;WINDs3
      1,2,1,$ ;;ISMf2
      1,2,1,$ ;;ISMf3
      1,2,1,$ ;;WINDf2
      1,2,1,$ ;;WINDf3
      1,2,1,$ ;;JET2
      1,2,1,$ ;;JET3
      1,2,1,$     ;;JETsISM2
      1,2,1,$     ;;JETsISM3
      1,2,1,$     ;;JETsWIND2
      1,2,1,$     ;;JETsWIND3
      1,2,$     ;;DEL ALPHA S0 ISM
      1,2,$
      1,2,$
      1,2]   ;;DEL ALPHA S0 WIND
  
  bb[w]=0
;;exclude nu<nu_a (for optical not xray)
  
  
  g=[1,0,$   ;;HighLAT 
     1,1,1,$ ;;ISMs2
     1,1,1,$ ;;ISMs3
     1,1,1,$ ;;WINDs2
     1,1,1,$ ;;WINDs3
     1,1,1,$ ;;ISMf2
     1,1,1,$ ;;ISMf3
     1,1,1,$ ;;WINDf2
     1,1,1,$ ;;WINDf3
     1,1,1,$ ;;JET2                ;;TURNED ON JET+EI
     1,1,1,$ ;;JET3                ;;TURNED ON JET+EI
     1,1,1,$     ;;JETsISM2
     1,1,1,$     ;;JETsISM3
     1,1,1,$     ;;JETsWIND2
     1,1,1,$     ;;JETsWIND3
     1,0,$     ;;DEL ALPHA S0 ISM       ;;NOT TRUSTING DERIV OF SO JET p<2
     1,0,$
     1,0,$
     1,0]   ;;DEL ALPHA S0 WIND
  
  if keyword_set(twocomp) then begin 
     g=[1,0,$   ;;HighLAT 
        1,1,1,$ ;;ISMs2
        1,1,1,$ ;;ISMs3
        1,1,1,$ ;;WINDs2
        1,1,1,$ ;;WINDs3
        1,1,1,$ ;;ISMf2
        1,1,1,$ ;;ISMf3
        1,1,1,$ ;;WINDf2
        1,1,1,$ ;;WINDf3
        1,1,0,$ ;;JET2                ;;TURNED ON JET+EI
        1,1,0,$ ;;JET3                ;;TURNED ON JET+EI
        1,1,0,$ ;;JETsISM2
        1,1,0,$ ;;JETsISM3
        1,1,0,$ ;;JETsWIND2
        1,1,0,$ ;;JETsWIND3
        1,0,$   ;;DEL ALPHA S0 ISM       ;;NOT TRUSTING DERIV OF SO JET p<2
        1,0,$
        1,0,$
        1,0]   ;;DEL ALPHA S0 WIND
  endif 
  
  ;; exclusions based on which part of canonical LC
  p1=where(posa eq 1,n1)
  if n1 eq 0 then g[0:1]=0  ;; High Lat only possible if LC 1
  p2=where(posa eq 2,n2)
  if n2 eq 0 then g[[4,7,10,13,16,19,22,25]]=0  ;;N+EI only in 2
  p3=where(posa eq 3,n3)
  if n3 eq 0 then g[[28,31,34,37,40,43]]=0  ;;JET + EI only in 3
  if n3 eq 0 then g[[2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24]]=0  ;;Normal decay only in 3
  p4=where(posa eq 4,n4)
  if n4 eq 0 then g[[26,27,29,30,32,33,35,36,38,39,41,42,44,45,46,47,48,49,50]]=0 ;;; Non-EI jet models only in 4

  ;;; only CR in I is highlat
  if n2 eq 0 and n3 eq 0 and n4 eq 0 then g[2:*]=0
  
;  p34=where(posa eq 3 or posa eq 4,np34) 
;  if n1 eq 0 and n2 eq 0 then g[0:1]=0
  if n3 eq 0 and n4 eq 0 then g[26:*]=0  ;;JET only possible in 3 or 4 or 0 
  ;;limits JB in 3 to JB+EI 
;  if n4 eq 0 then g[[26,27,29,30,32,33,35,36,38,39,41,42,44,45,46,47,48,49,50,51]]=0
;  if n_elements(posa) eq 4 then g[[28,31+indgen(21)]]=0  ;;no delalp possible in 0
;  if n_elements(posa) eq 4 then g[44:*]=0  ;;no delalp possible in 0
;  p34b=where(posa ne 3 and posa eq 4,np34b) ;;where definitely a 4 cannot have N
;  if np34b gt 0 then g[2:25]=0
  if n3 eq 0 and n4 gt 0 then g[2:25]=0 ;;norm not possible is 3 or 4 are not
  if n2 gt 0 and n1 eq 0 and n3 eq 0 and n4 eq 0 then $
     g[[0,1]]=0 ;;must keep other options for 2 incase 1 is incorrectly identified (flare confusion)
;     g[[0,1,2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24]]=0 ;;if 2 only option then 2 can only be N+EI

;  if n_elements(posa) eq 4 or delalp[0] eq 0. then g[n_elements(g)-14:*]=0
  ;;;change this 14 to something else if add more models
  
  good=where(g eq 1)
  
  if not know then ng=n_elements(good) else nwg=where(answer eq 1,ng)
;  n2=10
  ;;distinguish between energy injection and not
;  inj=indgen(10)*2+1
;  noinj=indgen
;  if keyword_set(injection) then good=good[inj] else $
;     good=good[]

  cr=cr[good]
  color=color[good]
  p=p[good]  
  pp=pp[good]
  ppt=ppt[good]
  bb=bb[good]
  if know then answer=answer[good]
  
  n=n_elements(cr)
  crval=dblarr(n)
  creq=strarr(n)
  errvalup=dblarr(n)
  errvallow=errvalup
  dcrdb=dblarr(n)
  if keyword_set(psc) then sym=!tsym else sym=!vsym
  alpha=sym.alpha
  beta=sym.beta
  dap=sym.delta_cap+sym.alpha
  
;     !p.multi=[0,5,5]
;     !p.multi=[0,2,n/2]
  
  q=dblarr(n)
  qerr=dblarr(2,n)
  make_qstruct,1,qstr
  make_pstruct,1,pstr

  k=dblarr(n)
  gg=-1
;  chisq=dblarr(n)
  
  if n_elements(xtitle) eq 0 then xtitle='closure relation'
  xrange=[-4,2]
  if keyword_set(incaption) then xrange=[-2,2]
  if not keyword_set(noplot) then plot,xrange,[0,ng+1],/nodata,xtitle=xtitle,/xsty,charsize=charsize,yrange=[0,ng+1],/ysty,ytickname=replicate(' ',10),yticks=1,yminor=0,title=title
  
  ii=0
  caption=''
  letters=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
  for i=0,n-1 do begin 
     qt=100.
     kkt=''
     ktgood=1
     case strtrim(cr[i],2) of 
        'HighLat': begin 
           crval[i]=a-2.-b
           creq[i]=alpha+'=2+'+beta
           dcrdb[i]=-1.
        end 
;        'HighLat2': begin ;;where did this come from????  Zhang et al (2006)
;           crval[i]=a-1.-3.*b/2.
;           creq[i]=alpha+'=1+3'+beta+'/2'
;           dcrdb[i]=-3./2.
;        end 
;        'ISMs1a': begin
;           crval[i]=a-3.*b/2.
;           creq[i]=alpha+'=3'+beta+'/2'
;           dcrdb[i]=-3./2.
;        end 
;        'ISMs1ai': begin
;           q[i]=(a+1-b)/(1+b/2.)
;           qerr[*,i]=[sqrt((1./(1.+b/2.))^2.*alow^2+(-4./(2.+b))^2.*blow^2),sqrt((1./(1.+b/2.))^2.*aup^2+(-4./(2.+b))^2.*bup^2)]
;           crval[i]=a-(q[i]-1.)-(2+q[i])*b/2.
;           creq[i]=alpha+'=(q-1)+(2+q)'+beta+'/2'
;           dcrdb[i]=-(2.+q[i])/2.
;        end 
        'ISMs2a': begin
           crval[i]=a-3.*b/2.
           creq[i]=alpha+'=3'+beta+'/2'
           dcrdb[i]=-3./2.
        end 
        'ISMs2b': begin
           crval[i]=a-3.*(2*b+3)/16.
           creq[i]=alpha+'=3(2'+beta+'+3)/16'
           dcrdb[i]=3./8.
        end 
        'ISMs2ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/(1.+b/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=1./(1.+b/2.)
           dqdb=(-2.*a-6.)/(2.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-1.)-(2+qq)*b/2.
           creq[i]=alpha+'=(q-1)+(2+q)'+beta+'/2'
           dcrdb[i]=-(2.+qq)/2.
        end 
        'ISMs3a': begin
           crval[i]=a-(3.*b-1.)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'
           dcrdb[i]=-3./2.
        end 
        'ISMs3b': begin
           crval[i]=a-(3.*b+5)/8.
           creq[i]=alpha+'=(3'+beta+'+5)/8'
           dcrdb[i]=3./8.
        end 
        'ISMs3ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a-4.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-2.)/2.-(2+qq)*b/2.
           creq[i]=alpha+'=(q-2)/2+(2+q)'+beta+'/2'
           dcrdb[i]=-(2.+qq)/2.
        end 
;        'ISMf1a': begin
;           crval[i]=a-b/2.
;           creq[i]=alpha+'='+beta+'/2'
;           dcrdb[i]=-1./2.
;        end 
;        'ISMf1b': begin
;           crval[i]=a-b/2.
;           creq[i]=alpha+'='+beta+'/2'
;           dcrdb[i]=-1./2.
;        end 
;        'ISMf1ai': begin
;           q[i]=(a+1.-b)/(1-b/2.)
;           crval[i]=a-(q[i]-1.)-(2-q[i])*b/2.
;           creq[i]=alpha+'=(q-1)+(2-q)'+beta+'/2'
;           dcrdb[i]=-(2.-q[i])/2.
;        end 
        'ISMf2a': begin
           crval[i]=a-b/2.
           creq[i]=alpha+'='+beta+'/2'
           dcrdb[i]=-1./2
        end 
        'ISMf2b': begin
           crval[i]=a-b/2.
           creq[i]=alpha+'='+beta+'/2'
           dcrdb[i]=-1./2
        end 
        'ISMf2ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/(1.-b/2.)           
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(2.-b)
           dqdb=(2.*a-2.)/(2.-b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-1.)-(2-qq)*b/2.
           creq[i]=alpha+'=(q-1)+(2-q)'+beta+'/2'
           dcrdb[i]=-(2.-qq)/2.
        end         
        'ISMf3a': begin
           crval[i]=a-(3.*b-1)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'              
           dcrdb[i]=-3./2.
        end 
        'ISMf3b': begin
           crval[i]=a-(3.*b+5)/8.
           creq[i]=alpha+'=(3'+beta+'+5)/8'
           dcrdb[i]=3./8.
        end 
        'ISMf3ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a-4.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-2.)/2.-(2+qq)*b/2.
           creq[i]=alpha+'=(q-2)/2+(2+q)'+beta+'/2'
           dcrdb[i]=-(2.+qq)/2.
        end 
;        'WINDs1a': begin
;           crval[i]=a-(3.*b+1.)/2.
;           creq[i]=alpha+'=(3'+beta+'+1)/2'
;           dcrdb[i]=-3./2.
;        end
;        'WINDs1b': begin
;           crval[i]=a-(3.*b+1.)/2.
;           creq[i]=alpha+'=(3'+beta+'+1)/2'
;           dcrdb[i]=-3./2.
;        end
;        'WINDs1ai': begin
;           q[i]=(a-b)/((1+b)/2.)
;           crval[i]=a-q[i]/2.-(2+q[i])*b/2.
;           creq[i]=alpha+'=q/2+(2+q)'+beta+'/2'
;           dcrdb[i]=-(2.+q[i])/2.
;        end 
        'WINDs2a': begin
           crval[i]=a-(3.*b+1.)/2.
           creq[i]=alpha+'=(3'+beta+'+1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDs2b': begin
           crval[i]=a-(2.*b+9.)/8.
           creq[i]=alpha+'=(2'+beta+'+9)/8'
           dcrdb[i]=-1./4.
        end        
        'WINDs2ai': begin
;           ktgood=0
           q[i]=(a-b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a-2.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-q[i]/2.-(2+qq)*b/2.
           creq[i]=alpha+'=q/2+(2+q)'+beta+'/2'
           dcrdb[i]=-(2.+qq)/2.
        end
        'WINDs3a': begin
           crval[i]=a-(3.*b-1.)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDs3b': begin
           crval[i]=a-(b+3.)/4.
           creq[i]=alpha+'=('+beta+'+3)/4'
           dcrdb[i]=1./4.
        end
        'WINDs3ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a-4.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-2.)/2.-(2+qq)*b/2.
           creq[i]=alpha+'=(q-2)/2+(2+q)'+beta+'/2'
           dcrdb[i]=-(2.+qq)/2.
        end
;        'WINDf1a': begin
;           crval[i]=a-(1.-b)/2.
;           creq[i]=alpha+'=(1-'+beta+')/2'
;           dcrdb[i]=1./2.
;        end
;        'WINDf1b': begin
;           crval[i]=a-(1.-b)/2.
;           creq[i]=alpha+'=(1-'+beta+')/2'
;           dcrdb[i]=1./2.
;        end        
;        'WINDf1ai': begin
;           q[i]=(a+b)/((1+b)/2.)
;           crval[i]=a-q[i]/2.+(2-q[i])*b/2.
;           creq[i]=alpha+'=q/2-(2-q)'+beta+'/2'
;           dcrdb[i]=(2.-q[i])/2.
;        end 
        'WINDf2a': begin
           crval[i]=a-(1.-b)/2.
           creq[i]=alpha+'=(1-'+beta+')/2'
           dcrdb[i]=1./2.
        end
        'WINDf2b': begin
           crval[i]=a-(1.-b)/2.
           creq[i]=alpha+'=(1-'+beta+')/2'
           dcrdb[i]=1./2.
        end        
        'WINDf2ai': begin
;           ktgood=0
           q[i]=(a+b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a+2.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-q[i]/2.+(2-qq)*b/2.
           creq[i]=alpha+'=q/2-(2-q)'+beta+'/2'
           dcrdb[i]=(2.-qq)/2.
        end 
        'WINDf3a': begin
           crval[i]=a-(3.*b-1.)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDf3b': begin
           crval[i]=a-(b+3.)/4.
           creq[i]=alpha+'=('+beta+'+3)/4'
           dcrdb[i]=1./4.
        end
        'WINDf3ai': begin
;           ktgood=0
           q[i]=(a+1.-b)/((1.+b)/2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(1.+b)
           dqdb=(-2.*a-4.)/(1.+b)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(q[i]-2.)/2.-(2+qq)*b/2.
           creq[i]=alpha+'=(q-2)/2+(2+q)'+beta+'/2'
           dcrdb[i]=(2.+qq)/2.           
        end
        'JETs2a': begin
           crval[i]=a-(2.*b+1.)
           creq[i]=alpha+'=2'+beta+'+1'
           dcrdb[i]=-2.
        end
        'JETs2b': begin
           crval[i]=a-(2.*b+7.)/4.
           creq[i]=alpha+'=(2'+beta+'+7)/4'
           dcrdb[i]=-2./4.
        end
        'JETs2ai': begin
;           ktgood=0
           q[i]=(3.*a-4.*b+1)/(2.*b+4)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=3./(2.*b+4.)
           dqdb=(-6.*a-18.)/(2.*b+4.)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(2.*b+1-2./3.*(1-qq)*(b+2.))
           creq[i]=alpha+'=2'+beta+'+1-2/3(1-q)('+beta+'+2)'
           dcrdb[i]=(4.+2.*qq)/3.
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-3.*delalp/(2.*b+4.)
;           qerr[*,i]=[sqrt((3./(2.*b+4.))^2.*delalperr[0]^2+(-6.*delalp/(4.+2.*b)^2)^2.*blow^2),sqrt((3./(4.+2.*b))^2.*delalperr[1]^2+(-6./(4.+2.*b)^2)^2.*bup^2)]
;           crval[i]=delalp-2./3.*(1.-q[i])*(b+2.)
;           creq[i]=dap+'=2/3(1-q)('+beta+'+2)'
;           dcrdb[i]=2./3.*(1.-q[i])
        end 
        'JETs3a': begin
           crval[i]=a-2.*b
           creq[i]=alpha+'=2'+beta+''
           dcrdb[i]=-2.
        end
        'JETs3b': begin
           crval[i]=a-(b+3.)/2.
           creq[i]=alpha+'=('+beta+'+3)/2'
           dcrdb[i]=-1./2.
        end 
        'JETs3ai': begin
;           ktgood=0
           q[i]=(3.*a-4.*b+2.)/(2.*b+2.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=3./(2.*b+2.)
           dqdb=(-6.*a-12.)/(2.*b+2.)^2
           qerr[*,i]=[sqrt(dqda^2.*alow^2.+dqdb^2*blow^2),sqrt(dqda^2.*aup^2.+dqdb^2*bup^2)]
           crval[i]=a-(2*b-2./3.*(1-qq)*(b+1.))
           creq[i]=alpha+'=2'+beta+'-2/3(1-q)('+beta+'+1)'
           dcrdb[i]=-(4.+2.*qq)/3.
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-3.*delalp/(2.*b+2.)
;           qerr[*,i]=[sqrt((3./(2.*b+2.))^2.*delalperr[0]^2+(-6*delalp/(2.+2.*b)^2)^2.*blow^2),sqrt((3./(2.+2.*b))^2.*delalperr[1]^2+(-6./(2.+2.*b)^2)^2.*bup^2)]
;           crval[i]=delalp-2./3.*(1.-q[i])*(b+1.)
;           creq[i]=dap+'=2/3(1-q)('+beta+'+1)'
;           dcrdb[i]=2./3.*(1.-q[i])
        end
        ;;;;DELTA ALPHA CLOSURE RELATIONS
        ;;NOT KOSHER TO USE DELALP TO GET ALP
        'JETsoISM2a':begin  ;;structured jet
;           k[i]=(8.*a-12.*b)/(3.+a)
           k[i]=delalp*8./(a+3.)
           kt=8./(2.*b+5.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
;           crval[i]=delalp-3.*k[i]/2.*(b+2.)/(8.-k[i])
;           creq[i]=dap+'=3k('+beta+'+2)/2(8-k)'
;           dcrdb[i]=3.*k[i]/(2.*(8.-k[i]))
           crval[i]=a-(3.*k[i]+12.*b)/(8-k[i])
           creq[i]=alpha+'=(3k+12'+beta+')/(8-k)'
           dcrdb[i]=12./(8.-k[i])
        end
        'JETsoISM2b': begin ;;DON'T BELIEVE
           k[i]=delalp*8./(a+3.)
           kt=8./(2.*b+5.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
           crval[i]=delalp-3.*k[i]/16.*(2.*b+5.)/(8.-k[i])
           creq[i]=dap+'=3k(2'+beta+'+5)/16(8-k)'
           dcrdb[i]=6.*k[i]/(16.*(8.-k[i]))
        end
        'JETsoISM3a':begin
;           k[i]=(8.*a-12.*b+4.)/(2.+a)
           k[i]=delalp*8./(a+2.)
           kt=8./(2.*b+3.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
;           crval[i]=delalp-3.*k[i]/2.*(b+1.)/(8-k[i])
;           creq[i]=dap+'=3k('+beta+'+1)/2(8-k)'
;           dcrdb[i]=3.*k[i]/(2.*(8-k[i]))           
;           crval[i]=a-(12.*b-4.+2.*k[i])/(8.-k[i])
;           creq[i]=alpha+'=(12'+beta+'-4+2k)/(8-k)'
;           dcrdb[i]=12./(8.-k[i])
           crval[i]=a-(12.*b+2.*k[i]-4.)/(8.-k[i])
           creq[i]=alpha+'=(12'+beta+'+2k-4)/(8-k)'
           dcrdb[i]=12./(8-k[i])
        end 
        'JETsoISM3b':begin ;;DON'T BELIEVE
           k[i]=delalp*8./(a+2.)
           kt=8./(2.*b+3.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
           crval[i]=delalp-3.*k[i]/8.*(b+7.)/(8-k[i])
           creq[i]=dap+'=3k('+beta+'+7)/8(8-k)'
           dcrdb[i]=3.*k[i]/(8.*(8-k[i]))           
        end 
        'JETsoWIND2a': begin
;           k[i]=(4.*a-6.*b-2.)/(a-b)
           k[i]=delalp*12./(2.*delalp+a+1.)
           kt=8./(2.*b+4.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
;           crval[i]=delalp-k[i]/2.*(b+1.)/(4.-k[i])
;           creq[i]=dap+'=k('+beta+'+1)/2(4-k)'
;           dcrdb[i]=k[i]/(2.*(4-k[i]))
;           crval[i]=a-(6.*b+2.-k[i]*b)/(4.-k[i])
;           creq[i]=alpha+'=(6'+beta+'+2-k'+beta+')/(4-k)'
;           dcrdb[i]=(6.-k[i])/(4.-k[i])
           crval[i]=a-(6.*b+k[i]*b+2.)/(4.-k[i])
           creq[i]=alpha+'=(6'+beta+'+k'+beta+'+2)/(4-k)'
           dcrdb[i]=(6.+k[i])/(4.-k[i])
        end 
        'JETsoWIND2b': begin;;DON'T BELIEVE
           k[i]=delalp*12./(2.*delalp+a+1.)
           kt=8./(2.*b+4.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
           crval[i]=delalp-k[i]/24.*(2.*b+17.)/(4.-k[i])
           creq[i]=dap+'=k(2'+beta+'+17)/24(4-k)'
           dcrdb[i]=2.*k[i]/(24.*(4-k[i]))
        end 
        'JETsoWIND3a': begin
;           k[i]=(4.*a-6.*b+2.)/(a-b+1.)
           k[i]=delalp*12./(2.*delalp+a+2.)
           kt=8./(2.*b+4.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
;           crval[i]=delalp-k[i]/2.*(b+1.)/(4.-k[i])
;           creq[i]=dap+'=k('+beta+'+1)/2(4-k)'
;           dcrdb[i]=k[i]/(2.*(4-k[i]))
;           crval[i]=a-(6.*b-2.-k[i]*b+k[i])/(4.-k[i])
;           creq[i]=alpha+'=(6'+beta+'-2-k'+beta+'+k)/(4-k)'
;           dcrdb[i]=(6.-k[i])/(4.-k[i])
           crval[i]=a-(6.*b+k[i]-k[i]*b-2.)/(4.-k[i])
           creq[i]=alpha+'=(6'+beta+'+k-k'+beta+'-2)/(4-k)'
           dcrdb[i]=(6.-k[i])/(4.-k[i])
        end 
        'JETsoWIND3b': begin ;;DON'T BELIEVE
           k[i]=delalp*12./(2.*delalp+a+2.)
           kt=8./(2.*b+4.)
           kkt='<'+sigfig(kt,3)
           if k[i] gt kt or delalp eq 0. then ktgood=0 else ktgood=1
           crval[i]=delalp-k[i]/12.*(b+11.)/(4.-k[i])
           creq[i]=dap+'=k('+beta+'+11)/12(4-k)'
           dcrdb[i]=k[i]/(12.*(4-k[i]))
        end 
        ;;;k>kt for structured jet implies no spreading
        ;;;need kerr for k check?
        'JETsISM2a':begin  ;;JET with no lateral spreading
           crval[i]=a-(6.*b+3.)/4.
           creq[i]=alpha+'=(6'+beta+'+3)/4'
           dcrdb[i]=3./2.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-3./4.
;           creq[i]=dap+'=3/4'
;           dcrdb[i]=0.
        end
        'JETsISM2b':begin  ;;JET with no lateral spreadin
           crval[i]=a-(6.*b+21.)/16.
           creq[i]=alpha+'=(6'+beta+'+21)/16'
           dcrdb[i]=3./8.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-3./4.
;           creq[i]=dap+'=3/4'
;           dcrdb[i]=0.
        end
        'JETsISM2ai': begin ;;JET with no spreading and with EI
;           ktgood=0
           q[i]=(4.*a-4.*b+2.)/(2.*b+5.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=4./(2.*b+5.)
           dqdb=(-8.*a-24.)/(2.*b+5.)^2.
           qerr[*,i]=[sqrt(dqda^2*alow^2+dqdb^2*blow^2),sqrt(dqda^2*aup^2+dqdb^2*bup^2)]
           crval[i]=a-((6.*b+3.)/4.-1./4.*(1-qq)*(2.*b+5.))
           creq[i]=alpha+'=(6'+beta+'+3)/4-1/4(1-q)(2'+beta+'+5)'
           dcrdb[i]=(1.-qq/2.)
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-4.*delalp/(2.*b+5.)
;           qerr[*,i]=[sqrt((4./(2.*b+5.))^2.*delalperr[0]^2+(-8.*delalp/(2.*b+5)^2)^2.*blow^2),sqrt((4./(5.+2.*b))^2.*delalperr[1]^2+(-8.*delalp/(2.*b+5)^2)^2.*bup^2)]
;           crval[i]=delalp-(1.-q[i])*(2.*b+5.)/4.
;           creq[i]=dap+'=(1-q)(2'+beta+'+5)/4'
;           dcrdb[i]=(1.-q[i])/2.
        end
        'JETsISM3a':begin
           crval[i]=a-(6.*b+1.)/4.
           creq[i]=alpha+'=(6'+beta+'+1)/4'
           dcrdb[i]=3./2.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-3./4.
;           creq[i]=dap+'=3/4'
;           dcrdb[i]=0.

        end
        'JETsISM3b':begin
           crval[i]=a-(3.*b+11.)/8.
           creq[i]=alpha+'=(3'+beta+'+11)/8'
           dcrdb[i]=3./8.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-3./4.
;           creq[i]=dap+'=3/4'
;           dcrdb[i]=0.
        end
        'JETsISM3ai': begin ;;JET with no spreading and with EI
;           ktgood=0
           q[i]=(4.*a-4.*b+2.)/(2.*b+3.)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=4./(2.*b+3.)
           dqdb=(-8.*a-16.)/(2.*b+3.)^2.
           qerr[*,i]=[sqrt(dqda^2*alow^2+dqdb^2*blow^2),sqrt(dqda^2*aup^2+dqdb^2*bup^2)]
           crval[i]=a-((6.*b+1.)/4.-1./4.*(1-qq)*(2.*b+3.))
           creq[i]=alpha+'=(6'+beta+'+1)/4-1/4(1-q)(2'+beta+'+3)'
           dcrdb[i]=(1.-qq/2.)
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-4.*delalp/(2.*b+3.)
;           qerr[*,i]=[sqrt((4./(2.*b+3.))^2.*delalperr[0]^2+(-8.*delalp/(2.*b+3.)^2)^2.*blow^2),sqrt((4./(3.+2.*b))^2.*delalperr[1]^2+(-8.*delalp/(2.*b+3.)^2)^2.*bup^2)]
;           crval[i]=delalp-(1.-q[i])*(2.*b+3.)/4.
;           creq[i]=dap+'=(1-q)(2'+beta+'+3)/4'
;           dcrdb[i]=(1.-q[i])/2.
        end 
        'JETsWIND2a': begin
           crval[i]=a-(3.*b+2.)/2.
           creq[i]=alpha+'=(3'+beta+'+2)/2'
           dcrdb[i]=3./2.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-1./2.
;           creq[i]=dap+'=1/2'
;           dcrdb[i]=0.
        end 
        'JETsWIND2b': begin
           crval[i]=a-(2.*b+13.)/8.
           creq[i]=alpha+'=(2'+beta+'+13)/8'
           dcrdb[i]=1./4.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-1./2.
;           creq[i]=dap+'=1/2'
;           dcrdb[i]=0.
        end 
        'JETsWIND2ai': begin ;;JET with no spreading and with EI
;           ktgood=0
           q[i]=(2.*a-2.*b)/(2.+b)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(2.+b)
           dqdb=(-2.*a-4.)/(2.+b)^2.
           qerr[*,i]=[sqrt(dqda^2*alow^2+dqdb^2*blow^2),sqrt(dqda^2*aup^2+dqdb^2*bup^2)]
           crval[i]=a-((3.*b+2.)/2.-1./2.*(1-qq)*(b+2.))
           creq[i]=alpha+'=(3'+beta+'+2)/2-1/2(1-q)('+beta+'+2)'
           dcrdb[i]=(1.+qq/2.)
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-2.*delalp/(b+2.)
;           qerr[*,i]=[sqrt((2./(b+2.))^2.*delalperr[0]^2+(-4.*delalp/(b+2.)^2)^2.*blow^2),sqrt((2./(2.+b))^2.*delalperr[1]^2+(-4.*delalp/(b+2.)^2)^2.*bup^2)]
;           crval[i]=delalp-(1.-q[i])*(b+2.)/2.
;           creq[i]=dap+'=(1-q)('+beta+'+2)/2'
;           dcrdb[i]=(1.-q[i])/2.
        end
        'JETsWIND3a': begin
           crval[i]=a-3.*b/2.
           creq[i]=alpha+'=3'+beta+'/2'
           dcrdb[i]=3./2.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-1./2.
;           creq[i]=dap+'=1/2'
;           dcrdb[i]=0.
        end 
        'JETsWIND3b': begin
           crval[i]=a-(b+5.)/4.
           creq[i]=alpha+'=('+beta+'+5)/4'
           dcrdb[i]=1./4.
;           if delalp eq 0. then ktgood=0
;           crval[i]=delalp-1./2.
;           creq[i]=dap+'=1/2'
;           dcrdb[i]=0.
        end 
        'JETsWIND3ai': begin ;;JET with no spreading and with EI
;           ktgood=0
           q[i]=(2.*a-2.*b+2)/(2.+b)
           if q[i] gt 1 then qq=1. else qq=q[i]
           dqda=2./(2.+b)
           dqdb=(-2.*a-6.)/(2.+b)^2.
           qerr[*,i]=[sqrt(dqda^2*alow^2+dqdb^2*blow^2),sqrt(dqda^2*aup^2+dqdb^2*bup^2)]
           crval[i]=a-(3.*b/2.-1./2.*(1-qq)*(b+2.))
           creq[i]=alpha+'=(3'+beta+')/2-1/2(1-q)('+beta+'+2)'
           dcrdb[i]=(1.+qq/2.)
;           if delalp eq 0. then ktgood=0
;           q[i]=1.-2.*delalp/(b+2.)
;           qerr[*,i]=[sqrt((2./(b+2.))^2.*delalperr[0]^2+(-4.*delalp/(b+2.)^2)^2.*blow^2),sqrt((2./(2.+b))^2.*delalperr[1]^2+(-4.*delalp/(b+2.)^2)^2.*bup^2)]
;           crval[i]=delalp-(1.-q[i])*(b+2.)/2.
;           creq[i]=dap+'=(1-q)('+beta+'+2)/2'
;           dcrdb[i]=(1.-q[i])/2.
        end 
        else:
     endcase
     errvalup[i]=sqrt(aup^2.+bup^2*dcrdb[i]^2.)
     errvallow[i]=sqrt(alow^2.+blow^2*dcrdb[i]^2.)
     if dcrdb[i] eq 0. then begin 
        errvalup[i]=delalperr[1]
        errvallow[i]=delalperr[0]
     endif 
     if k[i] ne 0. then begin 
        errvalup[i]=sqrt(delalperr[1]^2.+bup^2*dcrdb[i]^2.)
        errvallow[i]=sqrt(delalperr[0]^2.+blow^2*dcrdb[i]^2.)
     endif 
     
     ;;;IS IT REALLY NECESSARY TO LIMIT P WITHIN 3 SIGMA OF 1-2 OR >2
     if bb[i] eq 0 then npg=1
     if bb[i] eq 1 then pgood=where(p[i]+perr[1,i] gt 2,npg)
;     print,'p = ',p[i],perr[0,i],perr[1,i],bb[i]
;     if bb[i] eq 2 then pgood=where(p[i]-perr[i] le 2. and p[i]+perr[i] ge 1.5,npg)
     if bb[i] eq 2 then pgood=where(p[i]-perr[0,i] le 2. and p[i]+perr[1,i] ge 1.0,npg)
;     if q[i] ne 0. then print,'q=',q[i],qerr[0,i],qerr[1,i]

;     if (((q[i]-qerr[0,i] lt 1. and q[i]+qerr[1,i] gt 0.) or ktgood) and npg gt 0 and know eq 0) or (know eq 1 and answer[i] eq 1) then begin 

     nsig0=1.0
;     if q[i] eq 0. then begin
        ;;if not EI, can eval chisq based on how good fit to CR is
        if crval[i] gt 0. then chisq[good[i]]=crval[i]^2./(errvallow[i])^2. else chisq[good[i]]=crval[i]^2./(errvalup[i])^2.
;        print,'cr Nsig=',sqrt(chisq[i]),crval[i]
;     endif else begin
;        if q[i] gt 1 then chisq[good[i]]=(q[i]-1.)^2/(qerr[0,i])^2.
        if q[i] lt 1. and q[i] ne 0 then chisq[good[i]]=1.0
        if q[i] ne 0 and q[i] ne 1. and chisq[good[i]] le nsig0^2. then begin 
           com=execute('qstr.'+cr[i]+'=q[i]')
           com=execute('qstr.'+cr[i]+'err=qerr[*,i]')
        endif
        if strpos(strtrim(cr[i],2),'f2') eq -1 then begin 
           com=execute('pstr.'+cr[i]+'=p[i]')
           com=execute('pstr.'+cr[i]+'err=perr[*,i]')
        endif 
;        if q[i] gt 1 then begin ;;;need to change this so that if q>1 then q=1 and evaluate crval
;           crval[i]=q[i]-1
;           errvallow[i]=qerr[0,i]
;           errvalup[i]=qerr[1,i]
;        endif 
;        print,'q Nsig=',sqrt(chisq[i])
;     endelse 

     if (((q[i]-qerr[0,i] lt 1.) or ktgood) and npg gt 0 and know eq 0) or (know eq 1 and answer[i] eq 1) or keyword_set(plotevery) then begin 
;     if ((ktgood and npg gt 0 and know eq 0) or (know eq 1 and answer[i] eq 1)) or (keyword_set(plotevery) and g[i] eq 1 and abs(crval[i]) lt 2) then begin 
;        if crval[i]-errvallow[i] lt 0.06 and crval[i]+errvalup[i] gt -0.06 and ((q[i]-qerr[0,i] lt 1.) or ktgood) then begin 
        if sqrt(chisq[good[i]]) le nsig0 and ktgood or keyword_set(plotevery) then begin 
        ;;;ALLOWED A LITTLE LEEWAY IN BEING CONSISTENT, RESET TO 0 TO BE STRICTER
;           colprint,cr[i],crval[i],errvallow[i],errvalup[i],k[i]
           
;        if crval[i]-errvallow[i] lt 0 and crval[i]+errvalup[i] gt 0 and ktgood then begin 
           gg=[gg,good[i]]
           plotsym,0,1,/fill
           psym=8
           if sqrt(chisq[good[i]]) le nsig0 and ktgood then psym=8 else psym=4
;        endif; else psym=4
;        gg=[gg,good[i]]
        if not keyword_set(noplot) then begin 
;           ii=i
           if k[i] ne 0. then kk='k='+numdec(k[i],2)+kkt else kk=''
           if q[i] ne 0. then begin 
              kk='q='+numdec(q[i],2)+'!S!L-'+numdec(qerr[0,i],2)+'!R!U+'+numdec(qerr[1,i],2)+'!N)' 
              kktx='q='+numdec(q[i],2)+'_{-'+numdec(qerr[0,i],2)+'}^{+'+numdec(qerr[1,i],2)+'}$)' 
           endif else begin
              kk=''
              kktx=''
           endelse  
           comma=','
           if pp[i] ne '' and kk eq '' then begin
              pp[i]=pp[i]+')'
              ppt[i]=ppt[i]+'$)'
              comma=''
           endif 
           if pp[i] eq '' and kk ne '' then begin
              kk='('+kk
              kktx='($'+kktx
              comma=''
           endif
           sp=' '
           if pp[i] eq '' and kk eq '' then begin
              ppt[i]=''
              kktx=''
              sp=''
              comma=''
           endif 
           plots,crval[i],ng-ii,psym=psym,color=color[i]
           oplot,[crval[i]-errvallow[i],crval[i]+errvalup[i]],[ng-ii,ng-ii],line=0,color=color[i]
;           print,cr[i],crval[i]
;           if crval[i] ge -4 and crval[i] lt 2 then xyouts,-3.8,ng-ii,cr[i]+' '+creq[i]+' '+pp[i]+'  '+kk,color=color[i],charsize=0.7
           if not keyword_set(incaption) then begin 
              if crval[i] ge -4 and crval[i] lt 2 then xyouts,-3.8,ng-ii,cr[i]+' '+pp[i]+'  '+kk,color=color[i],charsize=0.7
           endif else begin
              whseg=segment+letters[ii]
              caption=caption+whseg+' - {\it '+cr[i]+'}'+sp+ppt[i]+comma+kktx+'; '
              xyouts,-1.8+(ii mod 2)*0.2,ng-ii,whseg,charsize=1.
           endelse 
           if n_elements(segment) gt 0 then legend,ntostr(segment),box=0,/top,/right,charsize=1
           if keyword_set(plotevery) then ii=ii+0.5 else ii=ii+0.8
        endif
;        if k[i] ne 0. then stop
     endif 

     endif
;     xyouts,-3.8,n-i+1,cr[i]+' '+altcr[i]+'  '+creq[i]+' ',color=color[i],charsize=1.5
;     if ktgood eq 0 then stop
;     key=get_kbrd(10)
;     print,cr[i]
;     stop
  endfor 

  print,caption
  if n_elements(gg) gt 1 then gg=gg[1:*]
  if not keyword_set(noplot) then oplot,[0,0],[0,ng+5],line=2

;  for i=0,n_elements(m1)-1 do $
;     oplot,[crval[m1[i]]-errvallow[m1[i]],crval[m1[i]]+errvalup[m1[i]]],[m1[i]+1,m1[i]+1],line=0,color=!green
;  oplot,crval[m1],m1+1,psym=2,color=!green
;  !p.multi=0
;  endif else print,'No closure relations fit'
 
  return
end 
  
pro closure_relations,psc=psc,go=go,dir=dir,onlygo=onlygo,nsig=nsig,phil=phil,plotevery=plotevery,crstr=crstr,qstr=qstr,pstr=pstr
  
  if n_elements(nsig) eq 0 then nsig=3.
;  nsig=2.
  if nsig eq 2. then begin 
     append='_2s'
     int='9'
  endif else int='8'
  if nsig eq 2. then signame='_2sig' else signame='_3sig'
  
  cd,!mdata
  if keyword_set(onlygo) then goto,cheat
  g=0
  if n_elements(dir) eq 0 then dir=file_search('GRB*')
  nw=n_elements(dir)
  if nw lt 5 then begin 
     g=0
     nw=1
     nocd=0
  endif else begin
     nocd=0
     stop
  endelse 
  count=0

  for i=g,nw-1 do begin 
     if not nocd then cd,dir[i]
     print
     print,dir[i],i
;     lcfile0='lc_fit_out_idl.dat'
     lcfile='lc_fit_out_idl_int'+int+'.dat'
     lcoutfile='lc_newout_noflares.txt'
     if not exist(lcoutfile) then lcoutfile='lc_newout.txt'
     if keyword_set(phil) then lcoutfile='lc_newout_phil.txt'
     if exist(lcfile) then begin ;or exist(lcfile0) then begin 
        if numlines(lcfile) gt 1 then begin 
           read_lcfit,lcfile,pname,p,perror,chisqs,dof,breaks,lc=lc
           wnotfin=where(finite(perror[0,*]) eq 0,nwnot)
           if nwnot gt 0 then perror[0,wnotfin]=perror[1,wnotfin]
           wnotfin=where(finite(perror[1,*]) eq 0,nwnot)
           if nwnot gt 0 then perror[1,wnotfin]=perror[0,wnotfin]
           if breaks eq 0 then count=count+1
           if n_elements(pname) gt 1 then begin 
;              fit_lc,/justplot,name=dir[i],lc=rlc,ps=ps
              if keyword_set(psc) then begplot,name='closure_relations'+signame+'.ps',/color ;,/land
              if exist('spec/seg1.dat') then begin 
                 read_specfit,spec,dir='spec',append=append
                 if n_elements(spec) eq 1 and spec[0].pow eq 0 then print,'No specfit' else begin 
                    
;              erase
;              multiplot,[1,breaks+3],/init
;              !p.multi=[0,1,breaks+2]
;              plot_like_qdp,name=dir[i],pmulti=[0,1,breaks+2]
;              multiplot
;              fit_lc,/justplot,charsize=1.5,name=dir[i],lc=rlc,xtitle=''

;                 fit_lc,/justplot,pmulti=[0,1,breaks+3],name=dir[i],charsize=1.5,lc=rlc
                    erase
                    multiplot2,[1,breaks+2],/init
                    multiplot2,ydowngap=0.08
                    fit_lc,/justplot,name=dir[i],charsize=1.5,lc=rlc,/noinit,/noxaxis,phil=phil,int=int
                    lcout=lcout2fits(lcoutfile)
                    wrlc=where(rlc.tot_hard gt 0 and finite(rlc.tot_hard),nwrlc)
;              multiplot
;                 !y.margin=[3,1]
;                 if nwrlc gt 0 then begin
;                    ploterror,rlc[wrlc].time,rlc[wrlc].tot_hard,rlc[wrlc].tot_hard_err,psym=3,/nohat,xtitle='Time since BAT trigger (s)',ytitle='hard ratio',charsize=1.5,/xlog,/ylog,yrange=minmax(rlc[wrlc].tot_hard) ;[min(rlc[wrlc].tot_hard-rlc[wrlc].tot_hard_err),max(rlc[wrlc].tot_hard+rlc[wrlc].tot_hard_err)]
;                    for r=0,n_elements(rlc[wrlc])-1 do oplot,[rlc[wrlc[r]].tstart,rlc[wrlc[r]].tstop],[rlc[wrlc[r]].tot_hard,rlc[wrlc[r]].tot_hard]
;                 endif 
                    alphas=p[indgen(breaks+1)*2+1]
                    alphaserr=perror[*,indgen(breaks+1)*2+1]
                    make_crstruct,breaks+1,crstr

;              multiplot
;              plot,indgen(10),color=0,/nodata
                    noj=intarr(breaks+1)
                    
                    crstr.tstart=min(lcout.tstart)
                    crstr.tstop=max(rlc.tstop)
                    wdet=where(rlc.src_rate_err gt 0.,nwdet)
;                 if nwdet le 3 then stop
                    crstr.tlastdet=rlc[wdet[nwdet-1]].tstop
                    case breaks of 
                       0: model='pow'
                       1: model='bknpow'
                       2: model='bkn2pow'
                       3: model='bkn3pow'
                    endcase 
                    times=['tstop','tlastdet']
              ;;;loop over times of interest and find fit count rate (ctr)
                    for j=0,n_elements(times)-1 do $
                       tmp=execute('crstr.ctr_'+times[j]+'='+model+'(crstr.'+times[j]+',p)')
                    tmp=execute('crstr.ctr_t1day[*]='+model+'(86400.,p)')

                    for j=0,breaks do begin
                       crstr[j].grb=dir[i]
                                ;                multiplot
                       alpha=p[j*2+1]
                       alplow=perror[0,j*2+1]
                       alphigh=perror[1,j*2+1]
                       if alplow eq 0. then alplow=alpha*0.3
                       if alphigh eq 0. then alphigh=alpha*0.3
                       
                       if j gt 0 then begin
                          crstr[j].tbreak=p[(j-1)*2+2]
                          crstr[j].tbreakerr=perror[*,(j-1)*2+2]
                       endif 
                       crstr[j].alpha=alpha
                       crstr[j].alphaerr=[alplow,alphigh]
;                    if n_elements(spec) gt breaks then begin 
                       crstr[j].beta=spec[j].pow-1
                       wnot0=where(spec.pow ne 0.)
                       if spec[j].pow eq 0 then begin
                          crstr[j].beta=mean(spec[wnot0].pow-1)
                          crstr[j].betaerr[0]=mean(spec[wnot0].pow_err[0])
                          crstr[j].betaerr[1]=mean(spec[wnot0].pow_err[1])
                          spec[j].pow=mean(spec[wnot0].pow)
                          spec[j].pow_err[0]=mean(spec[wnot0].pow_err[0])
                          spec[j].pow_err[1]=mean(spec[wnot0].pow_err[1])
                       endif 
                       crstr[j].betaerr=spec[j].pow_err
                       crstr[j].flux=spec[j].unabs_flux
                       crstr[j].rate=spec[j].rate
                                ;                   endif 

                       which_alpha,alphas,alphaserr,j,posas,delalp,delalperr
                       posa=posas[*,j]
                       print,posa
                       if j ne 0 then begin
                          dalp=delalp[j-1]
                          dalperr=delalperr[*,j-1]
                       endif else begin 
                          dalp=0.
                          dalperr=[0.,0.]
                       endelse 
                       print,dalp,dalperr
                       print,'Fitting Closure Relations for Segment '+ntostr(j+1)
                       if nwdet gt 2 then begin 
                          if n_elements(spec)+1 gt breaks then begin 
                             if spec[j].pow ne 0. then begin 
                                print,alpha,alplow,alphigh,spec[j].pow,spec[j].pow_err[0],spec[j].pow_err[1]
                                multiplot2,ydowngap=0
                                if j eq breaks then xtitle='closure relations' else xtitle=''
                                which_closure_relation,alpha,alplow,alphigh,spec[j].pow,spec[j].pow_err[0],spec[j].pow_err[1],cr,posa,dalp,dalperr,gg,chisq=chisq,psc=psc,qstr=qstr0,nsig=nsig,xtitle=xtitle,plotevery=plotevery,pstr=pstr0
                                if j gt 0 then begin
                                   concat_structs,qstr,qstr0,qstrcomb
                                   qstr=qstrcomb
                                   concat_structs,pstr,pstr0,pstrcomb
                                   pstr=pstrcomb
                                endif else begin
                                   qstr=qstr0
                                   pstr=pstr0
                                endelse 
                                if n_elements(posa) eq 2 then posa=posa[0]
                                for pa=1,4 do begin 
                                   w=where(posa eq pa,nw)
                                   if n_elements(posa) ne 4 then begin 
                                      if nw eq 1 then com=execute('crstr[j].seg'+ntostr(pa)+'=1') 
                                   endif else crstr[j].seg0=1
                                endfor 
                                if gg[0] ne -1 then $
                                   for c=0,n_elements(gg)-1 do com=execute('crstr[j].(gg[c]+6)=sqrt(chisq[gg[c]])')
                                noj[j]=1
                             endif else begin
                                print,'No Spectral fit available'
                             endelse 
                          endif else begin 
                             print,'No Spectral fit available'
                          endelse 
                          if breaks gt 0 then tmp=execute('crstr[j].ctr_tbreak='+model+'(crstr[j].tbreak'+',p)')
                       endif
                    endfor
                    multiplot2,/reset
                    multiplot2,/default
;                 wj=where(noj ne 0,nwj)
;                 if nwj gt 0 then crstr=crstr[wj]
                    elim=1
                    while elim gt 0 do begin
                       cr_consistency_check,crstr,elim,qstr,pstr,nsig=nsig
                       print,elim
                    endwhile 

;              cr_consistency_check,crstr ;;run twice to adapt to already removed possibilities

                    if n_elements(spec) eq 1 and spec[0].pow eq 0. or nwdet le 2 then begin
                       if exist('closure_relations'+signame+'.fits') then spawn,'rm closure_relations'+signame+'.fits'
                       print,'No spec fits' 
                    endif else begin
                       mwrfits,crstr,'closure_relations'+signame+'.fits',/create
                       mwrfits,qstr,'qstr'+signame+'.fits',/create
                       mwrfits,pstr,'pstr'+signame+'.fits',/create
                    endelse 
                    !p.multi=0
                    !y.margin=[4,2]
                    if keyword_set(psc) then endplot
;              multiplot,/reset
                    if not keyword_set(go) then k=get_kbrd(10) else k=''
                    if k eq 's' then stop
                    if k eq 'q' then begin
                       cd,!mdata
                       return
                    endif 
                 endelse 
              endif else print,'No specfit' 
           endif 
        endif else print,'No good temporal fit'
     endif else print,'No '+lcfile
     if not nocd then cd,!mdata
  endfor
  
  print,'Count of 0 breaks: ',count
  if nw gt 1 then begin
     cat_crstructs,cr,nsig=nsig
     maybe_jet_break,cr,nsig=nsig,phil=phil
     calc_jet_stuff,cr,nsig=nsig
     stop
  endif 
  
  cheat:
  if keyword_set(go) then begin
     cat_crstructs,cr,nsig=nsig
     maybe_jet_break,cr,nsig=nsig,phil=phil
     when_maybe_jetbreak,cr=cr,/go,nsig=nsig,phil=phil
     calc_jet_stuff,cr,nsig=nsig
     cr_paper_results,nsig=nsig
  endif 
  
  ;;;plot cases for each burst
  ;;use gamma to get beta
  ;;use beta to get nu
  ;;for nu with p options, use beta to get p
  ;;then have closure relation for each case (ISM, WIND, JET, fast, slow, etc)
  
  return
end 


;;run closure_relations for each burst
;;run cat_crstructs,cr
;;run maybe_jet_break,cr
;;calc_jet_stuff,cr
