@fit_functions
@calc_eiso
@calc_eiso2
@calc_dust_corr
pro lum_function

  dir='~/Fermi/Swift_pop_study/lum_function/'
  cd,dir
  readcol,'rate_and_luminosity.txt',lmin,lmax,lum,dphi,phierr1,phierr2,/silent
  readcol,'rate_and_luminosity.txt',logl,s3min,s2min,s1min,best,s1max,s2max,s3max,/silent
  w=where(lmin gt 40 and lmax gt 40)
  bat=w[0:9]
  obat=w[10:20]
  gbm=w[21:28]
  ogbm=w[29:36]
  lat=w[37:42]
  olat=w[43:48]
  db=indgen(47)
  dg=indgen(88-47)+48
  dl=indgen(117-88)+89

  files=['bat_L_LF1.dat','gbm_L_LF1.dat','lat_L_LF2.dat']
;  colors=lonarr(101)
;  for i=0,100 do temp=execute('colors[i]=!grey'+ntostr(i))
  begplot,name='lum_function.eps',/encap,/color
  colors=[!p.color,!grey50,!grey70]
  leg=['BAT','GBM','LAT']
  scale=[0.15,0.3,1]*1d
  !x.margin=[7,3]
;  !p.multi=[0,1,3]
  multiplot,[1,3],/init
  plotsym,0,1,/fill
  xtitle=''
  xtickname=' '
  ytitle=''
  for s=0,2 do begin 
     readcol,files[s],a,lb,p
     nx=n_elements(rem_dup(a))
     ny=n_elements(rem_dup(lb))

;     z=fltarr(nx,ny) & x=z & y=z
;     q=0
;     for i=0,nx-1 do begin
;        for j=0,ny-1 do begin  
;           x[i,j]=a[q]
;           z[i,j]=p[q]
;           y[i,j]=lb[q]
;           q+=1
;        endfor 
;     endfor 
;     print,minmax(z)
     case s of
        0: begin 
           who=bat
           h=db
        end
        1: begin
           who=gbm
           h=dg
        end 
        2: begin
           who=lat
           h=dl
        end 
     endcase 

     na=n_elements(a)
     cc=intarr(na)
     nn=100.
     m=0.1
     l=[indgen(nn)*m*0.01+0.01,indgen(nn)*m*0.1+0.1,indgen(nn)*m+1.,indgen(nn)*m*10.+10.,indgen(nn)*m*100.+100.,indgen(nn)*m*1000.+1000,indgen(nn)*m*1d4+1d4]*1d50
     l=l[rem_dup(l)]
     n=n_elements(l)
     phi=dblarr(na,n)
     multiplot
     if s eq 2 then begin
        xtitle='15-150 keV Peak Luminosity (erg/s)' 
;        xtickname='10!U'+ntostr(48+indgen(8))
     endif 
     if s eq 1 then ytitle=!tsym.phi+' (arbitrarily scaled)' else ytitle=''
     plot,[1d49,1d55],[1d-3,1e2],/nodata,/xlog,/ylog,charsize=1.5,xtitle=xtitle,ytitle=ytitle,/ysty,ytickformat='loglabels',/xsty;,xtickname=xtickname
     for i=0,n_elements(a)-1 do begin 
        if s le 1 then begin
           alpha1=a[i] 
           alpha2=2.2
        endif else begin
           alpha1=0.2
           alpha2=a[i]
        endelse 
        phi[i,*]=smbknpow(l,[scale[s],alpha1,lb[i]*1d50,alpha2])
        c=0
        if p[i] le 5.25 then c=1
        if p[i] le 2.04 then c=2
        if p[i] le 0.045 then c=3
        cc[i]=c
     endfor 
     
     prob=[5.25,2.04,0.045]
     ind=[0,0,694]
     for j=1,3 do begin 
;        nw=3
;        v=approx(p,prob[j-1],w,nw)
;        colprint,v,phi[w,0]
;        mm=minmax(phi[w,0],wmm)
;        w=w[wmm]
        nw=1
        v=where(cc eq j,nv)
        if nv ne 0 then begin 
        vv=minmax(phi[v,ind[s]],w)
        w=v[w]
        print,phi[w,ind[s]]
        print,p[w]
        for i=0,nw-1 do begin 
           x=[l,reverse(l),l[0]]
           y=dblarr(n_elements(x))
           y[0:n-1]=phi[w[i],*]
           y[n:n*2-1]=reverse(phi[w[i+1],*],2)
           y[n*2]=phi[w[i],0]
           wlim=where(y le 1d2 and y ge 1d-3 and x gt 1d49 and x lt 1d55)           
           x=x[wlim]
           y=y[wlim]
           polyfill,x,y,color=colors[j-1]
           oplot,l,phi[w[i],*],color=colors[j-1]
;           for z=0,n_elements(phi[*,0])-1 do oplot,l,phi[z,*],color=colors[j-1]
        endfor 
     endif 
     endfor 
     oplot,10d^lum[who],dphi[who],psym=8
     for q=0,n_elements(who)-1 do begin 
        oplot,10d^[lmin[who[q]],lmax[who[q]]],[dphi[who[q]],dphi[who[q]]];*scale[s]
        oplot,10d^[lum[who[q]],lum[who[q]]],[dphi[who[q]]-[phierr1[who[q]]],[phierr2[who[q]]]+dphi[who[q]]];*scale[s]
     endfor 
     oplot,10d^logl[h],s1min[h],color=!orange
     oplot,10d^logl[h],s1max[h],color=!orange
     oplot,10d^logl[h],s2min[h],color=!red
     oplot,10d^logl[h],s2max[h],color=!red
     oplot,10d^logl[h],s3min[h],color=!firebrick
     oplot,10d^logl[h],s3max[h],color=!firebrick

     legend,leg[s],box=0,/bottom,/left
     if s eq 0 then legend,['1','2','3']+!tsym.sigma,box=0,/top,/right,textcolor=reverse(colors)
;     contour,z,x,y,levels=[0.68,0.954,0.997],charsize=2,xtitle='alpha',ytitle='L break';,yrange=[200,1200]
  endfor 
  multiplot,/reset,/default
;  !p.multi=0
  endplot

stop
  b=db
  g=dg
  l=dl
  begplot,name='david_lum_function.eps',/land,/color
  multiplot,[1,3],/init
  multiplot
  plot,[48,55],[1e-3,10],/nodata,/ylog;,xtitle='log L',ytitle=!tsym.phi_cap
  oploterror2,lum[bat],phi[bat],[[lum[bat]-lmin[bat]],[lmax[bat]-lum[bat]]],[[phierr1[bat]],[phierr2[bat]]],/nohat,psym=1
;  oplot,[logl[b],reverse(logl[b]),logl[b[0]]],[s1min[b],s1max[b],s1min[b[0]]]
  oplot,logl[b],s1min[b],color=!orange
  oplot,logl[b],s1max[b],color=!orange
  oplot,logl[b],s2min[b],color=!red
  oplot,logl[b],s2max[b],color=!red
  oplot,logl[b],s3min[b],color=!firebrick
  oplot,logl[b],s3max[b],color=!firebrick

  legend,['BAT'],/top,/right,box=0

  multiplot
  plot,[48,55],[1e-3,10],/nodata,/ylog,ytitle=!tsym.phi_cap
  oploterror2,lum[gbm],phi[gbm],[[lum[gbm]-lmin[gbm]],[lmax[gbm]-lum[gbm]]],[[phierr1[gbm]],[phierr2[gbm]]],/nohat,psym=1
  oplot,logl[g],s1min[g],color=!orange
  oplot,logl[g],s1max[g],color=!orange
  oplot,logl[g],s2min[g],color=!red
  oplot,logl[g],s2max[g],color=!red
  oplot,logl[g],s3min[g],color=!firebrick
  oplot,logl[g],s3max[g],color=!firebrick
  legend,['GBM'],/top,/right,box=0

  multiplot
  plot,[48,55],[1e-3,10],/nodata,/ylog,xtitle='log L'
  oploterror2,lum[lat],phi[lat],[[lum[lat]-lmin[lat]],[lmax[lat]-lum[lat]]],[[phierr1[lat]],[phierr2[lat]]],/nohat,psym=1
  oplot,logl[l],s1min[l],color=!orange
  oplot,logl[l],s1max[l],color=!orange
  oplot,logl[l],s2min[l],color=!red
  oplot,logl[l],s2max[l],color=!red
  oplot,logl[l],s3min[l],color=!firebrick
  oplot,logl[l],s3max[l],color=!firebrick

  legend,['LAT'],/top,/right,box=0
  multiplot,/reset,/default
endplot
;  oploterror2,lum[obat],phi[obat],[[lum[obat]-lmin[obat]],[lmax[obat]-lum[obat]]],[[phierr1[obat]],[phierr2[obat]]],/nohat,psym=1
;  oploterror2,lum[ogbm],phi[ogbm],[[lum[ogbm]-lmin[ogbm]],[lmax[ogbm]-lum[ogbm]]],[[phierr1[ogbm]],[phierr2[ogbm]]],/nohat,psym=1,color=!blue
;  oploterror2,lum[olat],phi[olat],[[lum[olat]-lmin[olat]],[lmax[olat]-lum[olat]]],[[phierr1[olat]],[phierr2[olat]]],/nohat,psym=1,color=!red

  stop
  return
end 

pro fill_parstat,parstat
  
  ;;; loop over pars and samples and long/short in grbstr, all info
  ;;; should be in grbstr
  parstat=create_struct('who','','dur','',$
                        'xalpha_mean',0d,'xalpha_sig',0d,'xalpha_med',0d,'xalpha_n',0,$
                        'xbeta_mean',0d,'xbeta_sig',0d,'xbeta_med',0d,'xbeta_n',0,$
                        'oalpha_mean',0d,'oalpha_sig',0d,'oalpha_med',0d,'oalpha_n',0,$
                        'obeta_mean',0d,'obeta_sig',0d,'obeta_med',0d,'obeta_n',0,$
                        'z_mean',0d,'z_sig',0d,'z_med',0d,'z_n',0,$
                        'av_mean',0d,'av_sig',0d,'av_med',0d,'av_n',0,$
                        'nh_mean',0d,'nh_sig',0d,'nh_med',0d,'nh_n',0,$
                        'xlum_11hr_mean',0d,'xlum_11hr_sig',0d,'xlum_11hr_med',0d,'xlum_11hr_n',0,$
                        'xlum_1day_mean',0d,'xlum_1day_sig',0d,'xlum_1day_med',0d,'xlum_1day_n',0,$
                        'olum_11hr_mean',0d,'olum_11hr_sig',0d,'olum_11hr_med',0d,'olum_11hr_n',0,$
                        'olum_1day_mean',0d,'olum_1day_sig',0d,'olum_1day_med',0d,'olum_1day_n',0,$
                        'eiso_mean',0d,'eiso_sig',0d,'eiso_med',0d,'eiso_n',0,$
                        'thetaj_mean',0d,'thetaj_sig',0d,'thetaj_med',0d,'thetaj_n',0,$
                        'egam_mean',0d,'egam_sig',0d,'egam_med',0d,'egam_n',0,$
                        'nthetaj_mean',0d,'nthetaj_sig',0d,'nthetaj_med',0d,'nthetaj_n',0,$
                        'negam_mean',0d,'negam_sig',0d,'negam_med',0d,'negam_n',0,$
                        'eke_mean',0d,'eke_sig',0d,'eke_med',0d,'eke_n',0,$
                        'eta_mean',0d,'eta_sig',0d,'eta_med',0d,'eta_n',0)

  parstat=replicate(parstat,6) ;;; long and short

  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  pars=['xalpha','oalpha','xbeta','obeta','z','av','nh','xlum_11hr','xlum_1day','olum_11hr','olum_1day','eiso','thetaj','nthetaj','egam','negam','eke','eta']
  
;  texpars=['$\alpha_x$','$\alpha_o$','$\beta_x$','$\beta_o$','$z$','$A_V(\textrm{mag})$','$log~N_H(\textrm{cm}^{-2})$','\tablenotemark{\ddag}$log~L_{x,11~hr}$','\tablenotemark{\ddag}$log~L_{x,1~day}$','\tablenotemark{\ddag}$log~L_{o,11~hr}$','\tablenotemark{\ddag}$log~L_{o,1~day}$','\tablenotemark{\dag}$log~E_{\gamma,iso}$','$\theta_j(\textrm{deg})$','\tablenotemark{$\ast$}$\theta_{j,lim}(\textrm{deg})$','\tablenotemark{\dag}$log~E_{\gamma}$','\tablenotemark{$\ast$\dag}$log~E_{\gamma,lim}$','\tablenotemark{\dag}$log~E_k$','$\eta(\%)$']
  texpars=['$\alpha_x$','$\alpha_o$','$\beta_x$','$\beta_o$','$z$','$A_V~(\textrm{mag})$','$log~N_H~(\textrm{cm}^{-2})$','$log~L_{x,11~hr} ~(\textrm{erg~s}^{-1})$','$log~L_{x,1~day}~(\textrm{erg~s}^{-1})$','$log~L_{o,11~hr}~(\textrm{erg~s}^{-1})$','$log~L_{o,1~day}~(\textrm{erg~s}^{-1})$','$log~E_{\gamma,iso}~(\textrm{erg})$','$\theta_j~(\textrm{deg})$','\tablenotemark{$\ast$}$\theta_{j,lim}~(\textrm{deg})$','$log~E_{\gamma}~(\textrm{erg})$','\tablenotemark{$\ast$}$log~E_{\gamma,lim}~(\textrm{erg})$','$log~E_k~(\textrm{erg})$','$\eta~(\%)$']
  logpars=['','','','','','','log~','log~','log~','log~','log~','log~','','','log~','log~','log~','']
;;;NEED TO FINISH FIXING THIS - LOG ONLY LOG PARAMS
  npars=n_elements(pars)
  samp=['BAT','GBM','LAT']
  dur=['long','short']

  begplot,name='~/Fermi/Swift_pop_study/param_dists.ps',/land,/color
  for p=0,npars-1 do begin ;;; pars
     x=0
     for d=0,1 do begin       ;;; long/short
        for s=0,2 do begin    ;;; samp

           if pars[p] ne 'nthetaj' and pars[p] ne 'negam' and $
              pars[p] ne 'thetaj' and pars[p] ne 'egam' then begin 
              tmp=execute('w=where(strtrim(grbstr.who,2) eq samp[s] and strtrim(grbstr.dur,2) eq dur[d] and grbstr.'+pars[p]+' ne 0 and grbstr.'+pars[p]+' ne -1 and finite(grbstr.'+pars[p]+'),nw)')
              par=pars[p]
           endif else begin
              if pars[p] eq 'thetaj' or pars[p] eq 'egam' then begin 
                 tmp=execute('w=where(strtrim(grbstr.who,2) eq samp[s] and strtrim(grbstr.dur,2) eq dur[d] and grbstr.'+pars[p]+' ne 0 and finite(grbstr.'+pars[p]+') and grbstr.xjb eq 1,nw)')
                 par=pars[p]
              endif else begin 
                 par=strtrim(strmid(pars[p],1,strlen(pars[p])-1),2)
                 tmp=execute('w=where(strtrim(grbstr.who,2) eq samp[s] and strtrim(grbstr.dur,2) eq dur[d] and grbstr.'+par+' ne 0 and finite(grbstr.'+par+') and grbstr.xjb eq 0,nw)')
              endelse 
           endelse 
           parstat[x].who=samp[s]
           parstat[x].dur=dur[d]
;colprint,pars[p],par
           print,pars[p],samp[s],dur[d],nw
           tmp=execute('val=grbstr[w].'+par)
           if nw gt 1 then begin 
              if logpars[p] eq '' then begin 
                 tmp=execute('parstat[x].'+pars[p]+'_mean=mean(grbstr[w].'+par+')')
                 tmp=execute('parstat[x].'+pars[p]+'_med=median(grbstr[w].'+par+')')
                 tmp=execute('parstat[x].'+pars[p]+'_sig=stddev(grbstr[w].'+par+')') 
              endif else begin 
                 tmp=execute('parstat[x].'+pars[p]+'_mean=mean(alog10(grbstr[w].'+par+'))')
                 tmp=execute('parstat[x].'+pars[p]+'_med=median(alog10(grbstr[w].'+par+'))')
                 tmp=execute('parstat[x].'+pars[p]+'_sig=stddev(alog10(grbstr[w].'+par+'))')
              endelse                  
           endif else begin 
;              print,'hey',nw
              if nw eq 1 and logpars[p] eq '' then begin
                 tmp=execute('parstat[x].'+pars[p]+'_med=grbstr[w].'+par) 
                 tmp=execute('parstat[x].'+pars[p]+'_mean=grbstr[w].'+par) 
              endif else begin 
                 tmp=execute('parstat[x].'+pars[p]+'_med=alog10(grbstr[w].'+par+')')
                 tmp=execute('parstat[x].'+pars[p]+'_mean=alog10(grbstr[w].'+par+')')
              endelse 
              tmp=execute('parstat[x].'+pars[p]+'_sig=0')
;              tmp=execute('parstat[x].'+pars[p]+'_med=0')
           endelse 
           tmp=execute('parstat[x].'+pars[p]+'_n='+ntostr(nw))
           if logpars[p] eq 'log~' then val=alog10(val)
           if nw gt 1 then begin 
              plothist,val,bin=0.1,title=pars[p]+' '+samp[s]+' '+dur[d]
              mval=median(val)
              meval=mean(val)
              stval=stddev(val)
              oplot,[mval,mval],[0,200],line=1,color=!blue
              oplot,[meval,meval],[0,200],line=1,color=!green
              oplot,[mval-stval,mval-stval],[0,200],line=2,color=!red
              oplot,[mval+stval,mval+stval],[0,200],line=2,color=!red
              legend,[ntostr(mval),ntostr(stval)],box=0,/top,/right
;              k=get_kbrd(10)
           endif 
           x=x+1
           
        endfor 
     endfor 
  endfor 
  endplot
;;;; latex table
;  pars=['xalpha','oalpha','xbeta','obeta','z','xlum_11hr','xlum_1day','olum_11hr','olum_1day','eiso','thetaj','nthetaj','egam','negam','eke','eta']

  a=' & '
;  aa='$ & $'
;  tags=tag_names(parstat)
;  tag=strarr((n_elements(tags)-2)/3)
;  j=2
;  for i=0,n_elements(tag)-1 do begin
;     x=strlowcase(strsplit(tags[j],'_',/ex))
;     tag[i]=x[0]
;     j=j+3
;  endfor 

;  match,pars,tag,m1,m2
;  colprint,pars[m1],tag[m2]
  
 par=strarr(npars)
 for p=0,npars-1 do begin
    par=''
    x=0
    for s=0,2 do begin 
       for d=0,1 do begin 
          tmp=execute('med=parstat[x].'+pars[p]+'_med')
          tmp=execute('men=parstat[x].'+pars[p]+'_mean')
          tmp=execute('sig=parstat[x].'+pars[p]+'_sig')
          tmp=execute('n=parstat[x].'+pars[p]+'_n')
          medlog=round(alog10(med)-0.5)
;          siglog=round(alog10(sig)-0.5)
          
          if med ne 0 then begin 

;             if medlog gt 2 then mlog='('+numdec(med/10d^medlog,1) else mlog=numdec(med,1)
;             if medlog gt 2 then slog=numdec(sig/10d^medlog,1)+')
;             \times 10^{'+ntostr(medlog)+'}' else slog=numdec(sig,1)
             mlog=numdec(med,2)
             slog=numdec(sig,2)
             nlog=numdec(men,2)
;             if p eq 12 then mlog=numdec(med,2)
;             if medlog gt 2 then stop

;             pp='$'+numdec(med,3)+mlog+' \pm '+numdec(sig,1)+slog+'$'
;             pp='$'+mlog+' \pm '+slog+'$'
             pp='$'+nlog+'~('+slog+',~'+ntostr(n)+')$'
             if sig eq 0 then pp='$'+nlog+'$'
          endif else pp='--'
;          if s gt 0 then else par=pp
          par=par+a+pp 
          x=x+1
       endfor 
    endfor
    colprint,texpars[p]+par+' \\'
 endfor 


stop

  return
end
pro collect_for_sam

  goto,jump

  cd,'~/GRBs/'
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  grb=strtrim(grbstr.grb,2)
  ng=n_elements(grb)

  t=[400d,1000d,39600.,86400d]
  f400=dblarr(ng) & f1000=f400 & f11=f400 & f1=f400
  par=['f400','f1000','f11','f1']
  for i=0,ng-1 do begin 
     cd,grb[i]
     lcfitfile='lc_fit_out_idl_int8.dat'
     if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pname,p,perr,np=np
        lc=lcout2fits('lc_newout_phil.txt')
        det=where(lc.src_rate_err gt 0)
        tldet=max(lc[det].tstop)
        tfdet=min(lc[det].tstart)
        case np of
           2: mo='pow'
           4: mo='bknpow'
           6: mo='bkn2pow'
           8: mo='bkn3pow'
           10: mo='bkn4pow'
        endcase
        
        for j=0,3 do if tldet gt t[j] and tfdet lt t[j] then tmp=execute(par[j]+'[i]='+mo+'('+ntostr(t[j])+',p)*grbstr[i].xfluxfact') else print,'x'
     endif 
     cd,'..'
  endfor 

  calc_ke,/x,eket=eke11,m2=m11,eke1=eke1_11,eke2=eke2_11
  calc_ke,/x,eket=eke1,m2=m1,/day,eke1=eke1_1,eke2=eke2_1
  ek11=dblarr(ng)
  ek11[m11]=eke11
  ek1=dblarr(ng)
  ek1[m1]=eke1
  w=where(ek11 eq 0,nw)
  for i=0,nw-1 do begin
     if eke1_11[w[i]] eq 0 and eke2_11[w[i]] gt 0 then ek11[w[i]]=eke2_11[w[i]]
     if eke1_11[w[i]] gt 0 and eke2_11[w[i]] eq 0 then ek11[w[i]]=eke1_11[w[i]]
     if eke1_11[w[i]] gt 0 and eke2_11[w[i]] gt 0 then ek11[w[i]]=min([eke1_11[w[i]],eke2_11[w[i]]])
  endfor 

  w=where(ek1 eq 0,nw)
  for i=0,nw-1 do begin
     if eke1_1[w[i]] eq 0 and eke2_1[w[i]] gt 0 then ek1[w[i]]=eke2_1[w[i]]
     if eke1_1[w[i]] gt 0 and eke2_1[w[i]] eq 0 then ek1[w[i]]=eke1_1[w[i]]
     if eke1_1[w[i]] gt 0 and eke2_1[w[i]] gt 0 then ek1[w[i]]=min([eke1_1[w[i]],eke2_1[w[i]]])
  endfor 

  colprint,grbstr.grb,f400,f1000,f11,f1,ek11,ek1,grbstr.eiso,grbstr.bat_fluence

  jump:

  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  grb=strtrim(grbstr.grb,2)
  w=where(grbstr.oflux_11hr_obs gt 0,ng)
  grb=strtrim(grb[w],2)
  grbstr=grbstr[w]

  cd,'~/Fermi/Swift_pop_study/'
  grbdir=grbstr.who+'_UVOT_LC/'+grb
  t=3d5
  fd=dblarr(ng)
  for i=0,ng-1 do begin
     lcfitfile=grbdir[i]+'/lc_fit_out_idl_int8.dat'
     if not exist(lcfitfile) then lcfitfile=grbdir[i]+'/lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pname,p,perr,np=np
        if np eq 3 then f=pow(t,p)+p[np-1]
        if np eq 5 then f=bknpow(t,p)+p[np-1]
        if np eq 7 then f=bkn2pow(t,p)+p[np-1]
        if np eq 9 then f=bkn3pow(t,p)+p[np-1]
        fd[i]=f*1.63d-16*grbstr[i].hostfact
     endif 
  endfor

  w=where(fd gt 1d-16)
  colprint,grb[w],fd[w]
 
stop
end
pro uvot_grb_stats
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  cd,'~/Fermi/Swift_pop_study/'
  kdir='BAT_UVOT_LC/'
  cd,kdir
  w=where(grbstr.who eq 'BAT' and grbstr.olumfact ne 0)
  dir=strtrim(grbstr[w].grb,2)
  n=n_elements(dir)
;  fluxfact=[2.614,1.472,1.63,4.00,8.50,6.2,0.37]*1d-16    ;; erg cm-2 s-1 ang-1
;  ufluxfact=fluxfact[2] 

  lum4=dblarr(n)
  for i=0,n-1 do begin 
     cd,dir[i]
     lc=lcout2fits()
     lum=lc.src_rate*grbstr[w[i]].ofluxfact*grbstr[w[i]].olumfact*grbstr[w[i]].hostfact
     time=lc.time/(1.+grbstr[w[i]].z)
     mtime=min(abs(time-1d4),t)
     lum4[i]=lum[t]
     cd,'..'
  endfor 
  
  mx=max(grbstr[w].olum_11hr,wmx)
  mn=min(grbstr[w].olum_11hr,wmn)
  print,'Max: ',dir[wmx],lum4[wmx],grbstr[w[wmx]].olum_11hr
  print,'Min: ',dir[wmn],lum4[wmn],grbstr[w[wmn]].olum_11hr

  mx=max(lum4,wmx)
  mn=min(lum4,wmn)
  print,'Max: ',dir[wmx],lum4[wmx],grbstr[w[wmx]].olum_11hr
  print,'Min: ',dir[wmn],lum4[wmn],grbstr[w[wmn]].olum_11hr

  plotsym,0,1,/fill
  plot,[10,1d7],[1d40,1d50],/nodata,/xlog,/ylog
  cd,dir[wmx]
  lc=lcout2fits()
  lum=lc.src_rate*grbstr[w[wmx]].ofluxfact*grbstr[w[wmx]].olumfact*grbstr[w[wmx]].hostfact
  time=lc.time/(1.+grbstr[w[wmx]].z)
  oplot,time,lum,psym=8

  cd,'..'
  cd,dir[wmn]
  lc=lcout2fits()
  lum=lc.src_rate*grbstr[w[wmn]].ofluxfact*grbstr[w[wmn]].olumfact*grbstr[w[wmn]].hostfact
  time=lc.time/(1.+grbstr[w[wmn]].z)
  oplot,time,lum,psym=8
stop
end 

pro match_uvot_samples

  cd,'/Volumes/Apps_and_Docs/jracusin/Fermi/Swift_pop_study/FINAL'
  readcol,'../AVresults_tkII.dat',pgrb,av,averr1,averr0,gal,obeta,obetaerr1,obetaerr0,nh,nherr1,nherr0,model,format='(a,f,f,f,a,f,f,f,f,f,f,a)',/silent
  pgrb='GRB'+strtrim(pgrb,2)
  
  old_file=file_search('../*UVOT_LC/norm/GRB*uvot_lc.txt')
  nold=n_elements(old_file)
  oldgrb=strarr(nold)
  spos=strpos(old_file,'_uvot_lc')  
  for i=0,nold-1 do oldgrb[i]=strmid(old_file[i],20,spos[i]-20)

  file=file_search('FINAL_LC/*/GRB*norm_u.txt')
  spos=strpos(file,'_norm') 
  nnew=n_elements(file)
  newgrb=strarr(nnew)
  for i=0,nnew-1 do newgrb[i]=strmid(file[i],13,spos[i]-13)

  dont_match,oldgrb,newgrb,dm1,dm2
  print,'New is missing: ',oldgrb[dm1]
  if dm2[0] ne -1 then print,'New has added: ',newgrb[dm2]

  dont_match,oldgrb,pgrb,m1,m2
  print,'Pat is missing from old: ',oldgrb[m1]
  print,'Old is missing, but Pat includes (where are LCs?): ',pgrb[m2]
  
  dont_match,newgrb,pgrb,m1,m2
  print,'Pat is missing from new: ',newgrb[m1]
  print,'New is missing, but Pat includes (where are LCs?): ',pgrb[m2]


stop
  return
end 

pro calc_ke,z,beta,eiso,lx,xray=xray,opt=opt,ps=ps,eket=eke,m2=m2,day=day,eke1=eke1,eke2=eke2

  if not keyword_set(xray) and not keyword_set(opt) then begin
     print,'Choose xray or opt'
     return
  endif 
  grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  ks=mrdfits('~/Fermi/Swift_pop_study/kstest.fits',1)
;  c=where(grbstr.xflux_1day_obs ne 0)
;  ogrbstr=grbstr
;  grbstr=grbstr[c]


  z=grbstr.z
;  beta=grbstr.xbeta
  eiso=grbstr.eiso

;  goto,bing
  ;;;; Zhang et al. 2007 method
  ;;; two regimes nu<nu_c, nu>nu_c
  ;;; constants
  h=4.135e-15 ;; ev*s
  mpc2cm=3.08568025d24 
  
  ;;; assume
  eps_e=0.1
  eps_b=0.01
  eta=0.1
  y=0d ;;; IC is confusing, so let's ignore for now
  n=1.

  ;;; define
  td=11./24.  ;;; day
  if keyword_set(day) then td=1d ;; day
  if keyword_set(xray) then begin
     beta=grbstr.xbeta
     betaerr=grbstr.xbeta_err
     alpha=grbstr.xalpha3
     alphaerr=grbstr.xalpha3_err
;     flux=grbstr.xlum_1day_obs/grbstr.xlumfact*grbstr.xfluxfact
;     fluxerr=grbstr.xlum_1day_obs_err/grbstr.xlumfact*grbstr.xfluxfact
     flux=grbstr.xflux_11hr_obs
     fluxerr=grbstr.xflux_11hr_obs_err
     if keyword_set(day) then begin 
        flux=grbstr.xflux_1day_obs
        fluxerr=grbstr.xflux_1day_obs_err
     endif 
     nu=1000./h                 ;1d18 ;;; Hz - for now, will change to 1 keV, if works
     add='_x'
  endif 
  if keyword_set(opt) then begin
     beta=grbstr.obeta
     betaerr=grbstr.obeta_err
     alpha=grbstr.oalpha
     alphaerr=grbstr.oalpha_err
     ulam=3501d-8
     ulamang=ulam*1d8
     c=3d10
     nu=c/ulam
;     flux=grbstr.olum_11hr_obs/grbstr.olumfact;*grbstr.ofluxfact
;     fluxerr=grbstr.olum_11hr_obs_err/grbstr.olumfact;*grbstr.ofluxfact
     flux=grbstr.oflux_11hr_obs;*ulamang
     fluxerr=grbstr.oflux_11hr_obs_err;*ulamang
     if keyword_set(day) then begin 
        flux=grbstr.oflux_1day_obs     ;*ulamang
        fluxerr=grbstr.oflux_1day_obs_err ;*ulamang
     endif   
     add='_opt'
  endif 

  ;;; calculate
;  eke=(1-eta)/eta*eiso
  d=lumdist(z,h0=71,omega_m=0.27)*mpc2cm
  ;; if nu> nu_c
  p=2.*beta ;;; come back to this later to determine automatically from something??
  w=where(p le 2,nw)
  if nw gt 0 then p[w]=2.01
  w=where(p gt 3)
  p[w]=3.
  fp=6.73*((p-2)/(p-1.))^(p-1.)*(3.3e-6^((p-2.3)/2.))
  p1=p

  ;; convert to Bing units
  d28=d/1d28
  nu18=nu/1d18
;  eke52=eke/1d52
  eps_b2=eps_b/1d-2
  eps_e1=eps_e/1d-1

  eke1=(flux/5.2d-14)^(4./(p+2.))*d28^(8./(p+2.))*(1.+z)^(-1.)*td^((3.*p-2.)/(p+2.))*(1.+y)^(4./(p+2))*fp^((-4./(p+2)))*eps_b2^((2.-p)/(p+2.))*eps_e1^((4.*(1.-p)/(p+2)))*nu18^(2.*(p-2.)/(p+2.))
;  nufnu1=5.2e-14*d28^(-2.)*(1.+z)^((p+2.)/4.)*(1+y)^(-1)*fp*eps_b2^((p-2.)/4.)*eps_e1^(p-1.)*eke52^((p+2)/4.)*td^((2-3.*p)/4.)*nu18^((2.-p)/2.)

  ;; nu<nu_c
  p=2.*beta+1.
  w=where(p le 2,nw)
  if nw gt 0 then p[w]=2.01
  w=where(p gt 3)
  p[w]=3.
  p2=p

  fp=6.73*((p-2)/(p-1.))^(p-1.)*(3.3e-6^((p-2.3)/2.))
;  nufnu2=6.5d-13*d28^(-2.)*(1.+z)^((p+3)/4.)*fp*eps_b2^((p+1)/4.)*eps_e1^(p-1.)*eke52^((p+3)/4.)*n^0.5*td^((3.-3.*p)/4.)*nu18^((3.-p)/2.)
  eke2=(flux/6.5d-13)^(4./(p+3.))*d28^(8./(p+3.))*(1.+z)^(-1.)*td^(3.*(p-1.)/(p+3.))*fp^(-4./(p+3.))*eps_b2^(-(p+1.)/(p+3.))*eps_e1^(4.*(1.-p)/(p+3.))*n^(-2./(p+3.))*nu18^(2.*(p-3.)/(p+3.))

;  w=where(z eq 0 or eiso eq 0 or beta eq 0)
  w=where(z eq 0 or beta eq 0)
  eke1[w]=0
  eke2[w]=0
;  nufnu1[w]=0
;  nufnu2[w]=0
  if n_elements(grbstr) gt 0 then begin
;     readcol,'AVresults_tkII.dat',grb,av,averr1,averr0,dust,beta2,betaerr1,betaerr0,nh,nherr1,nherr0,model,format='(a,f,f,f,a,f,f,f,f,f,f,a)'
     w3=where(grbstr.xalpha3 gt 0 and grbstr.xbeta gt 0 and grbstr.hr gt 0)
;     match,'GRB'+grb,strtrim(grbstr[w3].grb,2),m1,m2
;     m2=w3[m2]
     m2=w3
     nm2=n_elements(m2)
;     s=where(model[m1] eq 'pow')
;     b=where(model[m1] eq 'bknp')     

     ;;; alpha=(3*beta-1)/2. ISM/WIND slow cooling nu>nu_c
     cr=intarr(n_elements(m2))
     x=grbstr[m2].xalpha3-(3.*grbstr[m2].xbeta-1.)/2.
     dcrdb=-3./2.
     errup=sqrt(grbstr[m2].xalpha3_err[1]^2.+grbstr[m2].xbeta_err[1]^2*dcrdb^2.)
     errlow=sqrt(grbstr[m2].xalpha3_err[0]^2.+grbstr[m2].xbeta_err[0]^2*dcrdb^2.)
     w0=where(x-2.*errlow lt 0. and x+2.*errup gt 0)
     cr[w0]=1
     plot,[0,6],[0,nm2+2],/nodata
     for i=0,nm2-1 do oplot,[x[i]+1-errlow[i],x[i]+1+errup[i]],[i+1,i+1],color=!red
     oplot,x[w0]+1,w0+1,psym=2

     ;;; alpha=3/2*beta ISM slow cooling nu<nu_c
     x=grbstr[m2].xalpha3-(3.*grbstr[m2].xbeta)/2.
     dcrdb=-3./2.
     errup=sqrt(grbstr[m2].xalpha3_err[1]^2.+grbstr[m2].xbeta_err[1]^2*dcrdb^2.)
     errlow=sqrt(grbstr[m2].xalpha3_err[0]^2.+grbstr[m2].xbeta_err[0]^2*dcrdb^2.)
     w1=where(x-2.*errlow lt 0. and x+2.*errup gt 0)
     for i=0,nm2-1 do oplot,[x[i]+3-errlow[i],x[i]+3+errup[i]],[i+1,i+1],color=!blue
     oplot,x[w1]+3,w1+1,psym=2
     cr[w1]=2
     
     ;;; alpha=(3*beta+1)/2. WIND slow cooling nu<nu_c
     x=grbstr[m2].xalpha3-(3.*grbstr[m2].xbeta+1.)/2.
     dcrdb=-3./2.
     errup=sqrt(grbstr[m2].xalpha3_err[1]^2.+grbstr[m2].xbeta_err[1]^2*dcrdb^2.)
     errlow=sqrt(grbstr[m2].xalpha3_err[0]^2.+grbstr[m2].xbeta_err[0]^2*dcrdb^2.)
     w2=where(x-2.*errlow lt 0. and x+2.*errup gt 0,nw2)
     for i=0,nm2-1 do oplot,[x[i]+5-errlow[i],x[i]+5+errup[i]],[i+1,i+1],color=!green
     if nw2 gt 0 then cr[w2]=3
     oplot,x[w2]+5,w2+1,psym=2
     oplot,[1,1],[0,nm2+2],line=2
     oplot,[3,3],[0,nm2+2],line=2
     oplot,[5,5],[0,nm2+2],line=2
     
     colprint,'consistent: '+ntostr(cr),grbstr[m2].grb,grbstr[m2].xalpha3,grbstr[m2].xalpha3_err[0],grbstr[m2].xbeta
     crgrb=grbstr[m2].grb
     b=where(cr eq 1,nb)
     s=where(cr eq 2 or cr eq 3,ns)
     grbstr[m2[b]].nuc=2 ;; nu<nu_c
     grbstr[m2[s]].nuc=1 ;; nu>nu_c

;     b=where(strtrim(grbstr[m2].dur,2) eq 'long')
;     s=where(strtrim(grbstr[m2].dur,2) eq 'short')

;     nufnu=dblarr(n_elements(m2))
;     nufnu[s]=nufnu2[m2[s]]
;     nufnu[b]=nufnu1[m2[b]]
;     p=nufnu
     eke=dblarr(n_elements(m2))
     eke[s]=eke2[m2[s]]
     eke[b]=eke1[m2[b]]
     w=where(eke ne 0 and eiso[m2] ne 0)
     eke=eke[w]
     ekerr=sqrt((fluxerr[m2[w]]/flux[m2[w]])^2.+(betaerr[0,m2[w]]/beta[m2[w]])^2)*eke
     ekgrb=grbstr[m2[w]].grb
     m2=m2[w]

     eta=eiso[m2]/(eiso[m2]+eke*1d52)*1d2
     etaerr=sqrt((grbstr[m2].xalpha3_err[0]/grbstr[m2].xalpha3)^2+(grbstr[m2].xbeta_err[0]/grbstr[m2].xbeta)^2+(grbstr[m2].hr_err/grbstr[m2].hr)^2)*eta
     grbstr[m2].eta=eta
     grbstr[m2].eta_err=etaerr
     grbstr[m2].eke=eke*1d52
     grbstr[m2].eke_err=ekerr*1d52

     mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',/create

;     p=eke
;     p[s]=p2[m2[s]]
;     p[b]=p1[m2[b]]

     flux=flux[m2]
     fluxerr=fluxerr[m2]
;     etaerr=fltarr(nm2)
;     nufnuerr=fltarr(nm2)
;     frange=[1d-15,1d-8]
     hrange=[0.2,3]
     range=[0.001,2]*100.
     !x.margin=[14,1]
     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/eke_efficiencies'+add+'.eps',/land,/encap,/color,font='helvetica' else erase

;     b=where(cr eq 1 and grbstr[m2].who eq 'BAT',nb)
;     s=where((cr eq 2 or cr eq 3) and grbstr[m2].who eq 'BAT',ns)
     ls=where(strtrim(grbstr[m2].dur,2) eq 'long' and grbstr[m2].who eq 'BAT',nb)
     ss=where(strtrim(grbstr[m2].dur,2) eq 'short' and grbstr[m2].who eq 'BAT',ns)
     ll=where(strtrim(grbstr[m2].dur,2) eq 'long' and grbstr[m2].who eq 'LAT',nb)
     sl=where(strtrim(grbstr[m2].dur,2) eq 'short' and grbstr[m2].who eq 'LAT',ns)
     lg=where(strtrim(grbstr[m2].dur,2) eq 'long' and grbstr[m2].who eq 'GBM',nb)
     sg=where(strtrim(grbstr[m2].dur,2) eq 'short' and grbstr[m2].who eq 'GBM',ns)

     plotsym,0,1,/fill
     hr=grbstr[m2].hr
     hrerr=grbstr[m2].hr_err

     scatter_hist,hr,eta,hrerr,etaerr,bin1=0.1,bin2=0.15,xrange=hrange,yrange=range,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle='SR',ytitle=!tsym.eta+'(%)',w0=ls,w1=ss,w2=lg,w3=sg,w4=ll,w5=sl,leg=['BAT','GBM','LAT'],pw0=[ls,ss],pw1=[lg,sg],pw2=[ll],pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/bottom,/right,com="xyouts,1.9,7,'GRB 090902B',/data,charsize=1.",charsize=1.

     if keyword_set(ps) then endplot

     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/eke_eiso_eta.eps',font='helvetica',/color,/land
     bcolor=!grey70
     gcolor=!grey50
;  gcolor=!blue
     lcolor=!grey20
;  lcolor=!red

     eiso=grbstr[m2].eiso
     eisoerr=sqrt((grbstr[m2].bat_fluence_err/grbstr[m2].bat_fluence)^2+0.1)*eiso
     x=1d52
     xrange=[1d48,1d56]
     yrange=[1d50,1d56]
     plot,xrange,yrange,/nodata,psym=3,/iso,xtitle='E!L'+!tsym.gamma+',iso!N (erg)',ytitle='E!Lk!N (erg)',/xlog,/ylog ;,xticks=8,yticks=6
     oploterror,eiso[ls],eke[ls]*x,eisoerr[ls],ekerr[ls]*x,color=bcolor,psym=8,/nohat,errcolor=bcolor
     oploterror,eiso[ss],eke[ss]*x,eisoerr[ss],ekerr[ss]*x,color=bcolor,psym=2,/nohat,errcolor=bcolor
     oploterror,eiso[lg],eke[lg]*x,eisoerr[lg],ekerr[lg]*x,color=gcolor,psym=8,/nohat,errcolor=gcolor
     oploterror,eiso[sg],eke[sg]*x,eisoerr[sg],ekerr[sg]*x,color=gcolor,psym=2,/nohat,errcolor=gcolor
     oploterror,eiso[ll],eke[ll]*x,eisoerr[ll],ekerr[ll]*x,color=lcolor,psym=8,/nohat,errcolor=lcolor
;     oploterror,eiso[sl],eke[sl]*x,eisoerr[sl],ekerr[sl]*x,color=lcolor,psym=2,/nohat,errcolor=lcolor
     etas=[0.01,0.05,0.2,0.5,0.75,0.9,0.99]
     setas=['0.01','0.05','0.2','0.5','0.75','0.9','0.99']
     xx=(1.-etas)/etas
     x=10^(dindgen(5)*2.+48d)
     for i=0,n_elements(etas)-1 do begin 
        oplot,x,x*xx[i],line=1
        xyouts,2d50/xx[i],2d50,'  '+setas[i],/data,charsize=1.2
     endfor 
     xyouts,1.2d48,2d50,!tsym.eta+'=',/data,charsize=1.2

      legend,['BAT','GBM','LAT'],box=0,color=[bcolor,gcolor,lcolor],textcolor=[bcolor,gcolor,lcolor],/top,/left
     legend,['Long','Short'],box=0,/bottom,/right,psym=[8,2]
     if keyword_set(ps) then endplot

     ;;;; Why missing so many GRBs?
     w=where(grbstr.eiso ne 0,nw)
     s=sort(grbstr[w].eiso)
     s=w[s]
;     w3=where(grbstr.xalpha3 gt 0 and grbstr.xbeta gt 0 and grbstr.hr
;     gt 0)
     match,grbstr[s].grb,crgrb,m1,m2
     crs=intarr(nw)
     crs[m1]=cr[m2]
     crgrbs=strarr(nw)
     crgrbs[m1]=crgrb[m2]
     match,grbstr[s].grb,ekgrb,m1,m2
     eket=dblarr(nw)
     eket[m1]=eke[m2]
     colprint,grbstr[s].grb,grbstr[s].eiso,eket,eke1[s],eke2[s],grbstr[s].xalpha3,grbstr[s].xbeta,grbstr[s].hr,crs,grbstr[s].xflux_11hr_obs
     stop




     kstwop,grbstr[m2[ls]].hr,grbstr[m2[lg]].hr,d,gprob
     kstwop,grbstr[m2[ls]].hr,grbstr[m2[ll]].hr,d,lprob
     kstwop,grbstr[m2[lg]].hr,grbstr[m2[ll]].hr,d,glprob
     ks[0].hr_bg=gprob
     ks[0].hr_bl=lprob
     ks[0].hr_gl=glprob
;     kstwop,grbstr[m2[ss]].hr,grbstr[m2[sg]].hr,d,gprob
;     kstwop,grbstr[m2[ss]].hr,grbstr[m2[sl]].hr,d,lprob
;     kstwop,grbstr[m2[sg]].hr,grbstr[m2[sl]].hr,d,glprob
;     ks[1].hr_bg=gprob
;     ks[1].hr_bl=lprob
;     ks[1].hr_gl=glprob

     kstwop,eta[ls],eta[lg],d,gprob
     kstwop,eta[ls],eta[ll],d,lprob
     kstwop,eta[lg],eta[ll],d,glprob
     ks[0].eta_bg=gprob
     ks[0].eta_bl=lprob
     ks[0].eta_gl=glprob
;     kstwop,eta[ss],eta[sg],d,gprob
;     kstwop,eta[ss],eta[sl],d,lprob
;     kstwop,eta[sg],eta[sl],d,glprob
;     ks[1].eta_bg=gprob
;     ks[1].eta_bl=lprob
;     ks[1].eta_gl=glprob
     mwrfits,ks,'~/Fermi/Swift_pop_study/kstest.fits',/create

;     goto,skiptable
     ;;; latex table
     pars=['$\alpha_x$','$\alpha_o$','$\beta_x$','$\beta_o$','$z$','$A_V$','$N_H$','$N_H/A_V (\times 10^{21})$','$L_{x,11 hr}$\tablenotemark{*}','$L_{x,1 day}$\tablenotemark{*}','$L_{o,11 hr}$\tablenotemark{*}','$L_{o,1 day}$\tablenotemark{*}','$E_{\gamma,iso}$','$\theta_j$','$E_{\gamma}$','$HR$','$\eta$']
     samp=['Long','Short']
     for i=0,0 do begin  ;;; not enough stats for short bursts only long
        print,'\multicolumn{4}{c}{'+samp[i]+' Bursts} \\'
        for j=0,15 do begin 
           case j of 
              0: p=sigfig([ks[i].xalpha_bg,ks[i].xalpha_bl,ks[i].xalpha_gl],2)
              1: p=sigfig([ks[i].oalpha_bg,ks[i].oalpha_bl,ks[i].oalpha_gl],2)
              2: p=sigfig([ks[i].xbeta_bg,ks[i].xbeta_bl,ks[i].xbeta_gl],2)
              3: p=sigfig([ks[i].obeta_bg,ks[i].obeta_bl,ks[i].obeta_gl],2)
              4: p=sigfig([ks[i].z_bg,ks[i].z_bl,ks[i].z_gl],2)
              5: p=sigfig([ks[i].av_bg,ks[i].av_bl,ks[i].av_gl],2)
              6: p=sigfig([ks[i].nh_bg,ks[i].nh_bl,ks[i].nh_gl],2)
              7: p=sigfig([ks[i].nhav_bg,ks[i].nhav_bl,ks[i].nhav_gl],2)
              8: p=sigfig([ks[i].xlum_11hr_bg,ks[i].xlum_11hr_bl,ks[i].xlum_11hr_gl],2)
              9: p=sigfig([ks[i].xlum_1day_bg,ks[i].xlum_1day_bl,ks[i].xlum_1day_gl],2)
              10: p=sigfig([ks[i].olum_11hr_bg,ks[i].olum_11hr_bl,ks[i].olum_11hr_gl],2)
              11: p=sigfig([ks[i].olum_1day_bg,ks[i].olum_1day_bl,ks[i].olum_1day_gl],2)
              12: p=sigfig([ks[i].eiso_bg,ks[i].eiso_bl,ks[i].eiso_gl],2)
              13: p=[sigfig(ks[i].theta_bg,2),' -- ',' -- '] ;ks[i].theta_bl,ks[i].theta_gl],2)
              14: p=[sigfig(ks[i].egam_bg,2),' -- ',' -- ']      ;ks[i].egam_bl,ks[i].egam_gl],2)
              15: p=sigfig([ks[i].eta_bg,ks[i].eta_bl,ks[i].eta_gl],2)
           endcase 
           if j le 11 or j ge 15 then wp=where((p*1.) le 0.1,nwp) else wp=where((p[0]*1.) le 0.1,nwp)
           if nwp eq 0 then pp=p else begin
              pp=p
              x=round(alog10(p[wp]*1.)-0.5)
              pp[wp]='$'+numdec(p[wp]/(10.^x),1)+'\times 10^{'+ntostr(fix(x))+'}$'
           endelse
           colprint,pars[j]+' & '+pp[0]+' & '+pp[1]+' & '+pp[2]+' \\'

        endfor 
     endfor 
;     skiptable:
;     stop
;;      ploterror,hr[s],eta[s],hrerr[s],etaerr[s],/nohat,/xlog,/ylog,psym=2,xrange=hrange,yrange=range,xtitle='HR',ytitle=!tsym.eta+'(%)',/xsty,/ysty
;; ;     ploterror,flux[s],nufnu[s],fluxerr[s],nufnuerr[s],/nohat,/xlog,/ylog,psym=1,/iso,xrange=range,yrange=range,xtitle='F!Lx,obs!N',ytitle='F!Lx,predict!N',/xsty,/ysty
;; ;     oploterror,flux[b],nufnu[b],fluxerr[b],nufnuerr[b],/nohat,psym=2
;;      oploterror,hr[b],eta[b],hrerr[b],etaerr[b],/nohat,psym=8
;; ;     ls=where(grbstr[m2].who eq 'LAT' and (cr eq 2 or cr eq 3));model[m1] eq 'pow')
;; ;     lb=where(grbstr[m2].who eq 'LAT' and cr eq 1);model[m1] eq 'bknp')
;;      lb=where(strtrim(grbstr[m2].dur,2) eq 'long' and grbstr[m2].who eq 'LAT',nb)
;;      ls=where(strtrim(grbstr[m2].dur,2) eq 'short' and grbstr[m2].who eq 'LAT',ns)

;; ;     oploterror,flux[ls],nufnu[ls],fluxerr[ls],nufnuerr[ls],/nohat,psym=1,color=!red,errcolor=!red
;; ;     oploterror,flux[lb],nufnu[lb],fluxerr[lb],nufnuerr[lb],/nohat,psym=2,color=!red,errcolor=!red
;;      if ns gt 0 then oploterror,hr[ls],eta[ls],hrerr[ls],etaerr[ls],/nohat,psym=2,color=!red,errcolor=!red
;;      oploterror,hr[lb],eta[lb],hrerr[lb],etaerr[lb],/nohat,psym=8,color=!red,errcolor=!red


;; ;     gs=where(grbstr[m2].who eq 'GBM' and (cr eq 2 or cr eq 3));model[m1] eq 'pow')
;; ;     gb=where(grbstr[m2].who eq 'GBM' and cr eq 1);model[m1] eq 'bknp')
;;      gb=where(strtrim(grbstr[m2].dur,2) eq 'long' and grbstr[m2].who eq 'GBM',nb)
;;      gs=where(strtrim(grbstr[m2].dur,2) eq 'short' and grbstr[m2].who eq 'GBM',ns)

;; ;     oploterror,flux[gs],nufnu[gs],fluxerr[gb],nufnuerr[gb],/nohat,psym=1,color=!blue,errcolor=!blue
;; ;     oploterror,flux[gb],nufnu[gb],fluxerr[gb],nufnuerr[gb],/nohat,psym=2,color=!blue,errcolor=!blue
;;      oploterror,hr[gs],eta[gs],hrerr[gs],etaerr[gs],/nohat,psym=2,color=!blue,errcolor=!blue
;;      oploterror,hr[gb],eta[gb],hrerr[gb],etaerr[gb],/nohat,psym=8,color=!blue,errcolor=!blue

;; ;     w3=where(grbstr[m2].xalpha3 gt 0)
;; ;     oplot,flux[w3],nufnu[w3],psym=5,color=!green

;;      ;;; need to test nu><nu_c using CRs
;;      ;;; need to check which segment 11 hrs or 1 days is at
;; ;     oplot,frange,range
;;      pnu=!tsym.nu
;; ;     legend,[pnu+'>'+pnu+'!Lc!N',pnu+'<'+pnu+'!Lc!N'],box=0,psym=[1,2],/top,/left
;;      legend,['long','short'],box=0,psym=[8,2],/top,/left
;;      legend,['BAT','GBM','LAT'],box=0,color=[!p.color,!blue,!red],textcolor=[!p.color,!blue,!red],/bottom,/right
     

  endif 
  

;  print,nufnu
;  fnu=nufnu;/nu
;  lx=fnu
;  fe=nufnu/(nu18*h^2)
;  bing:

  goto,max
  ;;; from Max's paper nu>nu_c
  h=4.135e-15 ;; ev*s
  mpc2cm=3.08568025d24 

  c1=1.4d-21 ;; cm^-3/2
  c2=6.1d-5  ;; s^3/2 g^-1/2 cm^-1
  c3=6.9d39  ;; s^-3/2 g^-1/2 cm^-2
  eps_b=0.01 ;*1d2 ;;; in units of 10^-2
  d=3d28
;  d=lumdist(z,h0=71,omega_m=0.27)*mpc2cm
;  nu=2.4d17 ;;; change to kev/h
  nu=1d18
  day=86400d
;  t=40d3;/day ;;; change to t/day
  t=day
  eps_e=0.2
  eta=0.1
  fluxjy=0.3d-6     ;; jy ;;; change
  flux=fluxjy*1d-23 ;; erg/(s cm2 Hz)
  fd=flux           ;flux2jy(flux,beta+1.)
  

  ;;; have p from beta, D_L from z, flux density from flux & beta, t (days), nu
  ;;; assume eps_b
  p=2*beta
  d1=(c2*c3)^(-0.5)*c1^(-1.)/(4*!pi)
  eps=(p-2)/(p+2)
  y=c1*c3^0.5*c2^(-1.5)*eps_b^(-1.)*d^(-2.)*nu*t^2*fd^(-1)*eps_e^(-3.)
  d2=y^eps
  fwe=d1*d^2/(1+z)*nu*t*fd*d2
  print,d2
  print,fwe

  c=d1*d2
  eke=eiso*(1.-eta)/eta
  pp=(-4.*p+4)/(p+2.)
  lx=eke/(c*eps_e^pp*nu*t)

  print,c,' should be 6.04e-16'
  max:

;;; ugh, not even close, maybe go back to zhang method
;;; zhang provides clear units, and both nu>nu_c, and nu<nu_c, but
;;; also depends on f_nu

  goto,fandw
  ;;; Freedman & Waxman
  ;;; Estimate Lx(t) as function of Eiso, p, nu, eps_e, eps_b
  ;;; but assume eps_e, eps_b (aka universal)
  ;;; however Eiso converts to Eke via eta
  ;;; Tests whether eta is universal between BAT/GBM/LAT

  ;;; assumptions
  eps_e=0.1
  eps_b=0.01
  eta=0.1  ;; from Zhang et al. for BAT sample

  ;;; constants
  h=4.135e-15 ;; ev*s
  mpc2cm=3.08568025d24 
  day=86400d
  
  ;;; set parameters
  nu=1000./h ;;; 1 keV
  p=2.*beta  ;;; for nu>nu_c
  t=1.*day   ;;; in observed frame, not rest frame

  ;;; from F & W (2001)
  c1=1.4d-21 ;; cm^-3/2
  c2=6.1d-5  ;; s^3/2 g^-1/2 cm^-1
  c3=6.9d39  ;; s^-3/2 g^-1/2 cm^-2

  ;;; let's calculate
  eke=eiso*(1-eta)/eta   ;;; convert gamma-ray energy to KE
  eps=(p-2)/(p+2)
  d=lumdist(z,h0=71,omega_m=0.27)*mpc2cm  
;  fd=flux2jy(flux,beta+1)
  y=c1*c3^0.5*c2^(-1.5)*eps_b^(-1.)*nu*t^2*eps_e^(-3.)*(4*!pi/(1+z)) ;*d^(-2.);*fd^(-1)
;  y=c1*c3^0.5*c2^(-1.5)*eps_b^(-1.)*eps_e^(-3.)*d^(-2.)*nu*t^2
  ye=y^eps

;  fnu=(eps_e*eke*(c2*c3)^0.5*c1*nu^(-1.)*t^(-1.)*d^(-2.)*(1.+z)*ye)^(1./(1+eps))
  Lx=(4.*!pi*eps_e*eke*(c2*c3)^0.5*c1*ye*nu^(-1.)*t^(-1.))^(1./(1-eps)) ;;; should be predicted Lx (lum dens at 1 keV) at 1 day
  ;;; do k-corr? but using observed frequency

  lx=lx*nu
;  print,fnu ;; erg/(cm2 s Hz)
;  print,fnu*1d23 ;; mJy
;  print,lx
;  print,lx*nu

;w=where(grbstr.xbeta ne 0 and grbstr.eiso ne 0)     
; calc_ke,grbstr.z,grbstr.xbeta,grbstr.eiso,lx     
;plot,grbstr[w].xlum_1day_obs,lx[w],psym=1,/xlog,/ylog,xtitle='L!Lx,obs!N',ytitle='L!Lx,predict!N'
; oplot,[1d20,1d100],[1d20,1d100]       
; l=where(grbstr.who eq 'LAT')          
; oplot,grbstr[l].xlum_1day_obs,lx[l],psym=1,color=!red
  fandw:
;  endplot
  stop
  return
end

pro collect_for_pat

;;  z, Egal (B-V), ra, dec, nH, phInd, x-ray temporal fits, optical temporal fits
  
  g=create_struct('grb','','whose','','z',0.,'ebv',0.,'ra',0d,'dec',0d,'nhgal',0d,'nh',0d,$
                  'phind',0d,'xnorm',0d,'xpow1',0d,'xtbreak1',0d,'xpow2',0d,$
                  'xtbreak2',0d,'xpow3',0d,'xtbreak3',0d,'xpow4',0d) ;,$
;                  'onorm',0d,'opow1',0d,'otbreak1',0d,'opow2',0d,'otbreak2',0d,$
;                  'opow3',0d)

  cd,'/Volumes/Apps_and_Docs/jracusin/Fermi/Swift_pop_study/'
  ogrbs=file_search('*UVOT_LC/norm/GRB*txt')
  nogrb=n_elements(ogrbs)
  gg=replicate(g,nogrb)
  for i=0,nogrb-1 do begin
     spos0=strpos(ogrbs[i],'GRB')
     spos1=strpos(ogrbs[i],'_uvot_lc.txt')
     ogrb=strmid(ogrbs[i],spos0,spos1-spos0)
     gg[i].grb=ogrb
  endfor 

  readcol,'sample_stats.csv',zgrb,who,z,which,how,format='(a,a,f,a,a)',delim=',',/silent

  match,gg.grb,zgrb,m1,m2
  help,gg.grb,zgrb,m1,m2  ;;; make sure all are matched
  gg[m1].whose=who[m2]
  gg[m1].z=z[m2]*1.
  
  ;;; E (B-V) galactic
  readcol,'UVOT_GRBs.dat',uggrbs,tid,ra,dec,format='(a,a,d,d)',skip=1,delim='|',/silent
  ngrbs=n_elements(uggrbs)
  grbs=strarr(ngrbs)
  for i=0,ngrbs-1 do begin
     g=str_sep(uggrbs[i],' ')
     grbs[i]=g[1]
  endfor 
  w=where(ra ne 0)
  ra=ra[w]
  dec=dec[w]
  grbs=grbs[w]
  filt='u'
  for i=0,nogrb-1 do begin
     g=where(strtrim(gg[i].grb,2) eq 'GRB'+grbs)
     calc_dust_corr,ra[g[0]],dec[g[0]],corr,/noplot,filter=filter,dir='ned/',ebv=ebv
     gg[i].ra=ra[g[0]]
     gg[i].dec=dec[g[0]]
     f=where(filt eq filter)
     gg[i].ebv=ebv              ;corr[f];;; no, need E(B-V)
  endfor 
  ;;; X-ray spectral fits
  ;;; X-ray temporal fits
;  dir=!adata
  dir=!grbs

  ;;; LC fits - need norm, pow, breaks
  for i=0,nogrb-1 do begin
     ;; x
     gdir=dir+'/'+gg[i].grb
     lcfitfile=gdir+'/lc_fit_out_idl_int8.dat'
     if not exist(lcfitfile) then lcfitfile=gdir+'/lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pname,p,perr,np=np

        gg[i].xnorm=p[0]
        gg[i].xpow1=p[1]
        if np gt 2 then begin 
           gg[i].xtbreak1=p[2]
           gg[i].xpow2=p[3]
           if np gt 4 then begin 
              gg[i].xtbreak2=p[4]
              gg[i].xpow3=p[5]
              if np gt 6 then begin
                 gg[i].xtbreak3=p[6]
                 gg[i].xpow4=p[7]
              endif 
           endif 
        endif 
        spec=mrdfits(gdir+'/UL_specfits.fits',1)
        ns=n_elements(spec)-1
        gg[i].nhgal=spec[ns].nhgal*1d22
        gg[i].nh=spec[ns].nh
        gg[i].phind=spec[ns].phind
        ;; o
;     cwho=strupcase(gg[i].whose)+'_UVOT_LC'
;     gdir='~/Fermi/Swift_pop_study/'+cwho+'/'+gg[i].grb
;     ffile=gdir+'/lc_fit_out_idl_int7.dat'
;     if exist(ffile) then begin 
;        read_lcfit,gdir+'/lc_fit_out_idl_int7.dat',pnames,p,perr
;        np=n_elements(p)
;        gg[i].onorm=p[0]
;        gg[i].opow1=p[1]
;        if np gt 2 then begin 
;           gg[i].otbreak1=p[2]
;           gg[i].opow2=p[3]
;           if np gt 4 then begin 
;              gg[i].otbreak2=p[4]
;              gg[i].opow3=p[5]
;           endif 
;        endif 
;     endif 
     endif  
  endfor 
  ;;; does PAT really need ra/dec if we send LCs?
  ;;; - if so get UVOT positions from SAM?

  mwrfits,gg,'UVOT_sample_grb_info.fits',/create
  stop

  return
end

pro convert_plots
  
  dir='~/Fermi/Swift_pop_study/'
  psfiles=dir+['xalpha_lum_corr.','oalpha_lum_corr.','xray_lc_compare_lum_both.','uvot_lc_compare_lum_both.','xray_lc_compare_lum_long.','uvot_lc_compare_lum_long.','xray_lc_compare_lum_short.','uvot_lc_compare_lum_short.','xlum_spec.','xbetas.','xalphas.','uvot_lum_spec.','uvot_alphas.','eiso_plots.','energetics_long.','energetics_short.','redshift_dist.','xlum_eiso.']
  for i=0,n_elements(psfiles)-1 do spawn,'convert -rotate 270 '+psfiles[i]+'eps '+psfiles[i]+'png'
end

pro which_alpha,ts,p,aspec,x,np=np

;  np=n_elements(p)-nflares*3
  if np mod 2 eq 1 then np=np-1
  case np of
     2: n=1
     4: n=[1,3]
     6: n=[1,3,5]
     8: n=[1,3,5,7]
     10: n=[1,3,5,7,9]
  endcase
  alphas=p[n]
  if np gt 2 then btime=p[n[1:np/2-1]-1] else btime=0

  case np of
     2: aspec=alphas[0]
     4: begin
        if ts lt btime[0] then aspec=alphas[0]
        if ts gt btime[0] then aspec=alphas[1]
     end 
     6: begin
        if ts lt btime[0] then aspec=alphas[0]
        if ts gt btime[0] and ts lt btime[1] then aspec=alphas[1]
        if ts gt btime[1] then aspec=alphas[2]
     end 
     8: begin
        if ts lt btime[0] then aspec=alphas[0]
        if ts gt btime[0] and ts lt btime[1] then aspec=alphas[1]
        if ts gt btime[1] and ts lt btime[2] then aspec=alphas[2]
        if ts gt btime[2] then aspec=alphas[3]
     end 
     10: begin
        if ts lt btime[0] then aspec=alphas[0]
        if ts gt btime[0] and ts lt btime[1] then aspec=alphas[1]
        if ts gt btime[1] and ts lt btime[2] then aspec=alphas[2]
        if ts gt btime[2] and ts lt btime[3] then aspec=alphas[3]
        if ts gt btime[3] then aspec=alphas[4]
     end 
  endcase 
  x=where(p eq aspec)
  return
end 

pro kcorr_demo

  mpc2cm=3.08568025d24 
  z=dindgen(20)/2.
  dist=lumdist(z)*mpc2cm
  t=(findgen(100)+1d)*100d
  flux=pow(t,[1d-11,1.]) 
  alpha=1d
  beta=1d
  plot,[1d2,1d4],[1d40,1d50],/nodata,/xlog,/ylog,xtitle='Time',ytitle='Lum'
  for i=0,19 do begin 
     lum=flux*4.*!pi*dist[i]^2*(1.+z[i])^(beta-alpha-1.) 
     oplot,t/(1.+z[i]),lum
  endfor
stop 
return
end 

pro plot_lat_lcs_too,lum=lum,ps=ps

 latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB100728A','GRB110625A','GRB110731A']  
 dir='~/Fermi/Swift_pop_study/LAT/'
 if keyword_set(ps) then begin
    if keyword_set(lum) then add='lum' else add='flux'
    begplot,name='~/Fermi/Swift_pop_study/lat_lc_'+add+'.eps',/encap,/color,/land,font='helvetica'
    charsize=2
 endif else charsize=1.
 colors=[!red,!blue,!green,!forestgreen,!cyan,!magenta,!purple,!orange]

 readcol,'~/Fermi/Swift_pop_study/LAT_GRB_z.dat',zgrbs,z,format='(a,f)'
 mpc2cm=3.08568025d24 
 dist=lumdist(z,h0=71,omega_m=0.27)*mpc2cm
 tf=1./(1.+z)

 if not keyword_set(lum) then lum=0
 if not lum then plot,[1,1e5],[1d-7,1d-2],/xlog,/ylog,xtitle='Time (s) / (1+z)',ytitle='Flux (ph cm!U-2!N s!U-1!N)',/nodata,charsize=charsize
 if lum then plot,[1,1e5],[1d43,1d50],/xlog,/ylog,xtitle='Time (s) / (1+z)',ytitle='Estimated Luminosity (erg)',/nodata,charsize=charsize
 plotsym,1,4,thick=5
 for i=0,n_elements(latgrbs)-1 do begin
    file=dir+latgrbs[i]+'/'+latgrbs[i]+'.dat'
    print,file
    if exist(file) then begin 
       readcol,file,interval,tstart,tstop,ts,flux,low_flux_err,high_flux_err,phind,low_phind,high_phind,/silent
       time=(tstop-tstart)/2.+tstart
       w=where(flux+low_flux_err le 0,nw)
       if nw gt 0 then low_flux_err[w]=0;-flux[w]*0.999d
       wdet=where(low_flux_err ne 0,nwdet)
       wndet=where(low_flux_err eq 0,nwndet)
       w=where(high_flux_err eq 0,nw)
       if nw gt 0 then high_flux_err[w]=-1.
       beta=mean(abs(phind[wdet]))-1.
       w0=where(phind[wdet] eq 0,nw0)
       if nw0 gt 0 then beta[w0]=1.

       gdir=dir+latgrbs[i]
       if not exist(gdir) then spawn,'mkdir '+gdir
       lcfile=gdir+'/lc_newout.txt'
       if not exist(lcfile) then begin 
          lc=lcout2fits(/empty)
          lc=replicate(lc,n_elements(time))
          lc.time=time
          lc.src_rate=flux
          lc.src_rate_err=high_flux_err
          lc.tstart=tstart
          lc.tstop=tstop
          write_lc,lc,lcfile
       endif 

     lcfitfile=dir+latgrbs[i]+'/lc_fit_out_idl_int8.dat'
     if not exist(lcfitfile) then lcfitfile=dir+latgrbs[i]+'/lc_fit_out_idl_int7.dat'
     if exist(lcfitfile) then begin 
        read_lcfit,lcfitfile,pname,p,perr,np=np
;       ofile=dir+latgrbs[i]+'/'+'lc_fit_out_idl_int7.dat'
;       if exist(ofile) then begin 
;          read_lcfit,ofile,pname,p,perr
;          np=n_elements(p)
          if np eq 2 then x=1
          if np eq 4 then x=3
          if np eq 6 then begin
             if p[1] gt p[3] then x=5 else x=3
          endif 
          if np eq 8 then x=5
          alpha=p[x]
          alpha_err=perr[*,x]
       endif else stop

       print,alpha,beta
       erg=2d-7
       lums=flux*erg*4.*!pi*dist[i]^2*(1.+z)^(beta-alpha-1.)
       lum0=(flux+low_flux_err)*erg*4.*!pi*dist[i]^2*(1.+z)^(beta-alpha-1.)
       lum1=(flux+high_flux_err)*erg*4.*!pi*dist[i]^2*(1.+z)^(beta-alpha-1.)

       for j=0,n_elements(time)-1 do begin
          if low_flux_err[j] ne 0 then begin 
             if lum then oplot,[time[j],time[j]]*tf[i],[lum0[j],lum1[j]],color=colors[i]
             if not lum then oplot,[time[j],time[j]]*tf[i],[flux[j]+low_flux_err[j],flux[j]+high_flux_err[j]],color=colors[i]
          endif 
          if lum then oplot,[tstart[j],tstop[j]]*tf[i],[lums[j],lums[j]],color=colors[i]
          if not lum then oplot,[tstart[j],tstop[j]]*tf[i],[flux[j],flux[j]],color=colors[i]          
       endfor 
       if nwndet gt 0 and lum then plots,time[wndet]*tf[i],lums[wndet],psym=8,color=colors[i]
       if nwndet gt 0 and not lum then plots,time[wndet]*tf[i],flux[wndet],psym=8,color=colors[i]
       
    endif 
 endfor 

  legend,['LAT '+latgrbs],textcolor=[colors],box=0,/top,/right
  if keyword_set(ps) then endplot
stop
return
end

pro swift_lat_pop_studies,flux=flux,lum=lum,ps=ps,uvot=uvot,lattoo=lattoo,energ=energ,fdens=fdens,skipuvot=skipuvot,justplot=justplot,mag=mag

  !p.charsize=2.
  xmarg=!x.margin
;  lum=1
  latgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090510','GRB090902B','GRB090926A','GRB091003','GRB100414A','GRB100728A','GRB110625A','GRB110731A'] 
;  olatgrbs=['GRB080916C','GRB090323','GRB090328A','GRB090902B','GRB090926A','GRB091003'] 
  olat=[0,1,2,4,5]
  colors=[!red,!blue,!green,!forestgreen,!cyan,!magenta,!purple,!orange]
  bcolor=!grey50
  gcolor=!grey70
;  gcolor=!blue
  lcolor=!grey20
;  lcolor=!red
;  colors=replicate(!p.color,8)
  mpc2cm=3.08568025d24 

;  readcol,'~/jetbreaks/grb_tid_z_table.csv',zgrbs,tids,zs,format='(a,l,f)',/silent
;  readcol,'~/Fermi/Swift_pop_study/Swift_other_obs.dat',tgrb,tid,ots1,ots2,ots3,ots4,format='(a,a,a,a,a,a)',delim='|',skip=1
;  gbm=where(strtrim(ots1,2) eq 'Fermi-GBM' or strtrim(ots2,2) eq 'Fermi-GBM' or strtrim(ots3,2) eq 'Fermi-GBM' or strtrim(ots4,2) eq 'Fermi-GBM')
;  tgrb=tgrb[gbm]
;  tgrb=strcompress(tgrb,/remove)
  if keyword_set(lum) then $
     readcol,'~/Fermi/Swift_pop_study/sample_stats.csv',ssgrbs,whose,zs,dur,t90,format='(a,a,f,a,a)',/silent else begin
     cd,'~/GRBs/'
     ssgrbs=strtrim(file_search('GRB*'),2)
     ssgrbs=[ssgrbs,latgrbs]
     ssgrbs=ssgrbs[rem_dup(ssgrbs)]
     ns=n_elements(ssgrbs)
     whose=replicate('BAT',ns)
     readcol,'~/Fermi/Swift_pop_study/Swift_other_obs.dat',tgrb,tid,ots1,ots2,ots3,ots4,format='(a,a,a,a,a,a)',delim='|',skip=1
     gbm=where(strtrim(ots1,2) eq 'Fermi-GBM' or strtrim(ots2,2) eq 'Fermi-GBM' or strtrim(ots3,2) eq 'Fermi-GBM' or strtrim(ots4,2) eq 'Fermi-GBM')
     tgrb=tgrb[gbm]
     tgrb=strcompress(tgrb,/remove)
     match,ssgrbs,tgrb,m1,m2
     whose[m1]='GBM'
     zs=fltarr(ns)
;     dur=replicate('long',ns) ;;; fix with Nora's table
     readcol,'~/Fermi/Swift_pop_study/noras_shb_list.dat',shb,format='(a)'
     match,ssgrbs,'GRB'+shb,m1,m2
     dur=replicate('long',ns)
     dur[m1]='short'
     match,ssgrbs,latgrbs,m1,m2
     whose[m1]='LAT'
  endelse 
  
;;; NEED THIS TO WORK FOR NON-Z TOO?
  grbstr=create_struct('grb','','who','','z',0.,'dur','','dist',0d,$
                       'xalpha',0d,'xalpha_err',dblarr(2),$
                       'xalpha3',0d,'xalpha3_err',dblarr(2),$
                       'xbeta',0d,'xbeta_err',dblarr(2),'nh',0d,'nh_err',dblarr(2),$
                       'nhs',0d,'nhs_err',dblarr(2),$
                       'xtbreak',0d,'xbreakflux',0d,'xjb',0,'xtype','',$
                       'xfluxfact',0d,'xfdfact',0d,'xlumfact',0d,'xlumfact_err',0d, $
                       'xlum_11hr',0d,'xlum_11hr_err',0d,$
                       'xlum_11hr_obs',0d,'xlum_11hr_obs_err',0d,$
                       'xlum_1day',0d,'xlum_1day_err',0d,$
                       'xlum_1day_obs',0d,'xlum_1day_obs_err',0d,$
                       'xflux_11hr_obs',0d,'xflux_11hr_obs_err',0d,$
                       'xflux_1day_obs',0d,'xflux_1day_obs_err',0d,$
                       'oalpha',0d,'oalpha_err',dblarr(2),'obeta',0d,'obeta_err',dblarr(2),$
                       'otbreak',0d,'ojb',0,'otype','', $
                       'ofluxfact',0d,'ofdfact',0d,'olumfact',0d,'olumfact_err',0d, $
                       'olum_11hr',0d,'olum_11hr_err',0d,$
                       'olum_11hr_obs',0d,'olum_11hr_obs_err',0d,$
                       'olum_1day',0d,'olum_1day_err',0d,$
                       'olum_1day_obs',0d,'olum_1day_obs_err',0d,$
                       'oflux_1day_obs',0d,'oflux_1day_obs_err',0d,$
                       'oflux_11hr_obs',0d,'oflux_11hr_obs_err',0d,$
                       'av',0.,'av_err',dblarr(2),'gal','','alam',0.,$
                       'hostfact',0d,'ra',0d,'dec',0d,$
                       'eiso',0d,'epeak',0d,'thetaj',0d,'egam',0d,'bat_fluence',0d,$
                       'bat_fluence_err',0d,'hr',0d,'hr_err',0d,'nuc',0,$
                       'eta',0d,'eta_err',0d,'eke',0d,'eke_err',0d)
  grbstr=replicate(grbstr,n_elements(ssgrbs))

  grbstr.grb=ssgrbs
  grbstr.who=whose
;  grand=where(grbstr.grb eq 'GRB090618')
;  grbstr[grand].who='LAT'
  grbstr.z=zs
  grbstr.dur=dur

  gbm=where(whose eq 'GBM')
  tgrb=ssgrbs[gbm]

  ks=create_struct('xalpha_bg',0d,'xalpha_bl',0d,'xalpha_gl',0d,$
                   'xbeta_bg',0d,'xbeta_bl',0d,'xbeta_gl',0d,$
                   'nh_bg',0d,'nh_bl',0d,'nh_gl',0d,$
                   'oalpha_bg',0d,'oalpha_bl',0d,'oalpha_gl',0d,$
                   'obeta_bg',0d,'obeta_bl',0d,'obeta_gl',0d,$
                   'av_bg',0d,'av_bl',0d,'av_gl',0d,$
                   'nhav_bg',0d,'nhav_bl',0d,'nhav_gl',0d,$
                   'z_bg',0d,'z_bl',0d,'z_gl',0d,$
                   'xlum_11hr_bg',0d,'xlum_11hr_bl',0d,'xlum_11hr_gl',0d,$
                   'xlum_1day_bg',0d,'xlum_1day_bl',0d,'xlum_1day_gl',0d,$
                   'olum_11hr_bg',0d,'olum_11hr_bl',0d,'olum_11hr_gl',0d,$
                   'olum_1day_bg',0d,'olum_1day_bl',0d,'olum_1day_gl',0d,$
                   'eiso_bg',0d,'eiso_bl',0d,'eiso_gl',0d,$
                   'theta_bg',0d,'theta_bl',0d,'theta_gl',0d,$
                   'egam_bg',0d,'egam_bl',0d,'egam_gl',0d,$
                   'ntheta_bg',0d,'ntheta_bl',0d,'ntheta_gl',0d,$
                   'negam_bg',0d,'negam_bl',0d,'negam_gl',0d,$
                   'hr_bg',0d,'hr_bl',0d,'hr_gl',0d,$
                   'eta_bg',0d,'eta_bl',0d,'eta_gl',0d)

  ks=replicate(ks,2)


;  t_spec=150d
  t_spec=86400d ;; 1 day
  t11=11.*3600d ;; 11 hours
;  t_spec=150000d
  t30=30.*86400d
  t60=60.*86400d
;  t200=2d5
;  t200=1d6
  t200=150000d
  flux200=0d

  ;;; identify short/long bursts

  if keyword_set(flux) then add='_flux'
  if keyword_set(lum) then add='_lum'
  if keyword_set(lattoo) then add=add+'_lat'
  if keyword_set(fdens) then add='_fdens'
  if n_elements(add) eq 0 then add='_counts'
  if keyword_set(mag) then add='_mag'
  if keyword_set(ps) then charsize=2 else charsize=1.5
  if keyword_set(uvot) then goto,skip2uvot
  if keyword_set(energ) then goto,skip2energ
;  if keyword_set(justplot) then goto,skip_justplot
  
;;; BEGIN XRT STUFF
;  cd,!adata
  cd,!grbs
;  grb=file_search('GRB*')
;  match,grb,tgrb,m1,m2
;  tgrb=tgrb[m2]
;  dont_match,grb,tgrb,m1,m2
;  grb=grb[m1]
;  n=n_elements(grb)
;  dont_match,tgrb,latgrbs,m1,m2
;  tgrb=tgrb[m1]
  swift=where(grbstr.who eq 'BAT')
  grb=grbstr[swift].grb
  gbm=where(grbstr.who eq 'GBM')
  lat=where(grbstr.who eq 'LAT')
  tgrb=grbstr[gbm].grb
  
  if not keyword_set(justplot) then begin 
     for q=0,2 do begin ;; long/short grbs
        if q eq 0 then add2=add+'_long'
        if q eq 1 then add2=add+'_short'
        if q eq 2 then add2=add+'_both'

  ;;; begin XRT
        if keyword_set(ps) then $
           begplot,name='~/Fermi/Swift_pop_study/xray_lc_compare'+add2+'.eps',/encap,/color,/land,font='helvetica'
        !x.margin=[12,0]
        
        colors=[!red,!blue,!green,!forestgreen,!cyan,!magenta,!purple,!orange,!sienna]
;        colors=replicate(!p.color,8)
        if not keyword_set(flux) and not keyword_set(lum) and not keyword_set(fdens) then $
           plot,[10,1e7],[1e-5,1e4],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Count Rate (0.3-10 keV) (counts s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[10,1e7],yrange=[1e-5,1e4],yticks=9,charsize=charsize
        if keyword_set(flux) and not keyword_set(fdens) then $
           plot,[10,1e7],[1e-15,1e-6],/nodata,/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='Flux (0.3-10 keV) (erg cm!U-2!N s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[10,1e7],yrange=[1e-15,1e-6],yticks=9,charsize=charsize
        if keyword_set(lum) then $
           plot,[10,1e7],[1d40,1d51],/nodata,/xlog,/ylog,xtitle='Time since trigger (s) / (1+z)',ytitle='X-ray Luminosity (erg s!U-1!N)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[10,1e7],yrange=[1d40,1d51],yticks=11,charsize=charsize
        if keyword_set(fdens) then $
           plot,[10,1e7],[1e-10,1e-2],/nodata,/xlog,/ylog,xtitle='Time since trigger (s) / (1+z)',ytitle='Flux Density (Jy)',xsty=1,ysty=1,xminor=9,yminor=9,xrange=[10,1e7],yrange=[1d-10,1d-2],yticks=8,charsize=charsize

        for k=0,2 do begin
;        kalphas=0d
;        kalphas_err=[2,0d]
;        kbetas=0d
;        kgrbs=''
           odd=''
;        jb=0
;        tbreak=0d
;        tzs=0.

           case k of 
              0: begin
;              kgrb=grb
                 samp=where(grbstr.who eq 'BAT')
                 color=!grey50
;                 color=!grey70
                 scolor=!orange
              end
              1: begin
;              kgrb=tgrb
                 samp=where(grbstr.who eq 'GBM')
                 color=!grey20
;                 color=!grey40
                 scolor=!salmon
              end
              2: begin
;              kgrb=latgrbs
                 samp=where(grbstr.who eq 'LAT')
;              table=dblarr(13,n_elements(kgrb))
           ;;; table pars
           ;;; alpha, beta, z, nh_gal, nh_intrin, last_flux, last_time, flux_30d, flux_60d,
           ;;; cxo_ctr, need_exptime
              end 
           endcase
           kgrb=grbstr[samp].grb
           dcolor=color
           nk=n_elements(grbstr[samp])
;        nk=n_elements(kgrb)
;        lum_spec=dblarr(nk)
;        kalphas=dblarr(nk)
;        kalphas_err=dblarr(2,nk)
;        kbetas=dblarr(nk)
;        kgrbs=strarr(nk)
;        jb=intarr(nk)
;        tzs=fltarr(nk)
;        tbreak=dblarr(nk)
           
;        if k le 1 then begin
;        match,kgrb,ssgrbs,dm1,dm2
;        short=intarr(nk)
;        x=where(grbstr[samp].dur eq 'short')
;        short[samp[x]]=1
;        x=where(strtrim(dur[dm2],2) eq 'short')
;        short[dm1[x]]=1
;        endif 
           

;;         if q eq 0 then begin 
;;            case k of
;;               0: begin 
;;                  alphas=dblarr(nk) & alphas_err=dblarr(2,nk) & betas=dblarr(nk)
;;                  xlum_spec=dblarr(nk) & sgrbs=strarr(nk) & stbreak=dblarr(nk)
;;                  sjb=intarr(nk) & sz=fltarr(nk)
;;               end 
;;               1: begin 
;;                  galphas=dblarr(nk) & galphas_err=dblarr(2,nk) & gbetas=dblarr(nk)
;;                  xglum_spec=dblarr(nk) & ggrbs=strarr(nk) & gtbreak=dblarr(nk) 
;;                  gjb=intarr(nk) & gz=fltarr(nk)
;;               end 
;;               2: begin 
;;                  lalphas=dblarr(nk) & lalphas_err=dblarr(2,nk) & lbetas=dblarr(nk)
;;                  xllum_spec=dblarr(nk) & lgrbs=strarr(nk) & ltbreak=dblarr(nk) 
;;                  ljb=intarr(nk) & lz=fltarr(nk)
;;               end
;;            endcase 
;;         endif 
           ind=0
           ind0=0
           for i=0,nk-1 do begin
              if (q eq 0 and grbstr[samp[i]].dur eq 'long') or (q eq 1 and grbstr[samp[i]].dur eq 'short') or q eq 2 then begin
                 print,kgrb[i]
                 cd,kgrb[i]
;              if k le 1 and short[i] eq 1 then color=scolor else color=dcolor
                 if k eq 2 then color=colors[i]
                 lc=lcout2fits(/phil,/silent)
                 dist=0.
                 tf=1.
                 if exist('lc_fit_out_idl_int7.dat') and exist('UL_specfits.fits') then begin 
                    if keyword_set(flux) or keyword_set(lum) then begin 
                       spec=mrdfits('UL_specfits.fits',1) ;,/silent)
                       ns=n_elements(spec)                ;;; IS THIS UNABSORBED??????
                       
                       fluxfact=spec[ns-1].unabs_cfratio ;;; GET's LEICESTER PC FIT, IF NOT PC THEN WT
                       if keyword_set(fdens) then fluxfact=fluxfact*flux2jy(1.,spec[ns-1].phind)
                       wz=where(kgrb[i] eq ssgrbs,nwz) ;;; grab correct z
                       if nwz gt 0 then z=zs[wz[0]] else z=0
                       if keyword_set(lum) and z gt 0 then begin

                          print,z,spec[0].z
                          tf=1./(1.+z)
                          dist=lumdist(z,h0=71,omega_m=0.27)*mpc2cm
                          grbstr[samp[i]].dist=dist
                          beta=spec[ns-1].phind-1
                          lcfitfile='lc_fit_out_idl_int8.dat'
                          if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
                          read_lcfit,lcfitfile,pname,p,perr,np=np
;                          read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perr
;                          np=n_elements(p)
                          if np eq 2 then begin
                             x=1
                             f=pow(t_spec/tf,p)
                             fo=pow(t_spec,p)
                             f11=pow(t11/tf,p)
                             fo11=pow(t11,p)
                             f30=pow(t30,p)
                             f60=pow(t60,p)
                             f200=pow(t200,p)
                             grbstr[samp[i]].xtype='SPL'
                          endif 
                          if np eq 4 then begin
                             x=3
                             f=bknpow(t_spec/tf,p)
                             fo=bknpow(t_spec,p)
                             f11=bknpow(t11/tf,p)
                             fo11=bknpow(t11,p)
                             f30=bknpow(t30,p)
                             f60=bknpow(t60,p)
                             f200=bknpow(t200,p)
                             if p[1] gt p[3] then grbstr[samp[i]].xtype='I-II'
                             if p[1] lt p[3] then grbstr[samp[i]].xtype='II-III'
                          endif 
                          if np eq 6 then begin
                             if p[1] gt p[3] then x=5 else x=3
                             f=bkn2pow(t_spec/tf,p)
                             fo=bkn2pow(t_spec,p)
                             f11=bkn2pow(t11/tf,p)
                             fo11=bkn2pow(t11,p)
                             f30=bkn2pow(t30,p)
                             f60=bkn2pow(t60,p)
                             f200=bkn2pow(t200,p)
                             if p[1] gt p[3] then grbstr[samp[i]].xtype='I-II-III'
                             if p[1] lt p[3] then grbstr[samp[i]].xtype='II-III-IV'
                          endif 
                          if np eq 8 then begin
                             x=5
                             f=bkn3pow(t_spec/tf,p)
                             fo=bkn3pow(t_spec,p)
                             f11=bkn3pow(t11/tf,p)
                             fo11=bkn3pow(t11,p)
                             f30=bkn3pow(t30,p)
                             f60=bkn3pow(t60,p)
                             f200=bkn3pow(t200,p)
;                             if p[1] gt p[3] then 
                             grbstr[samp[i]].xtype='I-II-III-IV'
;                             if p[1] lt p[3] then grbstr[samp[i]].xtype='II-III-IV-V'
                          endif 
;                          wlate=where(lc.time*tf gt 1e4,nwlate)
;              alpha=alp
                          which_alpha,t_spec/tf,p,aspec,aa,np=np
                          alpha=aspec
                          alp=p[x]
                          alpha_err=perr[*,aa]
                          which_alpha,t_spec,p,aospec,aa,np=np
                          aoerr=perr[*,aa]
                          which_alpha,t11/tf,p,a11spec,aa,np=np
                          a11err=perr[*,aa]
                          which_alpha,t11,p,ao11spec,aa,np=np
                          ao11err=perr[*,aa]
                 ;;; need to identify jet breaks
                 ;;; need to identify jet break limits
                 ;;; need to calculated opening angles, egamma, etc.
                          t11hr=11.*3600
                          t1d=86400.
                          g11=0
                          g1=0

;                          if nwlate gt 0 or k eq 2 then begin
                          ind=[ind,i]
;                          kalphas[i]=alpha
;                          kalphas_err[*,i]=alpha_err
                          grbstr[samp[i]].xalpha=alpha
                          grbstr[samp[i]].xalpha_err=alpha_err
;                          kbetas[i]=beta
                          grbstr[samp[i]].xbeta=beta
                          grbstr[samp[i]].xbeta_err=spec[ns-1].phinderr
                          grbstr[samp[i]].nh=spec[ns-1].nh
                          grbstr[samp[i]].nh_err=spec[ns-1].nherr
                                ;                         kgrbs[i]=kgrb[i]
                          last_alpha=p[np-1]
                          if grbstr[samp[i]].xtype eq 'I-II-III-IV' or grbstr[samp[i]].xtype eq 'II-III-IV' then begin 
                             grbstr[samp[i]].xjb=1
                             grbstr[samp[i]].xalpha3=p[np-3]
                             grbstr[samp[i]].xalpha3_err=perr[*,np-3]
                             if p[np-4] lt t11hr and p[np-2] gt t11hr then g11=1
                             if p[np-4] lt t1d and p[np-2] gt t1d then g1=1
                          endif 
                          if grbstr[samp[i]].xtype eq 'I-II-III' or grbstr[samp[i]].xtype eq 'II-III' then begin 
                             alp=[0.28,1.09,1.89]
                             if np eq 6 then begin
                                a0=p[3]
                                a1=p[5]
                                a0err=perr[*,3]
                                a1err=perr[*,5]
                             endif 
                             if np eq 4 then begin
                                a0=p[1]
                                a1=p[3]
                                a0err=perr[*,1]
                                a1err=perr[*,3]
                             endif 
                                ;;; first seg diff II
                             if a0-alp[0] lt 0 then a02rr=a0err[1] else a02rr=a0err[0]

                                ;;; first seg diff III
                             if a0-alp[1] lt 0 then a03rr=a0err[1] else a03rr=a0err[0]

                                ;;; second seg diff III
                             if a1-alp[1] lt 0 then a13rr=a1err[1] else a13rr=a1err[0]

                                ;;; second seg diff IV
                             if a1-alp[2] lt 0 then a14rr=a1err[1] else a14rr=a1err[0]

                             d23=sqrt(((a0-alp[0])/a02rr)^2+((a1-alp[1])/a13rr)^2)
                             d24=sqrt(((a0-alp[0])/a02rr)^2+((a1-alp[2])/a14rr)^2)
                             d34=sqrt(((a0-alp[1])/a03rr)^2+((a1-alp[2])/a14rr)^2)

                             m=min([d23,d24,d34],wm)
                             
                             if wm eq 0 then type=23
                             if wm eq 1 then type=24
                             if wm eq 2 then type=34

;                                d02=abs(a0-alp[0)]
;                                d03=abs(a0-alp[1])
;                                d13=abs(a1-alp[1])
;                                d14=abs(a1-alp[2])
;                                if d02 lt d03 and d13 lt d14 then type=23
;                                if d02 lt d03 and d13 gt d14 then type=24
;                                if d02 gt d03 and d13 gt d14 then type=34
                             if np eq 6 and type eq 24 then begin 
                                grbstr[samp[i]].xtype='I-II-IV'
                                grbstr[samp[i]].xjb=1
                             endif 
                             if np eq 6 and type eq 34 then begin 
                                grbstr[samp[i]].xtype='I-III-IV'
                                grbstr[samp[i]].xjb=1
                                grbstr[samp[i]].xalpha3=p[np-3]
                                grbstr[samp[i]].xalpha3_err=perr[*,np-3]
                                   ;;; need to determine if 11hr &
                                   ;;; 1day is in III
                                if p[np-4] lt t11hr and p[np-2] gt t11hr then g11=1
                                if p[np-4] lt t1d and p[np-2] gt t1d then g1=1
                             endif 
                             if type eq 23 then begin 
                                grbstr[samp[i]].xalpha3=p[np-1]
                                grbstr[samp[i]].xalpha3_err=perr[*,np-1]
                                if p[np-2] lt t11hr then g11=1
                                if p[np-2] lt t1d then g1=1
                             endif 
                             if np eq 4 and (type eq 24 or type eq 34) then begin
                                grbstr[samp[i]].xjb=1
                                if type eq 24 then grbstr[samp[i]].xtype='II-IV'
                                if type eq 34 then begin 
                                   grbstr[samp[i]].xtype='III-IV'
                                   grbstr[samp[i]].xalpha3=p[1]
                                   grbstr[samp[i]].xalpha3_err=perr[*,1]
                                   if p[2] gt t11hr then g11=1
                                   if p[2] gt t1d then g1=1
                                endif 
                             endif 
                          endif
                          if grbstr[samp[i]].xjb eq 1 then grbstr[samp[i]].xtbreak=p[np-2] else begin
                             wdet=where(lc.src_rate_err gt 0)
                             grbstr[samp[i]].xtbreak=max(lc[wdet].tstop)
                          endelse 
                          if grbstr[samp[i]].xtype eq 'SPL' and last_alpha gt 1.5 then begin
                             grbstr[samp[i]].xjb=2
                             grbstr[samp[i]].xtbreak=min(lc.tstart)
                          endif 
                          if grbstr[samp[i]].xtype eq 'SPL' and last_alpha lt 1.7 and last_alpha gt 0.8 then begin 
                             grbstr[samp[i]].xalpha3=grbstr[samp[i]].xalpha
                             grbstr[samp[i]].xalpha3_err=grbstr[samp[i]].xalpha_err
                             g11=1
                             g1=1
                          endif 
;;                              if last_alpha gt 1.5 and np ge 2 then begin 
;; ;                             tbreak[i]=p[np-2]
;;                                 grbstr[samp[i]].xtbreak=p[np-2]
;;                                 grbstr[samp[i]].xjb=1
;; ;                             jb[i]=1
;;                              endif else begin 
;; ;                             tbreak[i]=max(lc.time)
;;                                 grbstr[samp[i]].xtbreak=max(lc.time)
;; ;                             jb[i]=0
;;                                 grbstr[samp[i]].xjb=0
;;                              endelse 

;                          tzs[i]=z
;                          endif  else grbstr[samp[i]].xalpha3=-1 ;;; what is slope at t>10^4 s?
;              alpha=1.;;; needs to be normal decay??????
                          print,4.*!pi*dist^2*(1.+z)^(beta-alpha-1.),z,dist,alpha,beta
                          fdfact=flux2jy(1.,beta+1.,eeff=1.)
                          nu_1kev=2.42d17
                          lumfact=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-alpha-1.) ;*fdfact*1d-23*nu_1kev
                          lumfact_err=sqrt((max(grbstr[samp[i]].xalpha_err)/grbstr[samp[i]].xalpha)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*lumfact
                          lumfacto=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-aospec-1.) ;*fdfact*1d-23*nu_1kev
                          lumfacto_err=sqrt((max(aoerr)/aospec)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*lumfacto
                          lumfact11=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-a11spec-1.) ;*fdfact*1d-23*nu_1kev
                          lumfact11_err=sqrt((max(a11err)/a11spec)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*lumfact11
                          lumfact11o=fluxfact*4.*!pi*dist^2*(1.+z)^(beta-ao11spec-1.) ;*fdfact*1d-23*nu_1kev
                          lumfact11o_err=sqrt((max(ao11err)/ao11spec)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*lumfact11o

;                       lum_spec[i]=f*lumfact
                          ind0=[ind0,i]

                          grbstr[samp[i]].xfdfact=fdfact
                          grbstr[samp[i]].xfluxfact=fluxfact
                          fluxfact=lumfact
                          grbstr[samp[i]].xlumfact=lumfact
                          grbstr[samp[i]].xlumfact_err=lumfact_err
                          tmax=max(lc.tstop)
                          if tmax gt t1d/tf then begin 
                             grbstr[samp[i]].xlum_1day=f*lumfact
                             grbstr[samp[i]].xlum_1day_err=f*lumfact_err
                          endif 
                          if tmax gt t1d then begin 
                             grbstr[samp[i]].xlum_1day_obs=fo*lumfacto
                             grbstr[samp[i]].xlum_1day_obs_err=fo*lumfacto_err
                          endif 
                          if tmax gt t11hr/tf then begin 
                             grbstr[samp[i]].xlum_11hr=f11*lumfact11
                             grbstr[samp[i]].xlum_11hr_err=f11*lumfact11_err
                          endif 
                          if tmax gt t11hr then begin 
                             grbstr[samp[i]].xlum_11hr_obs=fo11*lumfact11o
                             grbstr[samp[i]].xlum_11hr_obs_err=fo11*lumfact11o_err
                          endif 
                          if g11 eq 1 then begin 
                             grbstr[samp[i]].xflux_11hr_obs=fo11*grbstr[samp[i]].xfluxfact
                             grbstr[samp[i]].xflux_11hr_obs_err=fo11*grbstr[samp[i]].xfluxfact*lumfact11o_err/lumfact11o
                          endif 
                          if g1 eq 1 then begin 
                             grbstr[samp[i]].xflux_1day_obs=fo*grbstr[samp[i]].xfluxfact
                             grbstr[samp[i]].xflux_1day_obs_err=fo*grbstr[samp[i]].xfluxfact*lumfacto_err/lumfacto
                          endif 
                       endif else begin 
              ;;; flux
                          dist=1.
                          lcfitfile='lc_fit_out_idl_int8.dat'
                          if not exist(lcfitfile) then lcfitfile='lc_fit_out_idl_int7.dat'
                          read_lcfit,lcfitfile,pname,p,perr,np=np

;                          read_lcfit,'lc_fit_out_idl_int7.dat',pname,p,perr
;                          np=n_elements(p)
                          which_alpha,t_spec,p,aospec,aa,np=np
                          aoerr=perr[*,aa]
                          which_alpha,t11,p,ao11spec,aa,np=np
                          ao11err=perr[*,aa]
                          grbstr[samp[i]].xbeta=spec[ns-1].phind-1
                          grbstr[samp[i]].xbeta_err=spec[ns-1].phinderr
                          beta=grbstr[samp[i]].xbeta
                          fdfact=flux2jy(1.,beta+1.,eeff=1.)
                          grbstr[samp[i]].xfdfact=fdfact
                          if np eq 2 then begin
                             f30=pow(t30,p)
                             f60=pow(t60,p)
                             f200=pow(t200,p)
                             fo=pow(t_spec,p)
                             fo11=pow(t11,p)
                             grbstr[samp[i]].xtype='SPL'
                          endif 
                          if np eq 4 then begin
                             f30=bknpow(t30,p)
                             f60=bknpow(t60,p)
                             f200=bknpow(t200,p)
                             fo=bknpow(t_spec,p)
                             fo11=bknpow(t11,p)
                             if p[1] gt p[3] then grbstr[samp[i]].xtype='I-II'
                             if p[1] lt p[3] then grbstr[samp[i]].xtype='II-III/III-IV'
                          endif 
                          if np eq 6 then begin
                             f30=bkn2pow(t30,p)
                             f60=bkn2pow(t60,p)
                             f200=bkn2pow(t200,p)
                             fo=bkn2pow(t_spec,p)
                             fo11=bkn2pow(t11,p)
                             if p[1] gt p[3] then grbstr[samp[i]].xtype='I-II-III'
                             if p[1] lt p[3] then grbstr[samp[i]].xtype='II-III-IV'
                             grbstr[samp[i]].xtbreak=p[np-2]
                             xbf=bkn2pow(p[np-2],p)
                          endif 
                          if np eq 8 then begin
                             f30=bkn3pow(t30,p)
                             f60=bkn3pow(t60,p)
                             f200=bkn3pow(t200,p)
                             fo=bkn3pow(t_spec,p)
                             fo11=bkn3pow(t11,p)
                             grbstr[samp[i]].xtype='I-II-III-IV'
                             grbstr[samp[i]].xtbreak=p[np-2]
                             xbf=bkn3pow(p[np-2],p)
                          endif 
                          alpha=p[np-1]
                          alpha_err=perr[*,np-1]
                          f30=f30*fluxfact
                          f60=f60*fluxfact
                          f200=f200*fluxfact
                          det=where(lc.src_rate_err gt 0,ndet)
;                       if max(lc[det].tstop) lt t200 then f200=0.
                          grbstr[samp[i]].xalpha=alpha
                          grbstr[samp[i]].xalpha_err=alpha_err
;                          grbstr[samp[i]].xlum_1day_obs=fo*fluxfact
;                          grbstr[samp[i]].xlum_1day_obs_err=fo*fluxfact_err
                          grbstr[samp[i]].xfluxfact=fluxfact
                          fluxfact11o_err=sqrt((max(ao11err)/ao11spec)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*fluxfact
                          fluxfacto_err=sqrt((max(aoerr)/aospec)^2+(max(grbstr[samp[i]].xbeta_err)/grbstr[samp[i]].xbeta)^2)*fluxfact
                          g11=0
                          g1=0
                          if max(lc.time) gt t11 then g11=1
                          if max(lc.time) gt t_spec then g1=1
                          if g11 eq 1 then begin 
                             grbstr[samp[i]].xflux_11hr_obs=fo11*grbstr[samp[i]].xfluxfact
                             grbstr[samp[i]].xflux_11hr_obs_err=fo11*grbstr[samp[i]].xfluxfact*fluxfact11o_err/fluxfact
                          endif 
                          if g1 eq 1 then begin 
                             grbstr[samp[i]].xflux_1day_obs=fo*grbstr[samp[i]].xfluxfact
                             grbstr[samp[i]].xflux_1day_obs_err=fo*grbstr[samp[i]].xfluxfact*fluxfacto_err/fluxfact
                          endif 
                          if grbstr[samp[i]].xtbreak ne 0 then grbstr[samp[i]].xbreakflux=xbf*grbstr[samp[i]].xfluxfact
                       endelse 
                    endif else begin
                       ;; count rate
                       fluxfact=1.
                       dist=1.
                    endelse 
                    if dist gt 0 then begin 
                       det=where(lc.src_rate_err gt 0,ndet)
                       if ndet gt 1 then begin 
                          oplot,lc[det].time*tf,lc[det].src_rate*fluxfact,psym=3,color=color
                          for j=0,ndet-1 do begin
                             oplot,[lc[det[j]].tstart,lc[det[j]].tstop]*tf,[lc[det[j]].src_rate,lc[det[j]].src_rate]*fluxfact,color=color
                             oplot,[lc[det[j]].time,lc[det[j]].time]*tf,[lc[det[j]].src_rate-lc[det[j]].src_rate_err,lc[det[j]].src_rate+lc[det[j]].src_rate_err]*fluxfact,color=color
                          endfor 

                       endif 
                    endif
                    if keyword_set(flux) and k eq 2 then begin 
                       ldet=det[ndet-1]
                       ;; 30 days
                       pimmsfile='pimms.xco'
                       com1='model pl '+ntostr(spec[ns-1].phind)+' '+ntostr(spec[ns-1].nh)+' z '+ntostr(z)+' '+ntostr(spec[ns-1].nhgal*1d22)
                       com2='from flux photons 0.3-10.0'
                       com3='instrument chandra acis-s 0.2-10.0'
                       com4='go '+ntostr(f30)+' flux ergs 0.3-10.0'
                       com5='output pimms.dat 0.2 10.0 0.005'
                       com6='quit'
                       writecol,pimmsfile,[com1,com2,com3,com4,com5,com6]
                       spawn,'pimms @'+pimmsfile+'> pimms.log'
                       readcol,'pimms.log',lines,delim='$',format='(a)'
                       for l=0,n_elements(lines)-1 do begin
                          spos=strpos(lines[l],'predicts')
                          if spos ne -1 then begin
                             str=str_sep(lines[l],' ')
                             ctr30=str[3]
                          endif
                       endfor 

                       ;; 60 days
                       com4='go '+ntostr(f60)+' flux ergs 0.3-10.0'
                       writecol,pimmsfile,[com1,com2,com3,com4,com5,com6]
                       spawn,'pimms @'+pimmsfile+'> pimms.log'
                       readcol,'pimms.log',lines,delim='$',format='(a)'
                       for l=0,n_elements(lines)-1 do begin
                          spos=strpos(lines[l],'predicts')
                          if spos ne -1 then begin
                             str=str_sep(lines[l],' ')
                             ctr60=str[3]
                          endif
                       endfor 
                       
                       ncounts=10.
                       exptime30=ncounts/ctr30
                       exptime60=ncounts/ctr60

                       ;; read in count rate and put in table
;                    table[*,i]=[alpha,spec[ns-1].phind,z,spec[ns-1].nhgal*1d22,spec[ns-1].nh,lc[ldet].src_rate*fluxfact,lc[ldet].time,f30,f60,ctr30,ctr60,exptime30,exptime60]


;; model pl 2.0 1e22 z 1.0 1e20
;; FROM FLUX PHOTONS 0.3-10.0   
;; INSTRUMENT CHANDRA ACIS-S
;; go 1e-15 flux ergs 0.2-10.0

           ;;; alpha, beta, z, nh_gal, nh_intrin, last_flux, last_time, flux_30d, flux_60d,
           ;;; cxo_ctr, need_exptime
                    endif 
                    if k eq 0 and keyword_set(lum) then flux200=[flux200,f200]
                    
                 endif
                 
                 cd,'..'
              endif 
           endfor
           ;; if keyword_set(lum) then begin 
;;            inc0=where(lum_spec ne 0)
;;            inc=where(kalphas ne 0)
;;            ind=ind[1:*]
;;            ind0=ind0[1:*]
;;            case k of 
;;               0: begin
;;                  alphas[ind]=kalphas[inc]
;;                  alphas_err[*,ind]=kalphas_err[*,inc]
;;                  betas[ind]=kbetas[inc]
;;                  xlum_spec[ind0]=lum_spec[inc0]
;;                  sgrbs[ind]=kgrbs[inc]
;;                  stbreak[ind]=tbreak[inc]
;;                  sjb[ind]=jb[inc]
;;                  sz[ind]=tzs[inc]
;;               end
;;               1: begin
;;                  galphas[ind]=kalphas[inc]
;;                  galphas_err[*,ind]=kalphas_err[*,inc]
;;                  gbetas[ind]=kbetas[inc]
;;                  xglum_spec[ind0]=lum_spec[inc0]
;;                  ggrbs[ind]=kgrbs[inc]
;;                  gtbreak[ind]=tbreak[inc]
;;                  gjb[ind]=jb[inc]
;;                  gz[ind]=tzs[inc]
;;               end
;;               2: begin
;;                  lalphas[ind]=kalphas[inc]
;;                  lalphas_err[*,ind]=kalphas_err[*,inc]
;;                  lbetas[ind]=kbetas[inc]
;;                  xllum_spec[ind0]=lum_spec[inc0]
;;                  lgrbs[ind]=kgrbs[inc]
;;                  ltbreak[ind]=tbreak[inc]
;;                  ljb[ind]=jb[inc]
;;                  lz[ind]=tzs[inc]
;;               end 
;;            endcase 
;;         end 
;;         if q eq 1 then begin 
;;            case k of
;;               0: begin 
;;                  ind=where(alphas ne 0)
;;                  alphas=alphas[ind] & alphas_err=alphas_err[*,ind] & betas=betas[ind]
;;                  xlum_spec=xlum_spec[ind] & sgrbs=sgrbs[ind] & stbreak=stbreak[ind]
;;                  sjb=sjb[ind] & sz=sz[ind]
;;               end 
;;               1: begin 
;;                  ind=where(galphas ne 0)
;;                  galphas=galphas[ind] & galphas_err=galphas_err[*,ind] & gbetas=gbetas[ind]
;;                  xglum_spec=xglum_spec[ind] & ggrbs=ggrbs[ind] & gtbreak=gtbreak[ind]
;;                  gjb=gjb[ind] & gz=gz[ind]
;;               end 
;;               2: begin
;;                  ind=where(lalphas ne 0) 
;;                  lalphas=lalphas[ind] & lalphas_err=lalphas_err[*,ind] & lbetas=lbetas[ind]
;;                  xllum_spec=xllum_spec[ind] & lgrbs=lgrbs[ind] & ltbreak=ltbreak[ind]
;;                  ljb=ljb[ind] & lz=lz[ind]
;;               end
;;            endcase
;;         endif 
        endfor 
;stop
;     if q eq 0 then s=where(short eq 0) else s=where(short eq 1)
        case q of 
           0: s=where(strtrim(grbstr[samp].dur,2) eq 'long')
           1: s=where(strtrim(grbstr[samp].dur,2) eq 'short')
           2: s=indgen(n_elements(samp))
        endcase 

        slen=strlen(latgrbs[s])
        wslen=where(slen eq 9)
        strl=strarr(n_elements(s))
        strl[wslen]='  '
        if q eq 1 then strl=''
        legend,['LAT '+latgrbs[s]+strl,'BAT GRBs','GBM GRBs'],textcolor=[colors[s],!grey50,!grey20],box=0,/top,/right
        if q le 1 then begin 
           if q eq 0 then leg='Long'
           if q eq 1 then leg='Short'
           legend,leg,/bottom,/left,box=0
        endif 

;        legend,['LAT GRBs','Swift GRBs','GBM/BAT GRBs'],textcolor=[!p.color,!grey70,!grey40],box=0,/top,/right
        if keyword_set(lattoo) then plot_lat_lcs_too
        if keyword_set(ps) then endplot

;     print,table
;     a=' & '
;     nhgal=table[3,*]
  ;;; alpha, beta, z, nh_gal, nh_intrin, last_flux, last_time, flux_30d, flux_60d,
  ;;; cxo_ctr30, cxo_ctr60, need_exptime30,need_exptime60

;     f1='$'+ntostr(table[5,*],3)+'\times 10^{'+ntostr(round(alog10(table[5,*])-0.5))+'}$'
;     f2='$'+ntostr(table[7,*],3)+'\times 10^{'+ntostr(round(alog10(table[7,*])-0.5))+'}$'
;     f3='$'+ntostr(table[8,*],3)+'\times 10^{'+ntostr(round(alog10(table[8,*])-0.5))+'}$'
;     c2='$'+ntostr(sigfig(table[9,*],3,/sci),3)+'\times 10^{'+ntostr(round(alog10(table[9,*])-0.5))+'}$'
;     c3='$'+ntostr(sigfig(table[10,*],3,/sci),3)+'\times 10^{'+ntostr(round(alog10(table[10,*])-0.5))+'}$'
;  c2='$'+numdec(table[9,*]*1e3,2)+'$' 
;  c3='$'+numdec(table[10,*]*1e3,2)+'$'
;     colprint,kgrb+a+numdec(table[0,*],1)+a+f2+a+f3+a+c2+a+c3+a+numdec(table[11,*]/1e3,1)+a+numdec(table[12,*]/1e3,1)+'\\'
;  colprint,kgrb+a+numdec(table[0,*],1)+a+f1+a+ntostr(table[6,*]/86400.,4)+a+f2+a+f3+a+c2+a+c3+a+numdec(table[11,*]/1e3,1)+a+numdec(table[12,*]/1e3,1)+'\\'
;  colprint,kgrb+a+numdec(table[0,*],2)+a+numdec(table[1,*],2)+a+numdec(t[2,*],2)+a+ntostr(table[3,*]*1d22)+a+ntostr(table[4,*])+a+ntostr(table[5,*])+a+ntostr(table[6,*])+a+ntostr(table[7,*])+a+ntostr(table[8,*])
     endfor      

     if keyword_set(lum) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',/create
     if keyword_set(flux) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',/create
  endif

  if keyword_set(justplot) and keyword_set(lum) then $
     grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  if keyword_set(justplot) and keyword_set(flux) then $
     grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',1)
  
  if keyword_set(lum) then begin 
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.xalpha ne 0 and grbstr.xlum_1day gt 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.xalpha ne 0 and grbstr.xlum_1day gt 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.xalpha ne 0 and grbstr.xlum_1day gt 0)

     ;;; CORRELATION PLOTS BETTER THAN HISTOGRAMS ??!??

     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short' and grbstr[sgrbs].xalpha le 3,nss)
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long' and grbstr[sgrbs].xalpha le 3,nls)
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short' and grbstr[ggrbs].xalpha le 3,nsg)
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long' and grbstr[ggrbs].xalpha le 3,nlg)
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short' and grbstr[lgrbs].xalpha le 3,nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long' and grbstr[lgrbs].xalpha le 3,nll)
     
     !x.margin=[12,0]
     begplot,name='~/Fermi/Swift_pop_study/xalpha_lum_corr.eps',/land,/color,/encap,font='helvetica'

     arange=[0,3]
     lumrange=10d^[40,47]
     plotsym,0,1,/fill

     scatter_hist,grbstr.xalpha,grbstr.xlum_1day,grbstr.xalpha_err,grbstr.xlum_1day_err,bin1=0.1,bin2=0.2,xrange=arange,yrange=lumrange,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle=!tsym.alpha+'!Lx!N',ytitle='L!Lx,1 day!N (erg s!U-1!N)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/top,/right,charsize=1.

;     legend,['long','short'],box=0,/bottom,/left,psym=[8,2]
     endplot

;     writecol,'~/Fermi/Swift_pop_study/x_betas.dat',[sgrbs,lgrbs,ggrbs],[alphas,lalphas,galphas],[betas,lbetas,gbetas],[replicate('Swift',n_elements(sgrbs)),replicate('LAT',n_elements(lgrbs)),replicate('GBM',n_elements(ggrbs))]
     writecol,'~/Fermi/Swift_pop_study/x_betas.dat',grbstr.grb,grbstr.xalpha,grbstr.xbeta,grbstr.who
     if not keyword_set(ps) then stop
  ;;; alphas dist plot
;  odd=odd[1:*]
;     dont_match,alphas,lalphas,m1,m2


     ;;; xalphas_xbetas scatter_hist

     sgrbs=where(grbstr.who eq 'BAT' and grbstr.xalpha ne 0 and grbstr.xbeta ne 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.xalpha ne 0 and grbstr.xbeta ne 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.xalpha ne 0 and grbstr.xbeta ne 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')

     kstwop,grbstr[sgrbs[ls]].xalpha,grbstr[ggrbs[lg]].xalpha,d,gprob
     kstwop,grbstr[sgrbs[ls]].xalpha,grbstr[lgrbs[ll]].xalpha,d,lprob
     kstwop,grbstr[ggrbs[lg]].xalpha,grbstr[lgrbs[ll]].xalpha,d,glprob
     ks[0].xalpha_bg=gprob
     ks[0].xalpha_bl=lprob
     ks[0].xalpha_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].xalpha,grbstr[ggrbs[sg]].xalpha,d,gprob
;     kstwop,grbstr[sgrbs[ss]].xalpha,grbstr[lgrbs[sl]].xalpha,d,lprob
;     kstwop,grbstr[ggrbs[sg]].xalpha,grbstr[lgrbs[sl]].xalpha,d,glprob
;     ks[1].xalpha_bg=gprob
;     ks[1].xalpha_bl=lprob
;     ks[1].xalpha_gl=glprob    

     kstwop,grbstr[sgrbs[ls]].xbeta,grbstr[ggrbs[lg]].xbeta,d,gprob
     kstwop,grbstr[sgrbs[ls]].xbeta,grbstr[lgrbs[ll]].xbeta,d,lprob
     kstwop,grbstr[ggrbs[lg]].xbeta,grbstr[lgrbs[ll]].xbeta,d,glprob
     ks[0].xbeta_bg=gprob
     ks[0].xbeta_bl=lprob
     ks[0].xbeta_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].xbeta,grbstr[ggrbs[sg]].xbeta,d,gprob
;     kstwop,grbstr[sgrbs[ss]].xbeta,grbstr[lgrbs[sl]].xbeta,d,lprob
;     kstwop,grbstr[ggrbs[sg]].xbeta,grbstr[lgrbs[sl]].xbeta,d,glprob
;     ks[1].xbeta_bg=gprob
;     ks[1].xbeta_bl=lprob
;     ks[1].xbeta_gl=glprob    

     plotsym,0,1,/fill
     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/xalpha_xbeta.eps',/color,/encap,/land,font='helvetica' else erase
     colors=[!grey50,!grey70,!grey20]
;     scatter_hist,grbstr.xalpha,grbstr.xbeta,grbstr.xalpha_err,grbstr.xbeta_err,bin1=0.1,bin2=0.1,xrange=[0,3],yrange=[0,3],psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle=!tsym.alpha+'!Lx!N',ytitle=!tsym.beta+'!Lx!N',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],charsize=1.
     scatter_hist,grbstr.xalpha,grbstr.xbeta,grbstr.xalpha_err,grbstr.xbeta_err,bin1=0.1,bin2=0.1,xrange=[0,3],yrange=[0,3],psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!grey70,!grey70,!grey20,!grey20],xtitle=!tsym.alpha+'!Lx!N',ytitle=!tsym.beta+'!Lx!N',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!grey70,!grey20],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],charsize=1.

     if keyword_set(ps) then endplot

     begplot,name='~/Fermi/Swift_pop_study/xalphas.eps',/color,/land,font='helvetica',/encap
     plot,[0,3],[0,15],/nodata,xtitle=!tsym.alpha+'!Lx!N (t=1 day rest frame)',ytitle='N',charsize=2.
     plothist,grbstr[sgrbs].xalpha,bin=0.1,/over,/fill,fcolor=!grey50,color=!grey50
;     wt1=where(theta lt 60)
;     wt2=where(theta ge 60)
;     plothist,galphas[wt2],bin=0.1,/over,color=!grey20
;     plothist,galphas[wt1],bin=0.1,/over,color=!grey20,/fill,fcolor=!grey20
     plothist,grbstr[ggrbs].xalpha,bin=0.1,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
     plothist,grbstr[lgrbs].xalpha,bin=0.1,/over,color=!red,line=2
     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]
;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4),'P!LKS,LAT!N = '+ntostr(olprob,4)],/top,/left,box=0,charsize=1.5,textcolor=[!grey50,!red,!blue]
     oplot,[0,100],[0,0]
     endplot
  ;;; betas dist plot
;     dont_match,betas,lbetas,m1,m2
     begplot,name='~/Fermi/Swift_pop_study/xbetas.eps',/color,/land,font='helvetica',/encap
     plot,[0,3],[0,30],/nodata,xtitle=!tsym.beta+'!Lx!N',ytitle='N',charsize=2.
     plothist,grbstr[sgrbs].xbeta,bin=0.1,/over,/fill,fcolor=!grey50,color=!grey50
     plothist,grbstr[ggrbs].xbeta,bin=0.1,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
                                ;    plothist,gbetas[wt2],bin=0.1,/over,color=!grey20
                                ;    plothist,gbetas[wt1],bin=0.1,/over,color=!grey20,/fill,fcolor=!grey20
     plothist,grbstr[lgrbs].xbeta,bin=0.1,/over,color=!red,line=2
     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]
;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4),'P!LKS,LAT,long!N = '+ntostr(olprob,4)],/top,/left,box=0,charsize=1.5,textcolor=[!blue,!red,!red]
     oplot,[0,100],[0,0]

     endplot
  endif 
  skip2uvot:
  if keyword_set(skipuvot) then goto,skipuvots
  if keyword_set(uvot) and keyword_set(lum) then grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)
  if keyword_set(uvot) and keyword_set(flux) then grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',1)
  simpctable

  if not keyword_set(justplot) then begin 
     cd,'~/Fermi/Swift_pop_study/'
     readcol,'UVOT_GRBs.dat',uggrbs,tid,ra,dec,format='(a,a,d,d)',skip=1,delim='|',/silent
     uggrbs=strcompress(uggrbs,/remove)
     w=where(ra ne 0)
     ra=ra[w]
     dec=dec[w]
     uggrbs=uggrbs[w]
     match,uggrbs,grbstr.grb,m1,m2
     grbstr[m2].ra=ra[m1]
     grbstr[m2].dec=dec[m1]
     
     readcol,'AVresults_tkII.dat',pgrb,av,averr1,averr0,gal,obeta,obetaerr1,obetaerr0,nh,nherr1,nherr0,model,format='(a,f,f,f,a,f,f,f,f,f,f,a)',/silent
     match,'GRB'+pgrb,strtrim(grbstr.grb,2),m1,m2
     grbstr.av=-1
     grbstr[m2].av=av[m1]
     grbstr[m2].av_err[0]=averr0[m1]
     grbstr[m2].av_err[1]=averr1[m1]
     grbstr[m2].nhs=nh[m1]*1d21
     grbstr[m2].gal=gal[m1]
     grbstr[m2].obeta=obeta[m1]
     
     for c=0,n_elements(m1)-1 do begin 
        grbstr[m2[c]].obeta_err[0]=conv_conf(obeta[m1[c]],obetaerr0[m1[c]],0.9)
        grbstr[m2[c]].obeta_err[1]=conv_conf(obeta[m1[c]],obetaerr1[m1[c]],0.9)
        grbstr[m2[c]].nhs_err[0]=conv_conf(nh[m1[c]],nherr0[m1[c]],0.9)*1d21
        grbstr[m2[c]].nhs_err[1]=conv_conf(nh[m1[c]],nherr1[m1[c]],0.9)*1d21
     endfor        

;     grbstr[m2].obeta_err[0]=obetaerr0[m1]
;     grbstr[m2].obeta_err[1]=obetaerr1[m1]

     for q=0,2 do begin ;; long/short grbs
        if q eq 0 then add2=add+'_long'
        if q eq 1 then add2=add+'_short'
        if q eq 2 then add2=add+'_both'
        !x.margin=[12,0]

        if keyword_set(ps) then $
           begplot,name='~/Fermi/Swift_pop_study/uvot_lc_compare'+add2+'.eps',/encap,/color,/land,font='helvetica'
        colors=[!red,!blue,!green,!forestgreen,!cyan,!magenta,!purple,!orange,!sienna]
;        colors=replicate(!p.color,8)

        filter=['v','b','u','uvw1','uvm2','uvw2','white']
        lam_eff=[5402,4329,3501,2634,2231,2030,3471]*1d-8       ;; cm
        lam_fwhm=[769,975,785,693,498,657]*1d-8                 ;; cm
        fluxfact=[2.614,1.472,1.63,4.00,8.50,6.2,0.37]*1d-16    ;; erg cm-2 s-1 ang-1
        zpt=[17.89,19.11,18.34,17.49,16.82,17.35,20.29]
        c=3d10                                               ;; cm/s
        h=4.135e-15                                          ;; ev*s
        
        ufluxfact=fluxfact[2]        ;;ufluxfact
        fdfact=uvot2jy(1.,'u',/flux) ;; FD_lambda to FD_nu in Jy
        fdfact=fdfact[0]
        nu_u=c/lam_eff[2]

        lowe=c/((3501.+785)*1d-8)*h*1d-3  ;;; keV
        highe=c/((3501.-785)*1d-8)*h*1d-3 ;;; keV
        
        ind=2                      ;; u filter
;        ulumfact=fdfact*1d-23*nu_u[ind]
;        ufd2flux=lam_eff[2]*1d8 ;;; fluxfact not fdfact
;     grbstr.olumfact=lumfact
        grbstr.ofdfact=fdfact      ;;; erg cm-2 s-1 ang-1 to jy
        grbstr.ofluxfact=ufluxfact ;*ufd2flux

;  lumfact=4.08567d-12  ;;; converts to erg cm-2 x D^2 = erg ;;; OR IS IT OPPOSITE /4E-12

        if keyword_set(lum) then plot,[10,1e7],[1d40,1d50],/xlog,/ylog,xtitle='Time since trigger (s) / (1+z)',ytitle='u Luminosity (ergs!U-1!N)',/nodata,/ysty,yticks=10,charsize=charsize
;        if keyword_set(lum) then plot,[10,1e7],[1d37,1d47],/xlog,/ylog,xtitle='Time since trigger (s) / (1+z)',ytitle='u Luminosity Density (erg s!U-1!N '+!tsym.angstrom+'!U-1!N)',/nodata,/ysty,yticks=10,charsize=charsize
        if keyword_set(flux) and not keyword_set(fdens) then plot,[10,1e7],[1d-16,1d-8],/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='u Flux (erg cm!U-2!N s!U-1!N)',/nodata,/ysty,charsize=charsize
        if not keyword_set(lum) and not keyword_set(flux) and not keyword_set(fdens) and not keyword_set(mag) then plot,[10,1e7],[1d-4,1d3],/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='u Count rate (counts s!U-1!N)',/nodata,/ysty,charsize=charsize,ytickformat='loglabels'
;        if keyword_set(fdens) then plot,[10,1e7],[1d-9,1d-1],/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='u Flux Density (Jy)',/nodata,/ysty,charsize=charsize,yticks=8
        if keyword_set(fdens) then plot,[10,1e7],[1d-19,1d-11],/xlog,/ylog,xtitle='Time since trigger (s)',ytitle='u Flux Density (erg cm!U-2!N s!U-1!N '+!tsym.angstrom+'!U-1!N)',/nodata,/ysty,charsize=charsize,yticks=8
        if keyword_set(mag) then plot,[10,1d7],[25,10],xtitle='Time since trigger (s)',ytitle='u Magnitude',/nodata,/ysty,charsize=charsize,/xlog,yrange=[25,10]

        for k=0,2 do begin 
           case k of
              0: begin
;              kgrb='GRB'+grb
                 samp=where(grbstr.who eq 'BAT')
                 kdir='BAT_UVOT_LC/'
;              zfile='Swift_GRB_z.dat'
;           fadd='_u_norm'
                 fadd=''
                 color=!grey50
;                 color=!grey70
              end 
              1: begin
;              kgrb=tgrb
                 samp=where(grbstr.who eq 'GBM')
                 kdir='GBM_UVOT_LC/'
;              zfile='GBM_GRB_z.dat'
;           fadd='_u_norm'
                 fadd=''
;                 color=!grey40
                 color=!grey20
              end 
              2: begin
;              kgrb=latgrbs
                 samp=where(grbstr.who eq 'LAT')
                 kdir='LAT_UVOT_LC/'
;              zfile='LAT_GRB_z.dat'
                 fadd=''
              end 

           endcase
           ufiles=kdir+'norm/'+strtrim(grbstr[samp].grb,2)+'_norm_u.txt'
           ex=0
           for j=0,n_elements(ufiles)-1 do ex=[ex,exist(ufiles[j])]
           ex=ex[1:*]
           wex=where(ex eq 1,nk)
           ex=wex
;ex=where(exist(ufiles),nk)

           if nk gt 0 then begin

;        readcol,zfile,zgrbs,zs,format='(a,f)'
;        for i=0,n_elements(zgrbs)-1 do zgrbs[i]=strtrim(strmid(zgrbs[i],3,7),2)
              
;           nk=n_elements(kgrb)
;           kalpha=dblarr(nk)
;           kalpha_err=dblarr(2,nk)
;           kbeta=dblarr(nk)
;           klum_spec=dblarr(nk)

                                ;          match,kgrb,zgrbs,dm1,dm2
                                ;          short=intarr(nk)
                                ;          x=where(strtrim(dur[dm2],2) eq 'short')
                                ;          short[dm1[x]]=1

              for i=0,nk-1 do begin
                 x=samp[ex[i]]

                 if (q eq 0 and strtrim(grbstr[x].dur,2) eq 'long') or (q eq 1 and strtrim(grbstr[x].dur,2) eq 'short') or q eq 2 then begin

;                 kfile=kdir+'norm/'+kgrb[i]+fadd+'.txt'     
                    kfile=ufiles[ex[i]]
                    if k eq 2 then color=colors[ex[i]]
;                 wz=where(kgrb[i] eq 'GRB'+zgrbs,nwz)      ;;; grab correct z
;                 if nwz gt 0 and exist(kfile) then begin   ;then 
;        if exist(kfile) and  z gt 0 then begin
;                 z=zs[wz[0]]
;                 g=where(kgrb[i] eq 'GRB'+grbs,nw)
                    readcol,kfile,time,terr,rate,raterr,format='(d,d,d,d)',/silent
;                    w0=where(rate gt 0 and rate-raterr gt 0,nw0)
                    w=where(rate gt 0 and rate/raterr gt 2.,nw)
;                    if nw0 gt nw then stop
                    rate=rate[w]
                    raterr=raterr[w]
                    time=time[w]
                    terr=terr[w]
                    gdir=kdir+strtrim(grbstr[x].grb,2)
;                 gdir=kdir+kgrb[i]
                    if not exist(gdir) then spawn,'mkdir '+gdir
                    lcfile=gdir+'/lc_newout.txt'
;                    if not exist(lcfile) then begin 
                    lc=lcout2fits(/empty)
                    lc=replicate(lc,nw)
                    lc.time=time
                    lc.src_rate=rate
                    lc.src_rate_err=raterr
                    lc.tstart=lc.time-terr
                    lc.tstop=lc.time+terr
;;; over estimate of errors
                    lc.exptime=terr*2
                    lc.src_counts=lc.src_rate*lc.exptime
                    lc.tot_back_cts=0.1*lc.exptime
                    lc.pu_corr=1.
                    lc.type=4
;;;
                    write_lc,lc,lcfile
;                    endif else lc=lcout2fits(lcfile)

     ;;; extinction correction
                    calc_dust_corr,grbstr[x].ra,grbstr[x].dec,corr,/noplot,filter=filter,dir='ned/'
                    mmag=zpt[ind]-2.5*alog10(rate)
                    case strtrim(grbstr[x].gal,2) of 
                       'MW': begin
                          mw=1
                          smc=0
                          lmc=0
                       end
                       'SMC': begin
                          mw=0
                          smc=1
                          lmc=0
                       end 
                       'LMC': begin
                          mw=0
                          smc=0
                          lmc=1
                       end
                    endcase 
                    alam=host_extin(lam_eff[ind]*1d4,grbstr[x].av,mw=mw,smc=smc,lmc=lmc)
                    grbstr[x].alam=alam
                    mag2=mmag-corr[ind]-alam
                    mag2err=mag2-((zpt[ind]-2.5*alog10(rate+raterr))-corr[ind]-alam)
;                    mag2err1=((zpt[ind]-2.5*alog10(rate-raterr))-corr[ind])-mag2
                    rate2=10.^((mag2-zpt[ind])/(-2.5))
                    uflux=ufluxfact*rate2
                    ufluxerr=ufluxfact*raterr
                    grbstr[x].hostfact=max(rate2/rate)
                    plotsym,0,1.0,/fill

                    if keyword_set(lum) then begin 
                       z=grbstr[x].z
                       dist=lumdist(z,h0=71,omega_m=0.27)*mpc2cm
                       tf=1./(1.+z)
;                    b=where(xgrb eq kgrb[i])
                       beta=grbstr[x].xbeta
                       xa=grbstr[x].xalpha
;                       grbstr[x].obeta=beta
                       help,time,uflux
                       w=where(time gt 1e4)
                       lcfitfile=gdir+'/lc_fit_out_idl_int8.dat'
                       if not exist(lcfitfile) then lcfitfile=gdir+'/lc_fit_out_idl_int7.dat'
                       if exist(lcfitfile) then begin 
                          read_lcfit,lcfitfile,pname,p,perr,np=np

;                       fitfile=gdir+'/lc_fit_out_idl_int7.dat'
;                       if exist(fitfile) then begin 
;                          read_lcfit,fitfile,pname,p,perr
;                          np=n_elements(p)-1
                          if np eq 2 then begin
                             xx=1
                             f=pow(t_spec/tf,p)
                             fo=pow(t_spec,p)
                             f11=pow(t11/tf,p)
                             fo11=pow(t11,p)
                             grbstr[x].otype='SPL'
                          endif 
                          if np eq 4 then begin
                             xx=3
                             f=bknpow(t_spec/tf,p)
                             fo=bknpow(t_spec,p)
                             f11=bknpow(t11/tf,p)
                             fo11=bknpow(t11,p)
                             if p[1] gt p[3] then grbstr[x].otype='I-II'
                             if p[1] lt p[3] then grbstr[x].otype='II-III'
                          endif 
                          if np eq 6 then begin
                             if p[1] gt p[3] then xx=5 else xx=3
                             f=bkn2pow(t_spec/tf,p)
                             fo=bkn2pow(t_spec,p)
                             f11=bkn2pow(t11/tf,p)
                             fo11=bkn2pow(t11,p)
                             if p[1] gt p[3] then grbstr[x].otype='I-II-III'
                             if p[1] lt p[3] then grbstr[x].otype='II-III-IV'
                          endif 
                          if np eq 8 then begin
                             xx=5
                             f=bkn3pow(t_spec/tf,p)
                             fo=bkn3pow(t_spec,p)
                             f11=bkn3pow(t11/tf,p)
                             fo11=bkn3pow(t11,p)
                             grbstr[x].otype='I-II-III-IV'
                          endif 
                          ;;; subtract off constant component
                          const=p[np]
;                          f2=f-const
;                       print,const,f2,f
;                          f=f2
                          ;;; not sure if i should subtract here or
                          ;;; what?
                          ;;; negative flux is not useful

;                       stop
                          alp=p[xx]
;                       kalpha[i]=p[x]
                          which_alpha,t_spec/tf,p,aspec,aa,np=np
                          alpha=aspec
                          which_alpha,t_spec,p,aospec,aa,np=np
                          aoerr=perr[*,aa]
                          which_alpha,t11/tf,p,a11spec,aa,np=np
                          a11err=perr[*,aa]
                          which_alpha,t11,p,ao11spec,aa,np=np
                          ao11err=perr[*,aa]

                          grbstr[x].oalpha=alpha ;p[xx]
;                       kalpha_err[*,i]=perr[*,x]
                          grbstr[x].oalpha_err=perr[*,aa] ;perr[*,xx]
;                          if xa-alp gt 0.4 then grbstr[x].obeta=beta-0.5 ;;; different spectral segment????
;                          grbstr[x].obeta_err=grbstr[x].xbeta_err
;                 print,kalpha[i]
                          last_alpha=p[np-1]   
                          if last_alpha gt 1.5 and np gt 2 then begin 
                             grbstr[x].otbreak=p[np-2]
                             grbstr[x].ojb=1
                          endif else begin 
                             grbstr[x].otbreak=max(lc.time)
                             grbstr[x].ojb=0
                          endelse 
                          tmax=max(lc.tstop)
                          if grbstr[x].obeta eq 0 then stop
                          fdfact1=flux2jy(1.,grbstr[x].obeta+1.,eeff=nu_u*h*1d-3,low=lowe,high=highe)
                          ufd2flux=fdfact/fdfact1
                          lumfact=ufd2flux*4.*!pi*dist^2*(1.+z)^(grbstr[x].obeta-grbstr[x].oalpha-1.)
                          grbstr[x].olumfact=lumfact
                          print,x,lumfact
                          lumo=uflux*lumfact
                          lumerr=ufluxerr*lumfact
;if strtrim(grbstr[x].grb,2) eq 'GRB060607A' then stop

                          oploterror,time*tf,lumo,terr*tf,lumerr,/nohat,errcolor=color,psym=8,color=color
                          lumfact_err=sqrt((max(grbstr[x].oalpha_err)/grbstr[x].oalpha)^2+(max(grbstr[x].obeta_err)/grbstr[x].obeta)^2)*lumfact

                          lumfacto=ufd2flux*4.*!pi*dist^2*(1.+z)^(grbstr[x].obeta-aospec-1.)
                          lumfacto_err=sqrt((max(aoerr)/aospec)^2+(max(grbstr[x].obeta_err)/grbstr[x].obeta)^2)*lumfacto
                          lumfact11=ufd2flux*4.*!pi*dist^2*(1.+z)^(grbstr[x].obeta-a11spec-1.)
                          lumfact11_err=sqrt((max(a11err)/a11spec)^2+(max(grbstr[x].obeta_err)/grbstr[x].obeta)^2)*lumfact11
                          lumfact11o=ufd2flux*4.*!pi*dist^2*(1.+z)^(grbstr[x].obeta-ao11spec-1.)
                          lumfact11o_err=sqrt((max(ao11err)/ao11spec)^2+(max(grbstr[x].obeta_err)/grbstr[x].obeta)^2)*lumfact11o

                          if tmax gt t_spec/tf then begin 
                             grbstr[x].olum_1day=f*lumfact*ufluxfact
                             grbstr[x].olum_1day_err=f*lumfact_err*ufluxfact
                          endif 
                          if tmax gt t_spec then begin 
                             grbstr[x].olum_1day_obs=fo*lumfacto*ufluxfact
                             grbstr[x].olum_1day_obs_err=fo*lumfacto_err*ufluxfact
                             grbstr[x].oflux_11hr_obs=fo*ufluxfact*ufd2flux
                             grbstr[x].oflux_11hr_obs_err=fo*ufluxfact*lumfacto_err/lumfacto*ufd2flux
                          endif 
                          if tmax gt t11/tf then begin 
                             grbstr[x].olum_11hr=f11*lumfact11*ufluxfact
                             grbstr[x].olum_11hr_err=f11*lumfact11_err*ufluxfact
                          endif 
                          if tmax gt t11 then begin 
                             grbstr[x].olum_11hr_obs=fo11*lumfact11o*ufluxfact
                             grbstr[x].olum_11hr_obs_err=fo11*lumfact11o_err*ufluxfact
                             grbstr[x].oflux_11hr_obs=fo11*ufluxfact*ufd2flux
                             grbstr[x].oflux_11hr_obs_err=fo11*ufluxfact*lumfact11o_err/lumfact11o*ufd2flux
                          endif 
                          

;                       whigh=where(time gt 1e4)
;                       if max(lum[whigh] gt 1d46) then stop


                          print,grbstr[x].grb,4.*!pi*dist^2*(1.+z)^(grbstr[x].obeta-grbstr[x].oalpha-1.),z,dist,grbstr[x].obeta,grbstr[x].oalpha
                       endif 
                    endif else begin
                          ;;;; flux
                       lcfitfile=gdir+'/lc_fit_out_idl_int8.dat'
                       if not exist(lcfitfile) then lcfitfile=gdir+'/lc_fit_out_idl_int7.dat'
                       if exist(lcfitfile) then begin 
                          read_lcfit,lcfitfile,pname,p,perr,np=np

;                       fitfile=gdir+'/lc_fit_out_idl_int7.dat'
                       
;                       if exist(fitfile) then begin 
;                          read_lcfit,fitfile,pname,p,perr
;                          np=n_elements(p)-1
                          if np eq 2 then begin 
                             fo=pow(t_spec,p)
                             fo11=pow(t11,p)
                          endif 
                          if np eq 4 then begin 
                             fo=bknpow(t_spec,p)
                             fo11=bknpow(t11,p)
                          endif 
                          if np eq 6 then begin
                             fo=bkn2pow(t_spec,p)
                             fo11=bkn2pow(t11,p)
                          endif 
                          if np eq 8 then begin 
                             fo=bkn3pow(t_spec,p)
                             fo11=bkn3pow(t11,p)
                          endif 
                          err=sqrt((max(grbstr[x].oalpha_err)/grbstr[x].oalpha)^2+(max(grbstr[x].obeta_err)/grbstr[x].obeta)^2)
                          fdfact1=flux2jy(1.,grbstr[x].obeta+1.,eeff=nu_u*h*1d-3,low=lowe,high=highe)
                          ufd2flux=fdfact/fdfact1

                          grbstr[x].oflux_1day_obs=fo*ufluxfact*ufd2flux
                          grbstr[x].oflux_1day_obs_err=fo*ufluxfact*err*ufd2flux
                          grbstr[x].oflux_11hr_obs=fo11*ufluxfact*ufd2flux
                          grbstr[x].oflux_11hr_obs_err=fo11*ufluxfact*err*ufd2flux
                       endif

                       if keyword_set(flux) and not keyword_set(fdens) then $
                          oploterror,time,uflux*ufd2flux,terr,ufluxerr*ufd2flux,/nohat,errcolor=color,psym=8,color=color
                       if not keyword_set(flux) and not keyword_set(fdens) then $
                          oploterror,time,rate2,terr,raterr,/nohat,errcolor=color,psym=8,color=color
                       if keyword_set(fdens) then $
                          oploterror,time,uflux,terr,ufluxerr,/nohat,errcolor=color,psym=8,color=color
;                          oploterror,time,uflux*fdfact,terr,ufluxerr*fdfact,/nohat,errcolor=color,psym=8,color=color
                       if keyword_set(mag) then $
                          oploterror,time,mag2,terr,mag2err,/nohat,errcolor=color,psym=8,color=color
;              print,rate2,raterr
;                    kalpha[i]=1
                       grbstr[x].oalpha=1.
                    endelse 
                 endif  
;           endif   
              endfor 
;     if keyword_set(lum) then begin 
;;            case k of 
;;               0: begin
;;                  gs=where(kalpha ne 0)
;;                  osalphas=kalpha[gs]
;;                  osalphas_err=kalpha_err[*,gs]
;;                  osbetas=kbeta[gs]
;;                  oslum_spec=klum_spec[gs]
;;                  osgrbs=kgrb[gs]
;;               end
;;               1: begin
;;                  gs=where(kalpha ne 0)
;;                  ogalphas=kalpha[gs]
;;                  ogalphas_err=kalpha_err[*,gs]
;;                  ogbetas=kbeta[gs]
;;                  oglum_spec=klum_spec[gs]
;;                  oggrbs=kgrb[gs]
;;               end
;;               2: begin
;;                  gs=where(kalpha ne 0)
;;                  olalphas=kalpha[gs]
;;                  olalphas_err=kalpha_err[*,gs]
;;                  olbetas=kbeta[gs]
;;                  ollum_spec=klum_spec[gs]
;;                  olgrbs=kgrb[gs]
;;               end
;;            endcase
;           if q eq 0 then s=where(short eq 0) else s=where(short[gs] eq 1)
           endif 
        endfor  
;  endfor 
        case q of 
           0: s=where(strtrim(grbstr[samp].dur,2) eq 'long' and ((grbstr[samp].olum_1day ne 0 or grbstr[samp].olum_1day_obs ne 0) or (grbstr[samp].oflux_1day_obs ne 0)))
           1: s=where(strtrim(grbstr[samp].dur,2) eq 'short' and ((grbstr[samp].olum_1day ne 0 or grbstr[samp].olum_1day_obs ne 0) or (grbstr[samp].oflux_1day_obs ne 0)))
           2: s=where(grbstr[samp].olum_1day ne 0 or grbstr[samp].olum_1day_obs ne 0 or grbstr[samp].oflux_1day_obs ne 0)
        endcase 

        slen=strlen(latgrbs[s])
        wslen=where(slen eq 9)
        strl=strarr(n_elements(s))
        strl[wslen]='  '
        if q eq 1 then strl=''
        legend,[grbstr[samp[s]].grb+strl,'BAT GRBs','GBM GRBs'],textcolor=[colors[s],!grey50,!grey20],box=0,/top,/right
        if q le 1 then begin 
           if q eq 0 then leg='Long'
           if q eq 1 then leg='Short'
           legend,leg,/bottom,/left,box=0
        endif 
;        legend,['LAT GRBs','Swift GRBs','GBM/BAT GRBs'],textcolor=[!p.color,!grey70,!grey40],box=0,/top,/right
        if keyword_set(ps) then endplot
     endfor 
     if keyword_set(lum) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',/create
     if keyword_set(flux) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',/create
     if not keyword_set(lum) then stop
     if not keyword_set(ps) then stop
  endif 
;  dont_match,osalphas,olalphas,m1,m2
;  dont_match,osalphas,ogalphas,gm1,gm2
  if not keyword_set(flux) then begin 
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.olum_1day ne 0 and grbstr.oalpha ne 0 and grbstr.obeta ne 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.olum_1day ne 0 and grbstr.oalpha ne 0 and grbstr.obeta ne 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.olum_1day ne 0 and grbstr.oalpha ne 0 and grbstr.obeta ne 0)

     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
     
     begplot,name='~/Fermi/Swift_pop_study/oalpha_lum_corr.eps',/land,/color,/encap,font='helvetica'

     arange=[0,3]
     lumrange=10d^[40,47]
     plotsym,0,1,/fill
     scatter_hist,grbstr.oalpha,grbstr.olum_1day,grbstr.oalpha_err,grbstr.olum_1day_err,bin1=0.1,bin2=0.2,xrange=arange,yrange=lumrange,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle=!tsym.alpha+'!Lo!N',ytitle='L!Lo,1 day!N (erg s!U-1!N)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=-1,w4=lgrbs[ll],w5=-1,leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/top,/right,charsize=1.

;     legend,['long','short'],box=0,/bottom,/left,psym=[8,2]

     endplot

     ;;; oalphas_obetas scatter_hist

     sgrbs=where(grbstr.who eq 'BAT' and grbstr.oalpha ne 0 and grbstr.obeta ne 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.oalpha ne 0 and grbstr.obeta ne 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.oalpha ne 0 and grbstr.obeta ne 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
     
     kstwop,grbstr[sgrbs[ls]].oalpha,grbstr[ggrbs[lg]].oalpha,d,gprob
     kstwop,grbstr[sgrbs[ls]].oalpha,grbstr[lgrbs[ll]].oalpha,d,lprob
     kstwop,grbstr[ggrbs[lg]].oalpha,grbstr[lgrbs[ll]].oalpha,d,glprob
     ks[0].oalpha_bg=gprob
     ks[0].oalpha_bl=lprob
     ks[0].oalpha_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].oalpha,grbstr[ggrbs[sg]].oalpha,d,gprob
;     kstwop,grbstr[sgrbs[ss]].oalpha,grbstr[lgrbs[sl]].oalpha,d,lprob
;     kstwop,grbstr[ggrbs[sg]].oalpha,grbstr[lgrbs[sl]].oalpha,d,glprob
;     ks[1].oalpha_bg=gprob
;     ks[1].oalpha_bl=lprob
;     ks[1].oalpha_gl=glprob    

     kstwop,grbstr[sgrbs[ls]].obeta,grbstr[ggrbs[lg]].obeta,d,gprob
     kstwop,grbstr[sgrbs[ls]].obeta,grbstr[lgrbs[ll]].obeta,d,lprob
     kstwop,grbstr[ggrbs[lg]].obeta,grbstr[lgrbs[ll]].obeta,d,glprob
     ks[0].obeta_bg=gprob
     ks[0].obeta_bl=lprob
     ks[0].obeta_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].obeta,grbstr[ggrbs[sg]].obeta,d,gprob
;     kstwop,grbstr[sgrbs[ss]].obeta,grbstr[lgrbs[sl]].obeta,d,lprob
;     kstwop,grbstr[ggrbs[sg]].obeta,grbstr[lgrbs[sl]].obeta,d,glprob
;     ks[1].obeta_bg=gprob
;     ks[1].obeta_bl=lprob
;     ks[1].obeta_gl=glprob    

     plotsym,0,1,/fill
     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/oalpha_obeta.eps',/color,/encap,/land,font='helvetica' else erase
     scatter_hist,grbstr.oalpha,grbstr.obeta,grbstr.oalpha_err,grbstr.obeta_err,bin1=0.1,bin2=0.1,xrange=[0,3],yrange=[0,3],psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle=!tsym.alpha+'!Lo!N',ytitle=!tsym.beta+'!Lo!N',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],charsize=1.

     if keyword_set(ps) then endplot
     stop


     olat=[0,2,4,5]
     begplot,name='~/Fermi/Swift_pop_study/uvot_alphas.eps',/color,/land,font='helvetica',/encap
     plot,[0,2],[0,4],/nodata,xtitle=!tsym.alpha+'!Lo!N (t=1 day)',ytitle='N',charsize=2.
     plothist,grbstr[sgrbs].oalpha,bin=0.1,/over,/fill,fcolor=!grey50,color=!grey50
     plothist,grbstr[ggrbs].oalpha,bin=0.1,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
     plothist,grbstr[lgrbs].oalpha,bin=0.1,/over,color=!red,line=2
     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]

;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4),'P!LKS,LAT!N = '+sigfig(olprob,2)],/top,/left,box=0,charsize=1.5,textcolor=[!grey50,!red,!blue]
     oplot,[0,100],[0,0]
     endplot
  ;;; betas dist plot
;  dont_match,osbetas,olbetas,m1,m2
;  dont_match,osbetas,ogbetas,gm1,gm2
     begplot,name='~/Fermi/Swift_pop_study/uvot_betas.ps',/color,/land,font='helvetica'
     plot,[0,2],[0,5],/nodata,xtitle=!tsym.beta+'!Lo!N',ytitle='N',charsize=2.
     plothist,grbstr[sgrbs].obeta,bin=0.1,/over,/fill,fcolor=!grey50,color=!grey50
     plothist,grbstr[ggrbs].obeta,bin=0.1,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
     plothist,grbstr[lgrbs].obeta,bin=0.1,/over,color=!red,line=2
     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]

;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4),'P!LKS,LAT!N = '+sigfig(olprob,2)],/top,/left,box=0,charsize=1.5,textcolor=[!grey50,!red,!blue]
;  legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4)],/top,/left,box=0,charsize=1.5 
     oplot,[0,100],[0,0]
     endplot

  ;;; lum as t_spec
     xsgrbs=where(grbstr.who eq 'BAT' and grbstr.xlum_1day ne 0)
     xggrbs=where(grbstr.who eq 'GBM' and grbstr.xlum_1day ne 0)
     xlgrbs=where(grbstr.who eq 'LAT' and grbstr.xlum_1day ne 0)
     osgrbs=where(grbstr.who eq 'BAT' and grbstr.olum_1day ne 0)
     oggrbs=where(grbstr.who eq 'GBM' and grbstr.olum_1day ne 0)
     olgrbs=where(grbstr.who eq 'LAT' and grbstr.olum_1day ne 0)

     xsgrbs11=where(grbstr.who eq 'BAT' and grbstr.xlum_11hr ne 0)
     xggrbs11=where(grbstr.who eq 'GBM' and grbstr.xlum_11hr ne 0)
     xlgrbs11=where(grbstr.who eq 'LAT' and grbstr.xlum_11hr ne 0)
     osgrbs11=where(grbstr.who eq 'BAT' and grbstr.olum_11hr ne 0)
     oggrbs11=where(grbstr.who eq 'GBM' and grbstr.olum_11hr ne 0)
     olgrbs11=where(grbstr.who eq 'LAT' and grbstr.olum_11hr ne 0)

     ls=where(strtrim(grbstr[xsgrbs].dur,2) eq 'long')
     ss=where(strtrim(grbstr[xsgrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[xggrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[xggrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[xlgrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[xlgrbs].dur,2) eq 'short')

     ols=where(strtrim(grbstr[osgrbs].dur,2) eq 'long')
     oss=where(strtrim(grbstr[osgrbs].dur,2) eq 'short')
     olg=where(strtrim(grbstr[oggrbs].dur,2) eq 'long')
     osg=where(strtrim(grbstr[oggrbs].dur,2) eq 'short')
     oll=where(strtrim(grbstr[olgrbs].dur,2) eq 'long')
     osl=where(strtrim(grbstr[olgrbs].dur,2) eq 'short')

     ls11=where(strtrim(grbstr[xsgrbs11].dur,2) eq 'long')
     ss11=where(strtrim(grbstr[xsgrbs11].dur,2) eq 'short')
     lg11=where(strtrim(grbstr[xggrbs11].dur,2) eq 'long')
     sg11=where(strtrim(grbstr[xggrbs11].dur,2) eq 'short')
     ll11=where(strtrim(grbstr[xlgrbs11].dur,2) eq 'long')
     sl11=where(strtrim(grbstr[xlgrbs11].dur,2) eq 'short')

     ols11=where(strtrim(grbstr[osgrbs11].dur,2) eq 'long')
     oss11=where(strtrim(grbstr[osgrbs11].dur,2) eq 'short')
     olg11=where(strtrim(grbstr[oggrbs11].dur,2) eq 'long')
     osg11=where(strtrim(grbstr[oggrbs11].dur,2) eq 'short')
     oll11=where(strtrim(grbstr[olgrbs11].dur,2) eq 'long')
     osl11=where(strtrim(grbstr[olgrbs11].dur,2) eq 'short')

     kstwop,grbstr[xsgrbs[ls]].xlum_1day,grbstr[xggrbs[lg]].xlum_1day,d,gprob
     kstwop,grbstr[xsgrbs[ls]].xlum_1day,grbstr[xlgrbs[ll]].xlum_1day,d,lprob
     kstwop,grbstr[xggrbs[lg]].xlum_1day,grbstr[xlgrbs[ll]].xlum_1day,d,glprob
     ks[0].xlum_1day_bg=gprob
     ks[0].xlum_1day_bl=lprob
     ks[0].xlum_1day_gl=glprob
;     kstwop,grbstr[xsgrbs[ss]].xlum_1day,grbstr[xggrbs[sg]].xlum_1day,d,gprob
;     kstwop,grbstr[xsgrbs[ss]].xlum_1day,grbstr[xlgrbs[sl]].xlum_1day,d,lprob
;     kstwop,grbstr[xggrbs[sg]].xlum_1day,grbstr[xlgrbs[sl]].xlum_1day,d,glprob
;     ks[1].xlum_1day_bg=gprob
;     ks[1].xlum_1day_bl=lprob
;     ks[1].xlum_1day_gl=glprob
     kstwop,grbstr[osgrbs[ols]].olum_1day,grbstr[oggrbs[olg]].olum_1day,d,gprob
     kstwop,grbstr[osgrbs[ols]].olum_1day,grbstr[olgrbs[oll]].olum_1day,d,lprob
     kstwop,grbstr[oggrbs[olg]].olum_1day,grbstr[olgrbs[oll]].olum_1day,d,glprob
     ks[0].olum_1day_bg=gprob
     ks[0].olum_1day_bl=lprob
     ks[0].olum_1day_gl=glprob
;     kstwop,grbstr[osgrbs[oss]].olum_1day,grbstr[oggrbs[osg]].olum_1day,d,gprob
;     kstwop,grbstr[osgrbs[oss]].olum_1day,grbstr[olgrbs[osl]].olum_1day,d,lprob
;     kstwop,grbstr[oggrbs[osg]].olum_1day,grbstr[olgrbs[osl]].olum_1day,d,glprob
;     ks[1].olum_1day_bg=gprob
;     ks[1].olum_1day_bl=lprob
;     ks[1].olum_1day_gl=glprob
     kstwop,grbstr[xsgrbs11[ls11]].xlum_11hr,grbstr[xggrbs11[lg11]].xlum_11hr,d,gprob
     kstwop,grbstr[xsgrbs11[ls11]].xlum_11hr,grbstr[xlgrbs11[ll11]].xlum_11hr,d,lprob
     kstwop,grbstr[xggrbs11[lg11]].xlum_11hr,grbstr[xlgrbs11[ll11]].xlum_11hr,d,glprob
     ks[0].xlum_11hr_bg=gprob
     ks[0].xlum_11hr_bl=lprob
     ks[0].xlum_11hr_gl=glprob
;     kstwop,grbstr[xsgrbs[ss]].xlum_11hr,grbstr[xggrbs[sg]].xlum_11hr,d,gprob
;     kstwop,grbstr[xsgrbs[ss]].xlum_11hr,grbstr[xlgrbs[sl]].xlum_11hr,d,lprob
;     kstwop,grbstr[xggrbs[sg]].xlum_11hr,grbstr[xlgrbs[sl]].xlum_11hr,d,glprob
;     ks[1].xlum_11hr_bg=gprob
;     ks[1].xlum_11hr_bl=lprob
;     ks[1].xlum_11hr_gl=glprob
     kstwop,grbstr[osgrbs11[ols11]].olum_11hr,grbstr[oggrbs11[olg11]].olum_11hr,d,gprob
     kstwop,grbstr[osgrbs11[ols11]].olum_11hr,grbstr[olgrbs11[oll11]].olum_11hr,d,lprob
     kstwop,grbstr[oggrbs11[olg11]].olum_11hr,grbstr[olgrbs11[oll11]].olum_11hr,d,glprob
     ks[0].olum_11hr_bg=gprob
     ks[0].olum_11hr_bl=lprob
     ks[0].olum_11hr_gl=glprob
;     kstwop,grbstr[osgrbs[oss]].olum_11hr,grbstr[oggrbs[osg]].olum_11hr,d,gprob
;     kstwop,grbstr[osgrbs[oss]].olum_11hr,grbstr[olgrbs[osl]].olum_11hr,d,lprob
;     kstwop,grbstr[oggrbs[osg]].olum_11hr,grbstr[olgrbs[osl]].olum_11hr,d,glprob
;     ks[1].olum_11hr_bg=gprob
;     ks[1].olum_11hr_bl=lprob
;     ks[1].olum_11hr_gl=glprob

     for i=0,1 do begin 
        case i of 
           0: begin
              add='long_'
              s=ls
              g=lg
              l=ll
              os=ols
              og=olg
              ol=oll
              s11=ls11
              g11=lg11
              l11=ll11
              os11=ols11
              og11=olg11
              ol11=oll11
              yrange=[0,20]
              bin=0.2
              leg='Long'
           end
           1: begin 
              add='short_'
              s=ss
              g=sg
              l=sl
              os=oss
              og=osg
              ol=osl
              s11=ss11
              g11=sg11
              l11=sl11
              os11=oss11
              og11=osg11
              ol11=osl11
              yrange=[0,5]
              bin=0.5
              leg='Short'
           end 
        endcase
        begplot,name='~/Fermi/Swift_pop_study/'+add+'lum_spec.eps',/color,/land,font='helvetica',/encap
        multiplot2,[2,2],/init
        lrange=[40,48]
        ;; x - 1day
        multiplot2
        plot,lrange,yrange,/nodata,ytitle='N',/xsty,/ysty,yrange=yrange,charsize=1.5 ;,xtitle='log L!Lx!N (t=1 day rest frame) (erg s!U-1!N)'
        
        plothist,[0,alog10(grbstr[xsgrbs[s]].xlum_1day)],x,y,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
        if n_elements(x) gt 2 then g1=gaussfit(x,y,a1,nterms=3)
;        oplot,x,g1,color=!green
        plothist,[0,alog10(grbstr[xggrbs[g]].xlum_1day)],x,y,bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
        if n_elements(x) gt 2 then g2=gaussfit(x,y,a2,nterms=3)
;        oplot,x,g2,color=!green
        plothist,[0,alog10(grbstr[xlgrbs[l]].xlum_1day)],x,y,bin=bin,/over,color=!red,line=2
        if n_elements(x) gt 2 then g3=gaussfit(x,y,a3,nterms=3)
;        oplot,x,g3,color=!green
        print,[[a1],[a2],[a3]]
        print,'x - 1day FWHM = ',2*sqrt(2*alog(2.))*[a1[2],a2[2],a3[2]]

;        legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]
        legend,leg,/top,/right,box=0
        oplot,[0,100],[0,0]
        legend,['1 day','X-ray'],box=0,/top,/left
        ;; o - 1day
        multiplot2
        plot,lrange,yrange,/nodata,/xsty,/ysty,yrange=yrange,charsize=1.5
        plothist,[0,alog10(grbstr[osgrbs[os]].olum_1day)],x,y,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
        if n_elements(x) gt 2 then g1=gaussfit(x,y,a1,nterms=3)
;        oplot,x,g1,color=!green
        if og[0] ne -1 then plothist,[0,alog10(grbstr[oggrbs[og]].olum_1day)],x,y,bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
        if n_elements(x) gt 2 then g2=gaussfit(x,y,a2,nterms=3)
;        oplot,x,g2,color=!green
        if og[0] ne -1 then plothist,[0,alog10(grbstr[olgrbs[ol]].olum_1day)],x,y,bin=bin,/over,color=!red,line=2
        if n_elements(x) gt 2 then g3=gaussfit(x,y,a3,nterms=3)
;        oplot,x,g3,color=!green
        print,[[a1],[a2],[a3]]
        print,'o - 1day FWHM = ',2*sqrt(2*alog(2.))*[a1[2],a2[2],a3[2]]

        legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]
        legend,['1 day','Optical'],box=0,/top,/left
        oplot,[0,100],[0,0]

        ;; x - 11hr
        multiplot2
        plot,lrange,yrange,/nodata,/xsty,/ysty,yrange=yrange,charsize=1.5,xtitle='log L!Lx!N (erg s!U-1!N)',ytitle='N'
        w=where(grbstr[xsgrbs11[s11]].xlum_11hr gt 0)
        plothist,[0,alog10(grbstr[xsgrbs11[s11[w]]].xlum_11hr)],x,y,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
        if n_elements(x) gt 2 then g1=gaussfit(x,y,a1,nterms=3)
;        oplot,x,g1,color=!green
        w=where(grbstr[xggrbs11[g11]].xlum_11hr gt 0)
        plothist,[0,alog10(grbstr[xggrbs11[g11[w]]].xlum_11hr)],x,y,bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
        if n_elements(x) gt 2 then g2=gaussfit(x,y,a2,nterms=3)
;        oplot,x,g2,color=!green
        w=where(grbstr[xlgrbs11[l11]].xlum_11hr gt 0)
        plothist,[0,alog10(grbstr[xlgrbs11[l11[w]]].xlum_11hr)],x,y,bin=bin,/over,color=!red,line=2
        if n_elements(x) gt 2 then g3=gaussfit(x,y,a3,nterms=3)
;        oplot,x,g3,color=!green
        print,[[a1],[a2],[a3]]
        print,'x - 11hr FWHM = ',2*sqrt(2*alog(2.))*[a1[2],a2[2],a3[2]]

        oplot,[20,60],[0,0]
        legend,['11 hour','X-ray'],box=0,/top,/left
        ;; o - 11 hr
        multiplot2
        plot,lrange,yrange,/nodata,/xsty,/ysty,yrange=yrange,xtitle='log L!Lo!N (erg s!U-1!N)',charsize=1.5
        w=where(grbstr[osgrbs11[os11]].olum_11hr gt 0)
        plothist,[0,alog10(grbstr[osgrbs11[s11[w]]].olum_11hr)],x,y,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
        if n_elements(x) gt 2 then g1=gaussfit(x,y,a1,nterms=3)
;        oplot,x,g1,color=!green
        w=where(grbstr[oggrbs11[og11]].olum_11hr gt 0)
        plothist,[0,alog10(grbstr[oggrbs11[g11[w]]].olum_11hr)],x,y,bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
        if n_elements(x) gt 2 then g2=gaussfit(x,y,a2,nterms=3)
;        oplot,x,g2,color=!green
        w=where(grbstr[olgrbs11[ol11]].olum_11hr gt 0)
        plothist,[0,alog10(grbstr[olgrbs11[l11[w]]].olum_11hr)],x,y,bin=bin,/over,color=!red,line=2
        if n_elements(x) gt 2 then g3=gaussfit(x,y,a3,nterms=3)
;        oplot,x,g3,color=!green
        print,[[a1],[a2],[a3]]
        print,'o - 11hr FWHM = ',2*sqrt(2*alog(2.))*[a1[2],a2[2],a3[2]]

        oplot,[20,60],[0,0]
        legend,['11 hour','Optical'],box=0,/top,/left
        endplot
        multiplot,/reset,/default
     endfor 
  endif 
  stop

;;      begplot,name='~/Fermi/Swift_pop_study/uvot_lum_spec.eps',/color,/land,font='helvetica',/encap
;;      plot,[41,46],[0,10],/nodata,xtitle='log L!Lopt!N (t=1 day rest frame) (erg)',ytitle='N',/xsty,/ysty,charsize=2.
;;      plothist,alog10(grbstr[sgrbs].olum_1day),bin=0.2,/over,/fill,fcolor=!grey50,color=!grey50
;;      plothist,alog10(grbstr[ggrbs].olum_1day),bin=0.2,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
;;      plothist,alog10(grbstr[lgrbs].olum_1day),bin=0.2,/over,color=!red,line=2
;;      kstwop,grbstr[sgrbs].olum_1day,grbstr[lgrbs].olum_1day,d,lprob
;;      kstwop,grbstr[sgrbs].olum_1day,grbstr[ggrbs].olum_1day,d,gprob
;;      kstwop,grbstr[sgrbs].olum_1day,grbstr[lgrbs[olat]].olum_1day,d,olprob
;;      legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4),'P!LKS,LAT!N = '+sigfig(olprob,2)],/top,/left,box=0,charsize=1.5,textcolor=[!grey50,!red,!blue]
;;      legend,['BAT','GBM/BAT','LAT/GBM'],/top,/right,box=0,textcolor=[!grey50,!blue,!red]
;; ;  legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4)],/top,/left,box=0,charsize=1.5
;;      oplot,[0,100],[0,0]
;;      endplot
;;      stop
  skipuvots:
  skip2energ:
  plotsym,0,1,/fill
  if keyword_set(energ) then grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',1)

  ;;; COMPARE REDSHIFTS
  if keyword_set(lum) then begin 
     sgrbs=where(grbstr.who eq 'BAT')
     ggrbs=where(grbstr.who eq 'GBM')
     lgrbs=where(grbstr.who eq 'LAT')
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short',nss)
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long',nls)
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short',nsg)
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long',nlg)
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long',nll)

     !x.margin=[12,2]
     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/redshift_dist.eps',/color,/encap,/land,font='helvetica'
     bin=0.2
     calc_cdf,grbstr[sgrbs[ls]].z,xs,ps ;,bin=bin
     calc_cdf,grbstr[ggrbs[lg]].z,xg,pg ;,bin=bin
     calc_cdf,grbstr[lgrbs[ll]].z,xl,pl ;,bin=bin

     xrange=[0,10]
;     multiplot2,[1,2],/init
;     multiplot2
     n=nls+nlg+nll+0d
     plot,xrange,[0,1],/nodata,xrange=xrange,/xsty,xtitle='z',ytitle='P',charsize=2.
     plot_cdf,xs,ps,/over,color=!grey50
     plot_cdf,xg,pg,/over,color=!blue,line=2
     plot_cdf,xl,pl,/over,color=!red,line=1
;     oplot,[0,xs],[0,ps],psym=10,color=!grey50
;     oplot,[0,xg],[0,pg],psym=10,color=!blue,line=2
;     oplot,[0,xl],[0,pl],psym=10,color=!red,line=1

;;      yrange=[0,15]
;;      plot,[0,9],yrange,/nodata,ytitle='N'
;;      plothist,grbstr[sgrbs[ls]].z,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
;;      plothist,grbstr[ggrbs[lg]].z,bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
;;      plothist,grbstr[lgrbs[ll]].z,bin=bin,color=!red,/over,line=2
;;      oplot,[0,10],[0,0]
;;      oplot,[0,0],yrange
     kstwop,grbstr[sgrbs[ls]].z,grbstr[ggrbs[lg]].z,d,gprob
     kstwop,grbstr[sgrbs[ls]].z,grbstr[lgrbs[ll]].z,d,lprob
     kstwop,grbstr[ggrbs[lg]].z,grbstr[lgrbs[ll]].z,d,glprob
     ks[0].z_bg=gprob
     ks[0].z_bl=lprob
     ks[0].z_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].z,grbstr[ggrbs[sg]].z,d,gprob
;     kstwop,grbstr[sgrbs[ss]].z,grbstr[lgrbs[sl]].z,d,lprob
;     kstwop,grbstr[ggrbs[sg]].z,grbstr[lgrbs[sl]].z,d,glprob
;     ks[1].z_bg=gprob
;     ks[1].z_bl=lprob
;     ks[1].z_gl=glprob

     legend,['BAT (long)','GBM (long)','LAT (long)','BAT (short)'],/bottom,/right,box=0,textcolor=[!grey50,!blue,!red,!grey50],line=[0,2,1,3],color=[!grey50,!blue,!red,!grey50]
;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4)],/top,/left,box=0,charsize=1.5
     
     bin=0.2
     calc_cdf,grbstr[sgrbs[ss]].z,xs,ps ;,bin=bin
     calc_cdf,grbstr[ggrbs[sg]].z,xg,pg ;,bin=bin
     calc_cdf,grbstr[lgrbs[sl]].z,xl,pl ;,bin=bin
;     multiplot2
;     plot,[-bin/2.,xs],[0,ps],psym=10,xrange=xrange,/xsty
;     oplot,[0,xs],[0,ps],psym=10,color=!grey50,line=3
     plot_cdf,xs,ps,/over,color=!grey50,line=3
;     oplot,[-bin/2.,xg],[0,pg],psym=10,color=!blue,line=2
;     oplot,[-bin/2.,xl],[0,pl],psym=10,color=!red,line=1

     ;; yrange=[0,5]
;;      multiplot2
;;      plot,[0,9],yrange,/nodata,xtitle='z',ytitle='N'
;;      plothist,grbstr[sgrbs[ss]].z,bin=bin,/over,/fill,fcolor=!grey50,color=!grey50
;;      plothist,[-1,grbstr[ggrbs[sg]].z],bin=bin,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
;;      plothist,[-1,grbstr[lgrbs[sl]].z],bin=bin,color=!red,/over,line=2
;;      w=where(grbstr[sgrbs[ss]].z gt grbstr[lgrbs[sl]].z,nw)
;;      print,nw,nss,nw/(nss*1.)
;;      w=where(grbstr[sgrbs[ss]].z gt grbstr[ggrbs[sg]].z,nw)
;;      print,nw,nss,nw/(nss*1.)
     
;     kstwop,grbstr[sgrbs[ss]].z,grbstr[ggrbs[sg]].z,d,gprob
;     kstwop,grbstr[sgrbs[ss]].z,grbstr[lgrbs[sl]].z,d,lprob
;     legend,['P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+ntostr(lprob,4)],/top,/left,box=0,charsize=1.5

;     oplot,[0,10],[0,0]
;     oplot,[0,0],yrange
;     multiplot2,/default,/reset
     if keyword_set(ps) then endplot

  ;;; z vs XLUM
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.xlum_1day gt 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.xlum_1day gt 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.xlum_1day gt 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')

     !x.margin=[14,0]
     begplot,name='~/Fermi/Swift_pop_study/xlum_z.eps',/land,/encap,/color,font='helvetica'
     arange=[0,10]
     lumrange=10d^[40,47]

     scatter_hist,grbstr.z,grbstr.xlum_1day,fltarr(n_elements(grbstr)),grbstr.xlum_1day_err,bin1=0.3,bin2=0.2,xrange=arange,yrange=lumrange,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle='z',ytitle='L!Lx,1 day!N (erg s!U-1!N)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/top,/right,charsize=1.

     endplot

  ;;; COMPARE ENERGETICS
;  cr=mrdfits('/Volumes/Firewire1/racusin/grbs/closure_relations_total_2sig.fits',1)
     gg=mrdfits(!mdata+'grb_info_z_epeak_10keV_10MeV_2009.fits',1)
;     w=where(gg.z gt 0 and gg.eiso gt 0)
;     gg=gg[w]
;  seiso=gg.eiso
;  grbs=gg.grb
     match,strtrim(grbstr.grb,2),strtrim(gg.grb,2),m1,m2
     grbstr[m1].eiso=gg[m2].eiso
     grbstr[m1].bat_fluence=gg[m2].sbat
     grbstr[m1].bat_fluence_err=gg[m2].sbat_err
     grbstr[m1].hr=gg[m2].hr
     grbstr[m1].hr_err=gg[m2].hr_err
     
;  match,ssgrbs,strtrim(g.grb,2),m1,m2
;stop
;  match,sgrbs,'GRB'+strtrim(g.grb,2),sm1,sm2
;  sz=where(g[w].z)
     !x.margin=[10,2]
;     if keyword_set(ps) then
     begplot,name='~/Fermi/Swift_pop_study/eiso_plots.eps',/encap,/color,font='helvetica'
;     begplot,name='~/proposals/Chandra_cycle13/eiso_plots.eps',/encap,/color,font='helvetica',/land
;  readcol,'~/Fermi/Swift_pop_study/Fermi_energetics.csv',grb,who,z,epeak,epeakerr,alpha,beta,eiso,stuff1,stuff2,format='(a,a,f,f,f,f,f,d,a,a)'
     if not keyword_set(justplot) then begin 
        readcol,'~/Fermi/Swift_pop_study/Fermi_energetics.csv',grb,who,z,fluence,flueerr,obs_emin,obs_emax,a,norm_eng,duration,epeak,alpha,beta,phind,a2,norm_eng2,eiso0,stuff1,stuff2,format='(a,a,f,d,d,d,d,d,f,f,f,f,f,f,f,f,d,a,a)'
        
        w1=where(a eq 0,nw1)
        w2=where(a ne 0,nw2)
        eiso=eiso0
        for i=0,nw1-1 do begin 
           emin=10d    ;; keV
;           emax=1d7    ;; 10 GeV
           emax=1d4    ;; 10 MeV
           j=w1[i]
           if beta[j] eq 0 and epeak[j] eq 0 then pl=1 else pl=0
           if beta[j] eq 0 and epeak[j] ne 0 then cutoff=1 else cutoff=0
           if phind[j] ne 0 then begin 
              print,'PHind + a=0 ',grb[j]
              tot_eiso=calc_eiso2(fluence[j],obs_emin[j],obs_emax[j],z[j],alpha[j],beta[j],epeak[j],phind[j],a2[j],emin=emin,emax=emax,h0=71,omega_m=0.27,/bandpl)
              ;;;; WRONG WRONG WRONG
;              beiso=calc_eiso2(fluence[j],obs_emin[j],obs_emax[j],z[j],alpha[j],beta[j],epeak[j],emin=emin,emax=emax,h0=71,omega_m=0.27,cutoff=cutoff,pl=pl)
;              eiso[j]=tot_eiso-beiso ;;; ?????
              eiso[j]=tot_eiso 
             
              ;;SUBTRACT OFF PL?
           endif else $
              eiso[j]=calc_eiso2(fluence[j],obs_emin[j],obs_emax[j],z[j],alpha[j],beta[j],epeak[j],emin=emin,emax=emax,h0=71,omega_m=0.27,cutoff=cutoff,pl=pl)        
;     w=where(grbstr.epeak eq 0. and grbstr.alpha_pl gt -2.3 and grbstr.alpha_pl lt -1.2)
;  grbstr[w].epeak=10^(2.76-3.61*alog10(-grbstr[w].alpha_pl))

        endfor 

        for i=0,nw2-1 do begin
           emin=10d    ;; keV
;           emax=1d7    ;; 10 GeV
           emax=1d4    ;; 10 MeV
           j=w2[i]
           p=[a[j],alpha[j],epeak[j],beta[j]]
           print,p,duration[j],norm_eng[j],z[j]
           eiso[j]=calc_eiso(z[j],p,0.,duration[j],model='band',emin=emin,emax=emax,h0=72d,enorm=norm_eng[j],omega_m=0.27)
     ;;; DO BANDPL?  ALREADY SUBTRACTS OFF THE PL BECAUSE THIS USES NORMALIZATION...
        endfor 
        match,'GRB'+grb,strtrim(grbstr.grb,2),m1,m2
        grbstr[m2].eiso=eiso[m1]
     endif 
     if not keyword_set(ps) then erase
     multiplot2,[1,2],/init
     multiplot2
     plot,[48,56],[0,30],/nodata,/xsty,ytitle='N';,xtitle='log E!L'+!tsym.gamma+',iso, 10 keV-10 MeV!N (erg)'

;  match,strtrim(ssgrbs,2),strtrim(grbs,2),m1,m2
;  ls=where(dur[m1] eq 'long')
;  ss=where(dur[m1] eq 'short')
;  ls=m2[ls]
;  ss=m2[ss]
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.eiso ne 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.eiso ne 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.eiso ne 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
     
     
;     plothist,alog10(grbstr[sgrbs[ls]].eiso),bin=0.3,/over,/fill,fcolor=!grey70,color=!grey70
     plothist,alog10(grbstr[sgrbs[ls]].eiso),bin=0.3,/over,/fill,fcolor=!grey50,color=!grey50

;;;;; SEPARATE OUT OR INDICATE SHORT BURSTS - dur=short/long
;  l=where(who eq 'LAT')
;  ll=where(eiso[l] 
;  g=where(who eq 'GBM')
;  match,strtrim(ssgrbs,2),'GRB'+strtrim(grb[l],2),m1,m2
;  ll=where(dur[m1] eq 'long')
;  sl=where(dur[m1] eq 'short')
;  ll=l[m2[ll]]
;  sl=l[m2[sl]]

;  match,strtrim(ssgrbs,2),'GRB'+strtrim(grb[g],2),m1,m2
;  lg=where(dur[m1] eq 'long')
;  sg=where(dur[m1] eq 'short')
;  lg=g[m2[lg]]
;  sg=g[m2[sg]]
     
;     plothist,alog10(grbstr[ggrbs[lg]].eiso),bin=0.3,/over,color=!grey40,/fline,forient=45,/fill,fcolor=!grey40
;     plothist,alog10(grbstr[lgrbs[ll]].eiso),bin=0.3,/over,color=!p.color,line=0
     plothist,alog10(grbstr[ggrbs[lg]].eiso),bin=0.3,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue
     plothist,alog10(grbstr[lgrbs[ll]].eiso),bin=0.3,/over,color=!red,line=0
     oplot,[0,100],[0,0]
     kstwop,grbstr[sgrbs[ls]].eiso,grbstr[lgrbs[ll]].eiso,d,lprob
     kstwop,grbstr[sgrbs[ls]].eiso,grbstr[ggrbs[lg]].eiso,d,gprob
     kstwop,grbstr[ggrbs[lg]].eiso,grbstr[lgrbs[ll]].eiso,d,glprob
     ks[0].eiso_bg=gprob
     ks[0].eiso_bl=lprob
     ks[0].eiso_gl=glprob

;     kstwop,grbstr[sgrbs[ss]].eiso,grbstr[lgrbs[sl]].eiso,d,lprob
;     kstwop,grbstr[sgrbs[ss]].eiso,grbstr[ggrbs[sg]].eiso,d,gprob
;     kstwop,grbstr[ggrbs[sg]].eiso,grbstr[lgrbs[sl]].eiso,d,glprob
;     ks[1].eiso_bg=gprob
;     ks[1].eiso_bl=lprob
;     ks[1].eiso_gl=glprob


;     legend,['Long','P!LKS,GBM!N = '+ntostr(gprob,4),'P!LKS,LAT!N = '+sigfig(lprob,2)],/top,/left,box=0,charsize=1.5
;     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey70,!grey40,!p.color]
     legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[!grey50,!blue,!red] 
;goto,skipshort
    legend,'Long',/top,/left,box=0

     multiplot2
     plot,[48,56],[0,5],/nodata,xtitle='log E!L'+!tsym.gamma+',iso, 10 keV-10 MeV!N (erg)',/xsty,ytitle='N'
;     plothist,alog10(grbstr[sgrbs[ss]].eiso),bin=0.3,/over,/fill,fcolor=!grey70,color=!grey70
;     plothist,[0,alog10(grbstr[ggrbs[sg]].eiso)],bin=0.3,/over,color=!grey40,/fline,forient=45,/fill,fcolor=!grey40,xmin=48,xmax=56
;     plothist,[0,alog10(grbstr[lgrbs[sl]].eiso)],bin=0.3,/over,color=!p.color,line=0,xmin=48,xmax=56
     plothist,alog10(grbstr[sgrbs[ss]].eiso),bin=0.3,/over,/fill,fcolor=!grey50,color=!grey50

     plothist,[0,alog10(grbstr[ggrbs[sg]].eiso)],bin=0.3,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue,xmin=48,xmax=56

     plothist,[0,alog10(grbstr[lgrbs[sl]].eiso)],bin=0.3,/over,color=!red,line=0,xmin=48,xmax=56
     oplot,[0,100],[0,0]
     legend,['Short'],/top,/left,box=0
;  kstwop,seiso[m2[ss]],eiso[sl],d,lprob
;  kstwop,seiso[m2[ss]],eiso[sg],d,gprob


     multiplot2,/reset,/default
;skipshort:
     if keyword_set(ps) then endplot

     begplot,name='~/Fermi/Swift_pop_study/ax_ao.eps',/encap,/land,/color,font='helvetica'
     w=where(grbstr.xalpha ne 0 and grbstr.oalpha ne 0 and grbstr.who eq 'BAT')
     ploterror,grbstr[w].xalpha,grbstr[w].oalpha,grbstr[w].xalpha_err,grbstr[w].oalpha_err,psym=3,/iso,xtitle=!tsym.alpha+'!Lx!N',ytitle=!tsym.alpha+'!Lo!N',/nohat,yrange=[0,3]
     w=where(grbstr.xalpha ne 0 and grbstr.oalpha ne 0 and grbstr.who eq 'GBM')
     oploterror,grbstr[w].xalpha,grbstr[w].oalpha,grbstr[w].xalpha_err,grbstr[w].oalpha_err,psym=3,color=!blue,errcolor=!blue,/nohat
     w=where(grbstr.xalpha ne 0 and grbstr.oalpha ne 0 and grbstr.who eq 'LAT')
     oploterror,grbstr[w].xalpha,grbstr[w].oalpha,grbstr[w].xalpha_err,grbstr[w].oalpha_err,psym=3,color=!red,errcolor=!red,/nohat
     oplot,[0,5],[0,5]
     endplot
;  erase
;  multiplot2,[1,2],/init
;  multiplot2

     day=86400d

;  swjb=where(sjb[sm1] eq 1)
;  swnjb=where(sjb[sm1] eq 0)
;  ns=n_elements(sm1)
;  sthetaj=dblarr(ns)
     if not keyword_set(justplot) then begin
        ns=n_elements(grbstr)
        for i=0,ns-1 do grbstr[i].thetaj=jet_angle(grbstr[i].xtbreak/day,z=grbstr[i].z,eiso=grbstr[i].eiso)
        grbstr.egam=grbstr.eiso*(1.-cos(grbstr.thetaj*!dtor))
        if keyword_set(lum) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',/create

        stop
     endif 
;;   gwjb=where(gjb eq 1)
;;   gwnjb=where(gjb eq 0)
;;   ng=n_elements(gjb)
;;   gthetaj=dblarr(ng)
;;   for i=0,ng-1 do gthetaj[i]=jet_angle(gtbreak[i]/day,z=gz[i],eiso=eiso[g[i]])
;;   gegam=eiso[g]*(1.-cos(gthetaj*!dtor))

;;   lwjb=where(ljb eq 1,nwl)
;;   lwnjb=where(ljb eq 0,nwln)
;;   nl=n_elements(ljb)
;;   lthetaj=dblarr(nl)
;;   for i=0,nl-1 do lthetaj[i]=jet_angle(ltbreak[i]/day,z=lz[i],eiso=eiso[l[i]])
;;   legam=eiso[l]*(1.-cos(lthetaj*!dtor))

;  !y.margin=[2,2]
     !x.margin=[12,0]
;  !x.margin=xmarg
     
     bin=0.5
     
     ogrbstr=grbstr
     swjb=where(grbstr.xjb ge 1 and grbstr.who eq 'BAT' and grbstr.eiso gt 0)
     swnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'BAT' and grbstr.eiso gt 0 and grbstr.thetaj ne 0)
     gwjb=where(grbstr.xjb ge 1 and grbstr.who eq 'GBM' and grbstr.eiso gt 0,nwg)
     gwnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'GBM' and grbstr.eiso gt 0 and grbstr.thetaj ne 0)
     lwjb=where(grbstr.xjb ge 1 and grbstr.who eq 'LAT' and grbstr.eiso gt 0,nwl)
     lwnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'LAT' and grbstr.eiso gt 0 and grbstr.thetaj ne 0,nwln)
     ss=where(strtrim(grbstr[swjb].dur,2) eq 'short')
     ls=where(strtrim(grbstr[swjb].dur,2) eq 'long')
;        sg=where(strtrim(grbstr[gwjb].dur,2) eq 'short')
     lg=where(strtrim(grbstr[gwjb].dur,2) eq 'long')
;        sl=where(strtrim(grbstr[lwjb].dur,2) eq 'short')
;        ll=where(strtrim(grbstr[lwjb].dur,2) eq 'long')

     kstwop,grbstr[swjb[ls]].thetaj,grbstr[gwjb[lg]].thetaj,d,gprob
;        kstwop,grbstr[swjb].thetaj,grbstr[lwjb].thetaj,d,lprob
;        kstwop,grbstr[gwjb].thetaj,grbstr[lwjb].thetaj,d,glprob
     ks[0].theta_bg=gprob
;        ks.theta_bl=lprob
;        ks.theta_gl=glprob
     kstwop,grbstr[swjb[ls]].egam,grbstr[gwjb[lg]].egam,d,gprob
;        kstwop,grbstr[swjb].egam,grbstr[lwjb].egam,d,lprob
;        kstwop,grbstr[gwjb].egam,grbstr[lwjb].egam,d,glprob
     ks[0].egam_bg=gprob
;        ks.egam_bl=lprob
;        ks.egam_gl=glprob

;        kstwop,grbstr[swjb[ss]].thetaj,grbstr[gwjb[sg]].thetaj,d,gprob
;        ks[1].theta_bg=gprob
;        kstwop,grbstr[swjb[ss]].egam,grbstr[gwjb[sg]].egam,d,gprob
;        ks[1].egam_bg=gprob

     ss=where(strtrim(grbstr[swnjb].dur,2) eq 'short')
     ls=where(strtrim(grbstr[swnjb].dur,2) eq 'long')
     sg=where(strtrim(grbstr[gwnjb].dur,2) eq 'short')
     lg=where(strtrim(grbstr[gwnjb].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lwnjb].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lwnjb].dur,2) eq 'long')
     kstwop,grbstr[swnjb[ls]].thetaj,grbstr[gwnjb[lg]].thetaj,d,gprob
     kstwop,grbstr[swnjb[ls]].thetaj,grbstr[lwnjb[ll]].thetaj,d,lprob
     kstwop,grbstr[gwnjb[lg]].thetaj,grbstr[lwnjb[ll]].thetaj,d,glprob
     ks[0].ntheta_bg=gprob
     ks[0].ntheta_bl=lprob
     ks[0].ntheta_gl=glprob
     kstwop,grbstr[swnjb[ls]].egam,grbstr[gwnjb[lg]].egam,d,gprob
     kstwop,grbstr[swnjb[ls]].egam,grbstr[lwnjb[ll]].egam,d,lprob
     kstwop,grbstr[gwnjb[lg]].egam,grbstr[lwnjb[ll]].egam,d,glprob
     ks[0].negam_bg=gprob
     ks[0].negam_bl=lprob
     ks[0].negam_gl=glprob

;     kstwop,grbstr[swnjb[ss]].thetaj,grbstr[gwnjb[sg]].thetaj,d,gprob
;     kstwop,grbstr[swnjb[ss]].thetaj,grbstr[lwnjb[sl]].thetaj,d,lprob
;     kstwop,grbstr[gwnjb[sg]].thetaj,grbstr[lwnjb[sl]].thetaj,d,glprob
;     ks[1].ntheta_bg=gprob
;     ks[1].ntheta_bl=lprob
;     ks[1].ntheta_gl=glprob
;     kstwop,grbstr[swnjb[ss]].egam,grbstr[gwnjb[sg]].egam,d,gprob
;     kstwop,grbstr[swnjb[ss]].egam,grbstr[lwnjb[sl]].egam,d,lprob
;     kstwop,grbstr[gwnjb[sg]].egam,grbstr[lwnjb[sl]].egam,d,glprob
;     ks[1].negam_bg=gprob
;     ks[1].negam_bl=lprob
;     ks[1].negam_gl=glprob
     
     mwrfits,ks,'~/Fermi/Swift_pop_study/kstest.fits',/create

     for i=0,1 do begin 
        grbstr=ogrbstr
        case i of 
           0: begin
              add='_long'
              w=where(strtrim(grbstr.dur,2) eq 'long')
              grbstr=grbstr[w]
              ymax=15
              ah=5
              leg='Long'
           end
           1: begin
              add='_short'
              w=where(strtrim(grbstr.dur,2) eq 'short')
              grbstr=grbstr[w]
              ymax=4
              ah=2
              leg='Short'
           end
        endcase
;     !p.multi=[0,2,2]
        swjb=where(grbstr.xjb eq 1 and grbstr.who eq 'BAT' and grbstr.eiso gt 0)
        swnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'BAT' and grbstr.eiso gt 0 and grbstr.thetaj ne 0)
        gwjb=where(grbstr.xjb eq 1 and grbstr.who eq 'GBM' and grbstr.eiso gt 0,nwg)
        gwnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'GBM' and grbstr.eiso gt 0 and grbstr.thetaj ne 0)
        lwjb=where(grbstr.xjb eq 1 and grbstr.who eq 'LAT' and grbstr.eiso gt 0,nwl)
        lwnjb=where(grbstr.xjb eq 0 and grbstr.who eq 'LAT' and grbstr.eiso gt 0 and grbstr.thetaj ne 0,nwln)

        if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/energetics'+add+'.eps',/land,/color,/encap,font='helvetica'
        !x.margin=[14,0]
        !y.margin=[5,2]
        multiplot2,[2,2],/init

;        bcolor=!grey70     ;;grey50
;        gcolor=!grey40     ;;blue
;        lcolor=!p.color    ;;red
        bcolor=!grey50
        gcolor=!blue
        lcolor=!red
        multiplot2
        plot,[0,15],[0,ymax],/nodata,ytitle='N',charsize=2 ;,xtitle=!tsym.theta+'!Lj!N'
        plothist,grbstr[swjb].thetaj,bin=bin,/over,/fill,fcolor=bcolor,color=bcolor
;        if nwg gt 0 then plothist,[-1,grbstr[gwjb].thetaj],bin=bin,/over,color=gcolor,/fline,forient=45,/fill,fcolor=gcolor
        if nwl gt 0 then plothist,[-1,grbstr[lwjb].thetaj],bin=bin,/over,color=lcolor,line=0
        oplot,[0,15],[0,0]
        legend,leg,/top,/left,box=0

        ebin=0.3
        multiplot2
        plot,[47,53],[0,ymax],/nodata,/xsty,charsize=2 ;,ytitle='N';xtitle='E!L'+!tsym.gamma+'!N (erg)',
        plothist,alog10(grbstr[swjb].egam),bin=ebin,/over,/fill,fcolor=bcolor,color=bcolor
;        if nwg gt 0 then plothist,[40,alog10(grbstr[gwjb].egam)],bin=ebin,/over,color=gcolor,/fline,forient=45,/fill,fcolor=gcolor
        if nwl gt 0 then plothist,[40,alog10(grbstr[lwjb].egam)],bin=ebin,/over,color=lcolor,line=0
        oplot,[47,53],[0,0]
;        legend,['BAT','GBM','LAT'],/top,/right,box=0,textcolor=[bcolor,gcolor,lcolor]
        legend,['BAT','LAT'],/top,/right,box=0,textcolor=[bcolor,lcolor]
        legend,'Measurements',/left,/top,box=0

;     !y.margin=[4,2]
        multiplot2
        plot,[0,15],[0,ymax],/nodata,xtitle=!tsym.theta,ytitle='N',xtickname=['0','5','10',' '],charsize=2
        plothist,grbstr[swnjb].thetaj,bin=bin,/over,/fill,fcolor=bcolor,color=bcolor
;        plothist,[-1,grbstr[gwnjb].thetaj],bin=bin,/over,color=gcolor,/fline,forient=45,fcolor=gcolor,/fill
        if nwln gt 0 then plothist,[-1,grbstr[lwnjb].thetaj],bin=bin,/over,color=lcolor,line=0
        oplot,[0,15],[0,0]
        arrow,10,ah,13,ah,/data,/solid,thick=10,hthick=3

        multiplot2
        plot,[47,53],[0,ymax],/nodata,/xsty,charsize=2,xtitle='E!L'+!tsym.gamma ;,ytitle='N'
        plothist,alog10(grbstr[swnjb].egam),bin=ebin,/over,/fill,fcolor=bcolor,color=bcolor
;        plothist,[40,alog10(grbstr[gwnjb].egam)],bin=ebin,/over,color=gcolor,/fline,forient=45,/fill,fcolor=gcolor
        if nwln gt 0 then plothist,[40,alog10(grbstr[lwnjb].egam)],bin=ebin,/over,color=lcolor,line=0
        oplot,[47,53],[0,0]
        arrow,51.6,ah,52.6,ah,/solid,thick=10,/data,hthick=3
        legend,'Lower Limits',/left,/top,box=0

;     !p.multi=0
        multiplot2,/reset,/default
        if keyword_set(ps) then endplot
     endfor 
     
     grbstr=ogrbstr

     begplot,name='~/Fermi/Swift_pop_study/energetics.eps',/encap,/land,/color,font='helvetica'

     w=where(grbstr.egam gt 0 and grbstr.thetaj gt 0 and grbstr.eiso gt 0)
     grbstr=grbstr[w]
     sgrbs=where(grbstr.who eq 'BAT')
     lgrbs=where(grbstr.who eq 'LAT')
     ggrbs=where(grbstr.who eq 'GBM')
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long',nll)

     ;;; do scatter hist with limits - ugh!
     plot,[0,35],[1d46,1d54],/nodata,/ylog,xtitle=!tsym.theta+'!Lj!N (deg)',ytitle='E!L'+!tsym.gamma+'!N (erg)',/xsty,charsize=2.
     plots,grbstr[sgrbs[ls]].thetaj,grbstr[sgrbs[ls]].egam,psym=8,color=!grey50
     plots,grbstr[sgrbs[ss]].thetaj,grbstr[sgrbs[ss]].egam,psym=2,color=!grey50
     plots,grbstr[ggrbs[lg]].thetaj,grbstr[ggrbs[lg]].egam,psym=8,color=!blue
     if nll gt 0 then plots,grbstr[lgrbs[ll]].thetaj,grbstr[lgrbs[ll]].egam,psym=8,color=!red
     plots,grbstr[lgrbs[sl]].thetaj,grbstr[lgrbs[sl]].egam,psym=2,color=!red

     grbstr=ogrbstr
     w=where(grbstr.egam gt 0 and grbstr.xjb eq 2 and grbstr.thetaj gt 0 and grbstr.eiso gt 0)
     grbstr=grbstr[w]
     sgrbs=where(grbstr.who eq 'BAT')
     lgrbs=where(grbstr.who eq 'LAT')
     ggrbs=where(grbstr.who eq 'GBM')
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short',nss)
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long',nls)
;     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
;     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long',nlg)
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long',nll)
     plotsym2,12,3,thick=3
     if nls gt 0 then plots,grbstr[sgrbs[ls]].thetaj,grbstr[sgrbs[ls]].egam,psym=8,color=!grey50
     if nss gt 0 then plots,grbstr[sgrbs[ss]].thetaj,grbstr[sgrbs[ss]].egam,psym=8,color=!grey50
     if nlg gt 0 then plots,grbstr[ggrbs[lg]].thetaj,grbstr[ggrbs[lg]].egam,psym=8,color=!blue
     if nll gt 0 then plots,grbstr[lgrbs[ll]].thetaj,grbstr[lgrbs[ll]].egam,psym=8,color=!red
     if nsl gt 0 then plots,grbstr[lgrbs[sl]].thetaj,grbstr[lgrbs[sl]].egam,psym=8,color=!red

     grbstr=ogrbstr
     w=where(grbstr.egam gt 0 and grbstr.xjb eq 0 and grbstr.thetaj gt 0 and grbstr.eiso gt 0)
     grbstr=grbstr[w]
     sgrbs=where(grbstr.who eq 'BAT')
     lgrbs=where(grbstr.who eq 'LAT')
     ggrbs=where(grbstr.who eq 'GBM')
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short',nss)
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long',nls)
;     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long',nlg)
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long',nll)
     plotsym2,9,3,thick=3
     if nls gt 0 then plots,grbstr[sgrbs[ls]].thetaj,grbstr[sgrbs[ls]].egam,psym=8,color=!grey50
     if nss gt 0 then plots,grbstr[sgrbs[ss]].thetaj,grbstr[sgrbs[ss]].egam,psym=8,color=!grey50
     if nlg gt 0 then plots,grbstr[ggrbs[lg]].thetaj,grbstr[ggrbs[lg]].egam,psym=8,color=!blue
     if nll gt 0 then plots,grbstr[lgrbs[ll]].thetaj,grbstr[lgrbs[ll]].egam,psym=8,color=!red
     if nsl gt 0 then plots,grbstr[lgrbs[sl]].thetaj,grbstr[lgrbs[sl]].egam,psym=8,color=!red

     legend,['BAT','GBM','LAT'],box=0,/top,/right,textcolor=[!grey50,!blue,!red]

;     scatter_hist,grbstr.thetaj,grbstr.egam,replicate(0.,n_elements(grbstr)),replicate(0.,n_elements(grbstr)),bin1=0.5,bin2=0.3,xrange=[0,10],yrange=10d^[46,54],psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle=!tsym.theta+'!Lj!N (deg)',ytitle='E!L'+!tsym.gamma+'!N (erg)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=0,w4=lgrbs[ll],w5=lgrbs[sl],leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/top,/right

     endplot


     grbstr=ogrbstr
  ;;;; plot xlum_1day vs Eiso & olum_1day vs Eiso for diff samples

     sgrbs=where(grbstr.who eq 'BAT' and grbstr.eiso ne 0 and grbstr.xlum_1day gt 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.eiso ne 0 and grbstr.xlum_1day gt 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.eiso ne 0 and grbstr.xlum_1day gt 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short')
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
     stop
     !x.margin=[14,0]
     begplot,name='~/Fermi/Swift_pop_study/xlum_eiso.eps',/land,/encap,/color,font='helvetica'
     plotsym,0,1,/fill
     multiplot2,[2,2],/init
     multiplot2
     plot,[1d48,1d55],[1d40,1d47],/nodata,ytitle='L!Lx!N (t=1 day rest frame) (erg)',yrange=[1d40,1d47],xrange=[1d48,1d55],/ysty,/xlog,/ylog,xtick_get=xtickv
     oplot,grbstr[sgrbs[ls]].eiso,grbstr[sgrbs[ls]].xlum_1day,psym=8,color=!grey50
     oplot,grbstr[ggrbs[lg]].eiso,grbstr[ggrbs[lg]].xlum_1day,psym=8,color=!blue
     oplot,grbstr[lgrbs[ll]].eiso,grbstr[lgrbs[ll]].xlum_1day,psym=8,color=!red
     oplot,grbstr[sgrbs[ss]].eiso,grbstr[sgrbs[ss]].xlum_1day,psym=2,color=!grey50
     plots,grbstr[ggrbs[sg]].eiso,grbstr[ggrbs[sg]].xlum_1day,psym=2,color=!blue
     plots,grbstr[lgrbs[sl]].eiso,grbstr[lgrbs[sl]].xlum_1day,psym=2,color=!red
     legend,['BAT','GBM','LAT'],box=0,/top,/left,textcolor=[!grey50,!blue,!red]


     multiplot2,xrightgap=0.25
     lumrange=[40,47]
     w=where(grbstr[sgrbs].xlum_1day gt 0 and grbstr[sgrbs].eiso ne 0)
     xlum_spec=grbstr[sgrbs[w]].xlum_1day
     w=where(grbstr[ggrbs].xlum_1day gt 0 and grbstr[ggrbs].eiso ne 0)
     xglum_spec=grbstr[ggrbs[w]].xlum_1day     
     w=where(grbstr[lgrbs].xlum_1day gt 0 and grbstr[lgrbs].eiso ne 0)
     xllum_spec=grbstr[lgrbs[w]].xlum_1day 

     plot,[0,20],lumrange,/nodata,yrange=lumrange,xrange=[0,20],/xsty,/ysty,xtickname=['0',' ','10',' ','20'],xtickv=[0,5,10,15,20]
     plothist,alog10(xlum_spec),bin=0.2,/rotate,/fill,fcolor=!grey50,color=!grey50,/over
     plothist,alog10(xglum_spec),bin=0.2,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue,/rotate
     plothist,alog10(xllum_spec),bin=0.2,/over,color=!red,line=2,/rotate
     xyouts,24,46,'log L!Lx, 1 day!N (erg s!U-1!N)',orient=-90,/data
     oplot,[0,0],lumrange

     multiplot2,yupgap=0.2,xrightgap=0
     nrange=[0,20]
     arange=[48,56]
     plot,arange,nrange,/nodata,xrange=arange,yrange=nrange,ytitle='N',xtitle='E!Liso!N (erg)',xtickname='10!U'+['48','50','52','54','56']+'!N'
     w=where(grbstr[sgrbs].xlum_1day gt 0 and grbstr[sgrbs].eiso ne 0)
     plothist,alog10(grbstr[sgrbs[w]].eiso),bin=0.2,/fill,fcolor=!grey50,color=!grey50,/over,yrange=nrange
     w=where(grbstr[ggrbs].xlum_1day gt 0 and grbstr[ggrbs].eiso ne 0)
     plothist,alog10(grbstr[ggrbs[w]].eiso),bin=0.2,/over,color=!blue,/fline,forient=45,/fill,fcolor=!blue,yrange=nrange,/ysty
     w=where(grbstr[lgrbs].xlum_1day gt 0 and grbstr[lgrbs].eiso ne 0)
     plothist,alog10(grbstr[lgrbs[w]].eiso),bin=0.2,/over,color=!red,line=2,yrange=nrange,/ysty
     oplot,[0,0],arange

     multiplot2,/reset,/default

     endplot
     grbstr=ogrbstr
  endif 
  ;;;; G-fluence vs X-flux - Chryssa's idea
  if not keyword_set(justplot) then begin 
     readcol,'~/Fermi/Swift_pop_study/Fermi_energetics.csv',grb,who,z,fluence,flueerr,obs_emin,obs_emax,a,norm_eng,duration,epeak,alpha,beta,phind,a2,norm_eng2,eiso0,stuff1,stuff2,format='(a,a,f,d,d,d,d,d,f,f,f,f,f,f,f,f,d,a,a)'
     w=where(who eq 'LAT' or who eq 'GBM',nw)
     bfluence=dblarr(nw) & bfluence_err=bfluence
     hratio=dblarr(nw) & hratioerr=hratio
     for i=0,nw-1 do begin 
        convert_gbm_bat_fluence,fluence[w[i]],flueerr[w[i]],obs_emin[w[i]],obs_emax[w[i]],epeak[w[i]],alpha[w[i]],beta[w[i]],bflue,hr,hrerr,bflue_err
        bfluence[i]=bflue
        bfluence_err[i]=bflue_err
        hratio[i]=hr
        hratioerr[i]=hrerr  ;;; needs to be a real value
     endfor 
     
     match,'GRB'+grb[w],strtrim(grbstr.grb,2),m1,m2
     grbstr[m2].bat_fluence=bfluence[m1]
     grbstr[m2].bat_fluence_err=bfluence_err[m1]
     grbstr[m2].hr=hratio[m1]
     grbstr[m2].hr_err=hratioerr[m1]
     if keyword_set(lum) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study.fits',/create
     if keyword_set(flux) then mwrfits,grbstr,'~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',/create

  endif


  if keyword_set(flux) then begin 
     readcol,'~/jetbreaks/bat_grb_fluence_pflux_bat2.txt',grb2,trig,model,s1,s1err,s2,s2err,s3,s3err,s4,s4err,s5,s5err,start,stop,format='(a,l,a,a,a,a,a,a,a,a,a,d,d,d,d)',/silent
     match,strtrim(grbstr.grb,2),grb2,m1,m2
     w=where(s5[m2] ne 0)
     m2=m2[w]
     grbstr[m1].bat_fluence=s5[m2]*1.
     grbstr[m1].bat_fluence_err=s5err[m2]*1.
     grbstr[m1].hr=s3[m2]/(s2[m2]*1.)
     grbstr[m1].hr_err=sqrt((s3err[m2]*1./(s3[m2]*1.))^2.+(s2err[m2]*1./(s2[m2]*1.))^2.)*grbstr[m1].hr

;  if keyword_set(justplot) then grbstr=mrdfits('~/Fermi/Swift_pop_study/grb_struct_pop_study_flux.fits',1)
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.bat_fluence ne 0 and grbstr.xflux_1day_obs ne 0)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.bat_fluence ne 0 and grbstr.xflux_1day_obs ne 0)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.bat_fluence ne 0 and grbstr.xflux_1day_obs ne 0)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
;  xflux=grbstr.xlum_1day_obs/grbstr.xlumfact*grbstr.xfluxfact
     xflux=grbstr.xflux_1day_obs
     xfluxerr=grbstr.xflux_1day_obs_err
;  xflux=grbstr.xflux_11hr_obs
;  xfluxerr=grbstr.xflux_11hr_obs_err
     sbat=grbstr.bat_fluence
     sbat_err=grbstr.bat_fluence_err

  ;;;; This plot can be made with all, not just with z bursts.  Need
  ;;;; to separate out flux grbstr.

  ;;; need to make UVOT version of plot
  ;;;; plot bat_fluence vs xlum for different populations
     !x.margin=[14,0]

     if keyword_set(ps) then begplot,name='~/Fermi/Swift_pop_study/xlum_sbat.eps',/land,/encap,/color,font='helvetica'
     plotsym,0,1,/fill
     multiplot2,[2,2],/init
     multiplot2
     srange=[1d-8,1d-3]
     frange=[1d-15,1d-10]

     scatter_hist,sbat,xflux,sbat_err,xfluxerr,bin1=0.15,bin2=0.2,xrange=srange,yrange=frange,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle='S!L'+!tsym.gamma+',15-150 keV!N (erg cm!U-2!N)',ytitle='F!Lx!N (erg cm!U-2!N s!U-1!N)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=0,w4=lgrbs[ll],w5=-1,leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs,pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/xlog,/ylog,/bottom,/right,charsize=1.

     if keyword_set(ps) then endplot

  endif 

  ;;;;;;;;;;;   TEST DIFFERENCES IN ENVIRONMENT VIA NH AV AND DARKNESS

  if keyword_set(lum) then begin 
     sgrbs=where(grbstr.who eq 'BAT' and grbstr.av ge 0 and grbstr.nhs ne 0 and grbstr.nhs gt 1d20)
     ggrbs=where(grbstr.who eq 'GBM' and grbstr.av ge 0 and grbstr.nhs ne 0 and grbstr.nhs gt 1d20)
     lgrbs=where(grbstr.who eq 'LAT' and grbstr.av ge 0 and grbstr.nhs ne 0 and grbstr.nhs gt 1d20)
     ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
     ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
     sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
     lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
     sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
     ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
     av=grbstr.av
     averr=grbstr.av_err
     averr0=dblarr(n_elements(averr[0,*]))
     averr90=dblarr(2,n_elements(averr[0,*]))
     for q=0,n_elements(averr[0,*])-1 do begin 
        if av[q] ne -1 then begin 
           averr0[q]=max(averr[*,q])
           averr90[*,q]=conv_conf(av[q],averr0[q],0.9)
        endif 
     endfor 
     w=where(av eq 0)
     av[w]=0.01
;     av[w]=av[w]+averr[1,w]
;     averr[*,w]=0.

;     avrange=[0.001,10]
     avrange=[-0.5,2]
     nhrange=[1d20,1d23]
     begplot,name='~/Fermi/Swift_pop_study/av_nh.eps',/color,font='helvetica';,/land
     !x.margin=[10,4]
     extralines=['plotsym,6,3,thick=5',$
                 'plots,x[wex0]+xerr[1,wex0],y[wex0],psym=8,color=colors[0]',$
                 'for k=0,n_elements(wex0)-1 do oplot,[x[wex0[k]],x[wex0[k]]]+xerr[1,wex0[k]],[y[wex0[k]]-yerr[0,wex0[k]],y[wex0[k]]+yerr[1,wex0[k]]],color=colors[0]',$
                 'plots,x[wex1]+xerr[1,wex1],y[wex1],psym=8,color=colors[2]',$
                 'for k=0,n_elements(wex1)-1 do oplot,[x[wex1[k]],x[wex1[k]]]+xerr[1,wex1[k]],[y[wex1[k]]-yerr[0,wex1[k]],y[wex1[k]]+yerr[1,wex1[k]]],color=colors[2]',$
                 'plots,x[wex2]+xerr[1,wex2],y[wex2],psym=8,color=colors[4]',$
                 'for k=0,n_elements(wex2)-1 do oplot,[x[wex2[k]],x[wex2[k]]]+xerr[1,wex2[k]],[y[wex2[k]]-yerr[0,wex2[k]],y[wex2[k]]+yerr[1,wex2[k]]],color=colors[4]']


     wdet=where(av[lgrbs[ll]] ne 0. and averr[1,lgrbs[ll]] ne 0. and av[lgrbs[ll]]-averr90[0,lgrbs[ll]] gt 0)
     lwul=where(av[lgrbs[ll]] le 0.1 or averr[1,lgrbs[ll]] eq 0. or av[lgrbs[ll]]-averr90[0,lgrbs[ll]] lt 0)
     gwul=where(av[ggrbs[[lg,sg]]] le 0.1 or averr[1,ggrbs[[lg,sg]]] eq 0. or av[ggrbs[[lg,sg]]]-averr90[0,ggrbs[[lg,sg]]] lt 0)
     swul=where(av[sgrbs[[ls,ss]]] le 0.1 or averr[1,sgrbs[[ls,ss]]] eq 0. or av[sgrbs[[ls,ss]]]-averr90[0,sgrbs[[ls,ss]]] lt 0)


     begplot,name='~/Fermi/Swift_pop_study/av_nh.eps',/color,font='helvetica';,/land
     
     scatter_hist,av,grbstr.nhs,averr90,grbstr.nhs_err,bin1=0.1,bin2=0.1,xrange=avrange,yrange=nhrange,psyms=[8,2,8,2,8,2],colors=[!grey50,!grey50,!blue,!blue,!red,!red],xtitle='A!LV!N',ytitle='N!LH!N (cm!U-2!N)',w0=sgrbs[ls],w1=sgrbs[ss],w2=ggrbs[lg],w3=ggrbs[sg],w4=lgrbs[ll[wdet]],w5=-1,leg=['BAT','GBM','LAT'],pw0=sgrbs,pw1=ggrbs,pw2=lgrbs[ll],pcolors=[!grey50,!blue,!red],fill=[1,1,0],forient=[0,45,0],fline=[0,1,0],plines=[0,0,2],/ylog,/bottom,/right,charsize=1.,extralines=extralines,wex0=sgrbs[ls[swul]],wex1=ggrbs[lg[gwul]],wex2=lgrbs[ll[lwul]]
endplot
     begplot,name='~/Fermi/Swift_pop_study/av_nh_ratio.eps',/color,font='helvetica';,/land

     na=grbstr.nhs/av*1d-21
     xrange=[-1,3]
     yrange=[0,6]
     multiplot2,[1,3],/init
     multiplot2
     nmw=where(strtrim(grbstr.gal,2) ne 'MW')
     na_mw=na
     na_mw[nmw]=0.001
     fs=0.4
     plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,ytitle='N',/xsty;,xtickname=['10!U-1!N','1','10','10!U2!N','10!U3!N'],xticks=4
     plothist,alog10(na_mw[sgrbs]),bin=0.2,color=!grey50,xrange=xrange,yrange=yrange,/over,/fill,fcolor=!grey50
     plothist,alog10(na_mw[ggrbs]),bin=0.2,color=!blue,xrange=xrange,yrange=yrange,/over,/fill,/fline,forient=45,fcolor=!blue,fspacing=fs
;     plothist,alog10(na_mw[lgrbs[ll]]),bin=0.2,color=!red,xrange=xrange,yrange=yrange,/over
;     oplot,xrange,[0,0]
     legend,['BAT','GBM','LAT'],box=0,color=[!grey50,!blue,!red],textcolor=[!grey50,!blue,!red],/top,/left
     legend,['MW'],/top,/right,box=0

     multiplot2
     nlmc=where(strtrim(grbstr.gal,2) ne 'LMC')
     na_lmc=na
     na_lmc[nlmc]=0.001
     plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,ytitle='N',/xsty;,xtickname=['10!U-1!N','1','10','10!U2!N','10!U3!N'],xticks=4
     plothist,alog10(na_lmc[sgrbs]),bin=0.2,color=!grey50,xrange=xrange,yrange=yrange,/over,/fill,fcolor=!grey50
     plothist,alog10(na_lmc[ggrbs]),bin=0.2,color=!blue,xrange=xrange,yrange=yrange,/over,/fill,/fline,forient=45,fcolor=!blue,fspacing=fs
;     plothist,alog10(na_lmc[lgrbs[ll]]),bin=0.2,color=!red,xrange=xrange,yrange=yrange,/over
;     oplot,xrange,[0,0]
     legend,['LMC'],/top,/right,box=0

     multiplot2
     nsmc=where(strtrim(grbstr.gal,2) ne 'SMC')
     na_smc=na
     na_smc[nsmc]=0.001
     plot,xrange,yrange,/nodata,xtitle='N!LH!N/A!LV!N (x 10!U21!N cm!U-2!N)',ytitle='N',/xsty,xrange=xrange,yrange=yrange,$
          xtickname=['10!U-1!N','1','10','10!U2!N','10!U3!N'],xticks=4
     plothist,alog10(na_smc[sgrbs]),bin=0.2,color=!grey50,xrange=xrange,yrange=yrange,/over,/fill,fcolor=!grey50
     plothist,alog10(na_smc[ggrbs]),bin=0.2,color=!blue,xrange=xrange,yrange=yrange,/over,/fill,/fline,forient=45,fcolor=!blue,fspacing=fs
     plothist,alog10(na_smc[lgrbs]),bin=0.2,color=!red,xrange=xrange,yrange=yrange,/over,line=2
     oplot,xrange,[0,0]
     legend,['SMC'],/top,/right,box=0
     multiplot2,/reset,/default

     endplot
  endif 

  kstwop,grbstr[sgrbs[ls]].nhs,grbstr[ggrbs[lg]].nhs,d,gprob
  kstwop,grbstr[sgrbs[ls]].nhs,grbstr[lgrbs[ll]].nhs,d,lprob
  kstwop,grbstr[ggrbs[lg]].nhs,grbstr[lgrbs[ll]].nhs,d,glprob
  ks[0].nh_bg=gprob
  ks[0].nh_bl=lprob
  ks[0].nh_gl=glprob
  kstwop,grbstr[sgrbs[ls]].av,grbstr[ggrbs[lg]].av,d,gprob
  kstwop,grbstr[sgrbs[ls]].av,grbstr[lgrbs[ll]].av,d,lprob
  kstwop,grbstr[ggrbs[lg]].av,grbstr[lgrbs[ll]].av,d,glprob
  ks[0].av_bg=gprob
  ks[0].av_bl=lprob
  ks[0].av_gl=glprob
  kstwop,na[sgrbs[ls]],na[ggrbs[lg]],d,gprob
  kstwop,na[sgrbs[ls]],na[lgrbs[ll]],d,lprob
  kstwop,[ggrbs[lg]],na[lgrbs[ll]],d,glprob
  ks[0].nhav_bg=gprob
  ks[0].nhav_bl=lprob
  ks[0].nhav_gl=glprob

  ;;; just plot NH measured from X-ray spectrum - get more measurements

  sgrbs=where(grbstr.who eq 'BAT' and grbstr.nh ne 0 and grbstr.nh-grbstr.nh_err[0] gt 0)
  ggrbs=where(grbstr.who eq 'GBM' and grbstr.nh ne 0 and grbstr.nh-grbstr.nh_err[0] gt 0)
  lgrbs=where(grbstr.who eq 'LAT' and grbstr.nh ne 0 and grbstr.nh-grbstr.nh_err[0] gt 0)
  ss=where(strtrim(grbstr[sgrbs].dur,2) eq 'short')
  ls=where(strtrim(grbstr[sgrbs].dur,2) eq 'long')
  sg=where(strtrim(grbstr[ggrbs].dur,2) eq 'short')
  lg=where(strtrim(grbstr[ggrbs].dur,2) eq 'long')
  sl=where(strtrim(grbstr[lgrbs].dur,2) eq 'short',nsl)
  ll=where(strtrim(grbstr[lgrbs].dur,2) eq 'long')
  
  begplot,name='~/Fermi/Swift_pop_study/nh.eps',/color,font='helvetica',/land
     
  plot,[20,24],[0,20],/nodata,xtitle='N!LH!N (cm!U-2!N)',ytitle='N',xtickname='10!U'+ntostr(indgen(5)+20)+'!N'
  plothist,alog10(grbstr[sgrbs].nh),bin=0.1,/over,color=!grey50,/fill,fcolor=!grey50
  plothist,alog10(grbstr[ggrbs].nh),bin=0.1,/over,color=!blue,/fill,/fline,forient=45,fcolor=!blue
  plothist,alog10(grbstr[lgrbs].nh),bin=0.1,/over,color=!red,line=2
  oplot,[20,24],[0,0]
  legend,['BAT','GBM','LAT'],box=0,/top,/left,textcolor=[!grey50,!blue,!red]
  axis,xaxis=0,xtickname=replicate(' ',5)
  endplot


stop

  mwrfits,ks,'~/Fermi/Swift_pop_study/kstest.fits',/create
  ;;; DARK BURSTS - VIA VAN DER HORST 2009
  lam_eff=[5402,4329,3501,2634,2231,2030,3471]*1d-8   
  lam_eff=lam_eff[2]
  ufd2flux=lam_eff*1d8
  w=where(grbstr.xflux_11hr_obs ne 0 and grbstr.oflux_11hr_obs ne 0,nw)
  xfd=grbstr[w].xflux_11hr_obs*grbstr[w].xfdfact   
  ofd=grbstr[w].oflux_11hr_obs*grbstr[w].ofdfact/ufd2flux

  box=dblarr(nw)
  c=3d10
  nu_u=c/lam_eff
  nu_1kev=2.42d17
  x=[nu_u,nu_1kev]
  for i=0,nw-1 do begin
     m=linfit(alog10(x),alog10([ofd[i],xfd[i]]))
     box[i]=-m[1]
  endfor 

  plot,grbstr[w].xbeta,box,psym=1,xtitle=!tsym.beta+'!Lx!N',ytitle=!tsym.beta+'!Lox!N',xrange=[0.4,2.2],yrange=[-0.2,1.2],/xsty,/ysty
  xx=[-1.,3.]
  oplot,xx,xx
  oplot,xx,xx-0.5
  d=where(box-grbstr[w].xbeta+0.5 lt 0) ;;; dark??





  ;;; PLOT REST FRAME LUM AT SPECIFIC REST FRAME TIME VERSUS
  ;;; ALPHA FOR OPT & X & COMPARE LAT - USE EARLY TIME (150 S, AND
  ;;; LATE TIME ~1 DAY?)
  ;;;It would be interesting if you could plot luminosity at restframe 150s and decay from restframe 150s onwards in the X-ray, just to see what you get

  ;;; also plot observed flux vs decay index in observed frame?
  ;;;The other thing to do would be to plot observed magnitude against decay (in observed frame). I got quite a tight correlation using the BAT UVOT lc from 500s onwards and magnitude at 400s - would have to change that of course if you  want to include LAT GRBs.

  ;;; Max, is pearing over my shoulder and has suggested that we use E_iso to infer E_k  and expected luminosity of the afterglow, if it is what we observe then we are just sampling the tail end of the GRB distribution with no significant differences - if it is different then this may mean different efficiencies or something (He did say that if we use his idea we have to include him as auther - jokinging - but probably meant as seriour)

  stop

  return
end 
