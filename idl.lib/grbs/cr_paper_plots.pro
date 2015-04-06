pro plot_seg1,cr
  
  w=where(cr.seg1 eq 1,nw)
  
  alpha=cr[w].alpha
  beta=cr[w].beta
  alphaerr=cr[w].alphaerr
  betaerr=cr[w].betaerr
  
  s=sort(alpha)
  alpha=alpha[s]
  beta=beta[s]
  alphaerr=alphaerr[*,s]
  betaerr=betaerr[*,s]
  
  begplot,name='~/papers/jetbreaks1/seg1_test.ps',/land,/color
  plot,alpha,beta,psym=1,/iso,xtitle=!tsym.alpha,ytitle=!tsym.beta,title='Seg I'
  for i=0,nw-1 do begin
     oplot,[alpha[i]-alphaerr[0,i],alpha[i]+alphaerr[1,i]],[beta[i],beta[i]]
     oplot,[alpha[i],alpha[i]],[beta[i]-betaerr[0,i],beta[i]+betaerr[1,i]]
  endfor 
  
  oplot,alpha,alpha-2
;  fitexy,alpha,beta,a1,b1,x_sig=alphaerr[0,*],y_sig=betaerr[0,*]
;  fitexy,alpha,beta,a2,b2,x_sig=alphaerr[1,*],y_sig=betaerr[1,*]
  
;  oplot,alpha,alpha*b1+a1,line=1,color=!green
;  oplot,alpha,alpha*b2+a2,line=2,color=!red
  
;  print,a1,b1
;  print,a2,b2
  
  w=where(alpha-(beta*2.+1) gt 0)
  aerr=alphaerr[1,*]
  aerr[w]=alphaerr[0,w]

  w=where(beta-(alpha/2.-0.5) gt 0)
  berr=betaerr[1,*]
  berr[w]=betaerr[0,w]
  fitexy,alpha,beta,a3,b3,x_sig=aerr,y_sig=berr
  print,a3,b3
  oplot,alpha,alpha*b3+a3,line=2,color=!orange
  print,correlate(alpha,beta)
  
  legend,box=0,/bottom,/right,[!tsym.alpha+'=2+'+!tsym.beta,!tsym.alpha+'=('+!tsym.beta+'+'+numdec(-1*a3,2)+')/'+numdec(b3,2),!tsym.alpha+'=2'+!tsym.beta+'+1'],line=[0,2,1],color=[!p.color,!orange,!blue]
  
  oplot,alpha,alpha/2.-0.5,color=!blue,line=1
  endplot
  
  return
end 

pro plot_compatible_crs,cr,list,charsize=charsize,nocolor=nocolor
  
  nsig0=1.
  crs=['HighLat','HighLat2',$
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
       'JETsoWIND3a','JETsoWIND3b'] 
  
  colors=[!red,!blue,!green,!orange,!cyan,!magenta,!purple,!sienna,!grey50,!forestgreen,!salmon,!darkred,!lightblue,!deeppink,!orangered,!skyblue,!slategrey,!navyblue,!firebrick,!seagreen]
  colors=[colors,colors,colors]
  if keyword_set(nocolor) then colors[*]=!p.color
  
  for i=0,n_elements(list)-1 do begin
     if type(list[0]) eq 2 or type(list[0]) eq 3 then begin 
        w=where(cr.grb eq cr[list[i]].grb,n) 
        grb=cr[list[i]].grb
     endif else begin
        w=where(strtrim(cr.grb,2) eq list[i],n)
        grb=list[i]
     endelse 
     print,grb,n
;     erase
;     multiplot2,[n,1],/init
     hl=0
;     if n gt 1 then begin 

     comp=fltarr(n,n_elements(crs))
     nn=intarr(n)
     for j=0,n-1 do begin 
        for k=0,n_elements(crs)-1 do tmp=execute('comp[j,k]=cr[w[j]].('+ntostr(k+6)+')')
        good=where(comp[j,*] le nsig0 and comp[j,*] ne 0,ng)
        nn[j]=ng
     endfor
     max=max(nn)
     print,max
     start=[5,5,6,7]
     for j=0,n-1 do begin 
        good=where(comp[j,*] le nsig0 and comp[j,*] ne 0,ng)
        
        if j eq 0 then begin 
;              max=ng
;              if max lt 20 then max=20
           plot,[0,n*10],[0,max+2],/nodata,title=grb,/xsty,/ysty,xticks=1,xtickname=[' ',' '],yticks=1,ytickname=[' ',' '],_extra=_extra,color=!white
        endif 
        
        for l=0,ng-1 do xyouts,1+10*j,l+1,crs[good[l]],charsize=charsize
        
        if j eq 0 and (good[0] eq 0 or good[0] eq -1) then hl=1

        if j eq 1 and hl then begin 
           for k=1,ng do oplot,[5,10*(j)],[1,k]
        endif 
        if j gt 1 or (j eq 1 and hl eq 0) then begin 
           for l=0,ng-1 do begin 
              if lg[0] ne -1 then begin 
                 case crs[good[l]] of
                    ;;III's
                    'ISMs2a': g=where(crs[lg] eq 'ISMs2ai' or crs[lg] eq 'ISMf2ai',ngd)
                    'ISMs2b': g=where(crs[lg] eq 'ISMs2ai' or crs[lg] eq 'ISMf2ai',ngd)
                    'ISMs3a': g=where(crs[lg] eq 'ISMs3ai' or crs[lg] eq 'ISMf3ai',ngd)
                    'ISMs3b': g=where(crs[lg] eq 'ISMs3ai' or crs[lg] eq 'ISMf3ai',ngd)
                    'ISMf2a': g=where(crs[lg] eq 'ISMf2ai',ngd)
                    'ISMf2b': g=where(crs[lg] eq 'ISMf2ai',ngd)
                    'ISMf3a': g=where(crs[lg] eq 'ISMf3ai',ngd)
                    'ISMf3b': g=where(crs[lg] eq 'ISMf3ai',ngd)
                    'WINDs2a': g=where(crs[lg] eq 'WINDs2ai' or crs[lg] eq 'WINDf2ai',ngd)
                    'WINDs2b': g=where(crs[lg] eq 'WINDs2ai' or crs[lg] eq 'WINDf2ai',ngd)
                    'WINDs3a': g=where(crs[lg] eq 'WINDs3ai' or crs[lg] eq 'WINDf3ai',ngd)
                    'WINDs3b': g=where(crs[lg] eq 'WINDs3ai' or crs[lg] eq 'WINDf3ai',ngd)
                    'WINDf2a': g=where(crs[lg] eq 'WINDf2ai',ngd)
                    'WINDf2b': g=where(crs[lg] eq 'WINDf2ai',ngd)
                    'WINDf3a': g=where(crs[lg] eq 'WINDf3ai',ngd)
                    'WINDf3b': g=where(crs[lg] eq 'WINDf3ai',ngd)
                    'JETs2ai': g=where(crs[lg] eq 'ISMs2ai' or crs[lg] eq 'ISMf2ai' or crs[lg] eq 'WINDs2ai' or crs[lg] eq 'WINDf2ai',ngd)
                    'JETs3ai': g=where(crs[lg] eq 'ISMs3ai' or crs[lg] eq 'ISMf3ai' or crs[lg] eq 'WINDs3ai' or crs[lg] eq 'WINDf3ai',ngd)
                    'JETsISM2ai': g=where(crs[lg] eq 'ISMs2ai' or crs[lg] eq 'ISMf2ai',ngd)
                    'JETsISM3ai': g=where(crs[lg] eq 'ISMs3ai' or crs[lg] eq 'ISMf3ai',ngd)
                    'JETsWIND2ai': g=where(crs[lg] eq 'WINDs2ai' or crs[lg] eq 'WINDf2ai',ngd)
                    'JETsWIND3ai': g=where(crs[lg] eq 'WINDs3ai' or crs[lg] eq 'WINDf3ai',ngd)
                    ;;IV's
                    'JETs2a': g=where(crs[lg] eq 'ISMs2a' or crs[lg] eq 'ISMf2a' or crs[lg] eq 'JETs2ai' or crs[lg] eq 'WINDs2a' or crs[lg] eq 'WINDf2a',ngd)
                    'JETs3a': g=where(crs[lg] eq 'ISMs3a' or crs[lg] eq 'ISMf3a' or crs[lg] eq 'JETs3ai' or crs[lg] eq 'WINDs3a' or crs[lg] eq 'WINDf3a',ngd)
                    'JETs2b': g=where(crs[lg] eq 'ISMs2b' or crs[lg] eq 'ISMf2b' or crs[lg] eq 'JETs2ai' or crs[lg] eq 'WINDs2b' or crs[lg] eq 'WINDf2b',ngd)
                    'JETs3b': g=where(crs[lg] eq 'ISMs3b' or crs[lg] eq 'ISMf3b' or crs[lg] eq 'JETs3ai' or crs[lg] eq 'WINDs3b' or crs[lg] eq 'WINDf3b',ngd)
                    'JETsISM2a': g=where(crs[lg] eq 'ISMs2a' or crs[lg] eq 'ISMf2a' or crs[lg] eq 'JETsISM2ai',ngd)
                    'JETsISM3a': g=where(crs[lg] eq 'ISMs3a' or crs[lg] eq 'ISMf3a' or crs[lg] eq 'JETsISM3ai',ngd)
                    'JETsISM2b': g=where(crs[lg] eq 'ISMs2b' or crs[lg] eq 'ISMf2b' or crs[lg] eq 'JETsISM2ai',ngd)
                    'JETsISM3b': g=where(crs[lg] eq 'ISMs3b' or crs[lg] eq 'ISMf3b' or crs[lg] eq 'JETsISM3ai',ngd)
                    'JETsWIND2a': g=where(crs[lg] eq 'WINDs2a' or crs[lg] eq 'WINDf2a' or crs[lg] eq 'JETsWIND2ai',ngd)
                    'JETsWIND3a': g=where(crs[lg] eq 'WINDs3a' or crs[lg] eq 'WINDf3a' or crs[lg] eq 'JETsWIND3ai',ngd)
                    'JETsWIND2b': g=where(crs[lg] eq 'WINDs2b' or crs[lg] eq 'WINDf2b' or crs[lg] eq 'JETsWIND2ai',ngd)
                    'JETsWIND3b': g=where(crs[lg] eq 'WINDs3b' or crs[lg] eq 'WINDf3b' or crs[lg] eq 'JETsWIND3ai',ngd)
                    'JETsoISM2a': g=where(crs[lg] eq 'ISMs2a' or crs[lg] eq 'ISMf2a' or crs[lg] eq 'ISMs2b' or crs[lg] eq 'ISMf2b',ngd)
                    'JETsoISM3a': g=where(crs[lg] eq 'ISMs3a' or crs[lg] eq 'ISMf3a' or crs[lg] eq 'ISMs3b' or crs[lg] eq 'ISMf3b',ngd)
                    'JETsoWIND2a': g=where(crs[lg] eq 'WINDs2a' or crs[lg] eq 'WINDf2a' or crs[lg] eq 'WINDs2b' or crs[lg] eq 'WINDf2b',ngd)
                    'JETsoWIND3a': g=where(crs[lg] eq 'WINDs3a' or crs[lg] eq 'WINDf3a' or crs[lg] eq 'WINDs3b' or crs[lg] eq 'WINDf3b',ngd)
;                 else: return
                 endcase 
;                 print,l,g
                 if ngd gt 0 then for k=0,ngd-1 do oplot,[start[j]+10*(j-1),10+10*(j-1)],[g[k]+1.25,l+1.25],color=colors[g[k]]
              endif 
           endfor 
        endif 
        lg=good
     endfor 
;     multiplot2,/reset,/default
;     endif 
;     kk=get_kbrd(10)
;     if kk eq 's' then stop
  endfor 
  
  
  return
end 

pro composite_gold_grbs,cr,gold
  
  cd,!mdata
  begplot,name='~/papers/jetbreaks1/gold_grbs_compare.eps',/encap,/color,font='helvetica'
  colors=[!red,!blue,!green,!orange,!cyan,!magenta,!grey50]
  colors=[colors,colors,colors,colors,colors,colors]

  !x.margin=[10,5]
  !p.charsize=1.
  multiplot2,[2,1],/init
  z1s=' (s) '
  for k=0,1 do begin 
     n=n_elements(gold)
     
     if k eq 0 then begin 
        w=indgen(n_elements(gold)) 
        xticknames=['1','10!U1','10!U2','10!U3','10!U4','10!U5','10!U6','10!U7'];,'10!U10']
        ytitle='Flux (arbitrarily scaled)'
        xrange=[1,1e7]
     endif else begin
        w=where(cr[gold].z gt 0)
        z1s='/(1+z) (s)'
        xticknames=[' ','10!U2','10!U3','10!U4','10!U5','10!U6','10!U7','10!U8']
        ytitle=''
        xrange=[10,1e8]
     endelse 
     multiplot2
     plot,[10.,1e7],[1e-5,1e6],/nodata,xrange=xrange,yrange=[1e-2,1e33],/xlog,/ylog,xtitle='Time since BAT trigger '+z1s,/xsty,/ysty,yticks=2,ytickname=replicate(' ',5),ytitle=ytitle,xtickname=xticknames,xminor=9
     t=[findgen(10)*3.+60.,findgen(9)*100.+100.,findgen(9)*1e3+1e3,findgen(9)*1e4+1e4,findgen(9)*1e5+1e5,findgen(9)*1e6+1e6]
     ff=fltarr(n)
     for i=0d,n-1 do begin
        if k eq 0 or (k eq 1 and cr[gold[i]].z gt 0) then begin
           if k eq 0 then z1=1. else z1=1.+cr[gold[i]].z
           dir=strtrim(cr[gold[i]].grb,2)
           cd,dir
           lc=lcout2fits(/phil)
           read_lcfit,'lc_fit_out_idl_int9.dat',pname,p
           np=n_elements(p)
           case np of 
              2: begin 
                 f=pow(t,p)
              end 
              4: begin
                 w1=where(t le p[2])
                 w2=where(t gt p[2])
                 t=[t[w1],p[2],t[w2]]
                 f=bknpow(t,p)
              end 
              6: begin
                 w1=where(t le p[2])
                 w2=where(t gt p[2] and t lt p[4])
                 w3=where(t gt p[4])
                 t=[t[w1],p[2],t[w2],p[4],t[w3]]
                 f=bkn2pow(t,p)
              end 
              8: begin
                 w1=where(t le p[2])
                 w2=where(t gt p[2] and t lt p[4])
                 w3=where(t gt p[4] and t lt p[6])
                 w4=where(t gt p[6])
                 t=[t[w1],p[2],t[w2],p[4],t[w3],p[6],t[w4]]
                 f=bkn3pow(t,p)
              end 
           endcase

;     x=1.;(i+1.)*10.

           bk=p[np-2]
           mint=min(abs(t-bk/z1),w0)
           x=1./f[w0]*(10^i+1.)
           
           det=where(lc.src_rate_err gt 0,ndet)
           oploterror,lc[det].time/z1,lc[det].src_rate*x,lc[det].src_rate_err*x,psym=3,/nohat,errcolor=colors[i]
           for j=0,n_elements(ndet)-1 do oplot,[lc[det[j]].tstart,lc[det[j]].tstop]/z1,[lc[det[j]].src_rate,lc[det[j]].src_rate]*x,color=colors[i]
           oplot,t/z1,f*x,color=colors[i],thick=2

;     oplot,[bk,bk],[1e-6,1e34],color=colors[i],line=2
           if k eq 0 then oplot,[bk,bk]/z1,[f[w0]*0.1*x,f[w0]*10.*x],color=colors[i],thick=5 else $
              oplot,[bk,bk]/z1,[f[w0]*0.05*x,f[w0]*5.*x],color=colors[i],thick=5
;           ff=f[0]*(10^(i+1.)+1.)
;           ff=f[w0]*x*1e3
           ff[i]=f[0]*x
           if i eq 1 then ff[i]=ff[i]/5.
           if i eq 5 then ff[i]=ff[i]/3.
           if i eq 9 then ff[i]=ff[i]/2.
           if i eq 10 then ff[i]=ff[i]/2.
           if i eq 11 then ff[i]=ff[i]/2.
           if i eq 13 then ff[i]=ff[i]*2.
           if i eq 14 then ff[i]=ff[i]*2.
           if i eq 17 then ff[i]=ff[i]/4.
           if i eq 19 then ff[i]=ff[i]*2.
           if i eq 16 then ff[i]=ff[i]/5.
           if i eq 25 then ff[i]=ff[i]*2.
           
;           ff=lc[det[0]].src_rate*x 
;           ff=lc[det[ndet-1]].src_rate*x
           if k eq 0 then begin 
              xx=1.5
              xyouts,xx,ff[i],dir,color=colors[i],charsize=0.7
           endif 
           cd,'..'
        endif 
     endfor 
     
     oplot,[86400.,86400.],[1e-10,1e34],line=1,thick=5
     xyouts,1e5,5d31,'1 day'
  endfor 
  endplot
  multiplot2,/reset,/default

  return
end

pro plot_jb_example,cr,grb
  
  w=where(strtrim(cr.grb,2) eq grb,nw)
  
  cd,!mdata+'/'+grb
  erase
  multiplot2,/init,[1,nw+1]
  
  multiplot2,ydowngap=0.08
  fit_lc,/justplot,title=grb,charsize=1,/noleg,/nocolor,/noinit,/noxaxis,/phil,/nohard
  
  ;;;loop over segments
  for i=0,nw-1 do begin 
     multiplot2
     crval=fltarr(52)
     crname=tag_names(cr[w[i]])
     crname=crname[6:57]
     for j=0,51 do crs[j]=cr[w[i]].(6+j)
     wcr=where(crs ne 100.,nwcr)
     crval=crval[wcr]
     crname=crname[wcr]
     plot,crval,ind
     
  endfor 
  multiplot2,/reset,default
  cd,!mdata
  
  stop
  return
end 

pro plot_timez,cr,w4,w2g,w3g,w4g
  
  erase
  z=where(cr[w4[w2g]].z gt 0)
  multiplot2,[1,2],/init
  xrange=[1,7]
  yrange=[0,8]
  bin=0.2
  multiplot2
  w2=where(cr[w4[w2g]].tbreak gt 0)
  tb=cr.tbreak
  plot,xrange,yrange,xrange=xrange,yrange=yrange,/xsty,/ysty,/nodata,ytitle='N'
  plothist,alog10(tb[w4[w2g[w2[z]]]]),bin=bin,xrange=xrange,yrange=yrange,/fill,fcolor=!grey30,color=!grey30,/over
  plothist,alog10(tb[w4[w4g[z]]]),bin=bin,xrange=xrange,yrange=yrange,/over,/fill,fcolor=!grey70,color=!grey70
  plothist,alog10(tb[w4[w3g[z]]]),bin=bin,xrange=xrange,yrange=yrange,/over,/fill,/fline,forient=45

  multiplot2
  tb=cr.tbreak/(1.+cr.z)
  w2z=where(tb[w4[w2g[z]]] gt 0)
  plot,xrange,yrange,xrange=xrange,yrange=yrange,/xsty,/ysty,/nodata,ytitle='N',xtitle='log Time (s)'
  plothist,alog10(tb[w4[w2g[z[w2z]]]]),bin=bin,xrange=xrange,yrange=yrange,/fill,fcolor=!grey30,color=!grey30,/over
  plothist,alog10(tb[w4[w4g[z]]]),bin=bin,xrange=xrange,yrange=yrange,/over,/fill,fcolor=!grey70,color=!grey70
  plothist,alog10(tb[w4[w3g[z]]]),bin=bin,xrange=xrange,yrange=yrange,/over,/fill,/fline,forient=45
  multiplot2,/reset,/default
  
  
  return
end 


pro cr_paper_plots
  
  cr=mrdfits(!mdata+'closure_relations_total_2sig.fits',1)
  
  nsig=2
  ;;; Prominent JB example plot
  grb='GRB060605'
  begplot,name='~/papers/jetbreaks1/gold_example.eps',/encap,font='helvetica'
  which_cr_results,cr,grb,/phil,/ps,nsig=nsig,/incaption
  endplot
  
  ;;; Hidden JB example plot
  grb='GRB050802'
;  plot_jb_example,cr,grb
  begplot,name='~/papers/jetbreaks1/silver_example.eps',/encap,font='helvetica'
  which_cr_results,cr,grb,/phil,/ps,nsig=nsig,/incaption
  endplot
  
  ;;; Real II-III example plot
  grb='GRB071020'
  begplot,name='~/papers/jetbreaks1/example_23.eps',/encap,font='helvetica'
  which_cr_results,cr,grb,/phil,/ps,nsig=nsig,/incaption
  endplot
  
  ;;; Probable III-IV example plot
  grb='GRB051008'
  begplot,name='~/papers/jetbreaks1/example_34.eps',/encap,font='helvetica'
  which_cr_results,cr,grb,/phil,/ps,nsig=nsig,/incaption,segment=['III','IV']
  endplot
  
  ;;; Probable II-IV example plot
  grb='GRB061201'
  begplot,name='~/papers/jetbreaks1/example_24.eps',/encap,font='helvetica'
  which_cr_results,cr,grb,/phil,/ps,nsig=nsig,/incaption,segment=['II','IV']
  endplot
    
  ;;;time plots
  begplot,name='~/papers/jetbreaks1/time_plots.eps',/encap,font='helvetica'
  maybe_jet_break,cr,w,last,nsig=nsig
  multiplot2,[1,5],/init
  xrange=[1,7]
  yrange=[0,90]
  bin=0.2
  multiplot2
  plothist,alog10(cr[last].tstart),xrange=xrange,yrange=yrange,min=xrange[0],max=xrange[1],ytitle='N',/fill,bin=bin,/xsty,/ysty
  legend,'t!Lstart!N',box=0,/top,/right
  multiplot2
  plothist,alog10(cr[last].tstop),xrange=xrange,yrange=yrange,min=xrange[0],max=xrange[1],ytitle='N',/fill,bin=bin,/xsty,/ysty
  legend,'t!Lstop!N',box=0,/top,/right
  multiplot2
  plothist,alog10(cr[last].tlastdet),xrange=xrange,yrange=yrange,min=xrange[0],max=xrange[1],ytitle='N',/fill,bin=bin,/xsty,/ysty
  legend,'t!Llast det!N',box=0,/top,/right
  multiplot2
  plothist,alog10(cr[last].tlastpos),xrange=xrange,yrange=yrange,min=xrange[0],max=xrange[1],ytitle='N',/fill,bin=bin,/xsty,/ysty;,xtitle='log time (s)'
  legend,'t!Ljet break limit!N',box=0,/top,/right
  multiplot2
  plothist,alog10(cr[last].tbreak),xrange=xrange,yrange=yrange,min=xrange[0],max=xrange[1],ytitle='N',/fill,bin=bin,/xsty,/ysty,xtitle='log time (s)'
  legend,'t!Llast break!N',box=0,/top,/right
  multiplot2,/reset,/default
  endplot
  
  return
end 
