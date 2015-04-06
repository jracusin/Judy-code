pro catalogs

  readcol,'~/Fermi/3FGL_list_for_Judy.txt',source,ra,dec,sig,flux100,flux100err,flux1000,flux1000err,format='(a,f,f,f,d,d,d,d)'


  total=[205,1451,1873,3032]
  unid=[51,671,633,901]
  year=[2009,2010,2011,2014]+0.5
  name=['0FGL','1FGL','2FGL','3FGL']

  b=barplot(year,unid,name='Unidentified Sources',xrange=[2008,2016],ytitle='Number of Sources',fill_color='yellow',width=0.8,xtitle='Year',yrange=[0,3300],aspect_ratio=0.002,xminor=1,font_name='Helvetica')
  b2=barplot(year,total,name='Total',bottom_values=unid,fill_color='blue',/overplot,width=0.8)
  t=text(year-0.35,total+100,name,/data,font_style='bold')
  l=legend(/data,target=[b,b2],text_color='black',position=[2012,3100])
  

;  a=axis(0,location=[2008,0],/data,textpos=1,tickdir=1,minor=0)
;  t=text(2008.7,1250,'Submitted',color='blue',/data,font_style='bold')

  b.save,'~/Fermi/Senior_Review/SR2014/slides/catalogs.png'
  b.refresh
  b.close


return
end 

pro novae
  
  cd,'~/Fermi/Senior_Review/SR2014/slides'
  files=['Cyg.txt','Sco.txt','Mon.txt','Del.txt','Cen.txt']
  name=['Cyg','Sco','Mon','Del','Cen']

  for i=0,n_elements(files)-1 do begin 
     readcol,files[i],t,f,ferr,format='(f,f,f)'
     f=f*1e-7
     ferr=ferr*1e-7
     det=where(ferr ne 0,ndet)
     p=errorplot(t[det],f[det],replicate(0.5,ndet),ferr[det],yrange=[0,2e-6],xrange=[-5,30],xtitle='Days',ytitle='Flux (ph cm!U-2!N s!U-1!N)',errorbar_capsize=0,symbol='o',/sym_filled,line='none',aspect_ratio=1e7,margin=[0.17,0.0,0.05,0.0],font_name='Helvetica')
     ul=where(ferr eq 0,nul)
     s=symbol(t[ul],f[ul]-6e-8,sym_text='$\downarrow$',sym_color='grey',sym_size=1.5,/data)
     p2=errorplot(t[ul],f[ul],replicate(0.5,nul),replicate(0.,nul),color='grey',line='none',/overplot,errorbar_capsize=0)
;     oplot,t[ul],f[ul],color=!grey,psym=8
     p.save,'~/Fermi/Senior_Review/SR2014/slides/nova_'+name[i]+'.png'
     p.refresh
     k=get_kbrd(10)
     if k eq 's' then stop
     p.close

  endfor

return
end 

pro timeline

  ;; add y-axis labels (crab different)
  ;; add PSR J1023 - data below from SR prop plot
  
  start=2008.5
  stop=2015
  dur=(stop-start)*365.25

  cd,'~/Fermi/Senior_Review/SR2014/slides'
;  files=['LSI+61303_86400.lc','3C454.3_86400.lc','PKS0507+17_86400.lc','PKS0528+134_86400.lc','S30218+35_86400.lc','S50836+71_86400.lc','all_instruments_crab_plot_points_thrujan2014.fits']
  files=['LSI+61303_86400.lc','judyaperture/3C454.3aperture.4day.fits',$
         'judyaperture/PKS0507+17.aperture8day.fits',$ ;'PKS0528+134_604800.lc',
         'judyaperture/B0218.aperture4day.fits','judyaperture/0836+71.aperture4day.fits',$
         'all_instruments_crab_plot_points_thrujan2014.fits',$
         'PSR1259_lc.fits',$                      ;'PSR1259_13_08_02_04_08_22_tcs_lc.fits',
         'PSRJ1023+0038.fits']
  source=['LSI+61 303','3C 454.3','PKS 0507+17',$;'PKS 0528+134',
          'S3 0218+35','S5 0836+71','Crab','PSR B1259-63','PSR J1023+0038']
  outname=strtrim(source,2)+'.png'
  what=['Binary with orbital period (26.6 days) & superorbital period (4.5 years)','Bright Blazar flare',$
       'Blazar not detectable until flare in April 2013',$;'EGRET Blazar',
        'Lensed Blazar','z>2 Blazar','GBM Hard X-ray Evolution',$
        'Binary with 3.4 year period','Variable Pulsar']
  yranges=[[4e-7,1.5e-6],[-2e-6,8e-6],[-1e-7,6e-7],$;[0,2e-7],
           [-2e-7,3e-6],[-3e-8,3e-7],[0.9,1.05],[-0.5e-6,2.5e-6],[-1e-8,2e-7]]
  nfiles=n_elements(files)
  met=date2met('2008-06-01-00:00:00')
  day=86400.

  g=0
  n=nfiles-1
  colprint,indgen(nfiles),source
  stop

  for i=g,n do begin 
     ytitle='Flux (ph cm!U-2!N s!U-1!N)'
     if i eq -1 then begin
        mjd=[54770,54936,55100,55275,55440,55600,55780,55950,56120,56286]
        daycnv,mjd+2400000.5,yr,mn,d,hr
        t=yr+mn/12.+d/365.+hr/24./365.
        f=[0.755,0.873,0.983,0.999,0.839,0.899,0.852,0.791,0.717,0.820]*1e-6
        err=replicate(0.2,10)*1e-6
        xerr=replicate(75/365.,n_elements(f))
        ylog=0
        bul=replicate('F',n_elements(f))        

     endif 
     if i eq 2 then begin 
        l=mrdfits(files[i],1)
     
        w=where(l.flux_300_1000 gt 0 and l.error_300_1000 gt 0 and l.flux_300_1000-l.error_300_1000 gt 0)
        l=l[w]
        f=l.flux_300_1000
        err=l.error_300_1000
        bul=l.ul_300_1000
        t=((l.start-met)/day)/365.25+2008.5
        xerr=fltarr(n_elements(f))
        ylog=0
;        p90a=percentile(f+err,0.99,ind90a)
;        p90b=percentile(f-err,0.99,ind90b)
;        fmin=p90b[0]
;        fmax=p90a[1]
        
;        yrange=10.^(round(alog10([fmin,fmax])*10.)/10.+[-0.5,0.2])
;        yrange=[-fmax/2.,fmax]
;        print,fmin,fmax
;        print,yrange
     endif
     if i eq 5 then begin 
        l=mrdfits(files[i],1)
     
        f=l.gbm_flux_15_50kev
        err=l.gbm_err_15_50kev
        jd=l.gbm_time_15_50kev+2400000.5
        daycnv,jd,yr,mn,d,hr
        t=yr+mn/12.+d/365.+hr/24./365.
        bul=replicate('F',n_elements(f))
        ylog=0
        xerr=replicate(20/365.,n_elements(f))
        ytitle='Flux (crab)'
;        yrange=[0.9,1.05]
     endif 
     if i ge 1 and i le 4 or i eq 6 or i eq 7 then begin
;        readcol,files[i],mjd,f,err
;        f=f*1e-6
;        err=err*1e-6
        l1=mrdfits(files[i],1)
;        l2=mrdfits(files[i],2)
        w=where(l1.error/l1.rate lt 10.)
        l1=l1[w]
;        l2=l2[w]

        f=l1.rate
        err=l1.error
        mjd=l1.time

;        wul=where(l2.ts lt 5)
;        f[wul]=l2[wul].upperlim
;        bul=replicate('F',n_elements(f))
;        bul[wul]='T'
        daycnv,mjd+2400000.5,yr,mn,d,hr
        t=yr+mn/12.+d/365.+hr/24./365.
;        wul=where(err eq 0)
        bul=strarr(n_elements(f))
        bul[*]='F'
;        bul[wul]='T'
        xerr=fltarr(n_elements(f))
        xerr[*]=3.5/365.
        if i eq 7 then xerr[*]=15./365.
;        yrange=[-0.5,2.5]*1e-6
     endif 
     ;; if i eq 6 then begin
     ;;    readcol,files[i],mjd,f,err,format='(d,d,d)'
     ;;    daycnv,mjd+2400000.5,yr,mn,d,hr
     ;;    t=yr+mn/12.+d/365.+hr/24./365.
     ;;    bul=replicate('F',n_elements(f))
     ;;    xerr=fltarr(n_elements(f))
     ;;    xerr[*]=3.5/365.
        
;;     endif 
     if i eq 7 then begin
;         readcol,files[i],metstart,metstop,flux,fluxerr,ul,format='(d,d,d,d,d)'
         
;         wul=where(ul lt 1,nul)
;         bul=strarr(n_elements(flux))
;         bul[*]='F'
;         bul[wul]='T'

;         d0=date2met('2008-06-01-00:00:00',/fermi)
;         year=86400.*365.25
;         yearstart=(metstart-d0)/year
;         yearstop=(metstop-d0)/year
;         t=(yearstop+yearstart)/2.+2008.5
;         xerr=(yearstop-yearstart)/2.
;         f=flux
;         err=fluxerr
         ytitle='Flux (erg cm!U-2!N s!U-1!N)'
         
      endif 

     yrange=yranges[*,i]

     m=median(f)

     ul=where(bul eq 'T' and f lt yrange[1] and f gt yrange[0],nul)
     det=where(bul eq 'F')

     p=plot([start,stop],yrange,axis_style=1,/nodata,xrange=[start,stop],dimensions=[600,110],ylog=ylog,yrange=yrange,font_size=6,xtitle='Year',font_name='Helvetica',ytitle=ytitle);,ytickvalues=yrange,ytickname=[' ',' '])
;     p2=plot([2014.25,2014.25],yrange,linestyle=':',/overplot)
          

;     s=(1.-0.01)/((f[ind90a[1]]+err[ind90a[1]])-(f[ind90b[0]]-err[ind90b[0]]))
;     s=1.
;     m=median(f*s)
;     mm=m-0.1

     nf=n_elements(f)
     xx=[t[0]-xerr[0],t,max(t+xerr),max(t+xerr),reverse(t),t[0]-xerr[0]]
     yy=[f[0]+err[0],f+err,f[nf-1]+err[nf-1],f[nf-1]-err[nf-1],reverse(f-err),f[0]-err[0]]
     p3b=polygon(xx,yy,fill_color='blue',transparency=60,/data)

     if nul gt 0 then  begin 
;        flin=(alog10(f[ul])-alog10(yrange[0]))/(alog10(yrange[1])-alog10(yrange[0]))
        flin=(f[ul]-yrange[0])/(yrange[1]-yrange[0])
        tnorm=(t[ul]-start)/(stop-start)
        p3a=symbol(tnorm,flin,sym_text='$\downarrow$',sym_color='grey',sym_size=0.5,target=p,/relative)
;        err[ul]=0
     endif 
     p3=errorplot(t[det],f[det],xerr[det],err[det],errorbar_capsize=0,/overplot,linestyle='')
     txt=text(0.15,0.8,source[i],font_size=8)
     txt2=text(0.15,0.7,what[i],font_size=6)

     case i of 
        0: begin 
           n=20.
           tper=26.6                                     ;; period time
           t0=date2met('1977-08-11-06:36:00')+(1667*86400.*5) ;43366.275
           t0=t0/86400./365.4
           per=sin((findgen(n)-n/4.)/n*360.*!dtor)*0.5 ;; sinusoidal angles over one period
           nper=(dur-t0*365.25*2)/tper            ;;; number of periods in mission
           tp=findgen(n*nper)/n*tper/365.25+start+t0 ;; time for each bin in each period
           period=per
           for j=1,nper do period=[period,per]
;           p4=plot(tp,period*m+m,color='red',/overplot,transparency=50)
           
           tper=1667.                            ;; period time
           per=sin((findgen(n)-n/4.)/n*360.*!dtor)*0.5 ;; sinusoidal angles over one period
           nper=(dur-t0*365.25*2)/tper                 ;;; number of periods in mission
           tp=findgen(n*nper)/n*tper/365.25+start+t0   ;; time for each bin in each period
           period=per
           for j=1,nper do period=[period,per]
           p5=plot(tp,period/3.*m+m,color='red',/overplot,transparency=50,thick=5)
        end
        6: begin
           per=3.4 ;; orbital period
;           t2=t[det]+3.4
;           f2=f[det]
;           p4=plot(t2,f2,color='blue',/overplot,transparency=50,line=':')
           t2=2011.1+per
;           sym=symbol(t2,1.3e-6,sym_text='?',sym_size=2,sym_color='blue',/data)
;           sym2=arrow([2013,2014],[1e-6,1e-6],color='blue',/data,line_thick=2)

        end 
        else:
     endcase 
     print,i,source[i]
     p2=plot([start,stop],[m,m],linestyle='--',color='green',/overplot)


     p.save,'~/Fermi/Senior_Review/SR2014/slides/'+outname[i]
     p.refresh
;     k=get_kbrd(10)
;     if k eq 's' then stop
     p.close
  endfor 
  stop
  return
end 

pro data_queries

  readcol,'~/Fermi/Senior_Review/SR2014/daily_queries.csv',date,q,format='(a,f)'
  year2=strmid(date,0,4)
  mn=strmid(date,5,2)
  day=strmid(date,8,2)
  year2=year2+day/30./12.+(mn-1.)/12.
 
  y2=0
  for i=0,n_elements(year2)-1 do if q[i] gt 0 then y2=[y2,replicate(year2[i],q[i])]
  y2=y2[1:*]
  w=where(y2 lt 2013.99)
  y2=y2[w]

  readcol,'~/Fermi/Senior_Review/SR2014/helpdesk_queries.csv',week,queries
  ;; weeks since Feb 1, 2009
  year=2009.+(31.+week*7.)/365.
  y=0
  for i=0,n_elements(year)-1 do if queries[i] gt 0 then y=[y,replicate(year[i],queries[i])]
  y=y[1:*]

  begplot,name='~/Fermi/Senior_Review/SR2014/data_queries.eps',/land,/color,/encap,font='helvetica'
  !y.margin=[0,0]
  !x.margin=[5,0]
  multiplot2,[1,2],/init
  multiplot2
  ytitle='Number of Queries per Week'
  plot,[0,0],[0,0],/nodata,xrange=[2009,2014],/xsty,psym=10,charsize=2,yrange=[0,1.1e4],xminor=4,xticks=5,/ysty

;  polyfill,[min(year),y,max(year),min(year),min(year)],[0,qu,0,0,y[0]],/fill,color=!blue
  plothist2,y2,color=!black,/over,bin=7./365.,/fill,fcolor=!blue
;  legend,'Data Queries',textcolor=!blue,box=0,/top,/left,charsize=2
  xyouts,2010,8000,'  Data',color=!blue,charsize=2
  xyouts,2010,7000,'Queries',color=!blue,charsize=2
  
  axis,xaxis=0,xtickformat='(A1)',xminor=4,xticks=5;,xticks=2,xtickv=[2010,2011,2012]
  xyouts,2009.6,500,'Release of photon data',orient=90,charsize=1.6

  multiplot2,yupgap=0.2
  plot,[0,0],[0,0],/nodata,xrange=[2009,2014],/xsty,psym=10,xtitle='Year',charsize=2,yrange=[0,60],yminor=2,xminor=4,xticks=5
  plothist,y,x,y1,color=!black,/over,bin=7./365.,/fill,fcolor=!red,xminor=12
;  oplot,x,y1*10.,color=!red,psym=10
  xyouts,2008.45,10,ytitle,orient=90,charsize=2
;  legend,'Helpdesk Queries',textcolor=!red,box=0,/top,/left,charsize=2
  xyouts,2009.7,48,'Helpdesk Queries',color=!red,charsize=2
;  xyouts,2009.1,32,' Queries',color=!red,charsize=2

  oplot,[2011.+(7./12.),2011.+(7./12.)],[0,60],line=2
  xyouts,2011.65,42,'Pass 7',charsize=1.6
  oplot,[2011.5,2011.5],[0,28],line=2
  xyouts,2011.2,30,'2FGL',charsize=1.6
;  oplot,[2011.,2011.]+(4./12),[0,50],line=2
;  xyouts,2011.1,40,'  Fermi',charsize=1.6
;  xyouts,2011.1,35,'Symposium',charsize=1.6

  oplot,[2010.,2010.]+(8./12),[0,38],line=2
  xyouts,2010.3,40,'software release',charsize=1.6

  oplot,[2010.,2010.]+0.5/12.,[0,34],line=2
  xyouts,2009.9,36,'1FGL',charsize=1.6

  oplot,[2009.13,2009.13],[0,60],line=2
  xyouts,2009.19,30,'0FGL',charsize=1.6

  oplot,[2013.92,2013.92],[0,60],line=2
  xyouts,2013.1,30,'Pass 7 Rep',charsize=1.6

  
;  oplot,[2009.,2009.]+(10./12),[0,40],line=2
;  xyouts,2009.7,45,'  Fermi',charsize=1.6
;  xyouts,2009.7,40,'Symposium',charsize=1.6


;  xyouts,2010.68,40,'release',charsize=1.6
  axis,xaxis=0,xtickformat='(A1)',xminor=4,xticks=5
  multiplot2,/reset,/default
  endplot
  spawn,'convert ~/Fermi/Senior_Review/SR2014/data_queries.eps ~/Fermi/Senior_Review/SR2014/data_queries.pdf'


; p=bibplot()

stop
return
end 

pro g2

  cd,'~/Fermi/Senior_Review/SR2014/Rodrigo'
  files=['year10.dat','year30.dat','year50.dat','year100.dat','month10.dat','month30.dat','month50.dat','month100.dat','day10.dat','day30.dat','day50.dat','day100.dat']
;  f=[0,3,4,7,8,11]
;  files=files[f]
  nf=n_elements(files)

  for i=0,nf-1,4 do begin
     readcol,files[i],logacc0,logsig0,/silent
     readcol,files[i+1],logacc1,logsig1,/silent
     readcol,files[i+2],logacc2,logsig2,/silent
     readcol,files[i+3],logacc3,logsig3,/silent
     case i of
        0: begin           
           sig1=10^[-1,logsig0,reverse(logsig3),-1]
           acc1=10^[-5.35,logacc0,reverse(logacc3),-8]
           x1=10^[logacc0,logacc1,logacc2,logacc3]
           y1=10^[logsig0,logsig1,logsig2,logsig3]
           p1=[replicate(10,n_elements(logsig0)),replicate(30,n_elements(logsig1)),replicate(50,n_elements(logsig2)),replicate(100,n_elements(logsig3))]
     end 
        4: begin           
           sig2=10^[-1,logsig0,reverse(logsig3),-1]
           acc2=10^[-5.32,logacc0,reverse(logacc3),-8]
           x2=10^[logacc0,logacc1,logacc2,logacc3]
           y2=10^[logsig0,logsig1,logsig2,logsig3]
           p2=[replicate(10,n_elements(logsig0)),replicate(30,n_elements(logsig1)),replicate(50,n_elements(logsig2)),replicate(100,n_elements(logsig3))]        
        end 
        8: begin           
           sig3=10^[-1,logsig0,reverse(logsig3),-1]
           acc3=10^[-5.32,logacc0,reverse(logacc3),-8]
;           sig3=10^[logsig0,reverse(logsig3)]
;           acc3=10^[logacc0,reverse(logacc3)]
           x3=10^[logacc0,logacc1,logacc2,logacc3]
           y3=10^[logsig0,logsig1,logsig2,logsig3]
           p3=[replicate(10,n_elements(logsig0)),replicate(30,n_elements(logsig1)),replicate(50,n_elements(logsig2)),replicate(100,n_elements(logsig3))]        
        end 
     endcase
  endfor 

  xrange=[1e-8,1e-5]
  yrange=[1,1e3]

  p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/xlog,/ylog,xtitle='Accretion Rate ($M_{\odot}\ yr^{-1}$)',ytitle='Significance of $\gamma$ ray Variability ($\sigma$)',title='Detection of G2 Cloud Accreting onto Sgr A*')
;  p=polygon(acc2,sig2,/overplot,/data,transparency=40,fill_color='blue')
;  p=polygon(acc3,sig3,/overplot,/data,transparency=40,fill_color='green')
  for i=0,2 do begin 
     case i of 
        0: begin
           pp=p1 & acc=acc1 & sig=sig1 & x=x1 & y=y1 & color='red' & dur='year' & y100=[50,2e3]
        end
        1: begin 
           pp=p2 & acc=acc2 & sig=sig2 & x=x2 & y=y2 & color='blue' & dur='month' & y100=[1.5,30]
        end 
        2: begin 
           pp=p3 & acc=acc3 & sig=sig3 & x=x3 & y=y3 & color='green' & dur='day' & y100=[0.3,0.6]
        end 
     endcase 
;     acc=acc[1:n_elements(acc)-2]
;     sig=sig[1:n_elements(sig)-2]

     p=polygon(acc,sig,/overplot,/data,transparency=40,fill_color=color)
     if i eq 0 then t=text(0.15,0.8,'Accretion lasting:')
     t=text(0.2,0.75-0.03*i,dur,font_color=color)
     
     w100=where(pp eq 100)
     w50=where(pp eq 50)
     w30=where(pp eq 30)
     w10=where(pp eq 10)
;     p=plot(x[w100],y[w100],/overplot,/data,thick=5)
;     p=plot(x[w50],y[w50],/overplot,/data,thick=5,color='dark slate grey')
;     p=plot(x[w30],y[w30],/overplot,/data,thick=5,color='grey')
;     p=plot(x[w10],y[w10],/overplot,/data,thick=5,color='light grey')
;     t=text(1e-7,y100[0],'100%',/data)
;     t=text(5e-6,y100[1],'10%',/data,font_color='white')
  endfor 
  t=text(1e-7,10,'100% of $\gamma$ ray flux due to jet',/data,orientation=37)
  t=text(4e-6,1.2,'10% of $\gamma$ ray flux due to jet',/data,orientation=80,font_color='white')
  l=plot([1e-8,1e-5],[3.,3.],linestyle='--',/data,/overplot)

  p.save,'~/Fermi/Senior_Review/SR2014/g2.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close


;;      case i of
;;         0: begin           
;;            sig1=10^[logsig0,logsig1,logsig2,logsig3]
;;            acc1=10^[logacc0,logacc1,logacc2,logacc3]
;;            p1=[replicate(100,n_elements(logsig0)),replicate(50,n_elements(logsig1)),replicate(30,n_elements(logsig2)),replicate(10,n_elements(logsig3))]
;;            s=sort(acc1)
;;            x=dblarr(n_elements(acc1),n_elements(sig1)) & y=x & z=x
;;            for j=0,n_elements(acc1)-1 do x[j,*]=acc1[s[j]]
;;            for j=0,n_elements(sig1)-1 do y[*,j]=sig1[s[j]]
;;            for j=0,n_elements(acc1)-1 do z[j,j]=p1[s[j]]

;;         end 
;;         4: begin
;;            sig2=10^[logsig0,logsig1,logsig2,logsig3]
;;            acc2=10^[logacc0,logacc1,logacc2,logacc3]
;;            p2=[replicate(100,n_elements(logsig0)),replicate(50,n_elements(logsig1)),replicate(30,n_elements(logsig2)),replicate(10,n_elements(logsig3))]
;;         end 
;;         8: begin
;;            sig3=10^[logsig0,logsig1,logsig2,logsig3]
;;            acc3=10^[logacc0,logacc1,logacc2,logacc3]
;;            p3=[replicate(100,n_elements(logsig0)),replicate(50,n_elements(logsig1)),replicate(30,n_elements(logsig2)),replicate(10,n_elements(logsig3))]
;;         end 

;;      endcase 
;;   endfor 

;;   w=where(z eq 0)
;;   zz=bilinear(z,w,w)
;;   p=contour(zz,x,y,/xlog,/ylog,rgb_table=8)
   
;; ;   p=plot(acc1,sig1,/data,/overplot,/xlog,/ylog,symbol='X')
;;    stop
  
;; ;;   z0=dblarr(n_elements(logsig0),n_elements(logacc0))
;; ;;   x0=z0 & y0=z0
;; ;;   for i=0,n_elements(logsig0)-1 do x0[i,*]=10^logsig0
;; ;;   for i=0,n_elements(logacc0)-1 do y0[*,i]=10^logacc0
;; ;;   w=where(x0 eq x and y0 eq y)
;; ;;   z0[w]=100

;; ;;   z1=dblarr(n_elements(logsig1),n_elements(logacc1))
;; ;;   x1=z1 & y1=z1
;; ;;   for i=0,n_elements(logsig1)-1 do x1[i,*]=10^logsig1
;; ;;   for i=0,n_elements(logacc1)-1 do y1[*,i]=10^logacc1
;; ;;   w=where(x1 eq x and y1 eq y)
;; ;;   z1[w]=50
  

;;   stop
;;   xrange=[1e-8,1e-5]
;;   xlog=alog10(xrange)
;;   nxlog=xlog[1]-xlog[0]
;;   yrange=[1e-1,1e5]
;;   ylog=alog(yrange)
;;   nylog=ylog[1]-ylog[0]

;;   d=dindgen(9)+1
;;   x=0d & y=0d
;;   for i=0,nxlog-1 do x=[x,d*10^(xlog[0]+i)]
;;   x=[x[1:*],xrange[1]]
;;   nx=n_elements(x)
;;   for i=0,nylog-1 do y=[y,d*10^(ylog[0]+i)]
;;   y=[y[1:*],yrange[1]]
;;   ny=n_elements(y)
;;   z=dblarr(nx,ny)
;;   xx=dblarr(nx,ny)
;;   yy=xx
;;   for i=0,nx-1 do xx[i,*]=x
;;   for i=0,ny-1 do yy[*,i]=y

;;   w1=where(xx ge 10^logacc0 and xx lt 10^logacc1 and yy ge 10^logsig0 and yy lt 10^logsig1)
;;   z[w1]=75
;;   w2=where(xx ge 10^logacc1 and xx lt 10^logacc2 and yy ge 10^logsig1 and yy lt 10^logsig2)
;;   z[w2]=40
;;   w3=where(xx ge 10^logacc2 and xx lt 10^logacc3 and yy ge 10^logsig2 and yy lt 10^logsig3)
;;   z[w3]=20

;;   rdis,z

;; ;  sig=interpolate(x,sig1)
;; ;  acc=interpolate(y,acc1)

;; stop
;;   p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,/xlog,/ylog)
;;   p1=contour(sig1,acc1,/overplot,/data,transparency=20,rgb_table=62)
;;   p2=contour(sig2,acc2,/overplot,/data,transparency=20,rgb_table=57)
;;   p3=contour(sig3,acc3,/overplot,/data,transparency=20,rgb_table=53)
;;   l=plot([1e-8,1e-5],[3.,3.],linestyle='--',/data,/overplot)

  stop
  return
end 

pro prop_stats

  prop=0L & lastname='' & dup='' & firstname='' & acycle=0

  readcol,'~/Fermi/Senior_Review/general/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

  prop=prop[1:*]
  lastname=last[1:*]
  firstname=first[1:*]
  approv=approv[1:*]
  pc=pc[1:*]
  inst=inst[1:*]
  acycle=prop;strmid(prop,0,1)

  prop=long(prop)
  w=where(pc eq 'PI',nw)
  nw=nw*1.
  c2=where(prop[w] ge 20000 and prop[w] lt 30000,nc2)
  c3=where(prop[w] ge 30000 and prop[w] lt 40000,nc3)
  c4=where(prop[w] ge 40000 and prop[w] lt 50000,nc4)
  c5=where(prop[w] ge 50000 and prop[w] lt 60000,nc5)
  c6=where(prop[w] ge 60000 and prop[w] lt 70000,nc6)

  print,nc2,nc3,nc4,nc5,nc6
  name=lastname+firstname

  w=where(pc eq 'PI' and approv eq 'Y')
  s=rem_dup(name[w])
  i=rem_dup(inst[w])

  help,s,i
  
  w=where(approv eq 'Y')
  s2=rem_dup(name[w])
  help,s2

stop
  s=rem_dup(name)
  names=name[s]
  props=prop[s]
  approvs=approv[s]
  lastnames=lastname[s]
  firstnames=firstname[s]
  acycle=acycle[s]

  wa=where(approvs eq 'Y')
  accname=names[wa]
  accprop=props[wa]


  stop
  return
end 
pro coi_plot,names,lastnames,firstnames,acycle,both=both
  
  if keyword_set(both) then add='_acc' else add=''
  cycle=indgen(5)+2
  
  prop=0L & lastname='' & dup='' & firstname='' & acycle=0
  readcol,'~/Fermi/Senior_Review/general/GI_CoIs_select.csv',prop,last,first,inst,ctry,pc,approv,format='(l,a,a,a,a,a,a)',skip=1,delim=',',/silent

  prop=prop[1:*]
  lastname=last[1:*]
  firstname=first[1:*]
  approv=approv[1:*]
  pc=pc[1:*]
  acycle=prop;strmid(prop,0,1)

  name=lastname+firstname

  s=rem_dup(name)
  names=name[s]
  props=prop[s]
  approvs=approv[s]
  lastnames=lastname[s]
  firstnames=firstname[s]
  acycle=acycle[s]

;  name=last+first
  wa=where(approvs eq 'Y')
  accname=names[wa]
  accprop=props[wa]

  n=lonarr(5) & nacc=n
  for i=1,6 do begin
     w=where(props ge 10000L*i and props lt 10000L*(i+1),nw)
     ws=where(accprop ge 10000L*i and accprop lt 10000L*(i+1),nwacc)
     n[i-2]=nw
     nacc[i-2]=nwacc
     print,10000L*i,10000L*(i+1),nw,nwacc
  endfor 
  cc=cumulative(n)
  cc5=cumulative(nacc)

  b=barplot(cycle+2007,cc,xrange=[2008.5,2013.5],ytitle='Cumulative Number of Guest Investigators',fill_color='blue',width=0.4,xtitle='Year',yrange=[0,1400],aspect_ratio=0.002,xminor=0)
  b2=barplot(cycle+2007+0.1,cc5,/overplot,transparency=25,fill_color='red',width=0.4)
  a=axis(0,location=[0,1400],/data,coord_transform=[-2007,1.],title='Cycle',textpos=1,tickdir=1,minor=0)
  t=text(2008.7,1250,'Submitted',color='blue',/data,font_style='bold')
  t=text(2008.7,1130,'Accepted',color='red',/data,font_style='bold')

  b.save,'~/Fermi/Senior_Review/SR2014/gis.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close

return
end 

pro theses
  
  readcol,'~/Fermi/Senior_Review/SR2014/theses.txt',year,skip=1,format='(i)'
  plothist,year,x,y,/noplot
  b=barplot(x+0.5,y,ytitle='       PhD Theses per year',fill_color='blue',xrange=[2000,2014],yrange=[0,60],xmajor=0,xminor=0,aspect_ratio=0.1,yminor=1)
  xaxis=axis(0,location=[0,60],/data,coord_transform=[-2008.44,1.],title='Mission Year',textpos=1,tickdir=1,minor=1)
  xaxis=axis(0,location=[0,0],/data,title='Year',minor=1)

  p=plot([2008.44,2008.44],[0,60],/overplot,linestyle=':')
  t=text(2008.2,20,'Launch',orientation=90,/data)
  t=text(0.07,0.30,'Fermi',font_style='it',orientation=90)

  b.save,'~/Fermi/Senior_Review/SR2014/theses_plot.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close

return
end 

pro too

  ;;; PLOT FERMI TOOS AS A FUNCTION OF MISSION TIME

  readcol,'~/Fermi/Senior_Review/SR2014/TOOs.csv',date,pi,target,duration,format='(a,a,a,i)',delim=',',skip=1,/silent

  w=where(date ne '')
  n=n_elements(date[w])
  
  ;; skip duplicates
  skip=[2,8]
  ind=intarr(n)
  ind[*]=1
  ind[skip]=0
  w=where(ind eq 1,n)
  date=date[w]

  met=dblarr(n)
  for i=0,n-1 do begin
     met[i]=date2met(date[i],/fermi)
  endfor 

  met0=date2met('2008-06-11-12:00:00')
  d=met-met0
  year=365.25*86400.
  years=d/year

  plothist,years,x,y,/noplot,bin=6/12.

;  x=[2010,2011,2012,2013]
;  y=[18,16,12.5,43.5]

  too=[[2010,098,3.5],[2010,266,4.5],[2010,361,10],[2011,087,4],[2011,104,7],[2011,243,2],[2011,250,3],[2012,068,2],[2012,186,3.5],[2012,268,7],[2013,66,4],[2013,104,1.5],[2013,134,5.5],[2013,228,5.5],[2013,234,6],[2013,240,7.5],[2013,240,7.5],[2013,291,3.5],[2013,298,4],[2013,313,1],[2013,339,5]]
  years=too[0,*]+too[1,*]/365.
  dur=too[2,*]

  bin=3./12.
  n=16
  y=fltarr(n) & x=y
  for i=0,n-1 do begin
     w=where(years ge 2010+i*bin and years lt 2010+(i+1)*bin,nw)
     if nw gt 0 then y[i]=total(dur[w])
     x[i]=2010+i*bin
;     if nw gt 0 then print,years[w]
;     print,x[i],y[i]
;     print
  endfor 
;  b=barplot(2008.5+x,y,ytitle='         Target of Opportunity
;  Observations',fill_color='blue',xrange=[2009,2014],yrange=[0,6],xmajor=0,xminor=0,aspect_ratio=0.4,yminor=0)
  b=barplot(x,y,ytitle='Days spent on TOO Observations',fill_color='blue',xrange=[2009,2014],yrange=[0,30],xmajor=0,xminor=0,aspect_ratio=0.1,yminor=0)
  xaxis=axis(0,location=[0,30],/data,coord_transform=[-2008.5,1.],title='Mission Year',textpos=1,tickdir=1,minor=3)
  xaxis=axis(0,location=[0,0],/data,title='Year',minor=3)
  p=plot([2009.65,2009.65],[0,50],/overplot,linestyle='--')
  t=text(2009.55,5,'TOO capability became available to community',orientation=90,/data,font_size=8)
;  t=text(0.067,0.22,'Fermi',font_style='it',orientation=90)

  b.save,'~/Fermi/Senior_Review/SR2014/too_plot.png'
  b.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  b.close
  
stop

return
end 

pro curved_polygon,x0,x1,y0,y1,x,y,rightside=rightside,ylog=ylog,xlog=xlog,ry=ry,rx=rx,xm=xm,ym=ym ;,color=color,noplot=noplot

  ang=indgen(90)*!dtor
  if n_elements(rx) eq 0 then rx=0.1d
  if n_elements(ry) eq 0 then begin 
     ry=0.5d
     if y1-y0 lt ry then ry=0.2d
  endif 
  up=0
  if n_elements(xm) gt 0 then if xm[0] ne 0 then up=1

  if keyword_set(xlog) then begin 
     ax0=alog10(x0)
     ax1=alog10(x1)
     if up then axm=alog10(xm)
  endif else begin 
     ax0=x0
     ax1=x1
     if up then axm=xm
  endelse 
  if keyword_set(ylog) then begin 
     ay0=alog10(y0)
     ay1=alog10(y1)
     if up then aym=alog10(ym)
  endif else begin 
     ay0=y0
     ay1=y1
     if up then aym=ym
  endelse 
  
  ca=rx*cos(ang)
  sa=ry*sin(ang)
  x=[-ca+ax0+rx,reverse(ca+ax1-rx),ca+ax1-rx,reverse(-ca+ax0+rx),-ca[0]+ax0+rx]
  y=[ay1+sa-ry,reverse(ay1+sa-ry),ay0-sa+ry,reverse(ay0-sa+ry),ay1+sa[0]-ry]
  if keyword_set(rightside) then begin
     x=[ax0,ax0,reverse(ca+ax1-rx),ca+ax1-rx,ax0]        
     y=[ay0,ay1,reverse(ay1+sa-ry),ay0-sa+ry,ay0]
  endif 
  if up then begin
     x=[-ca+ax0+rx,axm,axm,reverse(ca+ax1-rx),ca+ax1-rx,axm,axm,reverse(-ca+ax0+rx),-ca[0]+ax0+rx]
     y=[ay1+sa-ry,ay1,aym[1],reverse(aym[1]+sa-ry),aym[0]-sa+ry,aym[0],ay0,reverse(ay0-sa+ry),ay1+sa[0]-ry]
     if keyword_set(rightside) then begin
     x=[ax0,ax0,axm,axm,reverse(ca+ax1-rx),ca+ax1-rx,axm,axm,ax0]        
     y=[ay0,ay1,ay1,aym[1],reverse(aym[1]+sa-ry),aym[0]-sa+ry,aym[0],ay0,ay0]
     endif 
  endif      


  if keyword_set(xlog) then x=10^x
  if keyword_set(ylog) then y=10^y
 
;plot,[0,12],[10,1e4],/ylog  
;  if not keyword_set(noplot) then polyfill,x,y,color=color
  return
end

pro mw_plot

  h=4.135e-15 ;; eV s
  c=3e10      ;;cm/s
  yearstart=2012; 2008
  xrange=[yearstart,2019]
  yrange=[1e-8,1e21]

;;; rounded edges not right at beginning for already started missions

  p=plot(xrange,yrange,xrange=xrange,yrange=yrange,/nodata,xtitle='Year',/ylog,yminor=0,ymajor=0,margin=[0.13,0.1,0.13,0.1],xminor=0)
  fr=[1e6,1e9,1e12,1e15,1e18,1e21,1e24,1e27] ;,1e30]
  en=h*fr
  yaxis=axis(1,location=[2019,1.],/data,coord_transform=[1.,1./h],textpos=1,tickdir=1,/log,title='Frequency (Hz)',tickvalues=[1e10,1e15,1e20,1e25],minor=0)
  yaxis=axis(1,location=[yearstart,1.],/data,/log,title='Energy (eV)',tickformat='loglabels',tickvalues=[1e-5,1,1e5,1e10],minor=0)

  obs=create_struct('name','','ops',fltarr(2),'ext_ops',0.,'wavelength',dblarr(2),$
                    'eng',dblarr(2),'eng_upgrade',dblarr(2),'time_upgrade',0.,$
                    'freq',dblarr(2),$
                    'survey',0,'point',0,'space',0,'ground',0,$
                    'messenger','','color','','backcolor','','xoffset',0.,'yoffset',0.,$
                    'fcolor','','font_size',0.,$
                    'font_style',0,'transparency',0)
  n=70
  obs=replicate(obs,n)
  obs.font_size=9
  obs.font_style=0
  obs.fcolor='white'
  obs.transparency=30
  obs.yoffset=1.

  ;;; Gamma-ray

  i=50
  obs[i].name='Fermi'
  obs[i].ops=[2008.5,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[8,300e6]
;  obs[i].eng_upgrade=[8,2e9]
;  obs[i].time_upgrade=2015.5
  obs[i].color='red'
  obs[i].font_style=3
  obs[i].font_size=20
  obs[i].space=1
  obs[i].transparency=20
  obs[i].xoffset=0.8;1.5
  obs[i].backcolor='tomato'

  i=1
  obs[i].name='VERITAS'
  obs[i].ops=[2009,2018]
  obs[i].ext_ops=2020
  obs[i].eng=[0.1,50]*1e9
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=0.8
;  obs[i].xoffset=0.7

  i=2
  obs[i].name='HESS'
  obs[i].ops=[2004,2020];2012.67]
  obs[i].ext_ops=2020;2012.67
  obs[i].eng=[0.3,50]*1e9
  obs[i].eng_upgrade=[0.05,50]*1e9 
  obs[i].time_upgrade=2012.67
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=4

;  i=5
;  obs[i].name='HESS II'
;  obs[i].ops=[2012.67,2020]
;  obs[i].ext_ops=2020
;  obs[i].eng=[0.05,50]*1e9
;  obs[i].ground=1
;  obs[i].point=1
;;  obs[i].xoffset=0.1
;  obs[i].yoffset=8

  i=3
  obs[i].name='MAGIC'
  obs[i].ops=[2005,2020]
  obs[i].eng=[0.03,30]*1e9 ;;(TeV->keV)
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=0.3
  
  i=4
  obs[i].name='CTA'
  obs[i].ops=[2019,2020]
  obs[i].eng=[10e6,100e9] ;;keV
  obs[i].ground=1
  obs[i].point=1
  obs[i].yoffset=0.05

  i=9
  obs[i].name='HAWC'
  obs[i].ops=[2013,2020] 
  obs[i].eng=[0.1,100]*1e9 ;; keV
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=3
  obs[i].yoffset=2

  i=6
  obs[i].name='Swift-BAT'
  obs[i].ops=[2004,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[15.,150.] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=1.5

;  i=7
;  obs[i].name='SVOM-GRM/ECLAIRs'
;  obs[i].ops=[2016,2020]
;  obs[i].space=1
;  obs[i].eng=[4,5e3] ;;keV
;  obs[i].survey=1
;  obs[i].yoffset=20

  i=8
  obs[i].name='AGILE'
  obs[i].eng=[250,5e6] ;; keV
  obs[i].ops=[2007,2014]
  obs[i].ext_ops=2010
  obs[i].space=1
  obs[i].point=0 ;; ???
  obs[i].survey=1
  obs[i].yoffset=5

  ;; X-ray
;  i=9
;  obs[i].name='SVOM-MXT'
;  obs[i].ops=[2016,2020]
;  obs[i].space=1
;  obs[i].eng=[0.3,6] ;; keV
;  obs[i].point=1
;  obs[i].xoffset=1

  i=10
  obs[i].name='Swift-XRT'
  obs[i].ops=[2004,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[0.3,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5

  i=11
  obs[i].name='Chandra'
  obs[i].ops=[1999,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[0.2,10.] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=0.8

  i=12
  obs[i].name='MAXI'
  obs[i].ops=[2009,2020]
  obs[i].eng=[0.3,30] ;; keV
  obs[i].survey=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=1.2

  i=13
  obs[i].name='ASTRO-H'
  obs[i].ops=[2015+11./12,2020]
  obs[i].eng=[0.3,600] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=5

  i=16
  obs[i].name='NuStar'
  obs[i].ops=[2012.5,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[3,80] ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1.5

  i=15
  obs[i].name='eRosita'
  obs[i].ops=[2015,2020]
  obs[i].eng=[0.5,10] ;; keV
  obs[i].point=1
  obs[i].survey=1
  obs[i].space=1

  i=14
  obs[i].name='XMM-Newton'
  obs[i].ops=[1999,2015]
  obs[i].ext_ops=2019
  obs[i].eng=[0.1,10]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1.7
  obs[i].yoffset=0.8

  i=17
  obs[i].name='Integral'
  obs[i].ops=[2002,2016]
  obs[i].ext_ops=2018
  obs[i].eng=[15.,10e3]  ;; keV
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=3
  obs[i].yoffset=10

  i=43
  obs[i].name='NICER'
  obs[i].ops=[2016.8,2020]
  obs[i].ext_ops=2020
  obs[i].eng=[0.2,12]
  obs[i].point=1
  obs[i].space=1
  
  i=44
  obs[i].name='Suzaku'
  obs[i].ops=[2005,2014]
  obs[i].ext_ops=2020
  obs[i].eng=[0.3,600]
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=1.5

  ;; Optical
  i=20
  obs[i].name='Pan-STARRS'
  obs[i].color=!black
  obs[i].ops=[2010,2020]
  obs[i].wavelength=[0.5,1.0]*1e-4 ;; cm
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=5
  obs[i].xoffset=1.3

  i=19
  obs[i].name='Swift-UVOT'
  obs[i].ops=[2004,2015]
  obs[i].ext_ops=2019
  obs[i].wavelength=[150.,650.]*1e-7 ;; nm->cm
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=10
  obs[i].fcolor='black'

  i=18
  obs[i].name='Hubble'
  obs[i].ops=[1990,2015]
  obs[i].ext_ops=2019
  obs[i].wavelength=[300,1000]*1e-7
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2

  i=21
  obs[i].name='PTF'
  obs[i].ops=[2009,2020]
  obs[i].ext_ops=2020
  obs[i].wavelength=[490,630]*1e-7 ;; nm->cm
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=2
  obs[i].xoffset=0.7

  i=41
  obs[i].name='Robopol'
  obs[i].ops=[2013,2020]
  obs[i].ext_ops=2020
  obs[i].wavelength=[500,800]*1e-7
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=2
  obs[i].yoffset=2

  i=42
  obs[i].name='DES'
  obs[i].ops=[2012.75,2020]
  obs[i].ext_ops=2020
  obs[i].wavelength=[400,1200]*1e-7
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=3
  obs[i].yoffset=2

  i=46
  obs[i].name='Gaia'
  obs[i].ops=[2013.9,2020]
  obs[i].ext_ops=2020
  obs[i].wavelength=[320,1000]*1e-7
  obs[i].survey=1
  obs[i].space=1
  obs[i].xoffset=0.5
  obs[i].yoffset=2

  ;; IR
  i=22
  obs[i].name='Spitzer'
  obs[i].ops=[2003,2015]
  obs[i].ext_ops=2019
  obs[i].wavelength=[3,180]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].xoffset=1
  obs[i].yoffset=2

  i=23
  obs[i].name='Herschel'
  obs[i].ops=[2009.5,2013]
  obs[i].ext_ops=[2012.5]
  obs[i].wavelength=[55,672]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2

  i=24
  obs[i].name='WISE'
  obs[i].ops=[2010,2013]
  obs[i].wavelength=[3,22]*1e-4
  obs[i].ext_ops=2013
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=1.8

  i=34
  obs[i].name='JWST'
  obs[i].ops=[2018,2030]
  obs[i].wavelength=[0.6,28]*1e-4
  obs[i].point=1
  obs[i].space=1
  obs[i].yoffset=2
  obs[i].xoffset=0.2

  ;; RADIO

  i=25
  obs[i].name='LOFAR'
  obs[i].ops=[2011,2020]
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2

  i=26
  obs[i].name='VLBA'
  obs[i].ops=[1993,2020]
  obs[i].freq=[0.312,90]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=0.5

  i=27
  obs[i].name='JVLA'
  obs[i].ops=[2010,2020]
  obs[i].freq=[1.,50.]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.6
  obs[i].yoffset=2

  i=28
  obs[i].name='SKA-1'
  obs[i].ops=[2018,2020]
  obs[i].freq=[0.1,25]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].yoffset=0.4
  obs[i].xoffset=0.3

  i=29
  obs[i].name='Apertif'
  obs[i].ops=[2016,2020]
  obs[i].freq=[1.,1.7]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].xoffset=1.5

  i=48
  obs[i].name='MeerKAT'
  obs[i].ops=[2016,2020]
  obs[i].freq=[0.58,14.5]*1e9 ;; Hz
  obs[i].ground=1
  obs[i].survey=1
  obs[i].yoffset=4
  obs[i].xoffset=0.6

  i=49
  obs[i].name='ASKAP'
  obs[i].ops=[2015,2020]
  obs[i].freq=[0.7,1.8]*1e9 ;; Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2

  i=7
  obs[i].name='ALMA'
  obs[i].ops=[2013,2020]
  obs[i].freq=[84,720]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=2

  i=33
  obs[i].name='Planck'
  obs[i].ops=[2009.5,2013]
  obs[i].ext_ops=2013
  obs[i].freq=[27,1000]*1d9 ;; Hz
  obs[i].survey=1
  obs[i].space=1
  obs[i].yoffset=2

  i=32
  obs[i].name='Arecibo'
  obs[i].ops=[1997,2020]
  obs[i].ext_ops=2020
  obs[i].freq=[0.312,10.2]*1e9 ;; Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].xoffset=0.75

  i=45
  obs[i].name='MWA'
  obs[i].ops=[2012,2020]
  obs[i].ext_ops=2020
  obs[i].freq=[10,240]*1e6 ;; MHz->Hz
  obs[i].survey=1
  obs[i].ground=1
  obs[i].yoffset=2
  obs[i].xoffset=1

  i=47
  obs[i].name='ATCA'
  obs[i].ops=[2010,2020]
  obs[i].ext_ops=2020
  obs[i].freq=[1.1,150]*1e9 ;; GHz->Hz
  obs[i].point=1
  obs[i].ground=1
  obs[i].yoffset=1.5

  ;;; MULTI-MESSENGER
  i=35
  obs[i].name='IceCube'
  obs[i].ops=[2010,2020]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e14,1e15]
  obs[i].color='dim grey'
  obs[i].yoffset=2

  i=36
  obs[i].name='Auger'
  obs[i].ops=[2004,2020]
  obs[i].messenger='UHECRs'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e11,1e12]
  obs[i].color='black'
  obs[i].yoffset=2

  i=37
  obs[i].name='CALET'
  obs[i].ops=[2014.5,2020]
  obs[i].space=1
  obs[i].survey=1
  obs[i].messenger='electron'
  obs[i].eng=[1e15,1e16]
  obs[i].color='grey'
  obs[i].yoffset=2

  i=38
  obs[i].name='Advanced LIGO/Virgo'
  obs[i].ops=[2015,2020]
  obs[i].messenger='GW'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].color=!p.color
  obs[i].eng=[1e16,1e17]
  obs[i].color='dark grey'
  obs[i].yoffset=2

  i=39
  obs[i].name='AMS'
  obs[i].ops=[2011,2020]
  obs[i].messenger='Cosmic Rays'
  obs[i].space=1
  obs[i].survey=1
  obs[i].eng=[1e13,1e14]
  obs[i].color='slate grey'
  obs[i].yoffset=2

  i=40
  obs[i].name='ANTARES'
  obs[i].ops=[2008,2020]
  obs[i].messenger='Neutrinos'
  obs[i].ground=1
  obs[i].survey=1
  obs[i].eng=[1e12,1e13]
  obs[i].color='dark slate grey'
  obs[i].yoffset=2

  w=where(obs.ops[0] lt yearstart)
  obs[w].ops[0]=yearstart
  right=intarr(n_elements(obs))
  right[w]=1
;  w=where(obs.ops[1] gt 2019)
;  obs[w].ops[1]=2019
  obs.eng=obs.eng*1e3  ;;; convert from keV to eV
  obs.eng_upgrade=obs.eng_upgrade*1e3  ;;; convert from keV to eV
    
  w=where(obs.freq[0] ne 0)
  f=where(obs.wavelength[0] ne 0)
  en=where(obs.eng[0] ne 0)

  obs[w].wavelength=reverse(c/obs[w].freq)
  obs[w].eng=h*obs[w].freq

  obs[f].freq=reverse(c/obs[f].wavelength)
  obs[f].eng=h*obs[f].freq

  obs[en].freq=obs[en].eng/h
  obs[en].wavelength=reverse(h*c/obs[en].eng)

  w=where(obs.point eq 1 and obs.messenger eq '')
  obs[w].color='blue'
  obs[w].backcolor='light blue'
  w=where(obs.survey eq 1 and obs.messenger eq '')
  obs[w].color='lime green'
  obs[w].backcolor='pale green'


  p.refresh,/disable
  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        curved_polygon,obs[i].ops[0],obs[i].ops[1],obs[i].eng[0],obs[i].eng[1],xx,yy,right=right[i],/ylog
;;; insert ext_eng into xx,yy somehow - merge 2 curved polys?
        poly=polygon(xx,yy,/data,transparency=obs[i].transparency,fill_color=obs[i].color,color='black')
        if obs[i].ext_ops ne obs[i].ops[1] and obs[i].ext_ops ne 0. then begin
           curved_polygon,obs[i].ops[1],obs[i].ext_ops,obs[i].eng[0],obs[i].eng[1],xx,yy,/right,/ylog,ym=obs[i].eng_upgrade,xm=obs[i].time_upgrade
           poly=polygon(xx,yy,/data,fill_color=strtrim(obs[i].backcolor,2),transparency=70,thick=2,color=obs[i].color,linestyle='--')
        endif 
     endif 
  endfor 

  for i=0,n-1 do begin
     if obs[i].name ne '' then begin 
        if obs[i].messenger ne '' then mess=' ('+strtrim(obs[i].messenger,2)+')' else mess=''
        t=text(obs[i].ops[0]+obs[i].xoffset+0.1,10^((alog10(obs[i].eng[1])+alog10(obs[i].eng[0]))/2.)/5.*obs[i].yoffset,obs[i].name+mess,/data,font_color=obs[i].fcolor,font_style=obs[i].font_style,font_size=obs[i].font_size)
     endif 
  endfor      
;  leg=legend(label=['Survey','Pointing'],target=p,/data,position=[2010.1,1e20],text_color='black')
  l1=text(yearstart+.1,1.5e19,'Survey',font_color='lime green',/data,font_style=1)
  l2=text(yearstart+.1,1.5e18,'Pointing',font_color='blue',/data,font_style=1)
  p.save,'~/Fermi/Senior_Review/SR2014/mw_plot.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close
  
  stop
  return
end 

pro tda_plot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJECT TYPE VS VARIABILITY TIMESCALE - color scale with energy or
;;                                        dynamic range or flux level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  xrange=[1e-6,1e6]
;  xrange=[-6,8]
;  yrange=[0,10]
  yrange=[1e-4,1e8]

  p=plot(xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle='Shortest Timescale of Variability (s)',/xlog,/ylog,ytickformat='loglabels',xtickformat='loglabels',ymajor=0,yminor=0,margin=[0.1,0.1,0.1,0.2])
  a=axis(1,location=[6.,1.],title='Variability/Periodicity Duration (days)',/log,textpos=1,tickdir=1,coord_transform=[0.,1./86400.],tickformat='loglabels')
  a=axis(1,location=[-6.,1.],title='Variability/Periodicity Duration (s)',/log,tickformat='loglabels')


  p.refresh,/disable
  day=86400.
  hour=3600.
  year=365*day

  obj=create_struct('type','','timescale',fltarr(2),'duration',fltarr(2),'distance',0.,$
                    'periodic',0,'recurrent',0,'transient',0,$
                    'xoffset',1.,'yoffset',1.,'font_color','','ang',0.)
  obj=replicate(obj,20)
  obj.font_color='black'

  i=0
  obj[i].type='Unidentified Galactic Transients' ;; ATel #5625, 3163
  obj[i].timescale=[20.,5*60.]
  obj[i].duration=[20.,5*60.]
  obj[i].transient=1
  obj[i].distance=3
  obj[i].xoffset=0.015
  obj[i].yoffset=0.004

;  obj[i].type='Lensed Blazars'
;  obj[i].timescale=[hour,day]
;  obj[i].duration=[day,day*10.]
;  obj[i].distance=9
;  obj[i].recurrent=1
;  obj[i].xoffset=0.1
;  obj[i].yoffset=0.5

  i=1
  obj[i].type='Blazars'
  obj[i].timescale=[hour,day]
  obj[i].duration=[day,year*5];[day,day*10.]
  obj[i].distance=8
  obj[i].recurrent=1
  obj[i].xoffset=0.5
  obj[i].yoffset=1.6
  obj[i].font_color='white'

  i=2
  obj[i].type='Crab Flares'
  obj[i].timescale=[hour,day*3.]
  obj[i].duration=[day,day*21.]
  obj[i].distance=3
  obj[i].recurrent=1
  obj[i].xoffset=0.15
  obj[i].yoffset=0.2
  obj[i].font_color='white'

  i=5
  obj[i].type='Solar Flares'
  obj[i].timescale=[1e-2,1]
  obj[i].duration=[60.,7e4]
  obj[i].distance=1
  obj[i].recurrent=1
  obj[i].xoffset=0.5
  obj[i].yoffset=0.5
  obj[i].font_color='white'

  i=4
  obj[i].type='Magnetars'
  obj[i].timescale=[1e-3,1]
  obj[i].duration=[day,day*10.]
  obj[i].distance=4
  obj[i].recurrent=1
  obj[i].font_color='white'
  obj[i].xoffset=0.5
  obj[i].ang=-3

  i=3
  obj[i].type='GRBs'
  obj[i].timescale=[1e-3,1]
  obj[i].duration=[10,day]
  obj[i].distance=10
  obj[i].transient=1
  obj[i].xoffset=0.2
  obj[i].yoffset=0.03

  i=6
  obj[i].type='TGFs'
  obj[i].timescale=[10e-6,1e-3]
  obj[i].duration=[1e-3,1]
  obj[i].distance=0
  obj[i].transient=1
  obj[i].yoffset=1.;12.
;  obj[i].xoffset=1.

  i=7
  obj[i].type='Binaries'
  obj[i].timescale=[hour,day*3.]
  obj[i].duration=[day,year]
  obj[i].distance=5 ;; galactic
  obj[i].periodic=1
;  obj[i].xoffset=0.2
  obj[i].yoffset=1.5
;  obj[i].font_color='white'

  i=8
  obj[i].type='Pulsars'
  obj[i].timescale=[10e-6,2e-3]
  obj[i].duration=[1e-3,0.5]
  obj[i].distance=5
  obj[i].periodic=1
  obj[i].yoffset=1.5
  obj[i].xoffset=3.
  
  i=9
  obj[i].type='Accreting Pulsars'
  obj[i].timescale=[0.1,20]
  obj[i].duration=[1.2,600]
  obj[i].distance=5
  obj[i].periodic=1
  obj[i].xoffset=0.2
  obj[i].yoffset=0.09

  i=10
  obj[i].type='Novae'
  obj[i].timescale=[day,day*7.]
  obj[i].duration=[day*7,day*28.]
  obj[i].distance=3
  obj[i].transient=1
  obj[i].xoffset=0.11
  obj[i].yoffset=1.3

  w=where(obj.type ne '',n)
  obj=obj[w]

  rx=0.3
  ry=0.6
  for i=0,n-1 do begin 
;     ry=0.7
;     rx=0.7
;     if alog10(obj[i].duration[1])-alog10(obj[i].duration[0]) le 2 then begin 
;        rx=0.3
;        ry=0.3
;     endif 
;     curved_polygon,obj[i].timescale[0],obj[i].timescale[1],obj[i].duration[0],obj[i].duration[1],xx,yy,/xlog,/ylog,rx=rx,ry=ry
;     xx=[obj[i].timescale[0],obj[i].timescale[1],obj[i].timescale[1],obj[i].timescale[0],obj[i].timescale[0]]
;     yy=[obj[i].duration[0],obj[i].duration[0],obj[i].duration[1],obj[i].duration[1],obj[i].duration[0]]
     x0=alog10(obj[i].timescale[0])
     y0=alog10(obj[i].duration[0])
     x1=alog10(obj[i].timescale[1])
     y1=alog10(obj[i].duration[1])
     theta=atan((y1-y0)/(x1-x0))

     phi=!pi/2.-theta
     cosang=cos(phi)
     sinang=sin(phi)
;     xx=10^[x0-rx*cosang,x0+rx*cosang,x1+rx*cosang,x1-rx*cosang,x0-rx*cosang]
;     yy=10^[y0+ry*sinang,y0-ry*sinang,y1-ry*sinang,y1+ry*sinang,y0+ry*sinang]
     xx=10^[x0,x0,x1,x1,x0]
     yy=10^[y0+ry,y0-ry,y1-ry,y1+ry,y0+ry]

     phi=indgen(9)*10*!dtor+theta
;     rphi=reverse(phi)
     x0=x0+rx*cos(theta)
     y0=y0+ry*sin(theta)
     x1=x1-rx*cos(theta)
     y1=y1-ry*sin(theta)
     quad=!pi/2.
     
;     xx=10^[x0+rx*cos(quad+phi),x0+rx*cos(quad*2+phi),x1+rx*cos(quad*3+phi),x1+rx*cos(phi),x0+rx*cos(quad+phi[0])]
;     yy=10^[y0+ry*sin(quad+phi),y0+ry*sin(quad*2+phi),y1+ry*sin(quad*3+phi),y1+ry*sin(phi),y0+ry*sin(quad+phi[0])]

;stop
     if obj[i].recurrent eq 1 then color='indigo'
     if obj[i].periodic eq 1 then color='cornflower'
     if obj[i].transient eq 1 then color='lime green'
     poly=polygon(xx,yy,/data,fill_color=color,transparency=50,thick=2,color='grey')
;     p=plot(xxx,yyy,/overplot)
;     t=text(10^x0,10^y0,ntostr(theta*!radeg,3),/data)
;     p=plot(10^[x0,x1],10^[y0,y1],/overplot)
  endfor 
  for i=0,n-1 do begin
     x0=alog10(obj[i].timescale[0])
     y0=alog10(obj[i].duration[0])
     x1=alog10(obj[i].timescale[1])
     y1=alog10(obj[i].duration[1])
     theta=atan((y1-y0)/(x1-x0))
     t=text(obj[i].timescale[0]*10.*obj[i].xoffset,10^mean(alog10(obj[i].duration))*obj[i].yoffset*0.5,obj[i].type,/data,font_size=10,font_color=obj[i].font_color,orientation=theta*!radeg-10-obj[i].ang)
;     t=text(obj[i].timescale[0]*10.*obj[i].xoffset,10^mean(alog10(obj[i].duration))*obj[i].yoffset,obj[i].type,/data,font_size=10,font_color=font_color)
  endfor 
  t=text(2e-6,1e7,'Periodic',color='cornflower',/data,font_style=1)
  t=text(2e-6,3e6,'Recurrent',color='indigo',/data,font_style=1)
  t=text(2e-6,1e6,'Transient',color='lime green',/data,font_style=1)
;  p=plot([1e-10,1e10],[1e-10,1e10],/overplot,linestyle=':')

  p.save,'~/Fermi/Senior_Review/SR2014/tda_plot.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close


end 

pro read_bib,p
  
  cd,'~/Fermi/Senior_Review/SR2014'
  
  readcol,'fermi_bibcodes.csv',title,author,date,bibcode,citation,category,subject,format='(a,a,a,a,a,a,a)',delim=',',skip=1
  ;;; SEARCH AND REPLACE COMMAS WITH NOTHING

;  readcol,'fermi_bibcodes.txt',title,date,citation,category,subject,format='(a,a,a,a,a)',delim='	',skip=1;,stringskip='"'

;  file='fermi_bibcodes.csv'
;  readcol,file,lines,format='(a)',delim='%',stringskip='ERRATUM'
;  n=numlines(file)
  n=n_elements(title)
  p=create_struct('bibcode','','month',0,'year',0.,'date',0.,'citation',0,'analysis',0,'refer',0,$
                  'predict',0,'instr',0,'nocat',0,$
                  'agn',0,'catalogs',0,'crs',0,'dm',0,'ebl',0,'grb',0,'binary',0,'diffuse',0,$
                  'galaxies',0,'iamr',0,'other',0,'pulsars',0,'snr',0,'ss',0,'unid',0,$
                  'author','','title','')
  p=replicate(p,n)
  
;  p.bibcode=bibcode
  w=where(citation eq '-')
  citation[w]=0
  p.citation=citation
;  p.author=author
;  p.title=title

  for i=0,n-1 do begin 
     p[i].month=month(strmid(date[i],0,3))
     year=strmid(date[i],4,4)
     if strmid(year,0,2) ne '20' then stop
     p[i].year=year
     
     if strpos(category[i],'analysis') ne -1 then p[i].analysis=1
     if strpos(category[i],'Refer') ne -1 then p[i].refer=1
     if strpos(category[i],'Predict') ne -1 then p[i].predict=1
     if strpos(category[i],'Instr') ne -1 then p[i].instr=1
     if strpos(category[i],'Cannot') ne -1 then p[i].nocat=1
     if strpos(subject[i],'AGN') ne -1 then p[i].agn=1
     if strpos(subject[i],'Catalogs') ne -1 then p[i].catalogs=1
     if strpos(subject[i],'Cosmic') ne -1 then p[i].crs=1
     if strpos(subject[i],'Dark') ne -1 then p[i].dm=1
     if strpos(subject[i],'EBL') ne -1 then p[i].ebl=1
     if strpos(subject[i],'burst') ne -1 then p[i].grb=1
     if strpos(subject[i],'binary') ne -1 then p[i].binary=1
     if strpos(subject[i],'diffuse') ne -1 then p[i].diffuse=1
     if strpos(subject[i],'Galaxies') ne -1 then p[i].galaxies=1
     if strpos(subject[i],'methods') ne -1 then p[i].iamr=1
     if strpos(subject[i],'Other') ne -1 then p[i].other=1
     if strpos(subject[i],'Pulsars') ne -1 then p[i].pulsars=1
     if strpos(subject[i],'SNR') ne -1 then p[i].snr=1
     if strpos(subject[i],'Solar') ne -1 then p[i].ss=1
     if strpos(subject[i],'Unid') ne -1 then p[i].unid=1
  endfor 

  p.date=(p.month-1.)/12.+p.year ;;; does this -1 make sense?
  s=sort(p.date)
  p=p[s]

  mwrfits,p,'~/Fermi/Senior_Review/SR2014/Fermi_bibliography.fits',/create
stop  
  return
end 

pro cumulative_bib
  ;; which plots use?
  ;;; cumulative papers
  ;;; theses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  plot of papers as function of time (single panel of previous plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  cd,'~/Fermi/Senior_Review/SR2014'
  bib=mrdfits('Fermi_bibliography.fits',1)
  nbib=n_elements(bib)
  w=where(bib.date gt 2008.5,nbib)
  bib=bib[w]
  ind=intarr(nbib)
  ind[*]=1
  cind=cumulative(ind)

  begplot,name='bib_papers_category.eps',/land,/encap,/color,font='helvetica'
  !y.margin=[5,6]
  !x.margin=[14,1]
  th=10
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 
  plot,[2008,2014],[0,1500],/nodata,/xsty,/ysty,xrange=[2008.5,2014],yrange=[0,1600],xtitle='Year',charsize=2,xminor=0,ytitle='Cumulative !4Fermi !XPapers',ytick_get=ytick,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014]
  plotsym,0,1,/fill
  axis,xaxis=0,xminor=12,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014],xtickformat='(A1)'
  n=n_elements(bib.date)
  oplot,bib.date+1./24,cind,thick=th

  oplot,[2009.,2009.]+7./12.,[0,2000],line=1
  xyouts,2009.5,250,'Public Data Release',orient=90
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  oplot,[p7,p7],[0,2000],line=1
  xyouts,2011.5,800,'Pass 7 Release',orient=90

  endplot

  spawn,'convert -rotate 270 bib_papers_total.eps bib_papers_total.pdf'


stop
  begplot,name='bib_papers_category.eps',/land,/encap,/color,font='helvetica'
  !y.margin=[5,6]
  !x.margin=[14,1]
  th=10
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 
  plot,[2008,2014],[0,1500],/nodata,/xsty,/ysty,xrange=[2008.5,2014],yrange=[0,1600],xtitle='Year',charsize=2,xminor=0,ytitle='Cumulative !4Fermi !XPapers',ytick_get=ytick,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014]
  plotsym,0,1,/fill
  axis,xaxis=0,xminor=12,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014],xtickformat='(A1)'
  n=n_elements(bib.date)
;  oplot,[bib.date+1./24,2014],[cind,cind[n-1]],thick=th
  oplot,bib.date+1./24,cind,thick=th
  xyouts,2013,1400,'TOTAL',charsize=1

  color=[!red,!blue,!green,!orange]
  k=0
  tn=tag_names(bib)
  y=[700,350,250,50]
  yord=[1,0,2,3]
  for i=5,8 do begin
     j=where(bib.(i))
     cum=cumulative(ind[j])
     oplot,bib[j].date+1./24.,cum,color=color[k],psym=10
     xyouts,2013,y[k],tn[yord[k]+5],color=color[yord[k]],charsize=1
     k=k+1
  endfor      

  oplot,[2009.,2009.]+7./12.,[0,2000],line=1
  xyouts,2009.5,250,'Public Data Release',orient=90
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  oplot,[p7,p7],[0,2000],line=1
  xyouts,2011.5,800,'Pass 7 Release',orient=90

  endplot

  spawn,'convert -rotate 270 bib_papers_category.eps bib_papers_category.pdf'

  begplot,name='bib_papers_science.eps',/land,/encap,/color,font='helvetica'
  !y.margin=[5,6]
  !x.margin=[14,1]
  th=10
  DEVICE, /PALATINO, /ITALIC, /BOLD, FONT_INDEX=4 
  plot,[2008,2014],[1,1500],/nodata,/xsty,/ysty,xrange=[2008.5,2015],yrange=[1,2000],xtitle='Year',charsize=2,xminor=0,ytitle='Cumulative !4Fermi !XPapers',ytick_get=ytick,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014],/ylog
  plotsym,0,1,/fill
  axis,xaxis=0,xminor=12,xticks=5,xtickv=[2009,2010,2011,2012,2013,2014],xtickformat='(A1)'
  n=n_elements(bib.date)
;  oplot,[bib.date+1./24,2014],[cind,cind[n-1]],thick=th
  oplot,bib.date+1./24,cind,thick=th,psym=10
  xyouts,2014,max(cind)*0.95,'TOTAL',charsize=1

  color=[!red,!blue,!green,!darkgreen,!orange,!purple,!magenta,!cyan,!sienna,!salmon,!seagreen,!dodgerblue,!deeppink,!navyblue,!grey50]
  k=0
  tn=tag_names(bib)
  y=[700,500,350,250,180,130,100,70,50,35,25,25,18,13,10]
  yord=[0,3,7,5,11,2,12,8,4,6,10,14,9,1,13]
  for i=10,24 do begin
     if i ne 20 then begin 
        j=where(bib.(i))
        cum=cumulative(ind[j])
        oplot,bib[j].date+1./24.,cum,color=color[k],psym=10
;        xyouts,2014,max(cum)*adj[k],tn[i],color=color[k],charsize=1
        xyouts,2014,y[k],tn[yord[k]+10],color=color[yord[k]],charsize=1
     endif 
;        print,y[k],tn[i]
        k=k+1
  endfor      

  oplot,[2009.,2009.]+7./12.,[1,2000],line=1
  xyouts,2009.5,2250,'Public Data Release',orient=30
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  oplot,[p7,p7],[1,2000],line=1
  xyouts,2011.5,2250,'Pass 7 Release',orient=30
  endplot

  spawn,'convert -rotate 270 bib_papers_science.eps bib_papers_science.pdf'

stop  
end 

pro bib
  ;; which plots use?
  ;;; cumulative papers
  ;;; theses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  plot of papers as function of time (single panel of previous plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  cd,'~/Fermi/Senior_Review/SR2014'
  bib=mrdfits('Fermi_bibliography.fits',1)
  nbib=n_elements(bib)
  w=where(bib.date gt 2008.5,nbib)
  bib=bib[w]
  ind=intarr(nbib)
  ind[*]=1
  bin=1.;3./12
;  cind=cumulative(ind)

;  goto,skip
  plothist,bib.date,x,y,bin=bin,/noplot
  p=barplot(x,y,xrange=[2008,2014],yrange=[0,400],ytitle='             Papers',xminor=0,xtitle='Year',transparency=0.2,aspect_ratio=0.008,yminor=4)
  t=text(0.055,0.42,'Fermi',font_style='it',orientation=90)
  plotsym,0,1,/fill

  p=plot([2009.,2009.]+7./12.,[0,2000],linestyle='--',/overplot)
  t=text(2009.5,30,'Public Data',orientation=90,/data,font_color='white')
  t=text(2009.8,40,'Release',orientation=90,/data,font_color='white')
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  p=plot([p7,p7],[0,2000],linestyle='--',/overplot)
  t=text(2011.5,100,'Pass 7 Release',orientation=90,/data,font_color='white')
  for i=0,5 do colprint,2008+i,2009+i,n_elements(where(bib.year ge 2008+i and bib.year lt 2008+1+i))

  p.save,'~/Fermi/Senior_Review/SR2014/bib_papers_total.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close

stop
  p=plot([2008.5,2014],[0,150],/nodata,xrange=[2008.5,2014],yrange=[0,600],ytitle='             Papers',xminor=12,xtitle='Year',transparency=0.2)
  t=text(0.05,0.42,'Fermi',font_style='it',orientation=90)
  plotsym,0,1,/fill

;  xyouts,2013,1400,'TOTAL',charsize=1

  plothist,bib.date,x,y,bin=bin,/noplot
  color=['red','blue','green','orange']
  k=0
  tn=tag_names(bib)
  y0=intarr(n_elements(x))
  yord=[1,0,2,3]
  yout=[140,133,126,119]
  for i=5,8 do begin
     j=where(bib.(i))
     y=intarr(n_elements(x))
     for l=0,n_elements(x)-1 do begin
        w=where(bib[j].date+1./24. ge x[l] and bib[j].date+1./24 lt x[l]+bin,nw)
        y[l]=nw
     endfor 
     if i eq 5 then tmp=execute('b'+ntostr(i)+'=barplot(x,y,/overplot,fill_color=color[i-5])') else $
        tmp=execute('b'+ntostr(i)+'=barplot(x,y+y0,bottom_values=y0,/overplot,fill_color=color[i-5])')
     t=text(2013,yout[i-5],tn[yord[k]+5],font_color=color[yord[k]],/data)
     y0=y+y0
     k=k+1
  endfor      

  p=plot([2009.,2009.]+7./12.,[0,2000],linestyle='--',/overplot)
  t=text(2009.5,80,'Public Data Release',orientation=90,/data)
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  p=plot([p7,p7],[0,2000],linestyle='--',/overplot)
  t=text(2011.5,100,'Pass 7 Release',orientation=90,/data)

  p.save,'~/Fermi/Senior_Review/SR2014/bib_papers_category.png'
  p.refresh
;  k=get_kbrd(10)
;  if k eq 's' then stop
  p.close

;  skip:
  p=plot([2008.5,2014],[0,200],/nodata,xrange=[2008.5,2015],yrange=[0,600],ytitle='     Papers by Subject',xminor=12,xtitle='Year',transparency=20)
  t=text(0.055,0.31,'Fermi',font_style='it',orientation=90)
  plotsym,0,1,/fill
  plothist,bib.date,x,y,bin=bin,/noplot
  color=['maroon','salmon','red','magenta','indigo','purple','blue','cyan','aquamarine','green','lime','light coral','orange','gold','tan','gray']
  k=0
  tn=tag_names(bib)
  tn=tn[10:24]
  y0=intarr(n_elements(x))
;  yord=[0,3,7,5,11,2,12,8,4,6,10,14,9,1,13]
  tot=intarr(15)
  for i=10,24 do tot[i-10]=total(bib.(i))
  yord=reverse(sort(tot))
;  yord=14-indgen(15)
;  yout=[700,500,350,250,180,130,100,70,50,35,25,25,18,13,10]
  yout=indgen(15)*10+10
  font_color=replicate('white',15)
  font_color[[7,8,10,12,13,14]]='black'
  for i=10,24 do begin
;     if i ne 20 then begin 
        j=where(bib.(yord[i-10]+10) eq 1,nj)
        print,tn[yord[i-10]],nj
        y=intarr(n_elements(x))
        for l=0,n_elements(x)-1 do begin
           w=where(bib[j].date+1./24. ge x[l] and bib[j].date+1./24 lt x[l]+bin,nw)
           y[l]=nw
        endfor 
        if i eq 10 then tmp=execute('b'+ntostr(i)+'=barplot(x,y,/overplot,fill_color=color[i-10])') else $
           tmp=execute('b'+ntostr(i)+'=barplot(x,y+y0,bottom_values=y0,/overplot,fill_color=color[i-10])')
        t=text(2014.1,yout[i-10],tn[yord[i-10]],/fill_background,fill_color=color[i-10],/data,font_size=8,font_color=font_color[i-10],font_style=1)
        y0=y+y0
;     endif 
;print,tn[yord[k]+10]
  endfor      

  p=plot([2009.,2009.]+7./12.,[0,2000],linestyle='--',/overplot)
  t=text(2009.5,100,'Public Data Release',orientation=90,/data,font_size=10)
  ;; pass 7 aug 5, 2011
  p7=2011.+ymd2dn(2011,8,5)/365.
  p=plot([p7,p7],[0,2000],linestyle='--',/overplot)
  t=text(2011.9,120,'Pass 7 Release',orientation=90,/data,font_size=10)

  p.save,'~/Fermi/Senior_Review/SR2014/bib_papers_science.png'
  p.refresh
  k=get_kbrd(10)
  if k eq 's' then stop
  p.close

return
end 

pro bib2

  cd,'~/Fermi/Senior_Review/SR2014'
  bib0=mrdfits('Fermi_bibliography.fits',1)
  color=['maroon','salmon','red','magenta','indigo','purple','blue','cyan','aquamarine','green','lime','light coral','orange','gold','tan','gray']

  for k=3,3 do begin
     w=where(bib0.date ge 2008.+k and bib0.date lt 2009.+k,nbib)
;     print, 2008.5+k, 2009.5+k
     bib=bib0[w]
     tn=tag_names(bib)
     num=fltarr(15)
     xoffset=intarr(15) & yoffset=xoffset
     if k eq 0 then begin ;; 2008
        xoffset=[0,-15,0,0,0,0,0,0,-20,0,0,0,5,0]
        yoffset=[0,-15,0,0,0,0,0,0,0,0,0,0,-5,0]
     endif
     if k eq 1 then begin ;; 2009
        xoffset=[0,0,0,0,0,0,0,10,-10,0,0,0,0,0]
        yoffset=[0,0,0,-10,0,0,0,0,0,0,0,0,10,0]
     endif
     if k eq 2 then begin ;; 2010
        xoffset=[0,0,0,0,0,0,0,-10,30,0,0,0,0,0]
        yoffset=[0,0,0,-10,0,0,0,0,0,0,0,0,0,0]
     endif
     if k eq 3 then begin ;; 2011
        xoffset=[0,0,0,0,0,0,0,10,-40,0,0,0,0,0]
        yoffset=[0,0,0,-10,0,0,0,0,0,0,0,0,10,0]
     endif 
     if k eq 4 then begin ;; 2012
        xoffset=[0,0,0,0,0,0,0,10,-80,0,0,0,0,0]
        yoffset=[0,0,0,-10,0,0,0,0,0,0,0,0,0,0]
     endif 
     if k eq 5 then begin ;; 2013
        xoffset=[0,0,0,0,0,0,0,10,-40,0,0,0,0,0]
        yoffset=[0,0,0,-10,0,0,0,0,0,0,0,0,0,0]
     endif 

     
     for i=10,24 do begin
        j=where(bib.(i) eq 1,nj)
        num[i-10]=nj
           
;        print,tn[i],nj
     endfor 
     if k gt 0 then current=1 else current=0
;     color=[!salmon,!red,!sienna,!darkred,!orange,!yellow,!green,!forestgreen,!cyan,!skyblue,!blue,!navyblue,!violet,!purple,!pink]
                                ;tmp=execute('c'+ntostr(k)+"=pie_chart(num,/lotus,/outline,color=color,pienames=tn[10:24],title='Year
                                ;'+ntostr(2008.5+k)+'
                                ;'+ntostr(nbib),layout=[2,3,k+1],current=current)")

     num[20-10]=num[19-10]+num[20-10]
     num[19-10]=0
     w=where(num ne 0)
     num=num[w]
     tn=tn[w+10]
     p=plot([0,600],[0,600],/nodata,xrange=[0,600],yrange=[0,600],axis_style=0,aspect_ratio=1);,layout=[2,3,k+1],current=current)
     c=pie_chart(num,/outline,color=color,pienames=tn,xrange=[0,600],yrange=[0,600],/overplot,xoffset=xoffset,yoffset=yoffset);,current=current)
     t=text(300,500,ntostr(k+2008),/data,font_size=14,font_style='bf')
;     key=get_kbrd(10)
;     if key eq 's' then stop
     p.save,'~/Fermi/Senior_Review/SR2014/bib_pie_chart'+ntostr(k+1)+'.png'
     p.refresh
     p.close
  endfor 
  
  
  stop
  return
end 

pro dm
  
  readcol,'~/Fermi/Senior_Review/SR2014/dm/GC_Halo_Optimistic_3Sigma.txt',ehalo,halo,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/GalaxyClustersFermi.txt',ecluster,cluster,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/IGRB_BulSub.txt',ediffuse,diffuse,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/results_4yr_15dsphs.txt',edsph,dsph,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/sim_10yr_35dsphs_spectral_bands_00000_00499.txt',masses,mode,q02,q05,q16,q25,q50,q75,q84,q95,q97
  readcol,'~/Fermi/Senior_Review/SR2014/dm/Veritas_Segue1.txt',everitas,veritas,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/HESS_GC_Generic.txt',ehess,hess,format='(f,f)'
  s=sort(ehess)
  ehess=ehess[s]
  hess=hess[s]
  readcol,'~/Fermi/Senior_Review/SR2014/dm/direct.txt',edirect,direct,format='(f,f)'
  readcol,'~/Fermi/Senior_Review/SR2014/dm/colliders.txt',ecollid,collid,format='(f,f)'
  direct=direct*3e-26
  collid=collid*3e-26
  readcol,'~/Fermi/Senior_Review/SR2014/dm/limit_bb_10deg.txt',ehalo10,halo10,format='(d,d)'


  p=plot([10,1e3],[1e-27,1e-23],/xlog,/ylog,/nodata,xtitle='WIMP Mass (GeV)',ytitle='Annihilation Cross Section  $\langle\sigma_A v\rangle  (cm^3s^{-1})$',yrange=[1e-27,1e-23],xrange=[10,1e3],font_size=12)  
  p1=plot(ehalo,halo,/overplot,thick=2,color='blue')
  t1=text(10.7,9.8e-27,'LAT MW Halo (2 yrs)',/data,orientation=27,font_size=12,color='blue')
  p2=plot(ehalo10,halo10,/overplot,thick=3,color='blue',linestyle='--')
  t2=text(90,4.2e-26,'LAT MW Halo (10 yrs)',/data,orientation=12,font_size=12,color='blue')

;  p2=plot(ecluster,cluster,/overplot,linestyle=[2, 'F3F3'X],thick=2,color='green')
;  t2=text(40,8e-25,'LAT Cluster Stacking',/data,orientation=20,font_size=10,color='green')
;  p3=plot(ediffuse,diffuse,/overplot,thick=2,color='red');,linestyle=[1, 'E0E0'X])
;  t3=text(160,5e-26,'LAT Isotropic Diffuse (1 yr)',/data,orientation=20,font_size=10,color='red')
  p4=plot(edsph,dsph,/overplot,linestyle='-',thick=2);,color='red')
  t4=text(13,4e-26,'LAT Dwarf Spheriodal Stacking (4 yrs, 15 dSph)',/data,orientation=17,font_size=12);,color='red')
  p5=plot(masses,q50,/overplot,thick=3,linestyle=[2,'F3F3'X]);,color='red')
  t4=text(13,1.5e-27,'LAT Dwarf Spheriodal Stacking',/data,orientation=15,font_size=12);,color='red')
  t4=text(13,1e-27,'    (10 yrs, 35 dSph)',/data,orientation=15,font_size=12);,color='red')

;  p6=plot(everitas,veritas,/overplot,linestyle='--',color='grey')

  p7=plot(ehess,hess,/overplot,color='light slate grey',thick=3)
  t7a=text(435,4.5e-24,'Current',/data,font_size=12,color='light slate grey')
  t7b=text(420,3e-24,'IACTs GC',/data,font_size=12,color='light slate grey')
  t7c=text(390,2e-24,'Halo (100 hrs)',/data,font_size=12,color='light slate grey')
  h2=hess/2.
  h2[0]=2e-24
  h2[10]=1e-24
  h2[13]=6.3e-25
  i=indgen(n_elements(hess))
  i=[i[0],i[10],i[13:*]]
;  p6=plot(ehess,h2,/overplot,color='slate grey')
  p8=plot(ehess[i],h2[i],/overplot,color='light slate grey',thick=3,linestyle='--')
  t8a=text(255,2.2e-25,'H.E.S.S.-II',/data,font_size=12,color='light slate grey');,orientation=320)
  t8b=text(245,1.5e-25,'GC Halo (100 hrs)',/data,font_size=12,color='light slate grey');,orientation=320)

;  p9=plot(edirect,direct,/overplot,color='orange',thick=3)
;  t9=text(40,2e-25,'COUPP *',/data,font_size=12,color='orange',orientation=45)
;  p10=plot(ecollid,collid,/overplot,color='green',thick=3,transparency=20)
;  t10=text(40,7e-27,'LHC *',/data,font_size=12,color='green',orientation=39)

  pp=plot([10,1e4],[3e-26,3e-26],color='dim grey',thick=10,/overplot,transparency=50)
  t5=text(215,1.6e-26,'Thermal Cross Section',/data,font_size=12,color='dim grey')
  p.save,'~/Fermi/Senior_Review/SR2014/dm_v2.png'
  p.refresh
  p.close

stop
  return
end 

pro msp
  
  year=2008.5+indgen(7)
  msp=[0,17,29,38,46,56,0]

  p=barplot(year,msp,color='blue',xtitle='Year',ytitle='Cumulative Number',xrange=[2008,2014],yrange=[0,60],aspect_ratio=0.05,title='Millisecond Pulsars discovered in          -enabled searches',xminor=0,yminor=0)
  t=text(0.58,0.777,'Fermi',font_style='it')


  p.save,'~/Fermi/Senior_Review/SR2014/msp.png'
  p.refresh
  p.close

  return
end 

pro gbm_map

  cd,'~/Fermi/Senior_Review/SR2014/gbm_map/'

  trig=mrdfits('gbm_trigcat.fits',1)
  euler,trig.lii,trig.bii,elon,elat,6
  sf=where(strtrim(trig.trigger_type,2) eq 'SFL' or strtrim(trig.trigger_type,2) eq 'SFLARE' and elat lt 10 and elat gt -10)
  grb=where(strtrim(trig.trigger_type,2) eq 'GRB')
  sgr=where(strtrim(trig.trigger_type,2) eq 'SGR' and (elat gt 1 or elat lt -1))
  
  m=map('Hammer',label_position=0)
  m.mapgrid.label_show=0
  m.mapgrid.linestyle='none'
;  grid=m.mapgrid
;  grid.linestyle="dotted"
;  grid.label_position=0

  p1=plot(-trig[grb].lii,trig[grb].bii,/overplot,symbol='circle',linestyle=' ',/current)
  p1.sym_filled=1
  p1.sym_fill_color='green'
  p1.sym_size=0.8

  p2=plot(-trig[sf].lii,trig[sf].bii,/overplot,symbol='circle',linestyle=' ',/current)
  p2.sym_filled=1
  p2.sym_fill_color='yellow'
  p2.sym_size=0.8

  p3=plot(-trig[sgr].lii,trig[sgr].bii,/overplot,symbol='circle',linestyle=' ',/current)
  p3.sym_filled=1
  p3.sym_fill_color='red'
  p3.sym_size=0.8


  m.save,'~/Fermi/Senior_Review/SR2014/gbm_map/gbm_map.pdf',/landscape,width=10.5,/close
  m.refresh
  m.close

  stop
  return
end 

pro pulsars
;The columns are MET start and end, flux, flux error, and upper
;limit. The last three are all in counts per second per square
;centimeter.  If the upper limit is not infinite, then it should be
;used instead of the flux value. Additional information you might
;need: long-term average pre-disappearance (1.05(11)e-8) and
;post-disappearance (6.3(6)e-8), and the date of disappearance (2013
;June 23).

  readcol,'~/Fermi/Senior_Review/SR2014/top-fig2.txt',metstart,metstop,flux,fluxerr,ul,format='(d,d,d,d,d)'
  wul=where(ul lt 1,nul)
  wdet=where(ul gt 1)
  d0=date2met('2008-07-01-00:00:00',/fermi)
  year=86400.*365.25
  yearstart=(metstart-d0)/year
  yearstop=(metstop-d0)/year
  midyear=(yearstop+yearstart)/2.

  ul=ul*1e8
  flux=flux*1e8
  fluxerr=fluxerr*1e8

  !y.margin=[3,12]
  begplot,name='~/Fermi/Senior_Review/SR2014/psrj1023.ps',/land,/color,font='helvetica'
  loadct,39
  plot,[2008.5,2014],[0,10],/nodata,xrange=[2008.5,2014],yrange=[0,9],xtitle='Year',ytitle='Flux (0.1-300 GeV) (10!U-8!N erg cm!U-2!N s!U-1!N)',/ysty,xminor=4
;  p2=errorplot(2008.5+midyear[wul],ul[wul]-fluxerr[wul],midyear[wul]-yearstart[wul],fluxerr[wul],/overplot,linestyle='none')
  plotsym,1,3,thick=5
  oplot,2008.5+midyear[wul],ul[wul],psym=8
  for i=0,nul-1 do oplot,2008.5+[yearstart[wul[i]],yearstop[wul[i]]],[ul[wul[i]],ul[wul[i]]]
  oploterror,2008.5+midyear[wdet],flux[wdet],midyear[wdet]-yearstart[wdet],fluxerr[wdet],psym=3,thick=10
  oplot,[2008.5,2013.5],[1.7,1.7],color=!red,thick=7,line=2
  oplot,[2013.5,max(yearstop)+2008.5],[6.2,6.2],color=!red,thick=7,line=2
  oplot,[2013.48,2013.48],[0,9],color=60

  endplot
  spawn,'convert -rotate 270 ~/Fermi/Senior_Review/SR2014/psrj1023.ps ~/Fermi/Senior_Review/SR2014/psrj1023.pdf'

  stop
return
end 

pro data_latency


;;   readcol,'~/Fermi/Senior_Review/SR2012/GBM_data_latency.csv',trig,time,format='(a,a)'
;;   gbm=fltarr(n_elements(time))
;;   for i=1,n_elements(time)-1 do begin 
;;      c=strsplit(time[i],':',/extra)
;;      day=c[0]
;;      hour=c[1]
;;      minute=c[2]
;;      sec=c[3]
;;      gbm[i]=hour+minute/60.+sec/3600.+day*24.
;;   endfor 

;;   readcol,'~/Fermi/Senior_Review/SR2014/GBM_trigger_data_latency.csv',trig,nasa,gioc,total,format='(a,d,d,d)',delim=','
;;   gbm2=total/3600.
;;   gbm=[gbm,gbm2]
;; ;  gbm=gbm2
;;   w=where(gbm lt 100 and gbm gt 0)
;;   gbm=gbm[w];+1.

  readcol,'~/Fermi/Senior_Review/SR2014/GBM_trigger_data_latency_Rev.csv',trig,total,format='(a,d)'
  gbm=total/3600.
  w=where(gbm ne 0)
  gbm=gbm[w]

  readcol,'~/Fermi/Senior_Review/SR2014/datatime-stats2014.csv',runstart,slac,nasa,format='(d,d,d)',skip=1
  lat=slac+nasa;+1.
;  lat=lat[28000:*]

  plothist,alog10(gbm),gbmx,gbmy,bin=0.02,/noplot
  plothist,alog10(lat),latx,laty,bin=0.02,/noplot
  p=barplot(latx,laty*1./max(laty)*90.,xrange=[0,2],yrange=[0,100],ytitle='Arbitrary Units',fill_color='black',transparency=20,width=1.,color='black',xminor=0,xmajor=0,aspect_ratio=0.01)
  p=barplot(gbmx,gbmy*1./max(gbmy)*80.,/overplot,xrange=[0,2],yrange=[0,100],fill_color='blue',transparency=30,color='blue')
  t=text(alog10(1.3),90,'GBM Triggers',/data,font_color='blue',font_size=14)
  t=text(alog10(25.),90,'LAT Data Runs',/data,font_color='black',font_size=14)
  xaxis=axis(0,location=[0,0],/data,tickvalues=alog10([1.,10.,100.]),tickname=['1','10','100'],minor=0,title='Data Processing Latency (hours)')
  xaxis=axis(0,location=[0,100],/data,tickvalues=alog10([1.,10.,100.]),minor=0,tickname=[' ',' ',' '],tickdir=1)
  tval=[2,3,4,5,6,7,8,9,20,30,40,50,60,70,80,90.]
  for i=0,n_elements(tval)-1 do begin 
     p=plot([alog10(tval[i]),alog10(tval[i])],[0,2],/overplot)
     p=plot([alog10(tval[i]),alog10(tval[i])],[98,100],/overplot)
  endfor 

  p.save,'~/Fermi/Senior_Review/SR2014/data_latency.png',/landscape,/close;,bitmap=1,width=10
  p.refresh
  p.close

  stop
  return
end 
