@fit_functions
@jet_angle
@fit_lc2
pro venice_lcs,falpha,falphaerr,dofit=dofit,ps=ps,onefile=onefile
  
  mdir='/Users/jracusin/Documents/Grad School/Papers/Venice_jet_breaks/'
;  grbs='GRB'+['050315','060428A','050814','060614','050401','050416A','050607','050803','050822','051109A','051117A','060202','060206','060319','060729','060814','061007','050525A','060124']
;  grbs=grbs[sort(grbs)]
   grbs='GRB'+['050315','050814','050820A','051221A','060428A','060614','050401','050416A','051109A','060206','060729','061007','061007','050607','050803','050822','051117A','060202','060319','060814','060124']
  
  
  if keyword_set(dofit) then fit_lc_wrapper,dir=grbs,mdir=mdir,siglim=2.3
  
  font=2
  
  if keyword_set(ps) and keyword_set(onefile) then begplot,name='venice_plots.ps',/land,/color
;  !p.multi=[0,2,5]
  cd,mdir
  falpha=dblarr(n_elements(grbs)) & falphaerr=falpha & tbreaks=falpha
  for i=0,n_elements(grbs)-1 do begin
     if keyword_set(onefile) then title=grbs[i] else title=''
     if keyword_set(ps) and not keyword_set(onefile) then begplot,name=grbs[i]+'.ps',/color,/land,font='helvetica'
     print,i,' ',grbs[i]
     cd,grbs[i]
     if exist('lc_newout_noflares.txt') then file='lc_newout_noflares.txt' $
        else file='lc_newout.txt'
     lc=lcout2fits(file)
     if i eq 0 then $
        plot_like_qdp,name=grbs[i],title=title,charsize=2.0,siglim=2.8,xrange=[10,1e7],/xsty,font=font $
     else $
     plot_like_qdp,name=grbs[i],title=title,charsize=2.0,siglim=2.2,font=font
     file='lc_fit_out_idl.dat'
     read_lcfit,file,pname,p,perror,chisq,dof,breaks
     time=lc.time
     case breaks of
        0: yfit=pow(time,p)
        1: yfit=bknpow(time,p)
        2: yfit=bkn2pow(time,p)
        3: yfit=bkn3pow(time,p)
     endcase         
     print,p[n_elements(p)-2]/86400.

     br1=min(time) & br2=max(time)
     for j=0,breaks*2,2 do begin
        alpha=p[j+1]
        if breaks gt 0 and j ne breaks*2 then br2=p[j+2] else br2=max(time)
        
        print,br1,br2
        w=where(time ge br1 and time le br2)
        t=[br1,time[w],br2]
        case breaks of
           0: yfit2=pow(t,p)
           1: yfit2=bknpow(t,p)
           2: yfit2=bkn2pow(t,p)
           3: yfit2=bkn3pow(t,p)
        endcase         
;        x=(max(t)-min(t))/2.
        
        if j ne 0 then $
           x=10^((alog10(max(t))-alog10(min(t)))/2.+alog10(min(t))) $
           else x=median(t)
        mint=min(abs(x-time),m)
        y=yfit[m]
        
        alp=ntostr(round(alpha*10.)/10.,3)
        xyouts,x,y*2.,!tsym.alpha+!tsym.approx2+alp,charsize=2.,font=font

        br1=br2
        oplot,t,yfit2
        if i eq 9 then begin 
           t2=(indgen(15)*10.+6.)*1e4
           yfit3=pow(t2,p)
           norm=yfit3[0]/(t2[0]^(-1.79))
           yfit4=pow(t2,[norm,1.79])
           oplot,t2,yfit4,line=2,thick=3
           xyouts,1e5,5e-4,!tsym.alpha+!tsym.approx2+'1.8',charsize=2.,font=font
        endif 
        tbreaks[i]=p[n_elements(p)-2]
        falpha[i]=p[n_elements(p)-1]
        falphaerr[i]=perror[n_elements(perror)-1]
     endfor 
;     oplot,time,yfit
     
;     k=get_kbrd(10)
;     if k eq 's' then stop
;     cd,'..'
     cd,mdir
  endfor 
  !p.multi=0  
  if keyword_set(ps) then endplot
  
  tbreak=[2.8,1.0,14.5,4.1,9.4,1.3,9,71,16,15,125,13,9.3e-4,19,13,52,20,29,42,15]
;  tbreak=[2.2,1.0,1.2,14.5,4.1,9.4,12,43,16,29,125,15,9.3e-4,28,13,52,23,29,46,15]
;  tbreak[[0,2,3,5]]=tbreaks[[0,2,3,5]]/86400.
  t=2.5
  z=[1.949,5.3,2.612,0.546,t,0.125,2.9,0.654,2.345,4.048,0.54,1.26,1.26,t,t,t,t,t,t,t]
  t=10.
  eiso52=[3.3,18,83,0.15,t,0.25,35,0.12,5.,5.8,1.6,100,100,t,t,t,t,t,t,t]

  eiso53=eiso52/10.
  theta=dblarr(n_elements(tbreak))
  
  for i=0,n_elements(tbreak)-1 do begin
     if z[i] ne 2.5 then $
        j=jet_angle(tbreak[i],z=z[i],eiso=eiso53[i]) $
     else j=jet_angle(tbreak[i])
     theta[i]=j
  endfor
  
  egam=alog10(eiso52*1d52*(1.-cos(theta*!dtor)))
  w=where(eiso52 eq t)
  egam[w]=-1
  z[w]=-1
  eiso52[w]=-1
  
;  venice_test,tbreak,z,eiso52,theta,egam

  s=' '
  zs=sigfig(z,3)
  w=where(z eq -1)
  zs[w]='    '
  eiso52s=sigfig(eiso52,3)
  eiso52s[w]='    '
  egams=sigfig(egam,3)
  egams[w]='    '
  sp=[s,s,s,'','','',s,'','',s,s,s,s,s,s,s,'',s,s,s]+' '
  sa=strarr(n_elements(falpha))
  sa[where(falpha ge 1)]=' '
;  colprint,grbs+s+ntostr(fix(tbreak))+s+sigfig(falpha,3)+'+/-'+sigfig(falphaerr,2)+s+zs+s+eiso52s+s+ntostr(theta,3)+s+egams
  print,'   GRB   ','  tbreak','      a   ','     z  ',' E52 ',' theta','  Egam '
    colprint,grbs+sp,sigfig(tbreak,3)+' ',sigfig(falpha,3)+'+/-'+sigfig(falphaerr,2)+sa+' ',zs+' ',eiso52s+' ',sigfig(theta,3)+' ',egams
  
  

  stop
  return
end 
