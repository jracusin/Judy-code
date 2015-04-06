pro plot_lc_stats,ps=ps
  
  cd,!mdata
  outdir='~/papers/jetbreaks1/'
;  dir=file_search('GRB*')
  cr=mrdfits(!mdata+'closure_relations_total_2sig.fits',1)
  
;  lcfile='lc_fit_out_idl_int2.dat'
  
  a0=0. & a1=0. & a2=0. & a3=0. & a4=0.
  b0=0. & b1=0. & b2=0. & b3=0. & b4=0.
  
  u=uniq(cr.grb)
  for i=0,n_elements(u)-1 do begin 
     w=where(cr.grb eq cr[u[i]].grb,nw)
     alphas=cr[w].alpha
     betas=cr[w].beta
     breaks=nw-1
                                ;    cd,dir[i]
                                ;    print,dir[i]
     
;     if exist(lcfile) then begin 
;        read_lcfit,lcfile,pname,p,perror,chisq,dof,breaks,lc=lc
;        if exist('spec') then begin 
;           read_specfit,spec,dir='spec'
     
;           alphas=p[indgen(breaks+1)*2+1]
     case breaks of
        0: begin 
           a0=[a0,alphas[0]]
           b0=[b0,betas]
        end
        1: begin
           if alphas[0] gt alphas[1] then begin
              a1=[a1,alphas[0]]
              b1=[b1,betas[0]]
              a2=[a2,alphas[1]]
              b2=[b2,betas[1]]
           endif else begin 
              a2=[a2,alphas[0]]
              b2=[b2,betas[0]]
              a3=[a3,alphas[1]]
              b3=[b3,betas[1]]
           endelse 
        end
        2: begin
           if alphas[0] gt alphas[1] then begin
              a1=[a1,alphas[0]]
              b1=[b1,betas[0]]
              a2=[a2,alphas[1]]
              b2=[b2,betas[1]]
              a3=[a3,alphas[2]]
              b3=[b3,betas[2]]
           endif else begin 
              a2=[a2,alphas[0]]
              b2=[b2,betas[0]]
              a3=[a3,alphas[1]]
              b3=[b3,betas[1]]
              a4=[a4,alphas[2]]
              b4=[b4,betas[2]]
           endelse 
        end
        3: begin 
           a1=[a1,alphas[0]]
           b1=[b1,betas[0]]
           a2=[a2,alphas[1]]
           b2=[b2,betas[1]]
           a3=[a3,alphas[2]]
           b3=[b3,betas[2]]
           a4=[a4,alphas[3]]
           b4=[b4,betas[3]]
        end
     endcase 
     
;        endif 
;     endif

;     cd,'..'
  endfor 
  
  a0=a0[1:*]
  a1=a1[1:*]
  a2=a2[1:*]
  a3=a3[1:*]
  a4=a4[1:*]
  b0=b0[1:*]
  b1=b1[1:*]
  b2=b2[1:*]
  b3=b3[1:*]
  b4=b4[1:*]
  
  if keyword_set(ps) then begplot,name=outdir+'alphas.eps',font='helvetica',/encap
  bin=0.1
  yrange=[0,30]
  xrange=[-1,3]
;  window,0
  erase
  multiplot,[1,5],/init
  multiplot
  plothist,a0,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'single PL',box=0,/top,/right
  oplot,[0,0],[0,100],line=2
;  legend,!tsym.alpha+'!L0!N',box=0,/top,/right
  multiplot
  plothist,a1,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'I',box=0,/top,/right
  oplot,[0,0],[0,100],line=2
;  legend,!tsym.alpha+'!L1!N',box=0,/top,/right
  multiplot
  plothist,a2,bin=bin,ytitle='N',yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'II',box=0,/top,/right
  oplot,[0,0],[0,100],line=2
;  legend,!tsym.alpha+'!L2!N',box=0,/top,/right
  multiplot
  plothist,a3,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'III',box=0,/top,/right
  oplot,[0,0],[0,100],line=2
;  legend,!tsym.alpha+'!L3!N',box=0,/top,/right
  multiplot
  plothist,a4,bin=bin,xtitle=!tsym.alpha,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'IV',box=0,/top,/right
    oplot,[0,0],[0,100],line=2
;  legend,!tsym.alpha+'!L4!N',box=0,/top,/right
  multiplot,/reset
  if keyword_set(ps) then endplot  
;  window,1
  
  print,'KS TESTS ALPHA'
  print,'              ',ntostr(indgen(5))+'           '
  ;;KS tests alpha0 with other alpha
  kstwop,a0,a0,d00,p00
  kstwop,a0,a1,d01,p01
  kstwop,a0,a2,d02,p02
  kstwop,a0,a3,d03,p03
  kstwop,a0,a4,d04,p04
  print,0,p00,p01,p02,p03,p04
    
  ;;KS tests alpha1 with other alpha
  kstwop,a1,a0,d10,p10
  kstwop,a1,a1,d11,p11
  kstwop,a1,a2,d12,p12
  kstwop,a1,a3,d13,p13
  kstwop,a1,a4,d14,p14
  print,1,p10,p11,p12,p13,p14
  
  ;;KS tests alpha2 with other alpha
  kstwop,a2,a0,d20,p20
  kstwop,a2,a1,d21,p21
  kstwop,a2,a2,d22,p22
  kstwop,a2,a3,d23,p23
  kstwop,a2,a4,d24,p24
  print,2,p20,p21,p22,p23,p24
  
  ;;KS tests alpha3 with other alpha
  kstwop,a3,a0,d30,p30
  kstwop,a3,a1,d31,p31
  kstwop,a3,a2,d32,p32
  kstwop,a3,a3,d33,p33
  kstwop,a3,a4,d34,p34
  print,3,p30,p31,p32,p33,p34
  
  ;;KS tests alpha4 with other alpha
;  !p.multi=[0,2,3]
  kstwop,a4,a0,d40,p40;,/plot
  kstwop,a4,a1,d41,p41;,/plot
  kstwop,a4,a2,d42,p42;,/plot
  kstwop,a4,a3,d43,p43;,/plot
  kstwop,a4,a4,d44,p44;,/plot
  print,4,p40,p41,p42,p43,p44
;  !p.multi=0
  
  if keyword_set(ps) then begplot,name=outdir+'betas.eps',font='helvetica',/encap else k=get_kbrd(10)
  erase
  bin=0.1
  yrange=[0,30]
  xrange=[0,3]
  multiplot,[1,5],/init
  multiplot
  plothist,b0,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'single PL',box=0,/top,/right
;  legend,!tsym.beta+'!L0!N',box=0,/top,/right
  multiplot
  plothist,b1,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'I',box=0,/top,/right  
;  legend,!tsym.beta+'!L1!N',box=0,/top,/right
  multiplot
  plothist,b2,bin=bin,ytitle='N',yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'II',box=0,/top,/right  
;  legend,!tsym.beta+'!L2!N',box=0,/top,/right
  multiplot
  plothist,b3,bin=bin,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'III',box=0,/top,/right  
;  legend,!tsym.beta+'!L3!N',box=0,/top,/right
  multiplot
  plothist,b4,bin=bin,xtitle=!tsym.beta,yrange=yrange,xrange=xrange,/fill,min=xrange[0],max=xrange[1]
  legend,'IV',box=0,/top,/right  
;  legend,!tsym.beta+'!L4!N',box=0,/top,/right
  multiplot,/reset
  if keyword_set(ps) then endplot
  
  print,'KS TESTS BETA'
  print,'              ',ntostr(indgen(5))+'           '
  ;;KS tests beta0 with other beta
  kstwop,b0,b0,d00,p00
  kstwop,b0,b1,d01,p01
  kstwop,b0,b2,d02,p02
  kstwop,b0,b3,d03,p03
  kstwop,b0,b4,d04,p04
  print,0,p00,p01,p02,p03,p04
    
  ;;KS tests beta1 with other beta
  begplot,name='~/papers/jetbreaks1/seg1_ks.ps',/land
  !p.multi=[0,2,2]
  kstwop,b1,b0,d10,p10,/plot,xtitle=!tsym.beta+'!LI-SPL!N',ytitle='p'
  kstwop,b1,b1,d11,p11;,/plot,title='KS I-I',xtitle=!tsym.beta+'!LI!N'
  kstwop,b1,b2,d12,p12,/plot,xtitle=!tsym.beta+'!LI-II!N',ytitle='p'
  kstwop,b1,b3,d13,p13,/plot,xtitle=!tsym.beta+'!LI-III!N',ytitle='p'
  kstwop,b1,b4,d14,p14,/plot,xtitle=!tsym.beta+'!LI-IV!N',ytitle='p'
  print,1,p10,p11,p12,p13,p14
  !p.multi=0
  endplot
  
  ;;KS tests beta2 with other beta
  kstwop,b2,b0,d20,p20
  kstwop,b2,b1,d21,p21
  kstwop,b2,b2,d22,p22
  kstwop,b2,b3,d23,p23
  kstwop,b2,b4,d24,p24
  print,2,p20,p21,p22,p23,p24
  
  ;;KS tests beta3 with other beta
  kstwop,b3,b0,d30,p30
  kstwop,b3,b1,d31,p31
  kstwop,b3,b2,d32,p32
  kstwop,b3,b3,d33,p33
  kstwop,b3,b4,d34,p34
  print,3,p30,p31,p32,p33,p34
  
  ;;KS tests beta4 with other beta
  kstwop,b4,b0,d40,p40
  kstwop,b4,b1,d41,p41
  kstwop,b4,b2,d42,p42
  kstwop,b4,b3,d43,p43
  kstwop,b4,b4,d44,p44
  print,4,p40,p41,p42,p43,p44
  
  stop
  
  return
end
