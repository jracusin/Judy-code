pro grb080319b_wt_psf
  
  
  srcra=217.92154d
  srcdec=36.300338d

  cd,'~/Desktop/GRB080319B/psf_corr/'
  evfiles=file_search('seg*evt')
  expofiles=file_search('seg*ex.img')
  excl=[14,14,14,10,10,10,10,2,0,0]
  
  psfquick,2,2,r,expf,ef
  evfiles=[evfiles[1:*],evfiles[0]]
  expofiles=[expofiles[1:*],expofiles[0]]
  colprint,evfiles,expofiles
  
  rc0=400
  psfcorr=fltarr(10) & expfrac=psfcorr & psffrac=fltarr(10) & expcorr=fltarr(10)
  for i=0,9 do begin
;     regfile='src_excl'+ntostr(excl[i])+'.reg'
     find_wt_srcdetpos,evfiles[i],srcra,srcdec,srcx,srcy,pa
     ev=mrdfits(evfiles[i],1)
     roll=pa
     expomap=mrdfits(expofiles[i])
     expo2=rot(expomap,pa)
;     expo3=expo2[200:799,200:799]
     
     s1=399
     s2=s1+199
     expo3=expo2;[s1:s2,s1:s2]
     print,srcx,srcy
;     sx=(round(srcx)-200.)+5
;     sy=(round(srcy)-200.)+5
     sx=(srcx)+6
     sy=(srcy)+6
     
     evmap=fltarr(1000,1000)
     for k=0L,n_elements(ev)-1 do $
        evmap[ev[k].x,ev[k].y]=evmap[ev[k].x,ev[k].y]+1
     evmap2=rot(evmap,pa)
     evmap3=evmap2;[s1:s2,s1:s2]
;     rdis,evmap3,xmn=200,ymn=200,xmx=400,ymx=400
     
     wt_psf,excl[i],r,expf,ef,sx,sy,expo3,efrac,ecorr,pfrac,pcorr,evmap3,cmap
     oplot,ev.detx+200,ev.dety+200,psym=3,color=!red
     
     rdis,evmap3+cmap,xmn=rc0,ymn=rc0,xmx=rc0+200,ymx=rc0+200
     
     oplot,[sx-20,sx-20,sx-excl[i]/2.,sx-excl[i]/2.,sx-20],[sy-10,sy+10,sy+10,sy-10,sy-10],color=!green
     oplot,[sx+20,sx+20,sx+excl[i]/2.,sx+excl[i]/2.,sx+20],[sy-10,sy+10,sy+10,sy-10,sy-10],color=!green
;     roll=pa-90
;     evx=ev.x*cos(roll*!dtor)+ev.y*sin(roll*!dtor)-400
;     evy=400-ev.x*sin(roll*!dtor)+ev.y*cos(roll*!dtor)
;     oplot,evx,evy,psym=3,color=!red
     


     psffrac[i]=pfrac
     psfcorr[i]=pcorr
     expfrac[i]=efrac
     expcorr[i]=ecorr
     print,i
     k=get_kbrd(10)
     if k eq 's' then stop
  endfor 
    !p.multi=0
  corr=expcorr*psfcorr
  colprint,indgen(10)+1,expfrac,expcorr,psffrac,psfcorr

  print
  colprint,indgen(10)+1,excl,expcorr,psfcorr,corr
  
  stop
  return
end 
