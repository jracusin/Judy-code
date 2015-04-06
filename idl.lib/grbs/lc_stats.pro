@fit_functions
pro lc_stats,cumlc,tmin,tmax,use,fluxctsratio,redshift=redshift
  
  ;;redshift option not working properly
  cd,!mdata
  dir=file_search('GRB*')
  ndir=n_elements(dir)
  lcfitfile='lc_fit_out_idl_int9.dat'
  n=1000.
  cr=mrdfits(!mdata+'closure_relations_total_2sig.fits',1)  
  wz=where(cr.z gt 0.)
  
  u=uniq(cr.grb)
  crz=cr[u].z
  match,strtrim(cr[u].grb,2),dir,m1,m2
  z=fltarr(ndir)
  if keyword_set(redshift) then z[m2]=cr[u[m1]].z
  
;  tmn=1.
;  tmx=1e6
;  tdiff=tmx-tmn
;  t=reverse(alog10(n/(findgen(n)+1)+1e-3))/alog10(n+1)*tdiff[0]+tmn
  t=[findgen(99)+1,findgen(200)*10.+100.,findgen(200)*30.+2000.,findgen(101)*200.+8000.,findgen(100)*2000.+30000.,findgen(100)*1e4+2.5e5,findgen(200)*2e4+1.3e6]
  
  ;;1-99 x 1 (99),100-2000 x 10 (200),2000-8000 x 30 (200),8000-28000 x 100 (200), 30000-230000 x 2000 (100), 2.5e5-1.25e6 x 1e4 (100),1.15e6-2.6e7 x 2.5e5 (100)
  
  if n_elements(cumlc) eq 0 then begin 
     
     nlc=0
     use=0
     cumlc=fltarr(n,ndir)
     tmin=fltarr(ndir)
     tmax=fltarr(ndir)
     fluxctsratio=dblarr(ndir)
     
     for i=0,ndir-1 do begin
        print,dir[i]
        cd,dir[i]
        lcfile='lc_newout_noflares_xrt.txt'
        if not exist(lcfile) then lcfile='lc_newout_noflares.txt'
        if not exist(lcfile) then lcfile='lc_newout_phil.txt'
        print,lcfile
        if exist(lcfitfile) and exist(lcfile) and exist('spec/seg1.dat') then begin 
           read_specfit,spec;,dir='spec'
           w0=where(spec.nev gt 60 or spec.nev2 gt 60,nw0)
           if numlines(lcfitfile) gt 1 and nw0 gt 0 then begin 
              ;;mean flux-to-counts
              w2=where(spec.rate2 gt 0,nw2)
              wno0=where(spec.rate gt 0)
              if nw2 eq 0 then $
                 fluxctsratio[i]=total(spec[wno0].unabs_flux/spec[wno0].rate*spec[wno0].exptime)/total(spec[wno0].exptime) else $
                    fluxctsratio[i]=total(spec[wno0].unabs_flux/spec[wno0].rate*spec[wno0].exptime+spec[w2].unabs_flux2/spec[w2].rate2*spec[w2].exptime2)/total(spec[wno0].exptime+spec[w2].exptime2)              
              read_lcfit,lcfitfile,pname,p,perror,chisqs,dof,breaks,lc=lcfit
              lc=lcout2fits(lcfile)
              wdet=where(lc.src_rate_err gt 0,nwdet)
              if nwdet gt 2 then begin 
                 tmin[i]=lc[wdet[0]].tstart;/(1.+z[i])
                 tmax[i]=max(lc[wdet].tstop);/(1.+z[i])

                 case breaks of 
                    0: model='pow'
                    1: model='bknpow'
                    2: model='bkn2pow'
                    3: model='bkn3pow'
                 endcase 
                 
;                 if breaks gt 0 then begin
;                    g=(indgen(breaks)+1)*2
;                    p[g]=p[g]/(1.+z[i])
;                 endif 
                 
                 tmp=execute('ctr='+model+'(t,p)')
                 cumlc[*,i]=ctr

                 use=[use,i]
                 nlc=nlc+1
              endif 
           endif
        endif 
        cd,'..'
     endfor
     ;;removes any GRBs w/o LCs
     use=use[1:*]
     use=where(finite(fluxctsratio) eq 1)
;     cumlc=cumlc[*,use]
  endif 
  
  meanlc=dblarr(n)
  medianlc=dblarr(n)
  maxlc=dblarr(n)
  minlc=dblarr(n)
  mingrb=strarr(n)
  maxgrb=strarr(n)
  nmean=intarr(n)
  
  meanlcflux=dblarr(n)
  medianlcflux=dblarr(n)
  maxlcflux=dblarr(n)
  minlcflux=dblarr(n)
  mingrbflux=strarr(n)
  maxgrbflux=strarr(n)
  
  ;;only use those with z
  
  for i=0,n-1 do begin
     ;;if time in observed range
     if keyword_set(redshift) then wt=where(tmin[use] lt t[i] and tmax[use] gt t[i] and z[use] gt 0.,nwt) else wt=where(tmin[use] lt t[i] and tmax[use] gt t[i],nwt)

     if nwt gt 0 then begin 
        ;;counts
        meanlc[i]=mean(cumlc[i,use[wt]])
        nmean[i]=nwt
        medianlc[i]=median(cumlc[i,use[wt]])
        maxlc[i]=max(cumlc[i,use[wt]],m)
        maxgrb[i]=dir[use[wt[m]]]
        minlc[i]=min(cumlc[i,use[wt]],m)
        mingrb[i]=dir[use[wt[m]]]
        
        ;;flux
        meanlcflux[i]=mean(cumlc[i,use[wt]]*fluxctsratio[use[wt]])
        medianlcflux[i]=median(cumlc[i,use[wt]]*fluxctsratio[use[wt]])
        maxlcflux[i]=max(cumlc[i,use[wt]]*fluxctsratio[use[wt]],m)
        maxgrbflux[i]=dir[use[wt[m]]]
        minlcflux[i]=min(cumlc[i,use[wt]]*fluxctsratio[use[wt]],m)
        mingrbflux[i]=dir[use[wt[m]]]
     endif 
  endfor 
  
  plotsym,0,1,/fill
  s=0.5
  if keyword_set(redshift) then begin 
     name='lc_stats_z.ps' 
     title='Rest frame'
  endif else name='lc_stats.ps'
  begplot,name=name,/color
;  !p.multi=[0,1,3]
  multiplot,[1,3],/init

  wmean=where(meanlc gt 0.)
  wmedian=where(medianlc gt 0.)
  wmin=where(minlc gt 0.)
  wmax=where(maxlc gt 0.)
  
  yrange=[1e-5,1e3]
  multiplot
  plot,t[wmean],meanlc[wmean],/xlog,/ylog,ytitle='Count Rate (0.3-10.0 keV) (s!U-1!N)',psym=8,yrange=yrange,symsize=s,charsize=1,title=title
  oplot,t[wmedian],medianlc[wmedian],psym=1,color=!red,symsize=s
  oplot,t[wmin],minlc[wmin],psym=5,color=!blue,symsize=s
  oplot,t[wmax],maxlc[wmax],psym=4,color=!green,symsize=s
  legend,['Mean','Median','Min','Max'],color=[!p.color,!red,!blue,!green],psym=[8,1,5,4],box=0,/top,/right,charsize=1
  
  wmeanf=where(meanlcflux gt 0.)
  wmedianf=where(medianlcflux gt 0.)
  wminf=where(minlcflux gt 0.)
  wmaxf=where(maxlcflux gt 0.)
  
  yrange=[1e-15,1e-7]
  multiplot
  plot,t[wmeanf],meanlcflux[wmeanf],/xlog,/ylog,ytitle='Flux (0.3-10.0 keV) (erg s!U-1!N cm!U-2!N)',psym=8,yrange=yrange,symsize=s,charsize=1,title=title
  oplot,t[wmedianf],medianlcflux[wmedianf],psym=1,color=!red,symsize=s
  oplot,t[wminf],minlcflux[wminf],psym=5,color=!blue,symsize=s
  oplot,t[wmaxf],maxlcflux[wmaxf],psym=4,color=!green,symsize=s
  legend,['Mean','Median','Min','Max'],color=[!p.color,!red,!blue,!green],psym=[8,1,5,4],box=0,/top,/right,charsize=1
  
  multiplot
  plot,t,nmean,/xlog,xtitle='time since trigger (s)',ytitle='N in mean',charsize=1.,xrange=[10,1e7]
  multiplot,/reset
;  !p.multi=0
  endplot
  
  
  stop
  return
end 
