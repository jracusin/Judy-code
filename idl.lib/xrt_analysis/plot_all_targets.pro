pro plot_all_targets,outstr,update=update,targids=targids
  
;  targids=[50000,50001,50002,50003,50004,50005,50006,50007,50008,50009,50010,50011,50012,50013,50014,50015,50016,50017,50018,50019,50020,50050,50100,50101,50102,50103,50150,50200,50203,50250,50300,50302,50303,50400,50403,50404,50416,50500,50550,50650,50651,50652,50653,50654,50655,50656,50657,50658,50700,50701,50702,50750,50751,50752,50800,50801,50802,50803,50804,50805,50806,50807,50808,50850,50900,50950,51000,51050,51200,51300,51301,51302,51303,51304,51305,51306,51307,51308,51650,51750,51751,51752,51753,51754,51755,51756,51757,51758,51900,51950,52100,52150,52200,52250,52300,53000,53001,53003,53006,53008,53050,53051,53053,53056,53058,53200,53201,53203,53206,53208,53300,53301,53303,53306,53308,54000,54001,54002,54003,54004,54050,54100,54150,54200,54250,54251,54252,54253,54254,54300,54301,54302,54303,54304,54350,54400,54450,54500,54550,54600,54650,54700,54750,54800,54850,54900,54950,55000,55050,55051,55052,55053,55054,55100,55150,55200,55201,55202,55203,55204,55250,55300,55350,55400,55450,55500,55501,55502,55503,55504,55550,55600,55601,55602,55603,55604,55650,55700,55750,55800,55850,55851,55852,55853,55854,55900,55950,56000,56050,56100,56150,56200,56201,56202,56203,56204,56250,56350,56400,56401,56402,56403,56404,56450,56451,56452,56453,56454,58000,58001,58003,58006,58007,58008,58010,58026,58035,58036,58037,58039,58040,58041,58042,58043,58052,58053,58054,58055,58059,58060,58061,58062,58063,58064,58065,58066,58068,58069,58070,58071,58072,58073,58074,58075,58077,58079,58080,58081,58082,58083,58093,58094,58228,58230,58231,58232,58254,58307,58308,58309,58310,58311,58312,58313,58314,58315,58316,58318,58319,58320,58321,58322,58323,58325,58326,58327,58329,58331,58333,58334,58409,58410,58411,58412,58415,58416,58418,58419,58422,58423,58424,58425,58426,58429,58430,58431,58432,58433,58451,58458,58465,58466,58468,58469,58472,58473,58474,58475,58476,58479,58480,58481,58482,58483,58600]
  
  openr,lun,'/home/jracusin/calibration/caldat.txt',/get_lun
  line=''
  targid=0L
  name=''
  while not eof(lun) do begin 
     line=readline(lun,delim=',')
     targid=[targid,line[0]]
     name=[name,line[1]]
  endwhile
  targids=targid[1:*]
  names=name[1:*]
  
  if n_elements(outstr) eq 0 or keyword_set(update) then $
     collect_target_obs,targids,outstr,filedir='~/',instr=outstr
  
  tid=outstr[rem_dup(outstr.targetid)].targetid
  
  ntargs=n_elements(tid)
  psym=[1,2,4,5,6,7,1]
  simpctable
  colors=[!red,!blue,!green,!magenta,!cyan,!orange,!purple,!pink]
  begplot,/color,name='/home/jracusin/calibration/calplots.ps',/landscape

  mode=strtrim(outstr.mode,2)
  !p.multi=[0,2,2]
  for i=0L,ntargs-1 do begin
     
     w=where(outstr.targetid eq tid[i],nw)
     j=where(targids eq tid[i])
;     if nw gt 1 then begin 
        modes=mode[w]
        modes=modes[rem_dup(modes)]
        nmodes=n_elements(modes)
;        dt=outstr[w].date[1]+outstr[w].date[2]/24.+outstr[w].date[3]/24./60.+outstr[w].date[3]/24./3600;+(outstr[w].date[0]-2004.)*366D
;        dt=outstr[w].date[0]+dt/1000.
        
        yrs=outstr[w[rem_dup(outstr[w].date[0])]].date[0]
        nyrs=n_elements(yrs)
        tjd=0D
        for y=0,nyrs-1 do begin
           wyr=where(outstr[w].date[0] eq yrs[y])
           wyr=w[wyr]
           ydn2md,yrs[y],outstr[wyr].date[1],m,d
           hr=outstr[wyr].date[2]+outstr[wyr].date[3]/60.+outstr[wyr].date[4]/3600.
           jdcnv,yrs[y],m,d,hr,jd
           tjd=[tjd,jd]
        endfor 
        jd=tjd[1:*]
        mjd = jd-2400000.5D 
        ;;mission day
;        jdcnv,2004,11,20,0.,jdm
        
        dt=mjd                  ;-jdm
        
        if (max(dt)-min(dt)) le 2 then xform='(f10.2)' else xform='(i10)'
        
;        ldps=outstr[w[rem_dup(outstr[w].ldp)]].ldp
;        nldp=n_elements(ldps)
;        ffr=intarr(nldp)
;        for ld=0,nldp-1 do begin
;           wld=where(outstr[w].ldp eq ldps[ld],nwld)
;           tmp=min(outstr[w[wld]].frame,mffr)
;           ffr[ld]=wld[mffr]
;        endfor 
        
                                ;get total obs time
        q=where(((mode[w] eq 'PC' or mode[w] eq 'WT' or mode[w] eq 'LRPD' or mode[w] eq 'PUPD') and (outstr[w].deltat lt 30.) AND (outstr[w].deltat GE 0.)) or mode[w] eq 'IM' or mode[w] eq 'BIAS',nq) 
        IF nq GT 0 THEN BEGIN 
            obstime=total(outstr[w[q]].deltat > 0.)*1D-3
            obstimes=ntostr(obstime,(round(alog10(obstime)+0.5)>1.)+3)
        
            fq=where(outstr[w[q]].ccdt le -50. AND outstr[w[q]].settled eq 1,nfq)
            IF nfq GT 0 THEN BEGIN 
                filtime=total(outstr[w[q[fq]]].deltat > 0.)*1D-3
                filtimes=ntostr(filtime,(round(alog10(filtime)+0.5)>1.)+3)
            ENDIF ELSE filtimes='0 '
        ENDIF ELSE BEGIN
            filtimes='0.0 '
            obstimes='0.0 '
        ENDELSE 

        if nw eq 1 then begin
           xrange=[dt-0.5,dt+0.5] 
           yrange=[outstr[w].ccdt-0.5,outstr[w].ccdt+0.5]
           xform='(f10.1)'
        endif else begin
           xrange=[min(dt),max(dt)]
           yrange=[min(outstr[w].ccdt)-0.05,max(outstr[w].ccdt)+0.05]
        endelse 
        
        plot,xrange,yrange,/nodata,title='Target: '+ntostr(tid[i])+'   '+names[j],xtitle='MJD',ytitle='CCD Temp',charsize=1.,ystyle=1,xtickformat=xform

        ns=intarr(nmodes)
        for m=0,nmodes-1 do begin
           stmode=strtrim(modes[m],2)
           if stmode eq 'BIAS' then n=0
           if stmode eq 'PC' then n=1
           if stmode eq 'WT' then n=2
           if stmode eq 'IM' then n=3
           if stmode eq 'LRPD' then n=4
           if stmode eq 'PUPD' then n=5
           if stmode eq 'RAW' then n=6
           wm=where(mode[w] eq modes[m],nwm)
           colors=[!red,!blue,!green,!magenta,!cyan,!orange,!purple,!pink]
           if nwm gt 1 then oplot,dt[wm],outstr[w[wm]].ccdt,psym=1,color=colors[n],symsize=0.3 else plots,dt[wm],outstr[w[wm]].ccdt,psym=1,color=colors[n],symsize=0.3
           
           ns[m]=n

        endfor 
        colors=[!red,!blue,!green,!magenta,!cyan,!orange,!purple,!pink]
        legend,modes,box=0,charsize=1,color=colors[ns],psym=1,/top,/left
        legend,['t!Ltotal!N = '+obstimes+' ks','','t!Lfilt!N = '+filtimes+ ' ks'],/top,/right,box=0,charsize=1
        
        
  endfor 
  !p.multi=0
  endplot
  
  return
end 
