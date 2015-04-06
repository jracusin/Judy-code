pro wrap_fj,grbs

  cd,'~/GRBs'
;  grbs=strtrim(file_search('GRB*'),2)
  ngrbs=n_elements(grbs)
  g=0
  stop
  window,0
  window,1
  for i=g,ngrbs-1 do begin
     print,i,' ',grbs[i]
     find_jetbreak,grbs[i]
  endfor 
return
end 

pro find_jetbreak,grb,type,jb,tbreak,tbreaklim,alpha,alphaerr,mo=mo,go=go,ps=ps,z=z,eiso=eiso

  dir='~/GRBs/'+strtrim(grb,2)+'/'
  cd,dir
  lcfitfile='lc_fit_out_idl_int9.dat'
;  if not exist(lcfitfile) then lcfitfile=dir+'lc_fit_out_idl_int7.dat'
  if exist(lcfitfile) then begin    
     if numlines(lcfitfile) gt 2 then begin 
        read_lcfit,lcfitfile,pnames,p,perr,np=np,nf=nf
;        lcout='lc_newout_phil2.txt'
;        if not exist(lcout) then lcout='lc_newout_phil.txt'
        lc=lcout2fits(dir=dir)
        spec=mrdfits(dir+'UL_specfits.fits',1)
        nspec=n_elements(spec)
        gamma=spec[nspec-1].phind
        gammaerr=spec[nspec-1].phinderr
        
        mo=fit_models(pnames,p,np,nf,basemo=basemo)

        jb=-1
        tbreak=0d
        tbreaklim=-1 ;;; NEED TO CLARIFY LOWER OR UPPER LIM
        type=''

        case np of
           2: begin 
              type='SPL'
              pind=1
           end 
           4: begin
              if p[1] gt p[3] then type='I-II'
              if p[1] lt p[3] then type='II-III'
              pind=[1,3]
           end
           6: begin 
              if p[1] gt p[3] then type='I-II-III'
              if p[1] lt p[3] and p[3] lt p[5] then type='II-III-IV'
              if p[1] lt p[3] and p[3] gt p[5] then type='0-I-II'
              pind=[1,3,5]
           end 
           8: begin 
              if p[1] gt p[3] then type='I-II-III-IV'
              if p[1] lt p[3] then begin 
                 if p[3] gt 2. then type='0-I-II-III' else $
                    type='II-III-IV-V'
              endif 
              pind=[1,3,5,7]
           end 
           10: begin 
              if p[1] lt p[3] then begin
                 if p[3] gt 2. then type='0-I-II-III-IV' else $
                    type='II-III-IV-V-VI'
              endif 
              if p[1] gt p[3] then type='I-II-III-IV-V'
              pind=[3,5,7,9]
           end 
           12: begin
              type='I-II-III-IV-V-VI'
              pind=[3,5,7,9,11]
           end 
        endcase 
        alpha=p[pind]
        alphaerr=perr[*,pind]

  ;;; obvious jet break
        if type eq 'II-III-IV' or type eq 'I-II-III-IV' then begin
           tbreak=p[np-2]
           jb=1
           tbreaklim=0
        endif 

        ;; 2-comp jet with 1 jet break
        if type eq 'I-II-III-IV-V' or type eq 'II-III-IV-V' then begin
           tbreak=p[np-4]
           jb=3 
           tbreaklim=0
        endif 
        
        ;; 2-comp jet with 2 jet breaks
        if type eq 'I-II-III-IV-V-VI' or type eq 'II-III-IV-V-VI' then begin
           tbreak=p[np-2]
           jb=3
           tbreaklim=0
        endif 

  ;;; not so obvious jet breaks (missing plateau or normal decay or jb+energy injection)
        if type eq 'II-III' or type eq 'I-II-III' or type eq '0-I-II-III' then begin
           
           alp=[0.28,1.09,1.89] ;; from Racusin et al. 2009?
           if np eq 8 then begin
              a0=p[5]
              a1=p[7]
              a0err=perr[*,5]
              a1err=perr[*,7]
           endif 
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

           if wm eq 0 then type0=23
           if wm eq 1 then type0=24
           if wm eq 2 then type0=34

           if np eq 8 and type0 eq 24 then begin
              type='0-I-II-IV'
              jb=1
           endif 
           if np eq 8 and type0 eq 34 then begin
              type='0-I-III-IV'
              jb=1
           endif 
           if np eq 6 and type0 eq 24 then begin 
              type='I-II-IV'
              jb=1
           endif 
           if np eq 6 and type0 eq 34 then begin 
              type='I-III-IV'
              jb=1
           endif 
           if np eq 4 and (type0 eq 24 or type0 eq 34) then begin
              jb=1
              if type0 eq 24 then type='II-IV'
              if type0 eq 34 then type='III-IV'
           endif 
           if jb eq 1 then begin ;;; is jet break, not a limit
              tbreak=p[np-2]
              tbreaklim=0
           endif else jb=0 ;; all pre-jb
        endif

        last_alpha=p[np-1]
        if type eq 'SPL' and last_alpha gt 1.5 then begin
           jb=2
           tbreak=min(lc.tstart)
           tbreaklim=2
        endif 

        if jb le 0 then begin 
           tbreak=max(lc.tstop)
           tbreaklim=1
        endif 
        if not keyword_set(go) then begin 
           if not keyword_set(ps) then wset,0
           fit_lc,/justplot;,name=grb
           print
           print,grb
           if jb ge 1 then print,'Jet break = yes ',jb else print,'Jet break = no ',jb
           print,mo,' ',type
           print,np,p
           print,'tbreak = ',tbreak,tbreaklim
           if n_elements(z) ne 0 and n_elements(eiso) then begin
              theta=jet_angle(tbreak,z=z,eiso=eiso)
              print,'jet angle = ',theta
           endif 
           if not keyword_set(ps) then wset,1
           n=n_elements(alpha)
           fit_crs,alpha,alphaerr[0,*],alphaerr[1,*],replicate(gamma,n),replicate(gammaerr[0],n),replicate(gammaerr[1],n),/plotcompat,title=grb
        endif 
     endif 
  endif else print,grb,':  No fit file'

  if not keyword_set(go) and not keyword_set(ps) then begin 
     print,'type to continue'
     k=get_kbrd(10)
     if k eq 's' then stop
  endif 

;  cd,'..'
  return
end 
