pro which_closure_relation,a,alow,aup,g,blow,bup,cr,ps=ps
  
  b=g-1.
  
  cr=''
;;;ISM, slow cooling
  p=[0,2.*b+1.,2.*b+1.,2.*b,2.*b]
;  if blow lt -1./3 and bup gt -1./3. then cr=[cr,'ISMs1a']
;  pup=2.*bup+1.
;  plow=2.*blow+1.
;  if plow lt 2. and pup gt 2. then cr=[cr,'ISMs2a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'ISMs2b']
;  pup=2.*bup
;  plow=2.*blow
;  if plow lt 2. and pup gt 2. then cr=[cr,'ISMs3a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'ISMs3b']
  
;;;ISM, fast cooling
  p=[p,0,0,b/2.,b/2.]
;  if blow lt -1./3 and bup gt -1./3. then cr=[cr,'ISMf1a']
;  if blow lt 1./2 and bup gt 1./2. then cr=[cr,'ISMf2a']
;  pup=bup/2.
;  plow=blow/2.
;  if plow lt 2. and pup gt 2. then cr=[cr,'ISMf3a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'ISMf3b']
  
;;;WIND, slow cooling
  p=[p,0,2.*b+1.,2.*b+1.,2.*b,2.*b]
;  if blow lt -1./3 and bup gt -1./3. then cr=[cr,'WINDs1a']
;  pup=2.*bup+1.
;  plow=2.*blow+1.
;  if plow lt 2. and pup gt 2. then cr=[cr,'WINDs2a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'WINDs2b']
;  pup=2.*bup
;  plow=2.*blow
;  if plow lt 2. and pup gt 2. then cr=[cr,'WINDs3a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'WINDs3b']
  
;;WIND, fast cooling
  p=[p,0,0,b/2.,b/2.]
;  if blow lt -1./3 and bup gt -1./3. then cr=[cr,'WINDf1a']
;  if blow lt 1./2 and bup gt 1./2. then cr=[cr,'WINDf2a']
;  pup=bup/2.
;  plow=blow/2.
;  if plow lt 2. and pup gt 2. then cr=[cr,'WINDf3a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'WINDf3b']
  
;;;JET, slow cooling
  p=[p,0,2.*b+1.,2.*b+1.,2.*b,2.*b]
;  if blow lt -1./3 and bup gt -1./3. then cr=[cr,'JETs1a']
;  pup=2.*bup+1.
;  plow=2.*blow+1.
;  if plow lt 2. and pup gt 2. then cr=[cr,'JETs2a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'JETs2b']
;  pup=2.*bup
;  plow=2.*blow
;  if plow lt 2. and pup gt 2. then cr=[cr,'JETs3a']
;  if (plow lt 1 and pup lt 2) or (plow lt 2 and pup gt 2) or (plow lt 1 and pup gt 2) then cr=[cr,'JETs3b']
  
;  if n_elements(cr) gt 1 then begin 
;     fitcr=cr[1:*]
  
;     match,fitcr,cr,m1,m2
  cr=['ISMs1a','ISMs2a','ISMs2b','ISMs3a','ISMs3b','ISMf1a','ISMf2a','ISMf3a','ISMf3b','WINDs1a','WINDs2a','WINDs2b','WINDs3a','WINDs3b','WINDf1a','WINDf2a','WINDf3a','WINDf3b','JETs1a','JETs2a','JETs2b','JETs3a','JETs3b']
  altcr=['','Sa','','Sc','','','','','','','ja','','jc','','','','','','','Ja','','Jc','']
  nureg=[1,2,2,3,3,1,2,3,3,1,2,2,3,3,1,2,3,3,1,2,2,3,3]
     ;;;choosing which apply to xray
  mult=[0,1,2,1,2,0,0,1,2,0,1,2,1,2,0,0,1,2,0,1,2,1,2]
  color=[!blue,!red,!green,0]
;     good=[3,4,6,12,13,15,21,22]
  good=[1,2,3,4,6,7,8,10,11,12,13,15,16,17,19,20,21,22]
  cr=cr[good]
  nureg=nureg[good]
  mult=mult[good]
  p=p[good]
  altcr=altcr[good]
;  stop
  
  w=where(p lt 1. or p gt 2 and mult eq 2,nw)
  if nw gt 0 then nureg[w]=4
  w=where(p lt 2 and mult eq 1,nw)
  if nw gt 0 then nureg[w]=4
  
  w=where(nureg ne 4)
  cr=cr[w]
  nureg=nureg[w]
  mult=mult[w]
  p=p[w]
  altcr=altcr[w]
  
;     cr=cr[1:*]
  n=n_elements(cr)
  crval=dblarr(n)
  creq=strarr(n)
  errvalup=dblarr(n)
  errvallow=errvalup
  dcrdb=dblarr(n)
  if keyword_set(ps) then sym=!tsym else sym=!vsym
  alpha=sym.alpha
  beta=sym.beta
  
;     !p.multi=[0,5,5]
;     !p.multi=[0,2,n/2]
  plot,[-4,4],[0,n+1],/nodata,xtitle='closure relation',/xsty
  for i=0,n-1 do begin 
     case cr[i] of 
        'ISMs1a': begin
           crval[i]=a-3.*b/2.
           creq[i]=alpha+'=3'+beta+'/2'
           dcrdb[i]=-3./2.
        end 
        'ISMs2a': begin
           crval[i]=a-3.*b/2.
           creq[i]=alpha+'=3'+beta+'/2'
           dcrdb[i]=-3./2.
        end 
        'ISMs2b': begin
           crval[i]=a-3.*(2.*b+3.)/16.
           creq[i]=alpha+'=3(2B+3)/16'              
           dcrdb[i]=-3.*2./16.
        end 
        'ISMs3a': begin
           crval[i]=a-(3.*b-1)/2.
           creq[i]=alpha+'=(3B-1)/2'
           dcrdb[i]=-3./2.
        end 
        'ISMs3b': begin
           crval[i]=a-(3.*b+5)/8.
           creq[i]=alpha+'=(3B+5)/8'              
           dcrdb[i]=-3./8.
        end 
        'ISMf1a': begin
           crval[i]=a-b/2.
           creq[i]=alpha+'='+beta+'/2'
           dcrdb[i]=-1./2.
        end 
        'ISMf2a': begin
           crval[i]=a-b/2.
           creq[i]=alpha+'='+beta+'/2'
           dcrdb[i]=-1./2
        end 
        'ISMf3a': begin
           crval[i]=a-(3.*b-1)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'              
           dcrdb[i]=-3./2.
        end 
        'ISMf3b': begin
           crval[i]=a-(3.*b+5)/8.
           creq[i]=alpha+'=(3'+beta+'+5)/8'              
           dcrdb[i]=-3./8.
        end 
        'WINDs1a': begin
           crval[i]=a-(3.*b+1.)/2.
           creq[i]=alpha+'=(3'+beta+'+1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDs2a': begin
           crval[i]=a-(3.*b+1.)/2.
           creq[i]=alpha+'=(3'+beta+'+1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDs2b': begin
           crval[i]=a-(2.*b+9.)/8.
           creq[i]=alpha+'=(2'+beta+'+9)/8'
           dcrdb[i]=-2./8.
        end
        'WINDs3a': begin
           crval[i]=a-(3.*b-1.)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDs3b': begin
           crval[i]=a-(b+3.)/4.
           creq[i]=alpha+'=('+beta+'+3)/4'
           dcrdb[i]=-1./4.
        end
        'WINDf1a': begin
           crval[i]=a-(-b+1.)/2.
           creq[i]=alpha+'=(-'+beta+'+1)/2'
           dcrdb[i]=1./2.
        end
        'WINDf2a': begin
           crval[i]=a-(-b+1.)/2.
           creq[i]=alpha+'=(-'+beta+'+1)/2'
           dcrdb[i]=1./2.
        end
        'WINDf3a': begin
           crval[i]=a-(3.*b-1.)/2.
           creq[i]=alpha+'=(3'+beta+'-1)/2'
           dcrdb[i]=-3./2.
        end
        'WINDf3b': begin
           crval[i]=a-(b+3.)/4.
           creq[i]=alpha+'=('+beta+'+3)/4'
           dcrdb[i]=-1./4.
        end
        'JETs1a': begin
           crval[i]=a-(2.*b+1.)
           creq[i]=alpha+'=2'+beta+'+1'
           dcrdb[i]=-2.
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
        else:
     endcase
     errvalup[i]=sqrt((aup-a)^2.+(bup-b)^2*dcrdb[i]^2.)
     errvallow[i]=sqrt((a-alow)^2.+(b-blow)^2*dcrdb[i]^2.)

     plots,crval[i],n-i+1,psym=2,color=color[nureg[i]-1]
     oplot,[crval[i]-errvallow[i],crval[i]+errvalup[i]],[n-i+1,n-i+1],line=0,color=color[nureg[i]-1]
     xyouts,-3.8,n-i+1,cr[i]+' '+altcr[i]+'  '+creq[i]+' ',color=color[nureg[i]-1],charsize=1.5
     
  endfor 

  
  oplot,[0,0],[0,n+2],line=2
;  for i=0,n_elements(m1)-1 do $
;     oplot,[crval[m1[i]]-errvallow[m1[i]],crval[m1[i]]+errvalup[m1[i]]],[m1[i]+1,m1[i]+1],line=0,color=!green
;  oplot,crval[m1],m1+1,psym=2,color=!green
;  !p.multi=0
;  endif else print,'No closure relations fit'
  
  stop
  return
end 
  
pro closure_relations
  
  ;;;plot cases for each burst
  
  ;;use gamma to get beta
  ;;use beta to get nu
  ;;for nu with p options, use beta to get p
  ;;then have closure relation for each case (ISM, WIND, JET, fast, slow, etc)
  
  

  
  
  return
end 
