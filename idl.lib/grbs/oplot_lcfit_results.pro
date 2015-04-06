@fit_functions
pro oplot_lcfit_results,lc,newp,perror,chisq,dof,breaks,leg,pnames,charsize=charsize,noleg=noleg,noerr=noerr,name=name
  
  if not keyword_set(noerr) then noerr=0
  time=lc.time
  cts=lc.src_rate
  err=lc.src_rate_err
  
  norm=newp[0]
  normerr=perror[*,0]
  pow1=newp[1]
  pow1err=perror[*,1]
  case breaks of
     0: begin 
        t=time
        y=pow(t,newp)
        pnames='Pow1'
        leg='Pow1'+' = '+sigfig(pow1,3)
        if not noerr then leg=leg+' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3)
     end
     1: begin 
        pow2=newp[3]
        pow2err=perror[*,3]
        break1=newp[2]
        break1err=perror[*,2]
        w1=where(time gt 0 and time lt break1,nw1)
        w2=where(time ge break1)
        t=[time[w1],break1,time[w2]]
        y=bknpow(t,newp)
        pnames=['Pow1','Breaktime','Pow2']
        leg=['Pow1'+' = '+sigfig(pow1,3), $
             'Breaktime = '+sigfig(break1,5),$
             'Pow2'+' = '+sigfig(pow2,3)]
        
        if not noerr then $
           leg=leg+[' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
                    ' !S!E+'+sigfig(break1err[1],5)+' !R!I-'+sigfig(break1err[0],5),$
                    ' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3)]
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
     end
     2: begin 
        pow2=newp[3]
        pow2err=perror[*,3]
        pow3=newp[5]
        pow3err=perror[*,5]
        break1=newp[2]
        break1err=perror[*,2]
        break2=newp[4]
        break2err=perror[*,4]
        
        w1=where(time gt 0 and time lt break1,nw1)
        w2=where(time ge break1 and time lt break2,nw2)
        w3=where(time ge break2)
        t=[time[w1],break1,time[w2],break2,time[w3]]
        y=bkn2pow(t,newp)
        pnames=['Pow1','Breaktime1','Pow2','Breaktime2','Pow3']
        leg=['Pow1'+' = '+sigfig(pow1,3), $
             'Breaktime = '+sigfig(break1,5),$
             'Pow2'+' = '+sigfig(pow2,3),$
             'Breaktime2 = '+sigfig(break2,5),$
             'Pow3'+' = '+sigfig(pow3,3)]
        if not noerr then $
           leg=leg+[' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
                    ' !S!E+'+sigfig(break1err[1],5)+' !R!I-'+sigfig(break1err[0],5),$
                    ' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3),$
                    ' !S!E+'+sigfig(break2err[1],5)+' !R!I-'+sigfig(break2err[0],5),$
                    ' !S!E+'+sigfig(pow3err[1],3)+' !R!I-'+sigfig(pow3err[0],3) ]
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
        oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2
        
     end
     3: begin 
        pow2=newp[3]
        pow2err=perror[*,3]
        pow3=newp[5]
        pow3err=perror[*,5]
        pow4=newp[7]
        pow4err=perror[*,7]
        break1=newp[2]
        break1err=perror[*,2]
        break2=newp[4]
        break2err=perror[*,4]
        break3=newp[6]
        break3err=perror[*,6]

        w1=where(time ge 0 and time lt break1,nw1)
        w2=where(time ge break1 and time lt break2,nw2)
        w3=where(time ge break2 and time lt break3,nw3)
        w4=where(time ge break3,nw4)
        t=[time[w1],break1,time[w2],break2,time[w3],break3,time[w4]]
        y=bkn3pow(t,newp)
        pnames=['Pow1','Breaktime1','Pow2','Breaktime2','Pow3','Breaktime3','Pow4']
        leg=['Pow1'+' = '+sigfig(pow1,3), $
             'Breaktime = '+sigfig(break1,5),$
             'Pow2'+' = '+sigfig(pow2,3),$
             'Breaktime2 = '+sigfig(break2,5),$
             'Pow3'+' = '+sigfig(pow3,3),$
             'Breaktime3 = '+sigfig(break3,5),$
             'Pow4'+' = '+sigfig(pow4,3)]
        if not noerr then $
           leg=leg+[' !S!E+'+sigfig(pow1err[1],3)+' !R!I-'+sigfig(pow1err[0],3),$
                    ' !S!E+'+sigfig(break1err[1],5)+' !R!I-'+sigfig(break1err[0],5),$
                    ' !S!E+'+sigfig(pow2err[1],3)+' !R!I-'+sigfig(pow2err[0],3),$
                    ' !S!E+'+sigfig(break2err[1],5)+' !R!I-'+sigfig(break2err[0],5),$
                    ' !S!E+'+sigfig(pow3err[1],3)+' !R!I-'+sigfig(pow3err[0],3),$
                    ' !S!E+'+sigfig(break3err[1],5)+' !R!I-'+sigfig(break3err[0],5),$
                    ' !S!E+'+sigfig(pow4err[1],3)+' !R!I-'+sigfig(pow4err[0],3)]
        oplot,[break1,break1],[1e-8,1e5],color=!yellow,line=2
        oplot,[break2,break2],[1e-8,1e5],color=!yellow,line=2
        oplot,[break3,break3],[1e-8,1e5],color=!yellow,line=2

        
     end
  endcase 
  
  if norm gt 100 then sci=1 else sci=0
  normleg='Norm = '+sigfig(norm,3,sci=sci)
  if not noerr then $
     normleg=normleg+' !S!E+'+sigfig(normerr[1],3,sci=sci)+' !R!I-'+sigfig(normerr[0],3,sci=sci)
  
  leg=[leg,$
       normleg,$
       !vsym.chi+'!U2!N/dof = '+sigfig(chisq/dof,4),$
       'dof = '+ntostr(fix(dof))]

  oplot,t,y,color=!green,thick=1
  if not keyword_set(noleg) then legend,leg,box=0,/top,/right,charsize=2


  return
end 
