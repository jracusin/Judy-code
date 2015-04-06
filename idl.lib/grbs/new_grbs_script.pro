pro new_grbs_script,getbat=getbat,mydir=mydir,skip=skip
  
;  csvfile='old_grbs.csv'
;  csvfile='short_grbs.csv'
;  outcsvfile='short_grbs2.csv'
;  csvfile='new_grbs.csv'
  csvfile='~/jetbreaks/new_grbs_08_09.csv'
  outcsvfile='~/jetbreaks/new_grbs_08_09_trig.csv'
  

;  readcol,csvfile,grbname,tid,battime,wt,format='(a,l,d,i)',delim=','
  readcol,csvfile,grbname,tid,format='(a,a)',delim=',',skip=1 
  cd,!adata

  ngrbs=n_elements(grbname)
  g=0
  stop
;  if keyword_set(getbat) then begin 
     battime=strarr(ngrbs)
     wt=intarr(ngrbs)
     for i=g,ngrbs-1 do begin
        grb='GRB'+grbname[i]
        if not exist(grb) then spawn,'mkdir '+grb
        if tid[i] gt 100000L then begin 
           com='wget -O BATfile.txt --http-user=SwiftBA --http-passwd=BATgrb "http://swift.gsfc.nasa.gov/docs/swift/results/BATbursts/'+ntostr(tid[i])+'/bascript/top.html"'
           print,com
           spawn,com
           com2='grep "Trigger Time" BATfile.txt > BATtime.txt'
           print,com2
           spawn,com2
           readcol,'BATtime.txt',crap1,crap2,crap3,bt,delim=' ',format='(a,a,a,a)'
           battime[i]=bt
        endif
        print,'DOWNLOAD'
        download_phil_lc,grb,tid=tid[i]
        if exist(grb+'/WTCURVE.qdp') then wt[i]=1
     endfor 

     writecol,outcsvfile,grbname,tid,battime,wt,delim=','
     
;     stop
;  endif 

     

;  for i=g,ngrbs-1 do begin 
;     grb_lc_script,grbname[i],tid[i],battime[i],wt[i],mydir=mydir,skip=skip
;  endfor 

  return
end 
