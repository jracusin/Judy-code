pro get_centroid,imfile,detfile,outfile
  
  readcol,detfile,id,cts,x,y,vig,rah,ram,ras,decd,decm,decs,err,tmp1,tmp2,tmp3,format='(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a)',comment='!'
  
  openw,lun,outfile,/get_lun
  openw,rlun,outfile+'.reg',/get_lun
  printf,rlun,'global color=green font="helvetica 10 normal" select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  n=n_elements(x)
  for i=0,n-1 do begin 
     ra=rah[i]+' '+ram[i]+' '+ras[i]
     dec=ntostr(decd[i]*1)+' '+decm[i]+' '+decs[i]
     com='xrtcentroid infile='+imfile+' outfile=centroid'+ntostr(i)+' outdir="./" clobber=yes interactive=no boxra="'+ra+'" boxdec="'+dec+'" boxradius=0.6 calcpos=yes'
     print,com
     ;spawn,com
  endfor 
  k=get_kbrd(10)
  for i=0,n-1 do begin
     if exist('centroid'+ntostr(i)) then begin 
        readcol,'centroid'+ntostr(i),txt1,txt2,format='(a,a)',delim='='
    
        ra=txt2[0]
        dec=txt2[1]
        x=strtrim(txt2[6],2)
        y=strtrim(txt2[7],2)
;        err=txt2[8]
        
        ra=rah[i]+':'+ram[i]+':'+ras[i]
        dec=ntostr(decd[i]*1)+':'+decm[i]+':'+decs[i]
        printf,lun,ntostr(i)+','+ra+','+dec+','+x+','+y;+','+err
;        printf,rlun,'physical;circle('+x+','+y+',20)'
        printf,rlun,'fk5;circle('+ra+','+dec+',47.1462")'
     endif 
  endfor 
  close,lun
  close,rlun
  free_lun,lun
  free_lun,rlun
  return
end 
  
