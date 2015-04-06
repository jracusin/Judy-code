pro breakup_sed_file
  
;  dir='~/Desktop/GRB080319B/SEDs/ebv0.05nh9/'
  dir='~/Desktop/GRB080319B/SEDs/ebv0.02nh9/'
  cd,dir
;  file='fluxden_ebv05nh09.qdp'
  file='fluxden_evoltn_xrtfixed.qdp'
  readcol,file,lines,format='(a)',delim='$'
  seds=['sed150.txt','sed250.txt','sed350.txt','sed720.txt','sed1500.txt','sed5856.txt','sed1e4.txt','sed3e4.txt','sed8e4.txt','sed2e5.txt','sed5e5.txt']
  
  no=strpos(lines,'no')
  w=where(no eq -1)
  lines=lines[w]
  no=strpos(lines,'=')
  w=where(no eq -1)
  lines=lines[w]
  no=strpos(lines,'starts')
  w=where(no eq -1)
  lines=lines[w]
  no=strpos(lines,'ends')
  w=where(no eq -1)
  lines=lines[w]
  no=strpos(lines,'on')
  w=where(no eq -1)
  lines=lines[w]
  no=strpos(lines,'MODEL')
  w=where(no eq -1)
  lines=lines[w]
  w=where(strtrim(lines,2) eq '!DATA')
  help,w
  n=n_elements(lines)
  w=[w,n]
  for i=0,10 do begin
     outfile=seds[i]
     writecol,outfile,lines[w[i]+1:w[i+1]-1]
  endfor 
  
  stop
  return
end 
