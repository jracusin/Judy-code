pro read_htmltable,htmlfile,c,csvfile=csvfile

  if not exist(htmlfile) then begin
     print,'HTML FILE DOES NOT EXIST'
     return
  end 

  readcol,htmlfile,lines,format='(a)',delim='|'

  w=where(strpos(lines,'<th class=sortable-text>') ne -1,nw)
;  if nw eq 0 then w=where(strpos(lines,'<td><A TARGET=_blank') ne -1)
  columns=strsplit(lines[w],'<>',/ext)
  ncol=n_elements(columns)
  col=strarr(ncol)
  for i=0,ncol-1 do col[i]=columns[i,1]

  rows=''
  for i=0,ncol-2 do rows=rows+"'col"+ntostr(i+1)+"','',"
  rows=rows+"'col"+ntostr(i+1)+"',''"
;stop
  tmp=execute('c=create_struct('+rows+')')
  
  w=where(lines eq '<tr>',nentries)

  c=replicate(c,nentries)
  for j=0,nentries-1 do begin 
     for i=0,ncol-1 do begin
        x=strsplit(lines[w[j]+i+1],'<>',/ext)
        c[j].(i)=x[n_elements(x)-2]
        if c[j].(i) eq 'td' then c[j].(i)=''
     endfor 
  endfor 

  if n_elements(csvfile) gt 0 then begin
     for i=0,ncol-1 do c.(i)='"'+c.(i)+'"'
     st=strjoin('c.col'+ntostr(indgen(ncol-1)+1)+',')
     st=st+'c.col'+ntostr(ncol)
     com='writecol,"'+csvfile+'",'+st+",delim=','"
     print,com
     temp=execute(com)

  endif 

return
end
