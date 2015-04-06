;
;  Little program to read a target database csv file and print out ra and dec
;	for use by map_viewer
;

pro csv2radec

filename = pickfile(filter="*.csv")

luin=20
luout=21

openr,luin,filename
openw,luout,'coordinates.pts'
printf,luout,'celestial'
a=' '
readf,luin,a	; read header

while (not(eof(luin))) do begin
	readf,luin,a
	data = strsplit(a,',',/extract)
	printf,luout,data(5),'     ', data(6)
endwhile

close,/all

end
