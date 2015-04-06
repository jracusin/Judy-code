pro calc_ximage_ul,segs,trigtime,nospawn=nospawn

if n_params() eq 0 then begin
    print,'syntax - calc_ximage_ul,segs,trigtime'
return
end 

evfiles=file_search('00*'+ntostr(segs)+'-xrt/sw*pc*po*cl.evt')
exfiles=file_search('00*'+ntostr(segs)+'-xrt/sw*pc*po*ex.img')

print,evfiles
print,exfiles

nfiles=n_elements(evfiles)
expotime=0d
openw,lun,'ximage_ul.xim',/get_lun
printf,lun
for i=0,nfiles-1 do begin
    hdr=headfits(exfiles[i])
    expotime=expotime+sxpar(hdr,'ONTIME')
    printf,lun,'r30 '+evfiles[i]
    printf,lun,'rex '+exfiles[i]
    if i eq 0 then tstart=sxpar(hdr,'TSTART')
    if i gt 0 then begin 
        printf,lun,'sum'
    endif 
    printf,lun,'save'
    if i eq nfiles-1 then tstop=sxpar(hdr,'TSTOP')
endfor 

printf,lun,'cpd /xtk'
printf,lun,'disp'
if exist('src.reg') then printf,lun,'circle/regionfile=src.reg/disp'
printf,lun,'sosta/opt'
printf,lun,'exit'

close,lun
free_lun,lun

if not keyword_set(nospawn) then spawn,'ximage @ximage_ul.xim'

print,'tstart = '+ntostr(tstart-trigtime)
print,'tstop = '+ntostr(tstop-trigtime)
print,'tmean = '+sigfig((tstop-tstart)/2.+tstart-trigtime,10)
print,'expotime = '+ntostr(expotime)

return
end 
