;;; $Id: $
;;; Patrick Broos, 2008

;;; Tool to visualize differences between the DATA (not keywords) in two event lists.
;;; NOTE that the CIAO tool dmdiff is also very useful for comparing files!!!!
;;; This tool, however, can compare the STATUS column of ACIS files.

PRO compare_event_lists, eventlist1_fn, eventlist2_fn

old = mrdfits(eventlist1_fn, 1)
new = mrdfits(eventlist2_fn, 1)
num_events = n_elements(new)
if (n_elements(old) NE num_events) then begin
  print, 'ERROR!  The number of events has changed!!!'
  retall
endif

old_status = swap_endian(ulong(old.STATUS,0,num_events), /SWAP_IF_LITTLE_ENDIAN)
new_status = swap_endian(ulong(new.STATUS,0,num_events), /SWAP_IF_LITTLE_ENDIAN)
tag_names = tag_names(old)
plot_names = ['TIME',     'X',     'Y','PHA','ENERGY',     'PI','FLTGRADE','GRADE']
plot_units = [   's','skypix','skypix', 'DN',    'eV','channel',    'none', 'none']
name=strarr(32)
; Taken from "ACIS Event Data STATUS Bits memo, revision 2.2".
; Bits 16-19 and 31 descriptions revised for afterglow workaround used in our L1->L2 recipe..
name[0]  = 'invalid CHIP coordinates'
name[1]  = 'invalid PHAS'
name[2]  = 'PHAS overflow'
name[3]  = 'PHA too high'
name[4]  = '"bad pixel"'
name[5]  = 'island touches "bad pixel"'
name[6]  = 'bias=4095'
name[7]  = 'bias missing'
name[8]  = 'bias parity error'
name[9]  = 'overclock unknown'
name[10] = 'overclock out of range'
name[11] = 'corner pixels too small'
name[14] = 'corner pixels "bad"'
name[15] = 'flag from destreak tool'
name[16] = 'afterglow flag from acis_detect_afterglow'
name[17] = 'afterglow info from acis_detect_afterglow'
name[18] = 'afterglow info from acis_detect_afterglow'
name[19] = 'afterglow info from acis_detect_afterglow'
name[20] = 'CTI algorithm did not converge'
name[21] = 'flag from acisreadcorr tool'
name[22] = 'flag from acisreadcorr tool'
name[23] = 'flag from Clean55 algorithm'
name[31] = 'afterglow flag from acis_run_hotpix'
for ii=0,n_elements(plot_names)-1 do begin
  ind = where(tag_names EQ plot_names[ii], count)
  if (count EQ 0) then continue
  id=0L
  dataset_2d,id, new.(ind[0]), new.(ind[0])-old.(ind[0]),XTIT='new '+plot_names[ii]+' ('+plot_units[ii]+')', YTIT='new-old', TITLE=' ', PSYM=1   
endfor
print, '------number of events------   bit#  description'
print, '    1->0     0->1  (now set)'
for ii=0,31 do begin
  mask = ishft('1'XUL,ii)
  old_bit  = long(ishft(mask AND old_status, -ii))
  new_bit  = long(ishft(mask AND new_status, -ii))
  diff_bit = new_bit - old_bit  
  print, total(/INT, diff_bit EQ -1), total(/INT, diff_bit EQ 1), total(/INT, new_bit), ii, name[ii], F='(%"%8d %8d (%8d)  bit%d (%s)")'
endfor

return
end


