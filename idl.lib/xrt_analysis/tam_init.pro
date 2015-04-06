; $Id: tam_init.pro 1.0 2002/01/18 17:25:30 burrows $
; $Log: tam_init.pro $
;

frame_count = fltarr(20000)
tamx1 = fltarr(20000)
tamy1 = fltarr(20000)
tamx2 = fltarr(20000)
tamy2 = fltarr(20000)
tamsig1 = fltarr(20000)
tamsig2 = fltarr(20000)
tamtime = dblarr(20000)
targetid = lonarr(20000)
obsseg = intarr(20000)
calcx1 = tamx1
calcy1 = tamy1
calcx2 = tamx2
calcy2 = tamy2

iframe = - 1

page_count = 0UL
old_seq = -1
num_CCSDS_packets_read = 0UL

tam_frame = 0

ldp_num_old = -1

