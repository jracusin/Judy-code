@./parse_str.pro
@./get_lines.pro
pro corr_ahk_tam

;f_ahk = xrt_something.0.stripchart_table.fits
;f_tam = xrt_something_tam_two_win.0.tam_centroids

f_ahk = pickfile(title='Select XRT HK file: ', $
	filter='*stripchart_table*',/must_exist)

f_tam = pickfile(title='Select XRT tam centroid file: ', $
	filter='*centroid*',/must_exist)

data_ahk=mrdfits(f_ahk,1,ahk_head)
nl_ahk=n_elements(data_ahk)

openr,lun_tamcent,f_tam,/get_lun
nl_tamcent=get_lines(f_tam)
data_tam=dblarr(nl_tamcent,11)

for n=0,nl_tamcent-1 do begin
	line=parse_str(lun_tamcent)
	data_tam(n,*)=line
endfor

data_tam_rebin=dblarr(nl_ahk,11)
for n=0,10 do data_tam_rebin(*,n)=interpol(data_tam(*,n),data_tam(*,4),data_ahk.sctime)

w=where((data_ahk.sctime gt min(data_tam[*,4])) and (data_ahk.sctime lt max(data_tam[*,4])))

data_tam_rebin=data_tam_rebin[w,*]

;get filename
filein = f_tam
path_pos = strpos(filein, '/', /reverse_search)
filein = strmid(filein,path_pos+1)

;get filebase
path_pos = strpos(filein, '.', /reverse_search)
filebase = strmid(filein,0,path_pos+1)
print,'Writing out plot: '+filebase+'corr_ahk_tam.ps'

set_plot,'ps'
device,filename=filebase+'corr_ahk_tam.ps',/landscape
m_arr=[5,6,8,9]
!x.title='temp'
ytit=['frame1_xpos (pix)','frame1_ypos (pix)','frame2_xpos (pix)','frame2_ypos (pix)']
for m=0,3 do begin
	!y.title=ytit(m)
	m_ct=m_arr(m)
	!p.multi=[0,4,5]
	!y.range=[min(data_tam_rebin(where(data_tam_rebin(*,m_ct) ne 0),m_ct)),max(data_tam_rebin(where(data_tam_rebin(*,m_ct) ne 0),m_ct))]
	plot,data_ahk.FWDTUBET_15_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT15 corr='+string(correlate(data_ahk.FWDTUBET_15_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_16_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT16 corr='+string(correlate(data_ahk.FWDTUBET_16_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_2_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT2 corr='+string(correlate(data_ahk.FWDTUBET_2_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_1_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT1 corr='+string(correlate(data_ahk.FWDTUBET_1_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_13_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT13 corr='+string(correlate(data_ahk.FWDTUBET_13_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_14_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT14 corr='+string(correlate(data_ahk.FWDTUBET_14_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_4_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT4 corr='+string(correlate(data_ahk.FWDTUBET_4_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_3_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT3 corr='+string(correlate(data_ahk.FWDTUBET_3_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_11_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT11 corr='+string(correlate(data_ahk.FWDTUBET_11_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_12_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT12 corr='+string(correlate(data_ahk.FWDTUBET_12_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_6_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT6 corr='+string(correlate(data_ahk.FWDTUBET_6_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_5_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT5 corr='+string(correlate(data_ahk.FWDTUBET_5_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_9_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT9 corr='+string(correlate(data_ahk.FWDTUBET_9_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_10_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT10 corr='+string(correlate(data_ahk.FWDTUBET_10_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_8_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT8 corr='+string(correlate(data_ahk.FWDTUBET_8_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.FWDTUBET_7_RTD_,data_tam_rebin(*,m_ct),psym=3,title='FT7 corr='+string(correlate(data_ahk.FWDTUBET_7_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.XRTOBIFTEMP_3_RTD_,data_tam_rebin(*,m_ct),psym=3,title='OBIF3 corr='+string(correlate(data_ahk.XRTOBIFTEMP_3_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.XRTOBIFTEMP_4_RTD_,data_tam_rebin(*,m_ct),psym=3,title='OBIF4 corr='+string(correlate(data_ahk.XRTOBIFTEMP_4_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.XRTOBIFTEMP_2_RTD_,data_tam_rebin(*,m_ct),psym=3,title='OBIF2 corr='+string(correlate(data_ahk.XRTOBIFTEMP_2_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.XRTOBIFTEMP_1_RTD_,data_tam_rebin(*,m_ct),psym=3,title='OBIF1 corr='+string(correlate(data_ahk.XRTOBIFTEMP_1_RTD_,data_tam_rebin(*,m_ct)))
	XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
		'XRT centroid positions (page '+strupcase(strmid(2*m+1,7,8))+') from file ' + filein
	plot,data_ahk.REARTUBET_2_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT2 corr='+string(correlate(data_ahk.REARTUBET_2_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_16_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT16 corr='+string(correlate(data_ahk.REARTUBET_16_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_15_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT15 corr='+string(correlate(data_ahk.REARTUBET_15_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_1_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT1 corr='+string(correlate(data_ahk.REARTUBET_1_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_4_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT4 corr='+string(correlate(data_ahk.REARTUBET_4_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_14_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT14 corr='+string(correlate(data_ahk.REARTUBET_14_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_13_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT13 corr='+string(correlate(data_ahk.REARTUBET_13_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_3_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT3 corr='+string(correlate(data_ahk.REARTUBET_3_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_6_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT6 corr='+string(correlate(data_ahk.REARTUBET_6_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_12_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT12 corr='+string(correlate(data_ahk.REARTUBET_12_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_11_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT11 corr='+string(correlate(data_ahk.REARTUBET_11_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_5_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT5 corr='+string(correlate(data_ahk.REARTUBET_5_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_8_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT8 corr='+string(correlate(data_ahk.REARTUBET_8_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_10_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT10 corr='+string(correlate(data_ahk.REARTUBET_10_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_9_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT9 corr='+string(correlate(data_ahk.REARTUBET_9_RTD_,data_tam_rebin(*,m_ct)))
	plot,data_ahk.REARTUBET_7_RTD_,data_tam_rebin(*,m_ct),psym=3,title='RT7 corr='+string(correlate(data_ahk.REARTUBET_7_RTD_,data_tam_rebin(*,m_ct)))

XYOUTS,alignment=0.5,/device,12375,18000,size=1.1, $
	'XRT centroid positions (page '+strupcase(strmid(2*m+2,7,8))+') from file ' + filein

endfor 

device,/close

end


