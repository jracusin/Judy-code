; $Id: pass1_hk_plots.pro 1.0 2002/01/18 17:25:30 burrows $
;
;author	ljc
;date	feb17,1995
;	file to plot XRT housekeeping data.  This routine plots
;	all of the HK channels on the printer.
;
; Rev:
;	11/04/03 by DNB: changed scaling for time plot
;	04/02/02 by DNB: changed horizontal scaling of plots
;	02/23/02 by DNB: updated channel assignments for these plots
;
; $Log: pass1_hk_plots.pro $
;


pro	pass1_hk_plots

@pass1_common

filenm=''
;
filenm = hk_fits_file
;
data=readfits(filenm,h)
;
;set up variables for 12 channels of interest
time=data(0:0,*)
max_time = max(time)
scale = fix(alog10(max_time))
times = time/(10.0^scale)
units = '(10^'+strtrim(string(scale),2)+' s)'

;get header information?

!P.Multi=[0,5,6]
;
print,'PASS1_HK_PLOTS: Sending housekeeping plots to the printer'
set_plot,'ps'
device, /landscape

;	plot,***********************
ymin = min(times) 
ymax = max(times) 
plot,times[xmin:xmax],title='S/C Time',xtitle='Sample #', $
	ytitle='S/C Time '+units, yrange=[ymin,ymax]

;plot,time,title='S/C Time',xtitle='Sample #',ytitle='MET (s)'

for i=1,28 do begin
	chan = hdr_hk_chan[i-1]
	ymin = min(data[i,*]) - 0.5
	ymax = max(data[i,*]) + 0.5
	plot,times,data[i,*],title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax]
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime


;put in all plots then close device
	device,/close
	spawn,'lpr -r idl.ps'
	set_plot,'X'

;
!P.Multi=0	; reset to one plot per page

end
