; $Id: stripchart_hk_plots.pro 1.0 2002/01/18 17:25:30 burrows $
;
;author	ljc
;date	feb17,1995
;	file to plot XRT stripchart housekeeping data.  This routine plots
;	all of the HK channels on the printer.
;
; Rev:
;	05/31/04 by JLR: made time bin min/max use a widget rather than line input,
;			 needed for semi-automated analysis
;	05/27/04 by JLR: changes so that sctime listed is difference from start 
;			 sctime, so now x-values are readable.  also make /noplots 
;			 work so as to not print the plots.  also overplotted limits.
;			 Made noprint and noplot keywords work.
;	05/24/04 by JLR: changed page number on last page to be 6 instead of 5
;	11/04/03 by DNB: changed scaling for time plot
;	06/13/03 by DNB: put all plots in one file
;	06/12/03 by DNB: fixed error in assignment of plot names to channels
;		for plot pages 3 and 4.  Also re-arranged tube heater
;		plots to match locations on tube (matches Heater_Layout
;		spreadsheet).
;	03/28/03 by DNB: modified output Postscript plot file names.
;	04/12/02 by DNB: major changes to plot order to group plots in
;		more convenient order
;	04/02/02 by DNB: changed horizontal scaling of plots
;	02/23/02 by DNB: updated channel assignments for these plots
;
; $Log: stripchart_hk_plots.pro $
;


pro	stripchart_hk_plots,xmin,xmax
;	xmin and xmax are output variables

@pass1_common

;load_hk_tables

filenm=''
;
filenm = hk_fits_file

print, 'Reading HK FITS file ...' 
;
data=readfits(filenm,h)
;
;set up variables for 12 channels of interest
time=data(0:0,*)
max_time = max(time)
scale = long(alog10(max_time))
;min_time=min(time)
;times = time-min(time)  ;/(10.0^scale)

units = strtrim(min(time),1)+' + (s)' ;+' + (10^'+strtrim(string(scale),2)+' s)'

if not keyword_set(quick_look_mode) then begin
   print,'Opening interactive plot window.  Select min and max time bins'
   print,'    for use in stripchart plots...'
   show_plot = 2
   xsize = n_elements(time)
   channels = indgen(xsize)

   plotter,show_plot,channels,time,title='Select min and max time bins for stripchart',$
      xtitle='Time bin #', ytitle='S/C time (seconds)' ;,/noblock

   title='Input file has time bins from 0 - '+strtrim(string(xsize),2)

   tmp=dialog_input(prompt=['Time bin min:','Time bin max:'],/integer,nfields=2, $
	title=title,xsize=400,initial=[1,xsize-1])
   xmin=tmp[0]
   xmax=tmp[1]
endif else begin 
   xsize = n_elements(time)
   channels = indgen(xsize)
   xmin=1
   xmax=xsize-1
endelse 

;print,'Input file has time bins from 0 - ', strtrim(string(xsize),2)
;read,'Enter min and max time bins to use for stripchart: ', xmin, xmax
;xmin = max([xmin, 0])
;xmax = min([xmax, xsize-1])

;;;set up xtickinterval
max_time = max(time[xmin:xmax])
min_time=min(time[xmin:xmax])
times = time-min_time  ;/(10.0^scale)
xmaxtimes=max(times[xmin:xmax])
if max(times) gt 1000 then $
	xtickinterval=(xmaxtimes/4.-(xmaxtimes/4. mod 1000)) else $
	xtickinterval=(xmaxtimes/4.-(xmaxtimes/4. mod 100))

;get header information?

!P.Multi=[0,5,6]
;
print,' '
print,'STRIPCHART_HK_PLOTS: Generating stripchart plots...'
print,' '

set_plot,'ps'
device, /landscape

;	plot,***********************
ymin = min(times[xmin:xmax]) 
ymax = max(times[xmin:xmax])
plot,times[xmin:xmax],title='S/C Time',xtitle='Sample #', $
	ytitle='S/C Time '+units, yrange=[ymin,ymax],ytickformat='(f10.0)'

; Count rate
ymin = min(data[114,xmin:xmax]) - 0.5
ymax = max(data[114,xmin:xmax]) + 0.5
plot,times[xmin:xmax],data[114,xmin:xmax],$
		title='XRT Count Rate',  xtitle='S/C Time '+units, $
		ytitle='(cps)', yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval

plot_order =    [30, 45, 63, $
	 76,108, 92, 93,106, $
	105, 98,111, 31, 29, $
	 32, 47, 62, 78, 60, $
	 61, 46, 77, 91, 90, $
	 75, 94,107,104,103 ]

;HK channels 1-28
for i=1,28 do begin
	j = plot_order[i-1] 
	chan = stripchart_hk_chan[j-1]
	ymin = min(data[j,xmin:xmax]) - 0.5
	ymax = max(data[j,xmin:xmax]) + 0.5
	plot,times[xmin:xmax],data[j,xmin:xmax],$
		title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval
	oplot,[0,100000],[hk_high[chan],hk_high[chan]],line=1
	oplot,[0,100000],[hk_low[chan],hk_low[chan]],line=1
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 1) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime


;
;***************************************************************
; Now, do second page
;***************************************************************
;
!P.Multi=[0,5,6]
plot_order =    [ 1, 25, 24, 23, 22, $
		 28, 27, 16, 15, 14, $
		 13, 12, 10,  9,  8, $
		 21, 20,  7,  6,  5, $
		 19, 26,  4,  3,  2, $
		 18, 17, 112 ]
	

;	plot,***********************
for i=1,28 do begin
	j = plot_order[i-1]
	chan = stripchart_hk_chan[j-1]
	ymin = min(data[j,xmin:xmax]) - 0.5
	ymax = max(data[j,xmin:xmax]) + 0.5
	plot,times[xmin:xmax],data[j,xmin:xmax], $
		title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval
	oplot,[0,100000],[hk_high[chan],hk_high[chan]],line=1
	oplot,[0,100000],[hk_low[chan],hk_low[chan]],line=1
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 2) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime



;
;***************************************************************
; Now, do third page
;***************************************************************
;
!P.Multi=[0,5,6]
plot_order =    [101,100,102, 99,113, $
		 109, 95,110, 96, 97, $
		  35, 50, 65, 80, 34, $
		  49, 64, 79, 33, 48]
;	plot,***********************
for i=1,20 do begin
	j = plot_order[i-1]
	chan = stripchart_hk_chan[j-1]
	ymin = min(data[j,xmin:xmax]) - 0.5
	ymax = max(data[j,xmin:xmax]) + 0.5
	plot,times[xmin:xmax],data[j,xmin:xmax], $
		title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval
	oplot,[0,100000],[hk_high[chan],hk_high[chan]],line=1
	oplot,[0,100000],[hk_low[chan],hk_low[chan]],line=1
endfor

acs_flags = data[117,*] + data[118,*]*2 + data[119,*]*4 + data[120,*]*8
plot,times[xmin:xmax],acs_flags[xmin:xmax], $
		title='ACS Flags', xtitle='S/C Time '+units, $
		ytitle='', yrange=[-0.2,15.2]


flags = ['ACS: IS_SETTLED', 'ACS: IS_IN_10_ARCMIN',$
		'ACS: IN_SAA', 'ACS: Safe Mode Flag']
for i=117,120 do begin
	ymin = -0.1
	ymax = 1.1
	plot,times[xmin:xmax],data[i,xmin:xmax], $
		title=flags[i-117], xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval

endfor

i=116
ymin = min(data[i,xmin:xmax]) - 0.5
ymax = max(data[i,xmin:xmax]) + 0.5
plot,times[xmin:xmax],data[i,xmin:xmax], $
		title='XRT Mode Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval

i=115
;ymin = min(data[i,*]) - 0.5
;ymax = max(data[i,*]) + 0.5
ymin = 0
ymax = 100
plot,times[xmin:xmax],data[i,xmin:xmax], $
		title='XRT State Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval

xlabel = times[xmax] + 0.1*(times[xmax] - times[xmin])
xyouts,/data,xlabel,10.0,'Auto'
xyouts,/data,xlabel,28.0,'Manual'
xyouts,/data,xlabel,60.0,'RED'

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 3) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime



;
;***************************************************************
; Now, do fourth page
;***************************************************************
;
!P.Multi=[0,4,5]
plot_order =    [71, 86, 59, 44, $
		 41, 56, 89, 74, $
		 72, 87, 58, 43, $
		 42, 57, 88, 73, $
		 66, 81, 51, 36]

;	plot,***********************
for i=1,20 do begin
	j = plot_order[i-1]
	chan = stripchart_hk_chan[j-1]
	ymin = min(data[j,xmin:xmax]) - 0.5
	ymax = max(data[j,xmin:xmax]) + 0.5
	plot,times[xmin:xmax],data[j,xmin:xmax], $
		title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 4) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime



;
;***************************************************************
; Now, do fifth page
;***************************************************************
;
!P.Multi=[0,4,5]
plot_order =    [55, 82, 67, 40, $
		 85, 52, 37, 70, $
		 54, 83, 68, 39, $
		 84, 53, 38, 69 ]	

;	plot,***********************
for i=1,16 do begin
	j = plot_order[i-1]
	chan = stripchart_hk_chan[j-1]
	ymin = min(data[j,xmin:xmax]) - 0.5
	ymax = max(data[j,xmin:xmax]) + 0.5
	plot,times[xmin:xmax],data[j,xmin:xmax], $
		title='HK CH' + strtrim(hk_chan[chan],2) $
		+ ': ' + hk_name(chan), xtitle='S/C Time '+units, $
		ytitle=hk_units(chan), yrange=[ymin,ymax], $
		xtickformat='(f10.0)',xtickinterval=xtickinterval
endfor

XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 5) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime


;
;***************************************************************
; Now, do sixth page
;***************************************************************
;
!P.Multi=[0,1,2]


plot,times[xmin:xmax],acs_flags[xmin:xmax], $
		title='ACS Flags', xtitle='S/C Time '+units, $
		ytitle='', yrange=[-0.2,15.2]

xlabel = times[xmax] - 0.2*(times[xmax] - times[xmin])
xyouts,/data,xlabel,0.0,'1=SETTLED'
xyouts,/data,xlabel,5.0,'2=IN_10_ARCMIN'
xyouts,/data,xlabel,10.0,'4=IN SAA'
xyouts,/data,xlabel,15.0,'8=SAFEHOLD'


i=116
plot,times[xmin:xmax],data[i,xmin:xmax], $
		title='XRT Mode Flag', xtitle='S/C Time '+units, $
		ytitle='', yrange=[0.0,10.0]

xlabel = times[xmax] + 0.01*(times[xmax] - times[xmin])
xyouts,/data,xlabel,0.75,'Null'
xyouts,/data,xlabel,1.75,'SIM'
xyouts,/data,xlabel,2.75,'LIM'
xyouts,/data,xlabel,3.75,'PU'
xyouts,/data,xlabel,4.75,'LR'
xyouts,/data,xlabel,5.75,'WT'
xyouts,/data,xlabel,6.75,'PC'
xyouts,/data,xlabel,7.75,'Raw'
xyouts,/data,xlabel,8.75,'Bias'


XYOUTS,alignment=0.5,/device,13000,18000,size=1.1, $
	'XRT Housekeeping (page 6) from file ' + filein
XYOUTS,alignment=0,/device,1000,-200,size=0.8,runtime

;close device and print plots
device,/close
if (print_plots eq 1) then $
   print,'STRIPCHART_HK_PLOTS: Sending plots to the printer'
filename = filein + '.stripchart_plots.ps'
spawn,'mv idl.ps ' + filename
if (print_plots eq 1) then spawn,'lp  ' + filename
;spawn,'gzip ' + filename


set_plot,'X'

;
!P.Multi=0	; reset to one plot per page

end
