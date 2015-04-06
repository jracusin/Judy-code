PRO aplothist, arr, xhist,yhist, BIN=bin,  MIN=min, MAX=max, $
              NOPLOT=NoPlot, OVERPLOT=Overplot, $
              PSYM = psym, Peak=Peak, Fill=Fill, FCOLOR=Fcolor, FLINE=FLINE, $
              FSPACING=Fspacing, FPATTERN=Fpattern, $
              FORIENTATION=Forientation, $
              ANONYMOUS_ = dummy_, norm = norm, $
              _EXTRA = _extra

plothist, arr, xhist,yhist, BIN=bin,  MIN=min, MAX=max, $
              /NoPlot,  $
              PSYM = psym, Peak=Peak, Fill=Fill, FCOLOR=Fcolor, FLINE=FLINE, $
              FSPACING=Fspacing, FPATTERN=Fpattern, $
              FORIENTATION=Forientation, $
              ANONYMOUS_ = dummy_, norm = norm, $
              _EXTRA = _extra

aplot,1,xhist,yhist,/nodata,_extra=_extra
plothist, arr, xhist,yhist, BIN=bin,  MIN=min, MAX=max, $
              PSYM = psym, Peak=Peak, Fill=Fill, FCOLOR=Fcolor, FLINE=FLINE, $
              FSPACING=Fspacing, FPATTERN=Fpattern, $
              FORIENTATION=Forientation, $
              ANONYMOUS_ = dummy_, norm = norm, $
              _EXTRA = _extra,/overplot

return
end 
