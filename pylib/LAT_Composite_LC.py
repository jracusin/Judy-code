#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to make plots for the LAT GRBs

------------------------------------------------------------------------
"""
import numpy as np
import re
from astropy.io import fits
import matplotlib.pylab as plot

def plot_lc(trigger,trigtime,GBM_dets,tstart,tstop):

	dir='~/FermiData/'+trigger

	plot.close('all')
	fig=plot.figure(1)
	f, (ax1,ax2,ax3,ax4,ax5) = plot.subplots(5,sharex=True)
	axs=ax1,ax2,ax3,ax4

#	i=0
#	for det in GBM_dets:

	det=GBM_dets[0]
	d=fits.open('glg_tte_'+det+'_'+trigger+'_v00.fit')
	d=d[2].data
	d['TIME']=d['TIME']-trigtime
#	w=np.where((d['TIME']>tstart) & (d['TIME']<tstop))
	t=np.histogram(d['TIME'],bins=np.arange(min(d['TIME']),max(d['TIME']),0.256))
	# b=fits.open(trigger+'_'+det+'_bkgintervals.fits')
	# b=b[1].data
	# # wb0=np.where(((t[1]>b.START[0]-trigtime) & (t[1]<b.STOP[0]-trigtime)))
	# # wb1=np.where(((t[1]>b.START[1]-trigtime) & (t[1]<b.STOP[1]-trigtime)))
	# # wb=np.append(wb0,wb1[0:len(wb1)-1])
	# # wb=wb.astype('int')
	# # p=np.polyfit(t[1][wb],t[0][wb],1)
	# # print p
	ax1.plot(t[1][0:len(t[0])],t[0]/0.256)
	ax1.annotate('NaI',xy=(170,3000),xycoords='data',fontsize=16,color='black')
#	ax1.plot(t[1][0:len(t[0])],p[0]*t[1][0:len(t[0])]**p[1],color='green',ls='dashed')
	ax1.set_ylim([1000,4000])

	det=GBM_dets[1]
	d=fits.open('glg_tte_'+det+'_'+trigger+'_v00.fit')
	d=d[2].data
	d['TIME']=d['TIME']-trigtime
#	w=np.where((d['TIME']>tstart) & (d['TIME']<tstop))
	t=np.histogram(d['TIME'],bins=np.arange(min(d['TIME']),max(d['TIME']),0.256))
	ax2.plot(t[1][0:len(t[0])],t[0]/0.256)
	ax2.annotate('BGO',xy=(170,3000),xycoords='data',fontsize=14,color='black')
	ax2.set_ylim([1000,4000])

	l0=fits.open('gll_ft1_tr_bn170214649_v00_mkt_transient.fit')
	l1=fits.open('gll_ft1_tr_bn170214649_v00_mkt_source.fit')
	lle=fits.open('gll_lle_bn170214649_v01.fit')

	l0=l0[1].data
	l1=l1[1].data
	lle=lle[1].data
	print len(l0),len(l1)
#	t=np.histogram(l0.TIME-trigtime,bins=np.arange(tstart,tstop,1))
#	ax3.plot(t[1][0:len(t[0])],t[0])

	ax3.hist(lle.TIME-trigtime,bins=np.arange(tstart,tstop,0.512),histtype='step',weights=np.repeat(1./0.512,len(lle)))
	ax3.annotate('LLE',xy=(170,400),xycoords='data',fontsize=14,color='black')
	ax3.set_ylim([0,600])


	ax4.hist(l0.TIME-trigtime,bins=np.arange(tstart,tstop,0.512),histtype='step',weights=np.repeat(1./0.512,len(l0)))
	ax4.annotate('LAT Transient020',xy=(130,15),xycoords='data',fontsize=14,color='black')
	ax4.set_ylim([-2,25])

	ax5.scatter(l0.TIME-trigtime,l0.ENERGY,color='gray')
	ax5.scatter(l1.TIME-trigtime,l1.ENERGY,color='red')
	ax5.set_yscale('log')
	ax5.annotate('LAT Transient020',xy=(140,4.8e3),xycoords='data',fontsize=13,color='gray')
	ax5.annotate('LAT Source',xy=(145,2e3),xycoords='data',fontsize=13,color='red')


	f.subplots_adjust(hspace=0)
	ax3.set_xlim([tstart,tstop])
	ax4.set_xlabel('Time since trigger (s)')
	ax1.set_ylabel(r'Counts s$^-1$')
	ax2.set_ylabel(r'Counts s$^-1$')
	ax3.set_ylabel(r'Counts s$^-1$')
	ax4.set_ylabel(r'Counts s$^-1$')
	ax5.set_ylabel('Energy (MeV)')

	f.savefig('composite_LC.png', bbox_inches='tight')

	plot.show()
	plot.close()

	return t