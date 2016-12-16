#!/usr/bin/env python
"""
------------------------------------------------------------------------

Light curve fitting

------------------------------------------------------------------------
"""

import numpy as np
from astropy.io import ascii
from astropy.io import fits
from astropy.table import Table,Column,join
import math
import os
import matplotlib.pylab as plot

def download_UL():
	#wget files for LCs & spectra

	return

def read_curve(file):

	f=open(file,'r')
	lines=f.readlines()
	header=[]
	start=0
	done=0
	data=[]
	for line in lines:
		tmp=line.split()
		if len(tmp) >0:
			if (tmp[1] == 'WTSLEW') & (not header):
				start=1
				continue
			if (tmp[1] == 'NO'):
				break
			if (start == 1) & (not header):
				header=tmp
#				header[0]='Time'
				d=Table(names=header)
				continue
			if (start == 1) & (len(header)>0):
				d.add_row(tmp)

	f.close()

	return d

def read_phil():

	files=np.array(['WTCURVE.qdp','WTUL.qdp','PCCURVE.qdp','PCUL.qdp','curve.qdp'])
	type=np.array(['WT','WTUL','PC','PCUL','WTSLEW'])
	nfiles=len(files)

	# need dictionary with tags same as IDL
	# lc={'time':0.,'tstart':0.,'tstop':0.,'rate':0.,'rate_err':[0.,0.],'fracexp':0.,\
	# 	'bkg_rate':0.,'bkg_rate_err':[0.,0.],'corrfact':0.,'CtsInSrc':0.,'BGInSrc':0.,\
	# 	'Exposure':0.,'Sigma':0.,'SNR':0.}

	for f in range(nfiles):
		if os.path.isfile(files[f]):
			print(files[f])
			print f
			if f<=3:
				data=ascii.read(files[f],header_start=2,data_start=3)
				ncol=len(data[0])
				ndata=len(data)
				if ncol >16:
					del data['SYS_NEG','SYS_POS']
				t=Column(np.repeat(type[f],ndata),name='Type')
				data.add_column(t)
				if f>0:
					d=join(d,data,join_type='outer')
				else:
					d=data
				print len(d)
			if f==4:
				data=read_curve(files[f])
				ndata=len(data)
				t=Column(np.repeat(type[f],ndata),name='Type')
				data.add_column(t)
				d=join(d,data,join_type='outer')

	d.rename_column('!Time','Time')
	print(d)

	return d

def plot_lcfit(lc,p=False):

### if no p, plot without

	fig=plot.figure()
	type=np.array(['WTSLEW','WT','WTUL','PC','PCUL'])
	color=np.array(['cyan','blue','blue','red','red'])

	for t in range(len(type)):
		w=np.where(lc['Type']==type[t])
		if len(w[-1])>0:

			if 'UL' in type[t]: 
				uplims=True
				yerr=0.8*lc['Rate'][w]
				noleg='_'
			else:
				uplims=False
				yerr=[-lc['Rateneg'][w],lc['Ratepos'][w]]
				noleg=''
			plot.errorbar(lc['Time'][w],lc['Rate'][w],xerr=[-lc['T_-ve'][w],lc['T_+ve'][w]],\
				yerr=yerr,ecolor=color[t],linestyle='None',\
				capsize=0,label=noleg+type[t],uplims=uplims,fmt='none')

	if p:
		print(p)


	plot.yscale('log')
	plot.xscale('log')
	plot.xlabel('Time since trigger (s)')
	plot.ylabel(r'Count Rate (0.3-10 keV) (erg cm$-2$ s$-1$)')
	plot.legend(loc='upper right')
	plot.show()
	plot.close()


	return


def test():

	t=np.array([10,100,1000,1e4,1e5,1e6])
	tt=np.array([[10,30],[30,100],[100,300],[300,1000],[1000,3000],[3000,1e4],[1e4,3e4],[3e4,1e5]])
	tt=tt.transpose()
	p=np.array([1.,3.,100.,0.5,1000.,1.2,1e4,2.2])


	return