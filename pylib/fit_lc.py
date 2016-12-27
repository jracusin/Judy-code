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
import fit_functions

def download_UL():

	import wget
	import re

	# ### download trigIDs
	# url="http://swift.gsfc.nasa.gov/archive/grb_table/table.php?obs=Swift&year=All+Years&restrict=none&grb_time=1&grb_trigger=1"
	# filename=wget.download(url)

	# f=open(filename,'r')
	# lines=f.readlines()

	# for line in lines: 
	# 	if 'grb_table/tmp' in line: 
	# 		tmp=line
	# 		continue

	# tmp=re.split('<|>',tmp)

	# trigfile=tmp[4]

	# url="http://swift.gsfc.nasa.gov/archive/grb_table/tmp/"+tmp[4]
	# filename=wget.download(url)
	# print filename

	# os.remove(table.php)

	# data=ascii.read(filename,data_start=1)

	#wget files for LCs & spectra

	url="http://www.swift.ac.uk/xrt_curves/allcurves2.php"
	filename=wget.download(url)
	f=open(filename,'r')
	lines=f.readlines()

	for line in lines: 
		if 'xrt_curves' in line: 
			tmp=line
			continue
	tmp=re.split('<|>',tmp)


	return tmp

def read_lcfit(model):

	file='lc_fit_out_idl_int9.dat'
	f=open(file,'r')
	lines=f.readlines()
	pnames=[]
	par=[]
	pneg=[]
	ppos=[]
	go=0
	for line in lines:
		tmp=line.split()
		if len(tmp) > 2:
			pnames.append(tmp[0])
			par.append(float(tmp[1]))
			pneg.append(float(tmp[2]))
			ppos.append(float(tmp[3]))
		if (len(tmp) == 2) & go==0:
			chisq=float(tmp[1])
			go=1
		if (len(tmp) == 2) & go==1:
			dof=float(tmp[1])
			continue

	p=fit_params(model,pnames,par,pneg,ppos,chisq,dof)
	
	return p

class fit_params:
	
	def __init__(self,model,pnames,par,perror_neg,perror_pos,chisq,dof):
		self.model=model
		self.pnames=pnames
		self.par=par
		perror=np.array([perror_neg,perror_pos])
		perror=perror.transpose()
		self.perror=perror
		self.chisq=chisq
		self.dof=dof

	def list(self):
		print('Model = ',self.model)
		print('Pname = ',self.pnames)
		print('Par = ',self.par)
		print('Perror = ',self.perror)
		print('Chisq = ',self.chisq)
		print('DOF = ',self.dof)

	### to make list of object: plist=[fit_params(count) for count in xrange(n)]

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

	start=0
	for f in range(nfiles):
		if os.path.isfile(files[f]):
			print(files[f])
#			print f
			if f<=3:
				data=ascii.read(files[f],header_start=2,data_start=3)
				ncol=len(data[0])
				ndata=len(data)
				if ncol >16:
					del data['SYS_NEG','SYS_POS']
				t=Column(np.repeat(type[f],ndata),name='Type')
				data.add_column(t)
				if start==0:
					d=data
					start=1
				else:
					d=join(d,data,join_type='outer')
				print len(d)
			if f==4:
				if 'WTSLEW' in open(files[f]).read():
					data=read_curve(files[f])
					ndata=len(data)
					t=Column(np.repeat(type[f],ndata),name='Type')
					data.add_column(t)
					d=join(d,data,join_type='outer')

	d.rename_column('!Time','Time')

	return d

def plot_lcfit(lc,p=False,resid=True):

### if no p, plot without

	if resid: 
		f,(ax1,ax2) = plot.subplots(2,sharex=True)
	else: f,ax1=plot.subplots(1)

	xrttype=np.array(['WTSLEW','WT','WTUL','PC','PCUL'])
	color=np.array(['cyan','blue','blue','red','red'])

	for t in range(len(xrttype)):
		w=np.where(lc['Type']==xrttype[t])
		if len(w[-1])>0:

			if 'UL' in xrttype[t]: 
				uplims=True
				yerr=0.8*lc['Rate'][w]
				noleg='_'
			else:
				uplims=False
				yerr=[-lc['Rateneg'][w],lc['Ratepos'][w]]
				noleg=''

			ax1.errorbar(lc['Time'][w],lc['Rate'][w],xerr=[-lc['T_-ve'][w],lc['T_+ve'][w]],\
				yerr=yerr,ecolor=color[t],linestyle='None',\
				capsize=0,label=noleg+xrttype[t],uplims=uplims,fmt='none')

			if resid: 
				yfit=fit_functions.call_function(p.model,lc['Time'][w],p)
				res=lc['Rate'][w]/yfit
				ax2.errorbar(lc['Time'][w],res,xerr=[-lc['T_-ve'][w],lc['T_+ve'][w]],\
					yerr=yerr/yfit,linestyle='None',capsize=0,fmt='none',ecolor=color[t],\
					uplims=uplims,label=noleg+xrttype[t])
				ax2.plot([1,1e7],[1,1],linestyle='--',color='black')
				f.subplots_adjust(hspace=0)

	if p:
		print(p.model)
#		yfit=getattr(importlib.import_module('fit_functions'),p.model)(lc['Time'],p.par)
		yfit=fit_functions.call_function(p.model,lc['Time'],p)
		ax1.plot(lc['Time'],yfit,color='green')

		
	ax1.legend(loc="upper right")
	ax1.set_yscale('log')
	ax1.set_xscale('log')
	ax1.set_ylabel(r'Count Rate (0.3-10 keV) (erg cm$^{-2}$ s$^{-1}$)')
	if resid:
		ax2.set_xscale('log')
		ax2.set_xlabel('Time since trigger (s)')
		ax2.set_ylabel('Residual')
		ax2.set_ylim([0,3])
	else:
		ax1.set_xlabel('Time since trigger (s)')
	plot.show()
	plot.close('all')

	return


def test():

	t=np.array([10,100,1000,1e4,1e5,1e6])
	tt=np.array([[10,30],[30,100],[100,300],[300,1000],[1000,3000],[3000,1e4],[1e4,3e4],[3e4,1e5]])
	tt=tt.transpose()
	p=np.array([1.,3.,100.,0.5,1000.,1.2,1e4,2.2])


	return