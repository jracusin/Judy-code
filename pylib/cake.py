#!/usr/bin/env python
"""
------------------------------------------------------------------------

Cake

------------------------------------------------------------------------
"""

import numpy as np
from astropy.io import ascii

def read_agn():

	data=ascii.read("/Users/jracusin/Desktop/Personal/Cake/CTA102_1d_lc.txt",\
		names=['date','MJD','TS','Flux100','Flux100_Err','Spectral_Index','Spectral_Index_Err','UpperLimit'],\
		data_start=1)

	data2=ascii.read("/Users/jracusin/Desktop/Personal/Cake/results_gtlike_J1104.4%2B3812.txt",\
		names=['t1','t2','mjd1','mjd2','flux','fluxerr','Spectral_Index','Spectral_Index_Err','tmp1','tmp2',\
		'tmp3','tmp4','tmp5','tmp6'])

	np.set_printoptions(threshold=np.nan)

	for j in range(4):
		if j == 0: d=data['Flux100']
		if j == 1: d=data['Spectral_Index']
		if (j == 0) or (j == 1): t=data['MJD']
		if j == 2: d=data2['flux']
		if j == 3: d=data2['Spectral_Index']
		if (j == 2) or (j == 3): t=data2['mjd1']

		a=np.argsort(t)
		print len(t),len(d),len(a)
		d=d[a]
		fmin=min(d)
		fmax=max(d)

		s="["
		f=np.array(np.round(d/(fmax-fmin)*255.))

		for i in range(len(f)-1):
#			if f[i] == 0: print t[a[i]]
			s=s+str(int(f[i]))+","
		s=s+str(int(f[len(f)-1]))+"]"
		print len(s)

		if j == 0: s1=s
		if j == 1: s2=s
		if j == 2: s3=s
		if j == 3: s4=s

	return s1,s2,s3,s4,f