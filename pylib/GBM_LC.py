#!/usr/bin/env python

import matplotlib.pyplot as plot
import numpy as np
from astropy.io import ascii,fits

def download_lcs(trigtime):

	# turn trigtime into date, grab files from FSSC


	return

def get_files(dir):

	import os
	files=[]
	for file in os.listdir(dir):
		if file.endswith(".pha.txt"):
			files=np.append(files,(os.path.join(dir, file)))

	return files

def rebin_factor( a, newshape ):
        '''Rebin an array to a new shape.
        newshape must be a factor of a.shape.
        '''
        assert len(a.shape) == len(newshape)
        assert not sometrue(mod( a.shape, newshape ))

        slices = [ slice(None,None, old/new) for old,new in zip(a.shape,newshape) ]
        return a[slices]

def plot_lcs(files,trigtime,trange):

	fit=plot.figure(figsize=(5,8))
	nf=len(files)

	i=1
	c=[]
	bs=1.
	for f in files:
		n=fits.open(f)
		tstart=n[2].header['TSTART']
		zp=n[2].header['TZERO1']
		t=n[2].data['TIME']-trigtime
		w=np.where((t > trange[0]) & (t < trange[1]))
		nt=len(w[0])
		t=t[w]
		p=plot.subplot(nf,1,i)
		c=[sum(n[2].data['COUNTS'][w[0][j],:]) for j in range(nt)]
		c=np.array(c)
		rt=[]
		rc=[]
		for k in range(trange[0],trange[1]):
			w=np.where((t>k) & (t<k+bs))
			rt=np.append(rt,np.mean(t[w[0]]))
			rc=np.append(rc,sum(c[w[0]]))
		p.plot(rt,rc)
		plot.xlim(trange)
		i=i+1

	plot.show()

	return t,c