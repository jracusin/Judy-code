#!/usr/bin/env python

import matplotlib.pyplot as plot
import numpy as np

def grb130907_lat_lc():

	plot.figure()

	t=[[3500,4500],[4500,7000],[15000,18000]]
	tmid=[np.sum(t[i])/2. for i in range(3)]
	terr=[tmid[0]-t[0][0],tmid[1]-t[1][0],tmid[2]-t[2][0]]
	phflux=[4.89e-7,6.94e-7,4.33e-10]
	phfluxerr=[3.96e-7,5.47e-7,4.33e-10]
	engflux=[2.43e-10,2.23e-10,3.82e-13]
	engfluxerr=[1.73e-10,1.5e-10,3.82e-13]

	plot.scatter(tmid,phflux,label=r'Photon Flux (ph cm$^{-2}$ s$^{-1}$)')
	plot.errorbar(tmid,phflux,xerr=terr,yerr=phfluxerr, fmt='o')
	#plot.errorbar(tmid[1],phflux[1],xerr=terr[1],yerr=phfluxerr[1], fmt='o',uplims=True,color='blue')

	plot.scatter(tmid,engflux,label=r'Energy Flux (erg cm$^{-2}$ s$^{-1}$)')
	plot.errorbar(tmid,engflux,xerr=terr,yerr=engfluxerr, fmt='o')


	plot.xscale('log')
	plot.yscale('log')
	plot.xlim([3e3,20000])
	plot.ylim([1e-13,1e-5])
	plot.xlabel('Time (s)')
	plot.ylabel('Flux')

	plot.legend(loc='lower left')
	plot.savefig('GRB130907A_LAT_LC.png')
	plot.show()


	return tmid,phflux,terr,phfluxerr