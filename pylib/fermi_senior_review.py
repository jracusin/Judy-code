#!/usr/bin/env python
"""
------------------------------------------------------------------------

Fermi Senior Review Plots

------------------------------------------------------------------------
"""

import numpy as np
import matplotlib.pylab as plot
from astropy.io import ascii
from astropy.io import fits
from astropy.table import Table
from time import strptime

def read_bib(dir='/Users/jracusin/Fermi/Senior_Review/SR2019/'):
	file='fermi_bibcodes.csv'
	bib0=ascii.read(dir+file)

	date=bib0['Date Published']
	month=[d[0:3] for d in date]
	year==[d[5:9] for d in date]
	print bib0.keys()

	return month,year
#	strptime('Feb','%b').tm_mon

#	bib=Table(names=('date','citations','category','subjects'))


#	return bib