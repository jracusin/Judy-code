#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to make TAO observing movie

------------------------------------------------------------------------
"""

import numpy as np
import matplotlib.pylab as plot


def skymap():

	fig = plot.figure(figsize=(8,6))
	ax = fig.add_subplot(111, projection="mollweide")
	ax.set_xticklabels('None')
	ax.grid(True)

	plot.show()