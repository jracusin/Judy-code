#!/usr/bin/env python

import numpy as np
import ephem as eph

from BurstCube.LocSim.Utils import deg2HMS,deg2DMS

class GRB(object):
    
    def __init__(self,ra,dec,amplitude=10000.,
                 decay=2.0,
                 T0=0.0,
                 binz = 1.0,
                 real=False,filename=""):
        self.loc = (ra,dec)
        self.amplitude = amplitude
        self.decay = decay
        self.T0 = T0
        self.binz = binz #bin size in seconds
        self.window = 20 

        if real:
            self.locdb = "{},f|V,{},{},21.26,2000".format(filename,deg2HMS(ra),deg2DMS(dec))
        else:
            self.locdb = "GRB,f|V,{},{},21.26,2000".format(deg2HMS(ra),deg2DMS(dec))
        self.eph = eph.readdb(self.locdb)
        self.eph.compute()
        
        if real:
            self.loadReal(filename)
        else:
            self.loadFRED()

    def loadReal(self,filename,nbins=20000.,sub=True):

        from astropy.io import fits as pyfits

        grb_hdu = pyfits.open(filename)
        grb_hist = np.histogram(grb_hdu[1].data['TIME']-grb_hdu[1].header['TZERO1'],
                                bins=self.window/self.binz)

        self.counts = grb_hist[0]
        self.t = grb_hist[1][:-1]

        if sub:
            avg = np.average(self.counts[self.t < -1.0])
            self.counts = self.counts - avg

    def loadFRED(self):
            
        self.t = np.linspace(-self.window/2.,self.window/2.,self.window/self.binz)

        #amp = self.amplitude * self.binz
        amp = self.amplitude
        self.counts = np.zeros_like(self.t)
        self.counts[self.t > self.T0] = amp*(self.t[self.t > self.T0]-self.T0)\
            /np.exp(self.decay*(self.t[self.t > self.T0] - self.T0))


