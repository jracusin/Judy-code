#!/usr/bin/env python

import numpy as np
import ephem as eph

from BurstCube.LocSim.Utils import deg2DMS, deg2HMS

class Detector(object):

    def __init__(self, name = 'det', 
                       azimuth_angle = '0:0:0', 
                       zenith_angle = '0:0:0', 
                       noise = True,
                       background_rate = 10.,
                       window = 2.,
                       lat = "37:56:24.7", 
                       lon = "75:27:59",
                       elev = 550e3):


        self.sig_level = 5.
        self.obs = eph.Observer()
        self.name = name
        self.obs.lon = lon
        self.obs.lat = lat
        self.obs.elev = elev
        self.obs.date = "2001/1/1"
        self.obs.pressure = 0.
        self.zenith = eph.degrees(zenith_angle)
        self.azimuth = eph.degrees(azimuth_angle)
        self.noise = noise
        self.t = np.arange(0)
        self.background_rate = background_rate
        self.window = window
        self.significance = np.arange(0)
        self.square = False

    @property
    def altitude(self):
        return eph.degrees(np.pi/2 - self.zenith)
    
    @altitude.setter
    def altitude(self, altitude):
        alt = eph.degrees(altitude)
        self.zenith = eph.degrees(np.pi/2 - alt)

    @property
    def trigger_time(self):
    	above_five = self._sign_time[self.significance > self.sig_level]
    	if len(above_five) > 0:
            return above_five[0]
    	else:
            return -1

    @property
    def triggered_counts_error(self):
        if(self.trigger_time) > 0:
            return (np.round(np.sqrt(self.triggered_counts))).astype('int64')
        else:
            return -1

    @property
    def triggered_counts(self):
        if(self.trigger_time) > 0:
            window = int(self.window/self._grb.binz)
            return np.sum(self.response[self.trigger_time:self.trigger_time+window])
        else:
            return -1

    @property
    def sign_time(self):
        return self._sign_time*self._grb.binz - self._grb.window/2.

    @property
    def sign_times(self,key):
        return self._sign_times[key]*self._grb.binz - self.grb.window/2.
        
    def get_separation(self, grb):

        grb.eph.compute(self.obs)
        return eph.separation((self.altitude,self.azimuth),
            (grb.eph.alt,grb.eph.az))    

    def _separation(self):
        self._grb.eph.compute(self.obs)
        self.separation = eph.separation((self.altitude,self.azimuth),
            (self._grb.eph.alt,self._grb.eph.az))
        
    def _response(self, angular_response = True):
        
        if angular_response:
            if self._grb.eph.alt < -10.*np.pi/180.:
                y = np.zeros_like(self._grb.counts)
            else:
                if self.separation > np.pi/2.:
                    y = np.zeros_like(self._grb.counts)
                else:
                    y = self._grb.counts*np.cos(self.separation)
                    if self.square: 
                        az_diff = self.altitude - self._grb.eph.az 
                        y = y*(1./np.sqrt(2.))*(np.cos(az_diff)+np.sqrt(2))
        else:
            y = self._grb.counts

        bkgrd = self.background_rate*np.ones_like(y)
        y = np.maximum(0.001*bkgrd, y)
        # bkgrd = self.background_rate*np.ones(self._grb.T0+len(y))
        # y = np.maximum(bkgrd,np.concatenate([np.zeros(self._grb.T0),y]))
                        
        if self.noise:
            # This is slow (like 560 microsecs slow)
            y = np.maximum(bkgrd,y)
            y = [np.random.poisson(point,1)[0] for point in y]

        #self.t = np.arange(len(y))
        self.t = self._grb.t
        self.response = np.array(y)

    def _signifcances(self):

        windows = np.array([0.001,0.01,0.1,1.0])/self._grb.binz

        self._sign_times = dict([(window, np.arange(len(self.response))) for window in windows])
        self.significances = dict([(window, np.zeros_like(self.response)) for window in windows])

        for window in windows:

            window = int(window)
            
            if window < 1:
                print("Step must be bigger than 1.")
                self._sign_times[window] = np.arange(len(self.response))
                self.significances[window] = np.zeros_like(self.response)
            else:
                trunc = int(np.mod(len(self.response),window))
                resp_re = self.response[trunc:].reshape(-1,window)
                self._sign_times[window] = np.arange(len(self.response))[trunc:].reshape(-1,window)[1:,0]
                #self._sign_time = self._sign_time*self._grb.binz - self._grb.window/2.
                self.significances[window] = np.sum(resp_re[1:] - resp_re[:-1],axis=1)/np.sqrt(np.sum(resp_re[1:]+resp_re[:-1],axis=1))


    def _significance(self):

        window = int(self.window/self._grb.binz)

        if window < 1:
            print("Step must be bigger than 1.")
            self._sign_time = np.arange(len(self.response))
            self.significance = np.zeros_like(self.response)
        else:
            trunc = int(np.mod(len(self.response),window))
            resp_re = self.response[trunc:].reshape(-1,window)
            self._sign_time = np.arange(len(self.response))[trunc:].reshape(-1,window)[1:,0]
            #self._sign_time = self._sign_time*self._grb.binz - self._grb.window/2.
            self.significance = np.sum(resp_re[1:] - resp_re[:-1],axis=1)/\
                np.sqrt(np.sum(resp_re[1:]+resp_re[:-1],axis=1))

    def polar2cart(self,theta, phi):
        return np.array([np.sin(theta) * np.cos(phi),np.sin(theta) * np.sin(phi),np.cos(theta)])

    def cart2polar(self,x,y,z):
        theta,phi=np.array([np.arccos(z/np.sqrt(x**2+y**2+z**2)),np.arctan(y/x)])
        if np.isnan(theta): theta=0.
        if np.isnan(phi): phi=0.
        return theta,phi

    def exposure(self, ra, dec, FoV=False, alt=-10., fov=90., index=0.77):
        locdb = "Test,f|V,{},{},21.26,2000".format(deg2HMS(ra),deg2DMS(dec))
        test_point = eph.readdb(locdb)
        test_point.compute(self.obs)
        if test_point.alt < np.radians(alt):
            return 0.0
        else:
            if FoV:
                return 1.0
            else:
                sep = eph.separation((self.azimuth,self.altitude),(test_point.az,test_point.alt))
#                if sep > np.pi/2.:
                if sep > np.radians(fov):
                    return 0.0
                else:
                    return np.cos(sep)**index

    def throw_grb(self,grb):
        self._grb = grb
        self._separation()
        self._response()
        self._significance()

        return {'name': self.name, 
                'T0': self.trigger_time*self._grb.binz - self._grb.window/2.,
                'counts': self.triggered_counts, 
                'counts_err': self.triggered_counts_error,
                'ra': self._grb.eph._ra, 
                'dec': self._grb.eph._dec}


