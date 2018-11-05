#!/usr/bin/env python

from BurstCube.LocSim.Detector import Detector
import numpy as np


class Spacecraft(object):

        '''The pointings are just key,item pairs of the pointing directions of
        the detectors on the spacecraft (respect to 0,0 at zenith).
        The default are 12 detectors pointing every 30 degrees in
        azimuth and 15 degrees in zenith (offset a bit).'''

        def __init__(self, pointings={'01': ('0:0:0', '5:0:0'),
                                      '02': ('30:0:0', '20:0:0'),
                                      '03': ('60:0:0', '35:0:0'),
                                      '04': ('90:0:0', '10:0:0'),
                                      '05': ('120:0:0', '25:0:0'),
                                      '06': ('150:0:0', '40:0:0'),
                                      '07': ('180:0:0', '15:0:0'),
                                      '08': ('210:0:0', '30:0:0'),
                                      '09': ('240:0:0', '45:0:0'),
                                      '10': ('270:0:0', '20:0:0'),
                                      '11': ('300:0:0', '35:0:0'),
                                      '12': ('330:0:0', '50:0:0')},
                     lat="37:56:24.7",
                     lon="75:27:59",
                     elev=550e3,
                     window=4.,
                     background_rate=10.,
                     noise=True):

                self.lon = lon
                self.lat = lat
                self.elev = elev
                self.pointings = pointings
                self.detectors = [Detector(
                        name=pointing,
                        azimuth_angle=pointings[pointing][0],
                        zenith_angle=pointings[pointing][1],
                        noise=noise,
                        background_rate=background_rate,
                        window=window,
                        lon=self.lon,
                        lat=self.lat,
                        elev=self.elev) for pointing in pointings]
                self._dummyDet = Detector(name='dummy',
                                          lon=self.lon,
                                          lat=self.lat,
                                          elev=self.elev)

        @property
        def detnum(self):
                return len(self.detectors)

        def throw_grb(self, grb):

                return [detector.throw_grb(grb) for detector in self.detectors]

        def throw_grbs(self, grbs, scaled=False, save=False,
                       filename='grbs.pkl'):

                grb_sample = [[detector.throw_grb(grb)
                               for detector in self.detectors]
                              for grb in grbs]
                grb_rec = np.zeros((len(grb_sample),
                                    self.detnum), dtype=float)
                grb_rec_err = np.zeros((len(grb_sample),
                                        self.detnum), dtype=float)

                [[grb_rec.itemset((i, int(det['name']) - 1), det['counts'])
                  for det in sample]
                 for i, sample in enumerate(grb_sample)]
                [[grb_rec_err.itemset((i, int(det['name']) - 1),
                                      det['counts_err'])
                  for det in sample]
                 for i, sample in enumerate(grb_sample)]

                grb_rec_err[grb_rec < 0.] = 0.
                grb_rec[grb_rec < 0.] = 0.

                if scaled:
                        # mask = grb_rec > 0.
                        scaled_rec = np.array([rec/rec.max()
                                               for rec in grb_rec])

                        # Here we set the error to just be the
                        # scaled error on the max if the
                        # measurement is 0 so we can get around
                        # the divide by zero in the
                        # error propagation.

                        np.seterr(divide='ignore')

                        scaled_rec_err = scaled_rec*[np.sqrt(
                                (np.nan_to_num(np.divide(
                                        grb_rec_err[idx], rec)))**2 +
                                (np.nan_to_num(np.divide(
                                        grb_rec_err[idx][rec.argmax()],
                                        rec.max())))**2)
                                                     for idx, rec in enumerate(
                                                                     grb_rec)]
                        # grb_rec = np.round(100.*scaled_rec).astype('int32')
                        # grb_rec_err =
                        # np.round(100.*scaled_rec_err).astype('int32')
                        grb_rec = 100.*scaled_rec
                        grb_rec_err = 100.*scaled_rec_err

                return np.array(list(zip(grb_rec, grb_rec_err)))
