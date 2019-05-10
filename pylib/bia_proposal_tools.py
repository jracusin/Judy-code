import numpy as np
import matplotlib.pylab as plot
from astropy.io import ascii,fits
from scipy import interpolate
from BurstCube.LocSim.Detector import *
from BurstCube.LocSim.Spacecraft import *
from astropy.coordinates import SkyCoord
from astropy import units as u
from scipy.optimize import curve_fit
from astropy.table import Table
import healpy as hp
from pylab import cm
import matplotlib.colors as mpl_col

def plot_exposures(pointings,Aeff_fact,index=1,lat='00:00:00',lon='260:00:00',Earth=True,antiEarth=False,NSIDE=32):
    npointings=len(pointings)
    sc = Spacecraft(pointings,lat=lat,lon=lon)
    exposure_positions_hp = np.arange(hp.nside2npix(NSIDE))
    exposure_positions_pix = hp.pix2ang(NSIDE, exposure_positions_hp, lonlat=True)
    exposure_positions = np.vstack(exposure_positions_pix)
    exposures = np.array([[ detector.exposure(position[0],position[1], alt=-90.,index=index) for position in exposure_positions.T] 
                          for detector in sc.detectors])

    exps=exposures.sum(axis=0)*Aeff_fact
    fs=exps#-min(gbm_exps))/max(gbm_exps)

    if Earth: 
        vec=hp.ang2vec(180,0,lonlat=True)
        i=hp.query_disc(NSIDE,vec,67*np.pi/180.)
        fs[i]=0
        exposures[:,i]=0
    if antiEarth:
        vec=hp.ang2vec(0,0,lonlat=True)
        i=hp.query_disc(NSIDE,vec,67*np.pi/180.)
        fs[i]=0
        exposures[:,i]=0

    plot.figure(figsize=(20,npointings))
    s=np.argsort(pointings.keys())
    for j in range(npointings):
        i=s[j]
        hp.mollview(exposures[i]/max(exposures[i])*Aeff_fact,title='Detector '+pointings.keys()[i],\
                    sub = [np.round(npointings/3.+0.5),3,int(str(j+1))])

    hp.mollview(fs,title='Sum of All Detectors')
#    plot.savefig(biadir+'exposure_maps_'+str(ang)+'.png')
    return sc,fs,exposure_positions,pointings,exposures

def num_detectors(sc,exposure_positions,pointings,antiEarth=False,NSIDE=32,Earth=True):

    npointings=len(pointings)
    ## evaluate detector overlap
    exposures = np.array([[ detector.exposure(position[0],position[1], alt=-23.,fov=60.,index=0) for position in exposure_positions.T] 
                          for detector in sc.detectors])

    plot.figure(figsize=(20,npointings))
    s=np.argsort(pointings.keys())
    for j in range(npointings):
        i=s[j]
        hp.mollview(exposures[i],title='Detector '+pointings.keys()[i],\
                    sub = [np.round(npointings/3.+0.5),3,int(str(j+1))])
    exps=exposures.sum(axis=0)
    #bia_fs=(exps-min(exps))/max(exps)
    fs_det=exps#-min(gbm_exps))/max(gbm_exps)

    cmap_skewed=colormap_skewed(exps)

    if Earth:
        vec=hp.ang2vec(180,0,lonlat=True)
        i=hp.query_disc(NSIDE,vec,67*np.pi/180.)
        fs_det[i]=0

    if antiEarth:
        vec=hp.ang2vec(0,0,lonlat=True)
        i=hp.query_disc(NSIDE,vec,67*np.pi/180.)
        fs_det[i]=0

    hp.mollview(fs_det,title='Overlap of Detectors',cmap=cmap_skewed)

    return fs_det

def num_detectors_frac(fs_det):
    ndet=int(np.max(fs_det))
    print ndet
    npix=float(len(fs_det))

    print('Fraction of sky seen by # of detectors:')
    for i in range(ndet):
        frac=float(len(np.where(fs_det==i)[0])/npix)
        print(str(i)+' '+str(frac))

def colormap_skewed(exps):
    vmin_skewed = -1.0
    vmid_skewed =  3.0
    vmax_skewed = np.max(exps)
    vstep = 1.0
    levels = np.arange(vmin_skewed, vmax_skewed+vstep, vstep)
    cmap = cm.get_cmap('bwr', len(levels)-1)
    deltamax = max(vmax_skewed-vmid_skewed, vmid_skewed-vmin_skewed)

    vfull = [ vmid_skewed-deltamax, vmid_skewed+deltamax]  # Full range either side of vmid
    #levfull = np.arange( vfull[0], vfull[1], vstep ) # Levels over full value range

    ncols = len(levels) -1 # number of colours we actually want to use

    vlo_frac = (vmin_skewed-vfull[0]) / (2.0*deltamax) # 0 or greater
    vhi_frac = (vmax_skewed-vfull[0]) / (2.0*deltamax) # 1 or less
    cmap_base = cm.get_cmap('bwr') # maps the range 0-1 to colours
    cols = cmap_base( np.linspace( vlo_frac, vhi_frac, ncols) )
    cmap_skewed = mpl_col.LinearSegmentedColormap.from_list('skewed',cols, N=ncols)
    cmap_skewed.set_bad('gray')
    cmap_skewed.set_under('w')

    return cmap_skewed

def thetaphi2radec(theta,phi):

    dec=-np.degrees(theta-np.pi/2.)
    ra=np.degrees(np.pi*2-phi)

    return ra,dec
    
def random_sky(n=1):

    u=np.random.rand(n)
    v=np.random.rand(n)

    phi=2*np.pi*u
    theta=np.arccos(2*v-1.)

    ra,dec=thetaphi2radec(theta,phi)

    if len(ra)==1:
        ra=ra[0]
        dec=dec[0]

    return ra,dec

def separation(ra1,dec1,ra2,dec2):

    rra1=np.radians(ra1)
    rra2=np.radians(ra2)
    rdec1=np.radians(dec1)
    rdec2=np.radians(dec2)

#    sep=np.degrees(np.sqrt(2-2.*np.cos(rra1-rra2)-2.*np.sin(rra1)*np.sin(rra2)*(np.cos(rdec1-rdec2)-1.)))
    sep=np.degrees(np.arccos(np.sin(rdec1)*np.sin(rdec2)+np.cos(rdec1)*np.cos(rdec2)*np.cos(rra2-rra1)))
 
    return sep