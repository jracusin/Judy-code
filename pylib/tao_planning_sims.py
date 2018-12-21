#!/usr/bin/env python
#!/usr/bin/env python
"""
------------------------------------------------------------------------

Simulating TAO Observing Strategy

------------------------------------------------------------------------
"""

import numpy as np
import matplotlib.pylab as plot
from astropy.io import ascii
from astropy.io import fits
from astropy.cosmology import WMAP9 as cosmo
from scipy import interpolate
import math
import judy_astropy as jap
from matplotlib.ticker import MultipleLocator
from astropy.coordinates import SkyCoord
from astropy import units as u
from astropy.table import Table
from astropy.time import Time,TimeDelta
import grb_catalogs
import fit_lc
import fit_functions
import matplotlib.cm as cm
import healpy as hp
import astropy.coordinates as coord
import os.path
import glob
from matplotlib.ticker import FuncFormatter

#old method
# define FoR (half of sky - solar panel which moves)
# estimate when rest of sky becomes visible (use RA?)
# calc distance between random viewing position, and random new source position (assumes single tile?)
# for some slew speed + settling time, calc delay time
# assume GTM triggers or GW triggers
# calc flux of sGRB sample at delay time
# calc detection fraction for some exposure time (60 s?)
# do this many times and build stats to compare diff slew speeds

#new method 12/22/17
# mission config file, make object or dict - done
# need ephemeris over some time period - from gtorbsim - done
# need LV sky maps with real GRB position, and prob map
# define tiling strategy accounting for prob and delay time, with rotation?
# define exposure pattern, split tiles by window?
# random starting position in non-ram direction
# trigger at random time, begin follow-up
# output exposure time, time since trigger, ra/dec, detx/dety

def likelihood_counterparts():
	### numbers from Frank integrating over the distribution of merger rates for various sims
	### should figure out how to do this myself
	dir='/Users/jracusin/TAO/simulations/'

	### 18 month mission
	threshold=ascii.read(dir+'tao_events_thresh_18.out')
	req=ascii.read(dir+'tao_events_req_18.out')
	cbe=ascii.read(dir+'tao_events_cbe_18.out')

	plot.figure()
	plot.bar(threshold['m'],threshold['sum0'],label='Threshold Mission')
	plot.bar(req['m'],req['sum0'],label='Baseline Requirements',alpha=0.7)
	plot.bar(cbe['m'],cbe['sum0'],label='Performance/CBE',alpha=0.5)
	plot.plot(threshold['m'],threshold['sum0'],color='C0',label='Threshold Mission')
	plot.plot(req['m'],req['sum0'],color='C1',label='Baseline Requirements')
	plot.plot(cbe['m'],cbe['sum0'],color='C2',label='Performance/CBE')
	plot.xlabel('Number of WFI-detected GW Counterparts')
	plot.ylabel('Probability of Detecting N Counterparts')
	plot.xlim([0,30])
	plot.legend()
	plot.savefig('tao_events_likelihood_18_N.png')
#	plot.show()

	plot.figure()
	plot.bar(threshold['m'],threshold['sum_p'],label='Threshold Mission')
	plot.bar(req['m'],req['sum_p'],label='Baseline Requirements',alpha=0.7)
	plot.bar(cbe['m'],cbe['sum_p'],label='Performance/CBE',alpha=0.5)
	plot.plot(threshold['m'],threshold['sum_p'],color='C0',label='Threshold Mission')
	plot.plot(req['m'],req['sum_p'],color='C1',label='Baseline Requirements')
	plot.plot(cbe['m'],cbe['sum_p'],color='C2',label='Performance/CBE')
	plot.xlabel('Number of WFI-detected GW Counterparts')
	plot.ylabel(r'Probability of Detecting $\leq$N Counterparts')
	plot.xlim([0,30])
	plot.legend()
	plot.savefig('tao_events_likelihood_18_lessN.png')
#	plot.show()



def back_of_the_envelope(bh=False,grb=False):

	rate=1540.
	rate_low=1540.-1220
	rate_high=1540+3200.

	ligo_range=0.19 # Gpc
	virgo_range=0.125 # Gpc

	if bh==True:
		rate=3600.
		rate_low=0.
		rate_high=3600.
		ligo_range=0.294#0.19 # Gpc
		virgo_range=0.125/0.19*ligo_range#0.125 # Gpc

	if grb==True:
		rate=10.
		rate_low=5.
		rate_high=15.
		ligo_range=0.355#294
		virgo_range=0.125/0.19*ligo_range#0.125 # Gpc

	range=np.array([rate_low/rate,rate_high/rate])


	ligo_vol=4./3.*np.pi*ligo_range**3
	virgo_vol=4./3.*np.pi*virgo_range**3

	det3, det2, det1, det0 = duty_cycle(0.7)
	det2i=det2/3.

	opening_angle1=16.
	beaming_fraction1=1.-np.cos(np.radians(opening_angle1))

	opening_angle2=19.
	beaming_fraction2=1.-np.cos(np.radians(opening_angle2))

	if grb==True:
		beaming_fraction1=1.
		beaming_fraction2=0.

	jet_boost=1.5**3
	sub_thresh_boost=1.5**3-1.

	effic_snr12=0.5
	effic_snr8=0.15

	rate_snr12_angle1=rate*(ligo_vol*(det3+det2i)+virgo_vol*(det2i*2))*beaming_fraction1*jet_boost*effic_snr12
	rate_snr12_angle2=rate*(ligo_vol*(det3+det2i)+virgo_vol*(det2i*2))*beaming_fraction2*jet_boost*effic_snr12

	rate_snr8_angle1=rate*(ligo_vol*(det3+det2i)+virgo_vol*(det2i*2))*beaming_fraction1*jet_boost*sub_thresh_boost*effic_snr8
	rate_snr8_angle2=rate*(ligo_vol*(det3+det2i)+virgo_vol*(det2i*2))*beaming_fraction2*jet_boost*sub_thresh_boost*effic_snr8

	# rate_snr12_angle1=rate*(ligo_vol*(det3+det2i))*beaming_fraction1*jet_boost*effic_snr12
	# rate_snr12_angle2=rate*(ligo_vol*(det3+det2i))*beaming_fraction2*jet_boost*effic_snr12

	# rate_snr8_angle1=rate*(ligo_vol*(det3+det2i))*beaming_fraction1*jet_boost*sub_thresh_boost*effic_snr8
	# rate_snr8_angle2=rate*(ligo_vol*(det3+det2i))*beaming_fraction2*jet_boost*sub_thresh_boost*effic_snr8


	range_snr12_angle1=rate_snr12_angle1*range
	range_snr12_angle2=rate_snr12_angle2*range
	range_snr8_angle1=rate_snr8_angle1*range
	range_snr8_angle2=rate_snr8_angle2*range

	rate_angle1=rate_snr12_angle1+rate_snr8_angle1
	range_angle1=rate_angle1*range
	rate_angle2=rate_snr12_angle2+rate_snr8_angle2
	range_angle2=rate_angle2*range

	print('Back-of-the-Envelope:')
	print('Rate (SNR>12,16 deg opening angle): ',rate_snr12_angle1,range_snr12_angle1)
	print('Rate (SNR>8,16 deg opening angle): ',rate_snr8_angle1,range_snr8_angle1)
	print('Total Rate (16 deg opening angle): ',rate_angle1,range_angle1)
	print('Rate (SNR>12,19 deg opening angle): ',rate_snr12_angle2,range_snr12_angle2)
	print('Rate (SNR>8,19 deg opening angle): ',rate_snr8_angle2,range_snr8_angle2)
	print('Total Rate (19 deg opening angle): ',rate_angle2,range_angle2)


def sgrb_properties():

	oldgrbdir='/Users/jracusin/GRBs/'
	grbdir='/Users/jracusin/Swift/GRBfits/GRBs/'

	grbox=grb_catalogs.load_GRBOX(nointernet=True)

	z=np.zeros(len(grbox))
	mask=grbox['z'].mask
	for i in range(len(grbox)):
		if (mask[i]==False) and (grbox['z'][i] != 'low'):
			z[i]=float(grbox['z'][i])

	bat=grb_catalogs.load_BAT()
	w=np.where(bat['T90']!='N/A')
	bat=bat[w]
	t90=np.array(bat['T90']).astype('float')

	# replace some T90's with those in Fong et al. 2015 (short with long soft tails)
	lsgrbs=np.array(['GRB050724','GRB061006','GRB061210','GRB070714B','GRB070729','GRB071227','GRB090510'])
	lst90=np.array([1.99,0.4,0.2,1.99,0.9,1.8,0.3])
	m1,m2=grb_catalogs.match_catalogs_name(bat['GRBname'],lsgrbs)
	t90[m1]=lst90[m2]

	m1,m2=grb_catalogs.match_catalogs_name(np.core.defchararray.add('GRB',grbox['GRB']),bat['GRBname'])
	s=np.where((t90[m2] <= 2.) & (t90[m2] > 0))# & (z[m1]>0))
	sGRBs=bat['GRBname'][m2][s]
	print '# of sGRBs = ',len(s[0])
	t90=t90[m2][s]
	z=z[m1][s]

	nh=[]
	nhgal=[]
	nhz=[]
	gamma=[]

	for i in range(len(sGRBs)):

		if (os.path.exists(grbdir+sGRBs[i]+'/lc_fit_out_py_int1.dat') & os.path.exists(oldgrbdir+sGRBs[i])\
		& os.path.exists(oldgrbdir+sGRBs[i]+'') & (sGRBs[i] != 'GRB100628A') & (sGRBs[i] != 'GRB140311B')):
			p=fit_lc.read_lcfit(dir=grbdir+sGRBs[i]+'/')
			sp=fit_lc.read_specfit(dir=oldgrbdir+sGRBs[i]+'/')
			if (p != {}) & (sp != {}):
				if 'PC' in sp.keys(): spec=sp['PC'] 
				else: spec=sp['WT']

				nh.append(spec.nh)
				nhgal.append(spec.galnh)
				nhz.append(spec.z_abs)
				gamma.append(spec.gamma)

	nh=np.array(nh)*1e22
	nhgal=np.array(nhgal)*1e22
	nhz=np.array(nhz)
	gamma=np.array(gamma)

	#k=kcorr(0.3,10.0,0.3,10.0,gamma,nhgal,nh,nhz,0.)
# 	eng=np.linspace(0.3,10,100)
# 	for i in range(len(nh)):
# 		# k1=np.sum(jap.wabswabsPL(eng,nhgal[i],nh[i],[1.,gamma[i]],z=nhz[i]))
# 		# k2=np.sum(jap.wabswabsPL(eng,nhgal[i],nh[i],[1.,gamma[i]],z=0.))
# 		k1=np.sum(jap.wabs(eng,nh[i],z=nhz[i]))
# #		k=k2/k1
# 		print k,nhz[i],nh[i]

#	plot.figure()


	m=np.median(nh)
	print 'median intrinsic NH = ',m
	print 'median Galactic NH = ',np.median(nhgal)
	print 'median combined NH = ',np.median(nhgal+nh)
	print 'median gamma = ',np.median(gamma)

	plot.figure()
	r=np.array([8,24])
	plot.hist((nh+nhgal),label='Total N$_H$',bins=np.logspace(r[0],r[1],(r[1]-r[0])*2.+1))
	plot.hist(nh,label='Intrinsic N$_H$',bins=np.logspace(r[0],r[1],(r[1]-r[0])*2.+1),alpha=0.7)
	plot.hist(nhgal,label=r'Galactic N$_H$',bins=np.logspace(r[0],r[1],(r[1]-r[0])*2.+1),alpha=0.7)
	plot.plot([5e20,5e20],[0,25],linestyle='-',color='black',label=r'5x10$^{20}$ cm$^{-2}$')
	plot.plot([10**21.7,10**21.7],[0,25],linestyle='--',color='black',label='Campana et al. (2012)')
	plot.plot([m,m],[0,25],linestyle=':',color='black',label='Median Galactic+Intrinsic (this sample)')
	plot.xlabel(r'N$_H$ (cm$^{-2}$)')
	plot.ylabel('N')
	plot.xscale('log')
	plot.legend(loc=2)
	plot.xlim([1e10,1e24])
	plot.ylim([0,25])
	plot.savefig('sGRB_absorption.png')
	plot.show()

	return nh

def duty_cycle(singleGW=0.9):

	det3=singleGW**3
	det2=3*singleGW**2*(1-singleGW)
	det1=3*singleGW*(1-singleGW)**2#(1-det3-det2)
	det0=(1-singleGW)**3
	total=det0+det1+det2+det3

	print '3 detector frac = ',det3
	print '2 detector frac = ',det2
	print '1 detector frac = ',det1
	print '0 detector frac = ',det0
	print 'total = ',total

	return det3, det2, det1, det0

def gwarea_plot(dir='/Users/jracusin/TAO/simulations/',taoconfig=None,configfile='tao_config_v1.txt'):


	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

#	file=dir+'/simsfromLeo/tao_study_duty90_20deg/stats.txt'
	file=dir+'simsfromLeo/tap-study/stats.txt'
	stats=ascii.read(file)

	area=stats['area(90)']

	# ndet=[]
	# for i in range(387):
	# 	datafile=dir+str(i)+'.nside32.fits.gz'
	# 	theta,phi,m,hdr=read_healpix_map(datafile)
	# 	ngwdet=len(hdr['INSTRUME'].split(','))
	# 	ndet.append(ngwdet)

	# ndet=np.array(ndet)

	sims=read_sims(configfile=configfile)
	m1,m2=grb_catalogs.match_catalogs_name(stats['id'],sims['map'])
	m1=np.array(m1)
	m2=np.array(m2)
	w12=np.where((sims['snr'][m2]>=12))[0]
	w8=np.where((sims['snr'][m2] < 12 ) & (sims['snr'][m2]>=8))[0]

#	w=np.where(area <5000)
	area=np.array(area)

	plot.figure()
	# plot.hist(area[m1[w12]],bins=np.logspace(0,4,50),label='SNR>12')
	# plot.hist(area[m1[w8]],bins=np.logspace(0,4,50),label='8<SNR<12',alpha=0.7)
	plot.hist(area[m1[w12]],bins=np.linspace(1,1e4,50),label='SNR>12')
	plot.hist(area[m1[w8]],bins=np.linspace(1,1e4,50),label='8<SNR<12',alpha=0.7)
	plot.legend(fontsize=14)

	plot.xlabel(r'90% Enclosed Probability (deg$^2$)',fontsize=14)
	plot.ylabel('Number of Simulated GW Events',fontsize=14)
	plot.xlim([1,1e4])
#	plot.xscale('log')
	ax = plot.gca()
	ax.tick_params(axis = 'both', which = 'major', labelsize = 14)
	ax.tick_params(axis = 'both', which = 'minor', labelsize = 14)
	plot.tight_layout()

	plot.savefig(dir+'Enclosed_GW_Area.png')
	plot.show()


def pretty_plots(taoconfig=None,configfile='tao_config_v23.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	c=', '

	simdir=taoconfig['simdir']
	detsize=str(taoconfig['detsize'])
	allsimnums,allcenters,allrolls,allprobs,alltileperc=read_tiles(tilefile='tiles_wfi'+str(taoconfig['detsize'])+'_roll10.txt',dir='/Users/jracusin/TAO/simulations/')

	for i in range(2,100):
		ws=np.where(allsimnums==i)[0]
		ntiles=len(ws)
		if ntiles == 3:
			datafile=simdir+str(i)+'.fits.gz'
			print datafile
			centers=allcenters[ws]
			rolls=allrolls[ws]
			probs=allprobs[ws]
			tileperc=alltileperc[ws[0]]
			theta,phi,m,hdr=read_healpix_map(datafile)
			hp.mollview(m)
			plot_tiles(m,centers,rolls,doplot=False,taoconfig=taoconfig)
			plot.show()


def shb_plot(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	sims=read_sims(configfile=configfile)
	w=np.where(sims['exposure']>0)
	w12=np.where((sims['snr']>=12) & (sims['exposure']>0))[0]
	w8=np.where((sims['snr'] < 12 ) & (sims['snr']>=8) & (sims['exposure']>0))[0]
	w3=np.where(sims['ngwdet']==2)
	w2=np.where(sims['ngwdet']==3)
	sims=sims[w]
#	n=50
#	r=np.round(np.random.rand(n)*n).astype('int')
#	sims=sims[r]

	nsims=len(sims)

	grbox=grb_catalogs.load_GRBOX(nointernet=True)

	z=np.zeros(len(grbox))
	mask=grbox['z'].mask
	for i in range(len(grbox)):
		if (mask[i]==False) and (grbox['z'][i] != 'low'):
			z[i]=float(grbox['z'][i])

	bat=grb_catalogs.load_BAT()
#	bat2=ascii.read('/Users/jracusin/Swift/BATCAT/bat_grb_peakeneflux.txt',delimiter=' ')

	w=np.where(bat['T90']!='N/A')
	bat=bat[w]
	t90=np.array(bat['T90']).astype('float')

	# replace some T90's with those in Fong et al. 2015 (short with long soft tails)
	lsgrbs=np.array(['GRB050724','GRB061006','GRB061210','GRB070714B','GRB070729','GRB071227','GRB090510'])
	lst90=np.array([1.99,0.4,0.2,1.99,0.9,1.8,0.3])
	m1,m2=grb_catalogs.match_catalogs_name(bat['GRBname'],lsgrbs)
	t90[m1]=lst90[m2]

	m1,m2=grb_catalogs.match_catalogs_name(np.core.defchararray.add('GRB',grbox['GRB']),bat['GRBname'])
	s=np.where((t90[m2] <= 2.) & (t90[m2] > 0))# & (z[m1]>0))
	sGRBs=bat['GRBname'][m2][s]

	# ra=np.array(bat['RA_ground'][m2][s])
	# dec=np.array(bat['DEC_ground'][m2][s])
	# lat=[]
	# for i in range(len(ra)):
	# 	c = SkyCoord(ra=float(ra[i])*u.degree, dec=float(dec[i])*u.degree)
	# 	lat.append(c.galactic.b.value)
	# plot.figure()
	# plot.hist(lat,bins=36)
	# plot.xlabel('Galactic Latitude (deg)')
	# plot.savefig('sGRB_gal_lat.png')
	# plot.show()

	print '# of sGRBs = ',len(s[0])

	# D'Avanzo (2014) flux limited sample
	dagrbs=np.core.defchararray.add('GRB',np.array(['051221A','060313','061201','070714B',\
		'080123','080503','080905A','090426','090510','090515','100117A','100625A','101219A','111117A','130515A','130603B']))

	t90=t90[m2][s]
	z=z[m1][s]
	w0=np.where(z==0.)[0]
#	z[w0]=0.5
	lumdist=cosmo.luminosity_distance(z).value
	pc2cm=3.08568025e18	
	dist=lumdist*1e6*pc2cm
#	distgw=440e6*pc2cm  ## 440 Mpc
	distgw=sims['distance']*1e6*pc2cm
	zgw=grb_catalogs.dist2z(sims['distance'])
#	zgw=np.repeat(0.03,len(sims))
#	zgw=np.repeat(0.045,len(sims))

	grbdir='/Users/jracusin/Swift/GRBfits/GRBs/'
	oldgrbdir='/Users/jracusin/GRBs/'

	eng=np.linspace(0.3,10.,100)
	de=eng[1]-eng[0]
	wtao=np.where((eng >=taoconfig['WFI_E_low']) & (eng <= taoconfig['WFI_E_high']))

	det=np.zeros(nsims)
	det[:]=-1
	detfrac=[]
	ngrbs=len(sGRBs)

	### need to go through each sim and randomly grab a GRB LC

	plot.figure()

#	i=0
	f10=wfi_sensitivity(10.,sgrb=True,taoconfig=taoconfig)
	f500=wfi_sensitivity(500.,sgrb=True,taoconfig=taoconfig)
	f2000=wfi_sensitivity(2000,sgrb=True,taoconfig=taoconfig)

	fscaled_10=[]
	fscaled_500=[]
	fscaled_2000=[]
	fscaled_10a=[]
	fscaled_500a=[]
	fscaled_2000a=[]

	rr=np.random.randint(0,high=nsims,size=ngrbs)
#	print rr

	first1=True
	first2=True
	for i in range(ngrbs):
		p={}
		sp={}
		if (os.path.exists(grbdir+sGRBs[i]+'/lc_fit_out_py_int1.dat') & os.path.exists(oldgrbdir+sGRBs[i])\
			& os.path.exists(oldgrbdir+sGRBs[i]+'') & (sGRBs[i] != 'GRB100628A') & (sGRBs[i] != 'GRB140311B')):
			p=fit_lc.read_lcfit(dir=grbdir+sGRBs[i]+'/')
			sp=fit_lc.read_specfit(dir=oldgrbdir+sGRBs[i]+'/')
			if (p != {}) & (sp != {}):

				r=rr[i]#int(np.random.uniform()*nsims)
				linestyle='-'

				if sims['snr'][r]<12: linestyle=':'

			# scale by distance, and k-correct from 0.3-10 keV to 0.4-4.0 keV
				if 'PC' in sp.keys(): spec=sp['PC'] 
				else: spec=sp['WT']
				lc=fit_lc.read_lc('/Users/jracusin/Swift/GRBfits/GRBs/'+sGRBs[i]+'/')
				conv=np.sum(fit_functions.pow(eng[wtao],*[1.,spec.gamma])*de)/np.sum(fit_functions.pow(eng,*[1.,spec.gamma])*de)
				k1=grb_catalogs.kcorr(0.3,10.,taoconfig['WFI_E_low'],taoconfig['WFI_E_high'],-spec.gamma,z[i])
				k2=grb_catalogs.kcorr(taoconfig['WFI_E_low'],taoconfig['WFI_E_high'],taoconfig['WFI_E_low'],taoconfig['WFI_E_high'],-spec.gamma,zgw[r])

				conv=spec.flux/spec.rate*conv*dist[i]**2/distgw[r]**2/k1*k2  #distgw from map

				# flux of this GRB at time of obs
		#		tbin=np.array([sims['nettstart'][r]+sims['netExposure'][r]/2.])
#				t=np.logspace(np.log10(sims['nettstart'][r]),5,100)
				t=np.logspace(min(np.log10(lc['Time'])),max(np.log10(lc['T_+ve'])),100)
				fscaled=fit_functions.call_function(p.model,t/(1.+z[i])*(1.+zgw[r]),*p.par)*conv

				color='black'
				if sGRBs[i] in dagrbs: 
#					color='blue'
					fscaled_10.append(loginterpol(t,fscaled,150.))
					fscaled_500.append(loginterpol(t,fscaled,150.+250.))
					fscaled_2000.append(loginterpol(t,fscaled,150.+1000.))
					fscaled_10a.append(loginterpol(t,fscaled,150.+45.*60))
					fscaled_500a.append(loginterpol(t,fscaled,150.+250.+45.*60))
					fscaled_2000a.append(loginterpol(t,fscaled,150.+1000.+45.*60))


				if ((first1==True)) | ((first2==True)):
					if ((first1==True) & (linestyle=='-')):
						plot.plot(t,fscaled,color=color,linestyle=linestyle,label=r'$SNR_{GW}>12$')
						first1=False
					if ((first2==True) & (linestyle==':')):
						plot.plot(t,fscaled,color=color,linestyle=linestyle,label=r'$8<SNR_{GW}<12$')
						first2=False
				else:
					plot.plot(t,fscaled,color=color,linestyle=linestyle)

	a10=len(np.where(fscaled_10>f10)[0])*1./len(fscaled_10)
	a500=len(np.where(fscaled_500>f500)[0])*1./len(fscaled_500)
	a2000=len(np.where(fscaled_2000>f2000)[0])*1./len(fscaled_2000)
	a10a=len(np.where(fscaled_10a>f10)[0])*1./len(fscaled_10a)
	a500a=len(np.where(fscaled_500a>f500)[0])*1./len(fscaled_500a)
	a2000a=len(np.where(fscaled_2000a>f2000)[0])*1./len(fscaled_2000a)

	print a10,a500,a2000,a10a,a500a,a2000a
	print (a10*0.5+a10a*0.5),(a500*0.5+a500a*0.5),(a2000*0.5+a2000a*0.5)

	yrange=[1e-13,1e-5]
	plot.plot([15,15],yrange,color='darksalmon',linewidth=3)
	plot.annotate('Start of WFI Scan from GTM Trigger',xy=(18,3e-6),color='darksalmon',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	plot.plot([120,120],[1e-13,1.5e-6],color='steelblue',linewidth=3)
	plot.annotate('GW Skymap Available',xy=(150,6e-7),color='steelblue',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	s=np.sort(sims['nettstart'])
	m=max(s[0:-10])
#	m=max(sims['nettstart'])
	plot.plot([m,m],[1e-13,2e-7],color='lightslategrey',linewidth=3)
	plot.annotate('Max Observation Start',xy=(m+1e3,0.9e-7),color='lightslategrey',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	tao=ascii.read('/Users/jracusin/TAO/simulations/Ptak/tau_flux_limits_2018_prob1e-10.csv')
	c=248.
	g=3.
	R90= c*(tao['cntslim9']**2/(tao['cntslim9']+g*tao['bgd9']))**(-0.5)
  	tp=[10.,500.,2000.]
  	r90p=loginterpol(tao['expt'],R90,tp)/60.
  	print r90p

	plot.plot([10,1e6],[f10,f10],color='red',lw=2,linestyle='--')
	plot.annotate(r"$R_{90}$<1.7' 10 s Sensitivity",xy=(1e4,1.8*f10),color='red',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	plot.plot([500,1e6],[f500,f500],color='red',lw=2,linestyle='--')
	plot.annotate(r"$R_{90}$<1.3' 500 s Sensitivity",xy=(1e4,1.8*f500),color='red',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	plot.plot([2000,1e6],[f2000,f2000],color='red',lw=2,linestyle='--')
	plot.annotate(r"$R_{90}$<1.2' 2000 s Sensitivity",xy=(1e4,0.38*f2000),color='red',bbox=dict(boxstyle="round", fc="w",alpha=0.8,color='w'))

	plot.legend()
	plot.xscale('log')
	plot.yscale('log')
	plot.xlabel('Time (s)')
	plot.ylim([1e-13,1e-5])
	plot.xlim([10,1e6])
	plot.ylabel(r'0.4 - 4 keV Flux (erg cm$^{-2}$ s$^{-1}$)')
	ax=plot.gca()
	ax.get_yaxis().set_tick_params(direction='in',which='both')
	ax.get_xaxis().set_tick_params(direction='in',which='both')
	plot.title('sGRB X-ray afterglows scaled to GW simulated distances')
#	plot.title('sGRB X-ray afterglows scaled to 130 Mpc')
#	plot.title('sGRB X-ray afterglows scaled to 200 Mpc')
	plot.savefig('shb_plot.pdf')
	plot.savefig('shb_plot.eps')
	plot.savefig('shb_plot.png')
	plot.show()

	return #z

def write_results_table():
	versions=np.arange(23,28)#17)
#	versions=np.arange(22,23)
	versions=np.append(np.array([23,23]),versions)
	versions=np.append(versions,np.arange(14,19))
	simtext=np.array(['All Sky (maximum)',\
		# 'Nominal','0.5xSensitivity','2xSensitivity','Slew Speed 2 deg/s',\
		# 'WFI FoV 16.1x16.1 deg','GTM FoV 1.4 pi','Nominal - 500s Exposure',\
		# 'Slew Speed 2 deg/s - 500s Exposure',\
		# 'WFI FoV 16.1x16.1 deg - 500s Exposure','GTM FoV 1.4 pi - 500s Exposure',\
		# 'SVOM/CALET','Fermi-GBM','Swift-BAT','INTEGRAL SPI/ACS','BurstCube','WFI FoV 12.4x12.5 deg',\
		# 'No Solar Panel','No ISS Comm Delay','No solar panel & no ISS Comm Delay',\
		# '30 deg Sun Constraint','10 deg Moon Constraint','30/10 Sun/Moon',\
		'Requirements Nominal',\
		'Requirements 0.5xSensitivity','Requirements 2xSensitivity',\
		'Requirements Slew Speed 2 deg/s','Requirement WFI FoV x 0.5',\
		'Requirements GTM FoV 1.4pi sr','Requirements 500 s exposure',\
		'SVOM/CALET','Fermi-GBM','Swift-BAT','INTEGRAL SPI/ACS','BurstCube'])
#	simtext=simtext[versions]
	print versions

	outf=open('sim_table.txt','w+')
	outf.write('Simulation, Detection Fraction, Detection Fraction (SNR>12), '+ \
		'Detection Fraction (8<SNR<12), Number of Detections, Number of Detections (SNR>12), '+\
		'Number of Detections (8<SNR<12) \n')

	outf.close()

	gwrate=1540.
	lowgwrate=1540.-1220.
	highgwrate=1540+3200.
	sp=', '
	j=0
	for i in range(len(versions)):
		simfile='tao_config_v'+str(versions[i])+'.txt'
		print simfile
		if os.path.exists(simfile):
			outf=open('sim_table.txt','a')
			sens=1.
			if j==1: sens=0.5
			if j==2: sens=2.0
			obsrate,obsrate12,obsrate8,detfrac,detfrac12,detfrac8=gwrates(configfile=simfile,sensitivity_factor=sens)
			obsrate_=str(np.round(obsrate,1))
			obsraterr_=' ['+str(np.round(obsrate/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate/gwrate*highgwrate,1))+']'
			obsrate12_=str(np.round(obsrate12,1))
			obsraterr12_=' ['+str(np.round(obsrate12/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate12/gwrate*highgwrate,1))+']'		
			obsrate8_=str(np.round(obsrate8,1))
			obsraterr8_=' ['+str(np.round(obsrate8/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate8/gwrate*highgwrate,1))+']'

			detfrac_=str(np.round(detfrac,2))
			detfrac12_=str(np.round(detfrac12,2))
			detfrac8_=str(np.round(detfrac8,2))

			detrate_=str(np.round(obsrate*detfrac,1))
			detraterr_=' ['+str(np.round(obsrate*detfrac/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate*detfrac/gwrate*highgwrate,1))+']'
			detrate12_=str(np.round(obsrate12*detfrac12,1))
			detraterr12_=' ['+str(np.round(obsrate12*detfrac12/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate12*detfrac12/gwrate*highgwrate,1))+']'
			detrate8_=str(np.round(obsrate8*detfrac8,1))
			detraterr8_=' ['+str(np.round(obsrate8*detfrac8/gwrate*lowgwrate,1))+'-'+\
				str(np.round(obsrate8*detfrac8/gwrate*highgwrate,1))+']'
			if j==0:
				outf.write(simtext[i]+sp+str(1)+sp+str(1)+sp+str(1)+sp+obsrate_+obsraterr_+sp+obsrate12_\
					+obsraterr12_+sp+obsrate8_+obsraterr8_+' \n')

			outf.write(simtext[i+1]+sp+detfrac_+sp+detfrac12_+sp+detfrac8_\
				+sp+detrate_+detraterr_+sp+detrate12_+detraterr12_+sp+detrate8_\
				+detraterr8_+' \n')
			j=j+1

	outf.close()


def sim_results(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)
	sims=read_sims(configfile=configfile)

	w=np.where((sims['exposure']>0) & (sims['nettstart']>0))
	w12=np.where((sims['snr']>=12) & (sims['exposure']>0) & (sims['nettstart']>0))[0]
	w8=np.where((sims['snr'] < 12 ) & (sims['snr']>=8) & (sims['exposure']>0) & (sims['nettstart']>0))[0]
	w3=np.where(sims['ngwdet']==2)
	w2=np.where(sims['ngwdet']==3)

	plotfile='nettstart_'+configfile.split('.txt')[0]+'.png'
#	fig, ax = plot.subplots()
	# hist=plot.hist(sims['nettstart'][w],bins=np.logspace(2,4,50),label='8<SNR<12',color='C1')
	# hist=plot.hist(sims['nettstart'][w12],bins=np.logspace(2,4,50),label='SNR>12',color='C0')
	xrange=[min(sims['nettstart']),max(sims['nettstart'])]#[0,7000]
	bins=np.linspace(xrange[0],xrange[1],25)
	binwidth=250.#(xrange[1]-xrange[0])/25
	hist1=plot.histogram(sims['nettstart'][w],bins=bins)#,label='8<SNR<12')#,color='C1')
	hist2=plot.histogram(sims['nettstart'][w12],bins=bins)#,label='SNR>12')#,color='C0')
	yscale=1./taoconfig['simrate']/2.

	plot.bar(hist1[1][0:-1],(hist1[0]+hist2[0])*yscale,binwidth,color='C1',label='8<SNR<12',align='center')
	plot.bar(hist2[1][0:-1],hist2[0]*yscale,binwidth,color='C0',label='SNR>12',align='center')

#	print 1./taoconfig['simrate']/2.
#	hist=plot.hist(sims['nettstart'][w8],bins=np.logspace(2,4,50),label='8<SNR<12',stacked=True,histtype='bar')
	# scale to get rate per year
# 	def obsrate(x,pos):
# 		x=x/taoconfig['simrate']/2.
# #		x=10**np.round(np.log10(x))
# 		return '{:.1f}'.format(x)
# 	formatter = FuncFormatter(obsrate)
#	ax = plot.gca()

	plot.xlim(xrange)
	plot.xlabel('Time Since Merger (s)',fontsize=14)
	plot.ylabel(r'Total Rate of X-ray Counterparts (yr$^{-1}$ bin$^{-1}$)',fontsize=14)
	plot.title('Start Times of First Tiles Containing GW Counterparts',fontsize=14)
	plot.legend(fontsize=14)
	ax = plot.gca()
	ax.tick_params(axis = 'both', which = 'major', labelsize = 14)
	ax.tick_params(axis = 'both', which = 'minor', labelsize = 14)
	plot.tight_layout()
#	plot.xscale('log')
#	plot.yscale('log')
#	ax.yaxis.set_major_formatter(formatter)

	plot.savefig(plotfile)
#	plot.show()
	plot.close()

	v='netExposure'
	plotfile=v+'_'+configfile.split('.txt')[0]+'.png'
	nbins=24.
	bins=np.linspace(100,2500,nbins)
	binwidth=(2500.-100)/nbins*1.1
#	hist=plot.hist(sims[v][w],bins=bins,label='All')
	hist1=plot.histogram(sims[v][w12],bins=bins)
	hist2=plot.histogram(sims[v][w8],bins=bins)
	yscale=1./taoconfig['simrate']/2.

	plot.bar(hist1[1][0:-1],(hist1[0]+hist2[0])*yscale,binwidth,color='C1',label='8<SNR<12',align='center')
	plot.bar(hist2[1][0:-1],hist2[0]*yscale,binwidth,color='C0',label='SNR>12',align='center')

	plot.xlabel('Net Exposure (s)',fontsize=14)
	plot.ylabel(r'Total Rate of X-ray Counterparts (yr$^{-1}$ bin$^{-1}$)',fontsize=14)
	plot.legend(fontsize=14)
#	plot.xscale('log')
#	plot.yscale('log')
	ax = plot.gca()
	ax.tick_params(axis = 'both', which = 'major', labelsize = 14)
	ax.tick_params(axis = 'both', which = 'minor', labelsize = 14)
	plot.tight_layout()

	plot.savefig(plotfile)
#	plot.show()
	plot.close()

	v='distance'
	nbins=30
	bins=np.linspace(0,600,nbins)
	binwidth=(600.-0)/nbins*1.1
	plotfile=v+'_'+configfile.split('.txt')[0]+'.png'
#	hist=plot.hist(sims[v][w],bins=bins,label='All')
	hist1=plot.histogram(sims[v][w8],bins=bins)#,label='8<SNR<12',alpha=0.7)
	hist2=plot.histogram(sims[v][w12],bins=bins)#,label='SNR>12',alpha=0.7)

	plot.bar(hist1[1][0:-1],(hist1[0]+hist2[0])*yscale,binwidth,color='C1',label='8<SNR<12',align='center')
	plot.bar(hist2[1][0:-1],hist2[0]*yscale,binwidth,color='C0',label='SNR>12',align='center')

	plot.xlabel('Distance (Mpc)',fontsize=14)
	plot.ylabel(r'Total Rate of X-ray Counterparts (yr$^{-1}$ bin$^{-1}$)',fontsize=14)
	plot.legend(fontsize=14)
#	plot.xscale('log')
#	plot.yscale('log')
	ax = plot.gca()
	ax.tick_params(axis = 'both', which = 'major', labelsize = 14)
	ax.tick_params(axis = 'both', which = 'minor', labelsize = 14)
	plot.tight_layout()

	plot.savefig(plotfile)
#	plot.show()
	plot.close()

	v='numtiles'
	plotfile=v+'_'+configfile.split('.txt')[0]+'.png'
	hist=plot.hist(sims[v][w],bins=np.arange(0,9)+0.5,density=True)#,label='All')
#	hist=plot.hist(sims[v][w8],bins=np.arange(0,9),label='8<SNR<12',alpha=0.7)
#	hist=plot.hist(sims[v][w12],bins=np.arange(0,9),label='SNR>12',alpha=0.7)
	plot.xlabel('Number of Tiles',fontsize=14)
	plot.ylabel('Fraction of Simulation',fontsize=14)
#	plot.legend()
#	plot.xscale('log')
#	plot.yscale('log')
	ax = plot.gca()
	ax.tick_params(axis = 'both', which = 'major', labelsize = 14)
	ax.tick_params(axis = 'both', which = 'minor', labelsize = 14)
	plot.tight_layout()

	plot.savefig(plotfile)
#	plot.show()
	plot.close()

def detstats(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',jet_angle=26.):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	detfrac,wdet,wdet12,wdet8=throw_grbs(taoconfig=taoconfig,configfile=configfile,dir=dir,jet_angle=jet_angle)

	sims=read_sims(taoconfig=taoconfig,configfile=configfile,dir=dir)

	w12=np.where(sims['snr']>=12)[0]
	w8=np.where((sims['snr'] < 12 ) & (sims['snr']>=8))[0]
	n12=float(len(w12))
	n8=float(len(w8))

	w12obs=np.where(sims['exposure'][w12]>0)[0]
	w8obs=np.where(sims['exposure'][w8]>0)[0]

	print 'Nsims (SNR>12) = ',len(w12)
	print 'Nobs (SNR>12) = ',len(w12obs),len(w12obs)/n12
	print 'Ndet (SNR>12) = ',len(wdet12),len(wdet12)/n12
	print 'Nsims (8<SNR<12) = ',len(w8)
	print 'Nobs (8<SNR<12) = ',len(w8obs),len(w8obs)/n8
	print 'Ndet (8<SNR<12) = ',len(wdet8),len(wdet8)/n8


def gwrates(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',sensitivity_factor=1.,jet_angle=16.):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	sims=read_sims(configfile=configfile,dir=dir)
	simrate=taoconfig['simrate']  # for latest sim with max of 26 deg opening angle
	nsims=float(len(sims))

#	simrate=simrate*sph_cap(taoconfig['jetangle'])/sph_cap(jet_angle)

	### rate from Abbott et al.
	gwrate=taoconfig['bns_rate']*1e-3 # Gpc-3 yr-1 to Mpc-3 Myr-1
	print('GW rate = ',gwrate,' Mpc-3 Myr-1')
	gwrate=gwrate*1e-6 # Myr->yr
	print('GW rate = ',gwrate,' Mpc-3 yr-1')

	## 200 Mpc (design range) * 1.5 (on-axis) * 1.5 (sub-threshold)
	gwrange=200.*1.5*1.5
	print('Range = ',gwrange,' Mpc')
	volume=4./3.*np.pi*(200.*1.5*1.5)**3 #Mpc3
	totrate=gwrate*volume #BNS yr-1 in volume
	print('All-sky all-orientation rate = ',totrate,' yr-1')

	## need to account for beaming
	allsky=4.*np.pi*(180./np.pi)**2.
#	scale=2*np.pi*taoconfig['jetangle']**2/allsky
	scale=sph_cap(taoconfig['jetangle'])*2.
	print('Scale by opening angle = ',scale)

	totrate=totrate*scale  # all sky rate
	print('All-sky pointed-at-us rate = ',totrate,' yr-1')

	oldrate=10*(1e-3)**3*volume
	print('Old all-sky rate = ',oldrate,' yr-1')

	simyr=2.
	w12=np.where(sims['snr']>=12)[0]
	w8=np.where((sims['snr'] < 12 ) & (sims['snr']>=8))[0]
	n12=float(len(w12))
	n8=float(len(w8))

	obsrate=nsims/simrate/simyr
	print('All-sky Leo rate = ',obsrate,' yr-1')

	obsrate=nsims/simrate/simyr*1.54
	print('All-sky Leo rate * astrophysical rate = ',obsrate,' yr-1')
	print('All-sky Leo rate * astrophysical rate range = ',obsrate/1540*(1540-1220.),obsrate/1540*(1540+3200.),' yr-1')

	obsrate12=n12/simrate/simyr*1.54
	print('All-sky Leo rate * astrophysical rate (SNR>12)= ',obsrate12,' yr-1')
	print('All-sky Leo rate * astrophysical rate range (SNR>12)  = ',obsrate12/1540*(1540-1220.),obsrate12/1540*(1540+3200.),' yr-1')

	obsrate8=n8/simrate/simyr*1.54
	print('All-sky Leo rate * astrophysical rate (8<SNR<12)= ',obsrate8,' yr-1')
	print('All-sky Leo rate * astrophysical rate range (8<SNR<12) = ',obsrate8/1540*(1540-1220.),obsrate8/1540*(1540+3200.),' yr-1')


	print('Sim breakdown (total, SNR>12, 8<SNR<12) = ',nsims,n12,n8)
	nwdet=[] 
	nwdet12=[]
	nwdet8=[]
	df=[]
	df12=[]
	df8=[]
	n=3
	for i in range(n):
		wdet,wdet12,wdet8,detfrac,detfrac12,detfrac8=throw_grbs(configfile=configfile,dir=dir,taoconfig=taoconfig,sensitivity_factor=sensitivity_factor,jet_angle=jet_angle)
		nwdet.append(len(wdet))
		nwdet12.append(len(wdet12))
		nwdet8.append(len(wdet8))
		df.append(detfrac)
		df12.append(detfrac12)
		df8.append(detfrac8)

	nwdet=np.mean(nwdet)
	nwdet12=np.mean(nwdet12)
	nwdet8=np.mean(nwdet8)
	detfrac=np.mean(df)
	detfrac12=np.mean(df12)
	detfrac8=np.mean(df8)

	print ('Mean efficiencies = ',detfrac,detfrac12,detfrac8)
	print('Detected numbers = ',nwdet,nwdet12,nwdet8)
	detfrac=nwdet/nsims*taoconfig['iss_uptime']
	print detfrac
	wfirate=obsrate*detfrac#*0.87
	print('GW-GRB rate = ',wfirate,' yr-1')
	print('GW-GRB rate range = ',wfirate/1540*(1540-1220.),wfirate/1540*(1540+3200.),' yr-1')

	detfrac12=nwdet12/n12*taoconfig['iss_uptime']
	print detfrac12
	wfirate12=obsrate12*detfrac12#*0.87
	print('GW-GRB rate (SNR>12) = ',wfirate12,' yr-1')
	print('GW-GRB rate range = (SNR>12) ',wfirate12/1540*(1540-1220.),wfirate12/1540*(1540+3200.),' yr-1')

	detfrac8=nwdet8/n8*taoconfig['iss_uptime']
	print detfrac8
	wfirate8=obsrate8*detfrac8#*0.87
	print('GW-GRB rate (8<SNR<12) = ',wfirate8,' yr-1')
	print('GW-GRB rate range (8<SNR<12) = ',wfirate8/1540*(1540-1220.),wfirate8/1540*(1540+3200.),' yr-1')

	print('GW-GRB rate (SNR>8) = ',wfirate+wfirate8,' yr-1')
	print('GW-GRB rate range (SNR>8) = ',(wfirate8+wfirate12)/1540*(1540-1220.),(wfirate8+wfirate12)/1540*(1540+3200.),' yr-1')

	return obsrate,obsrate12,obsrate8,detfrac,detfrac12,detfrac8

#	wfirate2=len(wdet)/taoconfig['simrate']/2.#*0.87
#	print('WFI rate (weighted)= ',wfirate2,' yr-1')
#	print('WFI rate errors (weighted) = ',wfirate2/1540*(1540-1220.),wfirate2/1540*(1540+3200.),' yr-1')


def sample():
	grbox=grb_catalogs.load_GRBOX(nointernet=True)
#	grbs=np.array(['111117A','100625A','100206A','100117A'])#,'080905A','070809','090515','070729','100117A'])
#	z=np.array([2.211,0.452,0.407,0.915])#,0.122,0.473,0.403,0.8,0.915]
#	m1,m2=grb_catalogs.match_catalogs_name(grbox['GRB'],grbs)
#	grbox['z'][m1]=z[m2]

	z=np.zeros(len(grbox))
	mask=grbox['z'].mask
	for i in range(len(grbox)):
		if (mask[i]==False) and (grbox['z'][i] != 'low'):
			z[i]=float(grbox['z'][i])

	bat=grb_catalogs.load_BAT()
	w=np.where(bat['T90']!='N/A')
	bat=bat[w]
	t90=np.array(bat['T90']).astype('float')

	# replace some T90's with those in Fong et al. 2015 (short with long soft tails)
	lsgrbs=np.array(['GRB050724','GRB061006','GRB061210','GRB070714B','GRB070729','GRB071227','GRB090510'])
	lst90=np.array([1.99,0.4,0.2,1.99,0.9,1.8,0.3])
	m1,m2=grb_catalogs.match_catalogs_name(bat['GRBname'],lsgrbs)
	t90[m1]=lst90[m2]

	m1,m2=grb_catalogs.match_catalogs_name(np.core.defchararray.add('GRB',grbox['GRB']),bat['GRBname'])
	s=np.where((t90[m2] <= 2.) & (t90[m2] > 0))# & (z[m1]>0))
	sGRBs=bat['GRBname'][m2][s]
	print '# of sGRBs = ',len(s[0])
	t90=t90[m2][s]
	z=z[m1][s]

	return sGRBs,z

def kcorr(input_Emin,input_Emax,output_Emin,output_Emax,gamma,nhgal,nh,zin,zout):

	eng1=np.logspace(np.log10(output_Emin),np.log10(output_Emax),100)/(1.+zout)
	eng2=np.logspace(np.log10(input_Emin),np.log10(input_Emax),100)*(1.+zin)

#	f1=pl(eng,gamma,epiv=1)
#	f2=pl(eng2,gamma,epiv=1)

	f1=jap.wabswabsPL(eng1,nhgal,nh,[1.,gamma],z=zout)
	f2=jap.wabswabsPL(eng2,nhgal,nh,[1.,gamma],z=zin)

	k1=np.trapz(f1*eng1,eng1)
	k2=np.trapz(f2*eng2,eng2)
	# k1=np.trapz(f1*eng1/(1+zout),eng1/(1+zout))
	# k2=np.trapz(f2*eng2/(1+zin),eng2/(1+zin))

	k=k1/k2

	return k

def throw_grbs(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',sensitivity_factor=1.,jet_angle=26.):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	sims=read_sims(configfile=configfile)
	nsims=len(sims)

	### cut the sample by inclination angle to jet opening angle
	inclin=ascii.read(dir+'simsfromLeo/tap-study/inclination.txt',names=['index','angle'])
	inclin['angle']=np.degrees(inclin['angle'])
	w=np.where(inclin['angle']>90.)[0]
	inclin['angle'][w]=180-inclin['angle'][w]

#	r=np.random.choice(len(m2),size=1,p=m2/np.sum(m2))
	def gaussian(x,m,sigma):

		return 1./(sigma*np.sqrt(2*np.pi))*np.exp(-0.5*((x-m)/sigma)**2)

	angs=np.linspace(0.1,90,1000)
	p=gaussian(angs,16.,10.)
	wg=np.where(angs>=26.)[0]
	p[wg[0]-1]=np.sum(p[wg[0]:])
	p[wg[0]:]=0
	p=p/np.sum(p)
	ja=angs[np.random.choice(1000,size=len(sims),p=p)]
#	w=np.where(inclin['angle']<=jet_angle)[0]
	m1,m2=grb_catalogs.match_catalogs_name(sims['map'],inclin['index'])
	m1=np.array(m1)
	m2=np.array(m2)
	w=np.where(inclin['angle'][m2]<=ja[m1])[0]
	sims=sims[m1[w]]
	nsims=len(sims)

	print nsims

	grbox=grb_catalogs.load_GRBOX(nointernet=True)
#	grbs=np.array(['111117A','100625A','100206A','100117A'])#,'080905A','070809','090515','070729','100117A'])
#	z=np.array([2.211,0.452,0.407,0.915])#,0.122,0.473,0.403,0.8,0.915]
#	m1,m2=grb_catalogs.match_catalogs_name(grbox['GRB'],grbs)
#	grbox['z'][m1]=z[m2]

	z=np.zeros(len(grbox))
	mask=grbox['z'].mask
	for i in range(len(grbox)):
		if (mask[i]==False) and (grbox['z'][i] != 'low'):
			z[i]=float(grbox['z'][i])

	bat=grb_catalogs.load_BAT()
	w=np.where(bat['T90']!='N/A')
	bat=bat[w]
	t90=np.array(bat['T90']).astype('float')

	# replace some T90's with those in Fong et al. 2015 (short with long soft tails)
	lsgrbs=np.array(['GRB050724','GRB061006','GRB061210','GRB070714B','GRB070729','GRB071227','GRB090510'])
	lst90=np.array([1.99,0.4,0.2,1.99,0.9,1.8,0.3])
	m1,m2=grb_catalogs.match_catalogs_name(bat['GRBname'],lsgrbs)
	t90[m1]=lst90[m2]

	m1,m2=grb_catalogs.match_catalogs_name(np.core.defchararray.add('GRB',grbox['GRB']),bat['GRBname'])
	s=np.where((t90[m2] <= 2.) & (t90[m2] > 0))# & (z[m1]>0))
	sGRBs=bat['GRBname'][m2][s]
	print '# of sGRBs = ',len(s[0])
	t90=t90[m2][s]
	z=z[m1][s]
	w0=np.where(z==0.)[0]
	z[w0]=0.5
	lumdist=cosmo.luminosity_distance(z).value
	pc2cm=3.08568025e18	
	dist=lumdist*1e6*pc2cm
#	distgw=440e6*pc2cm  ## 440 Mpc
	distgw=sims['distance']*1e6*pc2cm
	zgw=grb_catalogs.dist2z(sims['distance'])

	grbdir='/Users/jracusin/Swift/GRBfits/GRBs/'
	oldgrbdir='/Users/jracusin/GRBs/'

	eng=np.linspace(0.3,10.,100)
	de=eng[1]-eng[0]
	wtao=np.where((eng >=taoconfig['WFI_E_low']) & (eng <= taoconfig['WFI_E_high']))

	det=np.zeros(nsims)
	det[:]=-1
	detfrac=[]
	ngrbs=len(sGRBs)

	### need to go through each sim and randomly grab a GRB LC

	for r in range(nsims):

		p={}
		sp={}
		while ((p == {}) | (sp == {})):
			i=int(np.random.uniform()*ngrbs) # grab random XRT GRB
			if (os.path.exists(grbdir+sGRBs[i]+'/lc_fit_out_py_int1.dat') & os.path.exists(oldgrbdir+sGRBs[i])):
				p=fit_lc.read_lcfit(dir=grbdir+sGRBs[i]+'/')
				sp=fit_lc.read_specfit(dir=oldgrbdir+sGRBs[i]+'/')
		# scale by distance, and k-correct from 0.3-10 keV to 0.4-4.0 keV
		if 'PC' in sp.keys(): spec=sp['PC'] 
		else: spec=sp['WT']
		conv=np.sum(fit_functions.pow(eng[wtao],*[1.,spec.gamma])*de)/np.sum(fit_functions.pow(eng,*[1.,spec.gamma])*de)
		# k1=kcorr(0.3,10.,0.4,4,-spec.gamma,z[i])
		# k2=kcorr(0.4,4,0.4,4,-spec.gamma,zgw[r])
		k=kcorr(0.3,10.,taoconfig['WFI_E_low'],taoconfig['WFI_E_high'],spec.gamma,spec.galnh,spec.nh,z[i],zgw[r])
		# k1=kcorr(0.3,10.0,0.4,4.0,spec.gamma,0,0,0,0)  ### kcorr just the bandpass
		# k2=kcorr(0.4,4.0,0.4,4.0,spec.gamma,0,0,z[i],zgw[r])  ### kcorr just the redshift
		# k3=kcorr(0.4,4.0,0.4,4.0,spec.gamma,spec.galnh,spec.nh,spec.z_abs,zgw[r],0,0)  ## kcorr just the nh
		# k=k1*k2*k3
		# print spec.z_abs

		conv=spec.flux/spec.rate*dist[i]**2/distgw[r]**2*k  #distgw from map

#		conv=spec.flux/spec.rate*conv*dist[i]**2/distgw[r]**2*k  #distgw from map
#
#		conv=spec.flux/spec.rate*conv*dist[i]**2/distgw[r]**2/k1*k2  #distgw from map
#				print spec.flux/spec.rate,conv,dist[i]**2/distgw**2,k

		# flux of this GRB at time of obs
		tbin=np.array([sims['nettstart'][r]+sims['netExposure'][r]/2.])
#			zgw=sim['distgw']  ### fix this, invert dist/z, kcorr, get fscaled
		fscaled=fit_functions.call_function(p.model,tbin/(1.+z[i])*(1.+zgw[r]),*p.par)*conv

		# compare flux to sensitivity in 100 s exposure at the time that obs start
		sensitivity=sims['sensitivity'][r]*sensitivity_factor
		#sensitivity=wfi_sensitivity(exposure=sim['netExposure'][r])
		wdet=(fscaled >=sensitivity)
		det[r]=wdet

	det=det.astype(np.bool)
	wdet2=np.where((det == True) & (sims['netExposure']>0) & (sims['nettstart']>0))[0]
	detfrac=float(len(wdet2))/float(nsims)
#	print 'Surviving GRBs',ngrbs
	print 'Det Frac',detfrac
	wdet=wdet2

	w12=np.where(sims['snr']>=12)[0]
	wdet2=np.where((det[w12] == True) & (sims[w12]['netExposure']>0) & (sims[w12]['nettstart']>0))[0]
	detfrac12=float(len(wdet2))/float(len(w12))
	print 'Det Frac (SNR>12)',detfrac12
	wdet12=wdet2

	w8=np.where((sims['snr'] < 12 ) & (sims['snr']>=8))[0]
	wdet2=np.where((det[w8] == True) & (sims[w8]['netExposure']>0) & (sims[w8]['nettstart']>0))[0]
	detfrac8=float(len(wdet2))/float(len(w8))
	print 'Det Frac (SNR>8)',detfrac8
	wdet8=wdet2

	# w=np.where(z != 0.7)[0]
	# s=np.argsort(sGRBs[w])
	# w=w[s]
	# for i in w: print sGRBs[i],z[i]


	return wdet,wdet12,wdet8,detfrac,detfrac12,detfrac8#,sGRBs,z

def read_tiles(tilefile='tiles_wfi18.6_roll10.txt',dir='/Users/jracusin/TAO/simulations/'):

	data=ascii.read(dir+tilefile,data_start=1)
	sims=np.array(data['col1'])
	centers0=np.array([np.stack([data['col2'][i],data['col3'][i]]) for i in range(len(data))])
	rolls0=np.array(data['col4'])#[np.stack(data['col4']) for i in range(len(data))]
	probs0=np.array(data['col5'])#[np.stack(data['col5']) for i in range(len(data))]
	tileperc=np.array(data['col6'])

	return sims,centers0,rolls0,probs0,tileperc

def just_tiling(taoconfig=None,configfile='tao_config_v23.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	c=', '

	simdir=taoconfig['simdir']
	detsize=str(taoconfig['detsize'])
	print detsize
	outf=open(dir+'tiles_wfi'+detsize+'_roll10.txt','w')
	outf.write('sim'+c+'centers'+c+'rolls'+c+'probs'+c+'tileperc'+' \n')

	for i in range(0,1688):
		outf=open(dir+'tiles_wfi'+detsize+'_roll10.txt','a')

		datafile=simdir+str(i)+'.nside32.fits.gz'
		print datafile
		theta,phi,m,hdr=read_healpix_map(datafile)
		centers0,rolls0,probs0,tileperc=tiling(theta,phi,m,taoconfig=taoconfig)
		for j in range(len(rolls0)):
			outf.write(str(i)+c+str(centers0[j][0])+c+str(centers0[j][1])+c+str(rolls0[j])+c+str(probs0[j])+c+str(tileperc)+' \n')
		outf.close()


def run_sims(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',doplot=False,istart=0,noGTM=False,other_instrument=False):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	outfile=configfile.split('.txt')[0]+'.out'
	print outfile

	stats=ascii.read(dir+'simsfromLeo/tap-study/stats.txt')

	jetangle=taoconfig['jetangle']

	# random trigger times over 2 year mission

	# select random sky map
	nsims=taoconfig['simrate']
	print 'nsims = ',nsims

	simdir=taoconfig['simdir']
	ntrig=len(glob.glob(simdir+'*.nside32.fits.gz'))
#	ntrig=int(nsims)

	outf=open(dir+outfile,'a')
	outf.write('map, time, distance, ngwdet, snr, numtiles, tileperc, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart, whichtiledet \n')
	outf.close()

	allsimnums,allcenters,allrolls,allprobs,alltileperc=read_tiles(tilefile='tiles_wfi'+str(taoconfig['detsize'])+'_roll10.txt',dir='/Users/jracusin/TAO/simulations/')

	for i in range(istart,ntrig):

		datafile=simdir+str(i)+'.nside32.fits.gz'
		print datafile
		theta,phi,m,hdr=read_healpix_map(datafile)
		t=Time(hdr['MJD-OBS'],format='mjd')
		tmetdiff=t-Time('2001-01-01T0:00:00')
		tmet=tmetdiff.value*86400.
		print t.isot
#		snr=hdr['SNR']

		## read in ft2 for that mission week + next to get zenith RA/Dec
		ft2=read_ft2(t.mjd)
		scra=ft2['RA_ZENITH']
		scdec=ft2['DEC_ZENITH']
		sctime=ft2['START']
		saa=ft2['IN_SAA']

		coinc_id=i#hdr['OBJECT'].split(':')[2]
		distance=hdr['DISTMEAN']
		detectors=hdr['INSTRUME']#np.array(hdr['INSTRUME'].split(','))
		history=str(hdr['HISTORY']).split(',')
		# snrs=np.array([float(st.replace('\n','').split('=')[1]) for st in history if 'snr=' in st.replace('\n','')])# | ('s\nnr=' in st) | ('sn\nr=' in st))])
		# print 'SNR = ',snrs
		# wsnr=np.where(snrs >= 4.0)[0]
		# snr=np.sqrt(np.sum(snrs[wsnr]**2))
		# print 'Network SNR = ',snr
		# ngwdet=len(snrs)
		snr=stats['snr'][i]
		ngwdet=len(hdr['INSTRUME'].split(','))

#		print coinc_id,data[randmap]['lon'],data[randmap]['lat'],data[randmap]['inclination'],data[randmap]['distance'],data[randmap]['detectors'],data[randmap]['run']
#		print dir+'simsfromLeo/fits_fat/'+str(randmap)+'.fits.gz'
	## rotate it by random time
		
#		theta,phi,m2=reduce_res(m,32)
		# select random position within distribution for true position
		m2=m
		r=np.random.choice(len(m2),size=1,p=m2/np.sum(m2))
		lat=theta[r[0]]#-np.pi/2.
		lon=phi[r[0]]#+np.pi
		realra,realdec=thetaphi2radec(lat,lon)
		m3=m2

#		m3 = hp.get_interp_val(m2, theta, phi-gmst.rad[i])

		## find ft2 time that is closest to trigger time
#		t=abs(tmet[i]-sctime)

		startdelay=20.
		wtime=np.where(((sctime-tmet-startdelay)>0) & ((sctime-tmet-startdelay)<(taoconfig['norbits']*taoconfig['orbittime']*60)))[0]
		wt=np.argmin(sctime[wtime])#abs(tmet-sctime+startdelay))
		wt=wtime[wt]

		sep=separation(scra[wt],scdec[wt],realra,realdec)

		if other_instrument==True: ## instead of GTM, use GBM or BC or CALET etc
			randra,randdec=random_sky()
			sep=separation(randra,randdec,realra,realdec)
			## SVOM-GRM = 2.6 sr - 54 deg
			## Fermi-GBM = 8.8 sr - 114 deg
			## Swift BAT = 1.4 sr - 39 deg 
			## CALET - CGBM = 3 sr - 58.5
			## INTEGRAL SPI/ACS - 11 sr - 139 deg
			## BurstCube - 5.2 sr - 80 deg


		# scenarios
		# 1 - GRB outside GTM_FoV, SNR<12, any ndet - do nothing
		# 2 - GRB outside GTM_FoV, SNR>12, ndet>1 - GW tiling
		# 3 - GRB inside GTM_FoV, ndet>1 - GTM tiling + GW tiling
		# 4 - GRB inside GTM_FoV, ndet=1 - continue GTM tiling
		# 5 - GRB outside GTM_FoV, SNR>12, ndet=1 - nothing to do

		# # scenario 1 - we have no info but sub-threshold GW
		# if ((noGTM==False) & (snr<12) & (sep>taoconfig['gtm_FoV'])):
		# 	gtmtiling=False
		# 	gwtiling=False
		# 	print('NO TILING - GRB OUTSIDE GTM FOV, SNR<12')

		# # scenario 2 - case like noGTM
		# if ((noGTM==False) & (snr>12) & (ngwdet>1) & (sep>taoconfig['gtm_FoV'])):
		# 	gtmtiling=False
		# 	gwtiling=True
		# 	print('GW TILING ONLY - GRB OUTSIDE GTM FOV, SNR>12, NGWDET>1')

		# # scenario 3 - do everything
		# if ((noGTM==False) & (ngwdet>1) & (sep<taoconfig['gtm_FoV'])):
		# 	gtmtiling=True
		# 	gwtiling=True
		# 	print('GTM + GW TILING - GRB INSIDE GTM FOV, NGWDET>1')

		# #scenario 4 - only GTM tiling
		# if ((noGTM==False) & (ngwdet==1) & (sep<taoconfig['gtm_FoV'])):
		# 	gtmtiling=True
		# 	gwtiling=False
		# 	print('GTM TILING ONLY - GRB INSIDE GTM FOV, NGWDET=1')

		# # scenario 5 - we have no info even though SNR>12
		# if ((noGTM==False) & (snr>12) & (sep>taoconfig['gtm_FoV']) & (ngwdet==1)):
		# 	gtmtiling=False
		# 	gwtiling=False
		# 	print('NO TILING - GRB OUTSIDE GTM FOV, SNR>12, NGWDET=1')

		if noGTM==True:
			if snr<12: 
				gtmtiling=False
				gwtiling=False
			if ((snr>12) & (ngwdet>1)):
				gtmtiling=False  ## fix this when adding in raster
				gwtiling=True
		else:
			if ((snr>12) & (sep>taoconfig['gtm_FoV']) & (ngwdet>1)):
				gtmtiling=False
				gwtiling=True
				print('LETS CHASE SNR>12 OUTSIDE GTM FOV')

			if ((snr>8) & (sep<=taoconfig['gtm_FoV']) & (ngwdet>1)):
				gtmtiling=False
				gwtiling=True
				print('LETS FOLLOW-UP SNR>8 BECAUSE IN GTM FOV')

			if ((snr<12) & (sep>taoconfig['gtm_FoV'])):
				gtmtiling=False
				gwtiling=False
				print('NO FOLLOWING SNR<12 UNLESS IN GTM FOV')

			if ngwdet==1:
				gtmtiling=False
				gwtiling=False
				print('CANNOT FOLLOW-UP SINGLE GW DET EVENTS')
		# if ((noGTM==True) & (snr>12)):
		# 	gtmtiling=False
		# 	gwtiling=True

		print coinc_id,lon,lat,distance,detectors,ngwdet,sep

		### LOOP OVER TILING SCHEMES
		for tile in range(2):
		### WHICH TILING?
			if ((tile == 0) & (gtmtiling)):
				startdelay=0
				settlingtime=taoconfig['raster_settling']
				minexposure=taoconfig['raster_exposure']
				## grab 2 orbits around trigger time
				wtime=np.where(((sctime-tmet-startdelay)>0) & ((sctime-tmet-startdelay)<(taoconfig['norbits']*taoconfig['orbittime'])))[0]
				wt=np.argmin(sctime[wtime])#abs(tmet-sctime+startdelay))
				wt=wtime[wt]
				if doplot: hp.mollview(m3)
				centers0,rolls0,probs0,tileperc=GTM_tiling(m3,scra[wt],scdec[wt],taoconfig=taoconfig,doplot=False)
			if ((tile == 0) & (not gtmtiling)): continue

			if ((tile == 1) & (gwtiling)):
				if doplot: hp.mollview(m3)
#				if donetiles==True:
				ws=np.where(allsimnums==i)[0]
				if len(ws)>0:
					centers0=allcenters[ws]
					rolls0=allrolls[ws]
					probs0=allprobs[ws]
					tileperc=alltileperc[ws[0]]
	#				else:
	#					centers0,rolls0,probs0,tileperc=tiling(theta,phi,m3,taoconfig=taoconfig)
					## grab 2 orbits around trigger time
					startdelay=taoconfig['command_delay']+iss_comm_delay()[0]
					wtime=np.where(((sctime-tmet-startdelay)>0) & ((sctime-tmet-startdelay)<(taoconfig['norbits']*taoconfig['orbittime']*60)))[0]
					settlingtime=taoconfig['slew_overhead']
					minexposure=taoconfig['mintilesec']
					wt=np.argmin(sctime[wtime])#abs(tmet-sctime+startdelay))
					wt=wtime[wt]
				else: break
			if ((tile == 1) & (not gwtiling)): 
				if noGTM==False: print('NO OBSERVATIONS BECAUSE SNR<12 AND NO GTM OBSERVATION ')
				if noGTM==True: print('NO OBSERVATIONS BECAUSE SNR<12 AND NOT USING GTM')
				print 'output: ',coinc_id,t.isot,distance,snr,0,0,0,0,0,0,0,0,0,0,0
				outf=open(outfile,'a')
				sp=', '
				outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(ngwdet)+sp+str(snr)+sp+'0,0,0,0,0,0,0,0,0,0,0,0,0,0'+' \n')
				outf.close()
				break


			ntiles=len(rolls0)
			numtiles=ntiles
			moontheta,moonphi,suntheta,sunphi=concoords(t,taoconfig=taoconfig)
			th,ph=radec2thetaphi(scra[wt],scdec[wt])
			if doplot: 
				plot_tiles(m3,centers0,rolls0,doplot=False,taoconfig=taoconfig)
				hp.projscatter(th,ph,marker='*',color='blue')
				hp.projscatter(lat,lon,marker='x',color='white')
	#			hp.projscatter(lat-np.pi/2.,np.pi+lon,marker='x',color='white')
				hp.projplot(moontheta,moonphi,color='white')
				hp.projplot(suntheta,sunphi,color='yellow')
				thetacirc=np.radians(np.arange(0,365,5))
				zenithra=np.degrees(np.radians(taoconfig['zenithcon'])*np.sin(thetacirc)+np.radians(scra[wt]))
				zenithdec=np.degrees(np.radians(taoconfig['zenithcon'])*np.cos(thetacirc)+np.radians(scdec[wt]))
				zeniththeta,zenithphi=radec2thetaphi(zenithra,zenithdec)
				hp.projplot(zeniththeta,zenithphi,color='blue')	
#				plot.show()


			## using these tiles, figure out gtis
			tmin=[]
			tmax=[]
			centers=[]
			rolls=[]
			probs=[]
			tiles=[]
			start=False
			for j in range(ntiles):
				cra,cdec=thetaphi2radec(centers0[j][0],centers0[j][1])
	#			tmin0,tmax0=wheninFoR(cra,cdec,scra[wtime],scdec[wtime],sctime[wtime],taoconfig=taoconfig)
				tmin0,tmax0=wfigti(ft2[wtime],cra,cdec,taoconfig=taoconfig)
				ngti=len(np.append(tmin0,0))-1
				tmin=np.append(tmin,tmin0)
				tmax=np.append(tmax,tmax0)
				if ngti == 1: 
					if not start: centers=centers0[j]
					else: centers=np.row_stack((centers,centers0[j]))
					start=True
				if ngti > 1:
					if not start: centers=np.tile(centers0[j],(ngti,1))
					else: centers=np.row_stack((centers,np.tile(centers0[j],(ngti,1))))
					start=True
#				if tmin[0]==0:
#					ngti=0
#					centers=[0,0]
				rolls=np.append(rolls,np.repeat(rolls0[j],ngti))
				probs=np.append(probs,np.repeat(probs0[j],ngti))
				tiles=np.append(tiles,np.repeat(j,ngti))

			tmin=np.array(tmin)
			tmax=np.array(tmax)
			centers=np.array(centers)
			rolls=np.array(rolls)
			probs=np.array(probs)
			tiles=np.array(tiles)
			# remove tiles that are never visible in window
			w0=np.where(tmin != 0)[0]
			s=np.argsort(tmin[w0])
			w0=w0[s]
			if len(w0)>1:
				tmin=tmin[w0]
				tmax=tmax[w0]
				ntiles=len(w0)
				centers=centers[w0]
				rolls=rolls[w0]
				probs=probs[w0]
				tiles=tiles[w0]

#			print tmin-tmet,tmax-tmet
	#		tstart,tstop=tile_score(sctime[wtime],tmin,tmax,probs,tmet[i],taoconfig=taoconfig)
			s=[]
			if ((tile==0) & (gtmtiling) & (len(w0)>0)): 
				tstart,tstop=shorttiles(sctime[wtime],tmin,tmax,tmet,tiles,taoconfig=taoconfig)
				s=np.arange(0,len(tmin))
			if ((tile==1) & (gwtiling) & (ngti>0)): 
				tstart,tstop=evensplit(sctime[wtime],tmin,tmax,probs,tmet,tiles,taoconfig=taoconfig)
				s=np.argsort(tstart)
			ntiles=len(s)
			if ntiles>1:
				tmin=tmin[s]
				tmax=tmax[s]
				centers=centers[s]
				rolls=rolls[s]
				probs=probs[s]
				tstart=tstart[s]
				tstop=tstop[s]

			if ntiles>0:
				print 'tmin,tmax = ',tmin,tmax
				print 'tmin,tmax -tmet = ',tmin-tmet
				print tmax-tmet
				print 'tile windows - ',tiles

				if ngti>0:
					exposure=tstop-tstart
			#		s=np.argsort(tstart)
					# remove tiles with zero exposure
					w0=np.where((tstart >= 0) & (exposure >= minexposure))[0]#taoconfig['mintilesec']))[0]
					if len(w0)>0:
						if len(w0)>1:
							s=np.argsort(tstart[w0])
							w0=w0[s]
							exposure=exposure[w0]
							tstart=tstart[w0]
							tstop=tstop[w0]
							tmin=tmin[w0]
							tmax=tmax[w0]
							centers=centers[w0]
							rolls=rolls[w0]
							probs=probs[w0]
							ntiles=len(w0)
						if len(w0)==1:
							w0=np.append(w0,-1)
							exposure=np.append(exposure,-1)
							tstart=np.append(tstart,-1)
							tstop=np.append(tstop,-1)
							centers=np.vstack([centers,np.zeros(2)])
	#						tmin.append(-1)
	#						tmax.
					else:
						exposure=[]
						ngti=0
				
				if ((ntiles==0) | (ngti==0)):
					print 'NO TILES ARE OUT OF CONSTRAINT AND CONTAIN THE GRB'
					print 'output: ',coinc_id,t.isot,distance,snr,numtiles,tileperc,0,0,0,0,0,0,0,0,0,0,0,0
					outf=open(outfile,'a')
					sp=', '
					outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(ngwdet)+sp+str(snr)+sp+str(numtiles)+sp+str(tileperc)+sp+'0,0,0,0,0,0,0,0,0,0,0,0'+' \n')
					outf.close()

		#			print 'NO TILES ARE OUT OF CONSTRAINT'
		#			print 'output: ',0,0,0,0,0,0,0,0,0
		#			where_saa(ft2,tmet[i])
		#			wsaa=np.where(saa[wtime] == True)[0]
		#			print 'SAA = ', min(sctime[wtime[wsaa]])-tmet[i],max(sctime[wtime[wsaa]])-tmet[i]
				else:
					print 'exposure = ',exposure
					print 'TSTART, TSTOP = ', tstart-tmet,tstop-tmet
					where_saa(ft2,tmet)
		#			wsaa=np.where(saa[wtime] == True)[0]
		#			print 'SAA = ', min(sctime[wtime[wsaa]])-tmet[i],max(sctime[wtime[wsaa]])-tmet[i]
					print 'probs = ',probs

#					ntiles=len(tmin)
					tilesra=[]
					tilesdec=[]
					slewtimes=[]

					# plot tile with FoR at time, real position
					for k in range(ntiles):
						j=k#s[k]
			#			l=q[k]
						tilera,tiledec=thetaphi2radec(centers[j][0],centers[j][1])
						# sub=0.
						# if np.pi+data['lon'][randmap]+gmst.rad[i]>2*np.pi: sub=2*np.pi
						# add=0.
						# if data['lat'][randmap]>np.pi/2.: add=np.pi
						realra,realdec=thetaphi2radec(lat,lon)#+gmst.rad[i])
		#				realdec=-realdec
						if realdec>90: realdec=realdec-180.
						if realra>360: realra=realra-360.
						if realdec<-90: realdec=realdec+180.
						if realra<0: realra=realra+360.
			#			print 'tile, real = ',tilera,tiledec,realra,realdec
						sep=separation(tilera,tiledec,realra,realdec)

			#			print 'probs',probs[j]
			#			t=abs(tstart[l]-sctime+taoconfig['slew_overhead'])
						wt=np.argmin(abs(tstart[j]-sctime))#+taoconfig['slew_overhead']))
						#where(t == min(t))[0][0]
			#			print round(tstart[l]-tmet[i]),round(sctime[wt]-tmet[i]),scra[wt],scdec[wt]

						if k == 0:
							# starting position of survey tile
							pra,pdec=survey_pointing(scra[wt],scdec[wt],taoconfig=taoconfig)
							pth,pph=radec2thetaphi(pra,pdec)
							sfra=pra
							sfdec=pdec

						slewtime=separation(sfra,sfdec,tilera,tiledec)/taoconfig['slewrate']
						slewtimes.append(slewtime)
						sfra=tilera
						sfdec=tiledec
						tilesra.append(tilera)
						tilesdec.append(tiledec)
						print 'slewtime = ',slewtime
						print 'sep from real = ',sep

						mask=m3.astype(np.bool)
						mask[:]=False
						m4=hp.ma(m3)
						m4.mask=mask
						outmap=apply_FoR(m4,theta,phi,scra[wt],scdec[wt],taoconfig=taoconfig,FoRvalue=0.01)
			#			outmap.mask=mask
			#			theta=np.radians(np.arange(0,365,1))
						if doplot:
							hp.mollview(outmap,title='Tstart='+str(round(tstart[j]-tmet))+', Tstop='+str(round(tstop[j]-tmet))+', exposure = '+str(round(exposure[j])))
			#			hp.projplot(np.radians(taoconfig['FoR_radius'])*np.sin(t)+np.radians(scra[wtime[wt]]),np.radians(taoconfig['FoR_radius'])*np.cos(t)+np.radians(scdec[wtime[wt]]))
							plot_tile(0,centers[j],rolls[j],doplot=False,taoconfig=taoconfig)
							th,ph=radec2thetaphi(scra[wt],scdec[wt])
							## plot zenith
							hp.projscatter(th,ph,marker='*',color='blue')
							## plot real source position
							hp.projscatter(lat,lon,marker='x',color='white')			
							hp.projscatter(centers[j][0],centers[j][1],marker='*',color='orange')
							hp.projscatter(pth,pph,marker='p',color='green')
							hp.projplot(moontheta,moonphi,color='white')
							hp.projplot(suntheta,sunphi,color='yellow')
							zenithra=np.degrees(np.radians(taoconfig['zenithcon'])*np.sin(thetacirc)+np.radians(scra[wt]))
							zenithdec=np.degrees(np.radians(taoconfig['zenithcon'])*np.cos(thetacirc)+np.radians(scdec[wt]))
							zeniththeta,zenithphi=radec2thetaphi(zenithra,zenithdec)

							hp.projplot(zeniththeta,zenithphi,color='blue')
							ttile=t+TimeDelta(tstart[j]-tmet,format='sec')
							spra,spdec=solar_panels(scra[wt],scdec[wt],ttile,taoconfig=taoconfig)
							if scra[0]!=-1:
								spth,spphi=radec2thetaphi(spra,spdec)
								hp.projplot(spth,spphi,color='blue')
							plot.savefig(dir+'plots/sim'+'_'+str(i)+'_tile_'+str(j)+'.png')		
#							plot.show()

					dist=separation(realra,realdec,tilesra,tilesdec)
					wreal=np.where(dist<np.sqrt(2.)/2.*taoconfig['detsize'])[0]
					if len(wreal)>0:
						if len(wreal)==1:
							realtile=0
						else:
							realtile=wreal[np.argmin(tstart[wreal])]
							print realtile,np.argmin(dist)

						print 'which tile = ',realtile,wreal#,dist
						slewtimes=np.array(slewtimes)+settlingtime

						print 'map, time, distance, ngwdet, snr, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart'
						print 'output: ',coinc_id,t.isot,distance,ngwdet, snr, numtiles, tileperc, tilesra[realtile],tilesdec[realtile],realra,realdec,dist[realtile],\
							rolls[realtile],exposure[j],slewtimes[realtile],exposure[j]-slewtimes[realtile],\
							tstart[realtile]-tmet,tstart[realtile]-tmet+slewtimes[realtile]+startdelay,realtile
						outf=open(outfile,'a')
						sp=', '
						outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(ngwdet)+sp+str(snr)+sp+str(numtiles)+sp+str(tileperc)+sp+\
							str(tilesra[realtile])+sp+str(tilesdec[realtile])+sp+str(realra)+sp+str(realdec)+sp+str(dist[realtile])+\
							sp+str(rolls[realtile])+sp+str(exposure[j])+sp+str(slewtimes[realtile])+sp+str(exposure[j]-slewtimes[realtile])+\
							sp+str(tstart[realtile]-tmet)+sp+str(tstart[realtile]-tmet+slewtimes[realtile]+startdelay)+sp+str(realtile)+' \n')
						outf.close()
					else:
						print 'NO TILES ARE OUT OF CONSTRAINT AND CONTAIN THE GRB'
						print 'output: ',coinc_id,t.isot,distance,ngwdet,snr,0,0,0,0,0,0,0,0,0,0,0,0
						outf=open(outfile,'a')
						sp=', '
						outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(ngwdet)+sp+str(snr)+sp+'0,0,0,0,0,0,0,0,0,0,0,0,0,0'+' \n')
						outf.close()
				# else:
				# 	print 'SINGLE GW LOCALIZATION, NO TILING FOR TAO'
				# 	print 'output: ',coinc_id,t.isot,distance,snr,0,0,0,0,0,0,0,0,0,0,0
				# 	outf=open(outfile,'a')
				# 	sp=','
				# 	outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(snr)+sp+'0,0,0,0,0,0,0,0,0,0,0'+' \n')
				# 	outf.close()

		if doplot:	
			keepgoing=raw_input('Next Sim? (Y/n/s) ').upper()
			if keepgoing=='S': return

def read_sims(taoconfig=None,outfile=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	if outfile== None: outfile=dir+configfile.split('.txt')[0]+'.out'
	print outfile

	sims=ascii.read(outfile)
	sens=Table.Column(np.zeros(len(sims['map'])),name='sensitivity')
#	weight=Table.Column(np.zeros(len(sims['map'])),name='weight')
#	sims.add_columns([sens,weight])
	## remove after next run
	distance=Table.Column(np.zeros(len(sims['map'])),name='distance')
#	sims.add_columns([sens,distance])
	data=Table.read(dir+'simsfromLeo/weighted_sample_duty90.hdf5')
#	for s in range(len(sims)):
#		w=np.where(sims['map'][s]==data['coinc_id'])[0]
#		sims['distance'][s]=data['distance'][w]
	####

	sims.add_column(sens)
	exptime=sims['exposure']
	sensitivity=[wfi_sensitivity(taoconfig=taoconfig,exposure=float(e),sgrb=True) for e in exptime] ## will replace with Amy's code
	sims['sensitivity']=sensitivity
	sims['sensitivity'][np.where(sims['sensitivity']>1)]=0
	## need to add column for weight*rate - weight added automatically now
	## then need to throw scaled light curves at it

	return sims

def make_small_files(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',start=0):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	simdir=taoconfig['simdir']
	nsims=taoconfig['simrate']
	print 'nsims = ',nsims

	ntrig=int(nsims)

	for i in range(start,1400):

		datafile=simdir+str(i)+'.fits.gz'
		outdatafile=simdir+str(i)+'.nside32.fits.gz'
		if not os.path.exists(outdatafile):
			hdu=fits.open(datafile)
			print datafile
			m,hdr=hp.read_map(hdu,verbose=False,h=True)
			theta,phi,m2=reduce_res(m,32)
			print outdatafile
			hp.write_map(outdatafile,m2,overwrite=True,extra_header=hdr[27:])
#		hdu[1].header['NSIDE']=32
#		hdu[1].data=m2
#		hdu.writeto(outdatefile)

	return 

def GTM_tiling(map,zra,zdec,taoconfig=None,doplot=True):

	gtmrad=taoconfig['gtm_FoV']
	exposure=taoconfig['raster_exposure']
	settling=taoconfig['raster_settling']
	wd=taoconfig['detsize']

	gridra=np.array([0,0,-wd,-wd,-wd,0,wd,wd,wd,wd,0,-wd,-2*wd,-2*wd,-2*wd,-2*wd])\
		+zra+taoconfig['zenithcon']
	griddec=(np.array([0,-wd,-wd,0,wd,wd,wd,0,-wd,-2*wd,-2*wd,-2*wd,-2*wd,-wd,0,wd]\
		)+zdec)+taoconfig['zenithcon']#*np.cos(np.radians(gridra))
	theta,phi=radec2thetaphi(gridra,griddec)
	centers=np.column_stack([theta,phi])
	rolls=np.zeros(len(gridra))

	if doplot:
		plot_tiles(map,centers,rolls,doplot=False,taoconfig=taoconfig)
		thetacirc=np.radians(np.arange(0,365,5))
		zenithra=np.degrees(np.radians(taoconfig['zenithcon'])*np.sin(thetacirc)+np.radians(zra))
		zenithdec=np.degrees(np.radians(taoconfig['zenithcon'])*np.cos(thetacirc)+np.radians(zdec))
		zeniththeta,zenithphi=radec2thetaphi(zenithra,zenithdec)
		hp.projplot(zeniththeta,zenithphi,color='blue')			
		plot.show()

	probs=np.repeat(1./16,16)
	tileperc=1.0

	return centers,rolls,probs,tileperc

def survey_pointing(zra,zdec,taoconfig=None):
# generate random place where survey was pointing
	rra,rdec=random_sky(500)
	#which ones in FoR
	winfor,dist=inFoR(zra,zdec,rra,rdec,taoconfig=taoconfig) 


#	winfor=winfor.astype(np.bool)
#			wnoram=np.where((rra[winfor]<cra) | (rra[winfor]>(360+cra)))[0][0]
	#bad if rra-cra<180, good if  rra-cra>180, if rra-cra<0 then add 360
	winfor=np.where(winfor)[0]
#	wnoram=np.where(((rra[winfor]-zra)>180) | (rra[winfor]-zra+360>180))[0]
	wnoram=np.where((rra[winfor]<zra) | (rra[winfor]>zra+180))[0]
	pra=rra[winfor[wnoram]]
	pdec=rdec[winfor[wnoram]]
	i=int(np.random.rand()*len(wnoram))
	pra=pra[i]
	pdec=pdec[i]

	return pra,pdec

def shorttiles(time,tmin,tmax,tmet,tiles,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	### GTM TILING - SET ORDER, SKIP ANY IN CONSTRAINT
	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	exposure=taoconfig['raster_exposure']
	settling=taoconfig['raster_settling']
	slewtime=taoconfig['detsize']/taoconfig['slewrate']

	### which ones are not in constraint, and skip any in constraint
	wgood=np.where(tmin>0)[0]
	tstart=np.zeros(len(tmin))
	tstop=np.zeros(len(tmax))

	ntiles=len(wgood)
	snapshot=exposure+settling+slewtime

	#### tile time slots
	tstart=tmet+snapshot*np.arange(ntiles)+20
	tstop=tmet+snapshot*(np.arange(ntiles)+1)+20
	tstart[0]=tstart[0]-20

#	print 'what ',tstart-tmet,tstop-tmet

#	good=np.repeat(False,len(tmin)) 

	### doesn't work
	# i=0
	# # whichtile=np.zeros(ntiles)
	# # for t in range(ntiles):
	# # 	if (tmin[i]<=tstart[t]) & (tmax[i]>=tstop[t]): 
	# # 		whichtile[t]=i
	# # 		i=i+1

	tstartout=np.zeros(len(tmin))
	tstopout=np.zeros(len(tmin))
	tstartout[wgood]=tstart[0:len(wgood)]
	tstopout[wgood]=tstop[0:len(wgood)]

	return tstartout,tstopout

def evensplit(time,tmin,tmax,probs,tmet,tiles,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	ntiles=len(tmin)
	extratiles=20
	tstartmin=0
	if len(np.unique(tiles)) == 1:
		exposures=tmax[0]-tmin[0]
		tstartmin=tmin[0]-tmet
		tstartout=np.array(tmin[0])
		tstopout=np.array(tmax[0])
		return tstartout,tstopout
	if len(np.unique(tiles)) >1:
		exposures=np.repeat(taoconfig['mintilesec'],ntiles+extratiles)

	print 'Exposures = ',exposures
	tstart=min(tmin)+exposures*np.arange(ntiles+extratiles)
	tstop=min(tmin)+exposures*(np.arange(ntiles+extratiles)+1)

	print tmet
#	whichtiles=np.array(np.repeat(-1,ntiles))
	whichtiles=[]
	wtind=[]
	done=np.zeros(ntiles)#+extratiles)
	for i in range(ntiles+extratiles): # loop through windows
		w=np.where((tmin<=tstart[i]) & (tmax>=tstop[i]) & (done==0))[0]
		print i,tstart[i]-tmet,tstop[i]-tmet,w,done
		t=-1
		if len(w)>1:
			tm=np.argmax(probs[w])
			t=w[tm]
			print t
		if len(w)==1:
			t=w[0]#probs[w]
		if t != -1:
			whichtiles.append(t)
			wtind.append(i)
		#whichtiles.append(t)
		if t != -1: 
			done[t]=1
			wtiles=np.where(tiles==tiles[t])[0]
			done[wtiles]=1


#	wg=np.where(whichtiles != -1)
#	print whichtiles[wg]
#	if len(whichtiles)>1: whichtiles=np.concatenate(whichtiles)
	whichtiles=np.array(whichtiles)
	tstartout=np.zeros(ntiles)
	tstopout=np.zeros(ntiles)
	wtind=np.array(wtind)
	if len(whichtiles)>0:
		tstartout[whichtiles]=tstart[wtind]
		tstopout[whichtiles]=tstop[wtind]
	else:
		tstartout=0
		tstopout=0
#	tstart[wg]=tstart[whichtiles[wg]]
#	tstop[wg]=tstop[whichtiles[wg]]

	return tstartout,tstopout

def bruteforce(time,tmin,tmax,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):
	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	ntiles=len(tmin)
	nvis=np.zeros(len(time))
#	tw=[]
	whichtile=[]
	for l in range(ntiles):
		w=np.where((time>tmin[l]) & (time<tmax[l]))
#		tw.append(w)
		nvis[w]=nvis[w]+1

	for t in time:
		w=np.where((t>tmin) & (t<tmax))[0]
		if len(w)==0: w=-1
		whichtile.append(w)

	tstart=np.zeros(ntiles)
	tstop=np.zeros(ntiles)

	w1=np.where(nvis == 1)[0]

	whogetsit=np.ones(len(time))
	whogetsit[:]=-1
	for i in range(len(w1)): whogetsit[w1[i]]=whichtile[w1[i]]

#	tstart[w1]

	return nvis,whichtile,whogetsit

		### make array of how many tiles visible in each window
		## schedule window with only 1 tile
		## remove that from vis calc
		## do it again
		## if none, divide some evenly

def tile_score(time,tmin,tmax,p,tmet,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	tilemin=taoconfig['mintilesec']
	torb=taoconfig['orbittime']*60.
	tdiff=time[1]-time[0]
	ntiles=len(tmin)
	timedone=np.zeros(ntiles)
	whichtile=[]
	for t in time:
		score=[]
		for l in range(ntiles):
			if (t>=tmin[l]) and (t<=tmax[l]): #score=0 if tile not yet visible
				timeleft=tmax[l]-t  # array of timeleft for all times
				tlscore=1.-2*timeleft/torb#(timeleft-min(timeleft))/max(timeleft)
				tdscore=1. # if not yet observed
				if l in whichtile: # if already observed
					n=float(len(np.where(np.array(whichtile)==l)[0])) # how many times
					timedone=n*tdiff  #time already observed
					if timedone<=tilemin: #doesn't affect score if observed less than min tile time
						tdscore=1.
					else:
						tdscore=1.-timedone/torb#(timedone-min(timedone))/max(timedone)
				score.append(tlscore*tdscore)#*p[l])
			else:
				score.append(0.)

		print t-tmet,score,np.argmax(score)
		if np.sum(score)>0.:
			whichtile.append(np.argmax(score))
#			w=np.argmax(score)
#			s=max(score)
		else:
			whichtile.append(-1)
#			w=-1
#			s=0

	whichtile=np.array(whichtile)
#	print whichtile
	tstart=[]
	tstop=[]
	for l in range(ntiles):
		w=np.where(whichtile == l)[0]
#		print l
		if len(w)>0:
			tstart.append(time[min(w)])
			tstop.append(time[max(w)]) ### not right because of switching!!!!!!!!!!
		else:
			tstart.append(0.)
			tstop.append(0.)

	if len(tstart)>1:
		s=np.argsort(tstart)
		for l in range(ntiles-1):
			if tstop[s[l]]>tstart[s[l+1]]: tstop[s[l]]=tstart[s[l+1]]-tdiff

	tstart=np.array(tstart)
	tstop=np.array(tstop)
	return tstart,tstop

#		score[t<tstart]=0 # those tiles not yet observable are zeroed


	# if tstart<tmin:
	# 	score=0
	# else:
	# 	timeleft=float(tmax-tstart-(slewdist*taoconfig['slewrate']))
	# 	score=1.-p#1.-timeleft/(taoconfig['orbittime']*60.)*p # divide by 1/2 orbit s to get <1

#	return score

def iss_comm_delay(n=1):


	perc0=[0.865,0.135]
	r=np.random.choice(len(perc0),size=n,p=perc0/np.sum(perc0))*1.
	w1=np.where(r == 1)[0]

	duration=np.array([0,57,127,355,560,820,1106,1207,1336,1477,1795.])
	perc=np.array([0,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.97,0.98,0.99])#*0.135+0.865
	dperc=np.diff(perc)
	d=np.arange(1795)
	f=interpolate.interp1d(duration[1:],dperc,bounds_error=False,fill_value="extrapolate",kind='linear')
	p=f(d)
	r1=np.random.choice(len(p),size=len(w1),p=p/np.sum(p))*1.
	r[w1]=r1

	return r

def where_saa(ft2,tmet):
	w=np.where((ft2['START']-tmet < 92.5*60.) & (ft2['START']-tmet >=0))[0]
	saa=np.where(ft2['IN_SAA'][w])[0]
	t=ft2['START'][w[saa]]-tmet
	tdiff=ft2['START'][1]-ft2['START'][0]
	x=np.where(t[1:]-t[0:-1] > tdiff)[0]
	if len(x)> 0: 
		x=x[0]
		print 'SAA = ',t[0],t[x],t[x+1],t[-1] 
	else:
		if len(saa)>0: print 'SAA = ',t[0],t[-1]

def test_wfi_plot():
	ra,dec,map=read_healpix_map('/Users/jracusin/LIGO/2016_fits/683114/bayestar.fits')
	
	ra,dec,theta,phi,map=reduce_res(map,32)

	### find best tiles for map
	centers,rolls=tiling(theta,phi,map,taoconfig=taoconfig)

	plot_tiles(map,centers,rolls)
	ras,decs=thetaphi2radec(centers[:,0],centers[:,1])

	return ras,decs,rolls

### need sky viewability as a function of time
### zero out map not in FoV
### determine best tiles for current map
### account for slew time somehow? -later

def plot_tiles(map,centers,rolls,doplot=True,taoconfig=None):
#	if doplot: hp.mollview(map=map)#,flip='astro')
	hp.graticule()
	ntiles=len(rolls)

	for i in range(ntiles):
		wfi,wfirot=wfi_fov(wficenter=np.degrees(centers[i]),wfiroll=rolls[i],taoconfig=taoconfig)

		hp.projplot(np.radians(wfirot[:,0]),np.radians(wfirot[:,1]),color='orange')
	if doplot: 
		plot.show()


def plot_tile(map,center,roll,doplot=True,taoconfig=None):
#	if doplot: hp.mollview(map=map)#,flip='astro')
	hp.graticule()

	wfi,wfirot=wfi_fov(wficenter=np.degrees(center),wfiroll=roll,taoconfig=taoconfig)

	hp.projplot(np.radians(wfirot[:,0]),np.radians(wfirot[:,1]),color='orange')

#	if doplot: 
#		plot.show()


def apply_FoR(inmap,theta,phi,ra_zenith,dec_zenith,taoconfig=None,FoRvalue=0,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	### given zenith position, zero out map outside FoR from taoconfig

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	ra,dec=thetaphi2radec(theta,phi)
	d=separation(ra,dec,ra_zenith,dec_zenith)
#	d=[separation(ra[i],dec[i],ra_zenith,dec_zenith) for i in range(len(ra))]

	outside=np.where(d >= taoconfig['FoR_radius'])[0]

	mask=inmap.mask#inmap.astype(np.bool)
	mask[outside]=True

#	outmap=hp.ma(inmap)
	outmap=inmap

	outmap.mask=mask#np.logical_not(mask)
#	outmap[outside]=hp.UNSEEN#FoRvalue

	return outmap

def tiling(theta,phi,m,prob_coverage=0.9,taoconfig=None):
### for every low res pixel > threshold, see if a FoV centered on that pixel provides the maximum enclosed prob
### interate until nothing left

	ptot=0.
	tilenum=0
	centers=[]
	rolls=[]
	probs=[]
	mask=m.astype(np.bool)
	mask[:]=False
	mmask=hp.ma(m)
	mmask.mask=mask
	pout=0.

	while ptot < prob_coverage:
		bestcenter,r,maxp=find_best_tile(theta,phi,mmask,taoconfig=taoconfig)
		if r!=-1:
			wfi,wfirot=wfi_fov(wficenter=np.degrees(bestcenter),wfiroll=r,taoconfig=taoconfig)

			incp,w=prob_inside_fov(np.degrees(theta),np.degrees(phi),mmask,wfirot)
			mask[w]=True
	# 		mask[w]=np.logical_not(mask[w])#False
	# 		print w
	# #		mmask[w]=hp.UNSEEN
	 		mmask.mask=mask
	# 		print len(np.where(mmask.mask==False)[0])
	# 		w0=np.where(mask==False)[0]
	# 		print len(w0)


			ptot=ptot+incp
			pout=ptot
		else:
			break

		tilenum += 1
		print incp,ptot,tilenum

		if tilenum>=taoconfig['maxtiles']: ptot=1.

		centers.append(bestcenter)
		rolls.append(r)
		probs.append(maxp)

	return centers,rolls,probs,pout

def find_best_tile(theta,phi,map,threshold=1e-3,taoconfig=None):

#	wm=np.where(map.mask == False)[0]
	pix=np.where(map >= threshold)[0]  # search only pixels above some probability threshold
#	pix=wm[pix]
	
	if len(pix)==0:
		return -1,-1,-1

	p=[]
	roll=[]
	for x in pix:  ## loop through pixels to find the one with the highest enclosed prob, and it's best roll
		wfi,wfirot=wfi_fov(wficenter=(np.degrees(theta[x]),np.degrees(phi[x])),wfiroll=0,taoconfig=taoconfig)
		bestroll,mxp=find_best_roll(np.degrees(theta),np.degrees(phi),map,wfi,wficenter=(np.degrees(theta[x]),np.degrees(phi[x])),taoconfig=taoconfig)
		p.append(mxp)
		roll.append(bestroll)
#		print maxp,bestroll

#	maxp=max(p) ## best pixel
	roll=np.array(roll)
	pp=np.ma.getdata(p)
#	wp=np.where(pp != np.nan)[0]
	wp=np.where(np.isnan(p)==False)
	p=pp[wp]
	roll=roll[wp]
	w=np.where(p == max(p))[0][0]  #find roll for best pixel
#	w=np.argmax(p)
#	w=np.where(p == np.ma.MaskedArray.max(p))[0]
#	w=np.ma.MaskedArray.argmax(p)
	maxp=p[w]
	r=roll[w]
	bestpix=pix[w]
	bestcenter=(theta[bestpix],phi[bestpix])

	return bestcenter,r,maxp

def find_best_roll(ra,dec,map,wfi,wficenter=(0,0),taoconfig=None):

	roll=np.arange(0,90,taoconfig['roll_delta'])
	p=[]
	for r in roll:
		wfi,wfirot=wfi_fov(wficenter=(wficenter[0],wficenter[1]),wfiroll=r,taoconfig=taoconfig)
		incp,w=prob_inside_fov(ra,dec,map,wfirot)
		p.append(incp)

	maxp=max(p)
	bestroll=float(roll[np.argmax(p)])

	return bestroll,maxp

def inside_box(phi,theta,wfi):

	from matplotlib.path import Path
	points=np.vstack((phi,theta)).T
	p=Path(wfi)
	grid=p.contains_points(points)
#	w=np.where((grid == True))[0]
	output=False
	if grid==True: output=True
	return output

def prob_inside_fov(ra,dec,probmap,wfi):

	from matplotlib.path import Path
	points=np.vstack((ra,dec)).T
	#wfi=wfi_fov(wficenter=wficenter,wfiroll=wfiroll)
	p=Path(wfi)
	grid=p.contains_points(points)
	w=np.where((grid == True))[0]
	wnomask=np.where(probmap[w] != hp.UNSEEN)[0]
	prob=np.sum(probmap[w[wnomask]])
#	probmap.mask[w]=True
	# pm=probmap.filled()
	# w0=np.where(probmap.mask == True)[0]
	# pm[w0]=0.
	# prob=np.sum(pm[w])
#	prob=np.sum(probmap[w])

	return prob,w

def wfi_fov(wficenter=(0,0),wfiroll=0,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	d=taoconfig['detsize']
	h=d/2.
	cx=wficenter[0]
	cy=wficenter[1]
	xarr=np.arange(cx-h,cx+h,1)
	nx=len(xarr)
	yarr=np.arange(cy-h,cy+h,1)
	ny=len(yarr)
	xwfi=np.append(np.append(xarr,np.repeat(cx+h,ny)),np.append(xarr[::-1],np.repeat(cx-h,ny)))
	ywfi=np.append(np.append(np.repeat(cy-h,nx),yarr),np.append(np.repeat(cy+h,nx),yarr[::-1]))
	wfi=[]

	wfi=[[xwfi[i],ywfi[i]] for i in range(len(xwfi))]
	wfi=np.array(wfi)
#	wfi=np.array([(cx-h,cy-h),(cx+h,cy-h),(cx+h,cy+h),(cx-h,cy+h),(cx-h,cy-h)])

	def Rotate2D(pts,cnt,angdeg=0):
		'''pts = {} Rotates points(nx2) about center cnt(2) by angle ang(1) in radian'''
		ang=angdeg*np.pi/180

		return np.dot(pts-cnt,np.array([[np.cos(ang),np.sin(ang)],[-np.sin(ang),np.cos(ang)]]))+cnt

	wfirot=Rotate2D(wfi,wficenter,wfiroll)

	return wfi,wfirot

def thetaphi2radec(theta,phi):

	dec=-np.degrees(theta-np.pi/2.)
	ra=np.degrees(np.pi*2-phi)

	return ra,dec

def radec2thetaphi(ra,dec):

	phi=2*np.pi-np.radians(ra)
	theta=np.pi/2-np.radians(dec)

	return theta,phi

def reduce_res(map,new_nside):

	maplow=hp.ud_grade(map,new_nside)
	npix=hp.nside2npix(new_nside)
	theta,phi=hp.pix2ang(new_nside,np.arange(npix))
#	ra=-np.degrees(theta-np.pi/2.)
#	dec=np.degrees(np.pi*2-phi)
	maplow=maplow/np.sum(maplow)

	return theta,phi,maplow

def read_healpix_map(filename):

	hdu=fits.open(filename)
	probmap=hp.read_map(hdu,verbose=False)
	hdr=hdu[1].header
	nside=hp.get_nside(probmap)
	npix=hp.nside2npix(nside)
	theta,phi=hp.pix2ang(nside,np.arange(npix))
#	ra=-np.degrees(theta-np.pi/2.)
#	dec=np.degrees(np.pi*2-phi)

	return theta,phi,probmap,hdr

def read_tao_config(filename='tao_config_v1.txt'):

	print filename
	tao_table=ascii.read(filename)
	tao={tao_table['col1'][i]:tao_table['col2'][i] for i in range(len(tao_table))}
	tao['suncon']=float(tao['suncon'])
	tao['mintilesec']=float(tao['mintilesec'])
	tao['slewrate']=float(tao['slewrate'])
	tao['roll_delta']=float(tao['roll_delta'])
	tao['simrate']=float(tao['simrate'])
	tao['maxtiles']=float(tao['maxtiles'])
	tao['FoR_radius']=float(tao['FoR_radius'])
	tao['command_delay']=float(tao['command_delay'])
	tao['detsize']=float(tao['detsize'])
	tao['mooncon']=float(tao['mooncon'])
	tao['raster_exposure']=float(tao['raster_exposure'])
	tao['iss_uptime']=float(tao['iss_uptime'])
	tao['raster_settling']=float(tao['raster_settling'])
	tao['FT2res']=float(tao['FT2res'])
	tao['bns_rate']=float(tao['bns_rate'])
	tao['gtm_FoV']=float(tao['gtm_FoV'])
	tao['slew_overhead']=float(tao['slew_overhead'])
	tao['jetangle']=float(tao['jetangle'])
	tao['orbittime']=float(tao['orbittime'])
	tao['zenithcon']=float(tao['zenithcon'])
	tao['norbits']=int(tao['norbits'])
	tao['solar_panel_width']=float(tao['solar_panel_width'])
	tao['WFI_E_low']=float(tao['WFI_E_low'])	
	tao['WFI_E_high']=float(tao['WFI_E_high'])	
	tao['WFI_sensitivity_factor']=float(tao['WFI_sensitivity_factor'])

	return tao

def run_gtorbsim(dir='/Users/jracusin/Lobster/TAO_2016/simulations/FT2/'):
	import os
	mjd0=59580
	mw=52*2+1
	mjds=mjd0+np.arange(mw)*7
	n='\n'
	for i in range(mw-1):
		file='tao_gtorbsim_MW'+str(i)+'.txt'
		print 'gtorbsim file '+ dir+file
		

def write_gtorbsim_files(ft2dir='/Users/jracusin/Lobster/TAO_2016/simulations/FT2/',taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)
	mjd0=59580
	mw=52*2+1
	mjds=mjd0+np.arange(mw)*7
	n='\n'
	for i in range(mw-1):
		outfile='FT2_TAO_TLE_MW'+str(i)+'.fits'
		file='tao_gtorbsim_MW'+str(i)+'.txt'
		file=open(ft2dir+file,'w')
		file.write('start_MJD = '+str(mjds[i])+n)
		file.write('stop_MJD = '+str(mjds[i+1])+n)
		file.write('TLType = SINGLE'+n)
		file.write('Timeline = |SURVEY|0.0|'+n)
		file.write('EphemFunc = tlederive'+n)
		file.write('EphemName = ISS.tle'+n)
		file.write('Units = 1.0'+n)
		file.write('Resolution = '+str(taoconfig['FT2res']/60.)+n)
		file.write('Initial_RA = 0'+n)
		file.write('Initial_DEC = 0'+n)
		file.write('saafile = TAO_SAA_2018.01'+n)
		file.write('OutPutFile = '+ft2dir+outfile+n)
		file.close()

def read_ft2(mjd,dir='/Users/jracusin/Lobster/TAO_2016/simulations/FT2/'):

	import os.path
	mjd0=59580
	mw=int(round((mjd-mjd0)/7.-0.5))
	filename=dir+'FT2_TAO_TLE_MW'+str(mw)+'.fits'
	ft2a=fits.open(filename)
	print filename
	filename=dir+'FT2_TAO_TLE_MW'+str(mw+1)+'.fits'
	if os.path.isfile(filename): 
		ft2b=fits.open(filename)#[1].data

		nrows1 = ft2a[1].data.shape[0]
		nrows2 = ft2b[1].data.shape[0]
		nrows = nrows1 + nrows2
		ft2 = fits.BinTableHDU.from_columns(ft2a[1].columns, nrows=nrows)
		for colname in ft2a[1].columns.names:
			ft2.data[colname][nrows1:] = ft2b[1].data[colname]
	else:
		ft2=ft2a
	if len(fits.HDUList(ft2))==1: 
		ft2=ft2.data
	else:
		if len(fits.HDUList(ft2))==2: ft2=ft2[1].data
#		ft2=ft2.data

	return ft2	

def sph_cap(ang,frac=False):
	if frac == False: 
		r=(1-np.cos(np.radians(ang)))/2.
	else:
		r=np.degrees(np.arccos(1.-2*ang))

	return r

def estimate_viewing_area():

	# eyeballing from Image Craig sent for Columbus SOX Zenith-facing

	a1=sph_cap(40) # unobcured central 40 deg
	a2=(sph_cap(50)-sph_cap(40))*0.875
	a3=(sph_cap(60)-sph_cap(50))*0.7
	a4=(sph_cap(90)-sph_cap(60))*0.5
	a=a1+a2+a3+a4

	print a1,a2,a3,a4,a

def separation(ra1,dec1,ra2,dec2):

	c=SkyCoord(ra=ra1*u.deg,dec=dec1*u.deg)
	d=SkyCoord(ra=ra2*u.deg,dec=dec2*u.deg)
	dist=c.separation(d)
	dist=dist.value

	return dist

def inFoR(ra,dec,cra,cdec,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	# estimate round FoR equivalent to image estimate from above 80 deg of hemisphere
	# is position currently in FoR
	# for a single tile point, test various zenith positions, return which are in FoR, and distance from zenith
	## ra, dec = zenith
	## cra, cdec = other points to compare

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	radius=taoconfig['FoR_radius']
	

	# for i in range(len(cra)):
	# 	d=separation(ra,dec,cra[i],cdec[i])
	# 	dist=np.append(dist,d)
	# 	if d<=radius:
	# 		winFoR=np.append(winFoR,True)
	# 	else:
	# 		winFoR=np.append(winFoR,False)

	dist=separation(ra,dec,cra,cdec)
	winFoR=dist<=radius

#	winFoR=winFoR.astype(np.bool)

	return winFoR,dist

def test_solar_panels(scra,scdec,t,taoconfig=None):


	thetacirc=np.radians(np.arange(0,365,5))
	c=180/np.pi

	for i in np.arange(0,24,1):
#		td=TimeDelta(i*60**2,format='sec')
		hp.graticule()
		scth,scph=radec2thetaphi(scra+i*15,scdec)
		scphc=np.radians(taoconfig['FoR_radius'])*np.sin(thetacirc)+scph
		scthc=np.radians(taoconfig['FoR_radius'])*np.cos(thetacirc)+scth
		hp.projscatter(scth,scph,color='blue')
		hp.projplot(scthc,scphc,color='blue')
#		plot.scatter(scph*c,scth*c,color='blue')
#		plot.plot(scphc*c,scthc*c,color='blue')

		d=np.radians(i*15.)
		moontheta,moonphi,suntheta,sunphi=concoords(t,taoconfig=taoconfig)
		hp.projplot(suntheta,sunphi,color='yellow')
#		plot.plot((sunphi+d)*c,suntheta*c,color='yellow')
		sun=coord.get_sun(t)
		sunth,sunph=radec2thetaphi(sun.ra.value,sun.dec.value)
		hp.projscatter(sunth,sunph,color='yellow')
#		plot.scatter((sunph+d)*c,sunth*c,color='yellow')

		newtime=t+TimeDelta(92.5*60./24.*i,format='sec')
		print newtime.isot
		ra,dec=solar_panels(scra+i*15,scdec,newtime,taoconfig=taoconfig)
		th,ph=radec2thetaphi(ra,dec)
		hp.projplot(th,ph)
#		plot.plot(ph*c,th*c)
#		plot.xlim(0,360)
#		plot.ylim(0,180)
		plot.show()


def solar_panels(scra,scdec,t,taoconfig=None):
	### they move at 3.8 deg/min = 0.063 deg/s
	### width = 20 deg, but account for edge of FoV, not mid add 0.5*detsize
#	width=20+30 #(for glints)
	width=taoconfig['solar_panel_width']
	elevation=70.

	sun=coord.get_sun(t)
	sunra=sun.ra.value
	sundec=sun.dec.value
	sunha=scra-sunra
	if sunha<-180: sunha=sunha+360.
	sunsep=abs(sunha)/15.#separation(sunra,sundec,scra,sundec)/15.

	# if sunsep>6:  # night time - all the time
	# 	spra=np.array([-1])
	# 	spdec=np.array([-1])
	# else:
	# print sunha
	# if ((sunha < 90) | ((sunha > -270) & (sunha<0))):
	d=(np.sin(np.radians(sunha)))*0.25
	x=90.
	xarr=np.arange(0,width,1)/width*x#round(x*d),x*d/width)
	yarr=np.arange(-width/2.,width/2.,1)
	n=len(xarr)

	ra=np.append(np.append(xarr,np.repeat(max(xarr),n)),np.append(xarr[::-1],np.repeat(min(xarr),n)))
#			[0,60,60,0,0])
	dec=np.append(np.append(np.repeat(width/2.,n),yarr[::-1]),np.append(np.repeat(-width/2.,n),yarr))#+width/2.
	#np.array([width/2.,width/2.,-width/2.,-width/2.,width/2.])
	spra=scra+ra+(360.-elevation-x/2.)#250#*d
	w=np.where(spra<0)[0]
	spra[w]=spra[w]+360.
	w=np.where(spra>360)[0]
	spra[w]=spra[w]-360.
#		print sunha
	spdec=scdec+dec-sunha
	w=np.where(spdec<-90)[0]
	spdec[w]=spdec[w]+180.
	w=np.where(spdec>90)[0]
	spdec[w]=spdec[w]-180.
#		if scdec<-90: spdec=spdec+180.
#		if scdec>90: spdec=spdec-180.

	return spra,spdec

def concoords(t,taoconfig=None):

	sun=coord.get_sun(t)
	moon=coord.get_moon(t)
	mooncon=float(taoconfig['mooncon'])
	suncon=float(taoconfig['suncon'])

	theta=np.radians(np.arange(0,365,5))
	moonra=np.degrees(np.radians(mooncon)*np.sin(theta)+np.radians(moon.ra.value))
	moondec=np.degrees(np.radians(mooncon)*np.cos(theta)+np.radians(moon.dec.value))
	sunra=np.degrees(np.radians(suncon)*np.sin(theta)+np.radians(sun.ra.value))
	sundec=np.degrees(np.radians(suncon)*np.cos(theta)+np.radians(sun.dec.value))

	moontheta,moonphi=radec2thetaphi(moonra,moondec)
	suntheta,sunphi=radec2thetaphi(sunra,sundec)

	return moontheta,moonphi,suntheta,sunphi

def wfigti(ft2,cra,cdec,taoconfig=None):
	# exclude SAA
	# when is cra/cdec inside FoR
	dist=separation(ft2['RA_ZENITH'],ft2['DEC_ZENITH'],cra,cdec)
	t=Time(ft2['START']+(Time('2001-01-01T0:00:00').gps-Time('1980-01-06T00:00:00').gps),format='gps')
	sun=coord.get_sun(t)
	moon=coord.get_moon(t)
	sundist=separation(cra,cdec,sun.ra.value,sun.dec.value)
	moondist=separation(cra,cdec,moon.ra.value,moon.dec.value)
	zenithdist=separation(cra,cdec,ft2['RA_ZENITH'],ft2['DEC_ZENITH'])

#	if (sundist>taoconfig['suncon']) & (moondist>taoconfig['mooncon']): 
	w=np.where((ft2['IN_SAA']==False) & (dist<taoconfig['FoR_radius']) & \
		(sundist>taoconfig['suncon']) & (moondist>taoconfig['mooncon']) & \
		(zenithdist>taoconfig['zenithcon']))[0]
	spblock=np.repeat(False,len(sundist))
	for i in range(len(w)):
		spra,spdec=solar_panels(ft2['RA_ZENITH'][i],ft2['DEC_ZENITH'][i],t[i],taoconfig=taoconfig)
		spblock[w[i]]=False
		if spra[0]!=-1:
			sp=np.vstack((spra,spdec)).T
			spblock[w[i]]=inside_box(cra,cdec,sp) # true if solar panel is blocking point
	w2=np.where(spblock[w]==False)
	w=w[w2]

#	w=np.where(dist<taoconfig['FoR_radius'])

	if len(w)>0:
		t=ft2['START'][w]
		tdiff=t[1:]-t[0:-1]
		wgap=np.where(tdiff > taoconfig['FT2res'])[0]
		gtistart=np.append(t[0],t[wgap+1])#-min(ft2['START'])
		gtistop=np.append(t[wgap],max(t))#-min(ft2['START'])
	else:
		gtistart=0
		gtistop=0

	gtistart=np.array(gtistart)
	gtistop=np.array(gtistop)
# else:
# 	gtistart=0
# 	gtistop=0

	return gtistart,gtistop

def wheninFoR(ra,dec,cra,cdec,time,taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	#ra/dec = tile position
	#cra/cdec = zenith
	#sra/sdec = starting position
	#time (from FT2)

	# for a given tile, how long does it take to start observations (slewtime + waittime) 
	#     and how long is the tile visible they last?
	#	overhead=30. # settling, acceleration, slew around constraints?
	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)
	orbittime=taoconfig['orbittime']*60. # min
#	slewrate=taoconfig['slewrate']
#	overhead=taoconfig['slew_overhead']
#	delaytime=taoconfig['command_delay']
#	radius=taoconfig['FoR_radius']

	if (max(time)-min(time))>(taoconfig['norbits']*orbittime):  # only look at first orbit
		w0=np.where((time-min(time))<=(taoconfig['norbits']*orbittime))[0]
		time2=time[0:max(w0)]
		cra=cra[0:max(w0)]
		cdec=cdec[0:max(w0)]
	else:
		time2=time

	## when in FoR
	winFoR,dist=inFoR(ra,dec,cra,cdec,taoconfig=taoconfig)

	if winFoR[0]==True:
		tmin=time2[0]
		w=np.where(winFoR == False)[0]
		tmax=time2[w[0]]
	else:
		w=np.where(winFoR == True)[0]
		if len(w)>0:
			tmin=time2[w[0]]
			wmax=np.where(winFoR[w[0]:] == False)[0]
			tmax=time2[wmax[0]+w[0]]
		else:
			tmin=0
			tmax=0

	return tmin,tmax

	# if min(dist)<radius:
	# 	slewtime=min(dist)/slewrate+overhead+delaytime

	# for i in range(len(winFoR)):
	# 	if winFoR[i]==True:
	# 		slewtime[i]=dist[i]/slewrate+overhead+delaytime
	# 	else:
	# 		## if outside FoR, see how long until it enters, then how long it'll take to slew
	# 		# not quite right because more than ra moves, FoV isn't 180 for all decs


	# 		s=separation(ra,dec,cra+radius,cdec)
	# 		waittime[i]=s/360.*orbittime
	# 		d=separation(ra[i],dec[i],center[0]+s,center[1])
	# 		slewtime[i]=d/slewrate+overhead
	# 		if waittime[i] < delaytime:
	# 			waittime[i]=delaytime
	# 		o=separation(center[0]+radius,dec[i],360.-center[0]-radius,center[1]+dec[i])
	# 		ontime[i]=o/360.*orbittime

	# return slewtime,waittime,ontime

def random_sky(n=1):

	u=np.random.rand(n)
	v=np.random.rand(n)#*(1.-2*0.043)+0.043 # limits to between -66 < dec < +66

	phi=2*np.pi*u
	theta=np.arccos(2*v-1.)

	ra,dec=thetaphi2radec(theta,phi)

	# ra=np.array(theta*180./np.pi)
	# dec=np.array(phi*180./np.pi-90.)

	return ra,dec

def loginterpol(x,y,x1):

	f=interpolate.interp1d(np.log10(x),np.log10(y),bounds_error=False,fill_value="extrapolate",kind='linear')
	y1=10**f(np.log10(x1))

	return y1

def plot_sensitivities():

	tao=ascii.read('/Users/jracusin/TAO/simulations/TAO-ISS_sensitivity.dat')
	minf=[]
	for t in tao:
		x=np.array([t['flux5'],t['flux6'],t['flux7'],t['flux8'],t['flux9'],t['flux10'],t['flux12'],t['flux14'],t['flux16']])
		minf.append(min(x))
#	flux=loginterpol(tao['secs'],minf,exposure)
	dick_time=np.array(tao['secs'])
	dick_flux=np.array(minf)

	ptak=ascii.read('/Users/jracusin/TAO/simulations/Ptak/tau_flux_limits_2018_prob1e-10.csv')
	ptak_flux=np.array(ptak['flim9'])
	ptak_time=np.array(ptak['expt'])

	lien=ascii.read('/Users/jracusin/TAO/simulations/Lien/mid_bkg_9arcmin_fwhm_3.4effarea_row536_col536.txt')
	lien_time=np.array(lien['col1'])
	lien_flux10=np.array(lien['col2'])
	lien_flux50=np.array(lien['col3'])
	lien_flux90=np.array(lien['col4'])

	plot.figure()
	k=tao.keys()[1:]
#	for i in k:
	i='flux9'
#	plot.plot(dick_time,tao[i],label='Dick '+i)		

#	plot.plot(dick_time,dick_flux,label='Dick Min Flux')

	plot.plot(ptak_time,ptak_flux,label='Ptak Flux',linestyle='--')
	plot.plot(lien_time,lien_flux10,label=r'Lien midbkg, 9 arcmin, 3.4, $10\%$')
	plot.plot(lien_time,lien_flux50,label=r'Lien midbkg, 9 arcmin, 3.4, $50\%$')
	plot.plot(lien_time,lien_flux90,label=r'Lien midbkg, 9 arcmin, 3.4, $90\%$')

#	d=np.where(dick_time == 100)[0]
	p=np.where(ptak_time == 100)[0]
#	r=dick_flux[d]/ptak_flux[p]
#	print r
#	plot.plot(ptak_time,ptak_flux*r,label='Andy Flux scaled to Dick Min Flux',linestyle='--')


	plot.xscale('log')
	plot.yscale('log')
	plot.xlabel('Exposure (s)')
	plot.ylabel('0.4-4 keV Sensitivity (erg cm-2 s-1)')
	plot.legend()
	plot.savefig('Ptak_Lien_WFI_sensitivites.pdf')
	plot.show()

def wfi_sensitivity(exposure=10,taoconfig=None,configfile=None,doplot=False,sgrb=False,performance=False,energy_range=[0.4,4],dir='/Users/jracusin/TAO/simulations/'):
#	tao=ascii.read('/Users/jracusin/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',names=['time','bcount','mcount','grbflux'],data_start=1)
#	flux=loginterpol(tao['time'],tao['grbflux'],exposure)
#	tao=ascii.read('/Users/jracusin/TAO/simulations/TAO-ISS_sensitivity.dat')
	# minf=[]
	# for t in tao:
	# 	x=np.array([t['flux5'],t['flux6'],t['flux7'],t['flux8'],t['flux9'],t['flux10'],t['flux12'],t['flux14'],t['flux16']])
	# 	minf.append(min(x))
	# flux=loginterpol(tao['secs'],minf,exposure)

	sensdir='/Users/jracusin/TAO/simulations/sensitivity_curves/'

	sens_factor=1.
	if taoconfig==None:
		if configfile != None:
			taoconfig=read_tao_config(dir+configfile)
			sensfile=taoconfig['sensitivity_file']
			energy_range=[taoconfig['WFI_E_low'],taoconfig['WFI_E_high']]
			tao=ascii.read(sensfile)
			sens_factor=taoconfig['WFI_sensitivity_factor']
		else:
			if performance==True:  # performance values
				if energy_range == [0.4,4]:
					tao=ascii.read(sensdir+'TAO-ISS_sensitivity_8_0.4_4.dat')
				if energy_range == [0.3,5]:
					tao=ascii.read(sensdir+'TAO-ISS_sensitivity_8_0.3_5.dat')		

			else: # requirements value of 0.4-4.0
				tao=ascii.read(sensdir+'TAO-ISS_sensitivity_9_0.4_4.dat')
	else:
		sensfile=taoconfig['sensitivity_file']
		energy_range=[taoconfig['WFI_E_low'],taoconfig['WFI_E_high']]
		tao=ascii.read(sensfile)
		sens_factor=taoconfig['WFI_sensitivity_factor']

	### curve used in CSR
	taocsr=ascii.read('/Users/jracusin/TAO/simulations/Ptak/tau_flux_limits_2018_prob1e-10.csv')
#	minf=tao['flim9']
#	time=tao['expt']

	### latest for site visit files from Dick 

	if sgrb==False:
		minf=tao['grbflux']
	else:
		minf=tao['sgrbflux']

	time=np.array(tao['time'])*1.

	minf=minf*sens_factor

	flux=loginterpol(time,minf,exposure)

	if doplot:
		### curve used in Step 1
		tao1=ascii.read('/Users/jracusin/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',names=['time','bcount','mcount','grbflux'],data_start=1)
		plot.figure()
		fs=14
		plot.plot(tao1['time'],tao1['grbflux'],label='WFI Step-1 Sensitivity (0.3-5 keV)')
		plot.plot(taocsr['expt'],taocsr['flim9'],label='WFI CSR Sensitivity (0.4-4 keV)')
		plot.plot(tao['time'],tao['grbflux'],label='WFI Site Visit Sensitivity (0.4-4 keV)')
		plot.legend(fontsize=fs)
		plot.xscale('log')
		plot.yscale('log')
		plot.xlabel('Exposure Time (s)',fontsize=fs)
		plot.ylabel(r'Sensitivity (erg cm$^{-2}$ s$^{-1}$',fontsize=fs)
		ax = plot.gca()
		ax.tick_params(axis = 'both', which = 'major', labelsize = fs)
		ax.tick_params(axis = 'both', which = 'minor', labelsize = fs)
		plot.tight_layout()
		plot.savefig('wfi_sensitivity_step1_csr.png')
		plot.show()

	return flux

def which_maps(leodir='/Users/jracusin/TAO/simulations/simsfromLeo/',taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	data=Table.read(leodir+'weighted_sample_duty90.hdf5')
	# include only sims within <20 or >160deg of on-axis
	mas=(data['inclination']<np.radians(taoconfig['jetangle'])) | (data['inclination']>np.radians(180-taoconfig['jetangle']))
	w=np.where(mas==True)[0]
		# include only 2020+ sims
	w2=[]
	for i in range(len(w)): 
	    if '2020' in data['run'][w[i]]: w2.append(i)

	w=w[w2]
	nsims=len(w)
	print 'nsims = ',nsims

	f=[]
	fout=''
	for i in w:
#		print data[i]
		f.append(leodir+'fits_fat/'+str(i)+'.fits.gz')
		fout=fout+' '+leodir+'fits_fat/'+str(i)+'.fits.gz' 

	com='tar cvf fits_2020_inc20.tar '+fout
	return data[w],f,com

def run_sims_old2(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',doplot=True,randnum=None,dorand=False):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	outfile=configfile.split('.txt')[0]+'.out'
	print outfile

	jetangle=taoconfig['jetangle']

	# random trigger times over 2 year mission

	# select random sky map
	data=Table.read(dir+'simsfromLeo/weighted_sample_duty90.hdf5')
	# include only sims within <20 or >160deg of on-axis
	mas=(data['inclination']<np.radians(jetangle)) | (data['inclination']>np.radians(180-jetangle))
	w=np.where(mas==True)[0]
	# include only 2020+ sims
	w2=[]
	for i in range(len(w)): 
	    if '2020' in data['run'][w[i]]: w2.append(i)

	w=w[w2]
	nsims=len(w)
	print 'nsims = ',nsims

	simfactor=taoconfig['simfactor']
	ntrig=int(simfactor*nsims)

	t0 = Time('2022-01-01T0:00:00')
	t1 = Time('2023-12-24T22:20:00')
#	t1 = Time('2024-01-01')
	t = np.random.uniform(size=ntrig) * (t1 - t0) + t0
	gmst=t.sidereal_time('mean', 'greenwich')
	tmetdiff=t-Time('2001-01-01T0:00:00')
	tmet=tmetdiff.value*86400.

	outf=open(outfile,'a')
	outf.write('map, time, weight, distance, numtiles,tileperc, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart \n')
	outf.close()

	for i in range(ntrig):
		print t[i]

		## read in ft2 for that mission week + next to get zenith RA/Dec
		ft2=read_ft2(t[i].mjd)
		scra=ft2['RA_ZENITH']
		scdec=ft2['DEC_ZENITH']
		sctime=ft2['START']
		saa=ft2['IN_SAA']

		if randnum == None:
			if dorand: rm=int(round(np.random.uniform()*(nsims-1))) # select a set of random maps, ntrig long
			else: rm=i % nsims
		else: rm=randnum
		randmap=w[rm]
		weight=data['weight'][randmap]
		coinc_id=data['coinc_id'][randmap]
		distance=data['distance'][randmap]
		print rm

		print randmap,data[randmap]['lon'],data[randmap]['lat'],data[randmap]['inclination'],data[randmap]['distance'],data[randmap]['detectors'],data[randmap]['run']
		print dir+'simsfromLeo/fits_fat/'+str(randmap)+'.fits.gz'

	## read in map
		theta,phi,m,hdr=read_healpix_map(dir+'simsfromLeo/fits_fat/'+str(randmap)+'.fits.gz')
	## rotate it by random time
		
		theta,phi,m2=reduce_res(m,32)
		m3 = hp.get_interp_val(m2, theta, phi-gmst.rad[i])

		## find ft2 time that is closest to trigger time
#		t=abs(tmet[i]-sctime)
		wt=np.argmin(abs(tmet[i]-sctime))#where(t == min(t))[0][0]
		moontheta,moonphi,suntheta,sunphi=concoords(t[i],taoconfig=taoconfig)

		if doplot: hp.mollview(m3)
		centers0,rolls0,probs0,tileperc=tiling(theta,phi,m3,taoconfig=taoconfig)
		ntiles=len(rolls0)
		numtiles=ntiles
		th,ph=radec2thetaphi(scra[wt],scdec[wt])
		if doplot: 
			plot_tiles(m3,centers0,rolls0,doplot=False,taoconfig=taoconfig)
			hp.projscatter(th,ph,marker='*',color='blue')
			hp.projscatter(data['lat'][randmap]-np.pi/2.,np.pi+data['lon'][randmap]+gmst.rad[i],marker='x',color='white')
			hp.projplot(moontheta,moonphi,color='white')
			hp.projplot(suntheta,sunphi,color='yellow')
			thetacirc=np.radians(np.arange(0,365,5))
			zenithra=np.degrees(np.radians(taoconfig['zenithcon'])*np.sin(thetacirc)+np.radians(scra[wt]))
			zenithdec=np.degrees(np.radians(taoconfig['zenithcon'])*np.cos(thetacirc)+np.radians(scdec[wt]))
			zeniththeta,zenithphi=radec2thetaphi(zenithra,zenithdec)
			hp.projplot(zeniththeta,zenithphi,color='blue')			
			plot.show()

		## grab 2 orbits around random time
		wtime=np.where(((sctime-tmet[i])>0) & ((sctime-tmet[i])<(taoconfig['norbits']*taoconfig['orbittime']*60)))[0]

		## using these tiles, figure out gtis
		tmin=[]
		tmax=[]
		centers=[]
		rolls=[]
		probs=[]
		tiles=[]
		start=False
		for j in range(ntiles):
			cra,cdec=thetaphi2radec(centers0[j][0],centers0[j][1])
#			tmin0,tmax0=wheninFoR(cra,cdec,scra[wtime],scdec[wtime],sctime[wtime],taoconfig=taoconfig)
			tmin0,tmax0=wfigti(ft2[wtime],cra,cdec,taoconfig=taoconfig)
#			print tmin0-tmet[i],tmax0-tmet[i]
			# if np.append(tmin0,0)[0] > 0:
			# 	tmin.append(tmin0[0])
			# 	tmax.append(tmax0[0])
			# else:
			ngti=len(np.append(tmin0,0))-1
			tmin=np.append(tmin,tmin0)
			tmax=np.append(tmax,tmax0)
			if ngti == 1: 
				if not start: centers=centers0[j]
				else: centers=np.row_stack((centers,centers0[j]))
				start=True
				# rolls.append(rolls0[j],ngti)
				# probs.append(probs0[j],ngti)
				# tiles.append(j)
			if ngti > 1:
				if not start: centers=np.tile(centers0[j],(ngti,1))
				else: centers=np.row_stack((centers,np.tile(centers0[j],(ngti,1))))
				start=True
			rolls=np.append(rolls,np.repeat(rolls0[j],ngti))
			probs=np.append(probs,np.repeat(probs0[j],ngti))
			tiles=np.append(tiles,np.repeat(j,ngti))

		tmin=np.array(tmin)
		tmax=np.array(tmax)
		centers=np.array(centers)
		rolls=np.array(rolls)
		probs=np.array(probs)
		tiles=np.array(tiles)
		# remove tiles that are never visible in window
		w0=np.where(tmin != 0)[0]
		s=np.argsort(tmin[w0])
		w0=w0[s]
		if len(w0)>1:
			tmin=tmin[w0]
			tmax=tmax[w0]
			ntiles=len(w0)
			centers=centers[w0]
			rolls=rolls[w0]
			probs=probs[w0]
			tiles=tiles[w0]

#		tstart,tstop=tile_score(sctime[wtime],tmin,tmax,probs,tmet[i],taoconfig=taoconfig)
		tstart,tstop=evensplit(sctime[wtime],tmin,tmax,probs,tmet[i],tiles,taoconfig=taoconfig)
		s=np.argsort(tstart)
		ntiles=len(s)
		if ntiles>1:
			tmin=tmin[s]
			tmax=tmax[s]
			centers=centers[s]
			rolls=rolls[s]
			probs=probs[s]
			tstart=tstart[s]
			tstop=tstop[s]

		print 'tmin,tmax = ',tmin,tmax
		print 'tmin,tmax -tmet = ',tmin-tmet[i]
		print tmax-tmet[i]
		print 'tile windows - ',tiles

		exposure=tstop-tstart
#		s=np.argsort(tstart)

		# remove tiles with zero exposure
		w0=np.where((tstart >= 0) & (exposure >= taoconfig['mintilesec']))[0]
		if len(w0)>0:
			s=np.argsort(tstart[w0])
			w0=w0[s]
			exposure=exposure[w0]
			tstart=tstart[w0]
			tstop=tstop[w0]
			tmin=tmin[w0]
			tmax=tmax[w0]
			centers=centers[w0]
			rolls=rolls[w0]
			probs=probs[w0]
			ntiles=len(w0)
		else:
			exposure=[]
	
		if len(exposure) ==0:
			print 'NO TILES ARE OUT OF CONSTRAINT AND CONTAIN THE GRB'
			print 'output: ',randmap,t[i],weight,distance,numtiles,tileperc,0,0,0,0,0,0,0,0,0,0,0
			outf=open(outfile,'a')
			outf.write(str(randmap)+sp+str(t[i])+sp+str(weight)+sp+str(distance)+sp+str(numtiles)+sp+str(tileperc)+sp+'0,0,0,0,0,0,0,0,0,0,0'+' \n')
			outf.close()

#			print 'NO TILES ARE OUT OF CONSTRAINT'
#			print 'output: ',0,0,0,0,0,0,0,0,0
#			where_saa(ft2,tmet[i])
#			wsaa=np.where(saa[wtime] == True)[0]
#			print 'SAA = ', min(sctime[wtime[wsaa]])-tmet[i],max(sctime[wtime[wsaa]])-tmet[i]
		else:
			print 'exposure = ',exposure
			print 'TSTART, TSTOP = ', tstart-tmet[i],tstop-tmet[i]
			where_saa(ft2,tmet[i])
#			wsaa=np.where(saa[wtime] == True)[0]
#			print 'SAA = ', min(sctime[wtime[wsaa]])-tmet[i],max(sctime[wtime[wsaa]])-tmet[i]
			print 'probs = ',probs

			ntiles=len(tmin)
			tilesra=[]
			tilesdec=[]
			slewtimes=[]

			# plot tile with FoR at time, real position
			for k in range(ntiles):
				j=k#s[k]
	#			l=q[k]
				tilera,tiledec=thetaphi2radec(centers[j][0],centers[j][1])
				# sub=0.
				# if np.pi+data['lon'][randmap]+gmst.rad[i]>2*np.pi: sub=2*np.pi
				# add=0.
				# if data['lat'][randmap]>np.pi/2.: add=np.pi
				realra,realdec=thetaphi2radec(data['lat'][randmap]-np.pi/2.,data['lon'][randmap]+gmst.rad[i])
				realdec=-realdec
				if realdec>90: realdec=realdec-180.
				if realra>360: realra=realra-360.
				if realdec<-90: realdec=realdec+180.
				if realra<0: realra=realra+360.
	#			print 'tile, real = ',tilera,tiledec,realra,realdec
				sep=separation(tilera,tiledec,realra,realdec)

	#			print 'probs',probs[j]
	#			t=abs(tstart[l]-sctime+taoconfig['slew_overhead'])
				wt=np.argmin(abs(tstart[j]-sctime))#+taoconfig['slew_overhead']))
				#where(t == min(t))[0][0]
	#			print round(tstart[l]-tmet[i]),round(sctime[wt]-tmet[i]),scra[wt],scdec[wt]

				if k == 0:
					# starting position of survey tile
					pra,pdec=survey_pointing(scra[wt],scdec[wt],taoconfig=taoconfig)
					pth,pph=radec2thetaphi(pra,pdec)
					sfra=pra
					sfdec=pdec

				slewtime=separation(sfra,sfdec,tilera,tiledec)/taoconfig['slewrate']
				slewtimes.append(slewtime)
				sfra=tilera
				sfdec=tiledec
				tilesra.append(tilera)
				tilesdec.append(tiledec)
				print 'slewtime = ',slewtime
				print 'sep from real = ',sep

				mask=m3.astype(np.bool)
				mask[:]=False
				m4=hp.ma(m3)
				m4.mask=mask
				outmap=apply_FoR(m4,theta,phi,scra[wt],scdec[wt],taoconfig=taoconfig,FoRvalue=0.01)
	#			outmap.mask=mask
	#			theta=np.radians(np.arange(0,365,1))
				if doplot:
					hp.mollview(outmap,title='Tstart='+str(round(tstart[j]-tmet[i]))+', Tstop='+str(round(tstop[j]-tmet[i]))+', exposure = '+str(round(exposure[j])))
	#			hp.projplot(np.radians(taoconfig['FoR_radius'])*np.sin(t)+np.radians(scra[wtime[wt]]),np.radians(taoconfig['FoR_radius'])*np.cos(t)+np.radians(scdec[wtime[wt]]))
					plot_tile(0,centers[j],rolls[j],doplot=False,taoconfig=taoconfig)
					th,ph=radec2thetaphi(scra[wt],scdec[wt])
					## plot zenith
					hp.projscatter(th,ph,marker='*',color='blue')
					## plot real source position
					hp.projscatter(data['lat'][randmap]-np.pi/2.,np.pi+data['lon'][randmap]+gmst.rad[i],marker='x',color='white')			
					hp.projscatter(centers[j][0],centers[j][1],marker='*',color='orange')
					hp.projscatter(pth,pph,marker='p',color='green')
					hp.projplot(moontheta,moonphi,color='white')
					hp.projplot(suntheta,sunphi,color='yellow')
					zenithra=np.degrees(np.radians(taoconfig['zenithcon'])*np.sin(thetacirc)+np.radians(scra[wt]))
					zenithdec=np.degrees(np.radians(taoconfig['zenithcon'])*np.cos(thetacirc)+np.radians(scdec[wt]))
					zeniththeta,zenithphi=radec2thetaphi(zenithra,zenithdec)

					hp.projplot(zeniththeta,zenithphi,color='blue')

					plot.show()

			dist=separation(realra,realdec,tilesra,tilesdec)
			wreal=np.where(dist<np.sqrt(2.)/2.*taoconfig['detsize'])[0]
			if len(wreal)>0:
				realtile=wreal[np.argmin(tstart[wreal])]
				print realtile,np.argmin(dist)

				print 'which tile = ',realtile,wreal#,dist
				slewtimes=np.array(slewtimes)

				print 'map, time, weight, distance, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart'
				print 'output: ',randmap,t[i],weight,distance, numtiles, tileperc, tilesra[realtile],tilesdec[realtile],realra,realdec,dist[realtile],\
					rolls[realtile],exposure[j],slewtimes[realtile],exposure[j]-slewtimes[realtile]-taoconfig['slew_overhead'],\
					tstart[realtile]-tmet[i],tstart[realtile]-tmet[i]+slewtimes[realtile]+taoconfig['slew_overhead']
				outf=open(outfile,'a')
				sp=','
				outf.write(str(randmap)+sp+str(t[i])+sp+str(weight)+sp+str(distance)+sp+str(numtiles)+sp+str(tileperc)+sp+\
					str(tilesra[realtile])+sp+str(tilesdec[realtile])+sp+str(realra)+sp+str(realdec)+sp+str(dist[realtile])+\
					sp+str(rolls[realtile])+sp+str(exposure[j])+sp+str(slewtimes[realtile])+sp+str(exposure[j]-slewtimes[realtile]-taoconfig['slew_overhead'])+\
					sp+str(tstart[realtile]-tmet[i])+sp+str(tstart[realtile]-tmet[i]+slewtimes[realtile]+taoconfig['slew_overhead']+taoconfig['command_delay'])+' \n')
				outf.close()

			else:
				print 'NO TILES ARE OUT OF CONSTRAINT AND CONTAIN THE GRB'
				print 'output: ',0,0,0,0,0,0,0,0,0,0,0
				outf=open(outfile,'a')
				outf.write(str(randmap)+sp+str(t[i])+sp+str(weight)+sp+str(distance)+sp+'0,0,0,0,0,0,0,0,0,0,0'+' \n')
				outf.close()

		if doplot:	
			keepgoing=raw_input('Next Sim? (Y/n/s) ').upper()
			if keepgoing=='S': return

def run_sims_old(exposure=100,overhead=30,delaytime=0):

	## given single center (0,0), and n ra/decs what are slewtime or waittime+slewtime
	## what is flux of scaled light curves at each time?
	## with large n, get distribution of n*ngrbs of flux compared to sensitivity
	## run with several slewrates
	slewrates=np.array([1,2,4])
	n=500	
	sensitivity=wfi_sensitivity(exposure=exposure)
	print 'Sensitivity in '+str(exposure)+' s: ',sensitivity
	print 'Delay time to start slew due to TOO or onboard detection: '+str(delaytime)+' s'
	print 'Overhead (settling, slew acceleration/deceleration): '+str(overhead)+' s'

	grbox=grb_catalogs.load_GRBOX(nointernet=True)
	grbs=np.array(['111117A','100625A','100206A','100117A','080905A'])
	z=[2.211,0.452,0.407,0.915,0.122]
	m1,m2=grb_catalogs.match_catalogs_name(grbox['GRB'],grbs)
	grbox['z'][m1]=z

	z=np.zeros(len(grbox))
	mask=grbox['z'].mask
	for i in range(len(grbox)):
		if (mask[i]==False) and (grbox['z'][i] != 'low'):
			z[i]=float(grbox['z'][i])

	bat=grb_catalogs.load_BAT()
	w=np.where(bat['T90']!='N/A')
	bat=bat[w]
	t90=np.array(bat['T90']).astype('float')

	m1,m2=grb_catalogs.match_catalogs_name(np.core.defchararray.add('GRB',grbox['GRB']),bat['GRBname'])
	s=np.where((t90[m2] <= 2.) & (t90[m2] > 0) & (z[m1]>0))
	sGRBs=bat['GRBname'][m2][s]
	print len(s[0])
	t90=t90[m2][s]
	z=z[m1][s]#grbox['z'][m1][s]
	lumdist=cosmo.luminosity_distance(z).value
	pc2cm=3.08568025e18	
	dist=lumdist*1e6*pc2cm
	distgw=440e6*pc2cm  ## 440 Mpc

	grbdir='/Users/jracusin/Swift/GRBfits/GRBs/'
	oldgrbdir='/Users/jracusin/GRBs/'


	eng=np.linspace(0.3,10.,100)
	de=eng[1]-eng[0]
	wtao=np.where(eng <= 5.0)


#	print sGRBs

	for slewrate in slewrates:
		center=np.array([0,0])
		ra,dec=random_sky(n)

		slewtime,waittime,ontime=wheninFoR(ra,dec,center=center,slewrate=slewrate,overhead=overhead,delaytime=delaytime)
		times=slewtime+waittime

		fig=plot.figure()
		m=plot.subplot(111,projection='aitoff')
#		c=slewtime/(max(slewtime)-min(slewtime))
		wra=np.where(ra >180)
		rarad=ra*np.pi/180
		rarad[wra]=rarad[wra]-2*np.pi
		pm=m.scatter(rarad,dec*np.pi/180.,c=times,cmap=cm.rainbow,alpha=0.7)
		m.axes.xaxis.set_ticklabels([])
		m.grid(True)
		plot.colorbar(pm,orientation='horizontal',label='Slew Time + Wait Time + Delay Time + Overhead (s)')

		det=[]
		detfrac=[]
		ngrbs=len(sGRBs)

		for i in range(len(sGRBs)):

			p=fit_lc.read_lcfit(dir=grbdir+sGRBs[i]+'/')
			sp=fit_lc.read_specfit(dir=oldgrbdir+sGRBs[i]+'/')
			if (p != {}) & (sp != {}):
			# scale by distance, and k-correct from 0.3-10 keV to 0.3-5.0 keV
				if 'PC' in sp.keys(): spec=sp['PC'] 
				else: spec=sp['WT']
				conv=np.sum(fit_functions.pow(eng[wtao],*[1.,spec.gamma])*de)/np.sum(fit_functions.pow(eng,*[1.,spec.gamma])*de)
				k=grb_catalogs.kcorr(0.3,10.,0.3,5,-spec.gamma,z[i])
				r=spec.flux/spec.rate*conv*dist[i]**2/distgw**2/k
#				print spec.flux/spec.rate,conv,dist[i]**2/distgw**2,k

				w=np.where(exposure > ontime)
				exptime=np.repeat(exposure,n)
				if w != None:
					exptime[w]=ontime


				# flux of this GRB at all the possible times from slews to random positions on sky
				fscaled=fit_functions.call_function(p.model,(times+exptime/2.)/(1.+z[i]),*p.par)*r

				# compare flux to sensitivity in 100 s exposure at the time that obs start
				sensitivity=wfi_sensitivity(exposure=exptime)
				wdet=np.where(fscaled >=sensitivity)
				det=np.append(det,wdet)
			else:
				print sGRBs[i]
				ngrbs=ngrbs-1

		detfrac=np.append(detfrac,len(det)/float(ngrbs*n))
		print 'Surviving GRBs',ngrbs
		print 'Slew Rate',slewrate,' deg/s'
		print 'Det Frac',detfrac
		plot.title('Detection Fraction = '+str(detfrac[0]))
#		plot.title('Slew rate = '+str(slewrate)+', Exposure = '+str(exposure)+'s, Overhead = '+str(overhead)+'s')
		plot.savefig('Skyplot_slewrate'+str(slewrate)+'_exposure'+str(exposure)+'_overhead'+str(overhead)+'_delaytime'+str(delaytime)+'.png', bbox_inches='tight')
		plot.close()


		# make plots of sky map of random points
		# make plot of all scaled LCs
		# make plot of distribution of slewtime & delay time + slewtime
