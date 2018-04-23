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

def gwrates(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	sims=read_sims(configfile=configfile,dir=dir)

	### rate from Abbott et al.
	gwrate=taoconfig['bns_rate']*1e-3 # Gpc-3 yr-1 to Mpc-3 Myr-1
	print('GW rate = ',gwrate,' Mpc-3 Myr-1')
	gwrate=gwrate*1e-6 # Myr->yr
	print('GW rate = ',gwrate,' Mpc-3 yr-1')

	## 200 Mpc (design range) * 1.5 (on-axis) * 1.5 (sub-threshold)
	range=200.*1.5*1.5
	print('Range = ',range,' Mpc')
	volume=4./3.*np.pi*(200.*1.5*1.5)**3 #Mpc3
	totrate=gwrate*volume #BNS yr-1 in volume
	print('All-sky all-orientation rate = ',totrate,' yr-1')

	## need to account for beaming
	allsky=4.*np.pi*(180./np.pi)**2.
	scale=2*np.pi*taoconfig['jetangle']**2/allsky
	print('Scale by opening angle = ',scale)

	totrate=totrate*scale  # all sky rate
	print('All-sky pointed-at-us rate = ',totrate,' yr-1')

	oldrate=10*(1e-3)**3*volume
	print('Old all-sky rate = ',oldrate,' yr-1')

### NOT RIGHT, NEED TO ACCOUNT FOR BOTH WEIGHTED RATE, ACTUAL RATE, AND EFFICIENCY AS FUNCTION OF THOSE RATES
	### currently double counting
	rate=sims['weight']
	obsrate=sum(rate)/taoconfig['simfactor']
	print('All-sky Leo rate = ',obsrate,' yr-1')

	detfrac,wdet=throw_grbs(configfile=configfile,dir=dir,taoconfig=taoconfig)
	wfirate=obsrate*detfrac*0.87
	print('WFI rate = ',wfirate,' yr-1')
	print('WFI rate errors = ',wfirate/1540*(1540-1220.),wfirate/1540*(1540+3200.),' yr-1')

	wfirate2=sum(rate[wdet])/taoconfig['simfactor']*0.87
	print('WFI rate (weighted)= ',wfirate2,' yr-1')
	print('WFI rate errors (weighted) = ',wfirate2/1540*(1540-1220.),wfirate2/1540*(1540+3200.),' yr-1')


def throw_grbs(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	sims=read_sims(configfile=configfile)
	nsims=len(sims)
#	print nsims

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
	wtao=np.where(eng <= 5.0)

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
		# scale by distance, and k-correct from 0.3-10 keV to 0.3-5.0 keV
		if 'PC' in sp.keys(): spec=sp['PC'] 
		else: spec=sp['WT']
		conv=np.sum(fit_functions.pow(eng[wtao],*[1.,spec.gamma])*de)/np.sum(fit_functions.pow(eng,*[1.,spec.gamma])*de)
		k1=grb_catalogs.kcorr(0.3,10.,0.3,5,-spec.gamma,z[i])
		k2=grb_catalogs.kcorr(0.3,5,0.3,5,-spec.gamma,zgw[r])

		conv=spec.flux/spec.rate*conv*dist[i]**2/distgw[r]**2/k1*k2  #distgw from map
#				print spec.flux/spec.rate,conv,dist[i]**2/distgw**2,k

		# flux of this GRB at time of obs
		tbin=np.array([sims['nettstart'][r]+sims['netExposure'][r]/2.])
#			zgw=sim['distgw']  ### fix this, invert dist/z, kcorr, get fscaled
		fscaled=fit_functions.call_function(p.model,tbin/(1.+z[i])*(1.+zgw[r]),*p.par)*conv

		# compare flux to sensitivity in 100 s exposure at the time that obs start
		sensitivity=sims['sensitivity'][r]
		#sensitivity=wfi_sensitivity(exposure=sim['netExposure'][r])
		wdet=(fscaled >=sensitivity)
		det[r]=wdet

	det=det.astype(np.bool)
	wdet2=np.where((det == True) & (sims['netExposure']>0) & (sims['nettstart']>0))[0]
	detfrac=float(len(wdet2))/float(nsims)
#	print 'Surviving GRBs',ngrbs
	print 'Det Frac',detfrac

	# w=np.where(z != 0.7)[0]
	# s=np.argsort(sGRBs[w])
	# w=w[s]
	# for i in w: print sGRBs[i],z[i]

	return detfrac,wdet2#,sGRBs,z

def run_sims(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/',doplot=True,istart=0,noGTM=False):

	if taoconfig==None:
		taoconfig=read_tao_config(filename=dir+configfile)

	outfile=configfile.split('.txt')[0]+'.out'
	print outfile

	jetangle=taoconfig['jetangle']

	# random trigger times over 2 year mission

	# select random sky map
	nsims=taoconfig['simrate']
	print 'nsims = ',nsims

	simdir=taoconfig['simdir']
	ntrig=len(glob.glob(simdir+'*.nside32.fits.gz'))
#	ntrig=int(nsims)

	outf=open(dir+outfile,'a')
	outf.write('map, time, distance, snr, numtiles, tileperc, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart \n')
	outf.close()

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
		snrs=np.array([float(st.replace('\n','').split('=')[1]) for st in history if 'snr=' in st.replace('\n','')])# | ('s\nnr=' in st) | ('sn\nr=' in st))])
		print 'SNR = ',snrs
		wsnr=np.where(snrs >= 4.0)[0]
#		wsnr=np.argsort(snrs)[::-1]
#		wsnr=wsnr[0:2]
#		detectors=','.join(detectors[wsnr])
		snr=np.sqrt(np.sum(snrs[wsnr]**2))
		print 'Network SNR = ',snr
		ngwdet=len(wsnr)

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
				centers0,rolls0,probs0,tileperc=tiling(theta,phi,m3,taoconfig=taoconfig)
				## grab 2 orbits around trigger time
				startdelay=taoconfig['command_delay']+iss_comm_delay()[0]
				wtime=np.where(((sctime-tmet-startdelay)>0) & ((sctime-tmet-startdelay)<(taoconfig['norbits']*taoconfig['orbittime']*60)))[0]
				settlingtime=taoconfig['slew_overhead']
				minexposure=taoconfig['mintilesec']
				wt=np.argmin(sctime[wtime])#abs(tmet-sctime+startdelay))
				wt=wtime[wt]
			if ((tile == 1) & (not gwtiling)): 
				if noGTM==False: print('NO OBSERVATIONS BECAUSE SNR<12 AND NO GTM OBSERVATION ')
				if noGTM==True: print('NO OBSERVATIONS BECAUSE SNR<12 AND NOT USING GTM')
				print 'output: ',coinc_id,t.isot,distance,snr,0,0,0,0,0,0,0,0,0,0,0
				outf=open(outfile,'a')
				sp=', '
				outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(snr)+sp+'0,0,0,0,0,0,0,0,0,0,0,0,0'+' \n')
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
				plot.show()


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
			if ((tile==0) & (gtmtiling) & (len(w0)>0)): 
				tstart,tstop=shorttiles(sctime[wtime],tmin,tmax,tmet,tiles,taoconfig=taoconfig)
				s=np.arange(0,len(tmin))
			if ((tile==1) & (gwtiling)): 
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

				exposure=tstop-tstart
		#		s=np.argsort(tstart)
				# remove tiles with zero exposure
				w0=np.where((tstart >= 0) & (exposure >= minexposure))[0]#taoconfig['mintilesec']))[0]
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
					print 'output: ',coinc_id,t.isot,distance,snr,numtiles,tileperc,0,0,0,0,0,0,0,0,0,0,0
					outf=open(outfile,'a')
					sp=', '
					outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(snr)+sp+str(numtiles)+sp+str(tileperc)+sp+'0,0,0,0,0,0,0,0,0,0,0'+' \n')
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

							plot.show()

					dist=separation(realra,realdec,tilesra,tilesdec)
					wreal=np.where(dist<np.sqrt(2.)/2.*taoconfig['detsize'])[0]
					if len(wreal)>0:
						realtile=wreal[np.argmin(tstart[wreal])]
						print realtile,np.argmin(dist)

						print 'which tile = ',realtile,wreal#,dist
						slewtimes=np.array(slewtimes)+settlingtime

						print 'map, time, distance, snr, tileRA, tileDec, RealRA, RealDec, offset, roll, exposure, slewtime, netExposure, tstart, nettstart'
						print 'output: ',coinc_id,t.isot,distance,snr, numtiles, tileperc, tilesra[realtile],tilesdec[realtile],realra,realdec,dist[realtile],\
							rolls[realtile],exposure[j],slewtimes[realtile],exposure[j]-slewtimes[realtile],\
							tstart[realtile]-tmet,tstart[realtile]-tmet+slewtimes[realtile],startdelay
						outf=open(outfile,'a')
						sp=', '
						outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(snr)+sp+str(numtiles)+sp+str(tileperc)+sp+\
							str(tilesra[realtile])+sp+str(tilesdec[realtile])+sp+str(realra)+sp+str(realdec)+sp+str(dist[realtile])+\
							sp+str(rolls[realtile])+sp+str(exposure[j])+sp+str(slewtimes[realtile])+sp+str(exposure[j]-slewtimes[realtile])+\
							sp+str(tstart[realtile]-tmet)+sp+str(tstart[realtile]-tmet+slewtimes[realtile]+startdelay)+' \n')
						outf.close()

					else:
						print 'NO TILES ARE OUT OF CONSTRAINT AND CONTAIN THE GRB'
						print 'output: ',coinc_id,t.isot,distance,snr,0,0,0,0,0,0,0,0,0,0,0
						outf=open(outfile,'a')
						sp=', '
						outf.write(str(coinc_id)+sp+str(t.isot)+sp+str(distance)+sp+str(snr)+sp+'0,0,0,0,0,0,0,0,0,0,0,0,0'+' \n')
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

	if outfile== None: outfile=configfile.split('.txt')[0]+'.out'
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
	sensitivity=[wfi_sensitivity(exposure=float(e)) for e in exptime] ## will replace with Amy's code
	sims['sensitivity']=sensitivity
	sims['sensitivity'][np.where(sims['sensitivity']>1)]=0
	## need to add column for weight*rate - weight added automatically now
	## then need to throw scaled light curves at it

	return sims

def make_small_files(taoconfig=None,configfile='tao_config_v1.txt',dir='/Users/jracusin/TAO/simulations/'):

	if taoconfig==None:
		taoconfig=read_tao_config(dir+configfile)

	simdir=taoconfig['simdir']
	nsims=taoconfig['simrate']
	print 'nsims = ',nsims

	ntrig=int(nsims)

	for i in range(603,1679):

		datafile=simdir+str(i)+'.fits.gz'
		outdatafile=simdir+str(i)+'.nside32.fits.gz'
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
		exposures=tmax-tmin
		tstartmin=tmin-tmet
	if ntiles >1:
		exposures=np.repeat(taoconfig['mintilesec'],ntiles+extratiles)

	tstart=min(tmin)+exposures*np.arange(ntiles+extratiles)
	tstop=min(tmin)+exposures*(np.arange(ntiles+extratiles)+1)

	print tmet
#	whichtiles=np.array(np.repeat(-1,ntiles))
	whichtiles=[]
	wtind=[]
	done=np.zeros(ntiles)
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

		hp.projplot(np.radians(wfirot[:,0]),np.radians(wfirot[:,1]),color='red')

	if doplot: 
		plot.show()


def plot_tile(map,center,roll,doplot=True,taoconfig=None):
#	if doplot: hp.mollview(map=map)#,flip='astro')
	hp.graticule()

	wfi,wfirot=wfi_fov(wficenter=np.degrees(center),wfiroll=roll,taoconfig=taoconfig)

	hp.projplot(np.radians(wfirot[:,0]),np.radians(wfirot[:,1]),color='red')

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

def sph_cap(ang):
	return 2*np.pi*(1-np.cos(ang*np.pi/180.))/(4.*np.pi)

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

	if sunsep>6:  # night time 
		spra=np.array([-1])
		spdec=np.array([-1])
	else:
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

def random_sky(n):

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

def wfi_sensitivity(exposure=10):
	tao=ascii.read('/Users/jracusin/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',names=['time','bcount','mcount','grbflux'],data_start=1)
	flux=loginterpol(tao['time'],tao['grbflux'],exposure)

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
