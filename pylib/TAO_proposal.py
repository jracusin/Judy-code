#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to make plots for the TAO Proposal Section D

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

def loginterpol(x,y,x1):

	f=interpolate.interp1d(np.log10(x),np.log10(y),bounds_error=False,fill_value="extrapolate",kind='linear')
	y1=10**f(np.log10(x1))

	return y1

def interpol_sens(twant,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux):

	if len(twant) is 1:
		twant=np.repeat(twant,4)
	
	sensitivity=[]
	sensitivity.append(loginterpol(time,lob_flux,twant[0]))
	sensitivity.append(loginterpol(time,tao_wfi_flux,twant[1]))
	sensitivity.append(loginterpol(time,tap_wfi_flux,twant[2]))
	sensitivity.append(loginterpol(time,tap_xri_flux,twant[3]))
	sensitivity=np.array(sensitivity)

	return sensitivity

def compare_sensitivity(time,tao_wfi_flux,daily_exptimes):

#	crab=2.4e-8 # erg cm-2 s-1 (2-10 keV)
#	crab_2_30=crab*1.689 #2-30 keV
#	crab_25_10=crab*0.8644 #2.5-10 keV

	## WFI
	ltime=[2021,2024,2025]
	lflux=loginterpol(time,tao_wfi_flux,daily_exptimes[1])
	print(daily_exptimes[1])
	lfov=18.6*18.6

	## MAXI/SSC
	mtime=[2009.75,2018]
  	#mflux=3.2e-10
  	#maxi2lob=jap.pimms([2,30],[0.3,5],2,5e20)#0.7641 #2-30 to 0.3-5 assuming PL=2, NH=1e21
  	mexp=[96.*60.,86400*7.]
 # 	msens=[20e-3,2e-3]
  	meng=[0.5,10.]
 # 	mflux=loginterpol(mexp,msens,86400.*0.7)
 # 	print 'MAXI flux crab: ', mflux
 	mflux=50e-3 #mCrab from Tsunemi et al. (2011)
	mflux=mflux*jap.crab(meng[0],meng[1])*jap.pimms(meng,[0.4,4],2,5e20)
	mfov=1.5*90. # deg

	## Swift BAT
	btime=[2004.9,2018]
	bexp=np.array([1.3760613,4.662506,130.96564,13154.31,811265.2,6724503.5])
	bsens=np.array([393.8972,212.26125,39.98884,4.123951,0.51980466,0.18457437])*1e-3
	b1day=86400.*0.11*0.85 #1.4 sr FoV, 85% not in SAA
	bflux=loginterpol(bexp,bsens,b1day)
	# print bflux
	# bflux=5.3e-3
	# print bflux
	bflux=bflux*jap.crab(15,150)*jap.pimms([15,150],[0.4,4],2,0)#5e20)
	print jap.pimms([15,150],[0.4,4],2,0)
	print jap.pimms([15,150],[0.4,4],2,5e20)

	## RXTE ASM
	atime=[1996,2011]
	aflux=4e-10
	aeng=[2.,12.]
	aeff=0.4
	aflux=20e-3*jap.crab(aeng[0],aeng[1])*jap.pimms(aeng,[0.4,4],2,5e20)
	afov=12*110. #deg

	#astrosat
	astime=[2015.8,2021]
	asflux=28e-3*jap.crab(2.5,10)*jap.pimms([2.5,10],[0.4,4],2,5e20) ## mCrab for SSM instrument (2.5-10 keV, FoV 10x90 deg) 
	asfov=10*90. # deg

	xrange=[2016,2026]
	yrange=[1e-11,4e-9]
	fig=plot.figure(figsize=(8,5))

	print 'BAT: ',bflux
	print 'RXTE: ',aflux
	print 'MAXI: ', mflux
	print 'WFI: ',lflux
	print
	print 'BAT / WFI: ', bflux/lflux
	print 'RXTE / WFI: ', aflux/lflux
	print 'MAXI / WFI: ', mflux/lflux
	print

	plot.plot(ltime[0:2],np.repeat(lflux,2),lw=2,color='black')
	plot.plot(ltime[1],lflux,marker=r'$\blacktriangleright$',markersize=10,color='black')
	plot.plot(mtime[0:2],np.repeat(mflux,2),lw=2,color='black')
	plot.plot(mtime[1],mflux,marker=r'$\blacktriangleright$',markersize=10,color='black')
#	plot.plot(btime,np.repeat(bflux,2),lw=2,color='black')
#	plot.plot(btime[1],bflux,marker=r'$\blacktriangleright$',markersize=10,color='black')
	plot.plot([xrange[0],xrange[0]+4],np.repeat(aflux,2),'r--',lw=2,color='black')
	plot.plot(astime[1],asflux,marker=r'$\blacktriangleright$',markersize=10,color='black')
	plot.plot(astime,np.repeat(asflux,2),lw=2,color='black')

	plot.annotate('ISS-TAO/WFI',xy=(ltime[0],lflux*1.2),xycoords='data',fontsize=22,color='black')
#	plot.annotate('Swift/BAT',xy=(btime[0]+13.3,bflux*0.9),xycoords='data',fontsize=18,color='black')
	plot.annotate('MAXI/SSC',xy=(mtime[0]+8.5,mflux*0.9),xycoords='data',fontsize=18,color='black')
	plot.annotate('RXTE/ASM',xy=(xrange[0]+4.2,aflux*0.9),xycoords='data',fontsize=18,color='black')
	plot.annotate('ASTROSAT/SSM',xy=(xrange[0]+5.2,asflux*0.9),xycoords='data',fontsize=18,color='black')

	plot.xlim(xrange)
	plot.ylim(yrange)
	plot.yscale('log')
	plot.xlabel('Year')
	plot.axes().xaxis.set_minor_locator(MultipleLocator(1))
	plot.ylabel(r'0.4-4.0 keV Sensitivity (erg cm$^{-2}$ s$^{-1}$)')
	plot.title('Full Sky Sensitivity in 1 day')
	plot.savefig('compare_tao.eps')
	plot.savefig('compare_tao.png')
	plot.show()

	# fig=plot.figure()
	# plot.plot([0,2000],[1e-11,1e-8])
	# plot.yscale('log')
	# plot.xlabel('FoV (deg)')
	# plot.ylabel(r'0.3-5.0 keV Sensitivity (erg cm$^{-2}$ s$^{-1}$)')
	# plot.scatter(lfov,lflux,color='blue')
	# plot.scatter(afov,aflux,color='green')
	# plot.scatter(asfov,asflux,color='red')

	# plot.show()


def plot_sensitivity(time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux,jordan_wfi_flux,onlyTAO=False):

	fig=plot.figure()

	if onlyTAO:
		plot.plot(time,tao_wfi_flux,label='TAO WFI',lw=2)
		plot.ylim([1e-13,1e-7])
	else:
		#plot.plot(time,lob_flux,label='2014 ISS-Lobster')
		#plot.plot(time,tao_wfi_flux,color='red',label='TAO WFI')
		plot.plot(time,tap_wfi_flux,color='orange',label='TAP WFI')
		plot.plot(time,tap_xri_flux,color='magenta',label='TAP XRT')
		#plot.plot(time,jordan_wfi_flux,color='green',label=r'ISS-Lobster/$1.5^2$')
		plot.ylim([1e-17,1e-7])

	plot.legend(loc='upper right')
	plot.xscale('log')
	plot.yscale('log')
	plot.xlabel('Exposure Time (s)')
	plot.ylabel(r'Sensitivity (erg cm$^{-2}$ s$^{-1}$)')
	plot.yticks(np.logspace(-17,-7,11))
#	plot.savefig('Lob_TAO_XRI_sensitivities.png', bbox_inches='tight')
	plot.savefig('TAP_sensitivities.png', bbox_inches='tight')
	plot.show()
	plot.close()

def merger_plot():

	fig=plot.figure()
	xrange=[1,1e10]
	yrange=[1,1e10]
	plot.plot(xrange,yrange,'r--',color='black')

	plot.xlim(xrange)
	plot.ylim(yrange)
	plot.axes().set_aspect('equal')
	plot.xscale('log')
	plot.yscale('log')
	plot.xlabel('Companion 1 Mass (M$_\odot$)')	
	plot.ylabel('Companion 2 Mass (M$_\odot$)')	

	plot.annotate('NS-NS Merger (LIGO/Virgo)',xy=(20.,3.),xycoords='data',fontsize=16,color='black')
	plot.annotate('NS-BH Merger (LIGO/Virgo)',xy=(2,100.),xycoords='data',fontsize=16,color='black')
	plot.annotate('BH-BH Merger (LIGO/Virgo)',xy=(100,15),xycoords='data',fontsize=16,color='black')
	plot.annotate('SMBH-SMBH Binary (PTA)',xy=(1e1,1e7),xycoords='data',fontsize=16,color='black')
	plot.annotate('SMBH-SMBH Merger (LISA)',xy=(40,8e8),xycoords='data',fontsize=16,color='black')


	im1=plot.imread('/Users/jracusin/Lobster/TAP/images/NSNS.jpg')
	newax1=fig.add_axes([0.22,0.12,0.05,0.05],anchor='NE')
	newax1.imshow(im1)
	newax1.set_xticks([])
	newax1.set_yticks([])

	im1=plot.imread('/Users/jracusin/Lobster/TAP/images/BHBH.jpg')
	newax1=fig.add_axes([0.27,0.17,0.05,0.05],anchor='NE')
	newax1.imshow(im1)
	newax1.set_xticks([])
	newax1.set_yticks([])

	im1=plot.imread('/Users/jracusin/Lobster/TAP/images/BHBH.jpg')
	newax1=fig.add_axes([0.22,0.17,0.05,0.05],anchor='NE')
	newax1.imshow(im1)
	newax1.set_xticks([])
	newax1.set_yticks([])

	im1=plot.imread('/Users/jracusin/Lobster/TAP/images/SMBH_SMBH.jpg')
	#1e6 - LISA
	newax1=fig.add_axes([0.5,0.51,0.1,0.1],anchor='NE')
	newax1.imshow(im1)
	newax1.set_xticks([])
	newax1.set_yticks([])

	im1=plot.imread('/Users/jracusin/Lobster/TAP/images/SMBH_SMBH.jpg')
	#1e9 - PTA
	newax1=fig.add_axes([0.7,0.75,0.1,0.1],anchor='NE')
	newax1.imshow(im1)
	newax1.set_xticks([])
	newax1.set_yticks([])

	#im1=plot.imread('/Users/jracusin/Lobster/TAP/images/tidal_disruption.jpg')
	#newax1=fig.add_axes([0.22,0.75,0.1,0.1],anchor='NE')
	#newax1.imshow(im1)
	#newax1.set_xticks([])
	#newax1.set_yticks([])

	plot.savefig('/Users/jracusin/Lobster/TAP/merger_plot.eps')
	plot.savefig('/Users/jracusin/Lobster/TAP/merger_plot.png')
	plot.show()
	plot.close()

def GW_plot():

	fig=plot.figure()

	xrange=[1e-10,1e5]
#	plot.plot(xrange,[0,0],ms = 10, alpha=1)[0]
	plot.xscale('log')
	plot.xlim(xrange)
	plot.xlabel('Gravitational Wave Frequency (Hz)')
	plot.yticks([])

	# im1=plot.imread('/Users/jracusin/Lobster/TAP/images/shb.png')
	# newax1=fig.add_axes([0.25,0.5,0.1,0.1],anchor='NE')
	# newax1.imshow(im1)
	# newax1.set_xticks([])
	# newax1.set_yticks([])

	# im2=plot.imread('/Users/jracusin/Lobster/TAP/images/SMBH_SMBH.jpg')
	# newax1=fig.add_axes([0.5,0.5,0.1,0.1],anchor='NE')
	# newax1.imshow(im2)
	# newax1.set_xticks([])
	# newax1.set_yticks([])

	# im3=plot.imread('/Users/jracusin/Lobster/TAP/images/smbh.png')
	# newax1=fig.add_axes([0.75,0.5,0.1,0.1],anchor='NE')
	# newax1.imshow(im3)
	# newax1.set_xticks([])
	# newax1.set_yticks([])


	# im4=plot.imread('/Users/jracusin/Lobster/TAP/images/PTAs.gif')
	# newax1=fig.add_axes([0.2,0.15,0.15,0.15],anchor='NE')
	# newax1.imshow(im4)
	# newax1.set_xticks([])
	# newax1.set_yticks([])

	# im5=plot.imread('/Users/jracusin/Lobster/TAP/images/LISA.jpg')
	# newax1=fig.add_axes([0.36,0.1,0.3,0.3],anchor='NE')
	# newax1.imshow(im5)
	# newax1.set_xticks([])
	# newax1.set_yticks([])

	# im6=plot.imread('/Users/jracusin/Lobster/TAP/images/LLO.jpg')
	# newax1=fig.add_axes([0.72,0.15,0.18,0.18],anchor='NE')
	# newax1.imshow(im6)
	# newax1.set_xticks([])
	# newax1.set_yticks([])

	plot.savefig('/Users/jracusin/Lobster/TAP/GW_axes.pdf')
	plot.show()
	plot.close()

def xflare_rates(daily_sensitivity):

	### Calculated observed WFI flux for 2 of the sources in Irwin et al. (2016), Nature
	### http://www.nature.com/nature/journal/v538/n7625/pdf/nature19822.pdf
	### These transients are too faint for the WFI, and volumetric rates not known, unless assume nearby galaxies
	lum1=4.4e37#7.8e38
	lum2=9e39#9e40
	mpc2cm=3.08568025e24
	d=3.8#14.3
	d=d*mpc2cm
	pl=1.0#1.6
	conv=jap.pimms([0.3,10.0],[0.4,4.0],pl,0)
	flux1=lum1*conv/(4*math.pi*d**2)
	flux2=lum2*conv/(4*math.pi*d**2)
	l=4

	det=np.ones((2,l),dtype=bool)
	det[0]=np.repeat(False,l)
	det[1]=np.repeat(False,l)
	for i in range(0,l):
		if flux1>daily_sensitivity[i]:
			det[0][i]=True
		if flux2>daily_sensitivity[i]:
			det[1][i]=True

	return det,flux1,flux2


def ccSNe_rates(t400_sensitivity,fov_frac):

	mpc2cm=3.08568025e24
	rate=0.258e-4 # SN Ibc rate from Li et al. 2011
	lum=6e43
	#exposure=400. #from SN2008D?

	xrt2wfi = 0.7 #0.3-5 keV phind=2, galNH=3e21
	xrt2xri = 0.3 #0.5-2 keV phind=2, galNH=3e21
	conv=np.array([xrt2wfi,xrt2wfi,xrt2wfi,xrt2xri])

	limit_dist=np.sqrt(lum*conv/(4.*math.pi*t400_sensitivity))/mpc2cm

	### get array of lum dist to interpolate our limits from the sensitivity
	z0=np.arange(0,10,0.1)
	lumdist=cosmo.luminosity_distance(z0).value
	## turn that into a z
	f=interpolate.interp1d(lumdist,z0)
	limit_z=f(limit_dist)
	## turn z into a comoving dist & volume
	comdist=cosmo.comoving_distance(limit_z).value
	vol=4*math.pi/3.*(comdist)**3
	ccSNe_rate=rate*vol*fov_frac
	return ccSNe_rate,comdist

def TDE_rates(sensitivity,sky_frac):

	rate=1e-4 # yr-1 galaxy-1
	galdens=1e-2 #Mpc-3 of SMBH (10^6-10^8 Msolar)
	mpc2cm=3.08568025e24

	z0=np.arange(0,10,0.1)
	lumdist=cosmo.luminosity_distance(z0)
	f=interpolate.interp1d(lumdist,z0)

	##  NON-JETTED
	bol2wfi=0.027#0.14
	bol2xrt=0.0147#0.039
	conv=np.array([bol2wfi,bol2wfi,bol2wfi,bol2xrt])
	lum=1e44*conv #spectrum is thermal so same lum as XRT
	limit_dist=np.sqrt(lum/(4*math.pi*sensitivity))/mpc2cm

	limit_z=f(limit_dist)
	comdist=cosmo.comoving_distance(limit_z).value
	vol=4*math.pi/3.*(comdist)**3
	nonjet_rate=rate*galdens*vol*sky_frac
	print limit_z

	## JETTED

	xrt2wfi=0.66#0.594
	xrt2xri=0.70#0.16
	conv=np.array([xrt2wfi,xrt2wfi,xrt2wfi,xrt2xri])
	frac=0.1  # 10% are beamed (though very uncertain)
	theta=5.*math.pi/180. # deg->radian
	area=2*math.pi*(1.-math.cos(theta))
	areafrac=area/(4*math.pi)*2.

	lum=1e47*conv # erg/s
	limit_dist=np.sqrt(lum/(4.*math.pi*sensitivity))/mpc2cm
	limit_dist[3]=np.max(lumdist).value
	limit_z=f(limit_dist)

	comdist=cosmo.comoving_distance(limit_z).value
	vol=4*math.pi/3.*(comdist)**3
	jet_rate=rate*galdens*vol*frac*areafrac*sky_frac

	return nonjet_rate,jet_rate,comdist,limit_dist,limit_z

def AGN_rates(daily_sensitivity,weekly_sensitivity,sky_frac):

	mpc2cm=3.08568025e24
	nsim=1000.
	lum=np.array(10**(np.arange(0,nsim,10)/((nsim-1)/10.)+38.))
	alpha=-1.37
	lstar=10**(43.66)
	phistar=10**(-4.74)
	dl=np.array(lum[1:]-lum[0:len(lum)-1])
	lum=lum[1:len(lum)]
	xmm2wfi=0.796  # convert from 0.5-7 to 0.3-5, assumes PL=1.8, Nh=3e21
	xmm2xri=0.306
	conv=np.array([xmm2wfi,xmm2wfi,xmm2wfi,xmm2xri])

	z0=np.arange(0,10,0.01)
	lumdist=cosmo.luminosity_distance(z0).value
	f=interpolate.interp1d(lumdist,z0)

	ws = [daily_sensitivity,weekly_sensitivity]

	rate=np.zeros((2,4))
	for judy in range(0,2):
		for idiot in range(0,4):
			ld=np.array(np.sqrt(lum*conv[idiot]/(4.*math.pi*ws[judy][idiot]))/mpc2cm) # distance WFI can detect lums
			w=np.where((ld < np.max(lumdist)) & (ld > np.min(lumdist)))
			z=f(ld[w])
			phi=phistar*(lum[w]/lstar)**alpha*np.exp(-lum[w]/lstar)*dl[w]/lstar # density at those lums
			comdist=cosmo.comoving_distance(z[w]).value
			vol=4.*math.pi/3*comdist**3
			rate[judy][idiot]=np.sum(vol*phi)*sky_frac[idiot] # rate=vol*density

	return rate

def Blazar_rates(daily_sensitivity,weekly_sensitivity,sky_frac):

	f1=np.array([7.506645E-14,1.3298641E-13,2.986495E-13,6.521481E-13,1.8288595E-12,6.4100483E-12])
	f2=np.array([8.303352E-14,3.7369985E-13,1.4834874E-12,4.8445995E-12])
	f3=np.array([1.0131099E-13,1.7693365E-13,3.5526947E-13,1.0536481E-12,6.540384E-12,1.2409822E-11,6.057032E-11])

	n1=np.array([0.0024451583,0.0020690372,0.0016649443,0.0012539582,5.198891E-4,1.8254212E-4])
	n2=np.array([0.015891278,0.0028273796,5.288385E-4,1.0750702E-4])
	n3=np.array([0.13020536,0.0903178,0.061599646,0.027740812,0.0068690353,0.0029465847,1.123024E-4])

  	#1=low lum BLs, 2=blazar sequence, 3=other surveys?
  	# from Padovani et al 2007

	fconv=[1.346,1.346,1.346,0.5191]
	ws = [daily_sensitivity,weekly_sensitivity]
	f=[f1,f2,f3]
	n=[n1,n2,n3]
	allsky=4.*math.pi*(180./math.pi)**2.

	# fig=plot.figure()

	rate=np.zeros((2,3,4))
	finalrate=np.zeros((2,4))
	farr=10**np.arange(-15,-9.5,0.01)
	color=['blue','green','red']
	for j in range(0,2): #loop over daily, weekly
	# need to loop better, 3 types, 2 sets of timescales
		for k in range(0,4):
			for i in range(0,3):
				narr=loginterpol(f[i]*fconv[k],n[i],farr)
				nws=loginterpol(farr,narr,ws[j][k])
				#print ws[j][k]
				#rate[j][i][k]=loginterpol(f[i]*fconv,n[i],ws[j][k])*sky_frac[k]*allsky
				rate[j][i][k]=nws*sky_frac[k]*allsky
				#print j,i,k,rate[j][i][k]
				# print f[i]
				# plot.scatter(f[i],n[i],color=color[i])
				# plot.plot(farr,narr,color=color[i])
			finalrate[j][k]=np.sum(rate[j][-1][k])

	# plot.xscale('log')
	# plot.yscale('log')
	# plot.xlim([1e-14,1e-9])
	# plot.show()
	
#	print ws
	return finalrate

def stellar_flares_rates(fovs):

	#  scaling from 2010 rates which were 30-300 /yr
	oldlob=30*30.
	wfirate0=fovs/oldlob*30.
	wfirate1=fovs/oldlob*300.
	rate=[wfirate0,wfirate1]

	return rate

def novae_rates(fovs,t100_sensitivity):

	# classical novae 35 +/- 11 yr-1 in our Galaxy (Shafter 1997),
	#                41+/-20 yr-1 (Hatano et al. 1997)
  	# initial thermonuclear runaway burning phase lasts only a few x100s
	
	mpc2cm=3.08568025e24
	allsky=4.*math.pi*(180./math.pi)**2.
	galnovarate=35.

	lpeak=1e38 # erg cm-2 s-1
	dist=8*1e-3*mpc2cm # kpc to cm
	distm31=780*1e-3*mpc2cm
	fgal=lpeak/(4.*math.pi*dist**2)
	fm31=lpeak/(4.*math.pi*distm31**2)

	wfirate=np.zeros(4)
	for i in range(0,4):
		if t100_sensitivity[i] < fgal:
			wfirate[i]=wfirate[i]+galnovarate*fovs[i]/allsky
		if t100_sensitivity[i] < fm31:
			wfirate[i]=wfirate[i]+galnovarate*fovs[i]/allsky
	#print 'Flux at GC = ',fgal
	#print 'Flux at M31 = ',fm31

	#wfirate=galnovarate*fovs/allsky

	return wfirate

def thermonuclear_burst_rates(fovs):

	allsky=4.*math.pi*(180./math.pi)**2.
	# Keek et al. (2010)
	expo=np.array([83.1,30.9,53.9,52.4,60.6,49.8,60.1,47.9,54.7,56.3,48.3,55.1,71.5,34.3,55.7])*86400.
	bursts=np.array([269,17,41,67,241,27,125,18,50,269,24,31,63,60,55])
  	#adding Cornelisse et al. (2003)
  	expo2=np.array([7.4,6.7,6.5,7,8.9,6.9,7.1])*1e6
	bursts2=np.array([423,339,260,178,104,61,49])
	expo=np.append(expo,expo2)
	bursts=np.append(bursts,bursts2)

	rates=bursts/expo
	wfirates=np.sum(rates)*86400.*0.85*365*fovs/allsky

	return wfirates

def gw_counterpart_rates(time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux):

	volrate=10/1e3**3 # Mpc-3 yr-1 from Coward et al.
	h=[200,400]
	obj=['NS-NS','NS-BH']
	xrteff=0.87 # fraction of sGRBs w afterglow (from JD's table)
	gweff=0.95  # x # pointings gets gweff
	mpc2cm=3.08568025e24

	# rescaling from Peter's presentation (removing Kagra)
	f3=np.array([44.8+11.2])/100.
	f2=np.array([11.2+6.4+6.4+2.8+1.6+1.6])/100.
	f1=np.array([1.6+1.6+6.4+0.4+0.4+1.6])/100.
	f0=np.array([0.4+1.6])/100.
	f=f0+f1+f2+f3
	f1=(f0+f1)/f
	f2=f2/f
	f3=f3/f

	npointings=np.array([2,3,1,100]) #totally guessing based on FoV to tile a 2 det LIGO contour

	flux=[lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux]

	for p in range(0,4):
		npoint=npointings[p]*2.
		gf=flux[p] # looping through configs
		orbit=90.*60.
		slew=30.
		start=180. # when we get LIGO positions
		exptimes=np.array([orbit/2.-slew,orbit/2.-slew])
		exptimes=np.append(exptimes,np.repeat(orbit/npoint-slew,npoint))
		wfisens=loginterpol(time,gf,exptimes)

		# 2 pointings to get 95% enclosed
		tobs_stop=start+np.array([orbit/2.,orbit])
		tobs_stop=np.append(tobs_stop,np.arange(1,npoint+1)*(orbit/npoint-slew))
		tobs_start=start+np.array([0,orbit/2.])
		tobs_start=np.append(tobs_start,np.arange(0,npoint)*(orbit/npoint-slew))
		nobs=len(tobs_stop)

		print 'N Pointings = ',npoint
		
		t=np.logspace(180,1e4,num=100)
		dt=t[1:]-t[0:len(t)-1]

		grb=fits.open('/Users/jracusin/Swift/swift_grb_properties.fits')
		grb=grb[1].data
		w=np.where((grb['t90'] <= 2.) & (grb['t90'] > 0.) & (grb['grb'] != 'GRB100628A') & (grb['grb'] != 'GRB140311B'))
		ngrb=len(w)
		grb=grb[w]
		z=grb['z']
		wz=np.where(z == -1)
		z[wz]=0.7
		for k in range(0,2):   # loop over NS-NS & NS-BH
			print obj[k]
			gwhorizon=h[k]*1.5*1.5  
			gwallskyrate=volrate*gwhorizon**3*4./3.*math.pi
			distfull=gwhorizon*mpc2cm
			detuntil=np.zeros(nobs,ngrb)
			det=np.zeros(nobs,ngrb)
        	for i in range(0,ngrb):
				dist=cosmo.luminosity_distance(z[i])*mpc2cm
				eng=np.arange(0,98)/10.+0.3
				de=0.1
				conv=np.sum(pow(eng[0:48],[1.,grb['phind'][i]])*de*eng[0:48])/np.sum(pow(eng,[1.,grb['phind'][i]])*de*eng)
				#rat=grb['cfratio'][i]*conv*dist**2/distfull**2/kcorr(z[i],[1.,grb['phind'][i]],/pow)  

	# THIS DOESN'T WORK
	gw=0
	return gw

def grb_redshift_rates(doplot=False):

	zeff=np.array([0.33,0.33,0.9,0.9])
	wfi2014=ascii.read('/Users/jracusin/Lobster/2014_proposal/redshift_accum_data_all.txt', 
		names=['z','t0s','t500s','t1000s','t1500s','t2000s','t2500s','t3000s','t3500s','t4000s', 
		't4500s','t5000s','All_detected_withoutGTM','intrinsic','GTM','All_detected_withGTM'])

	# wfi45cm=ascii.read('/Users/jracusin/Lobster/TAO_2016/redshift_accum_data_all_tao.txt', 
	# 	names=['z','t0s','t500s','t1000s','t1500s','t2000s','t2500s','t3000s','t3500s','t4000s', 
	# 	't4500s','t5000s','All_detected_withoutGTM','intrinsic','GTM','All_detected_withGTM'])

	wfi45cm=ascii.read('/Users/jracusin/Lobster/TAO_2016/redshift_accum_data_all_tao_z0.1bin.txt', 
		names=['z','t0s','t1030s','t2060s','t3090s','t4120s','t5150s','All_detected_withoutGTM','intrinsic',
		'GTM','All_detected_withGTM'])

	# wfi45cm_UL=ascii.read('/Users/jracusin/Lobster/TAO_2016/redshift_accum_data_all_tao_Lien14GRB_upper_limit.txt', 
	# 	names=['z','t0s','t500s','t1000s','t1500s','t2000s','t2500s','t3000s','t3500s','t4000s', 
	# 	't4500s','t5000s','All_detected_withoutGTM','intrinsic','GTM','All_detected_withGTM'])

	wfi45cm_UL=ascii.read('/Users/jracusin/Lobster/TAO_2016/redshift_accum_data_all_tao_Graff16_upper_limit_z0.1bin.txt', 
		names=['z','t0s','t1030s','t2060s','t3090s','t4120s','t5150s', 
		'All_detected_withoutGTM','intrinsic','GTM','All_detected_withGTM'])

	total=np.array([wfi2014['All_detected_withGTM'][0],wfi45cm['All_detected_withGTM'][0],\
		wfi45cm['All_detected_withGTM'][0]*4.,wfi45cm_UL['All_detected_withGTM'][0]])

	i=np.where(wfi2014['z']==5)
	i45=np.where(wfi45cm['z']==5)
	i45UL=np.where(wfi45cm_UL['z']==5)

	highz=np.array([wfi2014['All_detected_withGTM'][i],wfi45cm['All_detected_withGTM'][i45],
		wfi45cm['All_detected_withGTM'][i45]*4.,wfi45cm_UL['All_detected_withGTM'][i45UL]])

	highz=np.reshape(highz,(4))*zeff

#	if doplot:
	grb=fits.open('/Users/jracusin/Swift/swift_grb_properties.fits')
	grb=grb[1].data
	z=grb['z']
	w=np.where(z > 0)
	z=z[w]
	z=np.sort(z)
	x=[]
	y=[]
	for i in range(0,len(z)):
		x=np.append(x,z[i])
		y=np.append(y,len(z)-i)
	swiftlen=9.33

	if doplot:
		fig=plot.figure(figsize=(8,5))
		plot.plot(x,y/swiftlen,drawstyle='steps-mid',color='black',lw=2)
		plot.fill_between([5,10.],[5e2,5e2],[0.1,0.1],facecolor='lightblue',interpolate=True,\
			color='lightblue',alpha=0.5)
		plot.annotate('Swift GRB Redshifts',xy=(1,1.5),xycoords='data',fontsize=16,color='black')#,rotation=340)

	#	plot.fill_between(wfi45cm['z'],wfi45cm['All_detected_withGTM']*zeff[1], \
	#		(wfi45cm_UL['All_detected_withGTM']+wfi45cm['GTM'])*zeff[1],alpha=0.8)
		plot.fill_between(wfi45cm['z'],wfi45cm['All_detected_withGTM']*zeff[2]*4, \
			(wfi45cm_UL['All_detected_withGTM']*4+wfi45cm['GTM'])*zeff[2],alpha=0.8)

		plot.annotate('Predicted Redshifts for TAP GRBs',xy=(1.5,70),xycoords='data', \
			fontsize=20,color='blue',rotation=340)

		plot.plot([5,5],[0.1,500],color='k')
#		plot.scatter(7,30,s=40,marker='>',color='k')
#		plot.plot([5,7],[30,30],color='k')
#		plot.annotate('Targets for JWST',xy=(5.2,40),xycoords='data',fontsize=14,color='k')

		plot.xlim(0,9)
		plot.ylim(0.1,5e2)
		plot.yscale('log')
		axis_font = {'fontname':'Arial', 'size':'16'}
		plot.xlabel('Redshift (z)',{'fontsize': 16})
		plot.ylabel(r'Cumulative ($>$z) GRB Rate (yr$^{-1}$)',{'fontsize': 16})
		plot.xticks(size=16)
		plot.yticks(size=16)
		plot.axes().xaxis.set_minor_locator(MultipleLocator(0.5))
		plot.savefig('grbz_plot.png')
		plot.savefig('grbz_plot.eps')
		plot.show()
		plot.close()

#	print highz

	return total,highz

def TAO_source_rates(doplot=False,onlyTAO=False):

	from tao_planning_sims import wfi_sensitivity

	fovs=np.array([12.4*12.4,18.6*18.6,4.*18.6*18.6,1.])
	config=['TAO TSM WFI: 12.4x12.4','TAO WFI: 18.6x18.6','TAP WFI: 4x18.6x18.6','TAP XRT: 1x1']
	allsky=4.*math.pi*(180./math.pi)**2.
	fov_frac=fovs/allsky
	frac_sky=np.array([0.8,0.8,0.85,100./allsky])
	pointings = np.round(allsky/fovs*frac_sky)
	pointings[3]=pointings[2] ### because XRT follows WFI
	print 'Pointings = ',pointings
	mpc2cm=3.08568025e24

	## Setting up sensitivity
	# oldlob=ascii.read('/Users/jracusin/Lobster/2014_proposal/lobster_sensitivity_0.3_5_Ptak.dat', 
	# 	names=['time','bcount','mcount','grbflux'],data_start=1)

	t1=np.array([0.1,0.15,0.2,0.3,0.4,0.5,0.7,1.])
	t2=np.array([2e4,4e4,7e4,9e4,1e5,1.3e5,1.7e5,2e5,2.5e5,3e5,3.5e5,4e5,5e5,8e5,1e6])
	# time=np.array(oldlob['time'])
	# grbflux=np.array(oldlob['grbflux'])
	# nl=len(time)
	# time2=np.append(t1,time)
	# time2=np.append(time2,t2)

	# flux=grbflux[0]/(t1/time[0])
	# flux=np.append(flux,grbflux)
	# flux=np.append(flux,grbflux[nl-1]/np.sqrt(t2/time[nl-1]))

	# w=np.where(flux < 1e-12)
	# flux[w]=1e-12

	# lob_flux=flux
	# lob_time=time2

#	tao=ascii.read('/Users/jracusin/Lobster/TAO_2016/simulations/Ptak/tau_flux_limits_2018_prob1e-10.csv',names=['time','bcount','mcount','grbflux'],data_start=1)
#	tao=ascii.read('/Users/jracusin/Lobster/TAO_2016/lobster_sensitivity_0.3_5_Ptak_45cm.dat',names=['time','bcount','mcount','grbflux'],data_start=1)
	tao=ascii.read('/Users/jracusin/TAO/simulations/sensitivity_curves/TAO-ISS_sensitivity_8_0.3_5.dat')#,names=['time','bcount','mcount','crabflux','grbflux','sgrbflux'],data_start=1)
#	tao_wfi_flux=flux/1.5**2
#	w=np.where(tao_wfi_flux < 1e-12)
#	tao_wfi_flux[w]=1e-12
	# jordan_wfi_flux=flux/1.5**2
	# w=np.where(jordan_wfi_flux < 1e-12)
	# jordan_wfi_flux[w]=1e-12
	
	time=np.array(tao['time'])
	grbflux=wfi_sensitivity(time,configfile='tao_config_v51.txt')
#	grbflux=np.array(tao['grbflux'])
	nl=len(time)
	#time2=np.append(t1,time)
	#time2=np.append(time2,t2)

	#flux=grbflux[0]/(t1/time[0])
	#flux=np.append(flux,grbflux)
	#flux=np.append(flux,grbflux[nl-1]/np.sqrt(t2/time[nl-1]))

	#w=np.where(flux < 1e-12)
	#flux[w]=1e-12

	tao_wfi_flux=grbflux#tao['grbflux']
	time2=time#tao['time']

	w=np.where(tao_wfi_flux < 1e-12)
	tao_wfi_flux[w]=1e-12

#	lob_flux=loginterpol(lob_time,lob_flux,time2)
#	jordan_wfi_flux=loginterpol(lob_time,jordan_wfi_flux,time2)

	tap_wfi_flux=tao_wfi_flux
	lob_flux=tao_wfi_flux
	lob_time=time2

	starx=ascii.read('/Users/jracusin/Lobster/TAP/XRI_sensitivity_15arcmin.dat',names=['time','flux'],data_start=1)
	starx_time=np.array(starx['time'])
	starx_flux=np.array(starx['flux'])
	xri_time=np.append(t1,starx_time)
	tap_xri_flux=starx_flux[0]/(t1/starx_time[0])
	tap_xri_flux=np.append(tap_xri_flux,starx_flux)
#	tck=interpolate.splrep(np.log10(xri_time),np.log10(tap_xri_flux),s=0)
	tap_xri_flux=loginterpol(xri_time,tap_xri_flux,time2)#10**interpolate.splev(np.log10(time2),tck,der=0)

	time=time2

	if doplot:
		plot_sensitivity(time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux,tao_wfi_flux,onlyTAO=onlyTAO)

	### sensitivity for various exposures
	uptime=np.array([0.95*0.85,0.95*0.85,1.,1.]) # down, SAA
		 # 0.85 for SAA, 30 s for settling, 0.95 for ISS downtime, 22.5 for average slew time @4 deg/s
	week_slew_factor=np.array([1.,1.,7.,7.]) # on longer exposures, will spread exposure across snapshots
	month_slew_factor=np.array([1.,1.,30.,30.]) # on longer exposures, will spread exposure across snapshots

	daily_exptimes = np.array(86400*uptime/pointings-30-22.5)
	daily_sensitivity=interpol_sens(daily_exptimes,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)	
	daily_sensitivity[1]=wfi_sensitivity(daily_exptimes[1])

	weekly_exptimes = 7*86400*uptime/pointings-(30-22.5)*week_slew_factor
	weekly_sensitivity=interpol_sens(weekly_exptimes,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)
	weekly_sensitivity[1]=wfi_sensitivity(weekly_exptimes[1])

	monthly_exptimes = 30*86400*uptime/pointings-(30-22.5)*month_slew_factor
	monthly_sensitivity=interpol_sens(monthly_exptimes,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)

	t400=[400.]
	t400_sensitivity=interpol_sens(t400,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)	
	t400_sensitivity[1]=wfi_sensitivity(t400)
	print t400_sensitivity

	t100=[100.]
	t100_sensitivity=interpol_sens(t100,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)	
	t100_sensitivity[1]=wfi_sensitivity(t100)
	print t100_sensitivity

	print 'Daily exptimes: ',daily_exptimes
	print 'Daily sensitivity: ',daily_sensitivity
	print
	print 'Weekly exptimes: ',weekly_exptimes
	print 'Weekly sensitivity: ',weekly_sensitivity
	print
	print 'Monthly exptimes: ',monthly_exptimes
	print 'Monthly sensitivity: ',monthly_sensitivity
	print

	if doplot:
		compare_sensitivity(time,tao_wfi_flux,daily_exptimes)


	ccSNe=ccSNe_rates(t400_sensitivity,fov_frac)
	tde=TDE_rates(weekly_sensitivity,fov_frac*pointings)
	agn=AGN_rates(daily_sensitivity,weekly_sensitivity,fov_frac*pointings)
	blazars=Blazar_rates(daily_sensitivity,weekly_sensitivity,fov_frac*pointings)
	stellar_flares=stellar_flares_rates(fovs)
	novae=novae_rates(fovs,t100_sensitivity)
	thermonuclear_burst=thermonuclear_burst_rates(fovs)
	#gw=gw_counterpart_rates(time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)
	grbs=grb_redshift_rates(doplot=False)
	xf,xfflux1,xfflux2=xflare_rates(daily_sensitivity)

	numprint=4

	print 'ccSNe Rates'
	for i in range(0,numprint):
		print config[i],ccSNe[0][i],ccSNe[1][i]

	print
	print 'TDE Rates (weekly)'
	print '       non-jetted       jetted'
	for i in range(0,numprint):
		print config[i],tde[0][i],tde[1][i],tde[0][i]+tde[1][i],tde[2][i],tde[3][i],tde[4][i]

	print
	print 'AGN Rates (daily/weekly)'
	for i in range(0,numprint):
		print config[i],agn[0][i],agn[1][i]

	print
	print 'Blazar Rates (daily/weekly)'
	for i in range(0,numprint):
		print config[i],blazars[0][i],blazars[1][i]

	print
	print 'Stellar Flares Rates'
	for i in range(0,numprint):
		print config[i],stellar_flares[0][i],stellar_flares[1][i]

	print
	print 'Novae Rates'
	for i in range(0,numprint):
		print config[i],novae[i]

	print
	print 'Thermonuclear Burst Rates'
	for i in range(0,numprint):
		print config[i],thermonuclear_burst[i]

	print
	print 'GRB z Rates'
	for i in range(0,numprint):
		print config[i],grbs[0][i],grbs[1][i]

	print
	print 'EG X-ray flashes'
	print xfflux1,xfflux2
	for i in range(0,numprint):
		print config[i],xf[0][i],xf[1][i]

	twant=np.array([2])
	print interpol_sens(twant,time,lob_flux,tao_wfi_flux,tap_wfi_flux,tap_xri_flux)


