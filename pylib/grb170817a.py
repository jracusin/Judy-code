#!/usr/bin/env python
"""
------------------------------------------------------------------------

GRB170817A energetics code inlcuding code to read in GBM GRB catalog and GRBOX redshift list and cross-match

To run:
import grb170817a
gbm=grb170817a.load_GBM()
grbox=grb170817a.load_GRBOX()
m1,m2,Eiso,Liso=grb170817a.calc_energetics(grbox,gbm)
grb170817a.plot_jet_angle()

rerun it faster after the first time with:
m1,m2,Eiso,Liso=grb170817a.calc_energetics(grbox,gbm,m1=m1,m2=m2)

input files:
gbmgrbcat.fits (obtained from GBM GRB catalog HEASARC browse)

outputs files:
GBM_Eiso_z.pdf
GBM_Liso_z.pdf
Eiso_thetaj.pdf

Written by Judy Racusin (NASA/GSFC)
------------------------------------------------------------------------
"""

import urllib
from astropy.io import fits,ascii
from astropy.table import Table,vstack
import numpy as np
from astropy.time import Time
from astropy.coordinates import SkyCoord
from astropy import units as u
import matplotlib.pylab as plot
from astropy.cosmology import Planck15 as cosmo
from scipy.optimize import curve_fit
import scipy.odr as odr
from matplotlib.ticker import LogLocator,MultipleLocator

def grbslike170817a(grblist=[]):
	# find grbs like GRB 170817A
	import fit_lc

	if grblist == []:
		grbdict,grbrec,grblist=fit_lc.load_data(dir='/Users/jracusin/Swift/GRBfits/GRBs/')
	ngrbs=len(grblist)

	tmin=[]
	UL=[]
	for i in range(ngrbs):
		tmin=np.append(tmin,min(grblist[i].lc['Time']+grblist[i].lc['T_-ve']))
		if grblist[i].lc['Ratepos'][0] == 0.0:
			UL=np.append(UL,True)
			print i

		else:
			UL=np.append(UL,False)

	return tmin,UL


## plot jet opening angles for all the short grbs with limits or values to compare to top-hat limit for GRB170817A
def grb090510(grbox,gbm,m1,m2,Eiso,Liso):

	gwz=0.009845
	gwdist=42.9
	gwdisterr=3.2
	gwzerr=0.01057-gwz

	grb='090510'
	z=0.903
	w=np.where(grbox[m1]['GRB'] == grb)

	alpha=gbm[m2][w]['PFLX_ALPHA']
	alphaerr0=gbm[m2][w]['PFLX_ALPHA_neg_err']
	alphaerr1=gbm[m2][w]['PFLX_ALPHA_pos_err']
	epeak=gbm[m2][w]['PFLX_EPEAK']
	epeakerr0=gbm[m2][w]['PFLX_EPEAK_neg_err']
	epeakerr1=gbm[m2][w]['PFLX_EPEAK_pos_err']
	pflux=gbm[m2][w]['PFLX']
	pfluxerr=gbm[m2][w]['PFLX_err']

	FLNC_Emin=10.;FLNC_Emax=1000.;Emin=1.;Emax=1e4
	eng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+gwz)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
	f1=comp(eng,alpha,epeak)
	f2=comp(eng2,alpha,epeak)

	k1=np.trapz(f1*eng,eng)
	k2=np.trapz(f2*eng2,eng2)

	geng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+z)
	geng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
	gf1=comp(geng,alpha,epeak)
	gf2=comp(geng2,alpha,epeak)

	gk1=np.trapz(gf1*geng,geng)
	gk2=np.trapz(gf2*geng2,geng2)


	grbLiso=calc_eiso(gwz,pflux,f1,f2,lumdist=gwdist,Liso=True)

	ef1p=comp(eng,alpha+alphaerr1,epeak+epeakerr1)
	ef2p=comp(eng2,alpha+alphaerr1,epeak+epeakerr1)
	ef1n=comp(eng,alpha-alphaerr0,epeak-epeakerr0)
	ef2n=comp(eng2,alpha-alphaerr0,epeak-epeakerr0)

	liso_err0=grbLiso-calc_eiso(gwz,pflux-pfluxerr,ef1n,ef2n,Liso=True)
	liso_err1=calc_eiso(gwz,pflux+pfluxerr,ef1p,ef2p,Liso=True)-grbLiso

	# using LAT spectrum

	FLNC_Emin=1e5;FLNC_Emax=1e7;Emin=1e5;Emax=1e6
	# start from 100 MeV to 10 GeV, and convert to 100 MeV to 1 GeV
	alpha=-2.2
	eng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+gwz)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
	f1=pl(eng,alpha)
	f2=pl(eng2,alpha)
	k1=np.trapz(f1*eng,eng)
	k2=np.trapz(f2*eng2,eng2)

	geng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+z)
	geng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
	gf1=pl(geng,alpha)
	gf2=pl(geng2,alpha)
	gk1=np.trapz(gf1*geng,geng)
	gk2=np.trapz(gf2*geng2,geng2)




	print 'GRB090510 at distance of GRB170817A',grbLiso[0],liso_err0[0],liso_err1[0]
	print 'GRB090510 at z=0.903',Liso[w]
	print gk1/gk2, k1/k2, gk1/gk2/(k1/k2)


def plot_jet_angle():

	#from Fong et al. 2015
	grbs=np.array(['050709','050724A','051221A','090426A','101219A','111020A','111117A','120804A','130603B','140903A','140930B'])
	thetamin=np.array([15,25,6,5,4,3,3,13,4,5,9,9])
	thetamax=np.array([90,90,7,7,90,8,10,90,8,5,90,90])
	eiso=np.array([0.09,0.03,0.24,1.3,2.0,0.74,0.17,0.55,3.4,0.37,0.08,0.4])*1e52
	gwthetamin=28

	fig,ax=plot.subplots()
	m=np.where(thetamax-thetamin == 0)
	u=np.where(thetamax == 90)
	l=np.where((thetamax-thetamin > 0) & (thetamax != 90))
	plot.scatter(thetamin[l],eiso[l],marker='>')
	plot.scatter(thetamax[l],eiso[l],marker='<',color='C0')
	plot.scatter(thetamin[u],eiso[u],marker='>',color='C0')
	plot.scatter(thetamin[m],eiso[m],color='C0')
	for i in range(len(l)):
		plot.plot([thetamin[l[i]],thetamax[l[i]]],[eiso[l[i]],eiso[l[i]]],linestyle='--',color='C0')

	i=1
	plot.annotate('GRB'+grbs[i],xy=(thetamin[i]-7,eiso[i]*0.8),xycoords='data',color='C0')

	### from calc_energetics detailed analysis value below
	gweiso=5.26e46
	gweisoerr=8e45

	plot.scatter(gwthetamin,gweiso,marker='>',color='magenta')#,label='GRB170817A')
	plot.annotate('GRB170817A',xy=(21,gweiso*0.8),xycoords='data',color='magenta')

	plot.legend(loc=3)
	plot.yscale('log')

	ax.yaxis.set_minor_locator(LogLocator(numticks=8,subs=np.arange(2,10)))
	minor_ticks = np.arange(0, 30, 1)
	ax.set_xticks(minor_ticks, minor = True)
	ax.tick_params(direction='in',axis='both',which='both',right='on',top='on')

	plot.xlabel(r'$\theta_j$ (deg)')
	plot.ylabel(r'$E_{iso}$ (1 keV - 10 MeV) (erg)')
	plot.xlim([0,30])
	plot.ylim([1e46,1e53])
	plot.savefig('Eiso_thetaj.pdf', bbox_inches='tight')


def logline(beta,x):

	m=beta[0]
	b=beta[1]
	f=b*(x/1e52)**m

	return f

### Spectral models
def pl(eng,alpha,epiv=100):

	f=(eng/epiv)**alpha

	return f

def sbpl(eng,alpha,epeak,beta,epiv=100,delta=0.3):

	m=(beta-alpha)/2
	b=(alpha+beta)/2
	q=np.log10(eng/epeak)/delta
	qpiv=np.log10(epiv/epeak)/delta
	apiv=m*delta*np.log((np.exp(qpiv)+np.exp(-qpiv))/2)
	a=m*delta*np.log((np.exp(q)+np.exp(-q))/2)

	f=(eng/epiv)**b*10**(a-apiv)

	return f

def comp(eng,alpha,epeak,epiv=100):
  
  f=(eng/epiv)**alpha*np.exp(-eng*(2.+alpha)/epeak)

  return f

def band(eng,alpha,epeak,beta,enorm=100):

  w1=np.where(eng <= (alpha-beta)*epeak/(2+alpha))
  w2=np.where(eng >= (alpha-beta)*epeak/(2+alpha))
  f=np.zeros(len(eng))
  if len(w1[0]) > 0:
  	f[w1]=(eng[w1]/enorm)**alpha*np.exp(-(2+alpha)*eng[w1]/epeak)
  if len(w2[0]) > 0:
  	f[w2]=((alpha-beta)*epeak/enorm/(2+alpha))**(alpha-beta)*np.exp(beta-alpha)*(eng[w2]/enorm)**beta

  return f

def bbody(eng,kt):

	f=8.0525*eng**2/(kt**4*(np.exp(eng/kt)-1))

	return f

def calc_eiso(z,fluence,f1,f2,FLNC_Emin=10.,FLNC_Emax=1000.,Emin=1.,Emax=1e4,lumdist=None,Liso=False):

	## energy over output Eiso range
	eng=np.logspace(np.log10(Emin),np.log10(Emax),1e4)/(1.+z)
	## energy over  measure fluence range
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),1e4)
	k1=np.trapz(f1*eng,eng)
	k2=np.trapz(f2*eng2,eng2)
	pc2cm=3.08568025e18	

	k=k1/k2
#	print k,k1,k2
	if lumdist==None:
		lumdist=cosmo.luminosity_distance(z).value
	dist=lumdist*1e6*pc2cm
	if Liso==False:
		Eiso=4.*np.pi*k*dist**2*fluence/(1.+z)
	else:
		Eiso=4.*np.pi*k*dist**2*fluence

	return Eiso

def calc_energetics(grbox,gbm,FLNC_Emin=10.,FLNC_Emax=1000.,Emin=1.,Emax=1e4,m1=None,m2=None):

	if type(m1)==type(None):
		m1,m2=match_catalogs_time_coord(grbox['RA'],grbox['Dec'],grbox['TRIG_MET'],gbm['RA'],gbm['Dec'],gbm['TRIG_MET'])
	w=np.where(grbox['z'][m1].mask == False)
	m1=m1[w]
	m2=m2[w]

	w=np.where((gbm['FLNC_BEST_FITTING_MODEL'][m2] != ''))

	m1=m1[w] ; m2=m2[w]
	n=len(w[0])

	size=14
	plot.rc('font', size=size)          # controls default text sizes
	plot.rc('axes', titlesize=size)     # fontsize of the axes title
	plot.rc('axes', labelsize=size+2)    # fontsize of the x and y labels
	plot.rc('xtick', labelsize=size)    # fontsize of the tick labels
	plot.rc('ytick', labelsize=size)    # fontsize of the tick labels
	plot.rc('legend', fontsize=size)    # legend fontsize


	zs=grbox['z'][m1]
	pc2cm=3.08568025e18

	Eiso=[] ; Eiso_err0=[] ; Eiso_err1=[] ; Liso=[] ; Liso_err0=[] ; Liso_err1=[]
	aval=0.7

	redshifts=[] 
	for i in range(n):
		eng=np.logspace(np.log10(Emin),np.log10(Emax),100)
		eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
		## loop through each grb with a redshift
		if '?' not in zs[i]:
			z=float(zs[i])
			redshifts=np.append(redshifts,z)
			eng=eng/(1.+z)

			## use fluence to get Eiso
			if gbm['FLNC_BEST_FITTING_MODEL'][m2][i]=='FLNC_BAND':
				f1=band(eng,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]),float(gbm['FLNC_BETA'][m2[i]]))
				f2=band(eng2,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]),float(gbm['FLNC_BETA'][m2[i]]))
			if gbm['FLNC_BEST_FITTING_MODEL'][m2][i]=='FLNC_COMP':
				f1=comp(eng,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]))
				f2=comp(eng2,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]))
			if gbm['FLNC_BEST_FITTING_MODEL'][m2][i]=='FLNC_SBPL':
				f1=sbpl(eng,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]),float(gbm['FLNC_BETA'][m2[i]]))
				f2=sbpl(eng2,float(gbm['FLNC_ALPHA'][m2[i]]),float(gbm['FLNC_EPEAK'][m2[i]]),float(gbm['FLNC_BETA'][m2[i]]))
			if gbm['FLNC_BEST_FITTING_MODEL'][m2][i]=='FLNC_PLAW':
				f1=pl(eng,float(gbm['FLNC_ALPHA'][m2[i]]))
				f2=pl(eng2,float(gbm['FLNC_ALPHA'][m2[i]]))

			eiso=calc_eiso(z,gbm['FLUENCE'][m2[i]],f1,f2)
			Eiso=np.append(Eiso,eiso)
			## errors from fluence values which should include spectral fit parameter covariance uncertainties, ends up very small
			eiso_err0=eiso-calc_eiso(z,gbm['FLUENCE'][m2[i]]-gbm['FLUENCE_err'][m2[i]],f1,f2)
			eiso_err1=calc_eiso(z,gbm['FLUENCE'][m2[i]]+gbm['FLUENCE_err'][m2[i]],f1,f2)-eiso
			Eiso_err0=np.append(Eiso_err0,eiso_err0)
			Eiso_err1=np.append(Eiso_err1,eiso_err1)

			### use peak flux to get Liso
			if gbm['PFLX_BEST_FITTING_MODEL'][m2][i]=='PFLX_BAND':
				pf1=band(eng,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]),float(gbm['PFLX_BETA'][m2[i]]))
				pf2=band(eng2,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]),float(gbm['PFLX_BETA'][m2[i]]))

			if gbm['PFLX_BEST_FITTING_MODEL'][m2][i]=='PFLX_COMP':
				pf1=comp(eng,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]))
				pf2=comp(eng2,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]))

			if gbm['PFLX_BEST_FITTING_MODEL'][m2][i]=='PFLX_SBPL':
				pf1=sbpl(eng,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]),float(gbm['PFLX_BETA'][m2[i]]))
				pf2=sbpl(eng2,float(gbm['PFLX_ALPHA'][m2[i]]),float(gbm['PFLX_EPEAK'][m2[i]]),float(gbm['PFLX_BETA'][m2[i]]))

			if gbm['PFLX_BEST_FITTING_MODEL'][m2][i]=='PFLX_PLAW':
				pf1=pl(eng,float(gbm['PFLX_ALPHA'][m2[i]]))
				pf2=pl(eng2,float(gbm['PFLX_ALPHA'][m2[i]]))

			liso=calc_eiso(z,gbm['PFLX'][m2[i]],pf1,pf2,Liso=True)
			Liso=np.append(Liso,liso)
			liso_err0=liso-calc_eiso(z,gbm['PFLX'][m2[i]]-gbm['PFLX_err'][m2[i]],f1,f2,Liso=True)
			liso_err1=calc_eiso(z,gbm['PFLX'][m2[i]]+gbm['PFLX_err'][m2[i]],f1,f2,Liso=True)-liso
			Liso_err0=np.append(Liso_err0,liso_err0)
			Liso_err1=np.append(Liso_err1,liso_err1)

		else:
			redshifts=np.append(redshifts,np.nan)
			Eiso=np.append(Eiso,np.nan)
			Eiso_err0=np.append(Eiso_err0,np.nan)
			Eiso_err1=np.append(Eiso_err1,np.nan)
			Liso=np.append(Liso,np.nan)
			Liso_err0=np.append(Liso_err0,np.nan)
			Liso_err1=np.append(Liso_err1,np.nan)


	## plot Eiso vs z
	fix,ax=plot.subplots()
	## separate short & long bursts
	s=np.where((gbm[m2]['T90']!=0) & (gbm[m2]['T90']<=2) & (gbm['FLNC_BEST_FITTING_MODEL'][m2] != 'FLNC_PLAW'))[0]
	l=np.where((gbm[m2]['T90']>2)  & (gbm['FLNC_BEST_FITTING_MODEL'][m2] != 'FLNC_PLAW'))[0]

	spl=np.where((gbm[m2]['T90']<=2) & (gbm['FLNC_BEST_FITTING_MODEL'][m2] == 'FLNC_PLAW'))[0]
	lpl=np.where((gbm[m2]['T90']>2)  & (gbm['FLNC_BEST_FITTING_MODEL'][m2] == 'FLNC_PLAW'))[0]

	plot.scatter(redshifts[l],Eiso[l],label='Long GRBs',linestyle='None',marker='o',color='C0',alpha=aval)
#	plot.errorbar(redshifts[l],Eiso[l[0]],yerr=[Eiso_err0[l[0]],Eiso_err1[l[0]]],linestyle='None',color='C0',alpha=aval)
	plot.errorbar(redshifts[lpl],Eiso[lpl],0.7*Eiso[lpl],linestyle='None',marker='o',uplims=True,color='C0',alpha=aval)
#	plot.errorbar(redshifts[s],Eiso[s[0]],yerr=[Eiso_err0[s[0]],Eiso_err1[s[0]]],linestyle='None',zorder=32,color='C1',alpha=aval)
	plot.scatter(redshifts[s],Eiso[s],label='Short GRBs',linestyle='None',marker='o',color='C1',zorder=32,alpha=aval)
	wn15=np.where(gbm['GBMNAME'][m2[spl]]!='GRB150101641')[0]
	wn15=spl[wn15]
	plot.errorbar(redshifts[wn15],Eiso[wn15],0.7*Eiso[wn15],linestyle='None',marker='o',uplims=True,color='C1',zorder=32,alpha=aval)

	#GRB 170817A
	gwz=0.009845
	gwdist=42.9
	gwdisterr=3.2
	gwzerr=0.01057-gwz


	## 'standard' fit from GBM-only paper
	gwfluence=1.4e-7
	gwfluenceerr=0.3e-7
	gwpl=0.14
	gwplerr=0.59
	gwepeak=215.1
	gwepeakerr=54.2

	## 'detailed' fit from GBM-only paper (Comp+BB)
	gwcfluence=3.1e-7*(0.256+0.32) # from table 3
	gwcfluenceerr=0.7e-7*(0.256+0.32)
	gwcpl=-0.62
	gwcplerr=0.4
	gwcepeak=185
	gwcepeakerr=62

	gwbbfluence=0.53e-7*(1.984-0.832)
	gwbbfluenceerr=0.1e-7*(1.984-0.832)
	gwbbkt=10.3
	gwbbkterr=1.5

	## peak flux spectrum
	gwpflux=7.3e-7
	gwpfluxerr=2.5e-7
	gwpflpl=0.85
	gwpflplerr=1.38
	gwpflepeak=229.2
	gwpflepeakerr=78.08

	eng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+gwz)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)

	### Eiso using standard fluence interval 
	f1=comp(eng,gwpl,gwepeak)
	f2=comp(eng2,gwpl,gwepeak)

	gweiso_standard=calc_eiso(gwz,gwfluence,f1,f2,lumdist=gwdist)
	gweiso_err0_standard=gweiso_standard-calc_eiso(gwz-gwzerr,gwfluence-gwfluenceerr,f1,f2,lumdist=gwdist-gwdisterr)
	gweiso_err1_standard=calc_eiso(gwz+gwzerr,gwfluence+gwfluenceerr,f1,f2,lumdist=gwdist+gwdisterr)-gweiso_standard

	gweiso=gweiso_standard
	gweiso_err0=gweiso_err0_standard
	gweiso_err1=gweiso_err1_standard

	### Eiso using detailed interval Comptonized component
	f1=comp(eng,gwcpl,gwcepeak)
	f2=comp(eng2,gwcpl,gwcepeak)

	gweiso_nt=calc_eiso(gwz,gwcfluence,f1,f2,lumdist=gwdist)
	gweiso_err_nt=gwcfluenceerr/gwcfluence*gweiso_nt
	gweiso_err0_nt=gweiso_nt-calc_eiso(gwz-gwzerr,gwcfluence-gwcfluenceerr,f1,f2,lumdist=gwdist-gwdisterr)
	gweiso_err1_nt=calc_eiso(gwz+gwzerr,gwcfluence+gwcfluenceerr,f1,f2,lumdist=gwdist+gwdisterr)-gweiso_nt

	### Eiso using detailed interval blackbody component
	f1=bbody(eng,gwbbkt)
	f2=bbody(eng2,gwbbkt)

	gweiso_t=calc_eiso(gwz,gwbbfluence,f1,f2,lumdist=gwdist)
	gweiso_err0_t=gweiso_t-calc_eiso(gwz-gwzerr,gwbbfluence-gwbbfluenceerr,f1,f2,lumdist=gwdist-gwdisterr)
	gweiso_err1_t=calc_eiso(gwz+gwzerr,gwbbfluence+gwbbfluenceerr,f1,f2,lumdist=gwdist+gwdisterr)-gweiso_t

	gweiso_d=gweiso_nt+gweiso_t
	gweiso_err0_d=np.sqrt(gweiso_err0_nt**2+gweiso_err0_t**2)
	gweiso_err1_d=np.sqrt(gweiso_err1_nt**2+gweiso_err1_t**2)

	print 'GW Eiso (standard) = ','{:0.2e}, {:0.2e}, {:0.2e}'.format(gweiso,gweiso_err0,gweiso_err1)
	print 'GW Eiso (comp) = ','{:0.2e}, {:0.2e}, {:0.2e}'.format(gweiso_nt,gweiso_err0_nt,gweiso_err1_nt)
	print 'GW Eiso (bbody) = ','{:0.2e}, {:0.2e}, {:0.2e}'.format(gweiso_t,gweiso_err0_t,gweiso_err1_t)
	print 'GW Eiso (total) = ','{:0.2e}, {:0.2e}, {:0.2e}'.format(gweiso_d,gweiso_err0_d,gweiso_err1_d)
	plot.scatter(gwz,gweiso,label='GRB 170817A',color='magenta',marker='*',s=100,zorder=32)
	plot.errorbar(gwz,gweiso,yerr=[[gweiso_err0,gweiso_err1]],color='magenta',alpha=aval)

	## add GRB150101B to plot
	w15=np.where(gbm['GBMNAME'][m2]=='GRB150101641')[0]
	plot.scatter(redshifts[w15],Eiso[w15],label='GRB 150101B',color='green',marker='*',s=100,zorder=33)
	plot.errorbar(redshifts[w15],Eiso[w15],0.7*Eiso[w15],color='green',uplims=True,linestyle='None')
	print gbm['GBMNAME'][m2[w15]][0],redshifts[w15][0],Eiso[w15][0],gbm['FLNC_BEST_FITTING_MODEL'][m2[w15]][0]
 	z=np.arange(0.001,9,0.01)

 	#detection limit, approximated
 	detlim=1e-7
	lumlim=detlim*4*np.pi*((cosmo.luminosity_distance(z).value*1e6*pc2cm)**2)/(1.+z)
	plot.plot(z,lumlim,linestyle='--',color='black')

	plot.legend(loc=2)
	plot.yscale('log')
	plot.xscale('log')
	plot.xlabel('Redshift (z)')
	plot.ylabel(r'$E_{iso}$ (1 keV - 10 MeV) (erg)')
	ax.yaxis.set_major_locator(LogLocator(numticks=15))
	ax.yaxis.set_minor_locator(LogLocator(numticks=15,subs=np.arange(2,10)))
#	minor_ticks = np.arange(0, 9, 0.5)
#	ax.set_xticks(minor_ticks, minor = True)
	ax.tick_params(direction='in',axis='both',which='both',right='on',top='on')

#	plot.xlim([-0.5,9])
	plot.xlim([0.005,10])
	plot.ylim([1e46,1e56])
	plot.savefig('GBM_Eiso_z.pdf', bbox_inches='tight')
	plot.close()


	### plot Liso vs z
	fig,ax=plot.subplots()

	s=np.where((gbm[m2]['T90']<=2) & (gbm['PFLX_BEST_FITTING_MODEL'][m2] != 'PFLX_PLAW'))[0]
	l=np.where((gbm[m2]['T90']>2)  & (gbm['PFLX_BEST_FITTING_MODEL'][m2] != 'PFLX_PLAW'))[0]

	spl=np.where((gbm[m2]['T90']<=2) & (gbm['PFLX_BEST_FITTING_MODEL'][m2] == 'PFLX_PLAW'))[0]
	lpl=np.where((gbm[m2]['T90']>2)  & (gbm['PFLX_BEST_FITTING_MODEL'][m2] == 'PFLX_PLAW'))[0]

	plot.scatter(redshifts[l],Liso[l],label='Long GRBs',linestyle='None',marker='o',color='C0',alpha=aval)
	plot.errorbar(redshifts[lpl],Liso[lpl],0.7*Liso[lpl],linestyle='None',marker='o',uplims=True,color='C0',alpha=aval)
	## GRBs with best fit spectra as power laws overestimate Eiso because the spectrum must turn over, so draw them as upper limits
	plot.scatter(redshifts[s],Liso[s],label='Short GRBs',linestyle='None',marker='o',color='C1',zorder=32,alpha=aval)
	wn15=np.where(gbm['GBMNAME'][m2[spl]]!='GRB150101641')[0]
	wn15=spl[wn15]
	plot.errorbar(redshifts[wn15],Liso[wn15],0.7*Liso[wn15],linestyle='None',marker='o',uplims=True,color='C1',zorder=32,alpha=aval)
#	plot.errorbar(redshifts[spl],Liso[spl],0.7*Liso[spl],linestyle='None',marker='o',uplims=True,color='C1',zorder=32,alpha=aval)

	eng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+gwz)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)
	f1p=comp(eng,gwpflpl,gwpflepeak)
	f2p=comp(eng2,gwpflpl,gwpflepeak)
	ef1p=comp(eng,gwpflpl+gwpflplerr,gwpflepeak+gwpflepeakerr)
	ef2p=comp(eng2,gwpflpl+gwpflplerr,gwpflepeak+gwpflepeakerr)
	ef1n=comp(eng,gwpflpl-gwpflplerr,gwpflepeak-gwpflepeakerr)
	ef2n=comp(eng2,gwpflpl-gwpflplerr,gwpflepeak-gwpflepeakerr)

	gwliso=calc_eiso(gwz,gwpflux,f1p,f2p,lumdist=gwdist,Liso=True)
	gwliso_err0=gwliso-calc_eiso(gwz,gwpflux-gwpfluxerr,ef1n,ef2n,Liso=True)
	gwliso_err1=calc_eiso(gwz,gwpflux+gwpfluxerr,ef1p,ef2p,Liso=True)-gwliso

	print 'GW Liso = ','{:0.2e}, {:0.2e}, {:0.2e}'.format(gwliso,gwliso_err0,gwliso_err1)
	plot.scatter(gwz,gwliso,label='GRB 170817A',color='magenta',marker='*',s=100,zorder=32)
	plot.errorbar(gwz,gwliso,yerr=[[gwliso_err0,gwliso_err1]],color='magenta')
	z=np.arange(0.001,9,0.01)
	detlim=2.7e-7#2.4e-7
	lumlim=detlim*4*np.pi*((cosmo.luminosity_distance(z).value*1e6*pc2cm)**2)#/(1.+z)
	plot.plot(z,lumlim,linestyle='--',color='black')

	## add GRB150101B to plot
	w15=np.where(gbm['GBMNAME'][m2]=='GRB150101641')[0]
	plot.scatter(redshifts[w15],Liso[w15],label='GRB 150101B',color='green',marker='*',s=100,zorder=32)
	plot.errorbar(redshifts[w15],Liso[w15],0.7*Liso[w15],uplims=True,color='green',linestyle='None',zorder=35)
	print gbm['GBMNAME'][m2[w15]][0],redshifts[w15][0],Liso[w15][0],gbm['PFLX_BEST_FITTING_MODEL'][m2[w15]][0]

 	z=np.arange(0.001,9,0.01)

	plot.legend(loc=2)
	plot.xlabel('Redshift (z)')
	plot.ylabel(r'$L_{iso}$ (1 keV - 10 MeV) (erg/s)')
#	plot.xlim([-0.5,9])
	plot.xlim([0.005,10])
	plot.ylim([1e47,1e55])
	plot.yscale('log')
	plot.xscale('log')
#	ax.yaxis.set_major_locator(LogLocator(numticks=15))
#	ax.yaxis.set_minor_locator(LogLocator(numticks=15,subs=np.arange(2,10)))
	ax.tick_params(direction='in',axis='both',which='both')
#	minor_ticks = np.arange(0, 9, 0.5)
#	ax.set_xticks(minor_ticks, minor = True)
	ax.tick_params(direction='in',axis='both',which='both',right='on',top='on')

	plot.savefig('GBM_Liso_z.pdf', bbox_inches='tight')
	plot.close()


	### Calc precursor Liso limit
	pregwalpha=-1.9
	pregwbeta=-3.7
	pregwepeak=70
	pregwflux=2.1e-7

	eng=np.logspace(np.log10(Emin),np.log10(Emax),100)/(1.+gwz)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),100)

	f1=band(eng,pregwalpha,pregwepeak,pregwbeta)
	f2=band(eng2,pregwalpha,pregwepeak,pregwbeta)
	preliso=calc_eiso(gwz,pregwflux,f1,f2,Liso=True)
	print 'Precursor Liso lim = ','{:0.2e}'.format(preliso)

	## write out Eiso, Liso
	data1=Table([gbm[m2[s]]['GBMNAME'],Liso[s],gbm[m2[s]]['PFLX_BEST_FITTING_MODEL']],names=['GRB','Liso','PFLX_BEST_FITTING_MODEL'])
	data2=Table([gbm[m2[spl]]['GBMNAME'],Liso[spl],gbm[m2[spl]]['PFLX_BEST_FITTING_MODEL']],names=['GRB','Liso','PFLX_BEST_FITTING_MODEL'])
	ascii.write(vstack(data1,data2),'GBM_sGRB_Liso.dat')

	return m1,m2,Eiso,Liso

def match_catalogs_name(name1,name2):

	ind_dict = dict((k,i) for i,k in enumerate(name1))
	inter = set(ind_dict).intersection(name2)
	m1 = [ ind_dict[x] for x in inter ]

	ind_dict = dict((k,i) for i,k in enumerate(name2))
	inter = set(ind_dict).intersection(name1)
	m2 = [ ind_dict[x] for x in inter ]

	return m1,m2

def match_catalogs_time_coord(ra1,dec1,met1,ra2,dec2,met2):

	c=SkyCoord(ra=ra1*u.deg,dec=dec1*u.deg)
	d=SkyCoord(ra=ra2*u.deg,dec=dec2*u.deg)

	m1=[]
	m2=[]
	for i in range(len(c)):

		dist=c[i].separation(d)
		fsep=abs(met1[i]-met2)
		w=np.where((fsep < 60) & (dist<50.*u.deg))
		if len(w[0])>0:
			m1=np.append(m1,i)
			m2=np.append(m2,w[0])

	m1=m1.astype('int')
	m2=m2.astype('int')

	return m1,m2

### read in GBM GRB catalog downloaded from https://heasarc.gsfc.nasa.gov/W3Browse/fermi/fermigbrst.html
def load_GBM():

	gbm=fits.open('gbmgrbcat.fits')
	gbm=gbm[1].data
	cols=gbm.columns

	met0=Time('2001-01-01 00:00:00',format='iso',scale='utc')	

	mo=[] ; alpha=[] ; alpha_neg_err=[] ; alpha_pos_err=[]
	epeak=[] ; epeak_neg_err=[] ; epeak_pos_err=[]
	beta=[] ; beta_neg_err=[] ; beta_pos_err=[]
	ra=[] ; dec=[] ; t90=[] ; t90_err=[] ; t90_start=[]
	fluence=[] ; fluence_err=[] 
	gbmname=[] ; trigtime=[] ; met=[]
	pflux_mo=[] ; pflux=[] ; pflux_err=[] ; pflux_alpha=[] 
	pflux_alpha_neg_err=[] ; pflux_alpha_pos_err=[]
	pflux_epeak=[] ; pflux_epeak_neg_err=[] ; pflux_epeak_pos_err=[]
	pflux_beta=[] ; pflux_beta_neg_err=[] ; pflux_beta_pos_err=[]
	pfluxph64=[] ; pfluxph256=[] ; pfluxph1024=[]
	i=0

	for j in range(len(gbm)):
		gbmname=np.append(gbmname,gbm.NAME[i]) 
		pflux_mo=np.append(pflux_mo,gbm.PFLX_BEST_FITTING_MODEL[i].strip())
		mo=np.append(mo,gbm.FLNC_BEST_FITTING_MODEL[i].strip())
		ra=np.append(ra,float(gbm.RA[i]))
		dec=np.append(dec,float(gbm.DEC[i]))

		if gbm.T90[i] != '        ':
			t90=np.append(t90,float(gbm.T90[i]))
			t90_err=np.append(t90_err,float(gbm.T90_ERROR[i]))
			t90_start=np.append(t90_start,float(gbm.T90_START[i]))
		else:
			t90=np.append(t90,np.nan)
			t90_err=np.append(t90_err,np.nan)
			t90_start=np.append(t90_start,np.nan)
		if gbm.FLUENCE[i] != '          ':
			fluence=np.append(fluence,float(gbm.FLUENCE[i]))
			fluence_err=np.append(fluence_err,float(gbm.FLUENCE_ERROR[i]))
		else:
			fluence=np.append(fluence,0.)
			fluence_err=np.append(fluence_err,0)
		if gbm.FLUX_64[i] != '         ':
			pfluxph64=np.append(pfluxph64,float(gbm.FLUX_64[i]))
		else: pfluxph64=np.append(pfluxph64,np.nan)
		if gbm.FLUX_256[i] != '         ':
			pfluxph256=np.append(pfluxph256,float(gbm.FLUX_256[i]))
		else: pfluxph256=np.append(pfluxph256,np.nan)
		if gbm.FLUX_1024[i] != '         ':
			pfluxph1024=np.append(pfluxph1024,float(gbm.FLUX_1024[i]))
		else: pfluxph1024=np.append(pfluxph1024,np.nan)

		if 'BAND' in gbm.FLNC_BEST_FITTING_MODEL[i]:
			alpha=np.append(alpha,gbm.FLNC_BAND_ALPHA[i])
			alpha_neg_err=np.append(alpha_neg_err,gbm.FLNC_BAND_ALPHA_NEG_ERR[i])
			alpha_pos_err=np.append(alpha_pos_err,gbm.FLNC_BAND_ALPHA_POS_ERR[i])
			epeak=np.append(epeak,gbm.FLNC_BAND_EPEAK[i])
			epeak_neg_err=np.append(epeak_neg_err,gbm.FLNC_BAND_EPEAK_NEG_ERR[i])
			epeak_pos_err=np.append(epeak_pos_err,gbm.FLNC_BAND_EPEAK_POS_ERR[i])
			beta=np.append(beta,gbm.FLNC_BAND_BETA[i])
			beta_neg_err=np.append(beta_neg_err,gbm.FLNC_BAND_BETA_NEG_ERR[i])
			beta_pos_err=np.append(beta_pos_err,gbm.FLNC_BAND_BETA_POS_ERR[i])

		elif 'COMP' in gbm.FLNC_BEST_FITTING_MODEL[i]:
			alpha=np.append(alpha,gbm.FLNC_COMP_INDEX[i])
			alpha_neg_err=np.append(alpha_neg_err,gbm.FLNC_COMP_INDEX_NEG_ERR[i])
			alpha_pos_err=np.append(alpha_pos_err,gbm.FLNC_COMP_INDEX_POS_ERR[i])
			epeak=np.append(epeak,gbm.FLNC_COMP_EPEAK[i])
			epeak_neg_err=np.append(epeak_neg_err,gbm.FLNC_COMP_EPEAK_NEG_ERR[i])
			epeak_pos_err=np.append(epeak_pos_err,gbm.FLNC_COMP_EPEAK_POS_ERR[i])
			beta=np.append(beta,0.)
			beta_neg_err=np.append(beta_neg_err,0.)
			beta_pos_err=np.append(beta_pos_err,0.)
		elif 'SBPL' in gbm.FLNC_BEST_FITTING_MODEL[i]:
			alpha=np.append(alpha,gbm.FLNC_SBPL_INDX1[i])
			alpha_neg_err=np.append(alpha_neg_err,gbm.FLNC_SBPL_INDX1_NEG_ERR[i])
			alpha_pos_err=np.append(alpha_pos_err,gbm.FLNC_SBPL_INDX1_POS_ERR[i])
			epeak=np.append(epeak,gbm.FLNC_SBPL_BRKEN[i])
			epeak_neg_err=np.append(epeak_neg_err,gbm.FLNC_SBPL_BRKEN_NEG_ERR[i])
			epeak_pos_err=np.append(epeak_pos_err,gbm.FLNC_SBPL_BRKEN_POS_ERR[i])
			beta=np.append(beta,gbm.FLNC_SBPL_INDX2[i])
			beta_neg_err=np.append(beta_neg_err,gbm.FLNC_SBPL_INDX2_NEG_ERR[i])
			beta_pos_err=np.append(beta_pos_err,gbm.FLNC_SBPL_INDX2_POS_ERR[i])
		elif 'PLAW' in gbm.FLNC_BEST_FITTING_MODEL[i]:
			alpha=np.append(alpha,gbm.FLNC_PLAW_INDEX[i])
			alpha_neg_err=np.append(alpha_neg_err,gbm.FLNC_PLAW_INDEX_NEG_ERR[i])
			alpha_pos_err=np.append(alpha_pos_err,gbm.FLNC_PLAW_INDEX_POS_ERR[i])
			epeak=np.append(epeak,0.)
			epeak_neg_err=np.append(epeak_neg_err,0.)
			epeak_pos_err=np.append(epeak_pos_err,0.)
			beta=np.append(beta,0.)
			beta_neg_err=np.append(beta_neg_err,0.)
			beta_pos_err=np.append(beta_pos_err,0.)
		else:
			alpha=np.append(alpha,0.)
			alpha_neg_err=np.append(alpha_neg_err,0.)
			alpha_pos_err=np.append(alpha_pos_err,0.)
			epeak=np.append(epeak,0.)
			epeak_neg_err=np.append(epeak_neg_err,0.)
			epeak_pos_err=np.append(epeak_pos_err,0.)
			beta=np.append(beta,0.)
			beta_neg_err=np.append(beta_neg_err,0.)
			beta_pos_err=np.append(beta_pos_err,0.)

		if 'BAND' in gbm.PFLX_BEST_FITTING_MODEL[i]:
			pflux=np.append(pflux,float(gbm.PFLX_BAND_ERGFLUX[i]))
			pflux_err=np.append(pflux_err,float(gbm.PFLX_BAND_ERGFLUX_ERROR[i]))
			pflux_alpha=np.append(pflux_alpha,gbm.PFLX_BAND_ALPHA[i])
			pflux_alpha_neg_err=np.append(pflux_alpha_neg_err,gbm.PFLX_BAND_ALPHA_NEG_ERR[i])
			pflux_alpha_pos_err=np.append(pflux_alpha_pos_err,gbm.PFLX_BAND_ALPHA_POS_ERR[i])
			pflux_epeak=np.append(pflux_epeak,gbm.PFLX_BAND_EPEAK[i])
			pflux_epeak_neg_err=np.append(pflux_epeak_neg_err,gbm.PFLX_BAND_EPEAK_NEG_ERR[i])
			pflux_epeak_pos_err=np.append(pflux_epeak_pos_err,gbm.PFLX_BAND_EPEAK_POS_ERR[i])
			pflux_beta=np.append(pflux_beta,gbm.PFLX_BAND_BETA[i])
			pflux_beta_neg_err=np.append(pflux_beta_neg_err,gbm.PFLX_BAND_BETA_NEG_ERR[i])
			pflux_beta_pos_err=np.append(pflux_beta_pos_err,gbm.PFLX_BAND_BETA_POS_ERR[i])

		elif 'COMP' in gbm.PFLX_BEST_FITTING_MODEL[i]:
			pflux=np.append(pflux,float(gbm.PFLX_COMP_ERGFLUX[i]))
			pflux_err=np.append(pflux_err,float(gbm.PFLX_COMP_ERGFLUX_ERROR[i]))
			pflux_alpha=np.append(pflux_alpha,gbm.PFLX_COMP_INDEX[i])
			pflux_alpha_neg_err=np.append(pflux_alpha_neg_err,gbm.PFLX_COMP_INDEX_NEG_ERR[i])
			pflux_alpha_pos_err=np.append(pflux_alpha_pos_err,gbm.PFLX_COMP_INDEX_POS_ERR[i])
			pflux_epeak=np.append(pflux_epeak,gbm.PFLX_COMP_EPEAK[i])
			pflux_epeak_neg_err=np.append(pflux_epeak_neg_err,gbm.PFLX_COMP_EPEAK_NEG_ERR[i])
			pflux_epeak_pos_err=np.append(pflux_epeak_pos_err,gbm.PFLX_COMP_EPEAK_POS_ERR[i])
			pflux_beta=np.append(pflux_beta,0.)
			pflux_beta_neg_err=np.append(pflux_beta_neg_err,0.)
			pflux_beta_pos_err=np.append(pflux_beta_pos_err,0.)

		elif 'SBPL' in gbm.PFLX_BEST_FITTING_MODEL[i]:
			pflux=np.append(pflux,float(gbm.PFLX_SBPL_ERGFLUX[i]))
			pflux_err=np.append(pflux_err,float(gbm.PFLX_SBPL_ERGFLUX_ERROR[i]))
			pflux_alpha=np.append(pflux_alpha,gbm.PFLX_SBPL_INDX1[i])
			pflux_alpha_neg_err=np.append(pflux_alpha_neg_err,gbm.PFLX_SBPL_INDX1_NEG_ERR[i])
			pflux_alpha_pos_err=np.append(pflux_alpha_pos_err,gbm.PFLX_SBPL_INDX1_POS_ERR[i])
			pflux_epeak=np.append(pflux_epeak,gbm.PFLX_SBPL_BRKEN[i])
			pflux_epeak_neg_err=np.append(pflux_epeak_neg_err,gbm.PFLX_SBPL_BRKEN_NEG_ERR[i])
			pflux_epeak_pos_err=np.append(pflux_epeak_pos_err,gbm.PFLX_SBPL_BRKEN_POS_ERR[i])
			pflux_beta=np.append(pflux_beta,gbm.PFLX_SBPL_INDX2[i])
			pflux_beta_neg_err=np.append(pflux_beta_neg_err,gbm.PFLX_SBPL_INDX2_NEG_ERR[i])
			pflux_beta_pos_err=np.append(pflux_beta_pos_err,gbm.PFLX_SBPL_INDX2_POS_ERR[i])

		elif 'PLAW' in gbm.PFLX_BEST_FITTING_MODEL[i]:
			pflux=np.append(pflux,float(gbm.PFLX_PLAW_ERGFLUX[i]))
			pflux_err=np.append(pflux_err,float(gbm.PFLX_PLAW_ERGFLUX_ERROR[i]))
			pflux_alpha=np.append(pflux_alpha,gbm.PFLX_PLAW_INDEX[i])
			pflux_alpha_neg_err=np.append(pflux_alpha_neg_err,gbm.PFLX_PLAW_INDEX_NEG_ERR[i])
			pflux_alpha_pos_err=np.append(pflux_alpha_pos_err,gbm.PFLX_PLAW_INDEX_POS_ERR[i])
			pflux_epeak=np.append(pflux_epeak,0.)
			pflux_epeak_neg_err=np.append(pflux_epeak_neg_err,0.)
			pflux_epeak_pos_err=np.append(pflux_epeak_pos_err,0.)
			pflux_beta=np.append(pflux_beta,0.)
			pflux_beta_neg_err=np.append(pflux_beta_neg_err,0.)
			pflux_beta_pos_err=np.append(pflux_beta_pos_err,0.)

		else:
			pflux=np.append(pflux,0.)
			pflux_err=np.append(pflux_err,0.)
			pflux_alpha=np.append(pflux_alpha,0.)
			pflux_alpha_neg_err=np.append(pflux_alpha_neg_err,0.)
			pflux_alpha_pos_err=np.append(pflux_alpha_pos_err,0.)
			pflux_epeak=np.append(pflux_epeak,0.)
			pflux_epeak_neg_err=np.append(pflux_epeak_neg_err,0.)
			pflux_epeak_pos_err=np.append(pflux_epeak_pos_err,0.)
			pflux_beta=np.append(pflux_beta,0.)
			pflux_beta_neg_err=np.append(pflux_beta_neg_err,0.)
			pflux_beta_pos_err=np.append(pflux_beta_pos_err,0.)

		utc=Time(float(gbm.TRIGGER_TIME[i]),format='mjd',scale='utc')
		tdiff=utc-met0
		tdiff.format='sec'
		met=np.append(met,tdiff.value)
		utc.format='iso'
		trigtime=np.append(trigtime,utc.value)

		i=i+1

	rtable=Table([gbmname,trigtime,met,ra,dec,t90,t90_err,t90_start,fluence,fluence_err,\
		#flux,flux_err,\
		mo,alpha,alpha_neg_err,alpha_pos_err,\
		epeak,epeak_neg_err,epeak_pos_err,beta,beta_neg_err,beta_pos_err,\
		pflux_mo,pflux,pflux_err,pflux_alpha,pflux_alpha_neg_err,pflux_alpha_pos_err,\
		pflux_epeak,pflux_epeak_neg_err,pflux_epeak_pos_err,\
		pflux_beta,pflux_beta_neg_err,pflux_beta_pos_err,\
		pfluxph64,pfluxph256,pfluxph1024],\
		names=['GBMNAME','TRIG_TIME','TRIG_MET','RA','Dec','T90',\
		'T90_err','T90_start','FLUENCE','FLUENCE_err',\
		'FLNC_BEST_FITTING_MODEL',\
		'FLNC_ALPHA','FLNC_ALPHA_neg_err','FLNC_ALPHA_pos_err',\
		'FLNC_EPEAK','FLNC_EPEAK_neg_err','FLNC_EPEAK_pos_err',\
		'FLNC_BETA','FLNC_BETA_neg_err','FLNC_BETA_pos_err',\
		'PFLX_BEST_FITTING_MODEL','PFLX','PFLX_err',\
		'PFLX_ALPHA','PFLX_ALPHA_neg_err','PFLX_ALPHA_pos_err',\
		'PFLX_EPEAK','PFLX_EPEAK_neg_err','PFLX_EPEAK_pos_err',\
		'PFLX_BETA','PFLX_BETA_neg_err','PFLX_BETA_pos_err',\
		'PFLX_PH_64','PFLX_PH_256','PFLX_PH_1024'],\
		dtype=('S12','S23','f4','f4','f4','f4','f4','f4','f4','f4','S20','f4','f4','f4','f4','f4','f4','f4','f4','f4','S20','f4','f4','f4','f4','f4','f4','f4','f4','f4','f4','f4','f4','f4','f4'))

	return rtable

### download and load GRBOX archive
def load_GRBOX():

	url='http://www.astro.caltech.edu/grbox/grboxtxt.php?form=submitted&starttime=080611&endtime=220101&sort=time&reverse=y&showindex=y&showt90=y&showra=y&showdec=y&showz=y&showut=y&xor=y&ref=y&observatory=t&obsdate=2017-08-18&posfmt=dec&xrtpos=gcn&format=txt'
	filename='grboxtxt.txt'
	urllib.urlretrieve(url,filename)
	grbox=ascii.read(filename,format='fixed_width',\
		names=['GRB','UT','T90','RA','DEC','z','det'],data_start=1,\
		col_starts=(0,8,17,23,36,49,55))

	ngrbox=len(grbox['GRB'])

	met0='2001-01-01 00:00:00'
	met=[]
	for i in range(ngrbox):

		if grbox['UT'][i] != '':
			year=grbox['GRB'][i][0:2]
			month=grbox['GRB'][i][2:4]
			day=grbox['GRB'][i][4:6]
			if ':' in grbox['UT'][i][0:2]: q=-1
			else: q=0
			hr=float(grbox['UT'][i][0:2+q])
			mn=float(grbox['UT'][i][3+q:5+q])
			sec=grbox['UT'][i][6+q:8+q]
			if sec != '': sec=float(sec)
			else: sec=0.
			utc='20'+year+'-'+month+'-'+day+' '+grbox[i]['UT']

			times=[met0,utc]
			t=Time(times,format='iso',scale='utc')
			tdiff=t[1]-t[0]
			tdiff.format='sec'
			met=np.append(met,tdiff.value)
		else: met=np.append(met,0.)

	# add short GRB redshifts not in GRBOX (from Fong et al. 2015)
	grbs=np.array(['111117A','100625A','100206A','100117A','080905A'])
	z=[2.211,0.452,0.407,0.915,0.122]
	m1,m2=match_catalogs_name(grbox['GRB'],grbs)
	grbox['z'][m1]=z

	w=np.where(grbox['z'].mask == False)
	grbox['z'][w]=np.array([x.replace('?','') for x in grbox['z'][w]])

	rtable=Table([grbox['GRB'],grbox['UT'],met,grbox['RA'],grbox['DEC'],grbox['z'],grbox['det']],\
		names=['GRB','TRIG_UT','TRIG_MET','RA','Dec','z','afterglow'])

	return rtable

def grb150101b(FLNC_Emin=10.,FLNC_Emax=1000.,Emin=1.,Emax=1e4):
## add GRB 150101B to energetics plots

	z=0.134
	dist=654.#637.7
	disterr=0
	zerr=0

	eng=np.logspace(np.log10(Emin),np.log10(Emax),1e4)/(1.+z)
	eng2=np.logspace(np.log10(FLNC_Emin),np.log10(FLNC_Emax),1e4)

	print 'Time integrated:'
	engflux=np.array([8.3,4.8,72.,3.1])*1e-7
	engfluxerr=np.array([1.4,1.1,8,0.4])*1e-7
	epeakkt=[0,0,550.,6.]
	epeakkterr=[0,0,190.,0.6]
	plind=[-1.8,-2.4,-0.8,0]
	plerr=[0.1,0.3,0.2,0]
	t1=[-0.064,0.0,-0.016,0.000]
	t2=[0.064,0.064,0.000,0.064]
	model=''
	par=''
	for i in range(len(engflux)):
		if ((plind[i] != 0) & (epeakkt[i] != 0)):
			f1=comp(eng,plind[i],epeakkt[i])
			f2=comp(eng2,plind[i],epeakkt[i])
			model='comp '
			par=str(plind[i])+' '+str(epeakkt[i])+' '
		if (plind[i]==0):
			f1=bbody(eng,epeakkt[i])
			f2=bbody(eng2,epeakkt[i])
			model='bbody '
			par=str(epeakkt[i])+' '
		if ((plind[i]!=0) & (epeakkt[i] == 0)):
			f1=pl(eng,plind[i])
			f2=pl(eng2,plind[i])
			model='pl '
			par=str(plind[i])+' '

		eiso=calc_eiso(z,engflux[i]*(t2[i]-t1[i]),f1,f2,lumdist=dist,Emin=Emin,Emax=Emax)
		eisoerr=eiso-calc_eiso(z,(engflux[i]-engfluxerr[i])*(t2[i]-t1[i]),f1,f2,lumdist=dist,Emin=Emin,Emax=Emax)		
		print '{:0.2} - {:0.2}: Eiso = {:0.2e} +/- {:0.2e} erg/s'.format(t1[i],t2[i],eiso,eisoerr), model,par, engflux[i]

		liso=calc_eiso(z,engflux[i],f1,f2,lumdist=dist,Liso=True,Emin=Emin,Emax=Emax)
		lisoerr=liso-calc_eiso(z,engflux[i]-engfluxerr[i],f1,f2,lumdist=dist,Liso=True,Emin=Emin,Emax=Emax)
		print '{:0.4} - {:0.2}: Liso = {:0.2e} +/- {:0.2e} erg/s'.format(t1[i],t2[i],liso,lisoerr),model,par, engflux[i]


	print 'Time resolved:'
	engflux=np.array([96,49,4.5,2.7,2.9,3.3])*1e-7
	engfluxerr=np.array([14,8,1.,0.8,0.7,0.7])*1e-7
	epeakkt=[1280,190,9,7.1,6.2,3.7]
	epeakkterr=[590,50,1.3,1.6,1.5,0.7]
	plind=[-0.4,-0.7,0,0,0,0]
	t1=[-0.016,-0.008,0.0,0.016,0.032,0.048]
	t2=[-0.008,0.000,0.016,0.032,0.048,0.064]
	model=''
	for i in range(len(engflux)):
		if plind[i] != 0:
			f1=comp(eng,plind[i],epeakkt[i])
			f2=comp(eng2,plind[i],epeakkt[i])
			model='comp'
		else:
			f1=bbody(eng,epeakkt[i])
			f2=bbody(eng2,epeakkt[i])			
			model='bbody'
		liso=calc_eiso(z,engflux[i],f1,f2,lumdist=dist,Liso=True,Emin=Emin,Emax=Emax)
		lisoerr=liso-calc_eiso(z,engflux[i]-engfluxerr[i],f1,f2,lumdist=dist,Liso=True,Emin=Emin,Emax=Emax)		
		print '{:0.4} - {:0.2}: Liso = {:0.2e} +/- {:0.2e} erg/s'.format(t1[i],t2[i],liso,lisoerr),model
