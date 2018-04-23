#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to collect data for LAT 2nd GRB Catalog

------------------------------------------------------------------------
"""

import numpy as np
import re
from astropy.io import fits
from astropy.table import Table
from astropy.io import ascii
import os
import urllib
import fileinput

def make_cat():

	#cat=load_BAtool_results()  # run at SLAC
	cat=load_cat0()
	cat=load_GRBOX(cat)
	cat=load_Nicola(cat)
	cat=load_BAT(cat)
	cat=load_GBM(cat)

	return cat

def load_cat():

	cat=Table.read('LAT_Cat_Table.html',format='ascii.html')

	return cat	


def load_cat0():

	cat=Table.read('LAT_Cat_Table0.html',format='ascii.html')

	return cat	

def read_catalog():

	### downloaded this manually
	url='http://glast-ground.slac.stanford.edu/LATBA/GRBCatalogReview.jsp'
	filename='LAT_GRB_cat.html'

	f=open(filename,'r')
	lines=f.readlines()

	l=0
	grb=[]
	met=[]
	date=[]
	time=[]
	ra=[]
	dec=[]
	ts=[]
	real=[]
	for line in lines: 
		if "CandidateReview.jsp?NAME=" in line:
			grb=np.append(grb,'GRB'+re.split('<|>|/',line)[4])
			met=np.append(met,float(re.split('<td>|</td>',lines[l+2])[1]))
			date=np.append(date,re.split('<td>|</td>',lines[l+3])[1])
			time=np.append(time,re.split('<td>|</td>',lines[l+4])[1])
			ra=np.append(ra,float(re.split('<td>|</td>',lines[l+5])[1]))
			dec=np.append(dec,float(re.split('<td>|</td>',lines[l+6])[1]))
			ts=np.append(ts,float(re.split('<td>|</td>',lines[l+7])[1]))
			real=np.append(real,re.split('title="|">',lines[l+9])[1])
			# if isreal == 'YES': r=True
			# if isreal == 'NO': r=False
			# real=np.append(real,r)
			
		l=l+1

	d=np.core.defchararray.add(date,' ')
	trigtime=np.core.defchararray.add(d,time)

# 	c1=fits.Column(name='grb',format='10A',array=grb)
# 	c2=fits.Column(name='met',format='D',array=met)
# 	c3=fits.Column(name='trigtime',format='23A',array=trigtime)
# 	c4=fits.Column(name='ra',format='D',array=ra)
# 	c5=fits.Column(name='dec',format='D',array=dec)
# 	c6=fits.Column(name='ts',format='D',array=ts)
# 	c7=fits.Column(name='real',format='L',array=real)
# 	coldefs = fits.ColDefs([c1, c2, c3, c4, c5, c6, c7])
# 	tbhdu = fits.BinTableHDU.from_columns(coldefs)
# 	cat=tbhdu.data
# 	s=np.argsort(cat['grb'])
#	cat=cat[s]

	n=len(grb)
#	catlist=[]
#	for i in range(n):
#		g={'grb':grb[i],'met':met[i],'trigtime':trigtime[i],'ra':ra[i],'dec':dec[i],'ts':ts[i],'real':real[i]}
#		catlist.append(g)

	s=np.argsort(grb)
#	cattable=Table([grb[s],met[s],trigtime[s],ra[s],dec[s],ts[s],real[s]],names=('grb','met','trigtime','ra','dec','ts','real'))

	return grb[s],met[s],trigtime[s],ra[s],dec[s],ts[s],real[s]

def load_BAtool_results():

	from decimal import Decimal
	grb,met,trigtime,ra,dec,ts,real=read_catalog()
	n=len(grb)
	z=np.zeros(n)
	na=np.chararray((n),itemsize=3)
	na[:]='---'
	na10=np.chararray((n),itemsize=10)
	na10[:]='----------'

	cat=Table([grb,na10,met,trigtime,z,z,z,ts,real,z,z,z,\
		z,z,z,z,z,z,z,z,z,z,z,z,z,z,na,na],\
		names=['grb','GRBNAME','met','trigtime','ra','dec','err','ts','real','LLE_sig','theta','zenith',\
		'source_events','transient_events','source_events_1GeV','transient_events_1GeV',\
		'max_photon_energy','max_photon_time','photon_flux','photon_flux_err',\
		'energy_flux','energy_flux_err',\
		'LAT_T90','LLE_T90','LAT_GCN',\
		'z','afterglow','BAT_det'])
		#'PH_index','temp_index',
		
	n=len(cat)
	for i in range(n):
		grb0=re.split('GRB',grb[i])[1]#cat[i]['grb']
		file='/nfs/slac/g/ki/ki08/kocevski/LATBA/DATA/GRBOUT/LTF/'+grb0+'/Prompt/results_'+grb0+'.txt'
#		file='results_'+grb+'.txt'
		if os.path.exists(file):
			print file
			f=open(file,'r')
			lines=f.readlines()

			l=0
			for line in lines:
				tmp=re.split('=|\n',line)
				if l == 0: 
					l=l+1
					continue
				var=tmp[0].replace(' ','')
				val=tmp[1].replace(' ','')
#				print var,val
				if 'FindSrc_DEC1' in line:
					cat[i]['dec']=float(val)
					cat[i]['ra']=float(re.split('=',lines[l+2])[1])
					cat[i]['err']=float(re.split('=',lines[l+1])[1])
				if 'LLE_DetMaxSign' in line:
					cat[i]['LLE_sig']=round(float(val),2)
				# if 'Galacticb' in line:
				# 	cat[i]['Galacticb']=float(val)
				# 	cat[i]['Galacticl']=float(re.split('=',lines[l+1])[1])
				if 'THETA' in line:
					cat[i]['theta']=round(float(val),1)
				if 'ZENITH' in line:
					cat[i]['zenith']=round(float(val),1)
				if var=='NumberOfEvents_S_ROI':
					cat[i]['source_events']=round(float(val))
				if var=='NumberOfEvents_T_ROI':
					cat[i]['transient_events']=round(float(val))
				if var=='NumberOfEvents1GeV_S_ROI':
					cat[i]['source_events_1GeV']=round(float(val))
				if var=='NumberOfEvents1GeV_T_ROI':
					cat[i]['transient_events_1GeV']=round(float(val))
				if 'MaxPhotonEnergy' in line:
					cat[i]['max_photon_energy']=round(float(val),1)
				if 'MaxPhotonEnergyTime' in line:
					cat[i]['max_photon_time']=round(float(val),1)
				if var=='LIKE_MY_FLUX':
					cat[i]['photon_flux']='%.2E' % Decimal(float(val))
					cat[i]['photon_flux_err']='%.2E' % Decimal(float(re.split('=',lines[l+3])[1]))
					cat[i]['energy_flux']='%.2E' % Decimal(float(re.split('=',lines[l+1])[1]))
					cat[i]['energy_flux_err']='%.2E' % Decimal(float(re.split('=',lines[l+2])[1]))



				l=l+1

#	ascii.write(cat,'LAT_Cat_Table.dat')
	cat=cat[cat['real']=='YES']
	cat.write('LAT_Cat_Table0.dat',format='ascii')
	cat.write('LAT_Cat_Table0.html',format='ascii.html')
	return cat

def load_GRBOX(cat):

	from astropy.coordinates import SkyCoord
	from astropy import units as u
	from astropy.time import Time

	#cat=Table.read('LAT_Cat_Table.html',format='ascii.html')
	grbox=ascii.read('grboxtxt.php',format='fixed_width',\
		names=['GRB','UT','T90','RA','DEC','z','det'],data_start=1,\
		col_starts=(0,8,17,23,36,49,55))

	ngrbox=len(grbox['GRB'])

	met0='2001-01-01 00:00:00'
#	gname=[]
#	gfrac=[]
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

	ncat=len(cat)
	cat['afterglow'].astype(str)
	cat[:]['afterglow']='---'

	for i in range(ncat):
		c=SkyCoord(ra=cat[i]['ra']*u.deg,dec=cat[i]['dec']*u.deg)
		d=SkyCoord(ra=grbox['RA']*u.deg,dec=grbox['DEC']*u.deg)
		dist=c.separation(d)
		fsep=abs(cat[i]['met']-met)
		w=np.where((fsep < 60) & (dist<5.*u.deg))
		if (len(w[0])>0):
#			print cat[i]['grb'],grbox[w]['GRB'][0],grbox[w]['z'][0]
			if float(grbox[w]['z'])>0:
				cat[i]['z']=grbox[w]['z'][0]
				cat[i]['GRBNAME']='GRB'+grbox[w[0]]['GRB'][0]
#			print cat[i]['afterglow'],grbox[w[0]]['det']
			# print
			# print grbox[w[0]]['det'][0]
			if grbox[w[0]]['det'].mask == False:
				cat[i]['afterglow']=grbox[w[0]]['det'][0]
				cat[i]['GRBNAME']='GRB'+grbox[w[0]]['GRB'][0]
#			print cat[i]['afterglow']
		# if 'X' in grbox[w]['det']: cat[i]['x_afterglow']='Y'
		# if 'O' in grbox[w]['det']: cat[i]['o_afterglow']='Y'
		# if 'R' in grbox[w]['det']: cat[i]['r_afterglow']='Y'


	cat.write('LAT_Cat_Table.html',format='ascii.html',overwrite=True)
	cat.write('LAT_Cat_Table.dat',format='ascii',overwrite=True)

	return cat

def load_Nicola(cat):

	filename='CatalogTable_Nicola.txt'

	GCNNAME = np.array([])
	RA = np.array([])
	DEC = np.array([])
	ERR = np.array([])
	SEP2CLOSEST = np.array([])
	THETA = np.array([])
	ZENITH = np.array([])
	LLE = np.array([])
	IRFS = np.array([])
	LIKE_SU = np.array([])
	TSTART_SU = np.array([])
	TSTOP_SU = np.array([])
	TS_SU = np.array([])
	Flux_SU = np.array([])
	Flux_SU_err = np.array([])
	Index_SU = np.array([])
	LIKE_BEST = np.array([])
	Index_SU_err = np.array([])
	TSTART_BEST = np.array([])
	TSTOP_BEST = np.array([])
	TS_BEST = np.array([])
	Flux_BEST = np.array([])
	Flux_BEST_err = np.array([])
	Index_BEST = np.array([])
	Index_BEST_err = np.array([])
	LIKE_AG = np.array([])
	TSTART_AG = np.array([])
	TSTOP_AG = np.array([])
	TS_AG = np.array([])
	Flux_AG = np.array([])
	Flux_AG_err = np.array([])
	Index_AG = np.array([])
	Index_AG_err = np.array([])

	# Read the file line by line
	for line in fileinput.input([filename]):

		# Skip lines with comment symbols
		if '#' in line:
			continue 

		# Split the line
		lineContents = line.split()

		# Extract the values
		GCNNAME = np.append(GCNNAME, lineContents[0])
		RA = np.append(RA, float(lineContents[1]))
		DEC = np.append(DEC, float(lineContents[2]))
		ERR = np.append(ERR, float(lineContents[3]))
		SEP2CLOSEST = np.append(SEP2CLOSEST, float(lineContents[4]))
		THETA = np.append(THETA, float(lineContents[5]))
		ZENITH = np.append(ZENITH, float(lineContents[6]))
		LLE = np.append(LLE, float(lineContents[7]))
		IRFS = np.append(IRFS, lineContents[8])
		TSTART_SU = np.append(TSTART_SU, float(lineContents[9]))
		TSTOP_SU = np.append(TSTOP_SU, float(lineContents[10]))
#		LIKE_SU = np.append(LIKE_SU, float(lineContents[11]))
		TS_SU = np.append(TS_SU, float(lineContents[11]))
		Flux_SU = np.append(Flux_SU, float(lineContents[12]))
		Flux_SU_err = np.append(Flux_SU_err, float(lineContents[13]))
		Index_SU = np.append(Index_SU, float(lineContents[14]))
		Index_SU_err = np.append(Index_SU_err, float(lineContents[15]))
#		LIKE_BEST = np.append(LIKE_BEST, float(lineContents[15]))
		TSTART_BEST = np.append(TSTART_BEST, float(lineContents[16]))
		TSTOP_BEST = np.append(TSTOP_BEST, float(lineContents[17]))
		TS_BEST = np.append(TS_BEST, float(lineContents[18]))
		Flux_BEST = np.append(Flux_BEST, float(lineContents[19]))
		Flux_BEST_err = np.append(Flux_BEST_err, float(lineContents[20]))
		Index_BEST = np.append(Index_BEST, float(lineContents[21]))
		Index_BEST_err = np.append(Index_BEST_err, float(lineContents[22]))
#		LIKE_AG = np.append(LIKE_AG, float(lineContents[21]))
		TSTART_AG = np.append(TSTART_AG, float(lineContents[23]))
		TSTOP_AG = np.append(TSTOP_AG, float(lineContents[24]))
		TS_AG = np.append(TS_AG, float(lineContents[25]))
		Flux_AG = np.append(Flux_AG, float(lineContents[26]))
		Flux_AG_err = np.append(Flux_AG_err, float(lineContents[27]))
		Index_AG = np.append(Index_AG, float(lineContents[28]))
		Index_AG_err = np.append(Index_AG_err, float(lineContents[29]))


	# Close the file
	fileinput.close()

	# Create a dictionary to store the results
	results = {}
	results['GCNNAME'] = GCNNAME
	results['RA'] = RA
	results['DEC'] = DEC
	results['ERR'] = ERR
	results['SEP2CLOSEST'] = SEP2CLOSEST
	results['THETA'] = THETA
	results['ZENITH'] = ZENITH
	results['LLE'] = LLE
	results['IRFS'] = IRFS
#	results['LIKE_SU'] = LIKE_SU
	results['TSTART_SU'] = TSTART_SU
	results['TSTOP_SU'] = TSTOP_SU
	results['TS_SU'] = TS_SU
	results['Flux_SU'] = Flux_SU
	results['Flux_SU_err'] = Flux_SU_err
	results['Index_SU'] = Index_SU
	results['Index_SU_err'] = Index_SU_err
#	results['LIKE_BEST'] = LIKE_BEST
	results['TSTART_BEST'] = TSTART_BEST
	results['TSTOP_BEST'] = TSTOP_BEST
	results['TS_BEST'] = TS_BEST
	results['Flux_BEST'] = Flux_BEST
	results['Flux_BEST_err'] = Flux_BEST_err
	results['Index_BEST'] = Index_BEST
	results['Index_BEST_err'] = Index_BEST_err
#	results['LIKE_AG'] = LIKE_AG
	results['TSTART_AG'] = TSTART_AG
	results['TSTOP_AG'] = TSTOP_AG
	results['TS_AG'] = TS_AG
	results['Flux_AG'] = Flux_AG
	results['Flux_AG_err'] = Flux_AG_err
	results['Index_AG'] = Index_AG
	results['Index_AG_err'] = Index_AG_err

	rtable=Table([GCNNAME,RA,DEC,ERR,SEP2CLOSEST,THETA,ZENITH,LLE,IRFS,\
		TSTART_SU,TSTOP_SU,TS_SU,Flux_SU,Flux_SU_err,Index_SU,Index_SU_err,\
		TSTART_BEST,TSTOP_BEST,TS_BEST,Flux_BEST,Flux_BEST_err,Index_BEST,Index_AG_err,\
		TSTART_AG,TSTOP_AG,TS_AG,Flux_AG,Flux_AG_err,Index_AG,Index_AG_err],\
		names=['GCNNAME','RA','DEC','ERR','SEP2CLOSEST','THETA','ZENITH','LLE','IRFS',\
		'TSTART_SU','TSTOP_SU','TS_SU','Flux_SU','Flux_SU_err','Index_SU','Index_SU_err',\
		'TSTART_BEST','TSTOP_BEST','TS_BEST','Flux_BEST','Flux_BEST_err','Index_BEST','Index_BEST_err',\
		'TSTART_AG','TSTOP_AG','TS_AG','Flux_AG','Flux_AG_err','Index_AG','Index_AG_err'])


#	results,rtable = CatalogTools.parseCatalogFile_Nicola('CatalogTable_Nicola.txt')

	# match GRB names, fill in table

	ngrb=[]
	for i in range(len(cat)): ngrb=np.append(ngrb,'GRB'+rtable['GCNNAME'][i])

	m1,m2=match(cat['grb'],ngrb)

#	r=Table(results)[m2]
	
	rtable=rtable[m2]
	cat.add_columns(rtable.columns.values())

	cat.write('LAT_Cat_Table.html',format='ascii.html',overwrite=True)
	cat.write('LAT_Cat_Table.dat',format='ascii',overwrite=True)

	return cat

def load_BAT(cat):

	url='http://swift.gsfc.nasa.gov/results/batgrbcat/summary_cflux/summary_general_info/summary_general.txt'
	file='BAT_summary_general.txt'

	if not os.path.exists(file):
		urllib.urlretrieve(url,file)

	f=open(file,'r')
	lines=f.readlines()
	lines=lines[23:]
	bgrb=[]
	for line in lines:
		d=np.array(re.split('\|| ',line))
		d=d[d!='']
		bgrb=np.append(bgrb,d[0])
	bgrb=np.array(bgrb)

	w=np.where(cat['GRBNAME'] != '----------')
	cgrb=np.array(cat['GRBNAME'][w[0]])
	m1,m2=match(cgrb,bgrb)
	m1=w[0][m1]

	cat['BAT_det']='N'
	cat['BAT_det'][m1]='Y'

	cat.write('LAT_Cat_Table.html',format='ascii.html',overwrite=True)
	cat.write('LAT_Cat_Table.dat',format='ascii',overwrite=True)

	return cat

def load_GBM(cat):

	gbm=fits.open('gbmcat.fits')
	gbm=gbm[1].data
	m1,m2=match(cat['grb'],gbm.NAME)
	gbm=gbm[m2]
	cols=gbm.columns

#	colname=[re.split(";|=|'",str(col))[2] for col in cols]
	
	#gbmcol=[gbm.field(c) for c in range(len(colname))]
	# doesn't work

	mo=[] ; alpha=[] ; alpha_neg_err=[] ; alpha_pos_err=[]
	epeak=[] ; epeak_neg_err=[] ; epeak_pos_err=[]
	beta=[] ; beta_neg_err=[] ; beta_pos_err=[]
	ra=[] ; dec=[] ; t90=[] ; t90_err=[] ; t90_start=[]
	fluence=[] ; fluence_err=[] ; flux=[] ; flux_err=[]
	gbmname=[]
	i=0
	for j in range(len(cat)):
		if (j in m1): 
			gbmname=np.append(gbmname,gbm.NAME[i]) 
			mo=np.append(mo,gbm.FLNC_BEST_FITTING_MODEL[i])
			ra=np.append(ra,gbm.RA[i])
			dec=np.append(dec,gbm.DEC[i])
			t90=np.append(t90,gbm.T90[i])
			t90_err=np.append(t90_err,gbm.T90_ERROR[i])
			t90_start=np.append(t90_start,gbm.T90_START[i])
			fluence=np.append(fluence,gbm.FLUENCE[i])
			fluence_err=np.append(fluence_err,gbm.FLUENCE_ERROR[i])
			flux=np.append(flux,gbm.FLUX_1024[i])
			flux_err=np.append(flux_err,gbm.FLUX_1024_ERROR[i])
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
			i=i+1
		else:
			gbmname=np.append(gbmname,'')
			mo=np.append(mo,'')
			alpha=np.append(alpha,0)
			alpha_neg_err=np.append(alpha_neg_err,0)
			alpha_pos_err=np.append(alpha_pos_err,0)
			epeak=np.append(epeak,0)
			epeak_neg_err=np.append(epeak_neg_err,0)
			epeak_pos_err=np.append(epeak_pos_err,0)
			beta=np.append(beta,0)
			beta_neg_err=np.append(beta_neg_err,0)
			beta_pos_err=np.append(beta_pos_err,0)
			ra=np.append(ra,0)
			dec=np.append(dec,0)
			t90=np.append(t90,0)
			t90_err=np.append(t90_err,0)
			t90_start=np.append(t90_start,0)
			fluence=np.append(fluence,0)
			fluence_err=np.append(fluence_err,0)
			flux=np.append(flux,0)
			flux_err=np.append(flux_err,0)


	rtable=Table([gbmname,ra,dec,t90,t90_err,t90_start,fluence,fluence_err,flux,flux_err,\
		mo,alpha,alpha_neg_err,alpha_pos_err,\
		epeak,epeak_neg_err,epeak_pos_err,beta,beta_neg_err,beta_pos_err],\
		names=['GBMNAME','GBM_RA','GBM_Dec','GBM_T90','GBM_T90_err','GBM_T90_start','GBM_FLUENCE','GBM_FLUENCE_err',\
		'GBM_PEAK_FLUX','GBM_PEAK_FLUX_err','GBM_FLNC_BEST_FITTING_MODEL',\
		'GBM_FLNC_ALPHA','GBM_FLNC_ALPHA_neg_err','GBM_FLNC_ALPHA_pos_err',\
		'GBM_FLNC_EPEAK','GBM_FLNC_EPEAK_neg_err','GBM_FLNC_EPEAK_pos_err',\
		'GBM_FLNC_BETA','GBM_FLNC_BETA_neg_err','GBM_FLNC_BETA_pos_err'])

	cat.add_columns(rtable.columns.values())

	cat.write('LAT_Cat_Table.html',format='ascii.html',overwrite=True)
	cat.write('LAT_Cat_Table.dat',format='ascii',overwrite=True)

	return cat

def match(a, b):

	m1 = []
	m2 = []
	for i in range(len(a)):
		w=np.where(b == a[i])
		if len(w[0]) > 0:
			m2=np.append(m2,w[0])
			m1=np.append(m1,i)

	m1=np.array(m1)
	m2=np.array(m2)

	m1=m1.astype(int)
	m2=m2.astype(int)

	return m1,m2

