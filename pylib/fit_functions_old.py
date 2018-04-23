#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to do power law functions
x & xx must be np.array with xx.transpose()

------------------------------------------------------------------------
"""

import numpy as np
import importlib

def models():

	models='pow','bknpow','bkn2pow','bkn3pow','bkn4pow'
	intmodels='int'+models

	return models,intmodels

def call_function(function,x,p):

	mod=importlib.import_module('fit_functions')
	func=getattr(mod,p.model)
	yfit=func(x,p.par)

	return yfit

def logmean(x):

	m=10**((np.log10(x[0,:])+np.log10(x[1,:]))/2)+np.min(x)

	return m

def pow(x,p,xnorm=1):

	norm=p[0]
	pow1=p[1]

	f=norm*(x/xnorm)**(-pow1)

	return f

def bknpow(x,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]

	#f=np.zeros(len(x))
	#f[x<break1]=norm*x[x<break1]**(-pow1)
	#f[x>=break1]=norm*break1**(pow2-pow1)*x[x>=break1]**(-pow2)

	f1=np.array([norm*x[x<break1]**(-pow1)])
	f2=np.array([norm*break1**(pow2-pow1)*x[x>=break1]**(-pow2)])
	f=np.append(f1,f2)

	return f

def bkn2pow(x,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]

	f=np.zeros(len(x))
	f[x<break1]=norm*x[x<break1]**(-pow1)
	f[(x>=break1)&(x<break2)]=norm*break1**(pow2-pow1)*x[(x>=break1)&(x<break2)]**(-pow2)
	f[x>=break2]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*x[x>=break2]**(-pow3)

	return f

def bkn3pow(x,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]
	break3=p[6]
	pow4=p[7]

	f=np.zeros(len(x))
	f[x<break1]=norm*x[x<break1]**(-pow1)
	f[(x>=break1)&(x<break2)]=norm*break1**(pow2-pow1)*x[(x>=break1)&(x<break2)]**(-pow2)
	f[(x>=break2)&(x<break3)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)* \
		x[(x>=break2)&(x<break3)]**(-pow3)
	f[x>=break3]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*break3**(pow4-pow3)* \
		x[x>=break3]**(-pow4)

	return f


def bkn4pow(x,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]
	break3=p[6]
	pow4=p[7]
	break4=p[8]
	pow5=p[9]

	f=np.zeros(len(x))
	f[x<break1]=norm*x[x<break1]**(-pow1)
	f[(x>=break1)&(x<break2)]=norm*break1**(pow2-pow1)*x[(x>=break1)&(x<break2)]**(-pow2)
	f[(x>=break2)&(x<break3)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)* \
		x[(x>=break2)&(x<break3)]**(-pow3)
	f[(x>=break3)&(x<break4)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*break3**(pow4-pow3)* \
		x[(x>=break3)&(x<break4)]**(-pow4)
	f[x>=break4]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*break3**(pow4-pow3)*break4**(pow5-pow4)* \
		x[x>=break4]**(-pow5)

	return f

def gauss(x,p):

	norm=p[0]
	center=p[1]
	width=p[2]

	f=p[0]*exp(-(t-p[1])^2/(2*p[2]^2))

	return f

def gauss1_pow(x,p):

	f=gauss(x,p[2:5])+intpow(x,p[0:2])

	return f

def gauss2_pow(x,p):

	f=gauss(x,p[2:5])+gauss(x,p[5:8])+pow(x,p[0:2])

	return f

def gauss3_pow(x,p):

	f=gauss(x,p[2:5])+gauss(x,p[5:8])+gauss(x,p[8:11])+pow(x,p[0:2])

	return f

def gauss4_pow(x,p):

	f=gauss(x,p[2:5])+gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+pow(x,p[0:2])

	return f

def gauss5_pow(x,p):

	f=gauss(x,p[2:5])+gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+pow(x,p[0:2])

	return f

def gauss6_pow(x,p):

	f=gauss(x,p[2:5])+gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])\
		+gauss(x,p[17:20])+pow(x,p[0:2])

	return f

def gauss1_bknpow(x,p):

	f=gauss(x,p[5:8])+bknpow(x,p[0:4])

	return f

def gauss2_bknpow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+bknpow(x,p[0:4])

	return f

def gauss3_bknpow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+bknpow(x,p[0:4])

	return f

def gauss4_bknpow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+bknpow(x,p[0:4])

	return f

def gauss5_bknpow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+bknpow(x,p[0:4])

	return f

def gauss6_bknpow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+gauss(x,p[20:23])+bknpow(x,p[0:4])

	return f

def gauss1_bkn2pow(x,p):

	f=gauss(x,p[7:10])+bkn2pow(x,p[0:6])

	return f

def gauss2_bkn2pow(x,p):

	f=gauss(x,p[7:10])+gauss(x,p[10:13])+bkn2pow(x,p[0:6])

	return f

def gauss3_bkn2pow(x,p):

	f=gauss(x,p[7:10])+gauss(x,p[10:13])+gauss(x,p[13:16])+bkn2pow(x,p[0:6])

	return f

def gauss4_bkn2pow(x,p):

	f=gauss(x,p[7:10])+gauss(x,p[10:13])+gauss(x,p[13:16])+gauss(x,p[16:19])+bkn2pow(x,p[0:6])

	return f

def gauss5_bkn2pow(x,p):

	f=gauss(x,p[7:10])+gauss(x,p[10:13])+gauss(x,p[13:16])+gauss(x,p[16:19])+gauss(x,p[19:22])\
		+bkn2pow(x,p[0:6])

	return f

def gauss6_bkn2pow(x,p):

	f=gauss(x,p[7:10])+gauss(x,p[10:13])+gauss(x,p[13:16])+gauss(x,p[16:19])+gauss(x,p[19:22])\
		+gauss(x,p[22:25])+bkn2pow(x,p[0:6])

	return f

def gauss1_bkn3pow(x,p):

	f=gauss(x,p[5:8])+bkn3pow(x,p[0:6])

	return f

def gauss2_bkn3pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+bkn3pow(x,p[0:6])

	return f

def gauss3_bkn3pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+bkn3pow(x,p[0:6])

	return f

def gauss4_bkn3pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+bkn3pow(x,p[0:6])

	return f

def gauss5_bkn3pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+bkn3pow(x,p[0:6])

	return f

def gauss6_bkn3pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+gauss(x,p[20:23])+bkn3pow(x,p[0:6])

	return f

def gauss1_bkn4pow(x,p):

	f=gauss(x,p[5:8])+bkn4pow(x,p[0:8])

	return f

def gauss2_bkn4pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+bkn4pow(x,p[0:8])

	return f

def gauss3_bkn4pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+bkn4pow(x,p[0:8])

	return f

def gauss4_bkn4pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+bkn4pow(x,p[0:8])

	return f

def gauss5_bkn4pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+bkn4pow(x,p[0:8])

	return f

def gauss6_bkn4pow(x,p):

	f=gauss(x,p[5:8])+gauss(x,p[8:11])+gauss(x,p[11:14])+gauss(x,p[14:17])+gauss(x,p[17:20])\
		+gauss(x,p[20:23])+bkn4pow(x,p[0:8])

	return f

############ INTEGRAL VERSIONS

def intpow(xx,p):

	norm=p[0]
	pow1=p[1]

	f=norm/(1.-pow1)*(xx[1,:]**(1.-pow1)-xx[0,:]**(1.-pow1))
	f=f/(xx[1,:]-xx[0,:])

	return f

def intbknpow(xx,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]

	f=np.zeros(len(xx[0]))
	m=logmean(xx)

	f[m<break1]=norm/(1.-pow1)*(xx[1,m<break1]**(1.-pow1)-xx[0,m<break1]**(1.-pow1))
	f[m>=break1]=norm*break1**(pow2-pow1)/(1.-pow2)* \
		(xx[1,m>=break1]**(1.-pow2)-xx[0,m>=break1]**(1.-pow2))
	f=f/(xx[1,:]-xx[0,:])

	return f

def intbkn2pow(xx,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]

	f=np.zeros(len(xx[0]))
	m=logmean(xx)

	f[m<break1]=norm/(1.-pow1)*(xx[1,m<break1]**(1.-pow1)-xx[0,m<break1]**(1.-pow1))
	f[(m>=break1)&(m<break2)]=norm*break1**(pow2-pow1)/(1.-pow2)* \
		(xx[1,(m>=break1)&(m<break2)]**(1.-pow2)-xx[0,(m>=break1)&(m<break2)]**(1.-pow2))
	f[m>break2]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)/(1.-pow3)* \
		(xx[1,m>break2]**(1-pow3)-xx[0,m>break2]**(1.-pow3))
	f=f/(xx[1,:]-xx[0,:])

	return f

def intbkn3pow(xx,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]
	break3=p[6]
	pow4=p[7]

	f=np.zeros(len(xx[0]))
	m=logmean(xx)

	f[m<break1]=norm/(1.-pow1)*(xx[1,m<break1]**(1.-pow1)-xx[0,m<break1]**(1.-pow1))
	f[(m>=break1)&(m<break2)]=norm*break1**(pow2-pow1)/(1.-pow2)* \
		(xx[1,(m>=break1)&(m<break2)]**(1.-pow2)-xx[0,(m>=break1)&(m<break2)]**(1.-pow2))
	f[(m>=break2)&(m<break3)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)/(1.-pow3)* \
		(xx[1,(m>=break2)&(m<break3)]**(1-pow3)-xx[0,(m>=break2)&(m<break3)]**(1.-pow3))
	f[m>=break3]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*break3**(pow4-pow3)/(1.-pow4)* \
		(xx[1,m>=break3]**(1-pow4)-xx[0,m>=break3]**(1.-pow4))
	f=f/(xx[1,:]-xx[0,:])

	return f
	
def intbkn4pow(xx,p):

	norm=p[0]
	pow1=p[1]
	break1=p[2]
	pow2=p[3]
	break2=p[4]
	pow3=p[5]
	break3=p[6]
	pow4=p[7]
	break4=p[8]
	pow5=p[9]

	f=np.zeros(len(xx[0]))
	m=logmean(xx)

	f[m<break1]=norm/(1.-pow1)*(xx[1,m<break1]**(1.-pow1)-xx[0,m<break1]**(1.-pow1))
	f[(m>=break1)&(m<break2)]=norm*break1**(pow2-pow1)/(1.-pow2)* \
		(xx[1,(m>=break1)&(m<break2)]**(1.-pow2)-xx[0,(m>=break1)&(m<break2)]**(1.-pow2))
	f[(m>=break2)&(m<break3)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)/(1.-pow3)* \
		(xx[1,(m>=break2)&(m<break3)]**(1-pow3)-xx[0,(m>=break2)&(m<break3)]**(1.-pow3))
	f[(m>=break3)&(m<break4)]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)* \
		break3**(pow4-pow3)/(1.-pow4)*(xx[1,(m>=break3)&(m<break4)]**(1-pow4)- \
		xx[0,(m>=break3)&(m<break4)]**(1.-pow4))
	f[m>=break4]=norm*break1**(pow2-pow1)*break2**(pow3-pow2)*break3**(pow4-pow3)* \
		break4**(pow5-pow4)/(1.-pow4)* \
		(xx[1,m>=break4]**(1-pow4)-xx[0,m>=break4]**(1.-pow4))
	f=f/(xx[1,:]-xx[0,:])

	return f

def intgauss(xx,p):

	f=sqrt(math.pi/2.)*(-p[0])*p[2]*(math.erf((p[1]-xx[1,:])/(sqrt(2)*p[2]))-math.erf((p[1]-xx[0,:])/(sqrt(2)*p[2])))
	f=f/(xx[1,:]-xx[0,:])
	
	return f

def intgauss1_pow(x,p):

	f=intgauss(xx,p[2:5])+intpow(xx,p[0:2])

	return f

def intgauss2_pow(xx,p):

	f=intgauss(xx,p[2:5])+intgauss(xx,p[5:8])+intpow(xx,p[0:2])

	return f

def intgauss3_pow(xx,p):

	f=intgauss(xx,p[2:5])+intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intpow(xx,p[0:2])

	return f

def intgauss4_pow(xx,p):

	f=intgauss(xx,p[2:5])+intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intpow(xx,p[0:2])

	return f

def intgauss5_pow(xx,p):

	f=intgauss(xx,p[2:5])+intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intpow(xx,p[0:2])

	return f

def intgauss6_pow(xx,p):

	f=intgauss(xx,p[2:5])+intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])\
		+intgauss(xx,p[17:20])+intpow(xx,p[0:2])

	return f

def intgauss1_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intbknpow(xx,p[0:4])

	return f

def intgauss2_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intbknpow(xx,p[0:4])

	return f

def intgauss3_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intbknpow(xx,p[0:4])

	return f

def intgauss4_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intbknpow(xx,p[0:4])

	return f

def intgauss5_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intbknpow(xx,p[0:4])

	return f

def intgauss6_bknpow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intgauss(xx,p[20:23])+intbknpow(xx,p[0:4])

	return f

def intgauss1_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intbkn2pow(xx,p[0:6])

	return f

def intgauss2_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intgauss(xx,p[10:13])+intbkn2pow(xx,p[0:6])

	return f

def intgauss3_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intgauss(xx,p[10:13])+intgauss(xx,p[13:16])+intbkn2pow(xx,p[0:6])

	return f

def intgauss4_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intgauss(xx,p[10:13])+intgauss(xx,p[13:16])+intgauss(xx,p[16:19])+intbkn2pow(xx,p[0:6])

	return f

def intgauss5_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intgauss(xx,p[10:13])+intgauss(xx,p[13:16])+intgauss(xx,p[16:19])+intgauss(xx,p[19:22])\
		+intbkn2pow(xx,p[0:6])

	return f

def intgauss6_bkn2pow(xx,p):

	f=intgauss(xx,p[7:10])+intgauss(xx,p[10:13])+intgauss(xx,p[13:16])+intgauss(xx,p[16:19])+intgauss(xx,p[19:22])\
		+intgauss(xx,p[22:25])+intbkn2pow(xx,p[0:6])

	return f

def intgauss1_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intbkn3pow(xx,p[0:6])

	return f

def intgauss2_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intbkn3pow(xx,p[0:6])

	return f

def intgauss3_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intbkn3pow(xx,p[0:6])

	return f

def intgauss4_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intbkn3pow(xx,p[0:6])

	return f

def intgauss5_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intbkn3pow(xx,p[0:6])

	return f

def intgauss6_bkn3pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intgauss(xx,p[20:23])+intbkn3pow(xx,p[0:6])

	return f

def intgauss1_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intbkn4pow(xx,p[0:8])

	return f

def intgauss2_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intbkn4pow(xx,p[0:8])

	return f

def intgauss3_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intbkn4pow(xx,p[0:8])

	return f

def intgauss4_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intbkn4pow(xx,p[0:8])

	return f

def intgauss5_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intbkn4pow(xx,p[0:8])

	return f

def intgauss6_bkn4pow(xx,p):

	f=intgauss(xx,p[5:8])+intgauss(xx,p[8:11])+intgauss(xx,p[11:14])+intgauss(xx,p[14:17])+intgauss(xx,p[17:20])\
		+intgauss(xx,p[20:23])+intbkn4pow(xx,p[0:8])

	return f
