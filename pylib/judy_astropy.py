#!/usr/bin/env python
"""
------------------------------------------------------------------------

Scripts to do astrophysics

------------------------------------------------------------------------
"""
import scipy.integrate as integrate
from scipy import interpolate
import numpy as np

def pimms(en1,en2,pl,nh):

	# f1=int_pow_func([1.,pl],en1[0],en1[1])
	# f2=int_pow_func([1.,pl],en2[0],en2[1])

	# eng=np.logspace(np.log10(en2[0]),np.log10(en2[1]))
	# abs1=wabs(eng,nh)
	# abs2=

	# conv=
	#p1=integrate.quad(pow_func,en1[0],en1[1],args=([1.,pl-1]))[0]
	#p2=integrate.quad(pow_func,en2[0],en2[1],args=([1.,pl-1]))[0]

	# abs1=integrate.quad(wabs,en1[0],en1[1],args=(nh))[0]/(en1[1]-en1[0])
	# abs2=integrate.quad(wabs,en2[0],en2[1],args=(nh))[0]/(en2[1]-en2[0])
	# int1=abs1*p1
	# int2=abs2*p2

	# conv=int2/int1

	# print abs1,abs2
	# print int1,int2

	int1=integrate.quad(wabsPL,en1[0],en1[1],args=(nh,[1.,pl-1.]))[0]
	int2=integrate.quad(wabsPL,en2[0],en2[1],args=(nh,[1.,pl-1.]))[0]
	#print int1,int2
	#print int2/int1

	conv=int2/int1

	return conv

def wabsPL(eng,nh,p):

	f=wabs(eng,nh)*pow_func(eng,p)

	return f

def wabs(eng,nh,z=0.0):

	# reproducing XSPEC WABS models from Morrison & McCammon 1983
	## need to interpolate and input eng

	e1=np.array([0.03,0.1,0.284,0.4,0.532,0.707,0.867,1.303,1.84,2.471,3.21,4.038,7.111,8.331])
	e2=np.array([0.1,0.284,0.4,0.532,0.707,0.867,1.303,1.84,2.471,3.21,4.038,7.111,8.331,10.0])
	em=(e2-e1)/2.+e1

	c0=np.array([17.3,34.6,78.1,71.4,95.5,308.9,120.6,141.3,202.7,342.7,352.2,433.9,629.0,701.2])
	c1=np.array([608.1,267.9,18.8,66.8,145.8,-380.6,169.3,146.8,104.7,18.7,18.7,-2.4,30.9,25.2])
	c2=np.array([-2150.,-476.0,4.3,-51.4,-61.1,294.,-47.7,-31.5,-17.0,0.0,0.0,0.75,0.0,0.0])

	en=em*(1.+z)

	sigma_e1=(c0+c1*e1+c2*e1**2)*e1**(-3)*1e-24
	sigma_e2=(c0+c1*e2+c2*e2**2)*e2**(-3)*1e-24
	sigma_em=(c0+c1*em+c2*em**2)*em**(-3)*1e-24

	sigma_e=(c0+c1*en+c2*en**2)*en**(-3.)*1e-24
  
	gsigma_e=loginterpol(en,sigma_e,eng)

	ab=np.exp(-nh*gsigma_e)

	return ab


def loginterpol(x,y,x1):

	f=interpolate.interp1d(np.log10(x),np.log10(y),bounds_error=False,fill_value="extrapolate",kind='linear')
	y1=10**f(np.log10(x1))

	return y1

def crab(emin,emax):
	## I(E)9.7 * E^-1.1 in 2-10 keV from Kirsch et al.
	norm=9.7
	pow1=1.1
	cemin=2.
	cemax=10.
	crab1=int_pow_func([norm,pow1],cemin,cemax)

	nh=0#3.26e21
	crab2=integrate.quad(wabsPL,emin,emax,args=(nh,[norm,pow1]))[0]
	#crab2=int_pow_func([norm,pow1],emin,emax)

	kev2erg=1.60218e-9
	crab2=crab2*kev2erg

	return crab2

def int_pow_func(p,b1,b2):

	f=p[0]/(1.-p[1])*(b2**(1.-p[1])-b1**(1.-p[1]))

	return f

def pow_func(x,p):

	f=p[0]*x**(-p[1])

	return f

def kband(x,p):

  alpha=p[0]
  beta=p[2]
  e0=p[1]/(p[0]-p[2]) # input Epeak, use E0
  w0=np.where(x < (alpha-beta)*e0)
  w1=np.where(x > (alpha-beta)*e0)
  y=np.zeros(len(x))
  y[w0]=x[w0]*(x[w0]/100.)**p[0]*np.exp(-x[w0]/p[1])
  y[w1]=x[w1]*((p[0]-p[2])*p[1]/100.)**(p[0]-p[2])*np.exp(p[2]-p[0])*(x[w1]/100.)**p[2]

  return y

def kcorr(z,p,emin=1.,emax=10000.,pow=False,band=False):

	eng=np.logspace(emin,emax,num=1000)

	p=np.array(p)

	if pow:
		k1=integrate.quad(pow_func,emin/(1+z),emax/(1+z),args=(p))[0]
		k2=integrate.quad(pow_func,emin,emax,args=(p))[0]

	k=k1/k2

	return k