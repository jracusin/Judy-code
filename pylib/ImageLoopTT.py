################################################################################
# This script uses localization sky maps to determine if the gravitational wave# 
# source is contained within a rectangular region under      #
# general rotation. The localization files are stored in speparate folders.    #
# Each localization map is named *.fits and are contained in separate #
# sub-directories each named for the eventID given by Leo P. Singer et. al.'s  #
# 2014 study. A separate file named "bayestarIndex.txt" was created using a    #
# shell script which archives all of the folders. The script also requires a   #
# text file named "inj.txt" containing the event ID and source location   #
#                                                                              #
# The script outputs the sky localization maps with the optimal field of view  #
# superimposed on them and a file named "eventdata.txt" containing the         #
# localization data for each event. The maps are visible while they are being  #
# constructed. The maps and the data is saved in the home directory in a       #
# directory named "RotFig."                                                    #
#                                                                              #
# To run this program:
#   python ./ImageLoopTT.py
#
# Written by Justin Tervala, August 2014                                       #
# with modifications by Peter Shawhan, October 2014 and later
################################################################################
import time
start = time.time()

import os
import numpy as np
import healpy as hp
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import math as mth
from astropy.table import Table
from itertools import chain

#Peter had to add these two lines:
from numpy import cos
from numpy import sin

# Function which contructs a Rodrigues rotation matrix and rotates a point about
# an axis by an arbitrary angle
def rotatePoint(rotAngle, axisPoint, rotPoint):
	v = 1-cos(rotAngle)
	rot11 = cos(rotAngle)+axisPoint[0]*axisPoint[0]*v
	rot21 = axisPoint[1]*axisPoint[0]*v+axisPoint[2]*sin(rotAngle)
	rot31 = axisPoint[2]*axisPoint[0]*v-axisPoint[1]*sin(rotAngle)
	rot12 = axisPoint[0]*axisPoint[1]*v-axisPoint[2]*sin(rotAngle)
	rot22 = cos(rotAngle)+axisPoint[1]*axisPoint[1]*v
	rot32 = axisPoint[2]*axisPoint[1]*v+axisPoint[0]*sin(rotAngle)
	rot13 = axisPoint[0]*axisPoint[2]*v+axisPoint[1]*sin(rotAngle)
	rot23 = axisPoint[1]*axisPoint[2]*v-axisPoint[0]*sin(rotAngle)
	rot33 = cos(rotAngle)+axisPoint[2]*axisPoint[2]*v
	x = rot11*rotPoint[0]+rot12*rotPoint[1]+rot13*rotPoint[2]
	y = rot21*rotPoint[0]+rot22*rotPoint[1]+rot23*rotPoint[2]
	z = rot31*rotPoint[0]+rot32*rotPoint[1]+rot33*rotPoint[2]
	return np.array([x,y,z])

# Function to find 4 axis points
def axisFind(baseRot, posVec, baseVec1, baseVec2):
	#-- Note: baseRot argument must be in degrees
	#Rotate base point 1 rotAng degrees to find first point, latFirst
	rot = mth.radians(baseRot)	
	axis1Vec = rotatePoint(rot, posVec, baseVec1)
	thetaaxis1, phiaxis1 = hp.vec2ang(axis1Vec)
	axis1Long = phiaxis1
	axis1Lat = 0.5*mth.pi - thetaaxis1
	#Rotate latFirst by 180 degrees to find second point, latSecond
	rot = mth.radians(180.0)
	axis2Vec = rotatePoint(rot, posVec, axis1Vec)
	thetaaxis2, phiaxis2 = hp.vec2ang(axis2Vec)
	axis2Long = phiaxis2
	axis2Lat = 0.5*mth.pi - thetaaxis2
	#Rotate base point 2 rotAng+90 degrees to find third point, longFirst
	rot = mth.radians(baseRot+90.0)	
	axis3Vec = rotatePoint(rot, posVec, baseVec2)
	thetaaxis3, phiaxis3 = hp.vec2ang(axis3Vec)
	axis3Long = phiaxis3
	axis3Lat = 0.5*mth.pi - thetaaxis3	
	#Rotate longFirst by 180 degrees to find fourth point, longSecond
	rot = mth.radians(180.0)
	axis4Vec = rotatePoint(rot, posVec, axis3Vec)
	thetaaxis4, phiaxis4 = hp.vec2ang(axis4Vec)
	axis4Long = phiaxis4
	axis4Lat = 0.5*mth.pi - thetaaxis4
	return [axis1Long, axis1Lat, axis2Long, axis2Lat, axis3Long, axis3Lat, axis4Long, axis4Lat]

def regionFind(maxPix):
	vecPos = hp.pix2vec(NSIDE,maxPix)
	theta, phi = hp.pix2ang(NSIDE, maxPix)
	ra = mth.degrees(phi)
	if ra > 180.0:
		ra -= 360.0
	dec = mth.degrees(0.5*mth.pi - theta)
	raRad = mth.radians(ra)
	decRad = mth.radians(dec)
	SINdecRad = mth.sin(decRad)
	COSdecRad = mth.cos(decRad)

	# :::::::::::::::::::::FIND AXIS POINTS OF GREAT CIRCLES::::::::::::::::::::
	# 4 lines, 2 to enclose the latitiude, 2 to enclose the latitude
	# have coordinates (lat,long): (axis1Lat, axis1Long), and 
	# (axis2Lat, axis2Long)
	# longitude lines to enclose longitude have coordinates 
	# (lat,long): (axis3Lat, axis3Long), and (axis4Lat, axis4Long). 
	# These points are rotated by 90 degrees to find rotation which encloses 
	# the maximum probability
	
	#Find point 75 degrees above or below the point of max. prob. density. This 
	#will be the basis of all axis points under all rotations
	if dec <= 0:
		latBaseLat1 = dec + 90.0 - FOVsize1/2.0
		latBaseLat2 = dec + 90.0 - FOVsize2/2.0
	else:
		latBaseLat1 = dec - 90.0 + FOVsize1/2.0
		latBaseLat2 = dec - 90.0 + FOVsize2/2.0
	latBaseTheta1 = mth.radians(90 - latBaseLat1)
	latBaseTheta2 = mth.radians(90 - latBaseLat2)
	latBaseVec1 = hp.ang2vec(latBaseTheta1, phi)
	latBaseVec2 = hp.ang2vec(latBaseTheta2, phi)
	
	#::::::DETERMINE ROTATION ANGLE WHICH MAXIMIZES ENCLOSED PROB. DENSITY::::::
	maxProbEncl = -1.0
	maxRot = -1.0

	#-- Determine maximum rotation angle for square vs. rectangle
	if FOVsize1 == FOVsize2:
		rotAngMax = 90
	else:
		rotAngMax = 180

	for ang in range(0,rotAngMax,rotAngInt):
		#(For Progress Visualization)
#		print "	Evaluating Rotation Angle: " + str(ang)
		Axes = axisFind(ang, vecPos, latBaseVec1, latBaseVec2)
		SINAxes1 = mth.sin(Axes[1])
		COSAxes1 = mth.cos(Axes[1])
		SINAxes3 = mth.sin(Axes[3])
		COSAxes3 = mth.cos(Axes[3])
		SINAxes5 = mth.sin(Axes[5])
		COSAxes5 = mth.cos(Axes[5])
		SINAxes7 = mth.sin(Axes[7])
		COSAxes7 = mth.cos(Axes[7])
		#Determine the enclosed probability density
		probEncl = 0.0
		ipixel = -1
		for j in SigPixels:
			ipixel += 1
			if SigPixUsed[ipixel] > 0: continue
			if MakePlots:
				rapoint = Imra[j]
				decpoint = Imdec[j]
				SINdecpoint = SINImdec[j]
				COSdecpoint = COSImdec[j]
			else:
				rapoint = Imra[ipixel]
				decpoint = Imdec[ipixel]
				SINdecpoint = SINImdec[ipixel]
				COSdecpoint = COSImdec[ipixel]

#			centerDot = COSdecpoint*COSdecRad*mth.cos(rapoint-raRad)+SINdecpoint*SINdecRad
#			if centerDot >=0:			
#				axis1Dot = COSdecpoint*COSAxes1*mth.cos(rapoint-Axes[0])+SINdecpoint*SINAxes1
#				axis2Dot = COSdecpoint*COSAxes3*mth.cos(rapoint-Axes[2])+SINdecpoint*SINAxes3
#				axis3Dot = COSdecpoint*COSAxes5*mth.cos(rapoint-Axes[4])+SINdecpoint*SINAxes5
#				axis4Dot = COSdecpoint*COSAxes7*mth.cos(rapoint-Axes[6])+SINdecpoint*SINAxes7
#				if axis1Dot >=0 and axis2Dot >=0 and axis3Dot >=0 and axis4Dot >=0:
#					probEncl += Im[j]

			if COSdecpoint*COSdecRad*mth.cos(rapoint-raRad)+SINdecpoint*SINdecRad >= 0:
				if COSdecpoint*COSAxes1*mth.cos(rapoint-Axes[0])+SINdecpoint*SINAxes1 >= 0:
					if COSdecpoint*COSAxes3*mth.cos(rapoint-Axes[2])+SINdecpoint*SINAxes3 >= 0:
						if COSdecpoint*COSAxes5*mth.cos(rapoint-Axes[4])+SINdecpoint*SINAxes5 >= 0:
							if COSdecpoint*COSAxes7*mth.cos(rapoint-Axes[6])+SINdecpoint*SINAxes7 >= 0:
								probEncl += Im[j]

		if probEncl > maxProbEncl:
			maxProbEncl = probEncl
			maxRot = ang 

	return [ra, dec, maxRot, vecPos, latBaseVec1, latBaseVec2, maxProbEncl]

def regionMark(centerPix,maxRot,code):
	vecPos = hp.pix2vec(NSIDE,centerPix)
	theta, phi = hp.pix2ang(NSIDE, centerPix)
	ra = mth.degrees(phi)
	if ra > 180.0:
		ra -= 360.0
	dec = mth.degrees(0.5*mth.pi - theta)
	raRad = mth.radians(ra)
	decRad = mth.radians(dec)
	SINdecRad = mth.sin(decRad)
	COSdecRad = mth.cos(decRad)

	# :::::::::::::::::::::FIND AXIS POINTS OF GREAT CIRCLES::::::::::::::::::::
	# 4 lines, 2 to enclose the latitiude, 2 to enclose the latitude
	# have coordinates (lat,long): (axis1Lat, axis1Long), and 
	# (axis2Lat, axis2Long)
	# longitude lines to enclose longitude have coordinates 
	# (lat,long): (axis3Lat, axis3Long), and (axis4Lat, axis4Long). 
	# These points are rotated by 90 degrees to find rotation which encloses 
	# the maximum probability
	
	#Find point 75 degrees above or below the point of max. prob. density. This 
	#will be the basis of all axis points under all rotations
	if dec <= 0:
		latBaseLat1 = dec + 90.0 - FOVsize1/2.0
		latBaseLat2 = dec + 90.0 - FOVsize2/2.0
	else:
		latBaseLat1 = dec - 90.0 + FOVsize1/2.0
		latBaseLat2 = dec - 90.0 + FOVsize2/2.0
	latBaseTheta1 = mth.radians(90 - latBaseLat1)
	latBaseTheta2 = mth.radians(90 - latBaseLat2)
	latBaseVec1 = hp.ang2vec(latBaseTheta1, phi)
	latBaseVec2 = hp.ang2vec(latBaseTheta2, phi)
	
	ang = maxRot
	Axes = axisFind(ang, vecPos, latBaseVec1, latBaseVec2)
	SINAxes1 = mth.sin(Axes[1])
	COSAxes1 = mth.cos(Axes[1])
	SINAxes3 = mth.sin(Axes[3])
	COSAxes3 = mth.cos(Axes[3])
	SINAxes5 = mth.sin(Axes[5])
	COSAxes5 = mth.cos(Axes[5])
	SINAxes7 = mth.sin(Axes[7])
	COSAxes7 = mth.cos(Axes[7])

	ipixel = -1
	for j in SigPixels:
		ipixel += 1
		if SigPixUsed[ipixel] > 0: continue
		if MakePlots:
			rapoint = Imra[j]
			decpoint = Imdec[j]
			SINdecpoint = SINImdec[j]
			COSdecpoint = COSImdec[j]
		else:
			rapoint = Imra[ipixel]
			decpoint = Imdec[ipixel]
			SINdecpoint = SINImdec[ipixel]
			COSdecpoint = COSImdec[ipixel]

		centerDot = COSdecpoint*COSdecRad*mth.cos(rapoint-raRad)+SINdecpoint*SINdecRad
		if centerDot >=0:			
			axis1Dot = COSdecpoint*COSAxes1*mth.cos(rapoint-Axes[0])+SINdecpoint*SINAxes1
			axis2Dot = COSdecpoint*COSAxes3*mth.cos(rapoint-Axes[2])+SINdecpoint*SINAxes3
			axis3Dot = COSdecpoint*COSAxes5*mth.cos(rapoint-Axes[4])+SINdecpoint*SINAxes5
			axis4Dot = COSdecpoint*COSAxes7*mth.cos(rapoint-Axes[6])+SINdecpoint*SINAxes7
			if axis1Dot >=0 and axis2Dot >=0 and axis3Dot >=0 and axis4Dot >=0:
				SigPixUsed[ipixel] = code

	return


def sourceCheck(SourceRA,SourceDec,regInfo):
	#Find axis points for this region
	Axes = axisFind(regInfo[2], regInfo[3], regInfo[4], regInfo[5])

	#:::::::CHECK IF SOURCE IS LOCALIZED
	# If the source is in all 4 hemispheres defined by the 4 axis points,
	# it is sufficiently localized. The source is localized if
	# the angle between the axis point and the source is <= 90 degrees
	# (dot product is >= 0).
		
	#Determine dot products between source and axis points
	SINSourceDec = mth.sin(SourceDec)
	COSSourceDec = mth.cos(SourceDec)
	axis1Dot = COSSourceDec*mth.cos(Axes[1])*mth.cos(SourceRA-Axes[0])+SINSourceDec*mth.sin(Axes[1])
	axis2Dot = COSSourceDec*mth.cos(Axes[3])*mth.cos(SourceRA-Axes[2])+SINSourceDec*mth.sin(Axes[3])
	axis3Dot = COSSourceDec*mth.cos(Axes[5])*mth.cos(SourceRA-Axes[4])+SINSourceDec*mth.sin(Axes[5])
	axis4Dot = COSSourceDec*mth.cos(Axes[7])*mth.cos(SourceRA-Axes[6])+SINSourceDec*mth.sin(Axes[7])

	if axis1Dot >=0 and axis2Dot >=0 and axis3Dot >=0 and axis4Dot >= 0:  
		return True
	else:
		return False

#------------------------------- Analysis parameters
ProjectDir = "TAOTAP"
#-- DataSet can be "2015", "2016", "O1", or "chosen"
DataSet = "chosen"
#-- SpecialSet could be "", "2015_10184" or "2016_288830" -- vars set below
#-- Note that SpecialSet overrides MaxImageCount, RotateQuads and MakePlots
SpecialSet = ""
#-- ChosenFile must be a single filename
#ChosenFile = "13635844.HL_LL.fits.gz"
#ChosenFile = "13686844.HL_LL.fits.gz"
#ChosenFile = "13619844.HLV_RPE.fits.gz"
#ChosenFile = "13719844.HLV_LL.fits.gz"
ChosenFile = "13779844.HLV_RPE.fits.gz"

EventSet = -1   #-- -1 means to process all events in set
#EventSet = range(0,14)  #-- Example to override default of doing all
#EventSet = range(5,6)

#-- FOVsize1 should be the longer dimension, if rectangular
FOVsize1 = 10
FOVsize2 = 10
SigPixelThresh = 1.0e-10
FixedResolution = True
#FixedNSIDE = 256   # 786432 pixels
FixedNSIDE = 128   # 196608 pixels
#FixedNSIDE = 64   # 49152 pixels
#FixedNSIDE = 32   # 12288 pixels
BruteForceSeed = False   #-- Find seed by integrating over a field (costly!)
rotAngInt = 5	#This variable should be an int, e.g. 5.  360 for no rotation
MaxImageCount = 3

##hack - overrides for IRT study
#FOVsize1 = 15
#FOVsize2 = 15
#FixedNSIDE = 256   # 786432 pixels
#rotAngInt = 30	#This variable should be an int, e.g. 5.  360 for no rotation
#MaxImageCount = 40

MakePlots = True
PlotMapTitle = True
PlotSourcePos = True
PlotRot = 180    #-- Use 180 to hide prime meridian line (place it at edge)
RotateQuads = 0  #-- Number of 90-degree rotations of the DATA to apply
FOVplotSteps = 40  #-- Must be even; 10 is OK for small FOVs, 40 good for large

#-- Overrides for special events
if DataSet == "2015" and SpecialSet == "2015_10184":
	EventSet = range(2,3)     #-- For 2015 event 10184
	MaxImageCount = 15      #-- For 2015 event 10184
	MakePlots = True       #-- For 2015 event 10184 or 2016 event 288830
	RotateQuads = 1   #-- For 2015 event 10184
elif DataSet == "2016" and SpecialSet == "2016_288830":
	EventSet = range(12,13)   #-- For 2016 event 288830
	MaxImageCount = 3      #-- For 2016 event 288830
	MakePlots = True       #-- For 2015 event 10184 or 2016 event 288830

#-- Colormap for plots
#FOVColor = 'w'
FOVColor = 'k'
GratColor = 'k'
# Note on color choices (first entry for each of RGB):
#    medblue  RGB = 0.4, 0.5, 1.0   with FOVcolor = 'w'
#    skyblue  RGB = 0.6, .75, 1.0   with FOVcolor = 'w'
#    ltblue   RGB = 0.8, 0.9, 1.0   with FOVcolor = 'k'
#    white    RGG = 1.0, 1.0, 1.0   with FOVcolor = 'k'
colordict = {'red':   [(0.0,  1.0, 1.0),
		       (0.2,  0.5, 0.5),
		       (0.4,  0.2, 0.2),
		       (0.6,  1.0, 1.0),
		       (0.8,  1.0, 1.0),
		       (1.0,  0.5, 0.5)],
	     'green': [(0.0,  1.0, 1.0),
		       (0.2,  1.0, 1.0),
		       (0.4,  1.0, 1.0),
		       (0.6,  1.0, 1.0),
		       (0.8,  0.0, 0.0),
		       (1.0,  0.0, 0.0)],
	     'blue':  [(0.0,  1.0, 1.0),
		       (0.2,  1.0, 1.0),
		       (0.4,  0.2, 0.2),
		       (0.6,  0.0, 0.0),
		       (0.8,  0.0, 0.0),
		       (1.0,  0.0, 0.0)]}
my_cmap = mcolors.LinearSegmentedColormap('CustomMap', colordict )
my_cmap.set_under("w",alpha="0") # sets underflow to white
#-- Alternative, to use a standard colormap:
#from matplotlib import cm
#my_cmap = cm.YlOrRd
#------------------------------- End of analysis parameters

# Set path for FITS file, and flag to do statistics for simulated events or not
if DataSet == "2015" or DataSet == "2016":
	SimEvents = True
	FitsPath = "/home/User/d/Lobster/LocFind/f2y/" + DataSet + "/fits/"
else:
	SimEvents = False
	FitsPath = "/home/User/d/Lobster/LocFind/" + ProjectDir + "/" + DataSet + "/fits/"

# Create list of skymap files in directory
if DataSet == "chosen":
	nFiles = 1
	fitsFiles = [ FitsPath+ChosenFile ]
	indexEventID = [str(1)]
else:
	fin = open( DataSet+'/bayestarIndex.txt', 'r')
	fitsFiles = fin.readlines()
	nFiles = len(fitsFiles)
	fin.close()
	indexEventID = []
	for i in range(0,nFiles):
	        #-- Extract the event ID (the directory name), which is a number
		evtdir, slash, evtfile = fitsFiles[i].partition("/");
		indexEventID.append(evtdir)
	        #-- Prepend the path to the file
		fitsFiles[i] = FitsPath + fitsFiles[i][0:-1]

#-- If an event set wasn't specified, do all entries
if EventSet == -1:
	EventSet = range(nFiles)

if SimEvents:
        #Find corresponding source data in data text file
	tCorresIndex = [] 
	tabl = Table.read( DataSet+'/inj.txt', format='ascii.cds')
	for i in range(nFiles):
		for j in range(len(tabl)):
			if tabl[j][0] == int(indexEventID[i]):
				tCorresIndex.append(j)
				break

#Open file for data and write header
try:
    os.stat( DataSet+"/RotFig" )
except:
    os.mkdir( DataSet+"/RotFig" )
f = open( DataSet+"/RotFig/Mdata.txt", 'w')
f.write("EventID Im LI Loc?   SourceRA SourceDec   PointRA  PointDec  PtRot    Encl    SumEncl \n")

#Loop over all skymap files
locCount = 0
for i in EventSet:
	#(For Progress Visualization)
	print "===== Evaluating Event " + str(i+1) + " of " + str(nFiles) + ".  ID=" + indexEventID[i]
	
	if SimEvents:
	        #-- Look up the source coordinates for this event
		tIndex = tCorresIndex[i]
		if tabl[tIndex][3] > 180.0:
			SourceRA = mth.radians(tabl[tIndex][3]-360.0)
		else:
			SourceRA = mth.radians(tabl[tIndex][3])
		SourceDec = mth.radians(tabl[tIndex][4])
	else:
		SourceRA = 0.0
		SourceDec = 0.0
	LocImageNum = 0

	#-- Read in the event
	print "Reading event from file " + os.path.basename(fitsFiles[i]) + " ..."
	if FixedResolution:
		ImOrig = hp.read_map(fitsFiles[i], verbose=False)
		NSIDEOrig = hp.get_nside(ImOrig) 
		print "NSIDE=" + str(NSIDEOrig) + ", Number of pixels = " + str(len(ImOrig)) + " -->"

		for irotate in range(RotateQuads):
			print "Rotating data in map by 90 degrees"
		        #-- Rotate the map by 90 degrees!
			iof = 0
			for irow in range(0,4*NSIDEOrig-1):
				qsz = min(irow+1,NSIDEOrig,4*NSIDEOrig-1-irow)
				tarr = 1*ImOrig[iof:iof+qsz]
				ImOrig[iof:iof+qsz] = ImOrig[iof+qsz:iof+2*qsz]
				ImOrig[iof+qsz:iof+2*qsz] = ImOrig[iof+2*qsz:iof+3*qsz]
				ImOrig[iof+2*qsz:iof+3*qsz] =ImOrig[iof+3*qsz:iof+4*qsz]
				ImOrig[iof+3*qsz:iof+4*qsz] = tarr
				iof += 4*qsz

		#-- Downsample (or upsample) map to fixed NSIDE
		Im = hp.ud_grade(ImOrig,FixedNSIDE,power=-2)
		NSIDE = hp.get_nside(Im) 

	else:
		Im = hp.read_map(fitsFiles[i], verbose=False)
		NSIDE = hp.get_nside(Im) 

		for irotate in range(RotateQuads):
			print "Rotating data in map by 90 degrees"
		        #-- Rotate the map by 90 degrees!
			iof = 0
			for irow in range(0,4*NSIDE-1):
				qsz = min(irow+1,NSIDE,4*NSIDE-1-irow)
				tarr = 1*Im[iof:iof+qsz]
				Im[iof:iof+qsz] = Im[iof+qsz:iof+2*qsz]
				Im[iof+qsz:iof+2*qsz] = Im[iof+2*qsz:iof+3*qsz]
				Im[iof+2*qsz:iof+3*qsz] =Im[iof+3*qsz:iof+4*qsz]
				Im[iof+3*qsz:iof+4*qsz] = tarr
				iof += 4*qsz

	print "NSIDE=" + str(NSIDE) + ", Number of pixels = " + str(len(Im))

	#-- Calculate trig quantities that we're going to use a lot
	#-- And also make a list of "significant" pixels
	#-- Note: if not making plots, we only need to store info
        #-- about the significant pixels; this saves some computation
	print "Finding maximum pixel and significant pixels..."
	SigPixels = [] 
	sigprob = 0.0
	halfpi = 0.5*mth.pi
	if MakePlots:
		#-- Make complete lists of RA/DEC values and sin/cos
		Imra = np.zeros_like(Im)
		Imdec = np.zeros_like(Im)
		SINImdec = np.zeros_like(Im)
		COSImdec = np.zeros_like(Im)
		for j in range(len(Im)):
			if Im[j] > SigPixelThresh:
				SigPixels.append(j)
				sigprob += Im[j]
			pointtheta,Imra[j] = hp.pix2ang(NSIDE,j)
			Imdec[j] = halfpi-pointtheta
			SINImdec[j] = mth.sin(Imdec[j])
			COSImdec[j] = mth.cos(Imdec[j])
	else:
		#-- Make sparse lists of RA/DEC values and sin/cos
		Imra = []
		Imdec = []
		SINImdec = []
		COSImdec = []
		for j in range(len(Im)):
			if Im[j] > SigPixelThresh:
				SigPixels.append(j)
				sigprob += Im[j]
				pointtheta,pointphi = hp.pix2ang(NSIDE,j)
				pointdec = halfpi-pointtheta
				Imra.append(pointphi)
				Imdec.append(pointdec)
				SINImdec.append(mth.sin(pointdec))
				COSImdec.append(mth.cos(pointdec))

	if MakePlots:
	        #-- Start plotting this event
		if PlotMapTitle:
			maptitle = "Localization Probability Density, Event ID: " + indexEventID[i]
		else:
			maptitle = ""

		if FixedResolution:
			hp.mollview(ImOrig, title=maptitle, rot=PlotRot, cmap=my_cmap, cbar=False, unit="Probability Density")
		else:
			hp.mollview(Im, title=maptitle, rot=PlotRot, cmap=my_cmap, cbar=False, unit="Probability Density")
		hp.graticule(dpar=30, dmer=30, verbose=False, color=GratColor)
		if SimEvents and PlotSourcePos:
		        #-- Plot source position using white circle
			hp.projplot(mth.degrees(SourceRA), mth.degrees(SourceDec), 'wo', lonlat=True) 	

	#-- Initialize 
	SigPixUsed = np.zeros(len(SigPixels))

	print "Number of significant pixels : " + str(len(SigPixels))
	print "Probability sum (sig pixels) = " + str(sigprob)

	#-- Initialize some things that apply to a set of images
	sumprobEncl = 0.0

	for iimage in range(1,MaxImageCount+1):

		ctrTried = np.zeros(len(Im))

	        #-- Find max. prob. significant pixel that has not yet been used
		maxprob = 0.0
		maxInd = -1
		ipixel = -1
		if BruteForceSeed:
			for j in SigPixels:
				ipixel += 1
				if SigPixUsed[ipixel] > 0: continue
				regInfo = regionFind(j)
				probEncl = regInfo[6]
				if probEncl > maxprob:
					maxInd = j
					maxprob = probEncl
		else:
			for j in SigPixels:
				ipixel += 1
				if SigPixUsed[ipixel] > 0: continue
				if Im[j] > maxprob:
					maxInd = j
					maxprob = Im[j]

		if maxInd >= 0:

			#-- Start out by centering the image on the max pixel
			centerPix = maxInd
			regInfo = regionFind(centerPix)
			ctrTried[centerPix] = 1
			foundBetter = True
			bestProbEncl = regInfo[6]
#			print "  Image " + str(iimage) + " field " + str(centerPix) + "       encloses " + str(bestProbEncl)
			print '  Image {0:2d} field{1:8d} encloses {2:8.6f}'.format(iimage,centerPix,bestProbEncl)

			while foundBetter:
				# Consider all the neighbors to that pixel
				print "    Find neighbors of " + str(centerPix)
				neighbors = hp.get_all_neighbours(NSIDE,centerPix)
				foundBetter = False
				for k in neighbors:
					if k < 0: continue
					if ctrTried[k]: continue
					tempInfo = regionFind(k)
					ctrTried[k] = iimage
					tempProbEncl = tempInfo[6]
#					print "    field " + str(k) + " encloses " + str(tempProbEncl)
#					print '    field{0:8d} encl {1:8.6f}'.format(k,tempProbEncl)
					if tempProbEncl > bestProbEncl:
#						print "           ** Better!   Now encloses " + str(tempProbEncl)
						print '      ** Better:{0:9d} encloses {1:8.6f}'.format(k,tempProbEncl)
						foundBetter = True
						centerPix = k
						regInfo = tempInfo
						bestProbEncl = tempProbEncl

	                #-- OK, we have found a local maximum of enclosed prob.
	                #-- Extract some information about the region we found
			ra = regInfo[0]
			dec = regInfo[1]
			maxRot = regInfo[2]
			probEncl = regInfo[6]

			if MakePlots:
				#-- Over-plot the FOV outline
				ctrVec = hp.pix2vec(NSIDE,centerPix)
				latBaseTheta1 = mth.radians(FOVsize1/2.0-dec)
				latBaseTheta2 = mth.radians(FOVsize2/2.0-dec)
				if latBaseTheta1 >= 0:
					latBasePhi1 = mth.radians(ra)
				else:
					latBaseTheta1 = -latBaseTheta1
					latBasePhi1 = mth.radians((ra+180.)%360.)
				if latBaseTheta2 >= 0:
					latBasePhi2 = mth.radians(ra)
				else:
					latBaseTheta2 = -latBaseTheta2
					latBasePhi2 = mth.radians((ra+180.)%360.)
				baseVec1 = hp.ang2vec(latBaseTheta1,latBasePhi1)
				baseVec2 = hp.ang2vec(latBaseTheta2,latBasePhi2)
				rotUnit1 = mth.radians(FOVsize1/float(FOVplotSteps))
				rotUnit2 = mth.radians(FOVsize2/float(FOVplotSteps))
				#-- Now adapt some of the code from axisFind...
				#-- Get the four "axis points", but just keep
				#-- them as cartesian vectors instead of
				#-- (lat,long) pairs.  (And in different order)
				psi0 = mth.radians(maxRot)
				pi = mth.pi
				vec1 = rotatePoint(psi0,ctrVec,baseVec1)
				vec2 = rotatePoint(psi0+0.5*pi,ctrVec,baseVec2)
				vec3 = rotatePoint(psi0+pi,ctrVec,baseVec1)
				vec4 = rotatePoint(psi0+1.5*pi,ctrVec,baseVec2)
				#-- We need one more point to rotate around,
				#-- to get from the center to the midde of one
				#-- edge.  Find it by doing a cross product.
				sVec = np.cross(ctrVec,vec1)

				#-- OK, now we can rotate the center point to
				#-- the midpoint of an edge (a short edge)
				halfmajor = mth.radians(0.5*FOVsize1)
				pVec = rotatePoint(halfmajor,sVec,ctrVec)
				#-- Step around the rectangle by doing small
				#-- rotations about the various axes, filling
				#-- in arrays of theta and phi values as we go. 
				btheta = np.zeros(4*FOVplotSteps+2)
				bphi = np.zeros(4*FOVplotSteps+2)
				btheta[0],bphi[0] = hp.vec2ang(pVec)
#				hp.projtext(btheta[0],bphi[1],str(iimage))
				fromstep = 1
				tostep = 1 + (FOVplotSteps/2)
				for ip in range(fromstep,tostep):
					pVec = rotatePoint(rotUnit2,vec3,pVec)
					btheta[ip],bphi[ip] = hp.vec2ang(pVec)
				fromstep = tostep
				tostep = tostep + FOVplotSteps
				for ip in range(fromstep,tostep):
					pVec = rotatePoint(rotUnit1,vec4,pVec)
					btheta[ip],bphi[ip] = hp.vec2ang(pVec)
				fromstep = tostep
				tostep = tostep + FOVplotSteps
				for ip in range(fromstep,tostep):
					pVec = rotatePoint(rotUnit2,vec1,pVec)
					btheta[ip],bphi[ip] = hp.vec2ang(pVec)
				fromstep = tostep
				tostep = tostep + FOVplotSteps
				for ip in range(fromstep,tostep):
					pVec = rotatePoint(rotUnit1,vec2,pVec)
					btheta[ip],bphi[ip] = hp.vec2ang(pVec)
				fromstep = tostep
				tostep = tostep + (FOVplotSteps/2)
				for ip in range(fromstep,tostep):
					pVec = rotatePoint(rotUnit2,vec3,pVec)
					btheta[ip],bphi[ip] = hp.vec2ang(pVec)
				#-- Force the outline to close
				btheta[4*FOVplotSteps+1] = btheta[0]
				bphi[4*FOVplotSteps+1] = bphi[0]
				#-- Plot the outline!
				hp.projplot(btheta,bphi,FOVColor,linewidth=2)

	                #-- Mark all the pixels in this region as used
			regionMark(centerPix,maxRot,iimage)

	                #-- Check whether source is contained in this region
			if SimEvents:
				locFlag = sourceCheck(SourceRA,SourceDec,regInfo)
				if locFlag:
					if LocImageNum == 0:
						locCount += 1
						LocImageNum = iimage
					print "	EVENT LOCALIZED in image " + str(iimage)
			else:
				locFlag = False


		else:
			#-- There was no usable significant pixel
			ra = 0.0
			dec = 0.0
			maxRot = 0.0
			probEncl = 0.0

		sumprobEncl += probEncl


	        #-- Write information to output file
		f.write( '{0:>7s} {1:2d} {2:2d} {3:5s} {4:9.3f} {5:9.3f} {6:9.3f} {7:9.3f} {8:6.2f} {9:9.6f} {10:9.6f}\n'.format(indexEventID[i],iimage,LocImageNum,str(locFlag),mth.degrees(SourceRA),mth.degrees(SourceDec),ra,dec,maxRot,probEncl,sumprobEncl) )
		f.flush()
	#-- End loop over images

	if MakePlots:
       	        #Save Figure to file
		fname = DataSet + "/RotFig/" + indexEventID[i] + ".png"
		plt.savefig(fname, bbox_inches='tight', transparent=True, dpi=300 )
		plt.close()
	
	#(Uncomment to pause after every map. Debugger.)
	#raw_input("Press ENTER to continue")
	
f.close()

if SimEvents:
	# Display Summary
	print str(locCount) + " sufficiently localized events of " + str(len(EventSet)) + " total events (" + str(locCount/float(len(EventSet))*100.0) + "%)"

end = time.time()
print "\nRUNTIME: " + str(end-start)
