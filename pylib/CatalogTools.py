import fileinput
import numpy
from astropy.table import Table


def parseCatalogFile_Nicola(filename):

	GCNNAME = numpy.array([])
	RA = numpy.array([])
	DEC = numpy.array([])
	ERR = numpy.array([])
	SEP2CLOSEST = numpy.array([])
	THETA = numpy.array([])
	ZENITH = numpy.array([])
	LLE = numpy.array([])
	IRFS = numpy.array([])
	LIKE_SU = numpy.array([])
	TSTART_SU = numpy.array([])
	TSTOP_SU = numpy.array([])
	TS_SU = numpy.array([])
	Flux_SU = numpy.array([])
	Flux_SU_err = numpy.array([])
	Index_SU = numpy.array([])
	LIKE_BEST = numpy.array([])
	Index_SU_err = numpy.array([])
	TSTART_BEST = numpy.array([])
	TSTOP_BEST = numpy.array([])
	TS_BEST = numpy.array([])
	Flux_BEST = numpy.array([])
	Flux_BEST_err = numpy.array([])
	Index_BEST = numpy.array([])
	Index_BEST_err = numpy.array([])
	LIKE_AG = numpy.array([])
	TSTART_AG = numpy.array([])
	TSTOP_AG = numpy.array([])
	TS_AG = numpy.array([])
	Flux_AG = numpy.array([])
	Flux_AG_err = numpy.array([])
	Index_AG = numpy.array([])
	Index_AG_err = numpy.array([])

	# Read the file line by line
	for line in fileinput.input([filename]):

		# Skip lines with comment symbols
		if '#' in line:
			continue 

		# Split the line
		lineContents = line.split()

		# Extract the values
		GCNNAME = numpy.append(GCNNAME, lineContents[0])
		RA = numpy.append(RA, float(lineContents[1]))
		DEC = numpy.append(DEC, float(lineContents[2]))
		ERR = numpy.append(ERR, float(lineContents[3]))
		SEP2CLOSEST = numpy.append(SEP2CLOSEST, float(lineContents[4]))
		THETA = numpy.append(THETA, float(lineContents[5]))
		ZENITH = numpy.append(ZENITH, float(lineContents[6]))
		LLE = numpy.append(LLE, float(lineContents[7]))
		IRFS = numpy.append(IRFS, lineContents[8])
		TSTART_SU = numpy.append(TSTART_SU, float(lineContents[9]))
		TSTOP_SU = numpy.append(TSTOP_SU, float(lineContents[10]))
#		LIKE_SU = numpy.append(LIKE_SU, float(lineContents[11]))
		TS_SU = numpy.append(TS_SU, float(lineContents[11]))
		Flux_SU = numpy.append(Flux_SU, float(lineContents[12]))
		Flux_SU_err = numpy.append(Flux_SU_err, float(lineContents[13]))
		Index_SU = numpy.append(Index_SU, float(lineContents[14]))
		Index_SU_err = numpy.append(Index_SU_err, float(lineContents[15]))
#		LIKE_BEST = numpy.append(LIKE_BEST, float(lineContents[15]))
		TSTART_BEST = numpy.append(TSTART_BEST, float(lineContents[16]))
		TSTOP_BEST = numpy.append(TSTOP_BEST, float(lineContents[17]))
		TS_BEST = numpy.append(TS_BEST, float(lineContents[18]))
		Flux_BEST = numpy.append(Flux_BEST, float(lineContents[19]))
		Flux_BEST_err = numpy.append(Flux_BEST_err, float(lineContents[20]))
		Index_BEST = numpy.append(Index_BEST, float(lineContents[21]))
		Index_BEST_err = numpy.append(Index_BEST_err, float(lineContents[22]))
#		LIKE_AG = numpy.append(LIKE_AG, float(lineContents[21]))
		TSTART_AG = numpy.append(TSTART_AG, float(lineContents[23]))
		TSTOP_AG = numpy.append(TSTOP_AG, float(lineContents[24]))
		TS_AG = numpy.append(TS_AG, float(lineContents[25]))
		Flux_AG = numpy.append(Flux_AG, float(lineContents[26]))
		Flux_AG_err = numpy.append(Flux_AG_err, float(lineContents[27]))
		Index_AG = numpy.append(Index_AG, float(lineContents[28]))
		Index_AG_err = numpy.append(Index_AG_err, float(lineContents[29]))


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


	return results,rtable