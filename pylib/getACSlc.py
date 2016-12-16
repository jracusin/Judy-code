#!/bin/env python

from urllib2 import urlopen
from urllib import urlencode
import sys
import argparse

parser                        = argparse.ArgumentParser(description="Download INTEGRAL/ACS light curve")

parser.add_argument("-t","--utc",help="Start time in UTC",type=str,required=True)
parser.add_argument("-d","--delta",help="Time before and after the UTC",
                    type=float,required=True)
parser.add_argument("-o","--outfile",help="Outfile",
                    type=str,required=True)

parser.add_argument("-p","--plot", help="Plot light curve?",type=str,required=False,
                    choices=['yes','no'],default='yes')


if __name__=='__main__':
  args = parser.parse_args()
  
  requestStr = "%s %s" %(args.utc,args.delta)
  
  resp = urlopen("http://isdc.unige.ch/~savchenk/spiacs-online/spiacs.pl", 
                 urlencode({"requeststring": requestStr,'submit':"Submit", 'generate':'ipnlc'})).read()
  
  lines = resp.replace("<br>","").split("\n")
  
  try:
    binsize = float(lines[1].split()[1])
  except:
    print resp
    raise RuntimeError("Could not interpret data (dumped above)")
  
  with open(args.outfile,"w+") as f:
    for line in lines[2:]:
      f.write("%s\n" %(line.replace("\n","")))
  
  if(args.plot=='yes'):
    import matplotlib.pyplot as plt
    import numpy
    
    data = numpy.genfromtxt(args.outfile)
    
    plt.plot(data[:,0],data[:,1],'.')
    plt.xlabel("Time since %s" %(args.utc))
    plt.ylabel("Counts in %.4f s" %(binsize))
    plt.show()
    raw_input("Press ENTER when you are done")
