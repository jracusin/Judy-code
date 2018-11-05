#!/usr/bin/env python

import numpy as np


def calcNorms(real_counts, training_counts):

    """Calculates the norm between the vectors in two numpy arrays.

    Parameters
    ----------
    real_counts : the array of real grb counts in each detector

    training_counts : the arrary of traning counts in each detector

    Returns
    -------

    null : array
         numpy array of the norm between all of the vectors

    """
    
    return np.array([[np.linalg.norm(rec[0] - test[0], 2)
                      for rec in training_counts]
                     for test in real_counts])

 
def addErrors(real_counts, training_counts):

    """Adds all of the errors together in quadrature

    Parameters
    ----------
    real_counts : the array of real grb counts in each detector

    training_counts : the arrary of traning counts in each detector

    Returns
    -------

    null : array
         numpy array of the errors

    """
    
    errors = np.sqrt([[test + rec
                       for rec in training_counts[:, 1]**2]
                      for test in real_counts[:, 1]**2])

    return np.round(errors).astype('int32')


def calcNormsWithError(real_counts, training_counts, real_counts_with_error):

    norms_errp = np.array([[np.linalg.norm(rec[0]
                                           - (test[0]
                                              + real_counts_with_error[idx, jdx]), 2)
                            for jdx, rec in enumerate(training_counts)]
                           for idx, test in enumerate(real_counts)])

    norms_errm = np.array([[np.linalg.norm(rec[0]
                                           - (test[0]
                                              - real_counts_with_error[idx, jdx]), 2)
                            for jdx, rec in enumerate(training_counts)]
                           for idx, test in enumerate(real_counts)])

    return norms_errp, norms_errm
