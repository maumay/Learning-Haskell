# -*- coding: utf-8 -*-
"""
Created on Fri Oct 27 13:54:53 2017

@author: ThomasB
"""
import os, cv2


#------------------------------------------------------------------------------
''' Global script variables
'''
region_indir = os.path.join('..', 'TrainingSetGeneration',
                             'generatedpics', 'regionoutputs')

debug_outdir = os.path.join('..', 'TrainingSetGeneration',
                             'generatedpics', 'regiondebugs')

thresh_config = {'thresh': 135, 'maxval': 255, 'type': cv2.THRESH_BINARY}
#------------------------------------------------------------------------------


def process_region(im):
    '''
    '''
    _, thresh = cv2.threshold(im, **thresh_config)
    return thresh


def writeim(im, dirpath, fname):
    ''' Convenience method for writing images.
    '''
    cv2.imwrite(os.path.join(dirpath, fname), im)


for fname in os.listdir(region_indir):
    im = cv2.imread(os.path.join(region_indir, fname))
    bw_im = cv2.cvtColor(im, cv2.COLOR_BGR2GRAY)
    assert len(bw_im.shape) == 2, bw_im.shape
    writeim(process_region(bw_im), debug_outdir, fname)