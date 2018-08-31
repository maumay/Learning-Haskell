#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 23:26:17 2017

@author: t
"""

import  declassedcornerdetection as cd
import regionpreprocessing as rp
from geomutils import Point
import numpy as np
from PIL import Image
import cv2, os, itertools
import keras


# Use the h5py package for saving/loading keras models to/from the disk
model_path = os.path.join('..',
                          'digitmodels',
                          'digitrecognisermodel34',
                          'digitrecognisermodel34.h5')
model = keras.models.load_model(model_path)

#cfinder = CornerFinder()
outpiclen = 32
bordershft = Point(1, 1)


def get_grid(file_path):
    regions = get_all_grid_squares(file_path)
    regions = [rp.process_region(region) for region in regions]
    grid = ''
    for i, region in enumerate(regions):
        pred = predict(region)
        grid += str(pred) + ('' if (i%9 != 8) else '\n')
    return grid


def get_all_grid_squares(file_path):
    '''
    '''
    if not os.path.exists(file_path):
        raise FileNotFoundError()
    img = cv2.cvtColor(
            cd.process_image(file_path), cv2.COLOR_BGR2GRAY)
    h, w = img.shape
    sq_div = h // 9
    assert sq_div == w/9, 'Not got a square image of side len div by nine'
    assert sq_div >= outpiclen
    regions = []
    for yshft, xshft in itertools.product(range(9), repeat=2):
        tl = bordershft + Point(xshft*sq_div, yshft*sq_div)
        rslices = get_gridsquare_region(tl, sq_div)
        regions.append(img[rslices[0]:rslices[1], rslices[2]:rslices[3]])
    return regions


def get_gridsquare_region(top_left_corner, square_division_len):
    tlc, sdl = top_left_corner, square_division_len
    len_diff = sdl - outpiclen
    shft = int(len_diff/2)
    return (tlc.y + shft, tlc.y + sdl - shft, tlc.x + shft, tlc.x + sdl - shft)


def predict(im):
    ''' im should be numpy array of shape (32, 32)
    '''
    model_inp = np.expand_dims(np.expand_dims(im, axis=3), axis=0)
    pred = model.predict(model_inp)
    return (np.argmax(pred.ravel()) + 1)%10


file_path = os.path.join('..', 'TrainingSetGeneration', 'generatedpics', 'test', 't6.png')#'IMAG0466.jpg')#
grid = get_grid(file_path)
print(grid)
x = get_all_grid_squares(file_path)
for idx, region in enumerate(x):
    im = Image.fromarray(region)
    outpath = os.path.join('..', 'TrainingSetGeneration', 'generatedpics', 'regionoutputs', str(idx) + '.png')
    im.save(outpath, 'PNG')


#fpath = r'C:\Users\thomasb\git\Snapsolve\TrainingSetGeneration\generatedpics\regiondebugs\3.png'
fpath = os.path.join('..', 'DigitTrainingData', 'six', '75.png')
im = cv2.cvtColor(cv2.imread(fpath), cv2.COLOR_BGR2GRAY)
print(predict(im))
# =============================================================================
# cv2.imshow('a', im)
# cv2.waitKey(0)
# cv2.destroyAllWindows()
# =============================================================================
