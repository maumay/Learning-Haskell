#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 23:26:17 2017

@author: t
"""

from cornerdetection import CornerFinder
from geomutils import Point
import cv2, os


cfinder = CornerFinder()
indiv_picsize = (32, 32)


def get_all_grid_squares(file_path):
    '''
    '''
    if not os.path.exists(file_path):
        raise FileNotFoundError()
    cleaned_img = cfinder.process_image(file_path)
    h, w, n_ch = cleaned_img.shape
    pass
