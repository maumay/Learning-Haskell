#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 22:14:19 2017

@author: t
"""

import numpy as np
import cv2

#cv2.copyMakeBorder(src, top, bottom, left, right, borderType[, dst[, value]]) → dst
file_path = os.path.join('/home','t','git','Snapsolve','TrainingSetGeneration','generatedpics','opencvtest.png')
img = cv2.imread(file_path)
img = cv2.copyMakeBorder(img, 300, )


out_path = '/home/t/git/Snapsolve/TrainingSetGeneration/generatedpics/out1.png'
cv2.imwrite(out_path, img)
