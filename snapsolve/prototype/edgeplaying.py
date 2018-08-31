# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 15:39:39 2017

@author: ThomasB
"""

import cv2, os
import numpy as np
from matplotlib import pyplot as plt
from geomutils import Point


def total_dist(arr):
    unrav = arr.ravel()
    points = [Point(unrav[i], unrav[i+1]) for i in range(0, len(unrav) - 1, 2)]
    plen = len(points)
    return sum(points[i%plen].dist(points[(i+1)%plen]) for i in range(plen))


img = cv2.imread(os.path.join('..', 'DigitTrainingData', '1', '10.png'))
edges = cv2.Canny(img,100,200)
ret, thresh = cv2.threshold(edges, 127, 255, 0)
im2, contours, hierarchy = cv2.findContours(thresh, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

# =============================================================================
# print(contours[0])
# print(contours[0].ravel())
# =============================================================================
argmax = max(range(len(contours)), key=lambda i: total_dist(contours[i]))
print(argmax)

cnt = contours[argmax]
cv2.drawContours(img, [cnt], 0, (0,255,0), 3)

cv2.imwrite(os.path.join('F:', 'snapsolve', 'pics', 'writingout.PNG'), img)

plt.imshow(edges)#,cmap = 'gray')
plt.title('Edge Image'), plt.xticks([]), plt.yticks([])

plt.show()