# -*- coding: utf-8 -*-
"""
Spyder Editor

Think getting cv2 installed on ubuntu might be a bit of a ballache.
Also needs the correct c++
"""
import cv2
import numpy as np
import math as m
from geomutils import Line, Point
import os


debug_outpath = os.path.join('..', 'TrainingSetGeneration', 'generatedpics', 'debugoutputs')


class CornerFinder:
    ''' Uses corner harris algorithm provided in the openCV module to detect
    the sudoku grid from the passed picture.
    '''
    _exp_imshape = (500, 500, 3)
    _harris_tol = 0.01
    _straight_tol = 1
    output_imsize = (360, 360)

    def __init__(self, blocksize=4, ksize=7, k=0.1):
        ''' Initialise with the parameters of the corner Harris algorithm
        we will use. These default values are subject to change
        '''
        self._blocksize = blocksize
        self._ksize = ksize
        self._k = k
        self._img = None
        # Now initialise the corner finding lines
        psl = self._exp_imshape[0] # half picture side length
        pmx, pmy = psl/2, psl/2 # Picture mid x and y
        # Diamond corners from left moving clockwise
        corners = [Point(pmx - psl, pmy), Point(pmx, pmy - psl),
                   Point(pmx + psl, pmy), Point(pmx, pmy + psl)]
        # Diagonal lines forming circumscribed diamond shape
        self._cornerfinders = ([Line(corners[i%4], corners[(i+1)%4]) for
                                i in range(4)])
        return


    def process_image(self, img_filepath):
        ''' Outputs a clean cv image of the grid, that is without rotation,
        unwarped and without a border
        '''
        self._img, cpy = cv2.imread(img_filepath), cv2.imread(img_filepath)
        if self._img is None:
            print('!!!' + img_filepath)
            raise FileNotFoundError()
        if not self._img.shape == self._exp_imshape:
            self._img = self._reshape_im(self._img)
        grey_im = cv2.cvtColor(cpy, cv2.COLOR_BGR2GRAY)
# =============================================================================
        # Debug
        writeim(grey_im, debug_outpath, 'greyified')
# =============================================================================
        grid_contr_img = self._reshape_im(self._get_grid_contour(grey_im))
# =============================================================================
        # Debug
        writeim(grid_contr_img, debug_outpath, 'contours')
# =============================================================================
        ch_result = cv2.cornerHarris(grid_contr_img, self._blocksize,
                                     self._ksize, self._k)
        # Get boolean array of whether px set then flatten this array of bools
        set_px = (ch_result > self._harris_tol*ch_result.max()).ravel()
        # get coordinates of the set pixels
        set_px_coords = ([self._get_px_coord(px_idx) for
                          px_idx, is_set in enumerate(set_px) if is_set])
        corners = self._get_corners(set_px_coords)
        # Finally warp the perspective using cv2 function
        # new grid length
        nglen = max(corners[i%4].dist(corners[(i+1)%4]) for i in range(4))
        new_corners = np.array([
                [0, 0], [nglen, 0], [nglen, nglen], [0, nglen]
                ], dtype = "float32")
        np_corners = np.array([[p.x, p.y] for p in corners], dtype = "float32")
        warper = cv2.getPerspectiveTransform(np_corners, new_corners)
        im = cv2.warpPerspective(self._img, warper, (int(nglen), int(nglen)))
# =============================================================================
        # Debug
        writeim(im, debug_outpath, 'final')
# =============================================================================
        return cv2.resize(im, self.output_imsize, 0, 0, cv2.INTER_AREA)


    def _get_grid_contour(self, grey_img):
        inret, init_thresh = cv2.threshold(grey_img, 150, 255, cv2.THRESH_BINARY_INV)
        aspect = init_thresh.shape[0]/init_thresh.shape[1]
        inverted = cv2.bitwise_not(init_thresh)
        r, c = inverted.shape
        dsize = (500, 500*aspect) if r > c else (int(500/aspect), 500)
        inverted = cv2.resize(inverted, dsize, 0, 0, cv2.INTER_AREA)
        # =============================================================================
        # Debug
        writeim(inverted, debug_outpath, 'initthreshinvert')
# =============================================================================
        edges = cv2.Canny(inverted, 100, 200, L2gradient=True)
# =============================================================================
        # Debug
        writeim(edges, debug_outpath, 'edges')
# =============================================================================
        ret, thresh = cv2.threshold(edges, 100, 255, 0)
        im2, contours, hierarchy = cv2.findContours(
                thresh, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        contour_total_length = lambda x: total_dist(x)
        contours.sort(key=contour_total_length)
        contours = contours[::-1]
        print(contours[0].shape, contours[1].shape)
        #c = sorted(range(len(contours)), key=contour_total_length)
# =============================================================================
        # Debug
        writeim(cv2.drawContours(np.zeros(thresh.shape, np.uint8), contours, -1, (255), 1), debug_outpath, 'allcontours')
# =============================================================================
        grid_contour = contours[0]
        blank = np.zeros(edges.shape, np.uint8)
        cv2.drawContours(blank, [grid_contour], 0, 255, 2)
        return blank


    def _get_req_rotation(self, ll_corner, ul_corner):
        '''
        '''
        p1, p2 = ll_corner, ul_corner
        if abs(p1.x - p2.x) > self._straight_tol:
            rad_angle = m.atan(abs((p2.x - p1.x)/(p2.y - p1.y)))
            return -rad_angle, -180*rad_angle/m.pi
        else:
            return 0, 0


    def _get_corners(self, px_coords):
        '''
        '''
        assert len(px_coords) > 3
        corner_idx = []
        for line in self._cornerfinders:
            search_key = lambda i: line.distance_from(px_coords[i])
            corner_idx.append(min(range(len(px_coords)), key=search_key))
        return tuple(px_coords[idx] for idx in corner_idx)


    def _get_px_coord(self, raveled_idx):
        ''' Careful here, the column becomes x coord, row becomes y coord
        '''
        n_rows, n_cols,  _ = self._exp_imshape
        return Point(raveled_idx % n_cols, raveled_idx // n_rows)


    def _reshape_im(self, im):
        '''
        '''
        current_shape = im.shape
        if not current_shape[0] == current_shape[1]:
            long_side = max(current_shape)
            shift = [(long_side - x) for x in current_shape]
            # Make square by adding white border
            im = cv2.copyMakeBorder(im, shift[0], 0, shift[1], 0,
                                           cv2.BORDER_CONSTANT, 255)
            current_shape = im.shape
        assert current_shape[0] == current_shape[1], 'Border not added right: '
        exp_shape = self._exp_imshape
        if current_shape == exp_shape:
            return im
        make_smaller = current_shape[0] > exp_shape[0]
        # Recommended interpolation settings in api documentation
        interpolation = cv2.INTER_AREA if make_smaller else cv2.INTER_CUBIC
        dsize = (exp_shape[0], exp_shape[1])
        return cv2.resize(im, dsize, 0, 0, interpolation)


def total_dist(arr):
    np_points = np.squeeze(arr, axis=1)
    assert len(np_points.shape) == 2, len(np_points.shape)
    x, y = np_points[:, 0], np_points[:, 1]
    return max(x) - min(x) + max(y) - min(y)


def writeim(im, dirpath, fname):
    cv2.imwrite(os.path.join(dirpath, fname + '.png'), im)


# =============================================================================
# dir_path = os.path.join('..', 'TrainingSetGeneration', 'generatedpics', 'test')
# out_path = os.path.join('..', 'TrainingSetGeneration', 'generatedpics', 'testout')
#
# for file in os.listdir(dir_path):
#     img = CornerFinder().process_image(file)
#     cv2.imwrite(os.path.join(out_path, file), img)
#
#
# plt.imshow(img),plt.show()
# =============================================================================
