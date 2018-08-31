# -*- coding: utf-8 -*-
"""
Created on Fri Oct 27 12:12:45 2017

@author: ThomasB

Less confusing now I stopped using classes
"""
import os, cv2, numpy as np
from geomutils import Line, Point

#------------------------------------------------------------------------------
''' Global script variables
'''
debug_outpath = os.path.join('..', 'TrainingSetGeneration',
                             'generatedpics', 'debugoutputs')

# Must be square
desired_imsize = (500, 500)
output_imsize = (360, 360)

harris_tol = 0.01
harris_config = {'blockSize': 4, 'ksize': 7, 'k': 0.1}
cornerfinders = Line.get_corner_finders(desired_imsize[0])

init_thresh_config = {'thresh': 130, 'maxval': 255, 'type': cv2.THRESH_BINARY}
canny_config = {'threshold1': 100, 'threshold2': 200, 'L2gradient': True}
sec_thresh_config = {'thresh': 20, 'maxval': 255, 'type': cv2.THRESH_BINARY}
contour_config = {'mode': cv2.RETR_TREE, 'method': cv2.CHAIN_APPROX_SIMPLE}
#------------------------------------------------------------------------------


def process_image(img_path):
    ''' Does stuff
    '''
    img, cpy_img = cv2.imread(img_path), cv2.imread(img_path)
    if img is None:
        raise FileNotFoundError()
    if not img.shape == desired_imsize:
        img = reshape_im(img)
    bw_img = cv2.cvtColor(cpy_img, cv2.COLOR_BGR2GRAY)
    writeim(bw_img, debug_outpath, 'greyified') # Debug
    grid_contour_img = reshape_im(get_grid_contour(bw_img))
    writeim(grid_contour_img, debug_outpath, 'grid contour') # Debug
    ch_result = cv2.cornerHarris(grid_contour_img, **harris_config)
    set_px = (ch_result > harris_tol*ch_result.max()).ravel()
    spx_coords = [get_px_coord(i) for i, is_set in enumerate(set_px) if is_set]
    corners = get_corners(spx_coords)
    nglen = max(corners[i % 4].dist(corners[(i+1) % 4]) for i in range(4))
    new_corners = np.array([
                [0, 0], [nglen, 0], [nglen, nglen], [0, nglen]
                ], dtype = "float32")
    np_corners = np.array([[p.x, p.y] for p in corners], dtype = "float32")
    warper = cv2.getPerspectiveTransform(np_corners, new_corners)
    im = cv2.warpPerspective(img, warper, (int(nglen), int(nglen)))
    writeim(im, debug_outpath, 'final') # Debug
    return cv2.resize(im, output_imsize, 0, 0, cv2.INTER_AREA)


def get_grid_contour(bw_img):
    ''' Could try initially using adaptive thresholding at some point to
    deal with shadows but this seems to work ok for the time being.
    '''
    nrows, ncols = bw_img.shape
    aspect = nrows / ncols
    dlen = desired_imsize[0]
    dsize = (dlen, int(dlen*aspect)) if nrows > ncols else (int(dlen/aspect), dlen)
    print(dsize)
    bw_img = cv2.resize(bw_img, dsize, 0, 0, cv2.INTER_AREA)
    _, init_thresh = cv2.threshold(bw_img, **init_thresh_config)
    writeim(init_thresh, debug_outpath, 'initial thresh') # Debug
    ''' So I think discard the canny algorithm, its a bit poor,
    slight issue if the background ends up as white because the
    contour finding algorithm sees the whole image as a contour.

    Since we should only have px values of 0 and 255 we could
    look for the most numerous of the two from which we can
    decide whether to invert or not.
    '''
    raveled = init_thresh.ravel()
    assert all(a or b for a, b in zip(raveled == 0, raveled == 255))
    invert = np.sum(raveled == 0) < np.sum(raveled == 255)
    edges = cv2.bitwise_not(init_thresh) if invert else init_thresh#cv2.Canny(init_thresh, **canny_config)#
    writeim(edges, debug_outpath, 'Edges') # Debug
    _, thresh = cv2.threshold(edges, **sec_thresh_config)
    writeim(thresh, debug_outpath, 'Second thresh') # Debug
    _, contours, _ = cv2.findContours(thresh, **contour_config)
    contour_area = lambda x: area_metric(x)
    contours.sort(key=contour_area)
    grid_contour = contours[-1]
    blank = np.zeros(thresh.shape, np.uint8)
    return cv2.drawContours(blank, [grid_contour], 0, 255, 3)


def get_corners(px_coords):
    '''
    '''
    assert len(px_coords) > 3
    corner_idx = []
    for line in cornerfinders:
        search_key = lambda i: line.distance_from(px_coords[i])
        corner_idx.append(min(range(len(px_coords)), key=search_key))
    return tuple(px_coords[idx] for idx in corner_idx)


def get_px_coord(flattened_index):
    ''' Returns the (x, y) coordinate of a pixel given its raveled index.

    Careful, the column num becomes xcoord, row num becomes ycoord
    '''
    # Assume picture was already resized
    n_rows, n_cols = desired_imsize
    return Point(flattened_index % n_cols, flattened_index // n_rows)


def reshape_im(im):
    ''' Resize passed image to the desired_imsize global variable
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
    if current_shape == desired_imsize:
        return im
    make_smaller = current_shape[0] > desired_imsize[0]
    # Recommended interpolation settings in api documentation
    interpolation = cv2.INTER_AREA if make_smaller else cv2.INTER_CUBIC
    return cv2.resize(im, desired_imsize, 0, 0, interpolation)


def area_metric(np_arr):
    ''' This is the metric we use for determining which contour is
    surrounding our grid, i.e. the metric is designed so that it is
    maximised for large squares.
    '''
    np_points = np.squeeze(np_arr, axis=1)
    assert len(np_points.shape) == 2, len(np_points.shape)
    x, y = np_points[:, 0], np_points[:, 1]
    return max(x) - min(x) + max(y) - min(y)


def writeim(im, dirpath, fname):
    ''' Convenience method for writing images.
    '''
    cv2.imwrite(os.path.join(dirpath, fname + '.png'), im)