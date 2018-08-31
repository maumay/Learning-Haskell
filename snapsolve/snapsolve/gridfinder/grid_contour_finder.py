import cv2, itertools as it

from collections import Counter
from . import opencv_config as config

## TODO - need to write a test, could have sample image with black back
## and a few white squares. 

Corners = namedtuple('Corners',
                     'upper_left upper_right lower_left lower_right')


def find_corners_of_largest_rectangle(thresholded_image):
    '''
    '''
    im, raveled_im = thresholded_image, thresholded_image.ravel()
    if not all(px == 0 or px == 255 for px in raveled_im):
        raise ValueError('Image has not been thresholded correctly')
    color_counts = Counter(raveled_im)
    if color_counts[255] >= color_counts[0]:
        raise ValueError('Image does not have a black background')
    largest_rect_contour = find_contour_of_greatest_area(im)
    corner_harris = cv2.cornerHarris(largest_rect_contour, **config.HARRIS)
    corner_pixels = corner_harris > config.HARRIS_TOL*max(corner_harris)
    return find_set_pixel_coords_closest_to_corners(corner_pixels)


def find_set_pixel_coords_closest_to_corners(rect_bool_array):
    '''
    '''
    #for i, j in it.product(range())
    raise AssertionError('not yet implemented')


def find_contour_of_greatest_area(image):
    ''' We expect a thresholded black and white image,
        with a black background. i.e. a numpy 
        array with shape (px_height, px_width, 1).

        We return an image containing the contour of 
        largest area.
    '''
    _, contours, _ = cv2.findContours(image, **config.CONTOUR)
    contours.sort(key=contour_area_metric)
    blank_pic = np.zeros(image.shape, np.uint8)
    return cv2.drawContours(blank_pic, contours[-1:], 0, 255, 3)


def contour_area_metric(contour):
    assert len(contour.shape) == 2 : contour.shape
    x, y = contour[:, 0], contour[:, 1]
    return (max(x) - min(x))*(max(y) - min(y))