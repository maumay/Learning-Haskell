import cv2

# The folder we will log the intermediate pictures to.
DEBUG_OUTPATH = 'debug-picture-log'

# Image size config for the overall algorithm
INPUT_IMSIZE = (500, 500)
OUTPUT_IMSIZE = (360, 360)


HARRIS_TOL = 0.01
HARRIS = {'blockSize': 4, 'ksize': 7, 'k': 0.1}

init_thresh_config = {'thresh': 130, 'maxval': 255, 'type': cv2.THRESH_BINARY}
sec_thresh_config = {'thresh': 20, 'maxval': 255, 'type': cv2.THRESH_BINARY}
CONTOUR = {'mode': cv2.RETR_TREE, 'method': cv2.CHAIN_APPROX_SIMPLE}