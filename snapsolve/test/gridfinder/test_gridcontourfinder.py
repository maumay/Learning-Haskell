import unittest, cv2, numpy as np

from collections import namedtuple




def construct_testdata():
	''' Constructs a list of testdata for the below
		test class. Each element of the list is a 
		tuple with attributes image (np array), 
		expected_corners (tuple of four points), 
		description.
	'''
	Corners = namedtuple('Corners',
						 'upper_left upper_right lower_left lower_right')
	TestInput = namedtuple('TestInput', 
						   'image expected_corners description')



class CornerDetectionTest(unittest.TestCase):

