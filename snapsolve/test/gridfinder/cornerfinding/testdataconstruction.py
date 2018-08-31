import numpy as np

from collections import namedtuple


TestInput = namedtuple('TestInput', 'image expected_corners description')



def construct_testdata():
	''' Constructs a list of testdata for the below
		test class. Each element of the list is a 
		tuple with attributes image (np array), 
		expected_corners (tuple of four points), 
		description.
	'''
	data = [] 
	data += construct_case1_data()
	data += construct_case2_data()
	data += construct_case3_data()


def construct_case1_data():
	'''
	'''
	pass


def construct_case2_data():
	'''
	'''
	pass


def construct_case3_data():
	'''
	'''
	pass