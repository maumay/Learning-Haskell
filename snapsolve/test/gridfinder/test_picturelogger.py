import unittest, os, shutil
import numpy as np


from collections import namedtuple
from snapsolve.gridfinder.picture_logger import clean_folder_and_log_pictures


class PictureLoggerTest(unittest.TestCase):

    def test_logging(self):
        '''
            We test the picture logging function in picture_logger.py
            by creating a temp folder with some files in and then 
            calling the log function to serialise some blank pictures.
        '''
        temp_folder, temp_file = 'tmp', os.path.join('tmp', 'x.txt')
        try:
            self.assertFalse(os.path.exists(temp_folder))
            os.mkdir(temp_folder)
            open(temp_file, 'w').close()
            self.assertTrue(os.path.exists(temp_file))

            pictures = get_blank_picture_information()
            clean_folder_and_log_pictures(temp_folder, pictures)
            self.assertFalse(os.path.exists(temp_file))

            expected_filenames = set(p.name + '.png' for p in pictures)
            actual_files = list(os.scandir(temp_folder))
            self.assertEquals(len(expected_filenames), len(actual_files))
            for file in actual_files:
                self.assertTrue(file.name in expected_filenames)
        finally:
            shutil.rmtree(temp_folder)


def get_blank_picture_information():
    '''
    '''
    Picture = namedtuple('Picture', 'image_data name')
    shape = (128, 128)
    return [Picture(np.zeros(shape=shape, dtype=np.int), 'blank1'), 
            Picture(np.zeros(shape=shape, dtype=np.int), 'blank2')]