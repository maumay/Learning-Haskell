import os, cv2


def clean_folder_and_log_pictures(output_folder, image_dataset):
    '''
        Input: output_folder - a path-like object.

               image_dataset - an iterable of namedtuples containing
                               a numpy array (the image attribute) 
                               and a string (the filename attribute).

        This function deletes all files in the output_folder 
        and then saves all image data to the disk as .png files.
    '''
    if not os.path.isdir(output_folder):
        raise NotADirectoryError()
    for entry in os.scandir(output_folder):
        if entry.is_dir():
            raise IsADirectoryError()
        os.remove(entry.path)
    for image, name in image_dataset:
        path = os.path.join(output_folder, name + '.png')
        cv2.imwrite(path, image)
    return
