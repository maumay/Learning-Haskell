#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 23 22:09:55 2017

@author: t
"""

import os, cv2, h5py
import numpy as np
from collections import namedtuple
import keras.models as kmodels
import keras.layers as klayers
import keras.utils as kutils
import keras.optimizers as koptimizers


seed = 11
data_dir = os.path.join('..', 'DigitTrainingData')
training_ratio = 0.8
model_dir = os.path.join('..', 'digitmodels')
ConstrainedNetworkInput = namedtuple(
        'ConstrainedNetworkInput', 'input outlabels')


def write_ten_class_model(train, test):
    ''' Writes a model to the disk at the specified location
    along with info log.

    train and test are length 2 iterables with input data
    and output classes.
    '''
    np.random.seed(seed)
    train = ConstrainedNetworkInput(*train)
    m_name = get_name()
    outpath = os.path.join(model_dir, m_name)
    if os.path.exists(outpath):
        raise FileExistsError('No overwriting')
    os.mkdir(outpath)
    
    model = kmodels.Sequential(layers=get_layer_config(), name=m_name)
    cconfig = get_compilation_config()
    model.compile(loss=cconfig['loss'],
                  optimizer=cconfig['optimizer'],
                  metrics=cconfig['metrics'])
    
    X, Y = train.input, train.outlabels
    print(X.shape)
    print('fitting')
    model.fit(X, Y, batch_size=128, epochs=10)
    print('saving')
    model.save(os.path.join(outpath, m_name + '.h5'))
    print('evaluating')
    score = model.evaluate(*test)
    print('\n' + str(score))


def get_model_writing_data():
    '''
    '''
    trainX, trainY, testX, testY = [], [], [], []
    n_class = len(os.listdir(data_dir))
    for folder in os.listdir(data_dir):
        fpath = os.path.join(data_dir, folder)
        nfiles = len(os.listdir(fpath))
        for idx, img_file in enumerate(os.listdir(fpath)):
            file_path = os.path.join(fpath, img_file)
            im = cv2.cvtColor(cv2.imread(file_path), cv2.COLOR_BGR2GRAY)
            #im = cv2.Canny(im,100,200)
            clazz = int(folder) - 1
            if idx < training_ratio*nfiles:
                trainX.append(im)
                trainY.append(clazz)
            else:
                testX.append(im)
                testY.append(clazz)
    # Normalise px values
    trainX = np.array(trainX)/255
    testX = np.array(testX)/255
    one_hot_encode = kutils.to_categorical
    train = ((np.expand_dims(trainX, axis=3), 
              one_hot_encode(np.array(trainY), num_classes=n_class)))
    test = ((np.expand_dims(testX, axis=3), 
             one_hot_encode(np.array(testY), num_classes=n_class)))
    return train, test


def get_compilation_config():
    loss = 'categorical_crossentropy'
    metrics = ['accuracy']
    optimizer = 'adam'#koptimizers.SGD(lr=0.00001)
    #optimizer = koptimizers.RMSprop(lr=0.00001)
    return {'loss': loss, 'metrics': metrics, 'optimizer': optimizer}


def get_name():
    '''
    '''
    return 'digitrecognisermodel' + str(len(os.listdir(model_dir)) + 1)


def get_layer_config():
    '''
    ''' 
    return [klayers.Conv2D(32, 3, input_shape=(32, 32, 1), activation='relu'),
            klayers.MaxPooling2D(pool_size=(2, 2)),
            klayers.Dropout(0.2),
            klayers.Flatten(),
            klayers.Dense(128, activation='relu', use_bias=True),
            klayers.Dense(10, activation='softmax')]
# =============================================================================
#     return [klayers.Conv2D(32, 3, input_shape=(32, 32, 1), padding='same', use_bias=True),
#             klayers.Activation('relu'),
#             klayers.Conv2D(32, 3, padding='same', use_bias=True),
#             klayers.Activation('relu'),
#             klayers.MaxPool2D(pool_size=(2, 2)),
#             klayers.Flatten(),
#             klayers.Dense(95, activation='relu', use_bias=True),
#             klayers.Dense(75, activation='relu', use_bias=True),
#             klayers.Dense(9, activation='softmax')]
# =============================================================================


write_ten_class_model(*get_model_writing_data())
