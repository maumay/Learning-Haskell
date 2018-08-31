#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 24 20:33:06 2017

@author: t
"""

import keras.models as kmodels
import digitrecogniser as dr


model_dir = os.path.join('..', 'digitmodels')


def get_model_eval(m_name):
    fpath = os.path.join(model_dir, m_name, m_name + '.h5')
    model = kmodels.load_model(fpath)
    _, test = dr.get_model_writing_data()
    score = model.evaluate(*test)
    print('\n' + score)
    #print(model.evaluate(*test))


get_model_eval('digitrecognisermodel1')