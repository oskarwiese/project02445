import numpy as np 
import os 
import pyreadr 
#from rpy import *
#r.load("file name here")

os.chdir('/home/oskar/Desktop/02445/project02445/project1')
data = pyreadr.read_r('armdata.RData')

print(data['oddict_keys[]'])