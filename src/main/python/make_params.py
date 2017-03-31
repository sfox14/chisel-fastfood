import numpy as np
import sys
import os

from ffSuite import ffSuite

'''
Script for producing S, G, H, PHB, GPHB matrices

run:	python make_params.py n_dicts n_feats gType folder

When called from scala, use folder = /.tmp
'''

nd = int( sys.argv[1] )
nf = int( sys.argv[2] )
gt = int( sys.argv[3] )
folder = sys.argv[4]

directory = os.getcwd()+folder

if not os.path.exists( directory ):
	os.makedirs(directory)


rng = np.random.RandomState(seed=41)
f = ffSuite(sigma=11.47, n_features=nf, n_dicts=nd, random_state=rng)
f.fit( gType=gt )

alpha = rng.normal(size=( 1, f.n ))
print alpha

# save matrices as csv files
np.savetxt(directory+"/GPHB.csv", f.Vg, delimiter=",", fmt="%10.5f")
np.savetxt(directory+"/PHB.csv", f.Vp, delimiter=",", fmt="%.f")
np.savetxt(directory+"/H.csv", f.H, delimiter=",", fmt="%.f")
np.savetxt(directory+"/G.csv", f.G, delimiter=",", fmt="%10.12f")
np.savetxt(directory+"/S.csv", f.S_hw, delimiter=",", fmt="%10.12f")
np.savetxt(directory+"/U.csv", f.U_hw, delimiter=",", fmt="%10.12f")
np.savetxt(directory+"/B.csv", f.B, delimiter=",", fmt="%.f")
np.savetxt(directory+"/ALPHA.csv", alpha, delimiter=",", fmt="%10.12f")


import pandas as pd
"""
from sklearn import datasets

# The digits dataset
digits = datasets.load_digits(n_class=9)

# To apply an classifier on this data, we need to flatten the image, to
# turn the data in a (samples, feature) matrix:
n_samples = len(digits.data)
data = digits.data / 16.
data -= data.mean(axis=0)

data_train, targets_train = data[:n_samples / 2], digits.target[:n_samples / 2]
data_test, targets_test = data[n_samples / 2:], digits.target[n_samples / 2:]

X = data_train
np.savetxt(directory+"/X.csv", X, delimiter=",", fmt="%10.12f")

print X.shape
#"""
"""
# Mackey Glass
fname = "../../mg30_30_50k.csv"
X = pd.read_csv( fname, index_col=False, header=None )

targets = X[0].values
inputs = X.drop(X.columns[[0]], axis=1).values

X = inputs
np.savetxt(directory+"/X.csv", X, delimiter=",", fmt="%10.12f")
"""

#"""
#X = np.random.randn(10, nf)
data = np.loadtxt("../exanicFastfood/artificialNovMod.csv", delimiter=",", 
        usecols = range(3, 11) )
print data.shape
X = data
print X[0]

np.savetxt(directory+"/X.csv", X, delimiter=",", fmt="%10.12f")

GPHBX = np.dot(X, f.Vg.T).reshape(f.k, -1 ,f.d)
HGPHBX = np.vstack( [ np.dot(GPHBX[i], f.H) ] for i in range(f.k) )
print GPHBX.shape, HGPHBX.shape
#"""