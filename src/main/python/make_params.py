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
f = ffSuite(n_features=nf, n_dicts=nd, random_state=rng)
f.fit( gType=gt )

# save matrices as csv files
np.savetxt(directory+"/GPHB.csv", f.Vg, delimiter=",", fmt="%10.5f")
np.savetxt(directory+"/PHB.csv", f.Vp, delimiter=",", fmt="%.f")
np.savetxt(directory+"/H.csv", f.H, delimiter=",", fmt="%.f")
np.savetxt(directory+"/G.csv", f.G, delimiter=",", fmt="%10.12f")
np.savetxt(directory+"/S.csv", f.S_hw, delimiter=",", fmt="%10.12f")
np.savetxt(directory+"/B.csv", f.B, delimiter=",", fmt="%.f")

X = np.random.randn(10, nf)

np.savetxt(directory+"/X.csv", X, delimiter=",", fmt="%10.12f")

GPHBX = np.dot(X, f.Vg.T[:,0])

print GPHBX