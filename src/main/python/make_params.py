import numpy as np
import sys
import os

from Fastfood import Fastfood

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
f = Fastfood(sigma=11.47, n_features=nf, n_dicts=nd, random_state=rng)
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

#X = np.random.randn(10, nf)
X = np.loadtxt("../exanicFastfood/artificialNovMod.csv", delimiter=",", 
        usecols = range(3, 11) )

np.savetxt(directory+"/X.csv", X, delimiter=",", fmt="%10.12f")

GPHBX = np.dot(X, f.Vg.T).reshape(f.k, -1 ,f.d)
HGPHBX = np.vstack( [ np.dot(GPHBX[i], f.H) ] for i in range(f.k) )
print GPHBX.shape, HGPHBX.shape

