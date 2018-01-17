
import numpy as np
from scipy.stats import chi

"""
Python code for generating Fastfood parameters for FPGA
"""

d = 1024
n = 16384
k = n/d
sigma = 11.47
npts = 256
fb = 9

rng = np.random.RandomState(seed=41)

def l2norm_along_axis1(X):
    return np.sqrt(np.einsum('ij,ij->i', X, X))

def toFixed(x, fb):
    v = (1<<fb)
    if( x>=0 ):
        return int( x * v +0.5 )
    else:
        return int( x*v -0.5 )


# generate parameters

B = rng.randint( 2, size=(k, d) )*2 -1
G = rng.normal( size=(k, d) )
S = (1 / (sigma * np.sqrt(d)) ) * np.multiply(1 / l2norm_along_axis1(G)
            .reshape((-1, 1)),
            chi.rvs(d, size=(k, d) ) )

alpha = rng.normal( size=(1,n) )

x = np.linspace(0, npts, npts)/float(1<<(int(np.log2(npts))-1) )
b = rng.uniform(0, 1, size=npts)*2*np.pi
A = np.sqrt(2./n)
cos = A*np.cos(x*np.pi + 0)


# save parameters to csv and cosTab.txt
"""
np.savetxt("params/B.csv", B, delimiter=",")
np.savetxt("params/G.csv", G, delimiter=",")
np.savetxt("params/S.csv", S, delimiter=",")
np.savetxt("params/alpha.csv", alpha, delimiter=",")
"""

f = open('params/B.txt', 'w')
for i in range(B.shape[0]):
    for j in range(B.shape[1]):
        estr = "B[%d][%d] = (PRECTYPE) %d\n"%(i, j, toFixed(B[i][j], fb))
        f.write(estr)
f.close()

f = open('params/G.txt', 'w')
for i in range(G.shape[0]):
    for j in range(G.shape[1]):
        estr = "G[%d][%d] = (PRECTYPE) %d\n"%(i, j, toFixed(G[i][j], fb))
        f.write(estr)
f.close()

f = open('params/S.txt', 'w')
for i in range(S.shape[0]):
    for j in range(S.shape[1]):
        estr = "S[%d][%d] = (PRECTYPE) %d\n"%(i, j, toFixed(S[i][j], fb))
        f.write(estr)
f.close()

f = open('cosTab.txt', 'w')
for i,x in enumerate(cos):
    estr = "COS_LUT[%d] = (PRECTYPE) %d\n"%(i, toFixed(x, fb))
    f.write(estr)

f.close()

