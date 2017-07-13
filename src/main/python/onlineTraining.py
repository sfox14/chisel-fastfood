
import numpy as np
from time import time

# Import datasets, classifiers and performance metrics
from sklearn import svm
from sklearn.metrics import mean_squared_error
from Fastfood import Fastfood
import pandas as pd

"""
Test Mackey-Glass using features extracted from chisel fasfood/parallel  
"""

# The mackey-glass dataset
fname = "../../../datasets/mg30_30_50k.csv"
X = pd.read_csv( fname, index_col=False, header=None )

targets = X[0].values
inputs = X.drop(X.columns[[0]], axis=1).values

data_train = inputs[:30000]
data_test = inputs[30000:]
targets_train = targets[:30000]
targets_test = targets[30000:]

print data_train.shape, data_test.shape, targets_train.shape, targets_test.shape

n_features = inputs.shape[1]
n_examples = inputs.shape[0]


rng = np.random.RandomState(seed=41)
f = Fastfood(sigma=11.47, n_features=n_features, 
                          n_dicts=120, random_state=rng)

f.fit( gType=1 ) # binary G matirx
phi_train = f.transformSW( data_train )
phi_test = f.transformSW( data_test )


# input from chisel features
inputs = np.loadtxt( "../../../datasets/chiselFF_train_mg.csv", delimiter=',' )
phi_train = inputs[:30000]
phi_test = inputs[30000:]

alpha = rng.normal(size=(1, phi_train.shape[1]))
eta = 1e-2

# train
for i,x in enumerate(phi_train):
	x = x.reshape(1,-1)
	y_pred = np.dot( alpha, x.T )
	y = targets_train[i]

	err = y-y_pred
	dJ = np.dot( err, x.reshape(1,-1) )/np.dot( x, x.T )
	
	alpha = alpha + eta*dJ
	
# test
SE = []	
for i,x in enumerate( phi_test):
	x = x.reshape(1,-1)
	ypred = np.dot( alpha, x.T )
	y = targets_test[i]
	err = y - ypred
	SE.append( err[0]**2 )


print "Online MSE = ", np.mean( SE )

# Batch training
mod1 = svm.LinearSVR()
mod1.fit(phi_train, targets_train)
preds = mod1.predict( phi_test )
print "Batch MSE = ", mean_squared_error( preds, targets_test )


