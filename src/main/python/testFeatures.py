
import numpy as np
from time import time

# Import datasets, classifiers and performance metrics
from sklearn import datasets, svm, pipeline
from sklearn.kernel_approximation import (RBFSampler,
                                          Nystroem)
from Fastfood import Fastfood


"""
Test features produced by fastfood/parallel
"""


# The digits dataset
digits = datasets.load_digits(n_class=9)

n_samples = len(digits.data)
data = digits.data / 16.
data -= data.mean(axis=0)
print data.shape

data_train, targets_train = data[:n_samples / 2], digits.target[:n_samples / 2]
data_test, targets_test = data[n_samples / 2:], digits.target[n_samples / 2:]


# Create a classifier: a support vector classifier
kernel_svm = svm.SVC(gamma=.2)
linear_svm = svm.LinearSVC()

# create pipeline from kernel approximation
# and linear svm
feature_map_fourier = RBFSampler(gamma=.2, random_state=1)
feature_map_nystroem = Nystroem(gamma=.2, random_state=1)

fourier_approx_svm = pipeline.Pipeline([("feature_map", feature_map_fourier),
                                        ("svm", svm.LinearSVC())])

nystroem_approx_svm = pipeline.Pipeline([("feature_map", feature_map_nystroem),
                                        ("svm", svm.LinearSVC())])



kernel_svm.fit(data_train, targets_train)
kernel_svm_score = kernel_svm.score(data_test, targets_test)

linear_svm.fit(data_train, targets_train)
linear_svm_score = linear_svm.score(data_test, targets_test)

fourier_approx_svm.set_params(feature_map__n_components=128)
nystroem_approx_svm.set_params(feature_map__n_components=128)

nystroem_approx_svm.fit(data_train, targets_train)
fourier_approx_svm.fit(data_train, targets_train)

fourier_score = fourier_approx_svm.score(data_test, targets_test)
nystroem_score = nystroem_approx_svm.score(data_test, targets_test)


rng = np.random.RandomState(seed=41)
f = Fastfood(sigma=11.47, n_features=64, 
                          n_dicts=128, random_state=rng)

f.fit( gType=1 )
phi_train = f.transformSW( data_train )
phi_test = f.transformSW( data_test )
mod1 = svm.LinearSVC()
mod1.fit( phi_train, targets_train )
fastfood_score = mod1.score( phi_test, targets_test )

phi_train = np.loadtxt("../../../chiselFF_train1810128.csv", delimiter=',')
phi_test = np.loadtxt("../../../chiselFF_test1810128.csv", delimiter=',')
mod2 = svm.LinearSVC()
mod2.fit( phi_train, targets_train )
fastfood_score2 = mod2.score( phi_test, targets_test )

print fastfood_score2, fastfood_score, nystroem_score, fourier_score, kernel_svm_score, linear_svm_score


