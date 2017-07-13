import numpy as np 
import matplotlib.pyplot as plt
import sys

filename = "../../../test_results_18.csv"
if len(sys.argv)==2 :
	filename = sys.argv[1]

data = np.loadtxt(filename, delimiter=",")
length = len(data)*0.8

# data
ypred_train, y_train = data[:length,0], data[:length,1]
ypred_test, y_test = data[length:,0], data[length:,1]

# square error
sqerr_train = np.square( (y_train - ypred_train) )
sqerr_test = np.square( (y_test - ypred_test) )

# mse
print "MSE = ", np.mean(sqerr_test)


plt.figure(figsize=(8,8))
mse = plt.subplot(211)
yres = plt.subplot(212)

mse.plot( np.arange(len(sqerr_train)), sqerr_train )
yres.plot( np.arange(len(ypred_test)), ypred_test )
yres.plot( np.arange(len(ypred_test)), y_test )

plt.show()
