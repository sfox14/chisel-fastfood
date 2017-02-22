
import numpy as np

from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.utils import check_random_state
from sklearn.utils.random import choice
from scipy.stats import chi
from sklearn.utils.random import sample_without_replacement
import scipy.sparse as sp

try:
    from sklearn.utils import check_array
except ImportError:
    from sklearn.utils import check_arrays

    def check_array(*args, **kwargs):
        X, = check_arrays(*args, **kwargs)
        return X



def dim_constraints(d=4, n=8):
    if not is_power_of_two(d):
        # find d that fulfills 2^l
        d = np.power(2, np.floor(np.log2(d)) + 1)
    divisor, remainder = divmod(n, d)
    k = int(divisor)
    if remainder != 0:
    # output info, that we increase n so that d is a divider of n
        n = (divisor + 1) * d
        k = int(divisor+1)
    return int(d), int(n), k

def sparse_random_matrix(n_components, n_features, density='auto',random_state=None):
    '''
    From scikit-learn random projection module
    '''

    rng = check_random_state(random_state)
    if density == 1:
        # skip index generation if totally dense
        components = rng.binomial(1, 0.5, (n_components, n_features)) * 2 - 1
        return 1 / np.sqrt(n_components),  components

    else:
        # Generate location of non zero elements
        indices = []
        offset = 0
        indptr = [offset]
        for i in xrange(n_components):
            # find the indices of the non-zero components for row i
            n_nonzero_i = rng.binomial(n_features, density)
            indices_i = sample_without_replacement(n_features, n_nonzero_i,
                                                   random_state=rng)
            indices.append(indices_i)
            offset += n_nonzero_i
            indptr.append(offset)

        indices = np.concatenate(indices)

        # Among non zero components the probability of the sign is 50%/50%
        data = rng.binomial(1, 0.5, size=np.size(indices)) * 2 - 1

        # build the CSR structure by concatenating the rows
        components = sp.csr_matrix((data, indices, indptr),
                                   shape=(n_components, n_features))

        components = components.toarray()
        return np.sqrt(1 / density) / np.sqrt(n_components), components

def l2norm_along_axis1(X):

    return np.sqrt(np.einsum('ij,ij->i', X, X))

def HadamardMatrix(n=4):
    '''
    get the entries of hadamard matrix and flatten
    '''
    # make sure n is a power of 2
    assert (1 and ((n & (n - 1)) == 0) )

    k = int(np.log2(n)-1)
    h = np.array([[1, 1], [1, -1]])

    for i in range(k):
        row1 = np.hstack((h,h))
        row2 = np.hstack((h, h*-1))
        h = np.vstack((row1, row2))

    return h


from numba import jit

@jit(nopython=True)
def fht(array_):
    """ Pure Python implementation for educational purposes. """
    bit = length = len(array_) # dimension (d)
    for _ in xrange(int(np.log2(length))):
        # splits the array in half. then loops through i. operations to the
        # top and bottom halves of the array mirror each other with the 
        # exception that we negate the bottom
        bit >>= 1  #(32, 16, 8, 4, 2, 1)
        #print(bit)
        for i in xrange(length): #(0-63)
            # bitwise and
            if i & bit == 0:
                # bitwise or
                j = i | bit
                temp = array_[i]
                array_[i] += array_[j]
                array_[j] = temp - array_[j]

@jit(nopython=True)
def is_power_of_two(input_integer):
    """ Test if an integer is a power of two. """
    if input_integer == 1:
        return False
    return input_integer != 0 and ((input_integer & (input_integer - 1)) == 0)

@jit(nopython=True)
def fht2(array_):
    """ Two dimensional row-wise FHT. """
    if not is_power_of_two(array_.shape[1]):
        raise ValueError('Length of rows for fht2 must be a power of two')

    # loop through the dataset
    for x in xrange(array_.shape[0]):
        fht(array_[x])


class ffSuite( object ):

    def __init__( self, 
                  sigma=np.sqrt(1./2.), 
                  n_features=4, 
                  n_dicts=8,
                  sparsity=0.2,
                  random_state=None,
                  tradeoff='mem',
                  verbose=False):
        self.sigma = sigma
        self.n_dicts = n_dicts
        self.d_orig = n_features
        self.random_state = random_state
        self.density = 1-sparsity
        self.tradeoff = tradeoff
        self.verbose = verbose
        self.rng = check_random_state(self.random_state)

        assert (self.density <= 1.)

        self.enforce_constraints()
    

    def enforce_constraints(self):
        self.d, self.n, self.k = dim_constraints(d=self.d_orig, 
                                                 n=self.n_dicts)
        self.pad = (self.d - self.d_orig)

        if self.verbose:
            print "Model Constraints:"
            print "------------------"
            print " Input Dimension:     ", self.d_orig
            print " Input Dict Size:     ", self.n_dicts
            print " FF Dimension:        ", self.d
            print " FF Dict Size:        ", self.n
            print " FF Stack:            ", self.k
            print " Padding Req:          %d\n"%(self.pad)

    def pad_with_zeros(self, X):
        try:
            X_padded = np.pad(X,
                              ((0, 0),
                               (0, self.pad)),
                              'constant')
        except AttributeError:
            zeros = np.zeros((X.shape[0],
                              self.pad))
            X_padded = np.concatenate((X, zeros), axis=1)

        return X_padded

    @staticmethod
    def fast_walsh_hadamard(result):
        fht2(result)

    def apply_approximate_gaussian_matrix(self, B, G, P, X):
        """ Create mapping of all x_i by applying B, G and P step-wise """
        num_examples = X.shape[0]

        result = np.multiply(B, X.reshape((1, num_examples, 1, self.d)))
        result = result.reshape((num_examples*self.k, self.d))
        ffSuite.fast_walsh_hadamard(result)
        result = result.reshape((num_examples, -1))
        np.take(result, P, axis=1, mode='wrap', out=result)
        np.multiply(np.ravel(G), result.reshape(num_examples, self.n),
                    out=result)
        result = result.reshape(num_examples*self.k, self.d)
        ffSuite.fast_walsh_hadamard(result)
        return result

    def scale_transformed_data(self, S, VX):
        """ Scale mapped data VX to match kernel(e.g. RBF-Kernel) """
        VX = VX.reshape(-1, self.k*self.d)
        return (1 / (self.sigma * np.sqrt(self.d)) *
                np.multiply(np.ravel(S), VX))

    def phi(self, X):
        if self.tradeoff == 'accuracy':
            m, n = X.shape
            out = np.empty((m, 2 * n), dtype=X.dtype)
            np.cos(X, out=out[:, :n])
            np.sin(X, out=out[:, n:])
            out /= np.sqrt( X.shape[1] )
            return out
        else:
            np.cos(X+self.U, X)
            return X * np.sqrt(2. / X.shape[1])



    def fit(self, gType=1 ):
        self.T = gType
        if self.verbose:
            print "Gaussian Matrix: "
            print "----------------"
            if gType == 0:
                s = " N(0,1)\n"
            elif gType == 1:
                s = " {-1,1}\n"
            elif gType == 2:
                s = " {-1,0,1}\n"
            else:
                print "Error: select G marix"
                raise Exception
            print s
        
        # --- Software Diagonal Matrices ---   
        self.B = self.rng.randint( 2, size=(self.k, self.d) )*2 -1

        if self.T == 0:
            self.G = self.rng.normal(size=(self.k,self.d))
            coeff = 1
        elif self.T == 1:
            coeff, self.G = sparse_random_matrix(self.k, self.d, density=1., random_state=self.rng)
        elif self.T == 2:
            coeff, self.G = sparse_random_matrix(self.k, self.d, density=self.density, random_state=self.rng)
        else:
            raise Exception

        self.P = np.hstack([(i*self.d)+self.rng.permutation(self.d)
                            for i in range(self.k)])

        np.random.seed(seed=23)
        self.S = np.multiply(1 / l2norm_along_axis1(coeff*self.G)
                                 .reshape((-1, 1)),
                                 chi.rvs(self.d, size=(self.k, self.d)))
        self.S = self.S*coeff

        self.U = self.rng.uniform(0, 2 * np.pi, size=self.n)

        # --- Hardware RAM-based Shift Registers ----
        '''
        self.Vp - after the permutation
        self.Vg - after the gaussian
        self.Vf - after the 2nd Hadamard
        self.H  - hadamard matrix 
        '''
        B = self.B
        if self.pad>0:
            B[:, -self.pad:] *= 0
        H = HadamardMatrix(n=self.d)

        # for each k in B, multiply the array elementwise with each row of H and vstack rows
        V = np.vstack( [np.multiply( B[i], H) for i in range(self.k) ] )
        # apply permutation to inidces (n indices for n_dicts)
        p = np.arange(self.n).reshape(1,-1)
        np.take(p, self.P, axis=1, mode='wrap', out=p)
        p = np.ravel(p)
        # then apply permutation of the indices to V. need to transpose because np.take works on cols
        self.Vp = np.take( V.T, p, axis=1, mode='wrap' ).T

        self.Vg = np.multiply( np.ravel(self.G), self.Vp.T ).T

        # reshape H(d, d, 1)
        H = H.reshape(H.shape[0], H.shape[1], 1)

        # multiply combination of inputs after G with hadamard transform
        Vh = np.vstack( [ np.multiply( self.Vg.reshape(self.k,self.d,self.d)[i], H ) for i in range(self.k) ] )
        # simplify the expression, so it's represented as a multiple of the input (i.e. 0 + x2 -4x3 + x4)
        Vf = np.zeros( shape=(self.d*self.k,self.d) )
        for i in range(self.d*self.k):
            for j in range(self.d):
                Vf[i] += Vh[i][j]
        self.Vf = Vf
        self.H = HadamardMatrix(n=self.d)

        # remove zero columns
        if self.pad>0:
            self.Vp = self.Vp[:,:-self.pad]
            self.Vg = self.Vg[:,:-self.pad]
            self.Vf = self.Vf[:,:-self.pad]


    def transformSW(self, X):
        
        assert (X.shape[1] == self.d_orig)

        X_padded = self.pad_with_zeros(X) #pad if X dim not power of 2

        HGPHBX = self.apply_approximate_gaussian_matrix(self.B,
                                                        self.G,
                                                        self.P,
                                                        X_padded)
        VX = self.scale_transformed_data(self.S, HGPHBX)
        phi = self.phi(VX)
        return phi

    def transformHW(self, X, task='Vf'):
        # we don't need to pad with zeros (already truncated)
        if self.verbose:
            print "HW Construction:"
            print "----------------"
            print " %s\n"%task

        if task == 'Vf':
            HGPHBX = np.dot(X, self.Vf.T)
        elif task == 'Vg':
            GPHBX = np.dot(X, self.Vg.T).reshape(self.k, -1 ,self.d)
            HGPHBX = np.vstack( [ np.dot(GPHBX[i], self.H) ] for i in range(self.k) )
            HGPHBX = np.ravel(HGPHBX)
        elif task == 'Vp':
            PHBX = np.dot(X, self.Vp.T)
            GPHBX = np.multiply(np.ravel(self.G), PHBX).reshape(self.k, -1 ,self.d)
            HGPHBX = np.vstack( [ np.dot(GPHBX[i], self.H) ] for i in range(self.k) )
            HGPHBX = np.ravel(HGPHBX)
        else:
            print "Error: select a supported task"
            raise Exception

        VX = self.scale_transformed_data(self.S, HGPHBX)
        phi = self.phi(VX)
        return phi



def testHardwareRepresentation():

    # 144 tests: 4x4x3x3
    n_features = [4,5,8,11]
    n_dicts = [8,9,15,79]
    rng = np.random.RandomState(seed=41)
    gType = [0,1,2]
    task = ['Vp', 'Vg', 'Vf']

    print "Test HW: "
    print "---------"
    print " result = ",

    results = []
    for nf in n_features:
        for nd in n_dicts:
            f = ffSuite(n_features=nf, n_dicts=nd, random_state=rng)
            for gt in gType:
                f.fit( gType= gt )
                print f.Vg
                for t in task:
                    x = np.random.randn(5, nf)
                    sw = f.transformSW(x)
                    hw = f.transformHW(x, task=t)
                    results.append( (np.sum(sw) - np.sum(hw)) )

    pstring = "PASS\n" if (sum(results) <= 1e-10) else "FAIL\n"
    print pstring




if __name__ == "__main__":

    testHardwareRepresentation()

