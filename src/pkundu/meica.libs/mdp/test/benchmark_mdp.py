"""These are some benchmark functions for MDP.
"""

import mdp
#from mdp.utils import symeig
from mdp.utils import matmult as mult

numx = mdp.numx
numx_rand = mdp.numx_rand
numx_fft = mdp.numx_fft

####### benchmark function

def matmult_c_MDP_benchmark(dim):
    """    This benchmark multiplies two contiguous matrices using the
    MDP internal matrix multiplication routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim))
    b = numx_rand.random((dim,dim))
    mult(a,b)

def matmult_c_scipy_benchmark(dim):
    """    This benchmark multiplies two contiguous matrices using the
    scipy internal matrix multiplication routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim))
    b = numx_rand.random((dim,dim))
    numx.dot(a,b)

def matmult_n_MDP_benchmark(dim):
    """    This benchmark multiplies two non-contiguous matrices using the
    MDP internal matrix multiplication routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim)).T
    b = numx_rand.random((dim,dim)).T
    mult(a,b)

def matmult_n_scipy_benchmark(dim):
    """    This benchmark multiplies two non-contiguous matrices using the
    scipy internal matrix multiplication routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim)).T
    b = numx_rand.random((dim,dim)).T
    numx.dot(a,b)

def matmult_cn_MDP_benchmark(dim):
    """    This benchmark multiplies a contiguous matrix with a
    non-contiguous matrix using the MDP internal matrix multiplication
    routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim)).T
    b = numx_rand.random((dim,dim))
    mult(a,b)

def matmult_cn_scipy_benchmark(dim):
    """    This benchmark multiplies a contiguous matrix with a
    non-contiguous matrix using the scipy internal matrix multiplication
    routine.
    First argument matrix dimensionality"""
    a = numx_rand.random((dim,dim)).T
    b = numx_rand.random((dim,dim))
    numx.dot(a,b)

def quadratic_expansion_benchmark(dim, len, times):
    """    This benchmark expands random data of shape (len, dim)
    'times' times.
    Arguments: (dim,len,times)."""
    a = numx_rand.random((len,dim))
    qnode = mdp.nodes.QuadraticExpansionNode()
    for i in xrange(times):
        qnode(a)

def polynomial_expansion_benchmark(dim, len, degree, times):
    """    This benchmark expands random data of shape (len, dim)
    'times' times in the space of polynomials of degree 'degree'.
    Arguments: (dim,len,degree,times)."""
    numx_rand.seed(4253529)
    a = numx_rand.random((len,dim))
    pnode = mdp.nodes.PolynomialExpansionNode(degree)
    for i in xrange(times):
        pnode(a)

# ISFA benchmark

def _tobias_mix(src):
    mix = src.copy()
    mix[:,0]=(src[:,1]+3*src[:,0]+6)*numx.cos(1.5*numx.pi*src[:,0])
    mix[:,1]=(src[:,1]+3*src[:,0]+6)*numx.sin(1.5*numx.pi*src[:,0])
    return mix

def _get_random_slow_sources(nsrc, distr_fun):
    # nsrc: number of sources
    # distr_fun: random numbers function

    src = distr_fun(size=(50000, nsrc))
    fsrc = numx_fft.rfft(src, axis=0)
    # enforce different time scales
    for i in xrange(nsrc):
        fsrc[5000+(i+1)*1000:,i] = 0.
    src = numx_fft.irfft(fsrc,axis=0)
    return src

def isfa_spiral_benchmark():
    """    Apply ISFA to twisted data."""
    numx_rand.seed(116599099)
    # create independent sources
    src = _get_random_slow_sources(2, numx_rand.laplace)
    # subtract mean and rescale between -1 and 1
    src -= src.mean(axis=0)
    src /= abs(src).max()
    # apply nonlinear "twist" transformation
    exp_src = _tobias_mix(src)
    # train
    flow = mdp.Flow([mdp.nodes.PolynomialExpansionNode(5),
                     mdp.nodes.SFANode(),
                     mdp.nodes.ISFANode(lags=30, whitened=False,
                                        sfa_ica_coeff=[1.,300.],
                                        eps_contrast=1e-5,
                                        output_dim=2, verbose=False)])
    flow.train(exp_src)

def sfa_benchmark():
    """    Apply SFA to twisted data."""
    numx_rand.seed(424507)
    # create independent sources
    nsrc = 15
    src = _get_random_slow_sources(nsrc, numx_rand.normal)
    src = src[:5000,:]
    src = mult(src, numx_rand.uniform(size=(nsrc, nsrc))) \
          + numx_rand.uniform(size=nsrc)
    # train
    flow = mdp.Flow([mdp.nodes.PolynomialExpansionNode(3),
                     mdp.nodes.PCANode(output_dim = 100),
                     mdp.nodes.SFANode(output_dim = 30)])
    #src = src.reshape(1000,5,nsrc)
    flow.train([None, [src], [src]])

#### benchmark tools

# function used to measure time
import time
TIMEFUNC = time.time

def timeit(func,*args,**kwargs):
    """Return function execution time in 1/100ths of a second."""
    tstart = TIMEFUNC()
    func(*args,**kwargs)
    return (TIMEFUNC()-tstart)*100.

def _random_seed():
    import sys
    seed = int(numx_rand.randint(2**31-1))
    numx_rand.seed(seed)
    sys.stderr.write("Random Seed: " + str(seed)+'\n')

def run_benchmarks(bench_funcs, time_digits=15):

    results_str = '| %%s | %%%d.2f |' % time_digits
    label_str = '| %%s | %s |' % 'Time (sec/100)'.center(time_digits)

    tstart = TIMEFUNC()

    # loop over all benchmarks functions
    for func, args_list in bench_funcs:
        # number of combinations of arguments(cases)
        ncases = len(args_list)
        funcname = func.__name__[:-10]

        # loop over all cases
        for i in xrange(ncases):
            args = args_list[i]

            # format description string
            descr = funcname + str(tuple(args))
            if i==0:
                # print summary table header
                descrlen = len(descr)+6
                results_strlen = time_digits+descrlen+7
                print '\nTiming results (%s, %d cases):' % (funcname, ncases)
                print func.__doc__
                print '+'+'-'*(results_strlen-2)+'+'
                print label_str % 'Description'.center(descrlen)
                print '+'+'-'*(results_strlen-2)+'+'

            # execute function
            t = timeit(func, *args)

            # print summary table entry
            print results_str % (descr.center(descrlen), t)

        # print summary table tail
        print '+'+'-'*(results_strlen-2)+'+'

    print '\nTotal running time:', (TIMEFUNC()-tstart)*100.

####### /benchmark function

POLY_EXP_ARGS = [(2**i, 100, j, 200) for j in xrange(2,5) for i in xrange(2,4)]

#if mdp.numx_description in ['symeig', 'scipy', 'numpy']:
#    MUL_MTX_DIMS = [[2**i] for i in xrange(4,11)]
#    # list of (benchmark function, list of arguments)
#    BENCH_FUNCS = [(matmult_c_MDP_benchmark, MUL_MTX_DIMS),
#                   (matmult_c_scipy_benchmark, MUL_MTX_DIMS),
#                   (matmult_n_MDP_benchmark, MUL_MTX_DIMS),
#                   (matmult_n_scipy_benchmark, MUL_MTX_DIMS),
#                   (matmult_cn_MDP_benchmark, MUL_MTX_DIMS),
#                   (matmult_cn_scipy_benchmark, MUL_MTX_DIMS),
#                   (polynomial_expansion_benchmark, POLY_EXP_ARGS)]
#else:
#    BENCH_FUNCS = [(polynomial_expansion_benchmark, POLY_EXP_ARGS)]
BENCH_FUNCS = [(polynomial_expansion_benchmark, POLY_EXP_ARGS),
               (isfa_spiral_benchmark, [[]]),
               (sfa_benchmark, [[]])]

def get_benchmarks():
    return BENCH_FUNCS

if __name__ == "__main__":
    print "Running benchmarks: "
    run_benchmarks(get_benchmarks())
