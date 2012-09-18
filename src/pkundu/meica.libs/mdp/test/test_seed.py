###################################################################
### After changing this file, please copy it so that files
### ../../{mbp,bimdp}/test/test_seed.py are identical.
###################################################################

import mdp

_SEED = None

def _compare_with_seed(seed):
    global _SEED
    if _SEED is None:
        _SEED = seed
    return _SEED == seed

def test_seed():
    seed = mdp.numx_rand.get_state()[1][0]
    assert _compare_with_seed(seed), (_SEED, seed)
    mdp.numx_rand.seed(seed+1)

def test_seed_clone():
    # we need two identical functions to check that the seed
    # is reset at every call
    seed = mdp.numx_rand.get_state()[1][0]
    assert _compare_with_seed(seed), (_SEED, seed)
    mdp.numx_rand.seed(seed+1)

def test_seed_reset():
    # this function resets the global _SEED, so
    # that we can call the tests several times in
    # a row with different seeds without getting a failure
    global _SEED
    _SEED = None
