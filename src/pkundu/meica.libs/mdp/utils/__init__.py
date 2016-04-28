__docformat__ = "restructuredtext en"

from routines import (timediff, refcast, scast, rotate, random_rot,
                      permute, symrand, norm2, cov2,
                      mult_diag, comb, sqrtm, get_dtypes, nongeneral_svd,
                      hermitian, cov_maxima,
                      lrep, rrep, irep, orthogonal_permutations,
                      izip_stretched,
                      weighted_choice, bool_to_sign, sign_to_bool, gabor,
                      invert_exp_funcs2)
try:
    from collections import OrderedDict
except ImportError:
    ## Getting an Ordered Dict for Python < 2.7
    from _ordered_dict import OrderedDict

try:
    from tempfile import TemporaryDirectory
except ImportError:
    from temporarydir import TemporaryDirectory

from introspection import dig_node, get_node_size, get_node_size_str
from quad_forms import QuadraticForm, QuadraticFormException
from covariance import (CovarianceMatrix, DelayCovarianceMatrix,
                        MultipleCovarianceMatrices,CrossCovarianceMatrix)
from progress_bar import progressinfo
from slideshow import (basic_css, slideshow_css, HTMLSlideShow,
                       image_slideshow_css, ImageHTMLSlideShow,
                       SectionHTMLSlideShow, SectionImageHTMLSlideShow,
                       image_slideshow, show_image_slideshow)

from _symeig import SymeigException

import mdp as _mdp
# matrix multiplication function
# we use an alias to be able to use the wrapper for the 'gemm' Lapack
# function in the future
mult = _mdp.numx.dot
matmult = mult

if _mdp.numx_description == 'scipy':
    def matmult(a,b, alpha=1.0, beta=0.0, c=None, trans_a=0, trans_b=0):
        """Return alpha*(a*b) + beta*c.
        a,b,c : matrices
        alpha, beta: scalars
        trans_a : 0 (a not transposed), 1 (a transposed),
                  2 (a conjugate transposed)
        trans_b : 0 (b not transposed), 1 (b transposed),
                  2 (b conjugate transposed)
        """
        if c:
            gemm,=_mdp.numx_linalg.get_blas_funcs(('gemm',),(a,b,c))
        else:
            gemm,=_mdp.numx_linalg.get_blas_funcs(('gemm',),(a,b))

        return gemm(alpha, a, b, beta, c, trans_a, trans_b)

# workaround to numpy issues with dtype behavior:
# 'f' is upcasted at least in the following functions
_inv = _mdp.numx_linalg.inv
inv = lambda x: refcast(_inv(x), x.dtype)
_pinv = _mdp.numx_linalg.pinv
pinv = lambda x: refcast(_pinv(x), x.dtype)
_solve = _mdp.numx_linalg.solve
solve = lambda x, y: refcast(_solve(x, y), x.dtype)

def svd(x, compute_uv = True):
    """Wrap the numx SVD routine, so that it returns arrays of the correct
    dtype and a SymeigException in case of failures."""
    tc = x.dtype
    try:
        if compute_uv:
            u, s, v = _mdp.numx_linalg.svd(x)
            return refcast(u, tc), refcast(s, tc), refcast(v, tc)
        else:
            s = _mdp.numx_linalg.svd(x, compute_uv=False)
            return refcast(s, tc)
    except _mdp.numx_linalg.LinAlgError, exc:
        raise SymeigException(str(exc))

__all__ = ['CovarianceMatrix', 'DelayCovarianceMatrix','CrossCovarianceMatrix',
           'MultipleCovarianceMatrices', 'QuadraticForm',
           'QuadraticFormException',
           'comb', 'cov2', 'dig_node', 'get_dtypes', 'get_node_size',
           'hermitian', 'inv', 'mult', 'mult_diag', 'nongeneral_svd',
           'norm2', 'permute', 'pinv', 'progressinfo',
           'random_rot', 'refcast', 'rotate', 'scast', 'solve', 'sqrtm',
           'svd', 'symrand', 'timediff', 'matmult',
           'HTMLSlideShow', 'ImageHTMLSlideShow',
           'basic_css', 'slideshow_css', 'image_slideshow_css',
           'SectionHTMLSlideShow',
           'SectionImageHTMLSlideShow', 'image_slideshow',
           'lrep', 'rrep', 'irep',
           'orthogonal_permutations', 'izip_stretched',
           'weighted_choice', 'bool_to_sign', 'sign_to_bool',
           'OrderedDict', 'TemporaryDirectory', 'gabor', 'fixup_namespace']

def _without_prefix(name, prefix):
    if name.startswith(prefix):
        return name[len(prefix):]
    else:
        return None

import os
FIXUP_DEBUG = os.getenv('MDPNSDEBUG')

def fixup_namespace(mname, names, old_modules, keep_modules=()):
    """Update ``__module__`` attribute and remove ``old_modules`` from namespace

    When classes are imported from implementation modules into the
    package exporting them, the ``__module__`` attribute reflects the
    place of definition. Splitting the code into separate files (and
    thus modules) makes the implementation managable. Nevertheless, we
    do not want the implementation modules to be visible and delete
    their names from the package's namespace. This causes some
    problems: when looking at the exported classes and other objects,
    their ``__module__`` attribute points to something non-importable,
    ``repr`` output and documentation do not show the module from
    which they are supposed to be imported. The documentation
    generators like epydoc and sphinx are also confused. To alleviate
    those problems, the ``__module__`` attributes of all exported
    classes defined in a "private" module and then exported elsewhere
    are changed to the latter.

    For each name in ``names``, if ``<mname>.<name>`` is accessible,
    and if its ``__module__`` attribute is equal to one of the names
    in ``old_modules``, it is changed to ``"<mname>"``. In other
    words, all the ``__module__`` attributes of objects exported from
    module ``<mname>`` are updated, iff they used to point to one of the
    "private" modules in ``old_modules``.

    This operation is performed not only for classes, but actually for
    all objects with the ``__module__`` attribute, following the rules
    stated above. The operation is also performed recursively, not
    only for names in ``names``, but also for methods, inner classes,
    and other attributes. This recursive invocation is necessary
    because all the problems affecting top-level exported classes also
    affect their attributes visible for the user, and especially
    documented functions.

    If ``names`` is ``None``, all public names in module ``<mname>``
    (not starting with ``'_'``) are affected.

    After the ``__module__`` attributes are changed, "private" modules given
    in ``old_modules``, except for the ones in ``keep_modules``, are deleted
    from the namespace of ``<mname>`` module.
    """
    import sys
    module = sys.modules[mname]
    if names is None:
        names = [name for name in dir(module) if not name.startswith('_')]
    if FIXUP_DEBUG:
        print 'NAMESPACE FIXUP: %s (%s)' % (module, mname)
    for name in names:
        _fixup_namespace_item(module, mname, name, old_modules, '')

    # take care of removing the module filenames
    for filename in old_modules:
        # skip names in keep modules
        if filename in keep_modules:
            continue
        try:
            delattr(module, filename)
            if FIXUP_DEBUG:
                print 'NAMESPACE FIXUP: deleting %s from %s' % (filename, module)
        except AttributeError:
            # if the name is not there, we are in a reload, so do not
            # do anything
            pass

def _fixup_namespace_item(parent, mname, name, old_modules, path):
    try:
        item = getattr(parent, name)
    except AttributeError:
        if name.startswith('__'): # those sometimes fail unexplicably
            return
        else:
            raise
    current_name = getattr(item, '__module__', None)
    if (current_name is not None and
        _without_prefix(current_name, mname + '.') in old_modules):
        if FIXUP_DEBUG:
            print 'namespace fixup: {%s => %s}%s.%s' % (
                current_name, mname, path, name)
        try:
            item.__module__ = mname
        except AttributeError:
            try:
                item.im_func.__module__ = mname
            except AttributeError, e:
                if FIXUP_DEBUG:
                    print 'namespace fixup failed: ', e
            # don't recurse into functions anyway
            return
        subitems = [_name for _name in dir(item)
                    if _name.startswith('__') or not _name.startswith('_')]
        for subitem in subitems:
            _fixup_namespace_item(item, mname, subitem, old_modules,
                                  path + '.' + name)

fixup_namespace(__name__, __all__,
                ('routines',
                 'introspection',
                 'quad_forms',
                 'covariance',
                 'progress_bar',
                 'slideshow',
                 '_ordered_dict',
                 'templet',
                 'temporarydir',
                 'os',
                 ))
