from __future__ import with_statement
import sys
import os
import tempfile
import inspect
import mdp
from repo_revision import get_git_revision
import cStringIO as StringIO


__docformat__ = "restructuredtext en"

class MetaConfig(type):
    """Meta class for config object to allow for pretty printing
    of class config (as we never instantiate it)"""
    def __str__(self):
        return self.info()

    def __repr__(self):
        return self.info()

class config(object):
    """Provide information about optional dependencies.

    This class should not be instantiated, it serves as a namespace
    for dependency information. This information is encoded as a
    series of attributes called ``has_<dependency>``.

    Dependency parameters are object which have a a boolean value
    (``True`` if the dependency is available). If False, they contain an
    error string which will be used in ``mdp.config.info()`` output. If
    ``True``, they contain information about the available version of
    the dependency. Those objects should be created by using the helper
    class methods `ExternalDepFound` and `ExternalDepFailed`.

    >>> bool(config.has_python)
    True

    Dependency parameters are numbered in the order of creation,
    so the output is predictable.

    The selection of the numerical backend (`numpy` or `scipy`) can be
    forced by setting the environment variable MDPNUMX.  The loading
    of an optional dependency can be inhibited by setting the
    environment variables ``MDP_DISABLE_<DEPNAME>`` to a non-empty
    value.

    The following variables are defined:
      ``MDPNUMX``
        either ``numpy`` or ``scipy``. By default the latter is used
        if available.
      ``MDP_DISABLE_PARALLEL_PYTHON``
        inhibit loading of `mdp.parallel` based on parallel python
        (module ``pp``)
      ``MDP_DISABLE_SHOGUN``
        inhibit loading of the shogun classifier
      ``MDP_DISABLE_LIBSVM``
        inhibit loading of the svm classifier
      ``MDP_DISABLE_JOBLIB``
        inhibit loading of the ``joblib`` module and `mdp.caching`
      ``MDP_DISABLE_SKLEARN``
        inhibit loading of the ``sklearn`` module
      ``MDPNSDEBUG``
        print debugging information during the import process
      ``MDP_PP_SECRET``
        set parallel python (pp) secret. If not set, and no secret is known
        to pp, a default secret will be used.
      ``MDP_DISABLE_MONKEYPATCH_PP``
        disable automatic monkeypatching of parallel python worker script,
        otherwise a work around for debian bug #620551 is activated.
    """

    __metaclass__ = MetaConfig

    _HAS_NUMBER = 0

    class _ExternalDep(object):
        def __init__(self, name, version=None, failmsg=None):
            assert (version is not None) + (failmsg is not None) == 1

            self.version = str(version) # convert e.g. exception to str
            self.failmsg = str(failmsg) if failmsg is not None else None

            global config
            self.order = config._HAS_NUMBER
            config._HAS_NUMBER += 1
            setattr(config, 'has_' + name, self)

        def __nonzero__(self):
            return self.failmsg is None

        def __repr__(self):
            if self:
                return self.version
            else:
                return "NOT AVAILABLE: " + self.failmsg

    @classmethod
    def ExternalDepFailed(cls, name, failmsg):
        """Inform that an optional dependency was not found.

        A new `_ExternalDep` object will be created and stored
        in `config`.

        :Parameters:
          name
            identifier of the optional dependency. This should
            be a valid python identifier, because it will be
            accessible as ``mdp.config.has_<name>`` attribute.
          failmsg
            an object convertible to ``str``, which will be displayed in
            ``mdp.config.info()`` output. This will usually be either an
            exception (e.g. ``ImportError``), or a message string.
        """
        return cls._ExternalDep(name, failmsg=failmsg)

    @classmethod
    def ExternalDepFound(cls, name, version):
        """Inform that an optional dependency was found.

        A new `_ExternalDep` object will be created and stored
        in `config`.

        :Parameters:
          name
            identifier of the optional dependency. This should
            be a valid python identifier, because it will be
            accessible as ``mdp.config.has_<name>`` attribute.
          version
            an object convertible to ``str``, which will be displayed in
            ``mdp.config.info()`` output. Something like ``'0.4.3'``.
        """
        return cls._ExternalDep(name, version=version)

    @classmethod
    def info(cls):
        """Return nicely formatted info about MDP.

        >>> print mdp.config.info()                           # doctest: +SKIP
                  python: 2.7.2.final.0
                     mdp: 3.3, MDP-3.2-9-g4bc7356+
         parallel python: 1.6.1-monkey-patched
                  shogun: v1.1.0_02ce3cd_2011-12-12_08:17_
                  libsvm: libsvm.so.3
                  joblib: 0.5.4
                 sklearn: 0.9
                    numx: scipy 0.9.0
                  symeig: scipy.linalg.eigh

        This function is used to provide the py.test report header and
        footer.
        """
        listable_features = [(f[4:].replace('_', ' '), getattr(cls, f))
                             for f in dir(cls) if f.startswith('has_')]
        maxlen = max(len(f[0]) for f in listable_features)
        listable_features = sorted(listable_features, key=lambda f: f[1].order)
        return '\n'.join('%*s: %r' % (maxlen+1, f[0], f[1])
                         for f in listable_features)

def get_numx():
    # find out the numerical extension
    # To force MDP to use one specific extension module
    # set the environment variable MDPNUMX
    # Mainly useful for testing
    USR_LABEL = os.getenv('MDPNUMX')

    # check if the variable is properly set
    if USR_LABEL and USR_LABEL not in ('numpy', 'scipy'):
        err = ("Numerical backend '%s'" % USR_LABEL +
               "not supported. Supported backends: numpy, scipy.")
        raise ImportError(err)

    numx_description = None
    numx_exception = {}

    # if variable is not set or the user wants scipy
    if USR_LABEL is None or USR_LABEL == 'scipy':
        try:
            import scipy as numx
            from scipy import (linalg as numx_linalg,
                               fftpack as numx_fft,
                               random as numx_rand,
                               version as numx_version)
            numx_description = 'scipy'
            config.ExternalDepFound('numx', 'scipy ' + numx_version.version)
        except ImportError, exc:
            if USR_LABEL:
                raise ImportError(exc)
            else:
                numx_exception['scipy'] = exc

    # if the user wants numpy or scipy was not available
    if USR_LABEL == 'numpy' or numx_description is None:
        try:
            import numpy as numx
            from numpy import (linalg as numx_linalg,
                               fft as numx_fft,
                               random as numx_rand,
                               version as numx_version)
            numx_description = 'numpy'
            config.ExternalDepFound('numx', 'numpy ' + numx_version.version)
        except ImportError, exc:
            config.ExternalDepFailed('numx', exc)
            numx_exception['numpy'] = exc

    # fail if neither scipy nor numpy could be imported
    # the test is for numx_description, not numx, because numx could
    # be imported successfully, but e.g. numx_rand could later fail.
    if numx_description is None:
        msg = ([ "Could not import any of the numeric backends.",
                 "Import errors:" ] +
               [ lab+': '+str(exc) for lab, exc in numx_exception.items() ]
               + ["sys.path: " + str(sys.path)])
        raise ImportError('\n'.join(msg))

    return (numx_description, numx, numx_linalg,
            numx_fft, numx_rand, numx_version)

def get_symeig(numx_linalg):
    # if we have scipy, check if the version of
    # scipy.linalg.eigh supports the rich interface
    args = inspect.getargspec(numx_linalg.eigh)[0]
    if len(args) > 4:
        # if yes, just wrap it
        from utils._symeig import wrap_eigh as symeig
        config.ExternalDepFound('symeig', 'scipy.linalg.eigh')
    else:
        # either we have numpy, or we have an old scipy
        # we need to use our own rich wrapper
        from utils._symeig import _symeig_fake as symeig
        config.ExternalDepFound('symeig', 'symeig_fake')
    return symeig

def _version_too_old(version, known_good):
    """Return True iff a version is smaller than a tuple of integers.

    This method will return True only if the version string can
    confidently be said to be smaller than ``known_good``. If
    the string cannot be parsed as dot-separated-integers, ``None``
    (which is false) will be returned.

    The comparison is performed part by part, the first non-equal
    one wins.

    >>> _version_too_old('0.4.3', (0,4,3))
    False
    >>> _version_too_old('0.4.2', (0,4,3))
    True
    >>> _version_too_old('0.5.devel', (0,4,3))
    False
    >>> _version_too_old('0.4.devel', (0,4,3))
    """
    for part,expected in zip(version.split('.'), known_good):
        try:
            p = int(part)
        except ValueError:
            return None
        if p < expected:
            return True
        if p > expected:
            break
    return False

class _sys_stdout_replaced(object):
    "Replace systdout temporarily"
    def __enter__(self):
        self.sysstdout = sys.stdout
        sys.stdout = StringIO.StringIO()
        return sys.stdout
    def __exit__(self, *args):
        sys.stdout = self.sysstdout

def _pp_needs_monkeypatching():
    # only run this function the first time mdp is imported
    # otherwise reload(mdp) breaks

    if not hasattr(mdp, '_pp_needs_monkeypatching'):
        # check if we are on one of those broken system were
        # parallel python is affected by
        # http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=620551
        # this is a minimal example to reproduce the problem
        # XXX IMPORTANT XXX
        # This function only works once, i.e. at import
        # if you attempt to call it again afterwards,
        # it does not work [pp does not print the error twice]

        # we need to hijack stdout here, because pp does not raise
        # exceptions: it writes to stdout directly!!!

        # pp stuff
        import pp
        server = pp.Server()
        with _sys_stdout_replaced() as capture:
            server.submit(lambda: None, (), (), ('numpy',))()
            server.destroy()

        # read error from hijacked stdout
        error = capture.getvalue()
        mdp._pp_needs_monkeypatching = 'ImportError' in error 
        
    return mdp._pp_needs_monkeypatching

def set_configuration():
    # set python version
    config.ExternalDepFound('python', '.'.join([str(x)
                                                for x in sys.version_info]))
    version = mdp.__version__
    if mdp.__revision__:
        version += ', ' + mdp.__revision__
    config.ExternalDepFound('mdp', version)

    # parallel python dependency
    try:
        import pp
        # set pp secret if not there already
        # (workaround for debian patch to pp that disables pp's default password)
        pp_secret = os.getenv('MDP_PP_SECRET') or 'mdp-pp-support-password'
        # module 'user' has been deprecated since python 2.6 and deleted
        # completely as of python 3.0.
        # Basically pp can not work on python 3 at the moment.
        import user
        if not hasattr(user, 'pp_secret'):
            user.pp_secret = pp_secret
    except ImportError, exc:
        config.ExternalDepFailed('parallel_python', exc)
    else:
        if os.getenv('MDP_DISABLE_PARALLEL_PYTHON'):
            config.ExternalDepFailed('parallel_python', 'disabled')
        else:
            if _pp_needs_monkeypatching():
                if os.getenv('MDP_DISABLE_MONKEYPATCH_PP'):
                    config.ExternalDepFailed('parallel_python', pp.version +
                                             ' broken on Debian')
                else:
                    config.ExternalDepFound('parallel_python', pp.version +
                                            '-monkey-patched')
                    config.pp_monkeypatch_dirname = tempfile.gettempdir()
            else:
                config.ExternalDepFound('parallel_python', pp.version)

    # shogun
    try:
        import shogun
        from shogun import (Kernel as sgKernel,
                            Features as sgFeatures,
                            Classifier as sgClassifier)
    except ImportError, exc:
        config.ExternalDepFailed('shogun', exc)
    else:
        if os.getenv('MDP_DISABLE_SHOGUN'):
            config.ExternalDepFailed('shogun', 'disabled')
        else:
            # From now on just support shogun >= 1.0
            # Between 0.10 to 1.0 there are too many API changes...
            try:
                version = sgKernel.Version_get_version_release()
            except AttributeError:
                config.ExternalDepFailed('shogun',
                                         'too old, upgrade to at least version 1.0')
            else:
                if not version.startswith('v1.'):
                    config.ExternalDepFailed('shogun',
                                             'too old, upgrade to at least version 1.0.')
                else:
                    config.ExternalDepFound('shogun', version)

    # libsvm
    try:
        import svm as libsvm
        libsvm.libsvm
    except ImportError, exc:
        config.ExternalDepFailed('libsvm', exc)
    except AttributeError, exc:
        config.ExternalDepFailed('libsvm', 'libsvm version >= 2.91 required')
    else:
        if os.getenv('MDP_DISABLE_LIBSVM'):
            config.ExternalDepFailed('libsvm', 'disabled')
        else:
            config.ExternalDepFound('libsvm', libsvm.libsvm._name)

    # joblib
    try:
        import joblib
    except ImportError, exc:
        config.ExternalDepFailed('joblib', exc)
    else:
        version = joblib.__version__
        if os.getenv('MDP_DISABLE_JOBLIB'):
            config.ExternalDepFailed('joblib', 'disabled')
        elif _version_too_old(version, (0,4,3)):
            config.ExternalDepFailed('joblib',
                                     'version %s is too old' % version)
        else:
            config.ExternalDepFound('joblib', version)

    # sklearn
    try:
        try:
            import sklearn
        except ImportError:
            import scikits.learn as sklearn
        version = sklearn.__version__
    except ImportError, exc:
        config.ExternalDepFailed('sklearn', exc)
    except AttributeError, exc:
        config.ExternalDepFailed('sklearn', exc)
    else:   
        if os.getenv('MDP_DISABLE_SKLEARN'):
            config.ExternalDepFailed('sklearn', 'disabled')
        elif _version_too_old(version, (0,6)):
            config.ExternalDepFailed('sklearn',
                                     'version %s is too old' % version)
        else:
            config.ExternalDepFound('sklearn', version)
