# This is a backport of tempfile.TemporaryDirectory from Python 3.2

import os as _os
from tempfile import mkdtemp
import errno

template = "tmp"

class TemporaryDirectory(object):
    """Create and return a temporary directory.  This has the same
    behavior as mkdtemp but can be used as a context manager.  For
    example:

        with TemporaryDirectory() as tmpdir:
            ...

    Upon exiting the context, the directory and everthing contained
    in it are removed.
    """

    def __init__(self, suffix="", prefix=template, dir=None):
        self._closed = False
        self._ENOENT = errno.ENOENT
        self.name = None # Handle mkdtemp throwing an exception
        self.name = mkdtemp(suffix, prefix, dir)

    def __repr__(self):
        return "<%s %r>" % (self.__class__.__name__, self.name)

    def __enter__(self):
        return self.name

    def cleanup(self, _warn=False):
        if self.name and not self._closed:
            try:
                self._rmtree(self.name)
            except (TypeError, AttributeError), ex:
                # Issue #10188: Emit a warning on stderr
                # if the directory could not be cleaned
                # up due to missing globals
                if "None" not in str(ex):
                    raise
                return
            except OSError, ex:
                # ignore if the directory has been deleted already
                if ex.errno != self._ENOENT:
                    raise
            self._closed = True

    def __exit__(self, exc, value, tb):
        self.cleanup()

    def __del__(self):
        self.cleanup()

    # XXX (ncoghlan): The following code attempts to make
    # this class tolerant of the module nulling out process
    # that happens during CPython interpreter shutdown
    # Alas, it doesn't actually manage it. See issue #10188
    _listdir = staticmethod(_os.listdir)
    _path_join = staticmethod(_os.path.join)
    _isdir = staticmethod(_os.path.isdir)
    _remove = staticmethod(_os.remove)
    _rmdir = staticmethod(_os.rmdir)
    _os_error = _os.error

    def _rmtree(self, path):
        # Essentially a stripped down version of shutil.rmtree.  We can't
        # use globals because they may be None'ed out at shutdown.
        for name in self._listdir(path):
            fullname = self._path_join(path, name)
            try:
                isdir = self._isdir(fullname)
            except self._os_error:
                isdir = False
            if isdir:
                self._rmtree(fullname)
            else:
                try:
                    self._remove(fullname)
                except self._os_error:
                    pass
        try:
            self._rmdir(path)
        except self._os_error:
            pass
