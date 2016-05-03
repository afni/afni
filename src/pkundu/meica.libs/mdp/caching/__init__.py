from caching_extension import (activate_caching, deactivate_caching,
                               cache, set_cachedir,
                               __doc__, __docformat__)

from mdp.utils import fixup_namespace

__all__ = ['activate_caching', 'deactivate_caching',
           'cache', 'set_cachedir']

fixup_namespace(__name__, __all__,('caching_extension','fixup_namespace',))
