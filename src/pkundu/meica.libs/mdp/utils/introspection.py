import types
import cPickle
import mdp

class _Walk(object):
    """Recursively crawl an object and search for attributes that
    are reference to numpy arrays, return a dictionary:
    {attribute_name: array_reference}.

    Usage:
        _Walk()(object)
    """
    def __init__(self):
        self.arrays = {}
        self.start = None
        self.allobjs = {}

    def __call__(self, x, start = None):
        arrays = self.arrays
        # loop through the object dictionary
        for name in dir(x):
            # get the corresponding member
            obj = getattr(x, name)
            if id(obj) in self.allobjs.keys():
                # if we already examined the member, skip to the next
                continue
            else:
                # add the id of this object to the list of know members
                self.allobjs[id(obj)] = None

            if start is None:
                # initialize a string structure to keep track of array names
                struct = name
            else:
                # struct is x.y.z (where x and y are objects and z an array)
                struct = '.'.join((start, name))

            if isinstance(obj, mdp.numx.ndarray):
                # the present member is an array
                # add it to the dictionary of all arrays
                if start is not None:
                    arrays[struct] = obj
                else:
                    arrays[name] = obj
            elif name.startswith('__') or type(obj) in (int, long, float,
                                                        types.MethodType):
                # the present member is a private member or a known
                # type that does not support arrays as attributes
                # Note: this is to avoid infinite
                # recursion in python2.6. Just remove the "or type in ..."
                # condition to see the error. There must be a better way.
                continue
            else:
                # we need to examine the present member in more detail
                arrays.update(self(obj, start = struct))
        self.start = start
        return arrays

def _format_dig(dict_):
    longest_name = max(map(len, dict_.keys()))
    longest_size = max(map(lambda x: len('%d'%x[0]), dict_.values()))
    msgs = []
    total_size = 0
    for name in sorted(dict_.keys()):
        size = dict_[name][0]
        total_size += size
        pname = (name+':').ljust(longest_name+1)
        psize = ('%d bytes' % size).rjust(longest_size+6)
        msg = "%s %s" % (pname, psize)
        msgs.append(msg)
    final = "Total %d arrays (%d bytes)" % (len(dict_), total_size)
    msgs.append(final)
    return '\n'.join(msgs)

def dig_node(x):
    """Crawl recursively an MDP Node looking for arrays.

    Return (dictionary, string), where the dictionary is:
    { attribute_name: (size_in_bytes, array_reference)}
    and string is a nice string representation of it.
    """
    if not isinstance(x, mdp.Node):
        raise Exception('Cannot dig %s' % (str(type(x))))
    arrays = _Walk()(x)
    for name in arrays.keys():
        ar = arrays[name]
        if len(ar.shape) == 0:
            size = 1
        else:
            size = mdp.numx.prod(ar.shape)
        bytes = ar.itemsize*size
        arrays[name] = (bytes, ar)
    return arrays, _format_dig(arrays)

def get_node_size(x):
    """Return node total byte-size using cPickle with protocol=2.

    The byte-size is related to the memory needed by the node).
    """
    # TODO: add check for problematic node types, like NoiseNode?
    # TODO: replace this with sys.getsizeof for Python >= 2.6
    size = len(cPickle.dumps(x, protocol = 2))
    return size

def get_node_size_str(x, si_units=False):
    """Return node total byte-size as a well readable string.

    si_units -- If True si-units like KB are used instead of KiB.

    The get_node_size function is used to get the size.
    """
    return _memory_size_str(get_node_size(x), si_units=si_units)

_SI_MEMORY_PREFIXES = ("", "k", "M", "G", "T", "P", "E")
_IEC_MEMORY_PREFIXES = ("", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei")

def _memory_size_str(size, si_units=False):
    """Convert the given memory size into a nicely formatted string.

    si_units -- If True si-units like kB are used instead of kiB.
    """
    if si_units:
        base = 10**3
    else:
        base = 2**10
    scale = 0  # 1024**scale is the actual scale
    while size > base**(scale+1):
        scale += 1
    unit = "B"
    if scale:
        size_str = size = "%.1f" % (1.0 * size / (base**scale))
        if si_units:
            unit = _SI_MEMORY_PREFIXES[scale] + unit
        else:
            unit = _IEC_MEMORY_PREFIXES[scale] + unit
    else:
        size_str = "%d" % size
    return size_str + " " + unit
