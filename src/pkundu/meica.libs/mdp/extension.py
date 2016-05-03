"""
Extension Mechanism for nodes.

The extension mechanism makes it possible to dynamically add class attributes,
especially methods, for specific features to node classes
(e.g. nodes need a _fork and _join method for parallelization).
It is also possible for users to define new extensions to provide new
functionality for MDP nodes without having to modify any MDP code.

Without the extension mechanism extending nodes would be done by inheritance,
which is fine unless one wants to use multiple inheritance at the same time
(requiring multiple inheritance for every combination of extensions one wants
to use). The extension mechanism does not depend on inheritance, instead it
adds the methods to the node classes dynamically at runtime. This makes it
possible to activate extensions just when they are needed, reducing the risk
of interference between different extensions.

However, since the extension mechanism provides a special Metaclass it is
still possible to define the extension nodes as classes derived from nodes.
This keeps the code readable and is compatible with automatic code checkers
(like the background pylint checks in the Eclipse IDE with PyDev).
"""

from mdp import MDPException, NodeMetaclass

# TODO: Register the node instances as well?
#    This would allow instance initialization when an extension is activated.
#    Implementing this should not be too hard via the metclass.

# TODO: Add warning about overriding public methods with respect to
#    the docstring wrappers?

# TODO: in the future could use ABC's to register nodes with extension nodes


# name prefix used for the original attributes when they are shadowed
ORIGINAL_ATTR_PREFIX = "_non_extension_"
# prefix used to store the current extension name for an attribute
EXTENSION_ATTR_PREFIX = "_extension_for_"
# list of attribute names that are not affected by extensions,
NON_EXTENSION_ATTRIBUTES = ["__module__", "__doc__", "extension_name"]

# dict of dicts of dicts, contains a key for each extension,
# the inner dict maps the node types to their extension node,
# the innermost dict then maps attribute names to values
# (e.g. a method name to the actual function)
_extensions = dict()
# set containing the names of the currently activated extensions
_active_extensions = set()


class ExtensionException(MDPException):
    """Base class for extension related exceptions."""
    pass


def _register_attribute(ext_name, node_cls, attr_name, attr_value):
    """Register an attribute as an extension attribute.

    ext_name -- String with the name of the extension.
    node_cls -- Node class for which the method should be registered.
    """
    _extensions[ext_name][node_cls][attr_name] = attr_value

def extension_method(ext_name, node_cls, method_name=None):
    """Returns a function to register a function as extension method.

    This function is intended to be used with the decorator syntax.

    :Parameters:
      ext_name
        String with the name of the extension.
      node_cls
        Node class for which the method should be registered.
      method_name
        Name of the extension method (default value is ``None``).

        If no value is provided then the name of the function is used.

    Note that it is possible to directly call other extension functions, call
    extension methods in other node classes or to use super in the normal way
    (the function will be called as a method of the node class).
    """
    def register_function(func):
        _method_name = method_name
        if not _method_name:
            _method_name = func.__name__
        if not ext_name in _extensions:
            err = ("No ExtensionNode base class has been defined for this "
                   "extension.")
            raise ExtensionException(err)
        if not node_cls in _extensions[ext_name]:
            # register this node
            _extensions[ext_name][node_cls] = dict()
        _register_attribute(ext_name, node_cls, _method_name, func)
        return func
    return register_function


class ExtensionNodeMetaclass(NodeMetaclass):
    """This is the metaclass for node extension superclasses.

    It takes care of registering extensions and the attributes in the
    extension.
    """

    def __new__(cls, classname, bases, members):
        """Create new node classes and register extensions.

        If a concrete extension node is created then a corresponding mixin
        class is automatically created and registered.
        """
        if classname == "ExtensionNode":
            # initial creation of ExtensionNode class
            return super(ExtensionNodeMetaclass, cls).__new__(cls, classname,
                                                              bases, members)
        # check if this is a new extension definition,
        # in that case this node is directly derived from ExtensionNode
        if ExtensionNode in bases:
            ext_name = members["extension_name"]
            if not ext_name:
                err = "No extension name has been specified."
                raise ExtensionException(err)
            if ext_name not in _extensions:
                # creation of a new extension, add entry in dict
                _extensions[ext_name] = dict()
            else:
                err = ("An extension with the name '" + ext_name +
                       "' has already been registered.")
                raise ExtensionException(err)
        # find the node that this extension node belongs to
        base_node_cls = None
        for base in bases:
            if type(base) is not ExtensionNodeMetaclass:
                if base_node_cls is None:
                    base_node_cls = base
                else:
                    err = ("Extension node derived from multiple "
                           "normal nodes.")
                    raise ExtensionException(err)
        if base_node_cls is None:
            # This new extension is not directly derived from another class,
            # so there is nothing to register (no default implementation).
            # We disable the doc method extension mechanism as this class
            # is not a node subclass and adding methods (e.g. _execute) would
            # cause problems.
            cls.DOC_METHODS = []
            return super(ExtensionNodeMetaclass, cls).__new__(cls, classname,
                                                              bases, members)
        ext_node_cls = super(ExtensionNodeMetaclass, cls).__new__(
                                                cls, classname, bases, members)
        ext_name = ext_node_cls.extension_name
        if not base_node_cls in _extensions[ext_name]:
            # register the base node
            _extensions[ext_name][base_node_cls] = dict()
        # Register methods from extension class hierarchy: iterate MRO in
        # reverse order and register all attributes starting from the
        # classes which are subclasses from ExtensionNode.
        extension_subtree = False
        for base in reversed(ext_node_cls.__mro__):
            # make sure we only inject methods in classes which have
            # ExtensionNode as superclass
            if extension_subtree and ExtensionNode in base.__mro__:
                for attr_name, attr_value in base.__dict__.items():
                    if attr_name not in NON_EXTENSION_ATTRIBUTES:
                        # check if this attribute has not already been
                        # extended in one of the base classes
                        already_active = False
                        for bb in ext_node_cls.__mro__:
                            if (bb in _extensions[ext_name] and
                            attr_name in _extensions[ext_name][bb] and
                            _extensions[ext_name][bb][attr_name] == attr_value):
                                already_active = True
                        # only register if not yet active
                        if not already_active:
                            _register_attribute(ext_name, base_node_cls,
                                                attr_name, attr_value)
            if base == ExtensionNode:
                extension_subtree = True
        return ext_node_cls


class ExtensionNode(object):
    """Base class for extensions nodes.

    A new extension node class should override the _extension_name.
    The concrete node implementations are then derived from this extension
    node class.

    To call an instance method from a parent class you have multiple options:

    - use super, but with the normal node class, e.g.:

      >>>  super(mdp.nodes.SFA2Node, self).method()      # doctest: +SKIP

      Here SFA2Node was given instead of the extension node class for the
      SFA2Node.

      If the extensions node class is used directly (without the extension
      mechanism) this can cause problems. In that case you have to be
      careful about the inheritance order and the effect on the MRO.

    - call it explicitly using the __func__ attribute [python version < 3]:

      >>> parent_class.method.__func__(self)             # doctest: +SKIP

      or [python version >=3]:

      >>> parent_class.method(self)                      # doctest: +SKIP

    To call the original (pre-extension) method in the same class use you
    simply prefix the method name with '_non_extension_' (this is the value
    of the `ORIGINAL_ATTR_PREFIX` constant in this module).
    """
    __metaclass__ = ExtensionNodeMetaclass
    # override this name in a concrete extension node base class
    extension_name = None


def get_extensions():
    """Return a dictionary currently registered extensions.

    Note that this is not a copy, so if you change anything in this dict
    the whole extension mechanism will be affected. If you just want the
    names of the available extensions use get_extensions().keys().
    """
    return _extensions

def get_active_extensions():
    """Returns a list with the names of the currently activated extensions."""
    # use copy to protect the original set, also important if the return
    # value is used in a for-loop (see deactivate_extensions function)
    return list(_active_extensions)

def activate_extension(extension_name, verbose=False):
    """Activate the extension by injecting the extension methods."""
    if extension_name not in _extensions.keys():
        err = "Unknown extension name: %s" + str(extension_name)
        raise ExtensionException(err)
    if extension_name in _active_extensions:
        if verbose:
            print 'Extension %s is already active!' % extension_name
        return
    _active_extensions.add(extension_name)
    try:
        for node_cls, attributes in _extensions[extension_name].items():
            for attr_name, attr_value in attributes.items():
                if verbose:
                    print ("extension %s: adding %s to %s" %
                           (extension_name, attr_name, node_cls.__name__))
                ## store the original attribute / make it available
                ext_attr_name = EXTENSION_ATTR_PREFIX + attr_name
                if attr_name in dir(node_cls):
                    if ext_attr_name in node_cls.__dict__:
                        # two extensions override the same attribute
                        err = ("Name collision for attribute '" +
                               attr_name + "' between extension '" +
                               getattr(node_cls, ext_attr_name)
                               + "' and newly activated extension '" +
                               extension_name + "'.")
                        raise ExtensionException(err)
                    # only overwrite the attribute if the extension is not
                    # yet active on this class or its superclasses
                    if ext_attr_name not in dir(node_cls):
                        original_attr = getattr(node_cls, attr_name)
                        if verbose:
                            print ("extension %s: overwriting %s in %s" %
                                (extension_name, attr_name, node_cls.__name__))
                        setattr(node_cls, ORIGINAL_ATTR_PREFIX + attr_name,
                                original_attr)
                setattr(node_cls, attr_name, attr_value)
                # store to which extension this attribute belongs, this is also
                # used as a flag that this is an extension attribute
                setattr(node_cls, ext_attr_name, extension_name)
    except:
        # make sure that an incomplete activation is reverted
        deactivate_extension(extension_name)
        raise

def deactivate_extension(extension_name, verbose=False):
    """Deactivate the extension by removing the injected methods."""
    if extension_name not in _extensions.keys():
        err = "Unknown extension name: " + str(extension_name)
        raise ExtensionException(err)
    if extension_name not in _active_extensions:
        return
    for node_cls, attributes in _extensions[extension_name].items():
        for attr_name in attributes.keys():
            original_name = ORIGINAL_ATTR_PREFIX + attr_name
            if verbose:
                print ("extension %s: removing %s from %s" %
                       (extension_name, attr_name, node_cls.__name__))
            if original_name in node_cls.__dict__:
                # restore the original attribute
                if verbose:
                    print ("extension %s: restoring %s in %s" %
                           (extension_name, attr_name, node_cls.__name__))
                delattr(node_cls, attr_name)
                original_attr = getattr(node_cls, original_name)
                # Check if the attribute is defined by one of the super
                # classes and test if the overwritten method is not that
                # method, otherwise we would inject unwanted methods.
                # Note: '==' tests identity for .__func__ and .__self__,
                #    but .im_class does not matter in Python 2.6.
                if all(map(lambda x:getattr(x, attr_name, None) !=
                           original_attr, node_cls.__mro__[1:])):
                    setattr(node_cls, attr_name, original_attr)
                delattr(node_cls, original_name)
            else:
                try:
                    # no original attribute to restore, so simply delete
                    # might be missing if the activation failed
                    delattr(node_cls, attr_name)
                except AttributeError:
                    pass
            try:
                # might be missing if the activation failed
                delattr(node_cls, EXTENSION_ATTR_PREFIX + attr_name)
            except AttributeError:
                pass
    _active_extensions.remove(extension_name)

def activate_extensions(extension_names, verbose=False):
    """Activate all the extensions for the given names.

    extension_names -- Sequence of extension names.
    """
    try:
        for extension_name in extension_names:
            activate_extension(extension_name, verbose=verbose)
    except:
        # if something goes wrong deactivate all, otherwise we might be
        # in an inconsistent state (e.g. methods for active extensions might
        # have been removed)
        deactivate_extensions(get_active_extensions())
        raise

def deactivate_extensions(extension_names, verbose=False):
    """Deactivate all the extensions for the given names.

    extension_names -- Sequence of extension names.
    """
    for extension_name in extension_names:
        deactivate_extension(extension_name, verbose=verbose)

# TODO: add check that only extensions are deactivated that were
#    originally activcated by this extension (same in context manager)
#    also add test for this
def with_extension(extension_name):
    """Return a wrapper function to activate and deactivate the extension.
    
    This function is intended to be used with the decorator syntax.

    The deactivation happens only if the extension was activated by
    the decorator (not if it was already active before). So this
    decorator ensures that the extensions is active and prevents
    unintended side effects.

    If the generated function is a generator, the extension will be in
    effect only when the generator object is created (that is when the
    function is called, but its body is not actually immediately
    executed). When the function body is executed (after ``next`` is
    called on the generator object), the extension might not be in
    effect anymore. Therefore, it is better to use the `extension`
    context manager with a generator function.
    """
    def decorator(func):
        def wrapper(*args, **kwargs):
            # make sure that we don't deactive and extension that was
            # not activated by the decorator (would be a strange sideeffect)
            if extension_name not in get_active_extensions():
                try:
                    activate_extension(extension_name)
                    result = func(*args, **kwargs)
                finally:
                    deactivate_extension(extension_name)
            else:
                result = func(*args, **kwargs)
            return result
        # now make sure that docstring and signature match the original
        func_info = NodeMetaclass._function_infodict(func)
        return NodeMetaclass._wrap_function(wrapper, func_info)
    return decorator

class extension(object):
    """Context manager for MDP extension.
    
    This allows you to use extensions using a ``with`` statement, as in:

    >>> with mdp.extension('extension_name'):
    ...     # 'node' is executed with the extension activated
    ...     node.execute(x)

    It is also possible to activate multiple extensions at once:

    >>> with mdp.extension(['ext1', 'ext2']):
    ...     # 'node' is executed with the two extensions activated
    ...     node.execute(x)
        
    The deactivation at the end happens only for the extensions that were
    activated by this context manager (not for those that were already active
    when the context was entered). This prevents unintended side effects.
    """

    def __init__(self, ext_names):
        if isinstance(ext_names, str):
            ext_names = [ext_names]
        self.ext_names = ext_names
        self.deactivate_exts = []

    def __enter__(self):
        already_active = get_active_extensions()
        self.deactivate_exts = [ext_name for ext_name in self.ext_names
                                if ext_name not in already_active]
        activate_extensions(self.ext_names)

    def __exit__(self, type, value, traceback):
        deactivate_extensions(self.deactivate_exts)
