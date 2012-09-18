import mdp

def pca(x, **kwargs):
    """Filters multidimensioanl input data through its principal components.

    Observations of the same variable are stored on rows, different variables
    are stored on columns.

    This is a shortcut function for the corresponding node `nodes.PCANode`. If any
    keyword arguments are specified, they are passed to its constructor.

    This is equivalent to ``mdp.nodes.PCANode(**kwargs)(x)``
    """
    return mdp.nodes.PCANode(**kwargs)(x)

def fastica(x, **kwargs):
    """Perform Independent Component Analysis on input data using the FastICA
    algorithm by Aapo Hyvarinen.

    Observations of the same variable are stored on rows, different variables
    are stored on columns.

    This is a shortcut function for the corresponding node `nodes.FastICANode`.
    If any keyword arguments are specified, they are passed to its constructor.

    This is equivalent to ``mdp.nodes.FastICANode(**kwargs)(x)``
    """
    return mdp.nodes.FastICANode(**kwargs)(x)
