"""General routines and base classes for Support Vector Machine classifiers.

TODO: Implement some scaling. Either by special Scaling Node or internally.
"""

import mdp
from mdp import ClassifierCumulator
from itertools import count

class _LabelNormalizer(object):
    """This class provides a transparent mapping from arbitrary labels
    to a set of well-defined integers.

    TODO: This could actually be a node.
    TODO: Needs more refinement. E.g. could automatically round labels to +1, -1
    """
    def __init__(self, labels, mode=None):
        if mode is None:
            mode = "id"
        if mode == "id":
            # don't do anything.
            self.normalize = self._id
            self.revert = self._id
            return

        self._mode = mode
        self._labels = set(labels)
        self._mapping = {}
        self._inverse = {}
        if mode == "dual":
            if len(self._labels) > 2:
                msg = "In dual mode only two labels can be given"
                raise mdp.NodeException(msg)
            t_label_norm = zip(self._labels, [1, -1])
            self._set_label_dicts(t_label_norm)
        elif mode == "multi":
            # enumerate from zero to len
            t_label_norm = zip(self._labels, count())
            self._set_label_dicts(t_label_norm)
        else:
            msg = "Remapping mode not known"
            raise mdp.NodeException(msg)

    def _set_label_dicts(self, t_label_norm):
        self._mapping = dict(t_label_norm)
        self._inverse = dict((norm, label) for label, norm in t_label_norm)

        # check that neither original nor normalised labels have occured more than once
        if not (len(self._mapping) == len(t_label_norm) == len(self._inverse)):
            msg = "Error in label normalisation."
            raise mdp.NodeException(msg)

    def normalize(self, labels):
        return map(self._mapping.get, labels)

    def revert(self, norm_labels):
        return map(self._inverse.get, norm_labels)

    def _id(self, labels):
        return labels


class _SVMClassifier(ClassifierCumulator):
    """Base class for the SVM classifier nodes."""

    def __init__(self, input_dim=None, output_dim=None, dtype=None):
        self.normalizer = None
        super(_SVMClassifier, self).__init__(input_dim=input_dim,
                                             output_dim=output_dim,
                                             dtype=dtype)

    @staticmethod
    def is_invertible():
        return False

