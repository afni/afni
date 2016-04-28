import mdp
from mdp import PreserveDimNode, numx, VariadicCumulator
import operator


class ClassifierNode(PreserveDimNode):
    """A ClassifierNode can be used for classification tasks that should not
    interfere with the normal execution flow. A reason for that is that the
    labels used for classification do not form a vector space, and so they don't
    make much sense in a flow.
    """
    
    def __init__(self, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        """Initialize classifier.
        
        execute_method -- Set to string value 'label', 'rank', or 'prob' to
            force the corresponding classification method being used instead
            of the standard identity execution (which is used when
            execute_method has the default value None). This can be used when
            the node is last in a flow, the return value from Flow.execute
            will then consist of the classification results.
        """
        self.execute_method = execute_method
        super(ClassifierNode, self).__init__(input_dim=input_dim,
                                             output_dim=output_dim,
                                             dtype=dtype)

    ### Methods to be implemented by the subclasses

    def _label(self, x, *args, **kargs):
        raise NotImplementedError

    def _prob(self, x, *args, **kargs):
        raise NotImplementedError

    ### User interface to the overwritten methods

    def label(self, x, *args, **kwargs):
        """Returns an array with best class labels.

        By default, subclasses should overwrite _label to implement
        their label. The docstring of the '_label' method
        overwrites this docstring.
        """
        self._pre_execution_checks(x)
        return self._label(self._refcast(x), *args, **kwargs)

    def prob(self, x, *args, **kwargs):
        """Returns the probability for each datapoint and label
        (e.g., [{1:0.1, 2:0.0, 3:0.9}, {1:1.0, 2:0.0, 3:0.0}, ...])

        By default, subclasses should overwrite _prob to implement
        their prob. The docstring of the '_prob' method
        overwrites this docstring.
        """
        self._pre_execution_checks(x)
        return self._prob(self._refcast(x), *args, **kwargs)

    def rank(self, x, threshold=None):
        """Returns ordered list with all labels ordered according to prob(x)
        (e.g., [[3 1 2], [2 1 3], ...]).

        The optional threshold parameter is used to exclude labels having equal
        or less probability. E.g. threshold=0 excludes all labels with zero
        probability.
        """
        all_ranking = []
        prob = self.prob(x)
        for p in prob:
            if threshold is None:
                ranking = p.items()
            else:
                ranking = ((k, v) for k, v in p.items() if v > threshold)
            result = [k for k, v in
                      sorted(ranking, key=operator.itemgetter(1), reverse=True)]
            all_ranking.append(result)
        return all_ranking
    
    def _execute(self, x):
        if not self.execute_method:
            return x
        elif self.execute_method == "label":
            return self.label(x)
        elif self.execute_method == "rank":
            return self.rank(x)
        elif self.execute_method == "prob":
            return self.prob(x)

# XXX are the _train and _stop_training functions necessary anymore?
class ClassifierCumulator(VariadicCumulator('data', 'labels'), ClassifierNode):
    """A ClassifierCumulator is a Node whose training phase simply collects
    all input data and labels. In this way it is possible to easily implement
    batch-mode learning.

    The data is accessible in the attribute 'self.data' after
    the beginning of the '_stop_training' phase. 'self.tlen' contains
    the number of data points collected.
    'self.labels' contains the assigned label to each data point.
    """

    def __init__(self, input_dim=None, output_dim=None, dtype=None):
        super(ClassifierCumulator, self).__init__(input_dim=input_dim,
                                                  output_dim=output_dim,
                                                  dtype=dtype)

    def _check_train_args(self, x, labels):
        super(ClassifierCumulator, self)._check_train_args(x, labels)
        if (isinstance(labels, (list, tuple, numx.ndarray)) and
            len(labels) != x.shape[0]):
            msg = ("The number of labels must be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)

    def _train(self, x, labels):
        """Cumulate all input data in a one dimensional list."""
        self.tlen += x.shape[0]
        self.data.extend(x.ravel().tolist())

        # if labels is a number, all x's belong to the same class
        if isinstance(labels, (list, tuple, numx.ndarray)):
            pass
        else:
            labels = [labels] * x.shape[0]

        self.labels.extend(labels.ravel().tolist())

    def _stop_training(self, *args, **kwargs):
        """Transform the data and labels lists to array objects and reshape them."""
        self.data = numx.array(self.data, dtype=self.dtype)
        self.data.shape = (self.tlen, self.input_dim)
        self.labels = numx.array(self.labels)
        self.labels.shape = (self.tlen)

