__docformat__ = "restructuredtext en"

import mdp
from mdp import numx

from svm_classifiers import _SVMClassifier, _LabelNormalizer

import svmutil as libsvmutil

class LibSVMClassifier(_SVMClassifier):
    """
    The ``LibSVMClassifier`` class acts as a wrapper around the LibSVM library
    for support vector machines.

    Information to the parameters can be found on
    http://www.csie.ntu.edu.tw/~cjlin/libsvm/
    
    The class provides access to change kernel and svm type with a text string.
    
    Additionally ``self.parameter`` is exposed which allows to change all other
    svm parameters directly.

    This node depends on ``libsvm``.
    """
    # The kernels and classifiers which LibSVM allows.
    kernels = ["RBF", "LINEAR", "POLY", "SIGMOID"]
    classifiers = ["C_SVC", "NU_SVC", "ONE_CLASS", "EPSILON_SVR", "NU_SVR"]

    def __init__(self, kernel=None, classifier=None, probability=True, params=None,
                 input_dim=None, output_dim=None, dtype=None):
        """
        kernel -- The kernel to use
        classifier -- The type of the SVM
        params -- a dict of parameters to be passed to the svm_parameter
        probability -- Must be set to True, if algorithms based on probability
                       shall be used.
        """
        if not params:
            params = {}
        
        # initialise the parameter and be quiet
        self.parameter = libsvmutil.svm_parameter("-q")
        if probability:
            # allow for probability estimates
            self.parameter.probability = 1
        
        super(LibSVMClassifier, self).__init__(input_dim=input_dim,
                                               output_dim=output_dim,
                                               dtype=dtype)
        if kernel:
            self.set_kernel(kernel)
        if classifier:
            self.set_classifier(classifier)
        # set all other parameters
        for k, v in params.iteritems():
            if not k in self.parameter._names:
                # check that the name is a valid parameter
                msg = "'{}' is not a valid parameter for libsvm".format(k)
                raise mdp.NodeException(msg)
                
            if hasattr(self.parameter, k):
                setattr(self.parameter, k, v)
            else:
                msg = "'svm_parameter' has no attribute {}".format(k)
                raise AttributeError(msg)
            

    def _get_supported_dtypes(self):
        """Return the list of dtypes selfupported by this node."""
        # Support only float64 because of external library
        return ('float64',)

    def set_classifier(self, classifier):
        """
        Sets the classifier.

        classifier -- A string with the name of the classifier which
                      should be used. Possible values are in
                      self.classifiers
        """
        if classifier.upper() in self.classifiers:
            self.parameter.svm_type = getattr(libsvmutil, classifier.upper())
        else:
            msg = "Classifier Type %s is unknown or not supported." % classifier
            raise TypeError(msg)

    def set_kernel(self, kernel):
        """
        Sets the kernel.

        kernel     -- A string with the name of the classifier which
                      should be used. Possible values are in
                      self.kernels
        """
        if kernel.upper() in self.kernels:
            self.parameter.kernel_type = getattr(libsvmutil, kernel.upper())
        else:
            msg = "Kernel Type %s is unknown or not supported." % kernel
            raise TypeError(msg)

    def _stop_training(self):
        super(LibSVMClassifier, self)._stop_training()
        self.normalizer = _LabelNormalizer(self.labels)
                
        labels = self.normalizer.normalize(self.labels.tolist())
        features = self.data

        # Call svm training method.
        prob = libsvmutil.svm_problem(labels, features.tolist())
        # Train
        self.model = libsvmutil.svm_train(prob, self.parameter)

    def _label(self, x):
        if isinstance(x, (list, tuple, numx.ndarray)):
            y = [0] * len(x)
            p_labs, p_acc, p_vals = libsvmutil.svm_predict(y, x.tolist(), self.model)
            
            return numx.array(p_labs)
        else:
            msg = "Data must be a sequence of vectors"
            raise mdp.NodeException(msg)

    def predict_probability(self, x):
        self._pre_execution_checks(x)
        if isinstance(x, (list, tuple, numx.ndarray)):
            return self._prob(x)
        else:
            return self._prob([x])

    def _prob(self, x):
        y = [0] * len(x)
        p_labs, p_acc, p_vals = libsvmutil.svm_predict(y, x.tolist(), self.model, "-b 1")
        labels = self.model.get_labels()
        return [dict(zip(labels, ps)) for ps in p_vals]

    def _train(self, x, labels):
        super(LibSVMClassifier, self)._train(x, labels)
