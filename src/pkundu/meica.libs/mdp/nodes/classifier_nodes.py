__docformat__ = "restructuredtext en"

import mdp
from mdp import ClassifierNode, utils, numx, numx_rand, numx_linalg

# TODO: The GaussianClassifier and NearestMeanClassifier could be parallelized.


class SignumClassifier(ClassifierNode):
    """This classifier node classifies as ``1`` if the sum of the data points
    is positive and as ``-1`` if the data point is negative"""

    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node."""
        return (mdp.utils.get_dtypes('Float') +
                mdp.utils.get_dtypes('Integer'))

    @staticmethod
    def is_trainable():
        return False

    def _label(self, x):
        ret = [xi.sum() for xi in x]
        return numx.sign(ret)


class PerceptronClassifier(ClassifierNode):
    """A simple perceptron with input_dim input nodes."""
    
    def __init__(self, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        super(PerceptronClassifier, self).__init__(
                                                execute_method=execute_method,
                                                input_dim=input_dim,
                                                output_dim=output_dim,
                                                dtype=dtype)
        self.weights = []
        self.offset_weight = 0
        self.learning_rate = 0.1

    def _check_train_args(self, x, labels):
        if (isinstance(labels, (list, tuple, numx.ndarray)) and
            len(labels) != x.shape[0]):
            msg = ("The number of labels should be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)

        if (not isinstance(labels, (list, tuple, numx.ndarray))):
            labels = [labels]

        if (not numx.all(map(lambda x: abs(x) == 1, labels))):
            msg = "The labels must be either -1 or 1."
            raise mdp.TrainingException(msg)

    def _train(self, x, labels):
        """Update the internal structures according to the input data 'x'.

        x -- a matrix having different variables on different columns
             and observations on the rows.
        labels -- can be a list, tuple or array of labels (one for each data point)
              or a single label, in which case all input data is assigned to
              the same class.
        """

        # if weights are not yet initialised, initialise them
        if not len(self.weights):
            self.weights = numx.ones(self.input_dim)

        for xi, labeli in mdp.utils.izip_stretched(x, labels):
            new_weights = self.weights
            new_offset = self.offset_weight

            rate = self.learning_rate * (labeli - self._label(xi))
            for j in range(self.input_dim):
                new_weights[j] = self.weights[j] + rate * xi[j]

            # the offset corresponds to a node with input 1 all the time
            new_offset = self.offset_weight + rate * 1

            self.weights = new_weights
            self.offset_weight = new_offset

    def _label(self, x):
        """Returns an array with class labels from the perceptron.
        """
        return numx.sign(numx.dot(x, self.weights) + self.offset_weight)


class SimpleMarkovClassifier(ClassifierNode):
    """A simple version of a Markov classifier.
    It can be trained on a vector of tuples the label being the next element
    in the testing data.
    """
    def __init__(self, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        super(SimpleMarkovClassifier, self).__init__(
                                                execute_method=execute_method,
                                                input_dim=input_dim,
                                                output_dim=output_dim,
                                                dtype=dtype)
        self.ntotal_connections = 0

        self.features = {}
        self.labels = {}
        self.connections = {}
        
    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node."""
        return (mdp.utils.get_dtypes('Float') +
                mdp.utils.get_dtypes('AllInteger') +
                mdp.utils.get_dtypes('Character'))

    def _check_train_args(self, x, labels):
        if (isinstance(labels, (list, tuple, numx.ndarray)) and
            len(labels) != x.shape[0]):
            msg = ("The number of labels should be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)

        if (not isinstance(labels, (list, tuple, numx.ndarray))):
            labels = [labels]

    def _train(self, x, labels):
        """Update the internal structures according to the input data 'x'.

        x -- a matrix having different variables on different columns
             and observations on the rows.
        labels -- can be a list, tuple or array of labels (one for each data point)
              or a single label, in which case all input data is assigned to
              the same class.
        """
        # if labels is a number, all x's belong to the same class
        for xi, labeli in mdp.utils.izip_stretched(x, labels):
            self._learn(xi, labeli)

    def _learn(self, feature, label):
        feature = tuple(feature)
        self.ntotal_connections += 1

        if label in self.labels:
            self.labels[label] += 1
        else:
            self.labels[label] = 1

        if feature in self.features:
            self.features[feature] += 1
        else:
            self.features[feature] = 1

        connection = (feature, label)
        if connection in self.connections:
            self.connections[connection] += 1
        else:
            self.connections[connection] = 1

    def _prob(self, features):
        return [self._prob_one(feature) for feature in features]

    def _prob_one(self, feature):
        feature = tuple(feature)
        probabilities = {}

        try:
            n_feature_connections = self.features[feature]
        except KeyError:
            n_feature_connections = 0
            # if n_feature_connections == 0, we get a division by zero
            # we could throw here, but maybe it's best to simply return
            # an empty dict object
            return {}

        for label in self.labels:
            conn = (feature, label)
            try:
                n_conn = self.connections[conn]
            except KeyError:
                n_conn = 0

            try:
                n_label_connections = self.labels[label]
            except KeyError:
                n_label_connections = 0

            p_feature_given_label = 1.0 * n_conn / n_label_connections
            p_label = 1.0 * n_label_connections / self.ntotal_connections
            p_feature = 1.0 * n_feature_connections / self.ntotal_connections
            prob = 1.0 * p_feature_given_label * p_label / p_feature
            probabilities[label] = prob
        return probabilities


class DiscreteHopfieldClassifier(ClassifierNode):
    """Node for simulating a simple discrete Hopfield model"""
    # TODO: It is unclear if this belongs to classifiers or is a general node
    # because label space is a subset of feature space
    def __init__(self, execute_method=None,
                 input_dim=None, output_dim=None, dtype='b'):
        super(DiscreteHopfieldClassifier, self).__init__(
                                            execute_method=execute_method,
                                            input_dim=input_dim,
                                            output_dim=output_dim,
                                            dtype=dtype)
        self._weight_matrix = 0 # assigning zero to ease addition
        self._num_patterns = 0
        self._shuffled_update = True

    def _get_supported_dtypes(self):
        return ['b']

    def _train(self, x):
        """Provide the hopfield net with the possible states.

        x -- a matrix having different variables on different columns
            and observations on rows.
        """
        for pattern in x:
            self._train_one(pattern)

    def _train_one(self, pattern):
        pattern = mdp.utils.bool_to_sign(pattern)
        weights = numx.outer(pattern, pattern)
        self._weight_matrix += weights / float(self.input_dim)
        self._num_patterns += 1

    @property
    def memory_size(self):
        """Returns the Hopfield net's memory size"""
        return self.input_dim

    @property
    def load_parameter(self):
        """Returns the load parameter of the Hopfield net.
        The quality of memory recall for a Hopfield net breaks down when the
        load parameter is larger than 0.14."""
        return self._num_patterns / float(self.input_dim)

    def _stop_training(self):
        # remove self-feedback
        # we could use numx.fill_diagonal, but thats numpy 1.4 only
        for i in range(self.input_dim):
            self._weight_matrix[i][i] = 0

    def _label(self, x, threshold = 0):
        """Retrieves patterns from the associative memory.
        """
        threshold = numx.zeros(self.input_dim) + threshold
        return numx.array([self._label_one(pattern, threshold) for pattern in x])

    def _label_one(self, pattern, threshold):
        pattern = mdp.utils.bool_to_sign(pattern)

        has_converged = False
        while not has_converged:
            has_converged = True
            iter_order = range(len(self._weight_matrix))
            if self._shuffled_update:
                numx_rand.shuffle(iter_order)
            for row in iter_order:
                w_row = self._weight_matrix[row]

                thresh_row = threshold[row]
                new_pattern_row = numx.sign(numx.dot(w_row, pattern) - thresh_row)

                if new_pattern_row == 0:
                    # Following McKay, Neural Networks, we do nothing
                    # when the new pattern is zero
                    pass
                elif pattern[row] != new_pattern_row:
                    has_converged = False
                    pattern[row] = new_pattern_row
        return mdp.utils.sign_to_bool(pattern)

# TODO: Make it more efficient

class KMeansClassifier(ClassifierNode):
    """Employs K-Means Clustering for a given number of centroids."""
    def __init__(self, num_clusters, max_iter=10000, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        """
        :Arguments:
          num_clusters
            number of centroids to use = number of clusters
          max_iter
            if the algorithm does not reach convergence (for some
            numerical reason), stop after ``max_iter`` iterations
        """
        super(KMeansClassifier, self).__init__(execute_method=execute_method,
                                               input_dim=input_dim,
                                               output_dim=output_dim,
                                               dtype=dtype)
        self._num_clusters = num_clusters
        self.data = []
        self.tlen = 0
        self._centroids = None
        self.max_iter = max_iter

    def _train(self, x):
        # append all data
        # we could use a Cumulator class here
        self.tlen += x.shape[0]
        self.data.extend(x.ravel().tolist())

    def _stop_training(self):
        self.data = numx.array(self.data, dtype=self.dtype)
        self.data.shape = (self.tlen, self.input_dim)

        # choose initial centroids unless they are already given
        if not self._centroids:
            import random
            centr_idx = random.sample(xrange(self.tlen), self._num_clusters)
            #numx_rand.permutation(self.tlen)[:self._num_clusters]
            centroids = self.data[centr_idx]
        else:
            centroids = self._centroids

        for step in xrange(self.max_iter):
            # list of (sum_position, num_clusters)
            new_centroids = [(0., 0.)] * len(centroids)
            # cluster
            for x in self.data:
                idx = self._nearest_centroid_idx(x, centroids)
                # update position and count
                pos_count = (new_centroids[idx][0] + x,
                             new_centroids[idx][1] + 1.)
                new_centroids[idx] = pos_count

            # get new centroid position
            new_centroids = numx.array([c[0] / c[1] if c[1]>0. else centroids[idx]
                                        for idx, c in enumerate(new_centroids)])
            # check if we are stable
            if numx.all(new_centroids == centroids):
                self._centroids = centroids
                return
            centroids = new_centroids

    def _nearest_centroid_idx(self, data, centroids):
        dists = numx.array([numx.linalg.norm(data - c) for c in centroids])
        return dists.argmin()

    def _label(self, x):
        """For a set of feature vectors x, this classifier returns
        a list of centroids.
        """
        return [self._nearest_centroid_idx(xi, self._centroids) for xi in x]


class GaussianClassifier(ClassifierNode):
    """Perform a supervised Gaussian classification.

    Given a set of labelled data, the node fits a gaussian distribution
    to each class.
    """
    
    def __init__(self, execute_method=False,
                 input_dim=None, output_dim=None, dtype=None):
        super(GaussianClassifier, self).__init__(execute_method=execute_method,
                                                 input_dim=input_dim,
                                                 output_dim=output_dim,
                                                 dtype=dtype)
        self._cov_objs = {}  # only stored during training
        # this list contains the square root of the determinant of the
        # corresponding covariance matrix
        self._sqrt_def_covs = []
        # we are going to store the inverse of the covariance matrices
        # since only those are useful to compute the probabilities
        self.inv_covs = []
        self.means = []
        self.p = []  # number of observations
        self.labels = None

    @staticmethod
    def is_invertible():
        return False

    def _check_train_args(self, x, labels):
        if isinstance(labels, (list, tuple, numx.ndarray)) and (
            len(labels) != x.shape[0]):
            msg = ("The number of labels should be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)

    def _update_covs(self, x, lbl):
        if lbl not in self._cov_objs:
            self._cov_objs[lbl] = utils.CovarianceMatrix(dtype=self.dtype)
        self._cov_objs[lbl].update(x)

    def _train(self, x, labels):
        """
        :Arguments:
          x
              data
          labels
              Can be a list, tuple or array of labels (one for each data point)
              or a single label, in which case all input data is assigned to
              the same class.
        """
        # if labels is a number, all x's belong to the same class
        if isinstance(labels, (list, tuple, numx.ndarray)):
            labels_ = numx.asarray(labels)
            # get all classes from cl
            for lbl in set(labels_):
                x_lbl = numx.compress(labels_==lbl, x, axis=0)
                self._update_covs(x_lbl, lbl)
        else:
            self._update_covs(x, labels)

    def _stop_training(self):
        self.labels = self._cov_objs.keys()
        self.labels.sort()
        nitems = 0
        for lbl in self.labels:
            cov, mean, p = self._cov_objs[lbl].fix()
            nitems += p
            self._sqrt_def_covs.append(numx.sqrt(numx_linalg.det(cov)))
            if self._sqrt_def_covs[-1] == 0.0:
                err = ("The covariance matrix is singular for at least "
                       "one class.")
                raise mdp.NodeException(err)
            self.means.append(mean)
            self.p.append(p)
            self.inv_covs.append(utils.inv(cov))

        for i in range(len(self.p)):
            self.p[i] /= float(nitems)

        del self._cov_objs

    def _gaussian_prob(self, x, lbl_idx):
        """Return the probability of the data points x with respect to a
        gaussian.

        Input arguments:
        x -- Input data
        S -- Covariance matrix
        mn -- Mean
        """
        x = self._refcast(x)

        dim = self.input_dim
        sqrt_detS = self._sqrt_def_covs[lbl_idx]
        invS = self.inv_covs[lbl_idx]
        # subtract the mean
        x_mn = x - self.means[lbl_idx][numx.newaxis, :]
        # exponent
        exponent = -0.5 * (utils.mult(x_mn, invS)*x_mn).sum(axis=1)
        # constant
        constant = (2.*numx.pi)**(-dim/2.) / sqrt_detS
        # probability
        return constant * numx.exp(exponent)

    def class_probabilities(self, x):
        """Return the posterior probability of each class given the input."""
        self._pre_execution_checks(x)

        # compute the probability for each class
        tmp_prob = numx.zeros((x.shape[0], len(self.labels)),
                              dtype=self.dtype)
        for i in range(len(self.labels)):
            tmp_prob[:, i] = self._gaussian_prob(x, i)
            tmp_prob[:, i] *= self.p[i]

        # normalize to probability 1
        # (not necessary, but sometimes useful)
        tmp_tot = tmp_prob.sum(axis=1)
        tmp_tot = tmp_tot[:, numx.newaxis]
        return tmp_prob / tmp_tot

    def _prob(self, x):
        """Return the posterior probability of each class given the input in a dict."""

        class_prob = self.class_probabilities(x)
        return [dict(zip(self.labels, prob)) for prob in class_prob]

    def _label(self, x):
        """Classify the input data using Maximum A-Posteriori."""

        class_prob = self.class_probabilities(x)
        winner = class_prob.argmax(axis=-1)
        return [self.labels[winner[i]] for i in range(len(winner))]
    
# TODO: Maybe extract some common elements form this class and
#    GaussianClassifier, like in _train.

class NearestMeanClassifier(ClassifierNode):
    """Nearest-Mean classifier."""
    
    def __init__(self, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        super(NearestMeanClassifier, self).__init__(
                                            execute_method=execute_method,
                                            input_dim=input_dim,
                                            output_dim=output_dim,
                                            dtype=dtype)
        self.label_means = {}  # not normalized during training
        self.n_label_samples = {}
        # initialized after training, used for vectorized execution:
        self.ordered_labels = []
        self.ordered_means = None  # will be array
        
    def _train(self, x, labels):
        """Update the mean information for the different classes.
        
        labels -- Can be a list, tuple or array of labels (one for each data
            point) or a single label, in which case all input data is assigned
            to the same class (computationally this is more efficient).
        """
        if isinstance(labels, (list, tuple, numx.ndarray)):
            labels = numx.asarray(labels)
            for label in set(labels):
                x_label = numx.compress(labels==label, x, axis=0)
                self._update_mean(x_label, label)
        else:
            self._update_mean(x, labels)
            
    def _update_mean(self, x, label):
        """Update the mean with data for a single label."""
        if label not in self.label_means:
            self.label_means[label] = numx.zeros(self.input_dim)
            self.n_label_samples[label] = 0
        # TODO: use smarter summing to avoid rounding errors
        self.label_means[label] += numx.sum(x, axis=0)
        self.n_label_samples[label] += len(x)
        
    def _check_train_args(self, x, labels):
        if isinstance(labels, (list, tuple, numx.ndarray)) and (
            len(labels) != x.shape[0]):
            msg = ("The number of labels should be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)
        
    def _stop_training(self):
        """Calculate the class means."""
        ordered_means = [] 
        for label in self.label_means:
            self.label_means[label] /= self.n_label_samples[label]
            self.ordered_labels.append(label)
            ordered_means.append(self.label_means[label])
        self.ordered_means = numx.vstack(ordered_means)
            
    def _label(self, x):
        """Classify the data based on minimal distance to mean."""
        n_labels = len(self.ordered_labels)
        differences = x[:,:,numx.newaxis].repeat(n_labels, 2). \
                        swapaxes(1,2) - self.ordered_means
        square_distances = (differences**2).sum(2)
        label_indices = square_distances.argmin(1)
        labels = [self.ordered_labels[i] for i in label_indices]
        return labels
    
    
class KNNClassifier(ClassifierNode):
    """K-Nearest-Neighbour Classifier."""
    
    def __init__(self, k=1, execute_method=None,
                 input_dim=None, output_dim=None, dtype=None):
        """Initialize classifier.
        
        k -- Number of closest sample points that are taken into account.
        """
        super(KNNClassifier, self).__init__(execute_method=execute_method,
                                            input_dim=input_dim,
                                            output_dim=output_dim,
                                            dtype=dtype)
        self.k = k
        self._label_samples = {}  # temporary variable during training
        self.n_samples = None
        # initialized after training:
        self.samples = None  # 2d array with all samples
        self.sample_label_indices = None  # 1d array for label indices
        self.ordered_labels = []
        
    def _train(self, x, labels):
        """Add the sampel points to the classes.
        
        labels -- Can be a list, tuple or array of labels (one for each data
            point) or a single label, in which case all input data is assigned
            to the same class (computationally this is more efficient).
        """
        if isinstance(labels, (list, tuple, numx.ndarray)):
            labels = numx.asarray(labels)
            for label in set(labels):
                x_label = numx.compress(labels==label, x, axis=0)
                self._add_samples(x_label, label)
        else:
            self._add_samples(x, labels)
    
    def _add_samples(self, x, label):
        """Store x set for later neirest-neighbour calculation."""
        if label not in self._label_samples:
            self._label_samples[label] = []
        self._label_samples[label].append(x)
        
    def _check_train_args(self, x, labels):
        if isinstance(labels, (list, tuple, numx.ndarray)) and (
            len(labels) != x.shape[0]):
            msg = ("The number of labels should be equal to the number of "
                   "datapoints (%d != %d)" % (len(labels), x.shape[0]))
            raise mdp.TrainingException(msg)
        
    def _stop_training(self):
        """Organize the sample data."""
        ordered_samples = []
        for label in self._label_samples:
            ordered_samples.append(
                            numx.concatenate(self._label_samples[label]))
            self.ordered_labels.append(label)
        del self._label_samples
        self.samples = numx.concatenate(ordered_samples)
        self.n_samples = len(self.samples)
        self.sample_label_indices = numx.concatenate(
                                [numx.ones(len(ordered_samples[i]),
                                           dtype="int32") * i
                                 for i in range(len(self.ordered_labels))])

    def _label(self, x):
        """Label the data by comparison with the reference points."""
        square_distances = (x*x).sum(1)[:, numx.newaxis] \
                      + (self.samples*self.samples).sum(1)
        square_distances -= 2 * numx.dot(x, self.samples.T)
        min_inds = square_distances.argsort()
        win_inds = [numx.bincount(self.sample_label_indices[indices[0:self.k]]).
                    argmax(0) for indices in min_inds]
        labels = [self.ordered_labels[i] for i in win_inds]
        return labels
