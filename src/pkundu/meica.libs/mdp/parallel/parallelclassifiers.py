"""
Module for MDP classifiers that support parallel training.
"""

import mdp
from mdp import numx
from mdp.parallel import ParallelExtensionNode


class ParallelGaussianClassifier(ParallelExtensionNode,
                                 mdp.nodes.GaussianClassifier):

    def _fork(self):
        return self._default_fork()

    def _join(self, forked_node):
        if not self._cov_objs:
            self.set_dtype(forked_node._dtype)
            self._cov_objs = forked_node._cov_objs
        else:
            for key, forked_cov in forked_node._cov_objs.items():
                if key in self._cov_objs:
                    self._join_covariance(self._cov_objs[key], forked_cov)
                else:
                    self._cov_objs[key] = forked_cov
                    

class ParallelNearestMeanClassifier(ParallelExtensionNode,
                                    mdp.nodes.NearestMeanClassifier):
    
    def _fork(self):
        return self._default_fork()
    
    def _join(self, forked_node):
        for key in forked_node.label_means:
            if key in self.label_means:
                self.label_means[key] += forked_node.label_means[key]
                self.n_label_samples[key] += forked_node.n_label_samples[key]
            else:
                self.label_means[key] = forked_node.label_means[key]
                self.n_label_samples[key] = forked_node.n_label_samples[key]


class ParallelKNNClassifier(ParallelExtensionNode,
                            mdp.nodes.KNNClassifier):
    
    def _fork(self):
        return self._default_fork()
    
    def _join(self, forked_node):
        for key in forked_node._label_samples:
            if key in self._label_samples:
                self._label_samples[key] += forked_node._label_samples[key]
            else:
                self._label_samples[key] = forked_node._label_samples[key]


