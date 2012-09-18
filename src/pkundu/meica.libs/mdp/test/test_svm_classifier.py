from _tools import *

def _randomly_filled_hypercube(widths, num_elem=1000):
    """Fills a hypercube with given widths, centred at the origin.
    """
    p = []
    for i in xrange(num_elem):
        rand_data = numx_rand.random(len(widths))
        rand_data = [w*(d - 0.5) for d, w in zip(rand_data, widths)]
        p.append(tuple(rand_data))
    return p

def _randomly_filled_hyperball(dim, radius, num_elem=1000):
    """Fills a hyperball with a number of random elements.
    """
    r = numx_rand.random(num_elem)
    points = numx_rand.random((num_elem, dim))
    for i in xrange(len(points)):
        norm = numx.linalg.norm(points[i])
        scale = pow(r[i], 1./dim)
        points[i] = points[i] * radius * scale / norm
    return points

def _random_clusters(positions, radius=1, num_elem=1000):
    """Puts random clusters with num_elem elements at the given positions.
    positions - a list of tuples
    """
    data = []
    for p in positions:
        dim = len(p)
        ball = _randomly_filled_hyperball(dim, radius, num_elem)
        ball = [numx.array(b) + numx.array(p) for b in ball]
        data.append(ball)
    return data

def _separable_data(positions, labels, radius=1, num_elem=1000, shuffled=False):
    """
    For each position, we create num_elem data points in a certain radius around
    that position. If shuffled, we shuffle the output data and labels.
    
    positions -- List of position tuples, e.g. [(1, 1), (-1, -1)]
    labels    -- List of labels, e.g. [1, -1]
    radius    -- The maximum distance to the position
    num_elem  -- The number of elements to be created
    shuffled  -- Should the output be shuffled.
    
    Returns:
      data, labels
    """
    assert len(positions) == len(labels)

    data = numx.vstack( _random_clusters(positions, radius, num_elem) )
    #data = numx.vstack( (numx_rand.random( (num_elem,2) ) - dist,
    #                     numx_rand.random( (num_elem,2) ) + dist) )
    a_labels = numx.hstack(map(lambda x: [x] * num_elem, labels))
    if shuffled:
        ind = range(len(data))
        numx_rand.shuffle(ind)
        return data[ind], a_labels[ind]
    return data, a_labels

def _sqdist(tuple_a, tuple_b):
    return sum( (a-b)**2 for a, b in zip(tuple_a, tuple_b) )

def test_separable_data_is_inside_radius():
    positions = [[(1, 1), (-1, -1)],
                 [(1, 1, 10), (100, -20, 30), (-1, 10, 1000)]]
    labels = [[1, -1], [1, 2, 3]]
    radii = [0.5, 1, 10]
    num_elem = 100

    for pos, labs in zip(positions, labels):
        for rad in radii:
            data, ls = _separable_data(pos, labs, rad, num_elem)

            for d,l in zip(data, ls):
                idx = labs.index(l)
                assert rad**2 > _sqdist(pos[idx], d)

@skip_on_condition(
    "not hasattr(mdp.nodes, 'ShogunSVMClassifier')",
    "This test requires the 'shogun' module.")
def test_ShogunSVMClassifier():
    # TODO: Implement parameter ranges
    num_train = 100
    num_test = 50
    for positions in [((1,), (-1,)),
                      ((1,1), (-1,-1)),
                      ((1,1,1), (-1,-1,1)),
                      ((1,1,1,1), (-1,1,1,1)),
                      ((1,1,1,1), (-1,-1,-1,-1)),
                      ((1,1), (-1,-1), (1, -1), (-1, 1))
                      ]:

        radius = 0.3

        if len(positions) == 2:
            labels = (-1, 1)
        elif len(positions) == 3:
            labels = (-1, 1, 1)
        elif len(positions) == 4:
            labels = (-1, -1, 1, 1)

        traindata_real, trainlab = _separable_data(positions, labels,
                                                          radius, num_train)
        testdata_real, testlab = _separable_data(positions, labels,
                                                        radius, num_test)


        classifiers = ['GMNPSVM', 'GNPPSVM', 'GPBTSVM', #'KernelPerceptron',
                       'LDA', 'LibSVM', #'LibSVMOneClass', 'MPDSVM',
                       'Perceptron', 'SVMLin']
        kernels = ['PolyKernel', 'LinearKernel', 'SigmoidKernel', 'GaussianKernel']

        #kernels = list(mdp.nodes.ShogunSVMClassifier.kernel_parameters.keys())
        combinations = {'classifier': classifiers,
                        'kernel': kernels}

        for comb in utils.orthogonal_permutations(combinations):
            # this is redundant but makes it clear,
            # what has been taken out deliberately
            if comb['kernel'] in ['PyramidChi2', 'Chi2Kernel']:
                # We don't have good init arguments for these
                continue
            if comb['classifier'] in ['LaRank', 'LibLinear', 'LibSVMMultiClass',
                                      'MKLClassification', 'MKLMultiClass',
                                      'MKLOneClass', 'MultiClassSVM', 'SVM',
                                      'SVMOcas', 'SVMSGD', 'ScatterSVM',
                                      'SubGradientSVM']:
                # We don't have good init arguments for these and/or they work differently
                continue

            # something does not work here: skipping
            if comb['classifier'] == 'GPBTSVM' and comb['kernel'] == 'LinearKernel':
                continue

            sg_node = mdp.nodes.ShogunSVMClassifier(classifier=comb['classifier'])

            if sg_node.classifier.takes_kernel:
                sg_node.set_kernel(comb['kernel'])

            # train in two chunks to check update mechanism
            sg_node.train( traindata_real[:num_train], trainlab[:num_train] )
            sg_node.train( traindata_real[num_train:], trainlab[num_train:] )

            assert sg_node.input_dim == len(traindata_real.T)

            out = sg_node.label(testdata_real)

            if sg_node.classifier.takes_kernel:
                # check that the kernel has stored all our training vectors
                assert sg_node.classifier.kernel.get_num_vec_lhs() == num_train * len(positions)
                # check that the kernel has also stored the latest classification vectors in rhs
                assert sg_node.classifier.kernel.get_num_vec_rhs() == num_test * len(positions)

            # Test also for inverse
            worked = numx.all(numx.sign(out) == testlab) or \
                     numx.all(numx.sign(out) == -testlab)
            failed = not worked

            should_fail = False
            if len(positions) == 2:
                if comb['classifier'] in ['LibSVMOneClass',
                                          'GMNPSVM']:
                    should_fail = True
                if comb['classifier'] == 'GPBTSVM' and \
                   comb['kernel'] in ['LinearKernel']:
                    should_fail = True

            # xor problem
            if len(positions) == 4:
                if comb['classifier'] in ['LibSVMOneClass', 'SVMLin', 'Perceptron',
                                          'LDA', 'GMNPSVM']:
                    should_fail = True
                if comb['classifier'] == 'LibSVM' and \
                   comb['kernel'] in ['LinearKernel', 'SigmoidKernel']:
                    should_fail = True
                if comb['classifier'] == 'GPBTSVM' and \
                   comb['kernel'] in ['LinearKernel', 'SigmoidKernel']:
                    should_fail = True
                if comb['classifier'] == 'GNPPSVM' and \
                   comb['kernel'] in ['LinearKernel', 'SigmoidKernel']:
                    should_fail = True

            if should_fail:
                msg = ("Classification should fail but did not in %s. Positions %s." %
                      (sg_node.classifier, positions))
            else:
                msg = ("Classification should not fail but failed in %s. Positions %s." %
                      (sg_node.classifier, positions))

            assert should_fail == failed, msg



class TestLibSVMClassifier(object):

    @skip_on_condition("not hasattr(mdp.nodes, 'LibSVMClassifier')",
                       "This test requires the 'libsvm' module.")
    def setup_method(self, method):
        self.combinations = {'kernel': mdp.nodes.LibSVMClassifier.kernels,
                             'classifier': mdp.nodes.LibSVMClassifier.classifiers}

    def test_that_parameters_are_correct(self):
        import svm as libsvm
        for comb in utils.orthogonal_permutations(self.combinations):
            C = 1.01
            epsilon = 1.1e-5
            svm_node = mdp.nodes.LibSVMClassifier(params={"C": C, "eps": epsilon})
            svm_node.set_kernel(comb['kernel'])
            svm_node.set_classifier(comb['classifier'])
            
            # check that the parameters are correct
            assert svm_node.parameter.kernel_type == getattr(libsvm, comb['kernel'])
            assert svm_node.parameter.svm_type == getattr(libsvm, comb['classifier'])
            assert svm_node.parameter.C == C
            assert svm_node.parameter.eps == epsilon

    def test_linear_separable_data(self):
        num_train = 100
        num_test = 50
        C = 1.01
        epsilon = 1e-5
        for positions in [((1,), (-1,)),
                          ((1,1), (-1,-1)),
                          ((1,1,1), (-1,-1,1)),
                          ((1,1,1,1), (-1,1,1,1)),
                          ((1,1,1,1), (-1,-1,-1,-1))]:
            radius = 0.3

            traindata_real, trainlab = _separable_data(positions, (-1, 1),
                                                       radius, num_train, True)
            testdata_real, testlab = _separable_data(positions, (-1, 1),
                                                     radius, num_test, True)

            for comb in utils.orthogonal_permutations(self.combinations):
                # Take out non-working cases
                if comb['classifier'] in ["ONE_CLASS"]:
                    continue
                if comb['kernel'] in ["SIGMOID", "POLY"]:
                    continue
                if len(positions[0]) == 1 and comb['kernel'] == "RBF":
                    # RBF won't work in 1d
                    continue

                svm_node = mdp.nodes.LibSVMClassifier(kernel=comb['kernel'],
                                                      classifier=comb['classifier'],
                                                      probability=True,
                                                      params={"C": C, "eps": epsilon})
                
                # train in two chunks to check update mechanism
                svm_node.train(traindata_real[:num_train], trainlab[:num_train])
                svm_node.train(traindata_real[num_train:], trainlab[num_train:])

                assert svm_node.input_dim == len(traindata_real.T)

                out = svm_node.label(testdata_real)

                testerr = numx.all(numx.sign(out) == testlab)
                assert testerr, ('classification error for ', comb)

                # we don't have ranks in our regression models
                if not comb['classifier'].endswith("SVR"):
                    pos1_rank = numx.array(svm_node.rank(numx.array([positions[0]])))
                    pos2_rank = numx.array(svm_node.rank(numx.array([positions[1]])))

                    assert numx.all(pos1_rank == -pos2_rank)
                    assert numx.all(abs(pos1_rank) == 1)
                    assert numx.all(abs(pos2_rank) == 1)


