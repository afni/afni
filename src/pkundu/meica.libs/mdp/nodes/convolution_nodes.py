__docformat__ = "restructuredtext en"

from mdp import numx, numx_linalg, utils, NodeException
import mdp
import scipy.signal as signal

# TODO automatic selection of convolution

# TODO provide generators for standard filters

# TODO look into Theano, define TheanoConvolutionNode

class Convolution2DNode(mdp.Node):
    """Convolve input data with filter banks.

    The ``filters`` argument specifies a set of 2D filters that are
    convolved with the input data during execution. Convolution can
    be selected to be executed by linear filtering of the data, or
    in the frequency domain using a Discrete Fourier Transform.

    Input data can be given as 3D data, each row being a 2D array
    to be convolved with the filters, or as 2D data, in which case
    the ``input_shape`` argument must be specified.

    This node depends on ``scipy``.
    """

    def __init__(self, filters, input_shape = None,
                 approach = 'fft',
                 mode = 'full', boundary = 'fill', fillvalue = 0,
                 output_2d = True,
                 input_dim = None, dtype = None):
        """
        Input arguments:

        input_shape -- Is a tuple (h,w) that corresponds to the height and
                       width of the input 2D data. If the input data is given
                       in a flattened format, it is first reshaped before
                       convolution

        approach -- 'approach' is one of ['linear', 'fft']
                    'linear': convolution is done by linear filtering;
                    'fft': convoltion is done using the Fourier Transform
                    If 'approach' is 'fft', the 'boundary' and 'fillvalue' arguments
                    are ignored, and are assumed to be 'fill' and 0, respectively.
                    (*Default* = 'fft')

        mode -- Convolution mode, as defined in scipy.signal.convolve2d
                'mode' is one of ['valid', 'same', 'full']
                (*Default* = 'full')

        boundary -- Boundary condition, as defined in scipy.signal.convolve2d
                     'boundary' is one of ['fill', 'wrap', 'symm']
                     (*Default* = 'fill')

        fillvalue -- Value to fill pad input arrays with
                     (*Default* = 0)

        output_2d -- If True, the output array is 2D; the first index
                     corresponds to data points; every output data point
                     is the result of flattened convolution results, with
                     the output of each filter concatenated together.

                     If False, the output array is 4D; the format is
                     data[idx,filter_nr,x,y], with
                     filter_nr: index of convolution filter
                     idx: data point index
                     x, y: 2D coordinates
        """
        super(Convolution2DNode, self).__init__(input_dim=input_dim,
                                              dtype=dtype)

        self.filters = filters

        self._input_shape = input_shape

        if approach not in ['linear', 'fft']:
            raise NodeException("'approach' argument must be one of ['linear', 'fft']")
        self._approach = approach

        if mode not in ['valid', 'same', 'full']:
            raise NodeException("'mode' argument must be one of ['valid', 'same', 'full']")
        self._mode = mode

        self.boundary = boundary
        self.fillvalue = fillvalue
        self.output_2d = output_2d
        self._output_shape = None

    # ------- class properties
    def get_filters(self):
        return self._filters

    def set_filters(self, filters):
        if not isinstance(filters, numx.ndarray):
            raise NodeException("'filters' argument must be a numpy array")
        if filters.ndim != 3:
            raise NodeException('Filters must be specified in a 3-dim array, with each '+
                                'filter on a different row')
        self._filters = filters

    filters = property(get_filters, set_filters)

    def get_boundary(self):
        return self._boundary

    def set_boundary(self, boundary):
        if boundary not in ['fill', 'wrap', 'symm']:
            raise NodeException(
                "'boundary' argument must be one of ['fill', 'wrap', 'symm']")
        self._boundary = boundary

    boundary = property(get_boundary, set_boundary)

    @property
    def input_shape(self):
        return self._input_shape

    @property
    def approach(self):
        return self._approach

    @property
    def mode(self):
        return self._mode

    @property
    def output_shape(self):
        return self._output_shape
    # ------- /class properties

    def is_trainable(self):
        return False

    def is_invertible(self):
        return False

    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node.
      
        Support floating point types with size smaller or equal than 64 bits.
        This is because fftpack does not support floating point types larger
        than that.
        """
        return [t for t in utils.get_dtypes('Float') if t.itemsize<=8]

    def _pre_execution_checks(self, x):
        """This method contains all pre-execution checks.
        It can be used when a subclass defines multiple execution methods.

        In this case, the output dimension depends on the type of
        convolution we use (padding, full, ...). Also, we want to
        to be able to accept 3D arrays.
        """

        # check input rank
        if not x.ndim in [2,3]:
            error_str = "x has rank %d, should be 2 or 3" % (x.ndim)
            raise NodeException(error_str)

        # set 2D shape if necessary
        if self._input_shape is None:
            if x.ndim == 2:
                error_str = "Cannot infer 2D shape from 1D data points. " + \
                            "Data must have rank 3, or shape argument given."
                raise NodeException(error_str)
            else:
                self._input_shape = x.shape[1:]

        # set the input dimension if necessary
        if self.input_dim is None:
            self.input_dim = numx.prod(self._input_shape)

        # set the dtype if necessary
        if self.dtype is None:
            self.dtype = x.dtype

        # check the input dimension
        if not numx.prod(x.shape[1:]) == self.input_dim:
            error_str = "x has dimension %d, should be %d" % (x.shape[1],
                                                              self.input_dim)
            raise NodeException(error_str)

        # set output_dim if necessary
        if self.output_dim is None:
            input_shape = self.input_shape
            filters_shape = self.filters.shape
            if self.mode == 'same':
                self._output_shape = input_shape
            elif self.mode == 'full':
                self._output_shape = (input_shape[0]+filters_shape[1]-1,
                                      input_shape[1]+filters_shape[2]-1)
            else: # mode == 'valid'
                self._output_shape = (input_shape[0]-filters_shape[1]+1,
                                      input_shape[1]-filters_shape[2]+1)
            self.output_dim = self.filters.shape[0]*numx.prod(self._output_shape)

        if x.shape[0] == 0:
            error_str = "x must have at least one observation (zero given)"
            raise NodeException(error_str)

    def _execute(self, x):
        is_2d = x.ndim==2
        output_shape, input_shape = self._output_shape, self._input_shape
        filters = self.filters
        nfilters = filters.shape[0]

        # XXX depends on convolution
        y = numx.empty((x.shape[0], nfilters,
                        output_shape[0], output_shape[1]), dtype=self.dtype)
        for n_im, im in enumerate(x):
            if is_2d:
                im = im.reshape(input_shape)
            for n_flt, flt in enumerate(filters):
                if self.approach == 'fft':
                    y[n_im,n_flt,:,:] = signal.fftconvolve(im, flt, mode=self.mode)
                elif self.approach == 'linear':
                    y[n_im,n_flt,:,:] = signal.convolve2d(im, flt,
                                                          mode=self.mode,
                                                          boundary=self.boundary,
                                                          fillvalue=self.fillvalue)

        # reshape if necessary
        if self.output_2d:
            y.resize((y.shape[0], self.output_dim))

        return y
