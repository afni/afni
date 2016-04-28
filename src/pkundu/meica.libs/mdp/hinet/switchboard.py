"""
Module for Switchboards.

Note that additional args and kwargs for train or execute are currently not
supported.
"""


import mdp
from mdp import numx


class SwitchboardException(mdp.NodeException):
    """Exception for routing problems in the Switchboard class."""
    pass


# TODO: deal with input_dim, output_dim and dtype correctly,
#    like in IdentityNode

class Switchboard(mdp.Node):
    """Does the routing associated with the connections between layers.

    It may be directly used as a layer/node, routing all the data at once. If
    the routing/mapping is not injective the processed data may be quite large
    and probably contains many redundant copies of the input data.
    So is this case one may instead use nodes for individual output
    channels and put each in a MultiNode.

    SwitchboardLayer is the most general version of a switchboard layer, since
    there is no imposed rule for the connection topology. For practical
    applications should often derive more specialized classes.
    """

    def __init__(self, input_dim, connections):
        """Create a generic switchboard.

        The input and output dimension as well as dtype have to be fixed
        at initialization time.

        Keyword arguments:
        input_dim -- Dimension of the input data (number of connections).
        connections -- 1d Array or sequence with an entry for each output
            connection, containing the corresponding index of the
            input connection.
        """
        # check connections for inconsistencies
        if len(connections) == 0:
            err = "Received empty connection list."
            raise SwitchboardException(err)
        if numx.nanmax(connections) >= input_dim:
            err = ("One or more switchboard connection "
                   "indices exceed the input dimension.")
            raise SwitchboardException(err)
        # checks passed
        self.connections = numx.array(connections)
        output_dim = len(connections)
        super(Switchboard, self).__init__(input_dim=input_dim,
                                          output_dim=output_dim)
        # try to invert connections
        if (self.input_dim == self.output_dim and
            len(numx.unique(self.connections)) == self.input_dim):
            self.inverse_connections = numx.argsort(self.connections)
        else:
            self.inverse_connections = None

    def _execute(self, x):
        return x[:, self.connections]

    @staticmethod
    def is_trainable():
        return False

    def is_invertible(self):
        if self.inverse_connections is None:
            return False
        else:
            return True

    def _inverse(self, x):
        if self.inverse_connections is None:
            raise SwitchboardException("Connections are not invertible.")
        else:
            return x[:, self.inverse_connections]

    def _get_supported_dtypes(self):
        """Return the list of dtypes supported by this node."""
        return (mdp.utils.get_dtypes('AllFloat') +
                mdp.utils.get_dtypes('AllInteger') +
                mdp.utils.get_dtypes('Character'))

class MeanInverseSwitchboard(Switchboard):
    """Variant of Switchboard with modified inverse.

    If the switchboard mapping is not injective, then the mean
    values are used for the inverse. Inputs that are discarded in the
    mapping are set to zero.

    You can use this class as a mixin for other switchboard classes.
    """

    def _inverse(self, x):
        """Take the mean of overlapping values."""
        n_y_cons = numx.bincount(self.connections)  # n. connections to y_i
        y_cons = numx.argsort(self.connections)  # x indices for y_i
        y = numx.zeros((len(x), self.input_dim))
        i_x_counter = 0  # counter for processed x indices
        i_y = 0  # current y index
        while True:
            n_cons = n_y_cons[i_y]
            if n_cons > 0:
                y[:,i_y] = numx.sum(x[:,y_cons[i_x_counter:
                                               i_x_counter + n_cons]],
                                    axis=1) / n_cons
                i_x_counter += n_cons
                if i_x_counter >= self.output_dim:
                    break
            i_y += 1
        return y

    @staticmethod
    def is_invertible():
        return True


class ChannelSwitchboard(Switchboard):
    """Base class for Switchboards in which the data is bundled into channels.

    The dimensions of the input / output channels are constant.

    public attributes (in addition to inherited attributes):
        out_channel_dim
        in_channel_dim
        output_channels
    """

    def __init__(self, input_dim, connections, out_channel_dim,
                 in_channel_dim=1):
        """Initialize the switchboard.

        connections -- Connection sequence like for a standard switchboard
            (the indices do not correspond to whole channels, but single
             connections).
        out_channel_dim -- Number of connections per output channel.
        in_channel_dim -- Number of connections per input channel (default 1).
            All the components of an input channel are treated equally
            by the switchboard (i.e., they are routed to the same output
            channel).
        """
        super(ChannelSwitchboard, self).__init__(input_dim, connections)
        # perform checks
        if self.output_dim % out_channel_dim:
            err = ("Output dim %d is not multiple of out_channel_dim %d." %
                   (self.output_dim, out_channel_dim))
            raise SwitchboardException(err)
        if input_dim % in_channel_dim:
            err = ("Input dim %d is not multiple of in_channel_dim %d." %
                   (self.input_dim, in_channel_dim))
            raise SwitchboardException(err)
        # finalize initialization
        self.out_channel_dim = out_channel_dim
        self.in_channel_dim = in_channel_dim
        self.output_channels = self.output_dim // out_channel_dim
        self.input_channels = self.input_dim // in_channel_dim

    def get_out_channel_input(self, channel):
        """Return the input connections for the given channel index.

        channel -- index of the requested channel (starting at 0)
        """
        index = channel * self.out_channel_dim
        return self.connections[index : index+self.out_channel_dim]

    def get_out_channel_node(self, channel):
        """Return a Switchboard that does the routing for a single
        output channel.

        channel -- index of the requested channel (starting at 0)
        """
        return Switchboard(self.input_dim, self.get_out_channel_input(channel))

    def get_out_channels_input_channels(self, channels):
        """Return array of input channel indices for the given output channels.

        channels -- Sequence of the requested output channels or a single
            channel index (i.e. a number).

        The returned array contains the indices of all input channels which
        are connected to at least one of the given output channels.
        """
        if isinstance(channels, int):
            channels = [channels]
        # create boolean arry to determine with active inputs
        channels_input = self.connections.reshape((-1, self.out_channel_dim))
        channels_input = channels_input[channels].reshape(-1)
        covered = numx.zeros(self.input_dim, dtype="bool")
        covered[channels_input] = True
        # reshape to perform logical OR over the input channels
        covered = covered.reshape((-1, self.in_channel_dim))
        covered = covered.sum(axis=1, dtype=bool)
        return covered.nonzero()[0]


def to_2tuple(value):
    """Return value or (value, value) if value is not a tuple."""
    if isinstance(value, tuple):
        return value
    if isinstance(value, list):
        return tuple(value)
    return (value, value)

    
class Rectangular2dSwitchboardException(SwitchboardException):
    """Exception for routing problems in the Rectangular2dSwitchboard class."""
    pass


class Rectangular2dSwitchboard(ChannelSwitchboard):
    """Switchboard for a 2-dimensional topology.

    This is a specialized version of SwitchboardLayer that makes it easy to
    implement connection topologies which are based on a 2-dimensional network
    layers.

    The input connections are assumed to be grouped into so called channels,
    which are considered as lying in a two dimensional rectangular plane.
    Each output channel corresponds to a 2d rectangular field in the
    input plane. The fields can overlap.

    The coordinates follow the standard image convention (see the above
    CoordinateTranslator class).

    public attributes (in addition to init arguments and inherited attributes):
        unused_channels_xy
        out_channels_xy
    """
    
    def __init__(self, in_channels_xy, field_channels_xy,
                 field_spacing_xy=1, in_channel_dim=1, ignore_cover=False):
        """Calculate the connections.

        Keyword arguments:
        in_channels_xy -- 2-Tuple with number of input channels in the x- and
            y-direction (or a single number for both). This has to be
            specified, since the actual input is only one 1d array.
        field_channels_xy -- 2-Tuple with number of channels in each field in
            the x- and y-direction (or a single number for both).
        field_spacing_xy -- 2-Tuple with offset between two fields in the x-
            and y-direction (or a single number for both).
        in_channel_dim -- Number of connections per input channel.
        ignore_cover -- Boolean value defines if an
            Rectangular2dSwitchboardException is raised when the fields do not
            cover all input channels. Set this to True if you are willing to
            risk loosing input channels at the border.
        """
        in_channels_xy = to_2tuple(in_channels_xy)
        field_channels_xy = to_2tuple(field_channels_xy)
        field_spacing_xy = to_2tuple(field_spacing_xy)
        self.in_channels_xy = in_channels_xy
        self.field_channels_xy = field_channels_xy
        self.field_spacing_xy = field_spacing_xy
        # number of channels which are not covered
        out_channel_dim = (in_channel_dim *
                           field_channels_xy[0] * field_channels_xy[1])
        ## check parameters for inconsistencies
        for i, name in enumerate(["x", "y"]):
            if (field_channels_xy[i] > in_channels_xy[i]):
                err = ("Number of field channels exceeds the number of "
                       "input channels in %s-direction. "
                       "This would lead to an empty connection list." %
                       name)
                raise Rectangular2dSwitchboardException(err)
        # number of output channels in x-direction
        x_out_channels = (((in_channels_xy[0] - field_channels_xy[0]) //
                           field_spacing_xy[0]) + 1)
        x_unused_channels = in_channels_xy[0] - field_channels_xy[0]
        if x_unused_channels > 0:
            x_unused_channels %= field_spacing_xy[0]
        elif x_unused_channels < 0:
            x_unused_channels = in_channels_xy[0]
        # number of output channels in y-direction
        y_out_channels = (((in_channels_xy[1] - field_channels_xy[1]) //
                           field_spacing_xy[1]) + 1)
        y_unused_channels = in_channels_xy[1] - field_channels_xy[1]
        if y_unused_channels > 0:
            y_unused_channels %= field_spacing_xy[1]
        elif y_unused_channels < 0:
            y_unused_channels = in_channels_xy[1]
        self.unused_channels_xy = (x_unused_channels, y_unused_channels)
        for i, name in enumerate(["x", "y"]):
            if self.unused_channels_xy[i] and not ignore_cover:
                err = ("Channel fields do not "
                       "cover all input channels in %s-direction." % name)
                raise Rectangular2dSwitchboardException(err)
        self.out_channels_xy = (x_out_channels, y_out_channels)
        ## end of parameters checks
        # TODO: rewrite all this leveraging numpy?
        #    this might make the code much simpler
        #    use 3rd shape entry for channel width
        out_channels = x_out_channels * y_out_channels
        in_trans = CoordinateTranslator(*in_channels_xy)
        # input-output mapping of connections
        # connections has an entry for each output connection,
        # containing the index of the input connection.
        connections = numx.zeros([out_channels * out_channel_dim],
                                 dtype=numx.int32)
        first_out_con = 0
        for y_out_chan in range(self.out_channels_xy[1]):
            for x_out_chan in range(self.out_channels_xy[0]):
                # inner loop over field
                x_start_chan = x_out_chan * field_spacing_xy[0]
                y_start_chan = y_out_chan * field_spacing_xy[1]
                for y_in_chan in range(y_start_chan,
                                       y_start_chan + field_channels_xy[1]):
                    for x_in_chan in range(x_start_chan,
                                        x_start_chan + field_channels_xy[0]):
                        first_in_con = (in_trans.image_to_index(
                                                    x_in_chan, y_in_chan) *
                                        in_channel_dim)
                        connections[first_out_con:
                                    first_out_con + in_channel_dim] = \
                            range(first_in_con, first_in_con + in_channel_dim)
                        first_out_con += in_channel_dim
        super(Rectangular2dSwitchboard, self).__init__(
                                input_dim=(in_channel_dim *
                                    in_channels_xy[0] * in_channels_xy[1]),
                                connections=connections,
                                out_channel_dim=out_channel_dim,
                                in_channel_dim=in_channel_dim)


class DoubleRect2dSwitchboardException(SwitchboardException):
    """Exception for routing problems in the DoubleRect2dSwitchboard class."""
    pass


class DoubleRect2dSwitchboard(ChannelSwitchboard):
    """Special 2d Switchboard where each inner point is covered twice.

    First the input is covered with non-overlapping rectangular fields.
    Then the input is covered with fields of the same size that are shifted
    in the x and y direction by half the field size (we call this the
    uneven fields).

    Note that the output of this switchboard cannot be interpreted as
    a rectangular grid, because the short rows are shifted. Instead it is
    a rhombic grid (it is not a hexagonal grid because the distances of the
    field centers do not satisfy the necessary relation).
    See http://en.wikipedia.org/wiki/Lattice_(group)

    Example for a 6x4 input and a field size of 2 in both directions:

    long row fields:

    1 1 2 2 3 3
    1 1 2 2 3 3
    4 4 5 5 6 6
    4 4 5 5 6 6

    short row fields:

    * * * * * *
    * 7 7 8 8 *
    * 7 7 8 8 *
    * * * * * *

    Note that the short row channels come after all the long row connections in
    the connections sequence.

    public attributes (in addition to init arguments and inherited attributes):
        unused_channels_xy
        long_out_channels_xy -- Output channels in the long rows.
    """

    # TODO: settle on 'long' or 'even' term?

    def __init__(self, in_channels_xy, field_channels_xy,
                 in_channel_dim=1, ignore_cover=False):
        """Calculate the connections.

        Keyword arguments:
        in_channels_xy -- 2-Tuple with number of input channels in the x- and
            y-direction (or a single number for both). This has to be
            specified, since the actual input is only one 1d array.
        field_channels_xy -- 2-Tuple with number of channels in each field in
            the x- and y-direction (or a single number for both).
            Must be even numbers.
        in_channel_dim -- Number of connections per input channel
        ignore_cover -- Boolean value defines if an
            Rectangular2dSwitchboardException is raised when the fields do not
            cover all input channels. Set this to True if you are willing to
            risk loosing input channels at the border.
        """
        in_channels_xy = to_2tuple(in_channels_xy)
        field_channels_xy = to_2tuple(field_channels_xy)
        ## count channels and stuff
        self.in_channels_xy = in_channels_xy
        self.field_channels_xy = field_channels_xy
        out_channel_dim = (in_channel_dim *
                           field_channels_xy[0] * field_channels_xy[1])
        ## check parameters for inconsistencies
        for i, name in enumerate(["x", "y"]):
            if field_channels_xy[i] % 2:
                err = ("%s_field_channels must be an even number, was %d" %
                       (name, field_channels_xy[i]))
                raise Rectangular2dSwitchboardException(err)
        field_spacing_xy = (field_channels_xy[0] // 2,
                            field_channels_xy[1] // 2)
        for i, name in enumerate(["x", "y"]):
            if (field_channels_xy[i] > in_channels_xy[i]):
                err = ("Number of field channels"
                       "exceeds the number of input channels in %s-direction. "
                       "This would lead to an empty connection list." %
                       name)
                raise Rectangular2dSwitchboardException(err)
        # number of output channels in x-direction
        xl = in_channels_xy[0] // field_channels_xy[0]
        x_unused_channels = in_channels_xy[0] - field_channels_xy[0]
        if x_unused_channels > 0:
            x_unused_channels %= field_spacing_xy[0]
        elif x_unused_channels < 0:
            x_unused_channels = in_channels_xy[0]
        if x_unused_channels and not ignore_cover:
            err = ("Channel fields do not "
                   "cover all input channels in x-direction.")
            raise Rectangular2dSwitchboardException(err)
        if (in_channels_xy[0] - xl * field_channels_xy[0]) >= (field_channels_xy[0] // 2):
            err = ("x short rows have same length as long rows.")
            raise Rectangular2dSwitchboardException(err)
        # number of output channels in y-direction
        yl = in_channels_xy[1] // field_channels_xy[1]
        y_unused_channels = in_channels_xy[1] - field_channels_xy[1]
        if y_unused_channels > 0:
            y_unused_channels %= field_spacing_xy[1]
        elif y_unused_channels < 0:
            y_unused_channels = in_channels_xy[1]
        if y_unused_channels and not ignore_cover:
            err = ("Channel fields do not "
                   "cover all input channels in y-direction.")
            raise Rectangular2dSwitchboardException(err)
        if ((in_channels_xy[1] - yl * field_channels_xy[1]) >=
            (field_channels_xy[1] // 2)):
            err = ("y short rows have same length as long rows.")
            raise Rectangular2dSwitchboardException(err)
        # TODO: add check against n+1/2 size, long line length equals short one
        ## end of parameters checks
        self.long_out_channels_xy = (xl, yl)
        self.unused_channels_xy = (x_unused_channels, y_unused_channels)
        out_channels = xl * yl + (xl-1) * (yl-1)
        in_trans = CoordinateTranslator(*in_channels_xy)
        connections = numx.zeros([out_channels * out_channel_dim],
                                 dtype=numx.int32)
        first_out_con = 0
        ## first create the even connections
        even_x_out_channels = in_channels_xy[0] // (2 * field_spacing_xy[0])
        even_y_out_channels = in_channels_xy[1] // (2 * field_spacing_xy[1])
        for y_out_chan in range(even_y_out_channels):
            for x_out_chan in range(even_x_out_channels):
                # inner loop over field
                x_start_chan = x_out_chan * (2 * field_spacing_xy[0])
                y_start_chan = y_out_chan * (2 * field_spacing_xy[1])
                for y_in_chan in range(y_start_chan,
                                    y_start_chan + self.field_channels_xy[1]):
                    for x_in_chan in range(x_start_chan,
                                    x_start_chan + self.field_channels_xy[0]):
                        first_in_con = (in_trans.image_to_index(
                                            x_in_chan, y_in_chan) *
                                        in_channel_dim)
                        connections[first_out_con:
                                    first_out_con + in_channel_dim] = \
                            range(first_in_con,
                                  first_in_con + in_channel_dim)
                        first_out_con += in_channel_dim
        ## create the uneven connections
        for y_out_chan in range(even_y_out_channels - 1):
            for x_out_chan in range(even_x_out_channels - 1):
                # inner loop over field
                x_start_chan = (x_out_chan * (2 * field_spacing_xy[0]) +
                                field_spacing_xy[0])
                y_start_chan = (y_out_chan * (2 * field_spacing_xy[1]) +
                                field_spacing_xy[1])
                for y_in_chan in range(y_start_chan,
                                       y_start_chan + self.field_channels_xy[1]):
                    for x_in_chan in range(x_start_chan,
                                       x_start_chan + self.field_channels_xy[0]):
                        first_in_con = (in_trans.image_to_index(
                                                    x_in_chan, y_in_chan) *
                                        in_channel_dim)
                        connections[first_out_con:
                                    first_out_con + in_channel_dim] = \
                            range(first_in_con,
                                  first_in_con + in_channel_dim)
                        first_out_con += in_channel_dim
        super(DoubleRect2dSwitchboard, self).__init__(
                                input_dim=in_channel_dim *
                                    in_channels_xy[0] * in_channels_xy[1],
                                connections=connections,
                                out_channel_dim=out_channel_dim,
                                in_channel_dim=in_channel_dim)


class DoubleRhomb2dSwitchboardException(SwitchboardException):
    """Exception for routing problems in the DoubleRhomb2dSwitchboard class."""
    pass


class DoubleRhomb2dSwitchboard(ChannelSwitchboard):
    """Rectangular lattice switchboard covering a rhombic lattice.

    All inner points of the rhombic lattice are covered twice. The rectangular
    fields are rotated by 45 degree.

    We assume that both the first and last row is a long row, e.g.

    *   *   *   *
      *   *   *
    *   *   *   *
      *   *   *
    *   *   *   *

    The incoming data is expected to contain the long rows first, then
    the short rows.

    The alignment of the first field is chosen to minimize cutaway.

    public attributes (in addition to init arguments and inherited attributes):
        out_channels_xy
    """

    def __init__(self, long_in_channels_xy,
                 diag_field_channels, in_channel_dim=1):
        """Calculate the connections.

        Note that the incoming data will be interpreted as a rhombic grid,
        as it is produced by DoubleRect2dSwitchboard.

        Keyword arguments:
        long_in_channels_xy -- 2-Tuple with number of long input channels in
            the x- and y-direction (or a single number for both).
        diag_field_channels -- Field edge size (before the rotation).
        in_channel_dim -- Number of connections per input channel
        """
        long_in_channels_xy = to_2tuple(long_in_channels_xy)
        self.long_in_channels_xy = long_in_channels_xy
        if long_in_channels_xy[0] < long_in_channels_xy[1]:
            started_in_short = 1
        else:
            started_in_short = 0
        ## check parameters for inconsistencies ##
        if diag_field_channels % 2:
            err = ("diag_field_channels must be even (for double cover)")
            raise DoubleRhomb2dSwitchboardException(err)
        self.diag_field_channels = diag_field_channels
        # helper variables for the field range
        _x_chan_field_range = (long_in_channels_xy[0] -
                               (1 - started_in_short) - diag_field_channels)
        _y_chan_field_range = (long_in_channels_xy[1] - started_in_short -
                               diag_field_channels)
        if (_x_chan_field_range % (diag_field_channels // 2) or
            _x_chan_field_range < 0):
            err = ("diag_field_channels value is not compatible with "
                   "long_in_channels_xy[0]")
            raise DoubleRhomb2dSwitchboardException(err)
        if (_y_chan_field_range % (diag_field_channels // 2) or
            _y_chan_field_range < 0):
            err = ("diag_field_channels value is not compatible with "
                   "long_in_channels_xy[1]")
            raise DoubleRhomb2dSwitchboardException(err)
        ## count channels and stuff
        self.in_channel_dim = in_channel_dim
        input_dim = ((2 * long_in_channels_xy[0] * long_in_channels_xy[1]
                     - long_in_channels_xy[0] - long_in_channels_xy[1] + 1) *
                     in_channel_dim)
        out_channel_dim = in_channel_dim * diag_field_channels**2
        x_out_channels = (2 * _x_chan_field_range // diag_field_channels + 1)
        y_out_channels = (2 * _y_chan_field_range // diag_field_channels + 1)
        self.out_channels_xy = (x_out_channels, y_out_channels)
        ## prepare iteration over fields
        long_in_trans = CoordinateTranslator(*long_in_channels_xy)
        short_in_trans = CoordinateTranslator(long_in_channels_xy[0] - 1,
                                              long_in_channels_xy[1] - 1)
        short_in_offset = long_in_channels_xy[0] * long_in_channels_xy[1]
        connections = numx.zeros([x_out_channels * y_out_channels *
                                  out_channel_dim], dtype=numx.int32)
        first_out_con = 0
        for y_out_chan in range(y_out_channels):
            for x_out_chan in range(x_out_channels):
                # inner loop over perceptive field
                x_start_chan = (1 + x_out_chan) * diag_field_channels // 2
                y_start_chan = y_out_chan * diag_field_channels
                # set the initial field offset to minimize edge loss
                x_start_chan -= started_in_short
                y_start_chan += started_in_short
                # iterate over both long and short rows
                for iy, y_in_chan in enumerate(range(y_start_chan,
                                y_start_chan + (2 * diag_field_channels - 1))):
                    # half width of the field in the given row
                    if iy <= (diag_field_channels - 1):
                        field_width = iy + 1
                    else:
                        field_width = (diag_field_channels - 1 -
                                       (iy % diag_field_channels))
                    for x_in_chan in range(x_start_chan - field_width // 2,
                                           x_start_chan + field_width // 2
                                                        + field_width % 2):
                        # array index of the first input connection
                        # for this input channel
                        if not y_in_chan % 2:
                            if started_in_short:
                                x_in_chan += 1
                            first_in_con = (
                                long_in_trans.image_to_index(
                                                x_in_chan, y_in_chan // 2) *
                                                        self.in_channel_dim)
                        else:
                            first_in_con = (
                                (short_in_trans.image_to_index(
                                                x_in_chan, y_in_chan // 2)
                                 + short_in_offset) * self.in_channel_dim)
                        connections[first_out_con:
                                    first_out_con + self.in_channel_dim] = \
                            range(first_in_con,
                                  first_in_con + self.in_channel_dim)
                        first_out_con += self.in_channel_dim
        super(DoubleRhomb2dSwitchboard, self).__init__(
                                        input_dim=input_dim,
                                        connections=connections,
                                        out_channel_dim=out_channel_dim,
                                        in_channel_dim=in_channel_dim)


# utility class for Rectangular2dSwitchboard

class CoordinateTranslator(object):
    """Translate between image (PIL) and numpy array coordinates.

    PIL image coordinates go from 0..width-1 . The first coordinate is x.
    Array coordinates also start from 0, but the first coordinate is the row.
    As depicted below we have x = column, y = row. The entry index numbers are
    also shown.

      +------> x
      | 1 2
      | 3 4
    y v

    array[y][x]
    """

    def __init__(self, x_image_dim, y_image_dim):
        self.x_image_dim = x_image_dim
        self.y_image_dim = y_image_dim
        self._max_index = x_image_dim * y_image_dim - 1

    def image_to_array(self, x, y):
        return y, x

    def image_to_index(self, x, y):
        if not 0 <= x < self.x_image_dim:
            raise Exception("x coordinate %d is outside the valid range." % x)
        if not 0 <= y < self.y_image_dim:
            raise Exception("y coordinate %d is outside the valid range." % y)
        return y * self.x_image_dim + x

    def array_to_image(self, row, col):
        return col, row

    def array_to_index(self, row, col):
        if not 0 <= row < self.y_image_dim:
            raise Exception("row index %d is outside the valid range." % row)
        if not 0 <= col < self.x_image_dim:
            raise Exception("column index %d is outside the valid range." %
                            col)
        return row * self.x_image_dim + col

    def index_to_array(self, index):
        if not 0 <= index <= self._max_index:
            raise Exception("index %d is outside the valid range." %
                            index)
        return index // self.x_image_dim, index % self.x_image_dim

    def index_to_image(self, index):
        if not 0 <= index <= self._max_index:
            raise Exception("index %d is outside the valid range." %
                            index)
        return index % self.x_image_dim, index // self.x_image_dim
