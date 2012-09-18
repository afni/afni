"""
Module to convert a flow into an HTML representation.

This is especially useful for hinet structures.
The code uses the visitor pattern to reach and convert all the nodes in a flow.
"""

from __future__ import with_statement

import tempfile
import os
import webbrowser
import cStringIO as StringIO

import mdp

import switchboard

# TODO: use <pre>   </pre> for whitespaces?

class NewlineWriteFile(object):
    """Decorator for file-like object.

    Adds a newline character to each line written with write().
    """

    def __init__(self, file_obj):
        """Wrap the given file-like object."""
        self.file_obj = file_obj

    def write(self, str_obj):
        """Write a string to the file object and append a newline character."""
        self.file_obj.write(str_obj + "\n")

    # forward all other methods
    def __getattr__(self, attr):
        return getattr(self.file_obj, attr)


class HiNetHTMLVisitor(object):
    """Class to convert a hinet flow to HTML.

    This class implements the visitor pattern. It relies on the 'html'
    extension to get custom representations for normal node classes.
    """

    def __init__(self, html_file, show_size=False):
        """Initialize the HMTL converter.

        html_file -- File object into which the representation is written
            (only the write method is used).
        show_size -- Show the approximate memory footprint of all nodes.
        """
        self.show_size = show_size
        self._file = NewlineWriteFile(html_file)

    @mdp.with_extension("html")
    def convert_flow(self, flow):
        """Convert the flow into HTML and write it into the internal file."""
        f = self._file
        self._open_node_env(flow, "flow")
        for node in flow:
            f.write('<tr><td>')
            self._visit_node(node)
            f.write('</td></tr>')
        self._close_node_env(flow, "flow")

    _CSS_FILENAME = "hinet.css"

    @classmethod
    def hinet_css(cls):
        """Return the standard CSS string.

        The CSS should be embedded in the final HTML file.
        """
        css_filename = os.path.join(os.path.split(__file__)[0],
                                    cls._CSS_FILENAME)
        with open(css_filename, 'r') as css_file:
            css = css_file.read()
        return css

    def _visit_node(self, node):
        """Translate a node and return the translation.

        Depending on the type of the node this can be delegated to more
        specific methods.
        """
        if hasattr(node, "flow"):
            self._visit_flownode(node)
        elif isinstance(node, mdp.hinet.CloneLayer):
            self._visit_clonelayer(node)
        elif isinstance(node, mdp.hinet.SameInputLayer):
            self._visit_sameinputlayer(node)
        elif isinstance(node, mdp.hinet.Layer):
            self._visit_layer(node)
        else:
            self._visit_standard_node(node)

    def _visit_flownode(self, flownode):
        f = self._file
        self._open_node_env(flownode, "flownode")
        for node in flownode.flow:
            f.write('<tr><td>')
            self._visit_node(node)
            f.write('</td></tr>')
        self._close_node_env(flownode, "flownode")

    def _visit_layer(self, layer):
        f = self._file
        self._open_node_env(layer, "layer")
        f.write('<tr>')
        for node in layer:
            f.write('<td>')
            self._visit_node(node)
            f.write('</td>')
        f.write('</tr>')
        self._close_node_env(layer)

    def _visit_clonelayer(self, layer):
        f = self._file
        self._open_node_env(layer, "layer")
        f.write('<tr><td class="nodename">')
        f.write(str(layer) + '<br><br>')
        f.write('%d repetitions' % len(layer))
        f.write('</td>')
        f.write('<td>')
        self._visit_node(layer.node)
        f.write('</td></tr>')
        self._close_node_env(layer)

    def _visit_sameinputlayer(self, layer):
        f = self._file
        self._open_node_env(layer, "layer")
        f.write('<tr><td colspan="%d" class="nodename">%s</td></tr>' %
                (len(layer), str(layer)))
        f.write('<tr>')
        for node in layer:
            f.write('<td>')
            self._visit_node(node)
            f.write('</td>')
        f.write('</tr>')
        self._close_node_env(layer)

    def _visit_standard_node(self, node):
        f = self._file
        self._open_node_env(node)
        f.write('<tr><td class="nodename">')
        f.write(str(node))
        f.write('</td></tr>')
        f.write('<tr><td class="nodeparams">')
        f.write(node.html_representation())
        f.write('</td></tr>')
        self._close_node_env(node)

    # helper methods for decoration

    def _open_node_env(self, node, type_id="node"):
        """Open the HTML environment for the node internals.

        node -- The node itself.
        type_id -- The id string as used in the CSS.
        """
        self._file.write('<table class="%s">' % type_id)
        self._write_node_header(node, type_id)

    def _write_node_header(self, node, type_id="node"):
        """Write the header content for the node into the HTML file."""
        f = self._file
        if not (type_id=="flow" or type_id=="flownode"):
            f.write('<tr><td class="dim">in-dim: %s</td></tr>' %
                    str(node.input_dim))
        f.write('<tr><td>')
        f.write('<table class="nodestruct">')

    def _close_node_env(self, node, type_id="node"):
        """Close the HTML environment for the node internals.

        node -- The node itself.
        type_id -- The id string as used in the CSS.
        """
        f = self._file
        f.write('</table>')
        f.write('</td></tr>')
        if not (type_id=="flow" or type_id=="flownode"):
            f.write('<tr><td class="dim">out-dim: %s' % str(node.output_dim))
            if self.show_size:
                f.write('&nbsp;&nbsp;<span class="memorycolor">size: %s</span>'
                        % mdp.utils.get_node_size_str(node))
            f.write('</td></tr>')
        f.write('</table>')


class  HTMLExtensionNode(mdp.ExtensionNode, mdp.Node):
    """Extension node for custom HTML representations of individual nodes.

    This extension works together with the HiNetHTMLVisitor to allow the
    polymorphic generation of representations for node classes.
    """

    extension_name = "html"

    def html_representation(self):
        """Return an HTML representation of the node."""
        html_repr = self._html_representation()
        if type(html_repr) is str:
            return html_repr
        else:
            return " <br>\n".join(html_repr)

    # override this method
    def _html_representation(self):
        """Return either the final HTML code or a list of HTML lines."""
        return ""


@mdp.extension_method("html", switchboard.Rectangular2dSwitchboard,
                      "_html_representation")
def _rect2d_switchoard_html(self):
    lines = ['rec. field size (in channels): %d x %d = %d' %
                (self.field_channels_xy[0], self.field_channels_xy[1],
                 self.field_channels_xy[0] * self.field_channels_xy[1]),
             '# of rec. fields (out channels): %d x %d = %d' %
                (self.out_channels_xy[0], self.out_channels_xy[1],
                 self.output_channels),
             'rec. field distances (in channels): ' +
                str(self.field_spacing_xy),
             'channel width: %d' % self.in_channel_dim]
    if not all(self.unused_channels_xy):
        lines.append('unused channels: ' + str(self.unused_channels_xy))
    return lines

@mdp.extension_method("html", switchboard.DoubleRect2dSwitchboard,
                      "_html_representation")
def _double_rect2d_switchoard_html(self):
    lines = ['rec. field size (in channels): %d x %d = %d' %
                (self.field_channels_xy[0], self.field_channels_xy[1],
                 self.field_channels_xy[0] * self.field_channels_xy[1]),
             '# of long row rec. fields (out channels): ' +
                str(self.long_out_channels_xy),
             'total number of receptive fields: %d' %
                self.output_channels,
             'channel width: %d' % self.in_channel_dim]
    if self.x_unused_channels or self.y_unused_channels:
        lines.append('unused channels: ' + str(self.unused_channels_xy))
    return lines

@mdp.extension_method("html", switchboard.DoubleRhomb2dSwitchboard,
                      "_html_representation")
def _double_rhomb2d_switchoard_html(self):
    lines = ['rec. field size: %d' % self.diag_field_channels,
             '# of rec. fields (out channels): %d x %d = %d' %
                (self.out_channels_xy[0], self.out_channels_xy[1],
                 self.output_channels),
             'channel width: %d' % self.in_channel_dim]
    return lines

@mdp.extension_method("html", mdp.nodes.SFA2Node, "_html_representation")
def _sfa_html(self):
    return 'expansion dim: ' + str(self._expnode.output_dim)

@mdp.extension_method("html", mdp.nodes.NormalNoiseNode,
                      "_html_representation")
def _noise_html(self):
    return ['noise level: ' + str(self.noise_args[1]),
            'noise offset: ' + str(self.noise_args[0])]

@mdp.extension_method("html", mdp.nodes.CutoffNode, "_html_representation")
def _cutoff_html(self):
    return ['lower bound: ' + str(self.lower_bound),
            'upper bound: ' + str(self.upper_bound)]

@mdp.extension_method("html", mdp.nodes.HistogramNode, "_html_representation")
def _hist_html(self):
    return 'history data fraction: ' + str(self.hist_fraction)

@mdp.extension_method("html", mdp.nodes.AdaptiveCutoffNode,
                      "_html_representation")
def _adap_html(self):
    return ['lower cutoff fraction: ' + str(self.lower_cutoff_fraction),
            'upper cutoff fraction: ' + str(self.upper_cutoff_fraction),
            'history data fraction: ' + str(self.hist_fraction)]


class HiNetXHTMLVisitor(HiNetHTMLVisitor):
    """Modified converter to create valid XHTML."""

    def convert_flow(self, flow):
        """Convert the flow into XHTML and write it into the internal file."""
        # first write the normal HTML into a buffer
        orig_file = self._file
        html_file = StringIO.StringIO()
        self._file = NewlineWriteFile(html_file)
        super(HiNetXHTMLVisitor, self).convert_flow(flow)
        self._file = orig_file
        # now convert it to XHTML
        html_code = html_file.getvalue()
        html_code = html_code.replace('<br>', '<br />')
        html_code = html_code.replace('&nbsp;', '&#160;')
        self._file.write(html_code)


## Helper functions ##

def show_flow(flow, filename=None, title="MDP flow display",
              show_size=False, browser_open=True):
    """Write a flow into a HTML file, open it in the browser and
    return the file name.

    flow -- The flow to be shown.
    filename -- Filename for the HTML file to be created. If None
                a temporary file is created.
    title -- Title for the HTML file.
    show_size -- Show the approximate memory footprint of all nodes.
    browser_open -- If True (default value) then the slideshow file is
        automatically opened in a webbrowser.
    """
    if filename is None:
        (fd, filename) = tempfile.mkstemp(suffix=".html", prefix="MDP_")
        html_file = os.fdopen(fd, 'w')
    else:
        html_file = open(filename, 'w')
    html_file.write('<html>\n<head>\n<title>%s</title>\n' % title)
    html_file.write('<style type="text/css" media="screen">')
    html_file.write(mdp.utils.basic_css() + HiNetHTMLVisitor.hinet_css())
    html_file.write('</style>\n</head>\n<body>\n')
    html_file.write('<h3>%s</h3>\n' % title)
    explanation = '(data flows from top to bottom)'
    html_file.write('<par class="explanation">%s</par>\n' % explanation)
    html_file.write('<br><br><br>\n')
    converter = mdp.hinet.HiNetHTMLVisitor(html_file, show_size=show_size)
    converter.convert_flow(flow=flow)
    html_file.write('</body>\n</html>')
    html_file.close()
    if browser_open:
        webbrowser.open(os.path.abspath(filename))
    return filename
