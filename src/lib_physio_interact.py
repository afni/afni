#!/usr/bin/env python

# Implementation of adding, subtracting and moving points in
# Matplotlib by leveraging polygon functionality, based on this
# original Python demo:
# https://matplotlib.org/stable/gallery/event_handling/poly_editor.html
# 
# We have added in a constraining line for our particular use case, as
# well as consideration of having multiple populations of vertices
# (peaks and troughs).
#
# 

import sys, copy 
import numpy              as     np
from   matplotlib.artist  import Artist
from   matplotlib.lines   import Line2D
from   matplotlib.patches import Polygon
import matplotlib.pyplot  as     plt
 
# -------------------------------------------------------------------------

# make plot styles, which are similar
dict_plotP = {
    'marker'   : 7, 
    'mfc'      : 'k', 
    'mec'      : 'k', 
    'ms'       : 8,
    'alpha'    : 0.5,
    'lw'       : 0,
    'animated' : True,
    }
dict_plotT = copy.deepcopy(dict_plotP)
dict_plotT['marker'] = 6

# --------------------------------------------------------------------------

def compile_inter_xcoor(all_inter, label):
    """Take a list of PolygonInteractor objects, all_inter, and get a list
of peak or trough (as chosen with label) indices.

Parameters
----------
all_inter : list
    a list of PolygonInteractor objects from interactive peak/trough 
    checking
label : str
    can be 'p' or 't', to select peak or trough obj, respectively

Returns
-------
all_x : list
    sorted list of floats, where each int should represent the peak/trough 
    xcoor
"""
    
    ninter = len(all_inter)
    all_x  = []

    for ii in range(ninter):
        inter = all_inter[ii]
        # get xcoord of each pair except the last (= copy of [0]th in polygon)
        if inter.poly[label] != None :
            all_x.extend( list(inter.poly[label].get_xy()[:-1,0]) )
        
    all_x.sort()
    return all_x


# --------------------------------------------------------------------------

class PolygonInteractor:
    """A polygon editor.  Here, the vertices of the polygons being edited
are just made up of 1D arrays of peak and trough locations.

Key-bindings being used:

  '4' : delete the vertex under point; the vertex could represent a
        peak or a trough

  '3' : add a peak vertex

  '2' : add a trough vertex

  '1' : toggle vertex markers on and off.  When vertex markers are on,
        you can add, move and/or delete them

  Left-click : select the closest peak or trough to the click
        location, after which it can be dragged around to a new
        position along the refline.

NB: when deleting a vertex, the mouse must be hovering close to the
desired location (within eps_del).  Similarly, when adding a vertex,
the mouse must be close to the reference line (within eps_ref).  When
moving a vertex, the vertex follows the dragged mousepoint as long as
it is close to the reference line (within 2*eps_ref).

Any vertices that have been moved will be constrained to be on a
refline point.  In theory, at the beginning, vertices might not be on
index locations.

NB: The peak/trough arrays should not be considered sorted after this
operation, though we will try to keep adding/subtracting vertices in
approximately sorted order.

There will always be at least one vertex left (which is, in fact, a
'double point', because this is a closed polygon).

    """

    vert_on = True
    eps_ref = 50        # max pixel distance to count as a vertex hit
    eps_del = 75

    def __init__(self, ax):
        """The input here, ax, is an 'matplotlib.axes._subplots.AxesSubplot'
        object, and it must have:
        + one one added line (to be the refline)
        + one or two added patches (polygons, which are lists of peak or
          trough vertices)

        Note that the line and patch objects each get activately
        updated during the running of this interactive matplotlib
        session.  Therefore, the updated point lists and locations can
        be obtained when done with:
        + ax.lines[0].get_xydata()
        + ax.patches[0].get_xy()
        + ax.patches[1].get_xy()

        """

        npatch = len(ax.patches)
        nlines = len(ax.lines)

        # the ax obj must have at least one patch applied
        if npatch == 0 or ax.patches[0].figure is None:
            etxt = "** ERROR: you must first add the polygon to a figure\n"
            etxt+= "   or canvas before defining the interactor"
            raise RuntimeError(etxt)

        if nlines == 0 :
            etxt = "** ERROR: you must have just one refline added to\n"
            etxt+= "   the ax object, but you have: {}".format(len(ax.lines))
            raise RuntimeError(etxt)

        self.ax   = ax              # fundamental input, contains all else

        # two main ojs: vertex locations
        self.poly = {
            'p' : None,             # peak list
            't' : None,             # trough list
        }
        self.line = {
            'p' : None,             # peak line
            't' : None,             # trough line
        }

        # a main obj: the reference line, to constrain locations of verts.
        ii = self.find_line_refline()
        self.refline = ax.lines[ii]

        # ----- setup [0]th polygon obj (-> peaks); is req and defines canvas
        ii = self.find_patch_poly('p')
        if ii < 0 :
            print("** ERROR: could not find patch 'p' in PolygonInteractor")
            sys.exit(4)

        self.poly['p'] = ax.patches[ii]
        x, y           = zip(*self.poly['p'].xy)
        self.line['p'] = Line2D(x, y, **dict_plotP)
        self.ax.add_line(self.line['p'])
        self.pid       = self.poly['p'].add_callback(self.poly_changedP)

        # ----- setup [1]th polygon obj (-> troughs); is opt and mirrors peaks
        if len(ax.patches) > 1 :
            ii = self.find_patch_poly('t')
            if ii >= 0 :
                self.poly['t'] = ax.patches[ii]
                x1, y1         = zip(*self.poly['t'].xy)
                self.line['t'] = Line2D(x1, y1, **dict_plotT)
                self.ax.add_line(self.line['t'])
                self.tid       = self.poly['t'].add_callback(self.poly_changedT)

        # attributes defining which polygon (if any) is 'active'
        self.act_lab = None  # the label of the active poly
        self.act_ind = None  # the index of the active poly

        # define canvas
        canvas = self.poly['p'].figure.canvas
        canvas.mpl_connect('draw_event', self.on_draw)
        canvas.mpl_connect('button_press_event', self.on_button_press)
        canvas.mpl_connect('key_press_event', self.on_key_press)
        canvas.mpl_connect('button_release_event', self.on_button_release)
        canvas.mpl_connect('motion_notify_event', self.on_mouse_move)
        self.canvas = canvas

    # ---------------------

    @property
    def HAVE_T(self):
        """Simple flag"""
        if self.poly['t'] == None :  return False
        else:                        return True

    def find_line_refline(self):
        """Find and define refline, which we have to find by its
        label. Return the index in the lines list."""

        N = len(self.ax.lines)
        ii = 0
        while ii < N :
            if self.ax.lines[ii].get_label() == 'refline' :
                return ii 
            ii+= 1

        print("** ERROR: could not find refline in PolygonInteractor")
        sys.exit(4)

    def find_patch_poly(self, label):
        """Find index of polygon with correct label.  Return the index in the
        lines list; if not found, return a negative number."""
        N = len(self.ax.patches)
        # have to recognize it by its label
        ii = 0
        while ii < N :
            if self.ax.patches[ii].get_label() == label :
                return ii 
            ii+= 1

        return -1

    def nvert(self, label):
        """Return how many vertices there are in this label class."""
        if self.poly[label] == None :  return -1
        else:                          return len(self.poly[label].xy)

    def on_draw(self, event):
        self.background = self.canvas.copy_from_bbox(self.ax.bbox)
        self.ax.draw_artist(self.poly['p'])
        self.ax.draw_artist(self.line['p'])
        if self.HAVE_T :
            self.ax.draw_artist(self.poly['t'])
            self.ax.draw_artist(self.line['t'])
        # do not need to blit here, this will fire before the screen is
        # updated

    def poly_changedP(self, poly):
        """This method is called whenever the peak-related pathpatch object is
        called."""
        # only copy the artist props to the line (except visibility)
        vis = self.line['p'].get_visible()
        Artist.update_from(self.line['p'], poly)
        self.line['p'].set_visible(vis)  # don't use the poly visibility state

    def poly_changedT(self, poly):
        """This method is called whenever the trough-related pathpatch object
        is called."""
        # only copy the artist props to the line (except visibility)
        vis = self.line['t'].get_visible()
        Artist.update_from(self.line['t'], poly)
        self.line['t'].set_visible(vis)  # don't use the poly visibility state

    def get_ind_under_point_REFLINE(self, event, eps_fac=1.0):
        """Return the index of the refline point closest to the event
        position or *None* if no point is within ``eps_fac * self.epsilon`` to
        the event position."""

        # get locations as display coords
        A  = self.refline.get_data()
        xy = np.column_stack([A[0],A[1]])

        # just use one polygon for refline coord transform
        xyt    = self.poly['p'].get_transform().transform(xy)
        xt, yt = xyt[:, 0], xyt[:, 1]

        # calc all distances, and get index of minimal one
        d        = np.hypot(xt - event.x, yt - event.y)
        rindseq, = np.nonzero(d == d.min())
        rind     = rindseq[0]

        # are we too far to be selected?
        if d[rind] >= self.eps_ref*eps_fac :
            return None, None, None, None, None

        # return refline index and 2 forms of coord, if selected
        return rind, xt[rind], yt[rind], xy[rind, 0], xy[rind, 1]

    def get_ind_under_point(self, event):
        """Return the dictionary label ('p', 't', etc.) and index of the point
        closest to the event position, or empty string and None if no
        point is within ``self.epsilon`` to the event position."""

        # distance and index in p-list
        lab = 'p'
        dist, ind = self.get_closest_point(event, 'p')

        # dist and idx in t-list, with comparison and possible replacement
        if self.HAVE_T :
            distt, indt = self.get_closest_point(event, 't')

            # if this list is closer, replace all vals
            if distt < dist :
                dist = distt
                ind  = indt
                lab  = 't'

        # case in which we aren't close enough to anything
        if dist >= self.eps_del :
            return '', None
        
        # return label of which vertices are selected, and the index there
        return lab, ind

    def get_closest_point(self, event, label):
        """Return distance and index between event click and closest array
        point in label (='p', 't', etc.) list. Return None if the
        distance is outside prescribed epsilon dist."""

        # display coords
        xy     = np.asarray(self.poly[label].xy)
        xyt    = self.poly[label].get_transform().transform(xy)
        xt, yt = xyt[:, 0], xyt[:, 1]

        # calc distances from vertices to event, and get index of minimal one
        d       = np.hypot(xt - event.x, yt - event.y)
        indseq, = np.nonzero(d == d.min())
        ind     = indseq[0]

        # return distance and the index of that location
        return d[ind], ind

    def on_button_press(self, event):
        """Callback for mouse button presses."""
        if not self.vert_on:
            return
        if event.inaxes is None:
            return
        if event.button != 1:
            return
        self.act_lab, self.act_ind = self.get_ind_under_point(event)

    def on_button_release(self, event):
        """Callback for mouse button releases."""
        if not self.vert_on:
            return
        if event.button != 1:
            return
        self.act_lab, self.act_ind = None, None

    def on_key_press(self, event):
        """Callback for key presses."""
        all_lab = []  # list of labs to update, and for redrawing canvas

        if not event.inaxes:
            return

        if event.key == '4':
            # delete either 'p' or 't' element
            lab, ind = self.get_ind_under_point(event)
            if ind is not None:
                self.poly[lab].xy = np.delete(self.poly[lab].xy, ind, axis=0)
                self.line[lab].set_data(zip(*self.poly[lab].xy))
                all_lab.append(lab)

        elif event.key == '3' :
            # add vertex to 'p'
            is_bad = self.add_vertex(event, 'p')
            all_lab.append('p')

        elif event.key == '2' and self.HAVE_T :
            # add vertex to 't'
            is_bad = self.add_vertex(event, 't')
            all_lab.append('t')

        elif event.key == '1':
            # toggle between: seeing+editing verts, and not doing so.
            self.vert_on = not self.vert_on
            # apply new state to p and t
            self.line['p'].set_visible(self.vert_on)
            all_lab.append('p')
            if self.HAVE_T :
                self.line['t'].set_visible(self.vert_on)
                all_lab.append('t')
            # if we don't see verts, de-select current actionable label
            if not self.vert_on :
                self.act_lab, self.act_ind = None, None

        # make sure canvas updates any changed objects
        for lab in all_lab:
            if self.line[lab].stale :
                self.canvas.draw_idle()

    def add_vertex(self, event, lab):
        """Add one of the lab type of vertices, depending on where the event
        occurred."""

        # get info about nearest location on refline
        rind, xval, yval, xdataval, ydataval \
            = self.get_ind_under_point_REFLINE(event)

        # if close enough to refline, go into action
        if rind != None :
            
            allx = self.poly[lab].xy[:,0]  # all xcoor in this poly
            N    = len(allx)               # num of verts in this polygon

            # search for good place to add vert in polygon list 
            ii    = 0
            FOUND = False
            while ii < N-1 :
                if xdataval < allx[ii] :
                    break
                ii+= 1
                
            # ... and add it
            self.poly[lab].xy = np.insert(
                self.poly[lab].xy, ii,
                [xdataval, ydataval],
                axis=0)
            self.line[lab].set_data(zip(*self.poly[lab].xy))

        return 0

    def on_mouse_move(self, event):
        """Callback for mouse movements."""
        if not self.vert_on:
            return
        if self.act_ind is None:
            return
        if event.inaxes is None:
            return
        if event.button != 1:
            return
        
        # check if we can drop it down to the line
        rind, xval, yval, xdataval, ydataval \
                = self.get_ind_under_point_REFLINE(event, eps_fac=2.0)

        # constrained motion
        if rind != None :
            x, y = xdataval, ydataval
            self.poly[self.act_lab].xy[self.act_ind] = x, y

        if self.act_ind == 0:
            self.poly[self.act_lab].xy[-1] = x, y
        elif self.act_ind == len(self.poly[self.act_lab].xy) - 1:
            self.poly[self.act_lab].xy[0] = x, y
        self.line[self.act_lab].set_data(zip(*self.poly[self.act_lab].xy))

        # updates in image (keep drawing both p and t each time)
        self.canvas.restore_region(self.background)
        self.ax.draw_artist(self.poly['p'])
        self.ax.draw_artist(self.line['p'])
        if self.HAVE_T :
            self.ax.draw_artist(self.poly['t'])
            self.ax.draw_artist(self.line['t'])
        self.canvas.blit(self.ax.bbox)


if __name__ == '__main__':

    tvalues1 = np.linspace(-2, 2, 500)
    tvalues2 = np.linspace(-2, 2, 30)
    tvalues3 = np.linspace(-2, 2, 20)
    tvalues1b = copy.deepcopy(tvalues1)
    tvalues2b = copy.deepcopy(tvalues2)
    tvalues3b = copy.deepcopy(tvalues3)

    A1 = np.cos(2*np.pi*tvalues1)
    A2 = np.cos(2*np.pi*tvalues2)
    A3 = np.cos(2*np.pi*tvalues3)
    A1b = copy.deepcopy(A1)
    A2b = copy.deepcopy(A2)
    A3b = copy.deepcopy(A3)

    # create poly, and set shape to be transparent; just need [3]rd
    # element to be 0
    poly = Polygon(np.column_stack([tvalues2, A2]), animated=True)
    poly.set_color([0, 0, 0, 0])
    poly2 = Polygon(np.column_stack([tvalues2b, A2b]), animated=True)
    poly2.set_color([0, 0, 0, 0])

    polyT = Polygon(np.column_stack([tvalues3, A3]), animated=True)
    polyT.set_color([0, 0, 0, 0])
    polyT2 = Polygon(np.column_stack([tvalues3b, A3b]), animated=True)
    polyT2.set_color([0, 0, 0, 0])

    # refline - can't provide same one as ulay to both 
    lll  = Line2D(tvalues1, A1)
    lll2 = Line2D(tvalues1b, A1b)

    fff   = plt.figure( 'fname.img', figsize=(12,6) )
    subpl = fff.subplots( 2, 1)

    # --------------------------------
    pp = subpl[0]

    #fig, ax = plt.subplots()
    pp.add_line(lll)
    pp.add_patch(poly)
    pp.add_patch(polyT)
    P0 = PolygonInteractor(pp)

    pp.set_title('Click and drag a point to move it')
    pp.set_xlim((-2, 2))
    pp.set_ylim((-2, 2))

    # --------------------------------
    pp2 = subpl[1]

    #fig, ax = plt.subplots()
    pp2.add_line(lll2)
    pp2.add_patch(poly2)
    pp2.add_patch(polyT2)
    P1 = PolygonInteractor(pp2)

    pp2.set_title('Click and drag a point to move it2')
    pp2.set_xlim((-2, 2))
    pp2.set_ylim((-2, 2))



    #plt.ion()
    plt.show()
