
.. _SurfCont:

The Surface Controller:
-----------------------

The surface controller is for controlling the way surfaces and datasets defined over them are displayed. The same controller is shared by a :term:`family of surfaces <Family of surfaces>` and all the datasets displayed on them. Left and Right surfaces have separate controllers though in most cases actions on one hemisphere's controller are automatically mirrored on the contralateral side. The surface controller is initialized by the currently selected surface - the one said to be in focus.
You can launch the :ref:`Surface Controller <SurfCont>` with: :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Surface Controller`

.. figure:: media/SurfaceController.jpg
   :align: center


   ..




.. _SurfCont->Surface_Properties:

Surface Properties:
===================

Block providing information about selected surface.

   .. _SurfCont->Surface_Properties->more:

**more**: More info on Surface


Opens a dialog with detailed information about the object in geek speak.

   .. _SurfCont->Surface_Properties->Drw:

**Drw**: Choose the rendering (drawing) mode for this surface.


Choose the rendering mode for this surface.
   Viewer: Surface's rendering mode is set by the viewer's setting which can be changed with the :ref:`'p'<LC_p>` option.

   Fill:   Shaded rendering mode.

   Line:   Mesh rendering mode.

   Points: Points rendering mode.



   .. _SurfCont->Surface_Properties->Trn:

**Trn**: Choose the transparency for this surface.


Set the transparency for this surface to one of the following options.
   Viewer: Surface's transparency is set            by the viewer's setting which can            be changed with the 'o', 'O' options.

   0 :   No transparency, opaque.

   ...

   16:   Maximum transparency, invisibile



   .. _SurfCont->Surface_Properties->Dsets:

**Dsets**: Show/Hide Dataset (previously Color Plane) controllers



.. _SurfCont->Xhair_Info:

Crosshair Information:
======================


   .. _SurfCont->Xhair_Info->Xhr.r00:

**Xhr**: Crosshair coordinates.


Set/Get crosshair location in mm :term:`RAI` on
this controller's selected object.
Entering new coordinates 
makes the crosshair jump
to that location (like :ref:`'ctrl+j' <LC_Ctrl+j>`).
Use :ref:`'alt+l'<LC_Alt+l>` to center the
cross hair in your viewer.

   .. _SurfCont->Xhair_Info->Node.r00:

**Node**: Node index


Index of node in focus (1) and :term:`RAI` coordinates of that node (2).

1- The index is of the node in focus on this controller's surface. Nodes in focus are highlighted by the blue sphere in the crosshair.
This cell is editable; manually entering a new node's index will put that node in focus and send the crosshair to its location (like :ref:`'j' <LC_j>`). Use :ref:`'alt+l' <LC_Alt+l>` to center the cross hair in your viewer.

2- The :term:`RAI` coordinates are those of the surface node after all spatial transformations have been applied to the surface. Those transformations do not include visualization transformations applied in the viewer

   .. _SurfCont->Xhair_Info->Tri.r00:

**Tri**: 1- Triangle index, 2- Nodes forming tiangle


1- Triangle (faceset) index of triangle in focus on this  on this controller's surface.
Triangle in focus is highlighted in gray, and entering a new triangle's index will set a new triangle in focus (like :ref:`'J'<UC_j>`).

2- Indices of nodes forming triangle.



   .. _SurfCont->Xhair_Info->Val.c00:

**Val.c00**: Data Values at node in focus


Data values at node in focus. :term:`Intensity`, :term:`Threshold`, and :term:`Brightness` show the triplets of values  at the selected node that correspond to the dataset column choices in :ref:`I<SurfCont->Dset_Mapping->I>`, :ref:`T<SurfCont->Dset_Mapping->T>`, and :ref:`B<SurfCont->Dset_Mapping->B>` selectors.

   .. _SurfCont->Xhair_Info->Lbl.r00:

**Lbl**: Color at node in focus


Labels available at the selected datum.
If nothing is available, datum color
is displayed.

.. _SurfCont->Dset_Controls:

Dset Controls:
==============


   .. _SurfCont->Dset_Controls->Lbl+Par.r00:

**Lbl+Par**: Label of Dset


Label of dataset currently selected. Note that what you're viewingat this moment maybe a blend of multiple datasets. See :ref:`color mixing<ColorMixing>` for details.

   .. _SurfCont->Dset_Controls->Ord:

**Ord**: Order of Dset's colorplane.


Order of Dset's colorplane in the stack of all colorplanes of the parent surface.
The datset with highest order number is 
on top of the stack. Separate 
:ref:`stacks<Plane_Layering>` exist for foreground (fg:)
and background planes (bg:).

See :ref:`Color Mixing<ColorMixing>` for details on how colors are merged.

   .. _SurfCont->Dset_Controls->Opa:

**Opa**: Opacity of Dset's colorplane.


Opacity of Dset's colorplane.
Opaque planes have an opacity
of 1, transparent planes have
an opacity of 0. 
Opacities are used when mixing
planes within the same :ref:`group<Color_Plane_Grouping>`  
foreground (fg:) or background(bg:).

Opacity values are not applied
to the first plane in a group.
Consequently, if you have just
one plane to work with, opacity 
value is meaningless.

Color mixing can be done in two 
ways, use :ref:`F7<F7>` to toggle between 
mixing modes.


   .. _SurfCont->Dset_Controls->Dim:

**Dim**: Dimming factor to apply to colormap or color datasets.


Dimming factor to apply to colormap
before mapping the intensity (I) data.
The colormap, if displayed on the right,
is not visibly affected by Dim but the
colors mapped onto the surface, voxel grid, tracts, etc. are.
For RGB Dsets (e.g. .col files, or tract colors), Dim is
applied to the RGB colors directly.

Decreasing Dim is useful when the colors are too saturated
for lighting to reflect the object terrain.
When in doubt, just press the button and see what happens.

   .. _SurfCont->Dset_Controls->Dsp:

**Dsp**: Choose the rendering mode for this dataset.


Choose the viewing mode for this dataset.
   Col: Colours, only.

   Con: Contours (slower), only.

   C&C: Colours and Contours (slower), only.

   XXX: Unfortunately nothing, only.

 There is one contour created for each color  in the colormap. You'd want to use colormaps with  few colors to get a contour of use.  Contours are not created if colormap has panes  of unequal sizes.


   .. _SurfCont->Dset_Controls->1:

**1**: Show ONLY ONE selected Dset. Foreground only. 


If ON, view only the selected
Dset's colors. No mixing of colors in the
foreground stack is done.

If OFF, then mix the color planes
in the foreground stack.

This option makes it easy to view 
one Dset's colors at a time without
having to worry about color mixing,
opacity, and stacking order.

Needless to say, options such as
'Ord:' and 'Opa:' in this panel are 
of little use when this button is ON.

   .. _SurfCont->Dset_Controls->Switch_Dset:

**Switch_Dset**: Switch between datasets


Switch between datasets.

   .. _SurfCont->Dset_Controls->Load_Dset:

**Load_Dset**: Load a new dataset 


Load a new dataset (Dset).
Datasets can be of 3 formats:

1- NIML (.niml.dset)   :This format is internal to AFNI/SUMA. 

2- GIFTI (.gii.dset):The format to end all formats.

3- 1D   (.1D.dset)   :Simple ASCII tabular format supporting numerical values only.
     Each row i contains Nj data values per node.Since this format has no headerassociated with it, it makessome assumption about the datain the columns. 

   You can choose from 3 options:  (see below for nomenclature)

   - Each column has Ni values where Ni = N_Node.  In this case, it is assumed that row i has values for node i on the surface.
   - If Ni is not equal to N_Node then SUMA will check to see if column 0 (Col_0) is all integers with values v satisfying:  0 <= v < N_Node .
     If that is the case then column 0 contains the node indices. The values in row j of Dset are for the node indexed Col_0[j].
     In the sample :term:`1D` Dset shown below assuming N_Node > 58, SUMA will consider the 1st column to  contain node indices. In that case the values -12.1 and 0.9 are for  node 58 on the surface.
   - Lastly, if Col_0 fails the node index test, then SUMA considers the data in row i to be associated with node i.

   If you're confused, try creating some toy datasets like the one below and  load them into SUMA.

   Sample 1D Dset (Call it pickle.1D.dset):
     25    22.7     1.2   
     58    -12.1    0.9   

   Nomenclature and conventions:
     - N_Node is the number of nodes forming the surface.
     - Indexing always starts at 0. In the example, value v at  row 0, column 1 is v = 22.7 .
     - A Dset has Ni rows and Nj columns. In other terms, Ni is the number of values per node and Nj is the number of nodes for which data are specified in Dset. Ni = 2, Nj = 3 in the example.

   .. _SurfCont->Dset_Controls->Load_Col:

**Load_Col**: Load a new color plane 


Load a new color plane.
A color plane is a :term:`1D` text file with 
each row formatted as such

  n  r g b

where n is the node index, 
r, g, and b are the red, green and blue
color values, respectively. 
Color values must be between 0 and 1.0. 
A sample file would be: test.1D.col with content::

   0    0.1 0.2 1   
   1    0   1   0.8 
   4    1   1   1   
   7    1   0   1   
   14   0.7 0.3 0   

.. _SurfCont->Dset_Mapping:

Dset Color Mapping:
===================


   .. _SurfCont->Dset_Mapping->IxT:

**IxT**: Set I, T selection linking modes.


Switch between methods for the automatic linking of I, T selectors.

None: Do nothing.

Same: Set the T selector to match the I selection.

Stat: Swith T selector to match an I selection with 
'    'an obvious statistic. Matching is based on labels.

You can set your preference using environment variable
   SUMA_IxT_LinkMode


   .. _SurfCont->Dset_Mapping->I:

**I**: Select Intensity (I) column, aka sub-brick. 


Use this menu to select which column in the dataset (Dset) should be used for an Intensity (I)measure.

Values in (I) are the ones that get colored by the colormap,however, no coloring is done if the :ref:`'v'<SurfCont->Dset_Mapping->I->v>` button on the right isturned off.

The (I) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` tableof the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.
The value is also shown in the SUMA viewer

You can use a different type of selector to set (I). A right-click on 'I' opens a list widget, which is better when you have many columns from which to choose.

The style of this selector can also change depending on the numberof sub-bricks (columns) you have in your dataset. If the numberexceeds a threshold specified by the environment variable :ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`


   .. _SurfCont->Dset_Mapping->I->v:

**v**: View (ON)/Hide Dset node colors



   .. _SurfCont->Dset_Mapping->T:

**T**: Select Threshold (T) column, aka sub-brick. 


Use this menu to select which column in the
dataset (Dset) should be used for a Threshold (T) measure.

T values are the ones used to determine if a :term:`datum` gets colored based on its (I) value.

A node n is not colored if:

    T(n)   < Tscale   

or if :ref:`'\|T\|'<SurfCont->Dset_Mapping->abs_T>` option below is turned ON: 

  | T(n) | < Tscale .

Thresholding is not applied when the :ref:`'v'<SurfCont->Dset_Mapping->T->v>` button on the right is turned off.

The (T) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` tableof the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.
The value is also shown in the SUMA viewer

You can use a different type of selector to set (T). A right-click on 'T' opens a list widget, which is better when you have many columns from which to choose.

The style of this selector can also change depending on the numberof sub-bricks (columns) you have in your dataset. If the numberexceeds a threshold specified by the environment variable :ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`


   .. _SurfCont->Dset_Mapping->T->v:

**v**: Apply (ON)/Ignore thresholding



   .. _SurfCont->Dset_Mapping->B:

**B**: Select Brightness (B) column, aka sub-brick. 


Use this menu to select which column in the dataset (Dset) should be used for color Brightness (B) modulation.

(B) values are the ones used to control the brightness of a :term:`datum's<datum>` color.

Brightness modulation is controlled by ranges in the 'B' cells of the table below.

Brightness modulation is not applied when the :ref:`'v'<SurfCont->Dset_Mapping->B->v>` button on 
the right is turned off.

The (B) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` tableof the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.
The value is also shown in the SUMA viewer

You can use a different type of selector to set (B). A right-click on 'B' opens a list widget, which is better when you have many columns from which to choose.

The style of this selector can also change depending on the numberof sub-bricks (columns) you have in your dataset. If the numberexceeds a threshold specified by the environment variable :ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`


   .. _SurfCont->Dset_Mapping->B->v:

**v**: View (ON)/Ignore brightness modulation



   .. _SurfCont->Dset_Mapping->SetRangeTable.c00:

**SetRangeTable.c00**: Clipping ranges


Used for setting the clipping ranges. Clipping is only done for 
color mapping. Actual data 
values do not change.

   .. _SurfCont->Dset_Mapping->SetRangeTable.r01:

**SetRangeTable.r01**: Intensity clipping range (append '%' for percentiles, see BHelp)


Intensity clipping range rules:

   Values in the intensity data that are less than Min are colored by the first (bottom) color of the colormap. 

   Values larger than Max are mapped to the top color.

   Intermediate values are mapped according to the :ref:`'Col'<SurfCont->Dset_Mapping->Col>` menu below.

You can set the range as a percentile of the dataset's values by appending  '%' to the percentile for Min and/or Max such as 5% or 90%. Note that the percentile always gets replaced by the actual value in the dataset.

A left-click on 'I' locks ranges from automatic resetting, and the locked range applies to the current Dset only. A locked range is indicated with the reverse video mode.

A right-click resets values to the default range (usually 2% to 98%) in the dataset.

   .. _SurfCont->Dset_Mapping->SetRangeTable.r02:

**SetRangeTable.r02**: Brightness modulation clipping range 


Values in the brightness (B) :ref:`column<SurfCont->Dset_Mapping->B>` are clipped to the Min to Max range in this row before calculating their modulation factor per the values in the next table row.

You can set the range as a percentile of the dataset's values by appending  '%' to the percentile for Min and/or Max such as 8% or 75%. Note that the percentile always gets replaced by the actual value in the dataset.

A left-click locks ranges in this row from automatic resetting, and a locked range is applied to the current Dset only. A locked range is indicated with the reverse video mode.

A right-click resets values to the default range (usually 2% to 98%) for the dataset.

   .. _SurfCont->Dset_Mapping->SetRangeTable.r03:

**SetRangeTable.r03**: Brightness modulation factor range 


Brightness modulation factor range.
Brightness modulation values, after
clipping per the values in the row above,
are scaled to fit the range specified
here.

   .. _SurfCont->Dset_Mapping->Col:

**Col**: Switch between color mapping modes.


Switch between modes for mapping values to the color map.

The bottom color of the map C0 maps to the minimum value in the :ref:`I range<SurfCont->Dset_Mapping->SetRangeTable.r01>` row, and the top color to the maximum value. Colors for values in between the minimum and maximum of :ref:`I range<SurfCont->Dset_Mapping->SetRangeTable.r01>`, the following methods apply

Int: Interpolate linearly between colors in colormap to find color at

   icol=((V-Vmin)/Vrange * Ncol) 

NN : Use the nearest color in the colormap. The index into the colormap of Ncol colors is given by 

   icol=floor((V-Vmin)/Vrange * Ncol) 

with icol clipped to the range 0 to Ncol-1

Dir: Use intensity values as indices into the colormap. In Dir mode, the intensity  clipping range is of no use.

   icol=floor(V) with clipping to the range 0 to Ncol-1

   .. _SurfCont->Dset_Mapping->Bias:

**Bias**: Coordinate bias direction


Coordinate bias direction.

-: No bias thank you

x: X coord bias

y: Y coord bias

z: Z coord bias

n: bias along node's normal


See more info in Bhelp for
'C' table entry above.

This option will produce
'Extremely Cool'[1] images.
[1] Chuck E. Weiss (Slow River/Rykodisc) 1999.

   .. _SurfCont->Dset_Mapping->Cmp:

**Cmp**: Switch between available color maps. 


Switch between available color maps.
If the number of colormaps is too large
for the menu button, right click over
the 'Cmp' label and a chooser with a 
slider bar will appear.

More help is available via
ctrl+h while mouse is over the
colormap.

   .. _SurfCont->Dset_Mapping->Cmp->New:

**New**: Load new colormap


Load new colormap.
Loaded map will replace a
pre-existing one with the
same name.

See ScaleToMap -help for 
details on the format of 
colormap file. The formats
are described in the section
for the option -cmapfile.

A sample colormap would be::

 0 0 1
 1 1 1
 1 0 0

saved into a cmap file called
cmap_test.1D.cmap

   .. _SurfCont->Dset_Mapping->abs_T:

**abs_T**: Absolute threshold ON/OFF


Toggle Absolute thresholding.

OFF: Mask node color for nodes that have:   T(n) < Tscale

ON:  Mask node color for nodes that have:
| T(n) | < Tscale
where:

Tscale is the value set by the threshold scale.

T(n) is the node value in the selected threshold column (T). This value is seen in the second cell of the 'Value' table on the left side.

   .. _SurfCont->Dset_Mapping->sym_I:

**sym_I**: Intensity range symmetry about 0 


Toggle Intensity range symmetry about 0. 

ON : Intensity clipping range is forced to go from  -val to val. This allows you to mimic AFNI's ranging mode.

OFF: Intensity clipping range can be set to your liking.

   .. _SurfCont->Dset_Mapping->shw_0:

**shw_0**: Color masking of nodes with intensity = 0 


Toggle color masking of nodes with intensity = 0 

ON : 0 intensities are mapped to the colormap as any other values.

OFF: 0 intensities are masked, a la AFNI

   .. _SurfCont->Dset_Mapping->Cmap->bar:

**bar**: Colorbar for 'I' values


Colorbar used for colorizing values in 'I' sub-brick.
Colorization depends on the settings of ...
Use ctrl+h over the colorbar for help on manipulating the displayed
map.


   .. _SurfCont->Dset_Mapping->Cmap->scale:

**scale**: Set the threshold for 'T' values


Set threshold value to determine which nodes/voxels/edges will get coloredVoxels for which the value in the 'T' sub-brick is below that of the threshold will not get colored.

   .. _SurfCont->Dset_Mapping->Clst.c00:

**Clst.c00**: Clusterizing options


Used for setting the clustering parameters.

   .. _SurfCont->Dset_Mapping->Clst.c01:

**Clst.c01**: Connectedness criterion


Minimum distance between nodes.
Nodes closer than the minimum distance are in
same cluster. If you want to distance to be in
number of edges (N) separating nodes, set the minimum
distance to -N. This parameter is the same as -rmm in
the program SurfClust

   .. _SurfCont->Dset_Mapping->RangeTable.c00:

**RangeTable.c00**: Full range in Dset


Full range of values in Dset.

   .. _SurfCont->Dset_Mapping->RangeTable.r01:

**RangeTable.r01**: Range of values in intensity (I) column



   .. _SurfCont->Dset_Mapping->RangeTable.r02:

**RangeTable.r02**: Range of values in threshold (T) column



   .. _SurfCont->Dset_Mapping->RangeTable.r03:

**RangeTable.r03**: Range of values in brightness (B) column



   .. _SurfCont->Dset_Mapping->RangeTable.c01:

**RangeTable.c01**: Minimum value in Dset column


Minimum value in Dset column.

   .. _SurfCont->Dset_Mapping->RangeTable.c02:

**RangeTable.c02**: Node index at minimum


Node index at minimum.
Right click in cell to
have crosshair jump to
node's index.
Same as 'ctrl+j' or
an entry in the 'Node' cell
under Xhair Info block.

   .. _SurfCont->Dset_Mapping->RangeTable.c03:

**RangeTable.c03**: Maximum value in Dset column


Maximum value in Dset column.

   .. _SurfCont->Dset_Mapping->RangeTable.c04:

**RangeTable.c04**: Node index at maximum


Node index at maximum.
Right click in cell to
have crosshair jump to
node's index.
Same as 'ctrl+j' or
an entry in the 'Node' cell
under Xhair Info block.

