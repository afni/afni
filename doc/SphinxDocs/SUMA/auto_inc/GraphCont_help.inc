
.. _GraphCont:

The Graph Dataset Controller:
-----------------------------

The graph controller is for controlling the way graphs (matrices) are rendered.  Each graph gets its own controller. You can use the switch button above to switch between them. The graph controller is initialized by the graph of the last selected edge/cell.
 After you have selected an edge, you can launch the :ref:`Graph Controller <VolCont>` with: :ref:`ctrl+s <LC_Ctrl+s>` or :menuselection:`View-->Object Controller`

.. figure:: media/GraphController.jpg
   :align: center


   ..




.. _GraphCont->Graph_Dset_Properties:

Properties of graph dset:
=========================


   .. _GraphCont->Graph_Dset_Properties->more:

**more**: More info on Graph Dset


Opens a dialog with detailed information about the object in geek speak.

.. _GraphCont->Xhair_Info:

Crosshair Information:
======================


   .. _GraphCont->Xhair_Info->Xhr.r00:

**Xhr**: Crosshair coordinates.


Set/Get crosshair location in mm :term:`RAI` on
this controller's selected object.
Entering new coordinates 
makes the crosshair jump
to that location (like :ref:`'ctrl+j' <LC_Ctrl+j>`).
Use :ref:`'alt+l'<LC_Alt+l>` to center the
cross hair in your viewer.

   .. _GraphCont->Xhair_Info->Edge.r00:

**Edge**: 1- Edge index, 2- Nodes forming (directed) edge


1- Edge/Cell Index:  Get/Set index of edge/cell in focus on this controller's graph. This number is the :term:`1D index` of the edge/cell in the graph/matrix. Consider it the equivalent of a voxel 1D index in a volume, or a node in a surface dataset. 
Entering a new edge's index will put that edge  in focus and send the crosshair to its center (like :ref:`j<LC_j>`). Use :ref:`alt+l<LC_Alt+l>` to center the cross hair in your viewer.

Note that an edge can be formed by a pair of identical nodes - think matrix diagonal.

2- Nodes Forming Directed Edge/Cell: For a cell, this would its pair of row and column indices into the matrix. For a graph, this would be the indices of the nodes forming the directed edge.

   .. _GraphCont->Xhair_Info->Node.r00:

**Node**: Closest Node index


Index of the node closest to the selection location on the edge's representation.

   .. _GraphCont->Xhair_Info->Val.c00:

**Val.c00**: Data Values at Edge in Focus


Data values at edge in focus

   .. _GraphCont->Xhair_Info->Lbl.r00:

**Lbl**: Label of edge in focus


Color from the selected Dset
at the edge in focus.
If nothing is available, node color
is displayed.

.. _GraphCont->GDset_Controls:

Control appearance of 3D graphs and matrices:
=============================================


   .. _GraphCont->GDset_Controls->Dim:

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

   .. _GraphCont->GDset_Controls->Bundles:

**Bundles**: Show bundles instead of edges if possible.


Show bundles instead of edges between nodes if 
the graph dataset contains such information. For
the moment, only 3dProbTrackID creates such data.

   .. _GraphCont->GDset_Controls->CN:

**CN**: How to display connection to selected graph node.


Choose how connections to the selected node are displayed.
   Edg: Show with edges, a subset of the whole graph

   Col: Change the color of the connecting nodes, based on edge value

   Rad: Change the radius of the connecting nodes, based on edge value

   CaR: Both Col and Rad

   XXX: Do nothing special, keep showing whole graph.



   .. _GraphCont->GDset_Controls->Rd:

**Rd**: Choose the sizing option for graph nodes.


Choose the radius sizing for nodes of this dataset.
   Const: All nodes have a radius of 1 x Gain.

   Val: Nodes size equals its dset value x Gain

   XXX: Show no balls.



   .. _GraphCont->GDset_Controls->Rd->Gn:

**Gn**: Gain factor to apply to node radius.


Gain to apply to node radius.
This multiplier is always applied to whatever
radius value the node gets, whether is it constant
or data derived.


   .. _GraphCont->GDset_Controls->Br:

**Br**: Choose the partition ratio of matrix.


Choose the partition ratio of the matrix border.
   XX: No partition.

   5: Border is 1/5 of cell width.

   10: Border is 1/10 of cell width.

   20: Border is 1/20 of cell width.

   30: Border is 1/30 of cell width.

   40: Border is 1/40 of cell width.



   .. _GraphCont->GDset_Controls->Fo:

**Fo**: Choose the font for graph node labels.


Choose the font size for labels of nodes.
   8: 8x13.

   9: 9x15.

   TR10: Times New Roman 10.

   HE10: Helvetica 10.

   HE12: Helvetica 12.

   HE18: Helvetica 18.

   TR24: Times New Roman 24.

   XXX: Show no text.



   .. _GraphCont->GDset_Controls->Cl:

**Cl**: Choose the coloring option for graph nodes.


Choose the colorization method for nodes of this dataset.
   White: Alle weiss.

   Black: Tutti nero.

   Red: Sve crveno.

   Green: Killon akhdar.

   Blue: Tous bleu.

   Yellow: Todos amarillo.

   Gray50: Not there yet.

   Val: Nodes color is based its dset value and the chosen colormap



   .. _GraphCont->GDset_Controls->Sh:

**Sh**: Choose the partition ratio of matrix.


Choose the variants for how labels are handled.
   T : Text shown unless more than 50% occluded.

   Ts: Foreground text bright, occluded text shaded

   B : Text shown with background box unless more than 50% occluded

   Bs: Foreground text with background, occluded text shaded

   Ta: All text shown, occlusions be damned.

   Ba: All text shown with background, damn the torpedoes.



   .. _GraphCont->GDset_Controls->U:

**U**: Show Unconnected Graph Points.


Show graph nodes even if unconnected to other nodes.


   .. _GraphCont->GDset_Controls->Th:

**Th**: Choose the thickness option for graph edges.


Choose the thickness sizing for edges of this graph dataset.
   Const: All nodes have a radius of 1 x Gain.

   Val: Nodes size equals its dset value x Gain



   .. _GraphCont->GDset_Controls->Th->Gn:

**Gn**: Gain factor to apply to edge thickness.


Gain to apply to edge thickness.
This multiplier is always applied to whatever
thickness value the edge gets, whether is it constant
or data derived.


   .. _GraphCont->GDset_Controls->St:

**St**: Choose the stippling option for graph edges.


Choose the stippling for edges of this graph dataset.
   1 :   One pixel/16 off, almost solid

   ...

   15:   15/16 pixels off, almost invisible

   Val: Set stippling based on the dset value

   XXX: No stippling, solid line.



.. _GraphCont->GDset_Mapping:

Control mapping of edge/cell values to color map:
=================================================


   .. _GraphCont->GDset_Mapping->IxT:

**IxT**: Set I, T selection linking modes.


Switch between methods for the automatic linking of I, T selectors.

None: Do nothing.

Same: Set the T selector to match the I selection.

Stat: Swith T selector to match an I selection with 
'    'an obvious statistic. Matching is based on labels.

You can set your preference using environment variable
   SUMA_IxT_LinkMode


   .. _GraphCont->GDset_Mapping->I:

**I**: Select Intensity (I) column, aka sub-brick. 


Use this menu to select which column in the dataset (Dset) should be used for an Intensity (I)measure.

Values in (I) are the ones that get colored by the colormap,however, no coloring is done if the :ref:`'v'<SurfCont->Dset_Mapping->I->v>` button on the right isturned off.

The (I) value for the selected :term:`datum` (n) is shown in the :ref:`'Val'<SurfCont->Xhair_Info->Val.c00>` tableof the :ref:`'Xhair Info' <SurfCont->Xhair_Info>` section on the left.
The value is also shown in the SUMA viewer

You can use a different type of selector to set (I). A right-click on 'I' opens a list widget, which is better when you have many columns from which to choose.

The style of this selector can also change depending on the numberof sub-bricks (columns) you have in your dataset. If the numberexceeds a threshold specified by the environment variable :ref:`SUMA_ArrowFieldSelectorTrigger<SUMA_ArrowFieldSelectorTrigger>`


   .. _GraphCont->GDset_Mapping->I->v:

**v**: View (ON)/Hide graph edge colors


View (ON)/Hide Dset node colors

   .. _GraphCont->GDset_Mapping->T:

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


   .. _GraphCont->GDset_Mapping->T->v:

**v**: Apply (ON)/Ignore thresholding



   .. _GraphCont->GDset_Mapping->B:

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


   .. _GraphCont->GDset_Mapping->B->v:

**v**: View (ON)/Ignore brightness modulation



   .. _GraphCont->GDset_Mapping->ThrVal[0]:

**ThrVal[0]**: Threshold Value (append 'p' to set by p value, '%' to set by percentile)


Set/Get the threshold value.
When statistical parameters are set under :ref:`T <VolCont->Dset_Mapping->T>`, you can 
append a 'p' to set by the p value, as in 0.001p.

For percentile thresholding, append a '%' to
the value, such as 25%


   .. _GraphCont->GDset_Mapping->Cmap->bar:

**bar**: Colorbar for 'I' values


Colorbar used for colorizing values in 'I' sub-brick.
Colorization depends on the settings of ...
Use ctrl+h over the colorbar for help on manipulating the displayed
map.


   .. _GraphCont->GDset_Mapping->Cmap->scale:

**scale**: Set the threshold for 'T' values


Set threshold value to determine which nodes/voxels/edges will get coloredVoxels for which the value in the 'T' sub-brick is below that of the threshold will not get colored.

   .. _GraphCont->GDset_Mapping->Cmap->pval:

**pval**: Nominal p-value per node; FDR q-value


Shows the estimated significance
(p-value) of the threshold above,
if possible.

   * If not possible, will display as '[N/A]' instead.

   * p's that display as 1.2-7 should be interpreted as 1.2 x 10^(-7)

   * p-value here is significance PER NODE/VOXEL/etc.

* If FDR curves are pre-computed in the dataset's header, then the False Discovery Rate q-value will also be shown.

* You can add FDR curves to a dataset with '3drefit -addFDR'.


   .. _GraphCont->GDset_Mapping->SetRangeTable.c00:

**SetRangeTable.c00**: Clipping ranges


Used for setting the clipping ranges. Clipping is only done for 
color mapping. Actual data 
values do not change.

   .. _GraphCont->GDset_Mapping->SetRangeTable.r01:

**SetRangeTable.r01**: Intensity clipping range (append '%' for percentiles, see BHelp)


Intensity clipping range rules:

   Values in the intensity data that are less than Min are colored by the first (bottom) color of the colormap. 

   Values larger than Max are mapped to the top color.

   Intermediate values are mapped according to the :ref:`'Col'<SurfCont->Dset_Mapping->Col>` menu below.

You can set the range as a percentile of the dataset's values by appending  '%' to the percentile for Min and/or Max such as 5% or 90%. Note that the percentile always gets replaced by the actual value in the dataset.

A left-click on 'I' locks ranges from automatic resetting, and the locked range applies to the current Dset only. A locked range is indicated with the reverse video mode.

A right-click resets values to the default range (usually 2% to 98%) in the dataset.

   .. _GraphCont->GDset_Mapping->SetRangeTable.r02:

**SetRangeTable.r02**: Brightness modulation clipping range 


Values in the brightness (B) :ref:`column<SurfCont->Dset_Mapping->B>` are clipped to the Min to Max range in this row before calculating their modulation factor per the values in the next table row.

You can set the range as a percentile of the dataset's values by appending  '%' to the percentile for Min and/or Max such as 8% or 75%. Note that the percentile always gets replaced by the actual value in the dataset.

A left-click locks ranges in this row from automatic resetting, and a locked range is applied to the current Dset only. A locked range is indicated with the reverse video mode.

A right-click resets values to the default range (usually 2% to 98%) for the dataset.

   .. _GraphCont->GDset_Mapping->SetRangeTable.r03:

**SetRangeTable.r03**: Brightness modulation factor range 


Brightness modulation factor range.
Brightness modulation values, after
clipping per the values in the row above,
are scaled to fit the range specified
here.

   .. _GraphCont->GDset_Mapping->Col:

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

   .. _GraphCont->GDset_Mapping->Cmp:

**Cmp**: Switch between available color maps. 


Switch between available color maps.
If the number of colormaps is too large
for the menu button, right click over
the 'Cmp' label and a chooser with a 
slider bar will appear.

More help is available via
ctrl+h while mouse is over the
colormap.

   .. _GraphCont->GDset_Mapping->Cmp->New:

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

   .. _GraphCont->GDset_Mapping->abs_T:

**abs_T**: Absolute threshold ON/OFF


Toggle Absolute thresholding.

OFF: Mask node color for nodes that have:   T(n) < Tscale

ON:  Mask node color for nodes that have:
| T(n) | < Tscale
where:

Tscale is the value set by the threshold scale.

T(n) is the node value in the selected threshold column (T). This value is seen in the second cell of the 'Value' table on the left side.

   .. _GraphCont->GDset_Mapping->sym_I:

**sym_I**: Intensity range symmetry about 0 


Toggle Intensity range symmetry about 0. 

ON : Intensity clipping range is forced to go from  -val to val. This allows you to mimic AFNI's ranging mode.

OFF: Intensity clipping range can be set to your liking.

   .. _GraphCont->GDset_Mapping->shw_0:

**shw_0**: Color masking of nodes with intensity = 0 


Toggle color masking of nodes with intensity = 0 

ON : 0 intensities are mapped to the colormap as any other values.

OFF: 0 intensities are masked, a la AFNI

   .. _GraphCont->GDset_Mapping->RangeTable.c00:

**RangeTable.c00**: Full range in Dset


Full range of values in Dset.

   .. _GraphCont->GDset_Mapping->RangeTable.r01:

**RangeTable.r01**: Range of values in intensity (I) column



   .. _GraphCont->GDset_Mapping->RangeTable.r02:

**RangeTable.r02**: Range of values in threshold (T) column



   .. _GraphCont->GDset_Mapping->RangeTable.r03:

**RangeTable.r03**: Range of values in brightness (B) column



   .. _GraphCont->GDset_Mapping->RangeTable.c01:

**RangeTable.c01**: Minimum value in Dset column


Minimum value in Dset column.

   .. _GraphCont->GDset_Mapping->RangeTable.c02:

**RangeTable.c02**: Edge index at minimum


Node index at minimum.
Right click in cell to
have crosshair jump to
node's index.
Same as 'ctrl+j' or
an entry in the 'Node' cell
under Xhair Info block.

   .. _GraphCont->GDset_Mapping->RangeTable.c03:

**RangeTable.c03**: Maximum value in Dset column


Maximum value in Dset column.

   .. _GraphCont->GDset_Mapping->RangeTable.c04:

**RangeTable.c04**: Edge index at maximum


Node index at maximum.
Right click in cell to
have crosshair jump to
node's index.
Same as 'ctrl+j' or
an entry in the 'Node' cell
under Xhair Info block.

