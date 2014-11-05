
.. _ROICont:

ROI Cont.
---------

The ROI controller is for drawing ROIs on surfaces.

You can launch the :ref:`Draw ROI Controller <ROICont>` with: :ref:`ctrl+d <LC_Ctrl+d>` or :menuselection:`Tools-->Draw ROI`

.. figure:: media/ROICont.auto.ALL.jpg
   :align: center


   ..




.. _ROICont->ROI:

ROI
^^^

Controls for drawing ROIs.

.. figure:: media/ROICont.auto.ROI.jpg
   :align: right

   ..



   .. _ROICont->ROI->Draw:

**Draw**: Toggles ROI drawing mode


   Toggles ROI drawing mode.
   If turned on, then drawing is enabled 
   and the cursor changes to a target. 
   To draw, use the right mouse button. 
   If you want to pick a node without causing 
   a drawing action, use shift+right button.
   
   After the draw ROI window is open, you can toggle 
   this button via :ref:`ctrl+d<LC_Ctrl+d>` also.

   .. _ROICont->ROI->Cont.:

**Cont.**: Toggles showing ROI contours


   Toggles ROI contour drawing 
   If turned on, then contours are drawn around filled ROIs.
   Contours will *float* over other displayed datasets
   

   .. _ROICont->ROI->Pen:

**Pen**: Toggles Pen drawing mode


   Toggles Pen drawing mode
   If turned on, the cursor changes shape to a pen. 
   In the pen mode, drawing is done with button 1. 
   This is for coherence with AFNI's pen drawing mode, 
   which is meant to work pleasantly with a stylus directly 
   on the screen. In pen mode, you draw with the left mouse 
   button and move the surface with the right button. 
   To pick a node, use shift+left button. 
   Pen mode only works when Draw Mode is enabled.

   .. _ROICont->ROI->Afni:

**Afni**: Toggles Link to Afni


   Toggles Afni Link for ROI drawing.
   If turned on, then ROIs drawn on the surface are
   sent to AFNI. 
   Surface ROIs that are sent to AFNI are turned
   into volume ROIs (VOIs) on the fly and displayed 
   in a functional volume with the same colormap used in SUMA.
   The mapping from the surface domain (ROI) to the volume 
   domain (VOI) is done by intersection of the first with 
   the latter. The volume used for the VOI has the same 
   resolution (grid) as the Surface Volume (-sv option) 
   used when launching SUMA. The color map used for ROIs 
   is set by the environment variable :ref:`SUMA_ROIColorMap<SUMA_ROIColorMap>`.

   .. _ROICont->ROI->Dist:

**Dist**: Report length of drawn segments? 


   Report length of drawn segments?
   
      -----: No distance calculations.
   
      trace: Calculate distance along last  traced segment.
      all:   In addition to output from  'trace', calculate the shortest  distance between the first and  last node of the trace.
   
      The results are output to the Message Log  window (Help --> Message Log) with the following information:
   
      n0, n1: Indices of first and last node forming  the traced path.
   
      N_n:    Number of nodes forming the trace.
   
      lt:     Trace length calculated as the sum  of the distances from node to node. This length is a slight overestimation  of the geodesic length.  Units for all distances is the same as  the units for surface coordinates. Usually  and hopefully in mm.
   
      lt_c:   Trace length corrected by a scaling factor from [1] to better approximate geodesic  distances. Factor is 2/(1+sqrt(2)).  Do not use this factor when N_n is small.  Think of the extreme case when N_n is 2.
   
      sd:     Shortest distance on the mesh (graph)  between n0 and n1 using Dijkstra's algorithm.
   
      sd_c:   Corrected shortest distance as for lt_c.
   
   
      Note 1: sd and sd_c take some time to compute. That is  why they are only calculated when you select 'all'.
   
      Note 2: The output is formatted to be cut and pasted into  a .1D file for ease of processing.  You can include all the comment lines that  start with '#'. But you cannot combine entries  from the output obtained using 'all' option with  those from 'trace' since they produce different  numbers of values.
   
      [1] Fischl et al, Neuroimage 9, 195-207 1999,  Cortical Surface-Based Analysis.

   .. _ROICont->ROI->Label:

**Label**: Label of ROI being drawn


   Label of ROI being drawn.
   It is very advisable that you use 
   different labels for different ROIs. 
   If you don't, you won't be able to 
   differentiate between them afterwards.

   .. _ROICont->ROI->Value:

**Value**: Integer value associated with ROI


   Integer value associated with ROI.
   This value controls the color of the 
   ROI per the ROI colormap.

   .. _ROICont->ROI->Undo:

**Undo**: Undo the last action on the stack


   Undo the last action on the stack.

   .. _ROICont->ROI->Redo:

**Redo**: Redo the last undone action


   Redo the last undone action.

   .. _ROICont->ROI->Join:

**Join**: Join the first node of the path to the last


   Join the first node of the ROI to the last,
   thereby creating a close contour ROI.
   This is a necessary step before the filling
   operation. Joining is done by cutting the surface
   with a plane formed by the two nodes
   and the projection of the center of your window.
   You could double click at the last node, if you don't
   want to use the 'Join' button.

   .. _ROICont->ROI->Finish:

**Finish**: Label ROI as finished.


   Mark ROI as finished.
   Allows you to start drawing a new one.
   Once marked as finished, an ROI's label
   and value can no longer be changed.
   To do so, you will need to 'Undo' the 
   finish action.

   .. _ROICont->ROI->Switch_ROI:

**Switch_ROI**: Switch between ROIs.


   Allows you to switch between ROIs.
   This is where you'll suffer if ROIs 
   on topologically isomorphic surfaces 
   share identical labels.

   .. _ROICont->ROI->Load:

**Load**: Load a Drawn ROI


   Load a Drawn ROI.
   See BHelp for 'Save' below.

   .. _ROICont->ROI->delete_ROI:

**delete_ROI**: Click twice in 5 seconds to delete ROI. No Undo for this action.


   Delete a drawn ROI.
   This operation is not reversible,
   (no Undo here) so you'll have to click twice.

   .. _ROICont->ROI->Save:

**Save**: Save the Drawn ROI to disk.


   Save the Drawn ROI to disk.
   Choose the file format and what is to be
   saved from the two menus ahead.
   
   
   File format for saving ROI:
   Format options are 1D and NIML.
   
      The 1D format is the same one used in AFNI.
   
   It is an ASCII file with 2 values per line. The first 
   value is a node index, the second is the node's value. 
   Needless, to say, this format does not support the storage 
   of ROI auxiliary information such as Label and 
   Parent Surface, etc., nor does it preserve the order in which 
   nodes are traversed during a tracing. For that you'll have to use :term:NIML.
   
   NIML is a whole different story which will be documented 
   (if necessary) in the future. Suffice it to say that in NIML 
   format you can store all the auxiliary information about 
   each ROI, unlike with the .1D format.
   
   **But more importantly**, the NIML format allows you to preserve the order in which you traced the ROI. You can actually use 
   :ref:`Undo<ROICont->ROI->Undo>`/ref:`Undo<ROICont->ROI->Redo>` on ROIs save in NIML format.This information can be later used for the purpose of sampling 
   cortical activity along a particular path. This would be accomplished 
   with the aid of ROI2dataset's -nodelist\* options, along with 
   ConvertDset's -node_select_1D option.
   
   
   
   Which ROIs to save?
   
      This: saves the current ROI. 
      All: saves all ROIs on surfaces related to the Parent  surface of the current ROI.

