.. _drawing_ROIs:

=============
Drawing ROIs:
=============

You can draw Regions Of Interest (ROIs) directly on the surface models. To do so, you must first open the Draw ROI GUI with :kbd:`control-d` or :menuselection:`Tools --> Draw ROI`. An ROI can be a single :index:`node`, a curve (formed by connected nodes), a loop (or a closed curve or contour) and a filled loop. We begin with a small demo followed by a description of the GUI.

 
Demo:
-----

#. In the Surface Viewer window, open the DrawROI GUI with :kbd:`ctrl+d` or :menuselection:`Tools --> Draw ROI`. 
#. When you first open the DrawROI controller SUMA will be in Draw Mode. This is indicated by the cursor's shape changing from arrow to concentric circles. In drawing mode, you draw with the node picking (typically mouse button 3, or the right mouse button) button. For node picking without drawing, you will need to use Shift+Right Click :term:`SRC`. To return to normal cursor and mouse functions, toggle :guilabel:`Draw` off.
#. You can also draw in Pen mode by using the Pen toggle button. In Pen mode, the cursor changes into pen and the node picking button becomes the first (left) mouse button no matter what you chose for :ref:`default nodeselection <LC_Alt+s>` node picking button. This makes the drawing interface consistent with AFNI's ROI drawing. Pen button is only available when SUMA is in Draw Mode.
