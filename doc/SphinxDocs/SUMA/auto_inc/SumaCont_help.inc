
.. _SumaCont:

Suma Controller
---------------

The suma controller is for controlling parameters common to across viewers and objects.You can launch the :ref:`Suma Controller <SumaCont>` with: :ref:`ctrl+u <LC_Ctrl+u>` or :menuselection:`View-->Suma Controller`

.. figure:: media/SumaCont.auto.ALL.jpg
   :align: center


   ..




   .. _SumaCont->Lock:

**Lock**: 


   Set the crosshair lock 
   between viewers.
   
      **-** No Lock: Crosshair only moves in viewer where you clicked.
   
      **i** Node index Lock: Crosshair jumps to the same node index on related surfaces (or objects) in other viewers. Linking in this case is topology based.
   
     **c** Coordinate Lock: Crosshair jumps to the same XYZ mm coordinate in other viewers. Linking in this case is geometry based).

   .. _SumaCont->Lock->View:

**View**: 


   Lock the view point of all viewers. Depress toggle button to link view point across viewers.
   
      * Surface rotation and translation in one viewer is reflected in all linked viewers.
   
      * Liking is NOT done across viewers that are displaying objects of different :ref:`embedding dimensions<Spec_EmbedDimension>` such as 3D and 2D surfaces.
   

   .. _SumaCont->Lock->All:

**All**: 


   Set the crosshair lock 
   between viewers.
   
      **-** No Lock: Crosshair only moves in viewer where you clicked.
   
      **i** Node index Lock: Crosshair jumps to the same node index on related surfaces (or objects) in other viewers. Linking in this case is topology based.
   
     **c** Coordinate Lock: Crosshair jumps to the same XYZ mm coordinate in other viewers. Linking in this case is geometry based).

   .. _SumaCont->Viewer:

**Viewer**: Opens a new viewer


   Opens a new Surface viewer window.

   .. _SumaCont->BHelp:

**BHelp**: Press this button then click on a button/label/menu for more help.


   Click the hand
   on any button or 
   label, menu, etc. to
   get a little help.

   .. _SumaCont->Close:

**Close**: Close SUMA controller


   Close SUMA controller window.
   Current settings are preserved
   when controller is reopened.

   .. _SumaCont->done:

**done**: Click twice in 5 seconds to close everything and quit SUMA.


   Click twice in 5 seconds to quit application. All viewer windows will be closed, nothing is saved, SUMA will terminate, and there maybe no one left at this computer.

