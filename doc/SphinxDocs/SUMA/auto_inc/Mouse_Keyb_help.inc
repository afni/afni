.. _KeyboardControls:

Keyboard Controls:
------------------

*On MACs*, Alt is the Apple/Command key.
   If it is commandeered by the OS, and you can't get it back, then
   try the alt/option key instead.

*On Linux*, Turn NumLock OFF, otherwise certainly mouse or 
   keyboard combinations do not work as intended.

   
.. _LC_a:

:kbd:`a`: attenuation by background, toggle.

   
.. _UC_B:

:kbd:`B`: Backface/Frontface/Noface culling, toggle.
   
.. _LC_b:

:kbd:`b`: background color, toggle.

   
.. _LC_c:

:kbd:`c`: load a node color file.

   
.. _LC_Ctrl+d:

:kbd:`Ctrl+d`: draw ROI controller.

   
.. _UC_D:

:kbd:`D`: Attch to the current dataset 'parent' a dot product
        transform. The 'child' (transformed) dataset
        is created by calculating the dot product between
        each node time series and the time series of the current
        node. Each time you 'shift+ctrl+click (drag too if you like)'
        on the surface, with the child dataset in view, the dot product
        is recalculated. 
        You can save the resultant datasets with 'ctrl+W' key (see below).
        Dset names are automatically formed.
        To stop the interactive dot product computations,
        switch back to the parent dset and press 'D' again.
        If the parent dataset is properly detrended and each 
        time series is normalized so that its stdev is 1.0
        then the dot product is the cross correlation coefficient.
        Detrending and normalization can be carried out with:

           3dDetrend -polort 4 -normalize \
                     -prefix dt.TS.niml.dset \
                     v2s.TS.niml.dset

        You can get a good feel for what this 'D' does by running

               @Install_InstaCorr_Demo

        That script will download and setup demo data for resting-state
        correlations. In particular, script @RunSingleSurfInstaCorr of the
        demo illustrates the 'D' feature.


   
.. _UC_F:

:kbd:`F`: Flip light position between +z and -z.
   
.. _LC_f:

:kbd:`f`: functional overlay, toggle.

   
.. _LC_g:

:kbd:`g`: graph data.
        Open a graphing window for the dataset
        currently selected. The graphing window
        updates with each new node selection.
        A graphing window can be opened for each
        dataset, and all graphs will update unless
        '1 Only' is set in Surface Controller.
        For complex data its magnitude is plotted instead.
        Use 'ctrl+h' in graph window for more help.

   
.. _UC_H:

:kbd:`H`: Highlight nodes inside a specified box.
        Does not update other viewers
        Paints into existing colors
        Highlight is wiped out with new
        colors.

   
.. _LC_Ctrl+h:

:kbd:`Ctrl+h`: help message

   
.. _UC_J:

:kbd:`J`: Set the selected FaceSet on Surface Object
        in Focus. Does not update in other viewers
        or in AFNI.
   
.. _LC_j:

:kbd:`j`: Set the cross hair to a certain node on SO in Focus.
        Append/prepend L or R to switch hemispheres.
        Does update in other viewers if linked by index
        and AFNI if connected
   
.. _LC_Ctrl+j:

:kbd:`Ctrl+j`: Set the cross hair's XYZ location. 
        Does update in other viewers
        if linked by XYZ        and AFNI if connected
   
.. _LC_Alt+j:

:kbd:`Alt+j`: Set the Focus node. 
        Cross hair's XYZ remain unchanged.
        Does not update in other viewers
        or in AFNI

   
.. _UC_L:

:kbd:`L`: Light's XYZ coordinates.
        Default setting is 0.0 0.0 1.0 
   
.. _LC_l:

:kbd:`l`: look at point
   
.. _LC_Alt+l:

:kbd:`Alt+l`: look at cross hair
   
.. _LC_Ctrl+l:

:kbd:`Ctrl+l`: Switch locking mode for all viewers 
             between: No Lock, Index Lock and 
             XYZ Lock. The switching is order is 
             based on the lock of the first viewer.

   
.. _UC_Alt+Ctrl+M:

:kbd:`Alt+Ctrl+M`: Dumps memory trace to file 
                 called malldump.NNN where NNN
                 is the smallest number between
                 001 and 999 that has not been used.
   
.. _LC_m:

:kbd:`m`: momentum, toggle

   
.. _UC_O:

:kbd:`O`: Increase opacity of all surfaces in viewer by 4 levels.
       Transparency levels accessible are: 
       0 (opaque), 25%, 50%, 75%, 100% (invisible)

   
.. _LC_o:

:kbd:`o`: Decrease opacity of all surfaces in viewer by 4 levels.

   
.. _LC_Ctrl+o:

:kbd:`Ctrl+o`: Set new center of rotation.
       Enter nothing to go back to default.

  
.. _LC_Ctrl+n:

:kbd:`Ctrl+n`: Open a new surface viewer window.

   
.. _LC_p:

:kbd:`p`: Viewer rendering mode  
        (Fill, Line, Points, Hide), switch.

   
.. _LC_Ctrl+p:

:kbd:`Ctrl+p`: Cycle between restrictions of where DO node-based 
         objects are displayed. Available modes are:
           All: No restrictions
           n3Crosshair: Crosshair node + 3 neighboring layers
           n2Crosshair: Crosshair node + 2 neighboring layers
           n1Crosshair: Crosshair node only
           None: Show nothing.

              See also -do_draw_mask option in DriveSuma

        ** DO stands for displayable objects, see 'Ctrl+Alt+s'
           below.
        ** For the moment, 'Ctrl+p' only applies to segment 
           and sphere DOs  that are node based. 
           If you need it applied to other DOs, let me know.
   
.. _UC_P:

:kbd:`P`: Reset viewer and all surfaces to Fill  
        rendering mode.

   
.. _LC_r:

:kbd:`r`: record current image
       in an a la AFNI image viewer.
       Identical images are rejected.
       If you just save one image, the recording
       window has no visible controls for saving
       the image. Either take another picture, or
       use 'Shift+right click' to get a menu.

   
.. _LC_Ctrl+r:

:kbd:`Ctrl+r`: Record current image directly to disk.
        Images are saved with a date stamp of the
        format PREFIX.X.yymmdd_hhmmss.MMM.jpg where:

        PREFIX controlled with SUMA_AutoRecordPrefix.
           See environment variable SUMA_AutoRecordPrefix for
           controlling prefix and output image type (suma -update_env).
        X  The character indicating which viewer is recording (you can
           record from multiple viewers at once.
        yy, mm, dd, hh, mm, ss for year, month, day, hours, minutes,
           and seconds, respectively. MMM is a millisecond marker to
           avoid overwriting files. Unlike the other recording mode 
           (with the 'R' key), there is no rejection of identical images

        This option is useful for saving a large number of images
        without running out of memory in the recorder GUI. 

        Your current PREFIX is: ./SUMA_Recordings/autorecord.ppm

   
.. _LC_Alt+r:

:kbd:`Alt+r`: Increase the image oversampling factor.
             By increasing this factor, you can create
             images at a resolution higher than that 
             of the SUMA window. This is done by subdividing 
             the scene into NxN sections and rendering each
             section separately. The NxN renderings are
             saved in the image recorder. After you 
             save the images to disk, you can stitch them
             using imcat (a la AFNI montage). 

        Note that each section is still rendered at
             the resolution of the SUMA window. So the bigger
             the window the more resolution per section.
             However, you cannot exceed a certain limit
             on the number of pixels in the final image.
             This limitation is due to the graphics card
             on your system. SUMA will take care not to exceed
             this limit.
   
.. _UC_Ctrl+R:

:kbd:`Ctrl+R`: Toggle continuous jpeg saving to disk.
             Naming of output images is automatic, same as in Ctrl+r.
             See help for Ctrl+r above for more info.
   
.. _UC_R:

:kbd:`R`: Toggle continuous recording 
        to an a la AFNI image viewer.
        Identical images are rejected.

   
.. _LC_Ctrl+s:

:kbd:`Ctrl+s`: Open :ref:`controller <SurfCont>` for  surface in Focus.
   
.. _LC_Ctrl+Alt+s:

:kbd:`Ctrl+Alt+s`: Input filename containing displayable objects.
                 Files are of 1D format with a necessary comment
                 at the top to indicate the type of objects in 
                 the file.
                 Note 1: Repeatedly loading files with the same 
                 name will replace currently loaded versions.
                 Note 2: Node-based (Types 3 and 4) objects
                 will follow a node when its coordinates change.
                 Note 3: See also 'Ctrl+p' for restricting which 
                 node-based objects get displayed.

          Type 1:Segments between (x0,y0,z0) and (x1,y1,z1) 
                 1st line must be '#segments' (without quotes),
                 or '#oriented_segments' (slower to render).
                 One can also use #node-based_segments or 
                 #node-based_oriented_segments and use a node index
                 in place of (x,y,z) triplets.
                 Remainder of file is N rows, each defining a 
                 segment (or a vector) between two points.
                 Column content depends on the number of columns
                 in the file:

              For node-based:
                 2  cols: n0 n1
                 3  cols: n0 n1 th

                          with th being line thickness

                 6  cols: n0 n1 c0 c1 c2 c3

                          with c0..3 being the RGBA values
                          between 0 and 1.0

                 7  cols: n0 n1 c0 c1 c2 c3 th
                 8  cols: n0 n1 c0 c1 c2 c3 th st

                          with st being a stippling, or dashing for some,
                          flag. Use integers between 1 and 5 for a variety
                          of syles.

              For coordinate-based
                 6  cols: x0 y0 z0 x1 y1 z1
                 7  cols: x0 y0 z0 x1 y1 z1 th

                          with th being line thickness

                 10 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3

                          with c0..3 being the RGBA values
                          between 0 and 1.0

                 11 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th
                 12 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th st

                          with st defined above.

          Type 2: Directions, a variant of segments and oriented segments.
                 1st line must be '#directions' (without quotes).
                 Remainder of file is N rows, each defining a 
                 direction.
                 Column content depends on the number of columns
                 in the file:

                 3  cols: dx dy dz

                          with dx dy dz defining the direction. The
                          triplet need not be of unti norm though that
                          would affect the default coloring scheme detailed
                          below. The segment drawn has origin 0, 0, 0

                 4  cols: dx dy dz mag

                          with mag being a scaling factor for the direction.
                          mag is 1 by default.

                 5  cols: dx dy dz mag th

                          with th being the thickness of the line.
                          Default is 1

                 6  cols: ox oy oz dx dy dz

                          Specify the origin of the segment in o1, o2, o3.
                          Default is origin 0, 0, 0 for all

                 7  cols: o1 o2 o3 dx dy dz mag

                          Add individual scaling factors to case above
                          Segment is from origin to origin+mag*direction

                 8  cols: o1 o2 o3 dx dy dz mag th

                          Add thickness to case with 7 columns

                 9  cols: dx dy dz mag th c0 c1 c2 c3

                          Add colors for each segment, with origin at 0,0,0

                 11 cols: ox oy oz dx dy dz mag c0 c1 c2 c3
                 12 cols: ox oy oz dx dy dz mag th c0 c1 c2 c3

          Type 3:Spheres centered at (ox, oy, oz) 
                 1st line must be '#spheres' (without quotes).
                 Remainder of file is N rows, each defining a 
                 sphere.
                 Column content depends on the number of columns
                 in the file:
                 3  cols: ox oy oz
                 4  cols: ox oy oz rd

                          with rd being the radius of the sphere

                 5  cols: ox oy oz rd st

                          with st being the style of the sphere's
                          rendering. Choose from:

                             0: points
                             1: Lines
                             2: Filled

                 7  cols: ox oy oz c0 c1 c2 c3 

                          with c0..3 being the RGBA values
                          between 0 and 1.0

                 8  cols: ox oy oz c0 c1 c2 c3 rd
                 9  cols: ox oy oz c0 c1 c2 c3 rd st

          Type 4:Points at (ox, oy, oz) 
                 1st line must be '#points' (without quotes).
                 Remainder of file is N rows, each defining a 
                 point.
                 Column content depends on the number of columns
                 in the file:
                 3  cols: ox oy oz
                 4  cols: ox oy oz sz

                          with sz being the size of the point

                 7  cols: ox oy oz c0 c1 c2 c3 

                          with c0..3 being the RGBA values
                          between 0 and 1.0

                 8  cols: ox oy oz c0 c1 c2 c3 sz
          Type 5:Vectors (vx, vy, vz) at surface nodes 
                 1st line must be '#node-based_vectors' (without quotes)
                 or '#node-based_ball-vectors' (slower to render).
                 Remainder of file is N rows, each defining a 
                 a vector at a particular node of the current surface.
                 Column content depends on the number of columns
                 in the file:
                 3  cols: vx, vy, vz 

                          node index 'n' is implicit equal to row index.
                          Vector 'v' is from coordinates of node 'n' to 
                          coordinates of node 'n' + 'v'

                 4  cols: n, vx, vy, vz 

                          Here the node index 'n' is explicit. You can
                          have multiple vectors per node, one on 
                          each row.

                 5  cols: n, vx, vy, vz, gn

                          with gn being a vector gain factor

                 8  cols: n, vx, vy, vz, c0 c1 c2 c3

                          with with c0..3 being the RGBA values
                          between 0 and 1.0

                 9  cols: n, vx, vy, vz, c0 c1 c2 c3 gn

          Type 6:Spheres centered at nodes n of the current surface
                 1st line must be '#node-based_spheres' (without quotes).
                 Remainder of file is N rows, each defining a 
                 sphere.
                 Column content depends on the number of columns
                 in the file, see Type 2 for more details:
                 1  cols: n
                 2  cols: n rd
                 3  cols: n rd st
                 5  cols: n c0 c1 c2 c3 
                 6  cols: n c0 c1 c2 c3 rd
                 7  cols: n c0 c1 c2 c3 rd st

          Type 7:Planes defined with: ax + by + cz + d = 0.
                 1st line must be '#planes' (without quotes).
                 Remainder of file is N rows, each defining a 
                 plane.
                 Column content depends on the number of columns
                 in the file:
                 7  cols: a b c d cx cy cz

                          with the plane's equation being:
                          ax + by + cz + d = 0
                          cx,cy,cz is the center of the plane's
                          representation. 
                          Yes, d is not of much use here.

                 There are no node-based planes at the moment.
                 They are a little inefficient to reproduce with
                 each redraw. Complain if you need them.

         Type 8: Another class of displayble objects is described in
                 the output of suma -help_nido and the demonstration
                 script @DO.examples. This new class allows for displaying 
                 text and figures in both screen and world space.
   
.. _LC_Alt+s:

:kbd:`Alt+s`: Switch mouse buttons 1 and 3.

   
.. _LC_t:

:kbd:`t`: talk to AFNI, toggle.
   
.. _LC_Ctrl+t:

:kbd:`Ctrl+t`: Force a resend of 
            surfaces to AFNI.

   
.. _UC_T:

:kbd:`T`: Start listening for niml connections

   
.. _LC_Ctrl+u:

:kbd:`Ctrl+u`: Open SUMA controller.

   
.. _LC_w:

:kbd:`w`: Whereami window of little use at the moment.

   
.. _UC_Ctrl+W:

:kbd:`Ctrl+W`: Write items stowed in SUMA's save list.
             This is used to write temporary dsets that
             are created on the fly in SUMA. Such sets include
             those created via the 'D' option above,
             or the results sent by 3dGroupInCorr

   
.. _UC_W:

:kbd:`W`: Write ascii files containing the NodeList,
        the FaceSetList and the nodecolors of the 
        surface in focus.

   
.. _UC_Z:

:kbd:`Z`: Zoom in
   
.. _LC_z:

:kbd:`z`: Zoom out
   
.. _LC_[:

:kbd:`[`: Show/Hide left hemisphere.
   
.. _LC_]:

:kbd:`]`: Show/Hide right hemisphere.
        Window title shows which 
        hemispheres are shown 

        :-RL-: or :--:

  
.. _LC_8:

:kbd:`8`: Set the number of smoothing iterations
     to be applied to the foreground colors.
     This setting will be applied to all subsequent
     color sets.
  
.. _LC_*:

:kbd:`*`: Smooth node colors by averaging with neighbors.
     The smoothing is only applied to the current colors,
     and will be not be applied to new color sets.

   
.. _LC_,:

:kbd:`,`/
.. _LC_.:

:kbd:`.` (think </>): Switch to next/previous view state.
     Viewing angle is reset only when switching to
     a state with flat surfaces.
   
.. _SPACE:

:kbd:`SPACE`: Toggle between Mapping Reference and
            Current view state.
            Viewing angle is reset only when switching to
            a state with flat surfaces.

   
.. _L-R_arrows:

:kbd:`L-R arrows`: rotate about screen's Y axis
   
.. _U-D_arrows:

:kbd:`U-D arrows`: rotate about screen's X axis
   
.. _Shift+L-R_arrows:

:kbd:`Shift+L-R arrows`: translate along screen's 
                       X axis
     
.. _Shift+U-D_arrows:

:kbd:`Shift+U-D arrows` arrows: translate along screen's 
                       Y axis
   
.. _Ctrl+L-R_arrows:

:kbd:`Ctrl+L-R arrows`: LR cardinal views
   
.. _Ctrl+U-D_arrows:

:kbd:`Ctrl+U-D arrows`: IS cardinal views
   
.. _Ctrl+Shift+U-D_arrows:

:kbd:`Ctrl+Shift+U-D arrows`: AP cardinal views

   
.. _Ctrl+Shift+L-R_arrows:

:kbd:`Ctrl+Shift+L-R arrows`: rotate CCW and CW about Z screen axis

   
.. _Alt+L-R_arrows:

:kbd:`Alt+L-R arrows`: Move selected node to neighboring nodes
                     in the direction of the screen's 
                     X axis. The default is to move one
                     node at a time. You can alter this
                     setting with the environment variable:
                     SUMA_KeyNodeJump in your ~/.sumarc file.
   
.. _Alt+U-D_arrows:

:kbd:`Alt+U-D arrows`: Same as Alt+L-R but in the direction 
                     of the screen's Y axis

   
.. _F1:

:kbd:`F1`: screen axis (X-Red, Y-Green), toggle. 
   
.. _F2:

:kbd:`F2`: surface axis (X-Red, Y-Green, Z-Blue), 
         switch. 
   
.. _F3:

:kbd:`F3`: cross hair, toggle. 
   
.. _F4:

:kbd:`F4`: node selection highlight, toggle. 
   
.. _F5:

:kbd:`F5`: FaceSet selection highlight, toggle.
   
.. _F6:

:kbd:`F6`: Viewer background color, toggle.
   
.. _F7:

:kbd:`F7`: Switch between color mixing modes.
         ORIG: Col = ( 1 - opacity ) * OldCol + opacity * NewCol 
         MOD1: Col = ( 1 - opacity ) * OldCol +           NewCol 
   
.. _F8:

:kbd:`F8`: Viewing mode (Perspective or Orthographic Projection), toggle.
   
.. _F9:

:kbd:`F9`: Labels at cross hair, toggle.
   
.. _F10:

:kbd:`F10`: Toggle prying axis between surfaces' Z and Y axes.
   
.. _F11:

:kbd:`F11`: Change object rendering order.
          This order will affect the resultant image in
          the few instances where alpha transparency is
          used. The order can be specified for only three types of 
          objects for now: graphs, surfaces, and volumes. 
          If you want to render graphs first, followed by volumes then
          surfaces then set SUMA_ObjectDisplayOrder to something like:
          'graph,vol,surf', or 'GVS'
   
.. _F12:

:kbd:`F12`: Time 20 scene renderings.

   
.. _HOME:

:kbd:`HOME`: reset zoom and recenter surfaces.
           rest view angle for flat surfaces only.

   
.. _ESCAPE:

:kbd:`ESCAPE`: close the surface viewer window.
   
.. _Shift+ESCAPE:

:kbd:`Shift+ESCAPE`: close all surface viewer windows.

.. _MouseControls:

Mouse Controls:
---------------

*On MACs*, Alt is the Apple/Command key.
   If it is commandeered by the OS, and you can't get it back, then
   try the alt/option key instead.

*On Linux*, Turn NumLock OFF, otherwise certainly mouse or 
   keyboard combinations do not work as intended.

   
.. _Button_1-Motion:

:kbd:`Button 1-Motion`: For 3D scenes, rotation as if you were 
       using a trackball.
       For matrix displays, Button 1-Motion causes translation because
       rotations are of little use in matrix display mode.

       Pure vertical motion is equivalent to using 
       the up/down arrow keys.

       Pure horizontal motion is equivalent to using 
       the left/right arrow keys.

       Of course, the advantage to using the mouse is 
       a continuous range of rotation angles and 
       simultaneous rotations about the screen's 
       X & Y axis.
   
.. _Shift+Button_1-Motion:

:kbd:`Shift+Button 1-Motion`: Rotate surfaces about the Z
       axis. This is useful at times for reorienting flat
       maps.
   
.. _Control+Button_1-Motion:

:kbd:`Control+Button 1-Motion`: Pry open two hemispheres
       so that you can see medial or lateral walls better
       from one angle. Prying is disabled for flat surfaces
       and with spheres the effect is to rotate each sphere
       about the Y axis passing through its center. Also, prying
       is only enabled when the state you are viewing contains two 
       surfaces, one on the left and one on the right.
       in 3D views, left right mouse movement cause a rotation about
       the front or rear I/S axis.
       Up down movements cause a shift along the left/right direction.
       You can select nodes (Button 3) on pried surfaces and still
       have AFNI jump to the proper location, and vice versa. However
       for the moment, you cannot draw in pried mode. If you attempt
       to draw, the surfaces are put back together.
       To make best use of this option, you want to have env. variable
       SUMA_LHunify = YES (see your ~/.sumarc for help)
  
.. _Button_1-DoubleClick:

:kbd:`Button 1-DoubleClick`: Reset to Home vieweing angle, zooming is
       left unchanged. See also 'HOME' key
  
.. _Control+Button_1-DoubleClick:

:kbd:`Control+Button 1-DoubleClick`: Undo surface prying.
  
.. _Button_2-Motion:

:kbd:`Button 2-Motion`: Translation for 3D scenes. Rotation for
                      matrix displays.
  
.. _Shift+Button2-Motion:

:kbd:`Shift+Button2-Motion` or :kbd:`Button 1+2-Motion`: 
          Zoom in/out
  
.. _Button_3-Press:

:kbd:`Button 3-Press`: Node picking whenever surfaces are present
                     Initiates a path to new node in DrawROI mode.
                     No calls in Dot xform mode, or GroupInCorr.
                     Graph Edge/Matrix Cell picking whenever displaying
                     graph datasets only (no surfaces in sight).
                     Intersections with graph edges/nodes are ignored 
                     when surfaces are displayed. 
                     See Alt+Button 3-Press next.
  
.. _Alt+Button_3-Press:

:kbd:`Alt+Button 3-Press`: Graph edge/node picking in the presence 
                         of surfaces. Intersections with surfaces
                         are ignored.
  
.. _Shift+Alt+Button_3-Press:

:kbd:`Shift+Alt+Button 3-Press`: Same as Alt+Button 3, but also display
                         pick buffer. This is mostly for debugging or 
                         for understanding why selection is behaving
                         strangely.
  
.. _Shift+Button_3-Press:

:kbd:`Shift+Button 3-Press`: Same as without shift, except does not draw
                           in DrawROI mode.
  
.. _Ctrl+Button_3-Press:

:kbd:`Ctrl+Button 3-Press`: Yoke intensity selection to index of 
                          selected node*K. This is only possible if
                          the currently visualized dataset has K times 
                          many sub-bricks as the surface has nodes. 
                          K is an integer.
  
.. _Shift+Ctrl+Button_3-Press:

:kbd:`Shift+Ctrl+Button 3-Press`: Pick and initiate call in Dot xform
                               mode, or to GroupInCorr
  
.. _Button_3-DoubleClick:

:kbd:`Button 3-DoubleClick`: If double clicking on a tract mask, select
                           the tract mask and turn the viewer into Mask
                           Manipulation Mode. In this mode, the mask is
                           shown as a wiremesh, and selections on any 
                           object will move the mask to that location.
                           New tract/masks intersections are computed
                           at the new location.
                           To leave Mask Manipulation Mode, double 
                           click with button 3 either on the mask or 
                           in an area void of any objects.
                           If double clicking with a graph object in 
                           focus and only connections from one node  
                           are shown, then revert to showing all graph
                           connections. Without this, you can loose all
                           other clickables if a certain node is not 
                           connected to anything.
  
.. _Button_3-Motion:

:kbd:`Button 3-Motion`: continuous picking whenever surface are present.
                      No calls for dot product (InstaCorr)
                      or GroupInCorr, while dragging.
                      Continuous picking of graph edges/nodes if no
                      surfaces are displayed.
  
.. _Alt+Button_3-Motion:

:kbd:`Alt+Button 3-Motion`: Continuous picking of graph edges/nodes. 
                      Intersections with surfaces are ignored.
  
.. _Ctrl+Button_3-Motion:

:kbd:`Ctrl+Button 3-Motion`: continous yoking of intensity selection to
                           selected node*K.
  
.. _Shift+Ctrl+Button_3-Motion:

:kbd:`Shift+Ctrl+Button 3-Motion`: Continuous picking and calls 
                               for dot product (InstaCorr)
                               or GroupInCorr, while dragging.
  
.. _Scroll:

:kbd:`Scroll` or Wheel: Zoom in/out
  
.. _Shift+Scroll:

:kbd:`Shift+Scroll` or Shift+Wheel: change selected slice if current selected
                         object is a volume.
  
.. _Ctrl+Scroll:

:kbd:`Ctrl+Scroll` or Ctrl+Wheel: change the size of the currently selected 
                        tract mask. This only works when you're in
                        mask manipulation mode.
    
.. _File_Menu:

File Menu:
----------

  
.. _Save_View:

:menuselection:`Save View`: Save viewer's display settings.
  
.. _Load_View:

:menuselection:`Load View`: Load and apply display settings.
  
.. _Close:

:menuselection:`Close`: Close this viewer.
           Exit SUMA if this is the only viewer.

.. _View_Menu:

View Menu:
----------

  
.. _SUMA_Controller:

:menuselection:`SUMA Controller`: Open SUMA controller interface.
  
.. _Surface_Controller:

:menuselection:`Surface Controller`: Open selected surface's 
                          controller interface.
  
.. _Viewer_Controller:

:menuselection:`Viewer Controller`: Open viewer's controller interface.
  
  
.. _Cross_Hair:

:menuselection:`Cross Hair`: Toggle cross hair display.
  
.. _Node_in_Focus:

:menuselection:`Node in Focus`: Toggle highlight of selected node.
  
.. _Selected_Faceset:

:menuselection:`Selected Faceset`: Toggle highlight of selected faceset.

.. _Tools_Menu:

Tools Menu:
-----------

  
.. _Draw_ROI:

:menuselection:`Draw ROI`: Open Draw ROI controller.

.. _Help_Menu:

Help Menu:
----------

   
.. _Usage:

:menuselection:`Usage`: Opens window with this message.
   
.. _Message_Log:

:menuselection:`Message Log`: Opens window that will 
                   contain errors and warnings
                   typically output to screen.
   
   
.. _SUMA_Global:

:menuselection:`SUMA Global`: Output debugging information
                   about some of SUMA's global 
                   structure's variables.
   
.. _Viewer_Struct:

:menuselection:`Viewer Struct`: Output debugging info on 
                     a viewer's structure.
   
.. _Surface_Struct:

:menuselection:`Surface Struct`: Output debugging info on
                      the selected surface's struct.
   
   
.. _InOut_Notify:

:menuselection:`InOut Notify`: Turn on/off function in/out tracing.
   
.. _MemTrace:

:menuselection:`MemTrace`: Turn on memory tracing.
                Once turned on, this can't be turned off.


.. _ENV_List:

SUMA's list of environment variables:
-------------------------------------

Below is a list of all of SUMA's environment variables and their default values.
You can query the value of a variable as SUMA sees it with:

     :command:`suma -Vname=` 

with 'name' replaced by the environment variable's name.

     e.gcommand:`suma -VSUMA_ArrowRotAngle=`

*Always* update your environment variable list with:

     :command:`suma -update_env`

The List:
=========
.. _SUMA_ArrowRotAngle:

:envvar:`SUMA_ArrowRotAngle`: Incremental arrow rotation angle in degrees

  default value:   SUMA_ArrowRotAngle = 5

.. _SUMA_ColorPattern:

:envvar:`SUMA_ColorPattern`: Color pattern (AFNI, EURO, PRINT, DEFAULT)

  default value:   SUMA_ColorPattern = EURO

.. _SUMA_SwapButtons_1_3:

:envvar:`SUMA_SwapButtons_1_3`: Swap mouse buttons 1 and 3

  default value:   SUMA_SwapButtons_1_3 = NO

.. _SUMA_BackgroundColor:

:envvar:`SUMA_BackgroundColor`: Background color r g b. No space between values

  default value:   SUMA_BackgroundColor = 0.0,0.0,0.0

.. _SUMA_ROIColorMap:

:envvar:`SUMA_ROIColorMap`: ROI color map (bgyr64, roi64, roi128, roi256)

  default value:   SUMA_ROIColorMap = ROI_i256

.. _SUMA_NumConvSmooth:

:envvar:`SUMA_NumConvSmooth`: Number of smoothing operations to run on convexity data

  default value:   SUMA_NumConvSmooth = 5

.. _SUMA_ConvColorMap:

:envvar:`SUMA_ConvColorMap`: Colormap for convexity (gray02, gray_i02, ngray20, bgyr64, etc.)

  default value:   SUMA_ConvColorMap = gray02

.. _SUMA_ConvBrightFactor:

:envvar:`SUMA_ConvBrightFactor`: Brightness factor for convexity 

  default value:   SUMA_ConvBrightFactor = 0.5

.. _SUMA_NumForeSmoothing:

:envvar:`SUMA_NumForeSmoothing`: Number of smoothing operations to run on mixed foregroung color plane
 before mixing with background

  default value:   SUMA_NumForeSmoothing = 0

.. _SUMA_NumFinalSmoothing:

:envvar:`SUMA_NumFinalSmoothing`: Number of smoothing operations to run on final set of mixed colors.
 This would be the mixed foreground and background colors

  default value:   SUMA_NumFinalSmoothing = 0

.. _SUMA_ColorMixingMode:

:envvar:`SUMA_ColorMixingMode`: Setup the color mixing mode (ORIG, MOD1) 

  default value:   SUMA_ColorMixingMode = ORIG

.. _SUMA_AFNI_TCP_PORT:

:envvar:`SUMA_AFNI_TCP_PORT`: ** OBSOLETE: Port for communicating with AFNI
              Listening ports are derived from SUMA_AFNI_TCP_PORT
              Listening port i
              SUMA_AFNI_TCP_PORT + i (i > 0)

  default value:   SUMA_AFNI_TCP_PORT = 0

.. _SUMA_WarnBeforeClose:

:envvar:`SUMA_WarnBeforeClose`: Warn before closing with the Escape key (YES/NO)

  default value:   SUMA_WarnBeforeClose = YES

.. _SUMA_MaskZero:

:envvar:`SUMA_MaskZero`: Mask node values
 0 ? YES/NO

  default value:   SUMA_MaskZero = YES

.. _SUMA_AbsThreshold:

:envvar:`SUMA_AbsThreshold`: Threshold if Val < thr (NO) or | Val | < | Thr | (YES)

  default value:   SUMA_AbsThreshold = YES

.. _SUMA_ThresholdScalePower:

:envvar:`SUMA_ThresholdScalePower`: Threshold scale precision. 2 is the minimum allowed. 
 This value might be overriden in SUMA.

  default value:   SUMA_ThresholdScalePower = 2

.. _SUMA_CenterOnPatch:

:envvar:`SUMA_CenterOnPatch`: Center of Rotation is based on nodes used in the mesh and not 
 on all the nodes in NodeList

  default value:   SUMA_CenterOnPatch = NO

.. _SUMA_UseCrossTicks:

:envvar:`SUMA_UseCrossTicks`: Use cross ticks on axis ?

  default value:   SUMA_UseCrossTicks = NO

.. _SUMA_1D_Transpose_Warn:

:envvar:`SUMA_1D_Transpose_Warn`: Warn if 1D file looks like it needs a transpose

  default value:   SUMA_1D_Transpose_Warn = YES

.. _SUMA_AdjustMouseMotionWithZoom:

:envvar:`SUMA_AdjustMouseMotionWithZoom`: Adjust roation and translation factor of mouse with changes 
 in zoom levels 

  default value:   SUMA_AdjustMouseMotionWithZoom = YES

.. _SUMA_ViewOrthographicProjection:

:envvar:`SUMA_ViewOrthographicProjection`: Use orthographic projection 

  default value:   SUMA_ViewOrthographicProjection = NO

.. _SUMA_KeyZoomGain:

:envvar:`SUMA_KeyZoomGain`: Percent gain for zooming in and out with the 'z' and 'Z' keys. 
 Typical range from 0 to 50

  default value:   SUMA_KeyZoomGain = 5

.. _SUMA_FOV_Original:

:envvar:`SUMA_FOV_Original`: Original FOV. Set between 1.0 and 100.0 
 Default is 30.0, -1 == auto

  default value:   SUMA_FOV_Original = -1

.. _SUMA_Position_Original:

:envvar:`SUMA_Position_Original`: Original windows size and width in pixels 
 Allowed values are:

    'TopLeft'

    'RightOffset'

    'X Y' Sets only the position to top left corner

    'X Y Xwidth Ywidth' Set also width of window

 

  default value:   SUMA_Position_Original = TopLeft

.. _SUMA_Light0Color:

:envvar:`SUMA_Light0Color`: light0 color

  default value:   SUMA_Light0Color = 1.0,1.0,1.0

.. _SUMA_AmbientLight:

:envvar:`SUMA_AmbientLight`: Ambient light 

  default value:   SUMA_AmbientLight = 1.0,1.0,1.0

.. _SUMA_AllowDsetReplacement:

:envvar:`SUMA_AllowDsetReplacement`: Allow for replacement of pre-loaded dsets

  default value:   SUMA_AllowDsetReplacement = YES

.. _SUMA_AlwaysAssignSurface:

:envvar:`SUMA_AlwaysAssignSurface`: Allow a dataset to be assigned to a surface, even if
domain of dset is specified and different for the surface.


  default value:   SUMA_AlwaysAssignSurface = YES

.. _SUMA_ShareGrandChildrenOverlays:

:envvar:`SUMA_ShareGrandChildrenOverlays`: Allow for surfaces with same DomainGrandParentID to share overlays

  default value:   SUMA_ShareGrandChildrenOverlays = NO

.. _SUMA_SnapshotOverSampling:

:envvar:`SUMA_SnapshotOverSampling`: Increase the resolution of images recorded with 'r' button.
 Increase is done by taking multiple shots that once stitched  
 together form a high-resolution image.
 The maximum resolution is set by the GL_MAX_VIEWPORT_DIMS of your
 graphics card. I have 4096 pixels.
 If you exceed this number, SUMA will make adjustments automatically.
 Assemble images with program imcat.

  default value:   SUMA_SnapshotOverSampling = 1

.. _SUMA_NoDuplicatesInRecorder:

:envvar:`SUMA_NoDuplicatesInRecorder`: Ignore consecutive duplicate images in recorder

  default value:   SUMA_NoDuplicatesInRecorder = YES

.. _SUMA_START_NIML:

:envvar:`SUMA_START_NIML`: start NIML (can't do this for more than one suma at a time!)

  default value:   SUMA_START_NIML = YES

.. _SUMA_AllowFilenameDsetMatch:

:envvar:`SUMA_AllowFilenameDsetMatch`: Allow (YES) datasets with the same filename but differing ID 
 to be considered the same.
 This is only useful with SUMA_AllowDsetReplacement

  default value:   SUMA_AllowFilenameDsetMatch = YES

.. _SUMA_FreezeFOVAcrossStates:

:envvar:`SUMA_FreezeFOVAcrossStates`: Freeze zoom across states

  default value:   SUMA_FreezeFOVAcrossStates = NO

.. _SUMA_DsetColorMap:

:envvar:`SUMA_DsetColorMap`: Dset color map

  default value:   SUMA_DsetColorMap = Spectrum:red_to_blue

.. _SUMA_ShowOneOnly:

:envvar:`SUMA_ShowOneOnly`: Show only selected dset in suma's surface controller.

  default value:   SUMA_ShowOneOnly = YES

.. _SUMA_GraphHidden:

:envvar:`SUMA_GraphHidden`: Update graphs, even SUMA_ShowOneOnly (or suma's '1 Only') is turned on.

  default value:   SUMA_GraphHidden = YES

.. _SUMA_ColorMapRotationFraction:

:envvar:`SUMA_ColorMapRotationFraction`: Fraction of colormap to rotate with up/down arrow keys.

  default value:   SUMA_ColorMapRotationFraction = 0.05

.. _SUMA_SurfContFontSize:

:envvar:`SUMA_SurfContFontSize`: Size of surface controller font. 
 Values are SMALL, BIG (old style).

  default value:   SUMA_SurfContFontSize = SMALL

.. _SUMA_StartUpLocation:

:envvar:`SUMA_StartUpLocation`: Where to position SUMA window when first opened.
 Values are POINTER (at the mouse pointer's location)
            DEFAULT (let the window manager decide)


  default value:   SUMA_StartUpLocation = DEFAULT

.. _SUMA_KeyNodeJump:

:envvar:`SUMA_KeyNodeJump`: Numer of nodes to jump with the 'alt+arrow' keys. 
 Valid range from 1 to 10

  default value:   SUMA_KeyNodeJump = 1

.. _SUMA_DriveSumaMaxWait:

:envvar:`SUMA_DriveSumaMaxWait`: Numer of seconds to wait for SUMA to respond to DriveSuma. 
 Valid range from 0 to 60000, see also env SUMA_DriveSumaMaxCloseWait

  default value:   SUMA_DriveSumaMaxWait = 300.0

.. _SUMA_LEFT_FILE_DSET_IDENTIFIER:

:envvar:`SUMA_LEFT_FILE_DSET_IDENTIFIER`: String to use in creating left hemisphere dataset wildcards.

  default value:   SUMA_LEFT_FILE_DSET_IDENTIFIER = *lh*.dset

.. _SUMA_RIGHT_FILE_DSET_IDENTIFIER:

:envvar:`SUMA_RIGHT_FILE_DSET_IDENTIFIER`: String to use in creating left hemisphere dataset wildcards.

  default value:   SUMA_RIGHT_FILE_DSET_IDENTIFIER = *rh*.dset

.. _SUMA_LEFT_FILE_ROI_IDENTIFIER:

:envvar:`SUMA_LEFT_FILE_ROI_IDENTIFIER`: String to use in creating left hemisphere roi wildcards.

  default value:   SUMA_LEFT_FILE_ROI_IDENTIFIER = *lh*.roi

.. _SUMA_RIGHT_FILE_ROI_IDENTIFIER:

:envvar:`SUMA_RIGHT_FILE_ROI_IDENTIFIER`: String to use in creating right hemisphere roi wildcards.

  default value:   SUMA_RIGHT_FILE_ROI_IDENTIFIER = *rh*.roi

.. _SUMA_LEFT_FILE_OTHER_IDENTIFIER:

:envvar:`SUMA_LEFT_FILE_OTHER_IDENTIFIER`: String to use in creating left hemisphere roi wildcards.

  default value:   SUMA_LEFT_FILE_OTHER_IDENTIFIER = *lh*

.. _SUMA_RIGHT_FILE_OTHER_IDENTIFIER:

:envvar:`SUMA_RIGHT_FILE_OTHER_IDENTIFIER`: String to use in creating right hemisphere roi wildcards.

  default value:   SUMA_RIGHT_FILE_OTHER_IDENTIFIER = *rh*

.. _SUMA_ConvexityDsetOpacity:

:envvar:`SUMA_ConvexityDsetOpacity`: Initial Convexity Datasest opacity.

  default value:   SUMA_ConvexityDsetOpacity = 0.85

.. _SUMA_ShowLabelDsetAtStartup:

:envvar:`SUMA_ShowLabelDsetAtStartup`: Display mode of  Label Datasest specified in spec file at startup.

  'YES' or 'Col': Shows it in color

  'Con': Shows only contours (see also env SUMA_ContourThickness).

  'C&C': Shows both colors and contours 

  'XXX'or 'No': Does not show it.



  default value:   SUMA_ShowLabelDsetAtStartup = XXX

.. _SUMA_ShowLabelsAtCrossHair:

:envvar:`SUMA_ShowLabelsAtCrossHair`: Show label at cross hair in viewer
You can toggle the display at such labels with F9


  default value:   SUMA_ShowLabelsAtCrossHair = YES

.. _SUMA_LabelDsetOpacity:

:envvar:`SUMA_LabelDsetOpacity`: Initial Label Datasest opacity.

  default value:   SUMA_LabelDsetOpacity = 0.2

.. _SUMA_AttemptTalkRecover:

:envvar:`SUMA_AttemptTalkRecover`: Attempt to recover from AFNI <--> SUMA disconnection bug.


  default value:   SUMA_AttemptTalkRecover = Yes

.. _SUMA_CmapsDir:

:envvar:`SUMA_CmapsDir`: Name of directory containing user's own SUMA color maps (`*`.cmap)


  default value:   SUMA_CmapsDir = None

.. _SUMA_RetinoAngle_DsetColorMap:

:envvar:`SUMA_RetinoAngle_DsetColorMap`: Name of color map for datasets of retinotopy angles.
These would be produced by 3dRetinoPhase


  default value:   SUMA_RetinoAngle_DsetColorMap = rgybr20

.. _SUMA_VFR_DsetColorMap:

:envvar:`SUMA_VFR_DsetColorMap`: Name of color map for VFR datasets produced by SurfRetinoMap


  default value:   SUMA_VFR_DsetColorMap = afni_n2

.. _SUMA_NodeCoordsUnits:

:envvar:`SUMA_NodeCoordsUnits`: Coordinate units of surface nodes. Choose from 'mm' or 'cm'
A bad choice can make the surfaces render with many artifacts.


  default value:   SUMA_NodeCoordsUnits = mm

.. _SUMA_DoNotSendStates:

:envvar:`SUMA_DoNotSendStates`: Which anatomically correct surf. states should not NOT be sent to AFNI?
This is mostly for deciding whether one of 'white' or 'smoothwm'
FreeSurfer states should not be sent to AFNI.
The default is to let them all go.
You can specify multiple states with a , delimited list (no spaces!). 
By default nothing is excluded.


  default value:   SUMA_DoNotSendStates = N/A

.. _SUMA_AutoRecordPrefix:

:envvar:`SUMA_AutoRecordPrefix`: Prefix for autorecord (suma's Ctrl+R) files. 
FreeSurfer states should not be sent to AFNI.
Add a path if you want the files to endup in a particular directory.
You can also add an extension to prefix to specify the output type.
Choose from .jpg, .ppm, or .1D . The fallback type is .jpg


  default value:   SUMA_AutoRecordPrefix = ./SUMA_Recordings/autorecord.jpg

.. _SUMA_CrossHairLabelFont:

:envvar:`SUMA_CrossHairLabelFont`: Font for cross hair label in SUMA viewer
Choose one of: f8 f9 tr10 tr24 he10 he12 he18


  default value:   SUMA_CrossHairLabelFont = f9

.. _SUMA_IxT_LinkMode:

:envvar:`SUMA_IxT_LinkMode`: Linking mode of I and T sub-brick selectors
Choose one of: None, Same, Stat


  default value:   SUMA_IxT_LinkMode = Stat

.. _SUMA_ArrowFieldSelectorTrigger:

:envvar:`SUMA_ArrowFieldSelectorTrigger`: Minimum Number of sub-bricks to trigger use of arrow field for 
sub-brick selectors.


  default value:   SUMA_ArrowFieldSelectorTrigger = 200

.. _SUMA_Sym_I_Range:

:envvar:`SUMA_Sym_I_Range`: Use symmetric Intensity range at startup? Valid options are:
 YES or NO: For your preference if no decision is made by the software
 FYES or FNO: To force your preference and keep software from deciding


  default value:   SUMA_Sym_I_Range = YES

.. _SUMA_Auto_I_Range:

:envvar:`SUMA_Auto_I_Range`: Set auto Intensity range by default (YES or NO)


  default value:   SUMA_Auto_I_Range = NO

.. _SUMA_Auto_B_Range:

:envvar:`SUMA_Auto_B_Range`: Set auto Brightness range by default (YES or NO)


  default value:   SUMA_Auto_B_Range = NO

.. _SUMA_ContourThickness:

:envvar:`SUMA_ContourThickness`: Set thickness of dataset contours


  default value:   SUMA_ContourThickness = 1.0

.. _SUMA_LHunify:

:envvar:`SUMA_LHunify`: Merge separated left/right states for inflated/spherical/etc. surfaces
Choose from YES or NO

  default value:   SUMA_LHunify = YES

.. _SUMA_SameSurfCont:

:envvar:`SUMA_SameSurfCont`: Put surface controllers in same window
Choose from YES or NO

  default value:   SUMA_SameSurfCont = YES

.. _SUMA_WindowOffset:

:envvar:`SUMA_WindowOffset`: Adjust offset of Surface Viewers as they are first open
Choose from AUTO or provide two X Y offsets.

  default value:   SUMA_WindowOffset = Auto

.. _SUMA_LockViewers:

:envvar:`SUMA_LockViewers`: Lock views across viewers
Choose from YES or NO.

  default value:   SUMA_LockViewers = YES

.. _SUMA_VO_ColorMap:

:envvar:`SUMA_VO_ColorMap`: Set colormap for volumes, choose any of the standard list

  default value:   SUMA_VO_ColorMap = bw20

.. _SUMA_VO_Reorient:

:envvar:`SUMA_VO_Reorient`: Force reorienting of read volume.
To force reorientation,
Choose from RAI, LPI, RAS. etc...
Use NO to avoid reorientation. This env. is for debugging purposes.


  default value:   SUMA_VO_Reorient = NO

.. _SUMA_DriveSumaMaxCloseWait:

:envvar:`SUMA_DriveSumaMaxCloseWait`: Set maximum waiting time for proper detection of closed stream
This is to avoid DriveSuma's: Failed to detect closed stream ...
complaint which results in a forced stream closing. Time unit is
in seconds. See also env SUMA_DriveSumaMaxWait


  default value:   SUMA_DriveSumaMaxCloseWait = 5

.. _SUMA_ObjectDisplayOrder:

:envvar:`SUMA_ObjectDisplayOrder`: Set order in which object types are rendered. This order will affect
the resultant image in the few instances where alpha transparency is
used. The order can be specified for only three types of objects for 
now: graphs, surfaces, and volumes. If you want to render graphs first,
followed by volumes then surfaces then set SUMA_ObjectDisplayOrder to
something like: 'graph,vol,surf'. Do not include spaces between the
type names.

  default value:   SUMA_ObjectDisplayOrder = vol,surf,graph

.. _SUMA_Dset_Font:

:envvar:`SUMA_Dset_Font`: Font for datasets in SUMA viewer
Choose one of: f8 f9 tr10 tr24 he10 he12 he18


  default value:   SUMA_Dset_Font = f9

.. _SUMA_Dset_NodeConnections:

:envvar:`SUMA_Dset_NodeConnections`: Method for representing connections to a certain node in a graph dataset.
Choose one of: Edge, Color, Radius, C&R, XXX


  default value:   SUMA_Dset_NodeConnections = Edge

.. _SUMA_VO_InitSlices:

:envvar:`SUMA_VO_InitSlices`: Set which slices should be shown when a volume is first loaded.
You can set parameters for each of the Ax, Sa, and Co planes, and
the volume rendering.
Each plane gets its own string formatted as such: PL:SL:MON:INC
where:

   PL is the plane (Ax, Co, Sa, or Vr)

   SL is the slice number, you can also set the number as 
     a fraction of the number of slices in the volume.

   MON is the number of montage slices

   INC is the increment between montage slices. You can use 
       fractions for this parameter also.

If you want to set parameters for a certain plane, but do not
want to see it, prepend the plane name with 'h' (for hide) as in 'hAx'
Note that for Vr, there are no SL, MON, and INC qualifiers
Also, SUMA will force the display of at least one plane because
otherwise you have no way of opening a volume controller
Example: 'Ax:0.5:3:10 Co:123:2:50'

  default value:   SUMA_VO_InitSlices = Ax:0.5 Sa:0.5:2:0.5 hCo:0.5

.. _SUMA_HomeAfterPrying:

:envvar:`SUMA_HomeAfterPrying`: Perform 'Home' call in SUMA after each prying.
If YES, objects are repositioned to stay in the middle of the viewer
as you pry the surfaces apart. This behavior is desired in general, 
unless you don't like the initial positioning in the first place.
Choose from YES or NO

  default value:   SUMA_HomeAfterPrying = YES

.. _SUMA_SUMA_TESSCON_AutoScale:

:envvar:`SUMA_SUMA_TESSCON_AutoScale`: Assume surface in TESSCON units if range is extreme
If YES, surfaces with a big difference between max and min dims are
scaled by 319.7. Don't set this env to YES unless this jibber jabber 
means.
Choose from YES or NO

  default value:   SUMA_SUMA_TESSCON_AutoScale = NO


Current Version Info:

Compile Date:
   Oct 17 2014

