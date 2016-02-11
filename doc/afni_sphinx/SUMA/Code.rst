.. _Code:

**************************
**SUMA Programming Notes**
**************************

A pitiful set of tips to trick you into venturing down the SUMA code maze.

.. _Displayable_Objects_Code:

Displayable_Objects
===================

   Any of the varied objects that SUMA can render. :term:`DOs` include things as varied as surface objects (SOs), volume objects (VOs), and NIML Displayable Objects (NIDOs) which can be text, images, etc.

   The set of object types is enumerated in :literal:`SUMA_DO_Types` .

   DOs are tucked into structure  :literal:`SUMA_DO` and stored in global vector  :literal:`SUMAg_DOv` . The :literal:`SUMA_DO` structure consists of the Object Pointer :literal:`OP (void :literal:`)` , its enumerated type, and its coordinate type. Often, objects are referred to by their index into that global vector (see macros with iDO in their name).  

   All DOs can be cast into a structure called :literal:`SUMA_ALL_DO` , which is considerably more useful than a void pointer. For use examples, see macros and functions with ADO in their name. Also, make sure any newly defined DO structure begins with the same elements as in :literal:`SUMA_ALL_DO` .

   Most objects also share common properties such as an idcode, a label, a state, etc. I highly recommend you access object properties through macros or function calls. See the likes of functions :literal:`SUMA_ADO_Label()` , :literal:`SUMA_ADO_variant()` , and macros :literal:`ADO_LABEL` , :literal:`ADO_STATE` . Usually, all caps names are hints that they are macros.

   For an object to be considered for display in a particular viewer, it must be registered with it. When a viewer is placed in a certain viewing state, all objects  registered with that viewer and of a matching state will be eligible for rendering, i.e. they would be rendered unless they are hidden from view by the user's choice.

Functions of Interest
---------------------

   :literal:`SUMA_whichDO(), SUMA_LoadVolDO(), SUMA_Load_Surface_Object_eng(), SUMA_ReadTractDO(), SUMA_ADO_Info(), SUMA_ADO_Cont(), SUMA_DO_dbg_info(), SUMA_ADO_SelectedDatum(), SUMA_ADO_SelectedSecondary(), SUMA_ADO_Set_SelectedDatum()`


.. _DO_freaks:

DO freaks
--------- 

Graph Link Displayable Objects (GLDO)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   Most objects are rendered directly, such as surfaces, or tracts, but there can also be objects that are rendered in different ways depending on the context. For example, consider matrices (a.k.a graphs).

   In SUMA, a matrix is considered as a displayable object of type :literal:`GDSET_type` , that is because the matrix is stored as a NIML group in structure :literal:`SUMA_DSET` much as surface-based datasets. So a :literal:`GDSET_type` displayable object is mostly a pointer to the dataset object itself.
  
   The same matrix dataset can be rendered as a grid (matrix), as a graph usually embedded in 3D, or a variety of other ways (a.k.a. variants). As a result, the rendering of the matrix is done on the fly, using the matrix data, and the rendering variant. To display an object in SUMA however, you need to have an entry for it in :literal:`SUMAg_DOv` , and you need to have it registered with the viewer. We do have an entry for the matrix as an :literal:`GDSET_type` , but how to render it is ambiguous. To remedy this, without duplicating color overlay data and so on, we define the :literal:`SUMA_GraphLinkDO` structure (and enumeration :literal:`GRAPH_LINK_type` often abbreviated as GLDO) which contains little more than the idcode string for the matrix data set, and a string defining the rendering variant. 

   If this is all too confusing, ponder this. A surface object (SO) is mostly made up of nodes and triangles that define its fixed shape; it needs no datasets to be rendered. A graph object is mostly made up of dataset, and the way it is rendered depends on the user's whim.
   
Functions of Interest
---------------------

   See function :literal:`SUMA_CreateGraphDOs()` to see how these DOs are created and start from function :literal:`SUMA_DrawGraphLinkDO()` to get a feel for how these objects get rendered. 
 
.. _Datasets_Code:

Datasets  
========

   :term:`Datasets` are represented by the :literal:`SUMA_DSET` structure with much of the meat stored in the NI_group :literal:`ngr art of the structure. All datasets in SUMA are stored in a linked list called DsetList and present in the global Common Fields structure :literal:`SUMAg_CF` . 
   
   Originally datasets could not be displayed on their own: A surface dataset required a surface upon which it would be displayed. For this reason, they were not considered :term:`Displayable Objects`. 
   
   Graph datasets (i.e. matrices, connectivity datasets) ruined that neat distinction between DSETs and DOs. A graph is a dataset that can always be displayed as a matrix and as a 3D graph when some coordinates are assigned to each node of the graph. So a graph dataset is also a displayable object that can be rendered in multiple ways (read states). So when a graph dataset is loaded, it is stored in DsetList, but place holder DOs are also added to the :literal:`SUMAg_DOv` , as discussed in :ref:`Graph Link Displayable Objects<DO_freaks>`
   
   Here is a sample of the sequence of events when a dataset is loaded onto a surface objects (see :literal:`SUMA_LoadDsetOntoSO_eng()` ) 
   
      1- Load Dataset from file

      2- Assign domain parent to be the DO onto which it is loaded

      .. note:: If a graph dataset, ignore surface parenting and proceed in modified manner 

      3- Add dset to the list :literal:`SUMAg_CF->DsetList` 
      
      4- Setup :ref:`overlays<Overlays_Code>` for this dataset. 
      
      .. note:: Overlays are attached to the displayable object, rather than the dataset.  
      
      5- Colorize the color plane corresponding to this dataset, and make the colorplane be the currently selected one
      
      6- Refresh dataset selection list, update controller settings, remix and redisplay
      
Functions of Interest
---------------------

   :literal:`SUMA_CreateFullDsetPointer(), SUMA_InsertDsetPointer(), SUMA_InsertDsetNelCol(), SUMA_GetDsetColRange(), SUMA_GetDsetColIndex(), SUMA_GetDsetValInCol2(), SUMA_SetDsetLabeltable()` See also SUMA_TestDsetIO.c
 
.. _Viewers_Code:

Viewers
=======

   Viewers are the windows in which varied objects are displayed. Multiple viewers can be linked so that they show objects from the same angle and so that a selection on one object is propagated to the extent possible to other viewers. A viewer is always in a particular :ref:`state<States_Code>` and all objects from that state and that are registered with the viewer will get displayed. The viewer structure :literal:`SUMA_SurfaceViewer` is used to keep track of rendering parameters, to the extent that they apply to multiple objects, and of current user selections. The object last selected by the user is said to be *in focus* . 

Functions of Interest
---------------------

    :literal:`SUMA_RegisterDO(), SUMA_ADO_isRegistered(), SUMA_OneViewerWithADORegistered(), SUMA_SV_Focus_ADO()`
    
         
.. _States_Code:

States
======

:term:`States` in general define a category of objects that should be rendered together. At first, the term referred to the deformation state of a surface. Now however, the definition is stretched a bit. Basically objects of the same state get displayed together. Some states, such as "ANY_ANTOMICAL" are special in that objects with such states get displayed in any viewer state that is also tagged as anatomically correct. For instance, tractography or volume objects are displayed in viewer showing pial surfaces, white matter, or smooth white matter surfaces. Some states, such as "TheShadow" are used as place holders and are not meant to be displayed.
 
Functions of Interest
---------------------

   :literal:`SUMA_FirstGoodState(), SUMA_FirstGoodAnatCorrState(), SUMA_ViewState_Membs(), SUMA_ViewStateInfo(), SUMA_WhichState()`
  
.. _Overlays_Code:

.. _Color_Planes_Code:

Overlays or Color Planes
========================

All DOs have one or multiple :literal:`SUMA_OVERLAYS` , which are the colorized instance of the datasets as they are mapped onto them. These overlays (also called colorplanes) are mixed together to form one final set of colors per elementary object datum (nodes on the surface, edge of a graph, point of a tract, etc.)

   Here is a sequence of events that occurs after a new dataset is loaded. The sequence is loosely based on :literal:`SUMA_LoadDsetOntoSO_eng()` :
   
   1- Create an overlay pointer given the dataset and its domain. See :literal:`SUMA_CreateOverlayPointer()`
   
   2- Add this plane to the list of overlay planes already defined over this domain, e.g. :literal:`SUMA_AddNewPlane()`
   
   3- Setup the options for colorizing this particular plane
   
   4- Colorize the plane with :literal:`SUMA_ColorizePlane()`
   
   5- Remix all the color planes on a particular DO and redisplay with :literal:`SUMA_Remixedisplay()` 
      
      Remixing is handled in :literal:`SUMA_MixColors(Viewer)` where each DO registered with the viewer will get all of its color planes mixed with  :literal:`SUMA_Overlays_2_GLCOLAR4()` --> :literal:`SUMA_MixOverlays()`. The resultant colors for each DO are stored in a structure called :literal:`SUMA_COLORLIST_STRUCT` accessible from the Viewer's structure with the likes of :literal:`SUMA_GetColorListStruct()` and :literal:`SUMA_GetColorListPtr()`
      
         
Functions of Interest
---------------------

   :literal:`SUMA_ADO_Overlays(), SUMA_ADO_CurColPlane(),  SUMA_MixColors(), SUMA_Overlays_2_GLCOLAR4(), SUMA_MixOverlays(), SUMA_ScaleToMap(), SUMA_Fetch_OverlayPointerByDset(), SUMA_CreateOverlayPointer(), SUMA_AddNewPlane(), SUMA_ColorizePlane(), SUMA_Show_ColorOverlayPlanes()`
   

.. _Picking_Code:

Picking
=======

The general outline of the picking process is as follows:

   1- Look for intersection with a visible object
      There are two methods for intersections in SUMA depending on the type of object being tested for intersection. 
      
      The first method is geometric and applies to surfaces, matrices, and volumes. In the geometric approach the click location in the viewer is turned into a pick line in the 3D space in which the object resides. The intersections between its geometric primitives and the line are computed and the primitive closest to the view point is preserved.  

      The second method uses the graphics engine to render all applicable objects (tracts, 3D graphs, text boxes) into a pick buffer image whereby each object primitive is painted with a unique color (R G B A bytes). The pick buffer is then sampled at the click location and the primitive identified by its color. 
      
      .. note::
      
         You can see the pick buffer, for debugging purposes, by Shift Right-Clicking over the object to be selected. The pick buffer is displayed in the recorder window with color ids starting with reddish hues. Note that I don't start using very low R G B A values for the first primitives because I would not be able to visually distinguish between them in the pick buffer when debugging.
      
   2- If an object is intersected, store the intersection in :literal:`SUMA_PICK_RESULT` and add it to :literal:`SelAdo` which is the pick (selection) list inside the surface viewer structure :literal:`SUMA_SurfaceViewer` .
   
   3- Repeat 1 & 2 for all remaining visible objects
   
   4- Sort through all selected objects in the pick list and choose the one having the closest (usually) intersection location to the user's viewing point.  


Functions of Interest
---------------------

   :literal:`SUMA_Show_PickList(), SUMA_Add_To_PickResult_List(), SUMA_Get_From_PickResult_List(), SUMA_Process_Selected_ADO()`
   
   For geometric intersection approach
   
      :literal:`SUMA_ComputeLineDOsIntersect(), SUMA_ComputeLineMaskIntersect(), SUMA_ComputeLineSurfaceIntersect(), SUMA_ComputeLineVOslicesIntersect(), SUMA_ComputeLineVOvrIntersect()`
   
   For pick buffer approach
   
      :literal:`SUMA_PickBuffer(), SUMA_GetColidInPickBuffer4(), SUMA_WhatWasPicked(), SUMA_Bundle_Pick_Intersect()`

For better or for worse
=======================

   A colleaction of comments on some of the oddities in the way certain things are done in SUMA. All for a good reason at some point, including ignorance, but there they are.
   
The Little Engine That Is
-------------------------
   
   The engine function :literal:`SUMA_Engine()` is used to drive SUMA for much of user interactions. The function takes a list of engine structures that direct it to perform various tasks in the listed order. There are functions to create a new engine list, to add commands to an engine list (either prepend or append), and of course SUMA_Engine() to execute the list.  
   
   :literal:`SUMA_Engine()` was created with the tought that all user actions should be scriptable. Most GUI callbacks are mere shells to setup a command list and call :literal:`SUMA_Engine()` 
   
Levels of organization 
----------------------
   
   The big structures are for Displayable Objects ( :literal:`SUMA_SurfaceObject` , :literal:`SUMA_VolumeObject` :literal:`SUMA_TractDO` , etc), Viewers ( :literal:`SUMA_SurfaceViewer` ) ,  Datasets ( :literal:`SUMA_DSET` )
   
   The global variables are all prefixed with "SUMAg_" and the most relevant ones are: :literal:`SUMAg_CF` for all SUMA-wide settings and variables, :literal:`SUMAg_DOv` for all DOs, and :literal:`SUMAg_SVv` for all viewer structs.
   
   Many large pointers can be shared across objects, viewers, etc. Check existing accessor functions, make your own if need be.
   
   When adding fields to a structure, ponder whether they belong to the dataset level, the object level, the viewer level, or SUMA-wide level. Recall that datasets can be shared across objects, and that some datasets effectively double as displayable objects.
   
Debugging Utilities
===================
   
   FuncName: Almost all functions explicitly define the function name in a static variable called FuncName, and they use the macros :literal:`SUMA_ENTRY` and :literal:`SUMA_RETURN` or :literal:`SUMA_RETURNe` for returning variables or a void, respectively. 
   The only exception to this rule would be functions that are called a large number of times and with relatively brief execution time. If you follow this scheme, you can check for improperly entered or terminated functions with AnalyzeTrace -suma_c SUMA*.c ../suma_*.c
    
   LocalHead: A flag local to most functions that turns on otherwise hidden debugging messages with macros :literal:`SUMA_LH` . Macro :literal:`TLH` is a shorthand for turning LocalHead on and off locally within a function.
   
   SUMA_DUMP_TRACE: A macro to dump memory allocation table
   
   Structure Contents: Numerous functions with "Info" in the name create strings detailing the content of a particular structure. Those functions are usually called by counterparts with "Show" in the name. Older debugging functions have "Print" in the name. 

   Functions and macros look for stuff: Look for function and macro names beginning with "SUMA_Which, SUMA_which, or SUMA_WHICH". Also, look for functions and macros with "_Find or _FIND or _find" in the name. There are lots of them.
   
   Functions and macros to ask about stuff: Look for function and macro names beginning with "SUMA_is". 
                         
Unfinished Worthwhile Business
==============================

   On the fly rendering masks with operation such as Do when mask == 0 and Do when mask == 1, and variables such as $SEL $THR $BRI. See semblance of such a feature with patches and numerous surfaces - Daniel & Atlases
   
   Autoload datsets :literal:`SUMA_AutoLoad_SO_Dsets()`
   

Examples (musings perhaps)
==========================

Sitcky moving along the tract of first intersection 
---------------------------------------------------
      
   Tract intersection is done via the :ref:`picking buffer<Picking_Code>` mechanism so one can imagine implementing the sticky feature in one of the following two ways. When in sticky mode, search the pick buffer for the closest pixel that matches the color of the first pick. 
      
   Normally the determination of what was picked from the buffer involves finding the closest colored pixel to the mouse pointer's location (see :literal:`SUMA_ComputeLineDOsIntersect()` )  and then reverse looking up of the object represented by that color ( :literal:`SUMA_WhatWasPicked()`). For the sticky picking to work, the search function has to know to search only for a certain color and you will probably want to increase the search space around the pointer considerably from the current level. Also one should ponder the need to search with preference along the direction of displacement of the pointer to avoid unexpected jump, think of a tract that curls upwards and back on itself like a respectable moustache. 
      
   Another thing to consider is the fact that some tracts don't go far enough in the bundle they are in and one might actualy want to continue tracking along the bundle itself, or a new tract in the bundle should a stoppage be encountered. So in case of stoppage, one should consider the next closest color in the buffer that is for a tract in the same bundle, adopt the new tract if found and continue along it.
      
      .. note:: One could consider other scenarios to implement the searches above. For instance, when sticky track picking is desired, only render the tract or bundles of interest (see :literal:`SUMA_DrawTractDO()` ). Or one could decide to categorize at the bundle, rather than the tract level (see :literal:`SUMA_DO_get_pick_colid()` ).

      
   You will also need to see if there is a configuration of keyboard+click that would put the viewer in Sticky Tract Mode. Mouse and keyboard inputs are handled in :literal:`SUMA_input()` . Looking at "case ButtonPress: --> case Button3:" we see that ControlMask ony (without combination with ShiftMask, or Alt) is not used up. Similarly with mouse motion (dragging) "case MotionNotify:  --> case SUMA_Button_3_Motion:" and button release "case ButtonRelease: --> case Button3".
      
   So here is an outline for implementing this approach:

   #. Setup for adding a flag for being in Sticky Tract Mode.

      Per the reasoning above, this should be done at Ctrl+ButtonPress3 and can be encoded as a new value for :literal:`MouseMode` in the :literal:`SUMA_SurfaceViewer` struct. Search for constant :literal:`SUMA_MASK_MANIP_MMODE` and macro :literal:`MASK_MANIP_MODE` for an example on how such modes are set and queried. 

      However we must allow  :literal:`MouseMode` to simultaneously encode for both  Mask Manipulation  and Sticky Tract Modes. So to make :literal:`MouseMode`  more easily queried, consider turning it into a bitwise mask. At the moment, it is just a series of integer values. For an example of bitwise mask, see definitions for :literal:`UPDATE_ROT_MASK` and its ilk, along with the use of :literal:`viewopt` in :literal:`SUMA_SetupSVforDOs()` .

      Consider also changing the crosshair from arrow to '+' (perhaps) to indicate that one is in a different mouse manipulation mode. This is now done for drawing ROIs; see  :literal:`SUMA_UpdateViewerCursor()` for inspiration. 

      Also, should one only turn Sticky Mode on only when the hit is on a tract?

   #. Modify the search in :literal:`SUMA_ComputeLineDOsIntersect()` or perhaps only in  :literal:`SUMA_GetColidInPickBuffer4()` to act differently in Tract Sticky mode

   #. Snap out of Tacky Mode once Button3Release happens (regardless of whether or not user still has ctrl down perhaps?)

       So we can plan on setting :literal:`MouseMode` in sticky tract mode with ctrl+Button3Press (only if a tract is selected?), modify intersection rules during ctrl+Button_3_Motion, then unset Sticky Tract mode durin Button3Release.


Tract intersection with masks 
-----------------------------

   Currently, interactively controlled tract masks are either spheres or boxes that are defined on the fly in the :ref:`masks table <MaskCont>` of the :ref:`tract controller<TractCont>`. The intent here is to make it possible to specify a generic mask, let's say a surface of arbitrary shape as another mask type.
   
   Things we would need to consider:
   
      #. Intersection of the mask with tracts. For simple masks like sphere and box, the computation of intersections is rapid enough to allow for interactive use. For arbitrary shapes this may not be the case, so it would be wise to keep these objects fixed and preserve the intersection mask for repeated uses. To compute the intersection of a segment with an arbitrary surface, one can use :literal:`SUMA_MT_intersect_triangle()` . The problem is that one will be looking to intersect the segment of every pair of tract points with the whole surface, so this would make the process horrendously slow. The intersection could be sped up however by first checking the intersection of the segment with the box circuscribing the arbitrary SO ( see :literal:`SO->MaxDims`, :literal:`SO->MinDims` )and then proceeding for checking the intersection with the arbitrary surface. A similar strategy is carried out for tract with sphere intersection. See function :literal:`SUMA_TractMaskIntersect()` where you will also be handling the intersection with the arbitraty surface.
      
         .. note:: See also functions :literal:`SUMA_isinbox(), SUMA_ComputeLineMaskIntersect(), SUMA_TractMasksIntersect()` , and functions :literal:`Network_*(), SUMA_TDO*(), and SUMA_MDO*()` 
      
      #. Adding an entry of the mask object in the :ref:`masks controller table` <MaskCont>`. Some of the table's fields may not be terribly appropriate for such fixed objects, but I think that is OK. See functions :literal:`SUMA_InitMasksTable(), SUMA_ModifyTable(), SUMA_InitMasksTable_row()` for tips on how to start. One would also need a way to initiate the loading of an arbitrary mask. Such masks could come from :ref:`IsoSurface<IsoSurface>` and might be made up of multiple 'blobs'. In that instance you might consider combining such blobs into one surface object (see option :ref:`-mergerois+dset<IsoSurface--mergerois+dset>` in IsoSurface). 
      
      As for loading the mask object, you could piggy back on the current :ref:`Load Masks<MaskCont->Masks->Load_Masks>` button, and DriveSuma's :ref:`-load_masks option<DriveSuma--load_masks>`.
      
      

