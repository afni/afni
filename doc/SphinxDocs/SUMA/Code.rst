.. _Code:

**********************
SUMA Programming Notes
**********************

A pitiful set of tips to trick you into venturing down the SUMA code maze.

.. _Displayable_Objects_Code:

Displayable_Objects
===================

   Any of the varied objects that SUMA can render. :term:`DOs` include things as varied as surface objects (SOs), volume objects (VOs), and NIML Displayable Objects (NIDOs) which can be text, images, etc.

   The set of object types is enumerated in **SUMA_DO_Types** .

   DOs are tucked into structure  **SUMA_DO** and stored in global vector  **SUMAg_DOv**. The **SUMA_DO** structure consists of the Object Pointer **OP (void *)** , its enumerated type, and its coordinate type. Often, objects are referred to by their index into that global vector (see macros with iDO in their name).  

   All DOs can be cast into a structure called **SUMA_ALL_DO**, which is considerably more useful than a void pointer. For use examples, see macros and functions with ADO in their name. Also, make sure any newly defined DO structure begins with the same elements as in **SUMA_ALL_DO** .

   Most objects also share common properties such as an idcode, a label, a state, etc. I highly recommend you access object properties through macros or function calls. See the likes of functions *SUMA_ADO_Label()* , *SUMA_ADO_variant()* , and macros *ADO_LABEL* , *ADO_STATE* . Usually, all caps names are hints that they are macros.

   For an object to be considered for display in a particular viewer, it must be registered with it. When a viewer is placed in a certain viewing state, all objects  registered with that viewer and of a matching state will be eligible for rendering, i.e. they would be rendered unless they are hidden from view by the user's choice.

Functions of Interest
---------------------

   *SUMA_whichDO, SUMA_LoadVolDO(), SUMA_Load_Surface_Object_eng(), SUMA_ReadTractDO(), SUMA_ADO_Info(), SUMA_ADO_Cont(), SUMA_DO_dbg_info()*


.. _DO_freaks:

DO freaks
--------- 

Graph Link Displayable Objects (GLDO)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   Most objects are rendered directly, such as surfaces, or tracts, but there can also be objects that are rendered in different ways depending on the context. For example, consider matrices (a.k.a graphs).

   In SUMA, a matrix is considered as a displayable object of type **SDSET_type** , that is because the matrix is stored as a NIML group in structure **SUMA_DSET** much as surface-based datasets. So a **SDSET_type** displayable object is mostly a pointer to the dataset object itself.
  
   The same matrix dataset can be rendered as a grid (matrix), as a graph usually embedded in 3D, or a variety of other ways (a.k.a. variants). As a result, the rendering of the matrix is done on the fly, using the matrix data, and the rendering variant. To display an object in SUMA however, you need to have an entry for it in **SUMAg_DOv** , and you need to have it registered with the viewer. We do have an entry for the matrix as an **SDSET_type**, but how to render it is ambiguous. To remedy this, without duplicating color overlay data and so on, we define the **SUMA_GraphLinkDO** structure (and enumeration **GRAPH_LINK_type** often abbreviated as GLDO) which contains little more than the idcode string for the matrix data set, and a string defining the rendering variant. 

   If this is all too confusing, ponder this. A surface object (SO) is mostly made up of nodes and triangles that define its fixed shape; it needs no datasets to be rendered. A graph object is mostly made up of dataset, and the way it is rendered depends on the user's whim.
   
Functions of Interest
---------------------

   See function *SUMA_CreateGraphDOs()* to see how these DOs are created and start from function *SUMA_DrawGraphLinkDO()* to get a feel for how these objects get rendered. 
 
.. _Datasets_Code:

Datasets  
========

   :term:`Datasets` are represented by the **SUMA_DSET** structure with much of the meat stored in the NI_group *ngr art of the structure. All datasets in SUMA are stored in a linked list called DsetList and present in the global Common Fields structure **SUMAg_CF** . 
   
   Originally datasets could not be displayed on their own: A surface dataset required a surface upon which it would be displayed. For this reason, they were not considered :term:`Displayable Objects`. 
   
   Graph datasets (i.e. matrices, connectivity datasets) ruined that neat distinction between DSETs and DOs. A graph is a dataset that can always be displayed as a matrix and as a 3D graph when some coordinates are assigned to each node of the graph. So a graph dataset is also a displayable object that can be rendered in multiple ways (read states). So when a graph dataset is loaded, it is stored in DsetList, but place holder DOs are also added to the **SUMAg_DOv**, as discussed in :ref:`Graph Link Displayable Objects<DO_freaks>`
   
   Here is a sample of the sequence of events when a dataset is loaded onto a surface objects (see *SUMA_LoadDsetOntoSO_eng()* ) 
   
      1- Load Dataset from file

      2- Assign domain parent to be the DO onto which it is loaded

      .. note:: If a graph dataset, ignore surface parenting and proceed in modified manner 

      3- Add dset to the list **SUMAg_CF->DsetList**
      
      4- Setup :ref:`overlays<Overlays_Code>` for this dataset. 
      
      .. note:: Overlays are attached to the displayable object, rather than the dataset.  
      
      5- Colorize the color plane corresponding to this dataset, and make the colorplane be the currently selected one
      
      6- Refresh dataset selection list, update controller settings, remix and redisplay
      
Functions of Interest
---------------------

   *SUMA_CreateFullDsetPointer(), SUMA_InsertDsetPointer(), SUMA_InsertDsetNelCol(), SUMA_GetDsetColRange(), SUMA_GetDsetColIndex(), SUMA_GetDsetValInCol2(), SUMA_SetDsetLabeltable()* See also SUMA_TestDsetIO.c
 
.. _Viewers_Code:

Viewers
=======

   Nothing here yet.   
      
.. _States_Code:

States
======

:term:`States` in general define a category of objects that should be rendered together. At first, the term referred to the deformation state of a surface. Now however, the definition is stretched a bit. Basically objects of the same state get displayed together. Some states, such as "ANY_ANTOMICAL" are special in that objects with such states get displayed in any viewer state that is also tagged as anatomically correct. For instance, tractography or volume objects are displayed in viewer showing pial surfaces, white matter, or smooth white matter surfaces. Some states, such as "TheShadow" are used as place holders and are not meant to be displayed.
 
Functions of Interest
---------------------

   *SUMA_FirstGoodState(), SUMA_FirstGoodAnatCorrState(), SUMA_ViewState_Membs(), SUMA_ViewStateInfo(), SUMA_WhichState()*
  
.. _Overlays_Code:

.. _Color_Planes_Code:

Overlays or Color Planes
========================

All DOs have one or multiple **SUMA_OVERLAYS**, which are the colorized instance of the datasets as they are mapped onto them. These overlays (also called colorplanes) are mixed together to form one final set of colors per elementary object datum (nodes on the surface, edge of a graph, point of a tract, etc.)

   Here is a sequence of events that occurs after a new dataset is loaded. The sequence is loosely based on *SUMA_LoadDsetOntoSO_eng()* :
   
   1- Create an overlay pointer given the dataset and its domain. See *SUMA_CreateOverlayPointer()*
   
   2- Add this plane to the list of overlay planes already defined over this domain, e.g. *SUMA_AddNewPlane()*
   
   3- Setup the options for colorizing this particular plane
   
   4- Colorize the plane with *SUMA_ColorizePlane()*
   
   5- Remix all the color planes on a particular DO and redisplay with *SUMA_Remixedisplay()* 
      
      Remixing is handled in *SUMA_MixColors(Viewer)* where each DO registered with the viewer will get all of its color planes mixed with  *SUMA_Overlays_2_GLCOLAR4()* --> *SUMA_MixOverlays()*. The resultant colors for each DO are stored in a structure called **SUMA_COLORLIST_STRUCT** accessible from the Viewer's structure with the likes of *SUMA_GetColorListStruct()* and *SUMA_GetColorListPtr()* .
   
Functions of Interest
---------------------

   *SUMA_ADO_Overlays(), SUMA_ADO_CurColPlane(),  SUMA_MixColors(), SUMA_Overlays_2_GLCOLAR4(), SUMA_MixOverlays(), SUMA_ScaleToMap(), SUMA_Fetch_OverlayPointerByDset(), SUMA_CreateOverlayPointer(), SUMA_AddNewPlane(), SUMA_ColorizePlane(), SUMA_Show_ColorOverlayPlanes()*
   

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
      
   2- If an object is intersected, store the intersection in **SUMA_PICK_RESULT** and add it to **SelAdo** which is the pick (selection) list inside the surface viewer structure **SUMA_SurfaceViewer** .
   
   3- Repeat 1 & 2 for all remaining visible objects
   
   4- Sort through all selected objects in the pick list and choose the one having the closest (usually) intersection location to the user's viewing point.  


Functions of Interest
---------------------

   *SUMA_Show_PickList(), SUMA_Add_To_PickResult_List(), SUMA_Get_From_PickResult_List(), SUMA_Process_Selected_ADO()*
   
   For geometric intersection approach
   
      *SUMA_ComputeLineDOsIntersect(), SUMA_ComputeLineMaskIntersect(), SUMA_ComputeLineSurfaceIntersect(), SUMA_ComputeLineVOslicesIntersect(), SUMA_ComputeLineVOvrIntersect()*
   
   For pick buffer approach
   
      *SUMA_PickBuffer(), SUMA_GetColidInPickBuffer4(), SUMA_WhatWasPicked(), SUMA_Bundle_Pick_Intersect()*

For better or for worse
=======================

   The Little Engine That Is
   -------------------------
   
   The engine function *SUMA_Engine()* is used to drive SUMA for much of user interactions. The function takes a list of engine structures that direct it to perform various tasks in the listed order. There are functions to create a new engine list, to add commands to an engine list (either prepend or append), and of course SUMA_Engine() to execute the list.  
   
   *SUMA_Engine()* was created with the tought that all user actions should be scriptable. Most GUI callbacks are mere shells to setup a command list and call *SUMA_Engine()* 
   
   Levels of organization 
   ----------------------
   
   The big structures are for Displayable Objects ( **SUMA_SurfaceObject**, **SUMA_VolumeObject** **SUMA_TractDO**, etc), Viewers ( **SUMA_SurfaceViewer**) ,  Datasets ( **SUMA_DSET** )
   
   The global variables are all prefixed with SUMAg_ and the most relevant ones are: **SUMAg_CF** for all SUMA-wide settings and variables, **SUMAg_DOv** for all DOs, and **SUMAg_SVv** for all viewer structs.
   
   Many large pointers can be shared across objects, viewers, etc. Check existing accessor functions, make your own if need be.
   
   When adding fields to a structure, ponder whether they belong to the dataset level, the object level, the viewer level, or SUMA-wide level. Recall that datasets can be shared across objects, and that some datasets effectively double as displayable objects.
   
Debugging Utilities
===================

   LocalHead: A flag local to most functions that turns on otherwise hidden debugging messages with macros *SUMA_LH* . Macro *TLH* is a shorthand for turning LocalHead on and off locally within a function.
   
   SUMA_DUMP_TRACE: A macro to dump memory allocation table
   
   Structure Contents: Numerous functions with "Info" in the name create strings detailing the content of a particular structure. Those functions are usually called by counterparts with "Show" in the name. Older debugging functions have "Print" in the name. 

Document which functions, is functions, find functions and macros                      
Unfinished Worthwhile Business
==============================

   On the fly rendering masks with operation such as Do when mask == 0 and Do when mask == 1, and variables such as $SEL $THR $BRI. See semblance of such a feature with patches and numerous surfaces - Daniel & Atlases
   
   Autoload datsets *SUMA_AutoLoad_SO_Dsets()*
   
