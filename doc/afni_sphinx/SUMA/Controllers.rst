.. _Controllers:

***************
**Controllers**
***************

GUI controllers make up three groups:

   * :ref:`SUMA Controller<SumaCont>` To control aspects common to all the SUMA viewers. 
   
   * Viewer Controller: To control aspects particular to one viewer. This baby is practically nonexistent at this stage.
   
   * Object Controllers: To control how certain objects and the data defined over them are displayed. In modern SUMA versions, it is best that all controllers are collected in the same :ref:`controller notebook <Controller_Notebook>`. Example controllers include the :ref:`Surface Controller<SurfCont>`, the :ref:`Tract Controller<TractCont>`, the :ref:`Volume Controller<VolCont>` etc.


Global Controller
=================

.. include:: auto_inc/SumaCont_help.inc 

Object Controllers
==================

   .. note::
   
      .. _Controller_Notebook:

      **Controller Notebook** 

      Many of the displayable objects, particularly those that can carry data, have an object controller. Historically there was only surface-controllers (hence the **Ctrl+s** for the shortcut) but now volumes, tracts, and graphs also have their own controllers. 
      
      The easiest way to open a controller is to select an object and open its controller with :kbd:`ctrl+s`, or :menuselection:`View --> Object Controller`. Once a controller is open, selecting other objects automatically creates their own controller. 
      
      All object controllers are grouped in one notebook window as shown in :ref:`Object Controller <object_controller_notebook>`. If you don't have all your object controllers opening in the same notebook and your SUMA version is current, make sure environment variable :ref:`SUMA_SameSurfCont<SUMA_SameSurfCont>` is set to **YES** in your :term:`.sumarc` file. 

      .. _object_controller_notebook:

      .. figure:: media/object_controller_notebook_gray.jpg
         :align: center
         :figwidth: 50 %
         :name: media/object_controller_notebook_gray.jpg
         
         Object Controller Notebook: Holder of all controllers. Grayed out area will be different for different object types. :ref:`(link)<media/object_controller_notebook_gray.jpg>`

      Once you select an object, its controller is popped to the top. You can also use the :ref:`Switch <GL_CN_Switch>` to get at the controller for an object that you don't want to select or that is simply out of reach (invisible). 

      .. This next block should be automated with auto-snap etc...
      
      **Disp. Cont.**

      A few controls for the object controller notebook.

         .. _GL_CN_Close:

         :ref:`Close<GL_CN_Close>`: Close controller. Settings are not lost. You can bring it back with :ref:`Ctrl+s <LC_Ctrl+s>` key.

         .. _GL_CN_BHelp:

         :ref:`BHelp<GL_CN_BHelp>`: Obtain context specific help by clicking on this button then clicking on the context for which you want information.

         .. _GL_CN_WHelp:

         :ref:`WHelp<GL_CN_WHelp>`: Obtain web-based context specific help by clicking on this button then clicking on the context for which you want information.

         .. _CL_CN_All_Objs:

         :ref:`All Objs<CL_CN_All_Objs>`: Initialize controllers for all objects that have one. This is particularly useful when a particular may not be visible under the default settings.
 
         .. _GL_CN_Switch:

         :ref:`Switch<GL_CN_Switch>`: Switch between controller notebook pages. You can use the arrows to cycle between pages or set the page number directly. 

 
.. include:: auto_inc/SurfCont_help.inc 
.. include:: auto_inc/TractCont_help.inc 
.. include:: auto_inc/MaskCont_help.inc 
.. include:: auto_inc/VolCont_help.inc 
.. include:: auto_inc/GraphCont_help.inc
.. include:: auto_inc/ROICont_help.inc

