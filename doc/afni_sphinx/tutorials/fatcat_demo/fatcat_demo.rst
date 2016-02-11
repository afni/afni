.. _FATCAT_Demo:

===============
**FATCAT Demo**
===============

.. note::
   
   Looking for :ref:`installation instructions?<FATCAT_Demo_Install>`. 
   
.. Paul: Do you want to add a few words here?

.. _Do_00_PRESTO_ALL_RUNS.tcsh:

.. _Do_01_RUNdti_convert_grads.tcsh:

.. _Do_01_RUNhardi_convert_grads.tcsh:

.. _Do_02_RUNdti_DW_to_DTI.tcsh:

.. _Do_03_RUNdti_make_network_ROIs.tcsh:

.. _Do_04_RUNdti_match_network_ROIs.tcsh:

.. _Do_05_RUNdti_DET_tracking.tcsh:

.. _Do_05_RUNhardi_DET_tracking.tcsh:

.. _Do_07_RUNdti_uncertainty_est.tcsh:

.. _Do_07_RUNhardi_uncertainty_est.tcsh:

.. _Do_08_RUNdti_miniprob_track.tcsh:

.. _Do_08_RUNhardi_miniprob_track.tcsh:

.. _Do_10_RUNdti_fullprob_track.tcsh:

.. _Do_10_RUNhardi_fullprob_track.tcsh:

.. _Do_11_RUNdti_Connectome_Examp.tcsh:

.. _Do_12_RUNrsfc_netw_corr.tcsh:

.. _Do_13_RUNrsfc_RSFCfilt_param.tcsh:

.. _Do_14_RUNrsfc_ReHo_param.tcsh:

.. _Do_PostTORTOISE_2014.tcsh:

At some point you should read the README file under FATCAT_Demo, and go over the comments in each of the scripts that provide context to the different commands used. Scripts Do_*_VIS* are meant to illustrate interactive visualization of the results. The scripts will open results in AFNI and SUMA and a prompt window will guide throuh basic viewing steps. See also the :ref:`quick tour for tract viewing<Tract_Viewing>`. You should be able to create images such as the ones shown above. Fun times!


.. _Do_06_VISdti_SUMA_visual_ex1.tcsh:

Do_06_VISdti_SUMA_visual_ex1.tcsh
---------------------------------

Results visualized by script Do_06_VISdti_SUMA_visual_ex1.tcsh

   .. figure:: media/Do_06_vis1.jpg
      :align: left
      :figwidth: 40%
      :target: ../../_images/Do_06_vis1.jpg
      :name: media/Do_06_vis1.jpg
      
      Tracts going though an interactively positioned :ref:`mask<TractCont->Coloring_Controls->Masks>` in the midbrain area. :ref:`Slice transparency<VolCont->Slice_Controls->Trn>` set to 8. :ref:`(link)<media/Do_06_vis1.jpg>` 


   .. figure:: media/Do_06_vis2.jpg
      :align: right
      :figwidth: 40%
      :target: ../../_images/Do_06_vis2.jpg
      :name: media/Do_06_vis2.jpg

      Tracts going though one ROI, with :ref:`volume rendering<VolCont->VR->Ns->v>` turned on. :ref:`(link)<media/Do_06_vis2.jpg>` 
   
   .. container:: clearer

      .. image:: media/blank.jpg
   
   
.. _Do_09_VISdti_SUMA_visual_ex2.tcsh:

Do_09_VISdti_SUMA_visual_ex2.tcsh
---------------------------------

Results visualized by script Do_09_VISdti_SUMA_visual_ex2.tcsh

   .. figure:: media/Do_09_vis2.1.jpg
      :align: left
      :figwidth: 40%
      :target: ../../_images/Do_09_vis2.1.jpg
      :name: media/Do_09_vis2.1.jpg
      
      Surfaces with surface contour (see :ref:`IsoSurface<IsoSurface>`) of ROI in color, slices, and tracts barely visible. ref:`(link)<media/Do_06_vis2.1.jpg>`

   .. figure:: media/Do_09_vis2.2.jpg
      :align: right
      :figwidth: 40%
      :target: ../../_images/Do_09_vis2.2.jpg
      :name: media/Do_09_vis2.2.jpg

      Surfaces hidden to reveal deterministic tracts through ROIs of the DMN. ref:`(link)<media/Do_09_vis2.2.jpg>` 


   .. figure:: media/Do_09_vis2.3.jpg
      :align: center
      :figwidth: 50%
      :target: ../../_images/Do_09_vis2.3.jpg
      :name: media/Do_09_vis2.3.jpg

      Connection is maintained with AFNI which is displaying intersection of surfaces with the volume. Clicking anywhere in SUMA will cause AFNI to jump to the corresponding location. :ref:`(link)<media/Do_09_vis2.3.jpg>`

   .. container:: clearer

      .. image:: media/blank.jpg
      
   Comparison of deterministic to mini-probabilistic results.
   
   .. figure:: media/Do_09_vis2.4.jpg
      :align: left
      :figwidth: 44%
      :target: ../../_images/Do_09_vis2.4.jpg
      :name: media/Do_09_vis2.4.jpg
      
      :ref:`Deterministic.<media/Do_09_vis2.4.jpg>`


   .. figure:: media/Do_09_vis2.5.jpg
      :align: right
      :figwidth: 44%
      :target: ../../_images/Do_09_vis2.4.jpg
      :name: media/Do_09_vis2.5.jpg
      
      :ref:`Probabilistic.<media/Do_09_vis2.5.jpg>`

   .. container:: clearer

      .. image:: media/blank.jpg

   Composite display of :ref:`connectivity matrices<GraphCont>` 

   .. figure:: media/Do_09_vis3.1.jpg
      :align: center
      :name: media/Do_09_vis3.1.jpg
      
      :ref:`(link)<media/Do_09_vis3.1.jpg>`
      ..
      
Do_09_VISdti_SUMA_visual_ex3.tcsh
---------------------------------

.. _Do_09_VISdti_SUMA_visual_ex3.tcsh:

.. _F+A_Conn:


   Script Do_09_VISdti_SUMA_visual_ex3.tcsh can be used to demonstrate simultaneous, interactive, functional and anatomical connectivity. Just follow the directions that come up on the screen when you launch the script.
   
   .. figure:: ../../SUMA/media/f+a_conn.jpg
      :align: center
      :name: SUMA/media/f+a_conn.jpg
      
      :ref:`(link)<SUMA/media/f+a_conn.jpg>`
      

Connectoming
------------

   Results of script Do_11_RUNdti_Connectome_Examp.tcsh which finds connections between a large number of ROIs.
   
   .. figure:: media/Connectoming.jpg
      :align: center
      :target: ../../_images/Connectoming.jpg
      :name: media/Connectoming.jpg
      
      The tracts here are :ref:`colored<TractCont->Coloring_Controls->Switch_Dset>` depending on which pair of ROIs they join. :ref:`(link)<media/Connectoming.jpg>`. To launch suma with the results displayed here, use::
      
         suma -tract CONNECTOMING/o.OME8_000.niml.tract


