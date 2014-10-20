.. _FATCAT_Demo:

============
FATCAT Demo:
============

FATCAT Demo contains a set of diffusion weighted data and scripts to analyze them. The scripts are intended to illustrate the various tools of the FATCAT toolbox and some of the ways to visualize the results. 

To install FATCAT_DEMO, simply run::

   @Install_FATCAT_DEMO
   
The script will download and install the data into a directory called *FATCAT_DEMO/* . In there you will find a README file and a set of Do_* scripts that are meant to be run sequencially. If you would prefer to run all the scripts in one swell foop, just do::

   cd FATCAT_DEMO
   tcsh Do_00_PRESTO_ALL_RUNS.tcsh
   

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

In the meanwhile we trust you will read the README file and go over the comments in each of the scripts that provide context to the different commands used.

.. _Do_06_VISdti_SUMA_visual_ex1.tcsh:

.. figure:: media/Do_06_vis1.jpg
   :align: center
   
..

.. figure:: media/Do_06_vis2.jpg
   :align: center
   
..

.. _Do_09_VISdti_SUMA_visual_ex2.tcsh:

.. figure:: media/Do_09_vis2.1.jpg
   :align: center
   
   ..
   
.. figure:: media/Do_09_vis2.2.jpg
   :align: center
   
   ..

.. figure:: media/Do_09_vis2.3.jpg
   :align: center
   
   ..

.. figure:: media/Do_09_vis2.4.jpg
   :align: center
   
   ..

.. figure:: media/Do_09_vis2.5.jpg
   :align: center
   
   ..


.. _Do_09_VISdti_SUMA_visual_ex3.tcsh:

.. figure:: media/Do_09_vis3.1.jpg
   :align: center
   
..


Scripts Do_*_VIS* are meant to illustrate interactive visualization of the results. The scripts will open results in AFNI and SUMA and a prompt window will guide throuh basic viewing steps. Should be able to create images such as the ones shown above. Fun times!


