
.. begin_title

.. title(s) with links; usually just a single paper here
**Chen GC, Taylor PA, Shin Y-W, Reynolds RC, Cox RW (2016).**
`Untangling the Relatedness among Correlations, Part II: Inter-Subject
Correlation Group Analysis through Linear Mixed-Effects Modeling
<https://www.ncbi.nlm.nih.gov/pubmed/27751943Neuroimage>`_.
Neuroimage (in press).

**Chen GC, Shin Y-W, Taylor PA, Glen DR, Reynolds RC, Israel RB, Cox
RW (2016).** `Untangling the Relatedness among Correlations, Part I:
Nonparametric Approaches to Inter-Subject Correlation Analysis at the
Group Level <https://www.ncbi.nlm.nih.gov/pubmed/27195792>`_.
Neuroimage (in press).  `Corrigendum
<http://www.sciencedirect.com/science/article/pii/S1053811916305754>`_

.. end_title


.. begin_short_tags

:**tags**: naturalistic, EPI, MPRAGE, human, control, adult, Talairach,
           nonlinear, FreeSurfer, fANATICOR

.. end_short_tags


.. begin_long_tags

.. full table format of search strings
.. table::
   :column-alignment: left 
   :column-wrapping: true 
   :column-dividers: double single double

   =======================  ===================
   Tag                      Label
   =======================  ===================
   FMRI paradigm:           naturalistic 
   FMRI dset:               EPI          
   Anatomical dset:         MPRAGE       
   Subject population:      human        
   Subject characteristic:  control      
   Subject age:             adult        
   Template space:          Talairach    
   Template align method:   nonlinear    
   Tissue segmentation:     FreeSurfer   
   Tissue regression:       fANATICOR    
   Comments:                
   =======================  ===================

.. end_long_tags


.. NB, nothing needs to be put into this next field-- could just
   remain blank!
.. begin_script_note

|

.. note:: 

   :s.2016_ChenEtal_01_init.tcsh:

      The selection of WM regions in line 30 has changed to: ``-expr
      'amongst(a,2,7,41,46,251,252,253,254,255)'``.

.. end_script_note


.. begin_script_table

.. list-table:: 
   :header-rows: 0

   * - |s01|
     - FreeSurfer segmentation; ``@SUMA_Make_Spec_FS``; tissue selection
   * - |s02|
     - ``afni_proc.py`` command


.. aliases for scripts, so above is easier to read
.. |s01| replace:: :download:`s.2016_ChenEtal_01_init.tcsh
                   <fmri_proc/2016_ChenEtal/s.2016_ChenEtal_01_init.tcsh>`
.. |s02| replace:: :download:`s.2016_ChenEtal_02_ap.tcsh
                   <fmri_proc/2016_ChenEtal/s.2016_ChenEtal_02_ap.tcsh>`
  
.. end_script_table
