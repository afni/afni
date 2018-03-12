********
@AddEdge
********

.. _@AddEdge:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    A script to create composite edge-enhanced datasets and drive
     the AFNI interface to display the results
    The script helps visualize registration results and is an important
     part of assessing image alignmnent
    
    Basic usage:
    
       @AddEdge base_dset dset1 dset2 ....
    
       The output is a composite image of each dset nn with the base
       dataset where the composite image is the base dataset with the
       edges of each input dataset and its own edges
    
       Use without any parameters to drive AFNI's display to show
       the previously computed results from this script
    
       The script requires all input datasets to share the same grid, so
       a previous resample step may be required. Also it is recommended
       to use skull-stripped input datasets to avoid extraneous and
       extracranial edges.
    
    A typical use may be to compare the effect of alignment
     as in this example for the alignment of anatomical dataset with an
     epi dataset:
    
       @AddEdge epi_rs+orig. anat_ns+orig anat_ns_al2epi+orig
    
     Note this particular kind of usage is included in the
       align_epi_anat.py script as the -AddEdge option
    
    To examine results, rerun @AddEdge with -auto
    
       @AddEdge -auto
    
    Using the typical case example above, the edges from the EPI
     are shown in cyan (light blue); the edges from the anat dataset
     are shown in purple. Overlapping edges are shown in dark purple
     Non-edge areas (most of the volume) are shown in a monochromatic
     amber color scale in the overlay layer of the AFNI image window
     The underlay contains the edge-enhanced anat dataset with edges
     of the anat dataset alone snd no EPI edges
    By looking for significant overlap and close alignment of the
     edges of internal structures of the brain, one can assess the
     quality of the alignment.
    The script prompts the user in the terminal window to cycle between
     the pre-aligned and post-aligned dataset views. Options are also
     given to save images as jpeg files or to quit the @AddEdge script
    
    The colormap used is the AddEdge color scale which uses a monochrome
     amber for the overlay and purple, cyan and dark purple for edges
    
    Several types of datasets are created by this script, but using the
     @AddEdge script without options is the best way to visualize these
     datasets. The result datasets can be grouped by their suffix as
     follows:
    
    dset_nn_ec : edge composite image of dataset with its own edges
    base_dset_dset_nn_ec : edge composite image of base dataset together
                     with the edges of the input dset_nn dataset
    base_dset_e3, dset_nn_e3: edge-only datasets - used in single edge
                     display option
    
    Available options (must precede the dataset names):
    
     -help         : this help screen
     -examinelist mmmm : use list of paired datasets from file mmmm
                   (default is _ae.ExamineList.log)
     -ax_mont 'montformat': axial montage string (default='2x2:24')
     -ax_geom 'geomformat': axial image window geometry
                   (default = '777x702+433+334')
     -sag_geom 'geomformat': sagittal image window geometry
                   (default = '540x360+4+436')
     -layout mmmm  : use AFNI layout file mmmm for display
     -no_layout    : do not use layout. Use AFNI as it is open.
     -edge_percentile nn: specify edge threshold value (default=30%)
     -single_edge  : show only a single edge in composite image
     -opa          : set opacity of overlay (default=9 opaque)
     -keep_temp    : do not remove temporary files
     -no_deoblique : do not deoblique any data to show overlap
     -auto_record  : save jpeg files of current slices  without prompting
     -auto: Closes old AFNI sessions and relaunch a new one that
            ready to listen to @AddEdge in review mode. This is 
            the current default in review mode
     -no_auto: Opposite of -auto
