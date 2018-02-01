************
FSread_annot
************

.. _FSread_annot:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage:  
      FSread_annot   <-input ANNOTFILE>  
                     [-FScmap CMAPFILE]   
                     [-FScmaprange iMin iMax]
                     [-FSversion VER]
                     [-col_1D annot.1D.col]  
                     [-roi_1D annot.1D.roi] 
                     [-cmap_1D annot.1D.cmap]
                     [show_FScmap]
                     [-help]  
      Reads a FreeSurfer annotaion file and outputs
      an equivalent ROI file and/or a colormap file 
      for use with SUMA.
    
      Required options:
         -input ANNOTFILE: Binary formatted FreeSurfer
                           annotation file.
         AND one of the optional options.
      Optional options:
         -FScmap CMAPFILE: Get the colormap from the Freesurfer 
                           colormap file CMAPFILE.
                           Colormaps inside the ANNOTFILE would be
                           ignored. See also MakeColorMap's fscolut* options.
                           With FSversion set to 2009, if FScmap is not set, 
                           the program will attempt to locate 
                           FreeSurferColorLUT.txt based on the environment
                           variable $FREESURFER_HOME
                           You can use FS_DEFAULT to force the program to load
                           FreeSurfer's $FREESURFER_HOME/FreeSurferColorLUT.txt
         -FScmaprange iMin iMax: CMAPFILE contains multiple types of labels
                           The annotation values in ANNOTFILE can map to multiple
                           labels if you do not restrict the range with 
                           iMin and iMax. That is because annotation values
                           encode color in RGB which is used to lookup a name
                           and integer label from CMAPFILE. The same color is 
                           used for multiple labels.
                           When an external CMAPFILE is needed (a2009 versions), 
                           the programs uses a default of [13100 13199] for lh,
                           [14100 14199] for rh surfaces.
                           If CMAPFILE is set to FS_DEFAULT in a2005 versions,
                           the programs uses a default of [3100 3199] for lh,
                           [4100 4199] for rh surfaces.
         -FSversion VER: VER is the annotation file vintage. Choose from 2009, 
                         or 2005. The program will attempt to guess from the name
                         ANNOTFILE and would default to 2005.
         -hemi HEMI: Specify hemisphere. HEMI is one of lh or rh.
                     Program guesses by default
         -col_1D annot.1D.col: Write a 4-column 1D color file. 
                               The first column is the node
                               index followed by r g b values.
                               This color file can be imported 
                               using the 'c' option in SUMA.
                               If no colormap was found in the
                               ANNOTFILE then the file has 2 columns
                               with the second being the annotation
                               value.
         -roi_1D annot.1D.roi: Write a 5-column 1D roi file.
                               The first column is the node
                               index, followed by its index in the
                               colormap, followed by r g b values.
                               This roi file can be imported 
                               using the 'Load' button in SUMA's
                               'Draw ROI' controller.
                               If no colormap was found in the
                               ANNOTFILE then the file has 2 columns
                               with the second being the annotation
                               value. 
         -dset annot.niml.dset: Write the annotation and colormap as a 
                                niml formatted Label Dset. This type of dset
                                gets special treatement in SUMA.
         -cmap_1D annot.1D.cmap: Write a 4-column 1D color map file.
                                 The first column is the color index,
                                 followed by r g b and flag values.
                                 The name of each color is inserted
                                 as a comment because 1D files do not
                                 support text data.
         -show_FScmap: Show the info of the colormap in the ANNOT file.
         -dset DSET: Write out a niml formatted label dataset which is handled
                       in a special way in SUMA. If AFNI_NIML_TEXT_DATA is set
                       to YES, then output is ASCII NIML.
    
    
    
    Compile Date:
       Jan 29 2018
    
