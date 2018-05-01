.. _edu_afni_cbars:

**************************
List of all AFNI colorbars
**************************

This is a list of all AFNI colorbars at present, showing both the
colorbar itself and an image that has a full overlay spectrum
displaying the colorbar.  (The image isn't meant to look meaningful
for every colorbar, it's just an example.)  Some of these apply as
SUMA colorbars, as well.  Order is reverse alphabetical.

You can set your default overlay colorbar in with the
AFNI_COLORSCALE_DEFAULT environment variable in your "~/.afnirc" file.
Otherwise, you can select session-by-session in the AFNI GUI by
right-clicking on the colorbar when you have an overlay displayed.

We might/should add more over time.

|

.. list-table:: Reference images: the dsets viewed as overlay/underlays
   :header-rows: 1
   :widths: 15 35 35

   * - Name
     - anatomical dset
     - ROI dset
   * - Viridis
     - .. image:: media/cbars/Viridis.png
          :height: 3in
          :align: center
     - .. image:: media/cbar_refs/FIG_ref_anat.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbar_refs/FIG_ref_rois.axi.png
          :height: 3in
          :align: center

|

**Colorbars and example images**
==============

.. list-table:: 
   :header-rows: 1
   :widths: 15 10 35 35

   * - Name
     - Cbar
     - opacity=9, brain [1,256]
     - opacity=4, ROIs [1,256]
   * - Viridis
     - .. image:: media/cbars/Viridis.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Viridis.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Viridis.axi.png
          :height: 3in
          :align: center
   * - Spectrum:yellow_to_red
     - .. image:: media/cbars/Spectrum:yellow_to_red.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Spectrum:yellow_to_red.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Spectrum:yellow_to_red.axi.png
          :height: 3in
          :align: center
   * - Spectrum:yellow_to_cyan
     - .. image:: media/cbars/Spectrum:yellow_to_cyan.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Spectrum:yellow_to_cyan.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Spectrum:yellow_to_cyan.axi.png
          :height: 3in
          :align: center
   * - Spectrum:yellow_to_cyan+gap
     - .. image:: media/cbars/Spectrum:yellow_to_cyan+gap.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Spectrum:yellow_to_cyan+gap.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Spectrum:yellow_to_cyan+gap.axi.png
          :height: 3in
          :align: center
   * - Spectrum:red_to_blue
     - .. image:: media/cbars/Spectrum:red_to_blue.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Spectrum:red_to_blue.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Spectrum:red_to_blue.axi.png
          :height: 3in
          :align: center
   * - Spectrum:red_to_blue+gap
     - .. image:: media/cbars/Spectrum:red_to_blue+gap.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Spectrum:red_to_blue+gap.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Spectrum:red_to_blue+gap.axi.png
          :height: 3in
          :align: center
   * - ROI_i64
     - .. image:: media/cbars/ROI_i64.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_ROI_i64.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_ROI_i64.axi.png
          :height: 3in
          :align: center
   * - ROI_i32
     - .. image:: media/cbars/ROI_i32.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_ROI_i32.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_ROI_i32.axi.png
          :height: 3in
          :align: center
   * - ROI_i256
     - .. image:: media/cbars/ROI_i256.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_ROI_i256.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_ROI_i256.axi.png
          :height: 3in
          :align: center
   * - ROI_i128
     - .. image:: media/cbars/ROI_i128.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_ROI_i128.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_ROI_i128.axi.png
          :height: 3in
          :align: center
   * - Reds_and_Blues_w_Green
     - .. image:: media/cbars/Reds_and_Blues_w_Green.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Reds_and_Blues_w_Green.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Reds_and_Blues_w_Green.axi.png
          :height: 3in
          :align: center
   * - Reds_and_Blues_Inv
     - .. image:: media/cbars/Reds_and_Blues_Inv.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Reds_and_Blues_Inv.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Reds_and_Blues_Inv.axi.png
          :height: 3in
          :align: center
   * - Reds_and_Blues
     - .. image:: media/cbars/Reds_and_Blues.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Reds_and_Blues.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Reds_and_Blues.axi.png
          :height: 3in
          :align: center
   * - RedBlueGreen
     - .. image:: media/cbars/RedBlueGreen.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_RedBlueGreen.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_RedBlueGreen.axi.png
          :height: 3in
          :align: center
   * - red_monochrome
     - .. image:: media/cbars/red_monochrome.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_red_monochrome.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_red_monochrome.axi.png
          :height: 3in
          :align: center
   * - Plasma
     - .. image:: media/cbars/Plasma.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Plasma.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Plasma.axi.png
          :height: 3in
          :align: center
   * - Magma
     - .. image:: media/cbars/Magma.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Magma.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Magma.axi.png
          :height: 3in
          :align: center
   * - inverted_gray_circle
     - .. image:: media/cbars/inverted_gray_circle.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_inverted_gray_circle.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_inverted_gray_circle.axi.png
          :height: 3in
          :align: center
   * - inverted_amber_circle
     - .. image:: media/cbars/inverted_amber_circle.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_inverted_amber_circle.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_inverted_amber_circle.axi.png
          :height: 3in
          :align: center
   * - green_monochrome
     - .. image:: media/cbars/green_monochrome.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_green_monochrome.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_green_monochrome.axi.png
          :height: 3in
          :align: center
   * - gray_scale
     - .. image:: media/cbars/gray_scale.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_gray_scale.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_gray_scale.axi.png
          :height: 3in
          :align: center
   * - gray_circle
     - .. image:: media/cbars/gray_circle.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_gray_circle.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_gray_circle.axi.png
          :height: 3in
          :align: center
   * - FreeSurfer_Seg_i255
     - .. image:: media/cbars/FreeSurfer_Seg_i255.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_FreeSurfer_Seg_i255.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_FreeSurfer_Seg_i255.axi.png
          :height: 3in
          :align: center
   * - CytoArch_ROI_i256_gap
     - .. image:: media/cbars/CytoArch_ROI_i256_gap.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_CytoArch_ROI_i256_gap.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_CytoArch_ROI_i256_gap.axi.png
          :height: 3in
          :align: center
   * - CytoArch_ROI_i256
     - .. image:: media/cbars/CytoArch_ROI_i256.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_CytoArch_ROI_i256.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_CytoArch_ROI_i256.axi.png
          :height: 3in
          :align: center
   * - Color_circle_ZSS
     - .. image:: media/cbars/Color_circle_ZSS.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Color_circle_ZSS.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Color_circle_ZSS.axi.png
          :height: 3in
          :align: center
   * - Color_circle_AJJ
     - .. image:: media/cbars/Color_circle_AJJ.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Color_circle_AJJ.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Color_circle_AJJ.axi.png
          :height: 3in
          :align: center
   * - cb_spiral35
     - .. image:: media/cbars/cb_spiral35.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_cb_spiral35.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_cb_spiral35.axi.png
          :height: 3in
          :align: center
   * - cb_spiral
     - .. image:: media/cbars/cb_spiral.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_cb_spiral.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_cb_spiral.axi.png
          :height: 3in
          :align: center
   * - blue_monochrome
     - .. image:: media/cbars/blue_monochrome.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_blue_monochrome.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_blue_monochrome.axi.png
          :height: 3in
          :align: center
   * - amber_redtop_bluebot
     - .. image:: media/cbars/amber_redtop_bluebot.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_amber_redtop_bluebot.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_amber_redtop_bluebot.axi.png
          :height: 3in
          :align: center
   * - amber_monochrome
     - .. image:: media/cbars/amber_monochrome.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_amber_monochrome.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_amber_monochrome.axi.png
          :height: 3in
          :align: center
   * - amber_circle
     - .. image:: media/cbars/amber_circle.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_amber_circle.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_amber_circle.axi.png
          :height: 3in
          :align: center
   * - Add_Edge
     - .. image:: media/cbars/Add_Edge.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS/tt_cbar_Add_Edge.axi.png
          :height: 3in
          :align: center
     - .. image:: media/cbars/IMGS_MULTI/mm_cbar_Add_Edge.axi.png
          :height: 3in
          :align: center
