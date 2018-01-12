.. contents:: 
    :depth: 4 

***
FD2
***

.. code-block:: none

    
     Functional Display (32x32, 64x64, 128x128, 256x256) in X11 window.
     EPI images in 256x256 Signa format are accepted too.
     It displays EPI time or frequency course window.
    
     Usage: FD2 [options] image1, [image2, ..., image10000]
    
     Where options are:
        -d display       - X11 display
        -geom geometry   - initial geometry
        -nc #_of_colors  - initial number of colors [2-200] (def 64)
        -sp #_of_degree  - range of color spectrum [0-360] degree (def 240)
        -gam gamma       - gamma correction (1 for no correction)
        -num #_of_images - # of images in time course [2-10000].
        -im1 image_#     - first image in time course. Previous images will be 
                           filled with this one for proper timing of others.
        -ideal ref_file  - use ref_file for fim-like calculations
        -pcthresh #      - use # as threshold for correlation
        -extra           - files after this are not used in time course
                           (used instead of -num option)
        -fim_colors L thr1 pos1 neg2 ... thrL posL negL
                         - set up L thresholds and colors for FIM overlay
        -gsize x y       - set graph window size to x by y pixels
        -fmag val        - magnify scale of FFT by val
        -grid val        - initial grid separation 
        -phase           - image has negative values (for FFT)
        -cf              - center image to the frame
        -swap            - byte-swap data (default is no)
                           *** this is a new default!
    
     Events:
    
      Program quit      : <q> or <Q>
      Change to colors  : <C>
      Change to B & W   : <B>
      Swap colors       : <s>
      Restore colors    : Button_3 at image center 
      Squeeze colors    : #2 or #3 button - right side of image
      Expand  colors    :                   left  side of image
      Circ. color bar   : #2 or #3 button at the color bar
      Color saturation  : #2 or #3 button - the top or bottom
      Exact image number: press <I>, enter_number, <CR>
      First image       : 1
      Last image        : l
      Next     image    : >
      Previous image    : <
                          dragging red pointer works too
      Scale Plot up         : +
      Scale Plot down       : -
      Increase Grid Spacing : G
      Decrease Grid Spacing : g
      Toggle Grid and Colors: r
      Toggle Frame colors   : R
      Toggle time or box avr: t
      Increase matrix size  : M
      Decrease matrix size  : m
      Increase matrix y size: V
      Decrease matrix y size: v
      Exact matrix size     : N #of_size <CR> (1 to 25 only)
      Save minigraph in ASCII file   : press <p>
        [with xxx_yyy.suffix filename] press <w>
      Save current image to a file   : press <S>
      Save averaged image (not norm) : press <X>
      Position frame in the image    : press Button_1 in the image area,
                                        drag cursor, and release button.
      Center frame on desired pixel  : press Button_1 over desired minigraph.
      Rotate image 90 deg. clockwise : press Button_3 in [Rot] window.
                    counterclockwise : press Button_1 in [Rot] window.
      Change to differential display : press [Diff] window. Set first and
                                       last image for averaged reference.
      Average of set of images       : press [AvIm] (can be used in Diff mode).
      Compute FIM overlay            : press [FIM], choose ref file,threshold,
                                       then press [GO]
    
      Last image in time course      : L
      Toggle autoscale of images     : A
      Hide FIM overlay               : H
      Hide frame in image            : h
      Toggle overlay checkerboard    : O
      Read image into program        : F (for file)
      Remove image from program      : K (for kill)
      Move to image 1..9             : 1,2,...9
      Toggle common graph baselines  : b
      Toggle baseline to zero        : x
    
      Add/[subtract] 3600 from pixel : D / [d]
      In FT edit mode: 
                    increase value   : Arrow Up
                    decrease value   : Arrow Down
              Shift or Control Arrow : larger changes 
                    undo last change : u
                    undo all changes : U
    
