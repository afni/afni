*********
DriveSuma
*********

.. _DriveSuma:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Usage: A program to drive suma from command line.
           DriveSuma [options] -com COM1 -com COM2 ...
    Mandatory parameters:
    ---------------------
       -com COM: Command to be sent to SUMA.
                 At least one command must be used
                 and various commands can follow in
                 succession.
            COM is the command string and consists
                of at least an action ACT. Some actions
                require additional parameters to follow
                ACT. 
     Actions (ACT) and their parameters:
     -----------------------------------
     o pause [MSG]: Pauses DriveSuma and awaits
                    an 'Enter' to proceed with
                    other commands. 
                    MSG is an optional collection of
                    strings that can be displayed as
                    a prompt to the user. See usage
                    in examples below.
    
     o sleep DUR: Put DriveSuma to sleep for a duration DUR.
                  DUR is the duration, specified with something
                  like 2s (or 2) or 150ms
                  See usage in examples below.
    
     o show_surf: Send surface to SUMA.
         + Mandatory parameters for show_surf action:
            -surf_label S_LABEL: A label (identifier) to assign to the
                               surface
            -i_TYPE SURF: Name of surface file, see surface I/O 
                          options below for details.
         + Optional parameters for show_surf action:
              -surf_state STATE: Name the state of that surface
              -surf_winding WIND: Winding of triangles. Choose 
                                  from ccw or cw (normals on sphere
                                  pointing in). This option affects
                                  the lighting of the surface.
         + Example show_surf: 
            1- Create some surface
            2- Start SUMA
            3- Send new surface to SUMA
            ---------------------------
            CreateIcosahedron -rd 4
            suma -niml &
            echo 'Wait until suma is ready then proceed.'
            DriveSuma -com show_surf -label icoco \
                           -i_fs CreateIco_surf.asc
    
     o node_xyz: Assign new coordinates to surface in SUMA
         + Mandatory parameters for action node_xyz:
            -surf_label S_LABEL: A label to identify the target 
                               surface
            -xyz_1D COORDS.1D: A 1D formatted file containing a new 
                               coordinate for each of the nodes 
                               forming the surface. COORDS.1D must 
                               have three columns.
                               Column selectors can be used here as 
                               they are in AFNI.
            If you do not have the coordinates handy in a 1D file
            and would prefer to get them directly from a surface,
            you can substitute -xyz_1D COORDS.1D with any valid suma 
            surface input option. For example, if you want to send
            the coords of surface surf.gii, you can just use -i surf.gii,
            in lieu of -node_xyz COORDS.1D
         + Example node_xyz (needs surface from 'Example show_surf')
            1- Create some variation on the coords of the surface
            2- Send new coordinates to SUMA
            3- Manipulate the x coordinate now
            4- Send new coordinates again to SUMA
            -------------------------------------
     o get_label: have current label associated with current node printed
     o set_outplug filename: redirect output to file instead of stdout
            ConvertSurface -i_fs CreateIco_surf.asc \
                           -o_1D radcoord radcoord \
                           -radial_to_sphere 100
            DriveSuma -com node_xyz -label icoco \
                           -xyz_1D radcoord.1D.coord'[0,1,2]'
            1deval -a radcoord.1D.coord'[0]' -expr 'sin(a)*100' \
                > xmess.1D ;1dcat xmess.1D radcoord.1D.coord'[1,2]' \
                > somecoord.1D.coord ; rm xmess.1D
            DriveSuma -com node_xyz -label icoco \
                           -xyz_1D somecoord.1D.coord
    
     o viewer_cont: Apply settings to viewer or viewer controller
         + Optional parameters for action viewer_cont:
           (Parameter names reflect GUI labels or key strokes.)
            -autorecord RECORD_PREFIX: Set the autorecord prefix
                            See 'Ctrl+r' in suma's interactive help for
                            details.
                        You can can use this option to make different snapshots
                        go to different directories or filenames. For example:
               ... 
                   -com viewer_cont -autorecord left/Javier.ppm \
                                    -key 'ctrl+left' -key 'ctrl+r' \
                   -com viewer_cont -autorecord right/Javier.ppm \
                                    -key 'ctrl+right' -key 'ctrl+r' \
               ...
            -bkg_col R G B: Set the color of the background to R G B triplet.
                            R G B values must be between 0 and 1
            -load_view VIEW_FILE: Load a previously
                                  saved view file (.vvs).
                                  Same as 'File-->Load View'
            -load_do   DO_FILE: Load a displayable object file
                                For detailed information on DO_FILE's format,
                                see the section under suma's  help (ctrl+h)
                                where the function of Ctrl+Alt+s is detailed.
            -do_draw_mask MASKMODE: Restrict where DO node-based objects are
                                    displayed. MASKMODE is one of:
                              All: No restrictions
                              n3Crosshair: Crosshair node + 3 neighboring layers
                              n2Crosshair: Crosshair node + 2 neighboring layers
                              n1Crosshair: Crosshair node only
                              None: Show nothing.
                          See also Ctrl+p option in SUMA.
            -fixed_do NIML_DO_STRING: Load a fixed coordinate type NIML DO that 
                         is defined by the string NIML_DO_STRING.
                         This is more convenient than specifying
                         a simple DO in a file. For example:
                      DriveSuma -com viewer_cont \
                                  -fixed_do "<T text='Hi' coord='0.5 0.2 0'/>"
                   or the simpler:
                      DriveSuma -com viewer_cont \
                                  -fixed_do "<T text='Up here' p=tlf/>"
                      DriveSuma -com viewer_cont \
                                  -fixed_do "<T text='Down there' p=bcf/>"
    
                         Repeated calls to -fixed_do would replace the previous
                         object with the new one. You could specify multiple DOs
                         by adding a qualifier string to the option -fixed_do.
                         For example:
                      DriveSuma -com viewer_cont \
                              -fixed_do1 "<T text='Tango' coord='0.5 0.2 0'/>"
                      DriveSuma -com viewer_cont \
                              -fixed_do2 "<T text='ognaT' coord='0.2 0.2 0'/>"
                      DriveSuma -com viewer_cont \
                              -fixed_do1 "<T text='-X-' coord='0.5 0.2 0'/>"
                      DriveSuma -com viewer_cont \
                              -fixed_do3 "<Tex target='FRAME' \
                                      filename='funstuff/face_afniman.jpg'/>"
                   or for a more useful example for how you can add a logo on 
                   the bottom right side and way back in the viewer:
                      DriveSuma -com viewer_cont \
                              -fixed_do3 "<I target='FRAME' \
                                   coord   = '1 0 1' \
                                   h_align = 'right'  \
                                   v_align = 'bot'    \
                                   filename='funstuff/face_afniman.jpg'/>"
    
                   For more information about DOs, see NIDO section below 
                   (visible with -help option) and demo script @DO.examples.
    
            -Fixed_do NIML_DO_STRING: Same as -fixed_do, but spits out some 
                         debugging info.
            -mobile_do NIML_DO_STRING: Mobile version of -fixed_do
            -Mobile_do NIML_DO_STRING: Mobile version of -Fixed_do
    
     ---------------------------------------------
     Details for Displayble objects in NIML format (NIDO).
    A NIDO is a collection of displayable objects specified in an ascii file.
    NIDO is a collection of elements with the first element named 'nido_head'
    That first element can contain attributes that describe the entire NIDO 
    and default attributes for the remaining elements.
    The following example shows a nido_head element with possible attributes.
    You do not need to set them all if you don't care to do so. Note that all
     attributes are strings and should be enclosed in single or double quotes.
    
    <nido_head
    coord_type = 'fixed'
    default_color = '1.0 0.2 0.6'
    default_font = 'tr24'
    bond = ''
    />
    
      coord_type attribute:
         Describes the coordinate type of all elements in NIDO.
         * If 'fixed' then that means then the elements do not move with
         suma's surfaces, and the coordinate units are assumed to be in the
         range [0,1] with '0 0 0' being the lower left corner of the screen
         and closest to you. The z coordinate is useful for assigning elements
         to either the background (1) or the foreground (0) of the scene. 
         Elements in the foreground would always be visible, while those in the
         background may be obscured by the rendered surface.
         * If 'mobile' then the elements will move along with your object.
         In that case, the corrdinates you specify are in the same space 
         as your rendered objects. Also, with 'mobile' NIDO, you can specify
         location by specifying a 'node' attribute as illustrated below.
         * Default NIDO coordinate type is: 'mobile'
      default_color atribute:
         3 (R G B) , or 4 (R G B A) color values between [0, 1]
         Elements that do not have their own 'col' attribute set, will use 
         default_color instead. At the moment however, A is not being used.
         Default default_color is '1.0 1.0 1.0'
      default_font attribute:
         String specifying font. All fonts are from the GLUT library. 
         Elements that do not have their own 'font' attribute set, will use 
         default_font instead.
         Default default_font is 'f9'
            Allowed fonts are:
               'f8', or 'font8': Constant width 8 size font
               'f9', or 'font9': Constant width 9 size font
               'tr10', or 'times_roman10'
               'tr24', or 'times_roman24'
               'he10', or 'helvetica10'
               'he12', or 'helvetica12'
               'he18', or 'helvetica18'
      default_SO_label:
         Label identifying surface from which elements get their node based 
         parameters extracted.
         This is mostly useful when the coordinate system's type is 'mobile'
         The default is the currently selected surface in SUMA. If no surface
         is currently selected, some random surface is picked.
      default_node:
         One integer which specifies the index of the node to which all elements
         belong. This value essentially specfies the 'node' attribute of
         individual elements should the 'node' attribute be missing.
         A missing default_node, or a value of -1 indicate there is no default
         node.
      bond:
         If set to 'surface' then NIDO is attached to a particular surface.
         This means that if a surface is not displayed, none of the elements in
         this NIDO would be displayed. Default is 'none'
    
    After 'nido_head' comes a list of elements of various types.
    Text element example:
    <T
    font = 'he12'
    coord = '0.5 0.5 0'
    col = '0.21 0.9 0.61'
    text = 'The Middle
    ----------'
    h_align = 'center'
    v_align = 'center'
    />
      text attribute:
         Put the text you want to display between single or double quotes.
         You can do multi-line text.
      coord attribute:
         XYZ coordinates whose units are determined by nido_head's coord_type.
         See also p attribute
      p attribute:
         A convenience positioning attribute for placing text in fixed screen
         coordinates. If present, it will override coord, h_align, and v_align
         attributes. Its value is two to 3 characters long.
         1st char: t for top, c for center or m for middle, b for bottom
         2nd char: l for left, c for center or m for middle, r for right
         3rd char: f for front, r for rear (optional)
         h_align and v_align are set in a manner that makes sense for these 
         special position flags.
      font attribute:
         Sets the font for the text element. If not specified, font is set per 
         default_font.
      col attribute:
         Sets the color for the text element. If not specified, col is set per 
         default_color.
      h_align:
         Sets the horizontal alignment. Choose from 'l' (default) for left,
        'c' for center, or 'r' for right.
      v_align:
         Sets the horizontal alignment. Choose from 'b' (default) for bottom, 
         'c' for center, or 't' for top.
      node:
         Places the object at a node's location in the surface object defined by
         SO_label attribute. Note that this option overrides coord and might 
         confuse you if NIDO's coord_type is 'fixed'. In such a case, the 
         location would be that of the node, before you moved the surface.
      SO_label:
         Label of Surface Object from which the element gets its node based
         parameters extracted. Default is NIDO's default_SO_label
    Sphere element example (only new attributes are detailed):
    <S
    node = '0'
    col = '0.9 0.1 0.61'
    rad = '35'
    line_width = '1.5'
    style = 'silhouette'
    stacks = '20'
    slices = '20'
    />
      rad attribute:
         Radius of the sphere (default 10).
      rad.ef attribute:
         In lieu of rad, this parameter would
         make the radius be a fraction of the average edge length
         for the surface related to this sphere.
      line_width attribute:
         Width of line (segments) of sphere's mesh
      stacks attribute:
         Number of longitudes (default 10).
      slices attribute:
         Number of lattitudes (default 10).
      style attribute:
         Style of sphere rendering. Choose from:
         fill (default), line, silhouette, or point
         See OpenGL's gluQuadricStyle function for details.
      Other acceptable attributes:
      node, coord, and SO_label
    Image element example (only new attributes are detailed):
    <I
    coord = '0.4 0.5 1'
    filename = 'face_alexmartin2.jpg'
    h_align = 'center'
    v_align = 'bot'
    />
      filename attribute:
         Specifies the filename of the image. If the filename has no path, SUMA
         will search your path for a match before failing.
      Other acceptable attributes:
      h_align, v_align, coord, node, and SO_label.
    
    Texture element example:
    <Tex
    filename = 'face_afniman.jpg'
    target = 'FRAME'
    frame_coords = '
    0.0 0.0 1
    0.0 1.0 1
    1.0 1.0 1
    1.0 0.0 1 '
    mix_mode = 'blend'
    coord_gen = 'sphere'
    />
      filename attribute:
         Specifies the filename of the texture image.
      target attribute:
         Specifies the target of the texture. 
         If target is 'FRAME' then the texture is bound to a quadrilateral whose
         coordinates are defined by the frame_coords attribute. This is useful
         for putting a background image in SUMA for example, when NIDO is of
         a 'fixed' coord_type. Alternately, target can be the label of a 
         surface, or a bunch of surfaces sharing the label string.
         The default is 'ALL_SURFS' which targets all surfaces being displayed
      frame_coords attribute:
         Specify the coordinate of the quadrilateral onto which the texture
         is bound. This is of use when target is set to 'FRAME'. The default
         coordinates are set to:
            0.0 0.0 1
            0.0 1.0 1
            1.0 1.0 1
            1.0 0.0 1 '
         For 'fixed' coord_type, this defaut sets up a rectangle that fills up 
         the suma viewer in the background of the scene. 
         BUG: If you reduce z in 'fixed' coord_type, the texture map be
         positioned closer to the foreground, and should obscure objects behind  
         it. But for some reason, no surfaces get rendered in that case, no 
         matter where they lie relative to the texture frame.
         For 'mobile' coord_type, the texture frame coordinates are in the same
         units as those for the rendered objects. 
         Showing textures in frames is like displaying an image except that:
         - Textures will scale with changes in viewer size for 'fixed' coord_type
         and zoom factor for 'mobile' coord_type. While image size only depends
         on its number of pixels. 
         - Frame orientation is arbitrary for textures. For images, the frame is
         always aligned with the pixel arrays (always facing you). With images, 
         you can only control where its center is located.
      mix_mode attribute:
         Specifies the way texture mixes with node colors.
         Choose from: 'decal', 'blend', 'replace', and 'modulate'. 
         Default is 'replace' when target is 'frame' and 'modulate' for 
         other target values. These parallel OpenGL's GL_DECAL, GL_BLEND, etc.
      coord_gen attribute:
         Specifies how texture coordinate generation is done, when target is not
         'FRAME'. Choose from: 'sphere', 'object', 'eye'. Default is 'sphere'
         For detail, see OpenGL's GL_SPHERE_MAP, GL_OBJECT_LINEAR, etc.
    
      Try the script :ref:`@DO.examples<@DO.examples>` for concrete examples on  
      displayable objects.
    
     ---------------------------------------------
    
            -key KEY_STRING: Act as if the key press KEY_STRING
                             was applied in the viewer.
                             ~ Not all key presses from interactive
                             mode are allowed here.
                             ~ Available keys and their variants are:
                             [, ], comma (or ','), period (or '.'), space,
                             a, b, d, G, j, m, n, p, r, t, z, 
                             up, down, left, right, and F1 to F12.
                             ~ Key variants are specified this way:
                             ctrl+Up or ctrl+alt+Down etc.
                             ~ For help on key actions consult SUMA's
                             GUI help.
                             ~ Using multiple keys in the same command
                             might not result in the serial display of
                             the effect of each key, unless 'd' modifier
                             is used as shown further below. For example,
                             -key right -key right would most likely
                             produce one image rotated twice rather than
                             two images, each turned right once.
               The -key string can be followed by modifiers:
                  For example, -key:r5:s0.2 has two modifiers,
                  r5 and s0.2. All modifiers are separated by ':'.
                  'r' Repeat parameter, so r5 would repeat the 
                      same key 5 times.
                  's' Sleep parameter, so s0.2 would sleep for 0.2
                      seconds between repeated keys.
                  'd' Immediate redisplay flag. That is useful
                      when you are performing a succession of keys and
                      want to ensure each individual one gets displayed
                      and recorded (most likely). Otherwise, successive
                      keys may only display their resultant. 'd' is used
                      automatically with 's' modifier.
                  'p' Pause flag. Requires user intervention to proceed.
                  'v' Value string. The string is passed to the function
                      that processes this key, as if you'd entered that string
                      in the GUI directly. To avoid parsing headaches, you
                      should use quotes with this qualifier. For example, say
                      you want to pass 0.0 0.0 0.0 to the 'ctrl+j' key press.
                      At the shell you would enter:
                        DriveSuma -com viewer_cont '-key:v"0.8 0 10.3"' ctrl+j
                      In another example, say you want to jump to node 54 on the
                      right hemisphere (hence the 'R' in '54R'), then you would
                      execute:
                        DriveSuma -com viewer_cont '-key:v54R' j
            -viewer VIEWER: Specify which viewer should be acted 
                            upon. Default is viewer 'A'. Viewers
                            must be created first (ctrl+n) before
                            they can be acted upon.
                            You can also refer to viewers with integers
                            0 for A, 1 for B, etc.
                            For -viewer to take effect it must be in the
                            same -com viewer_cont ... commands. For example:
                   ... -com viewer_cont -viewer B -viewer_size 600 900 ...
            -viewer_width or (-width) WIDTH: Set the width in pixels of
                                         the current viewer.
            -viewer_height or (-height) HEIGHT: Set the height in pixels of
                                         the current viewer.
            -viewer_size WIDTH HEIGHT : Convenient combo of -viewer_width 
                                        and -viewer_height
            -viewer_position X Y: Set position on the screen
            -controller_position X Y: Set position of the object (surface)
                                      controller on the screen
            -inout_notify y/n: Turn on or off function call tracing
            -N_foreg_smooth n: Number of foreground smoothing iterations
                               Same as suma's interactive '8' key or what
                               you'd set with env: SUMA_NumForeSmoothing
            -N_final_smooth n: Number of final color smoothing iterations
                               Same as suma's interactive '*' key or what
                               you'd set with env: SUMA_NumForeSmoothing
         + Example viewer_cont (assumes all previous examples have
           been executed and suma is still running).
            - a series of commands that should be obvious.
           -------------------------------------
           DriveSuma -com  viewer_cont -key R -key ctrl+right
           DriveSuma -com  viewer_cont -key:r3:s0.3 up  \
                           -key:r2:p left -key:r5:d right \
                           -key:r3 z   -key:r5 left -key F6
           DriveSuma -com  viewer_cont -key m -key down \
                     -com  sleep 2s -com viewer_cont -key m \
                           -key:r4 Z   -key ctrl+right
           DriveSuma -com  viewer_cont -key m -key right \
                     -com  pause press enter to stop this misery \
                     -com  viewer_cont -key m 
    
     o recorder_cont: Apply commands to recorder window
         + Optional parameters for action recorder_cont:
           -anim_dup DUP: Save DUP copies of each frame into movie
                          This has the effect of slowing movies down
                          at the expense of file size, of course.
                          DUP's default is set by the value of AFNI_ANIM_DUP
                          environment variable. 
                          To set DUP back to its default value,
                          use -anim_dup 0.
           -save_as PREFIX.EXT: Save image(s) in recorder
                                 in the format determined by
                                 extension EXT.
                                 Allowed extensions are:
                                 agif or gif: Animated GIF (movie)
                                 mpeg or mpg: MPEG (movie)
                                 jpeg or jpg: JPEG (stills)
                                 png: PNG (stills)
           -save_index IND: Save one image indexed IND (start at 0)
           -save_range FROM TO: Save images from FROM to TO 
           -save_last: Save last image (default for still formats)
           -save_last_n N: Save last N images
           -save_all: Save all images (default for movie formats)
           -cwd ABSPATH: Set ABSPATH as SUMA's working directory. 
                         This path is used for storing output files
                         or loading dsets.
         + Example recorder_cont (assumes there is a recorder window)
           currently open from SUMA.
           -------------------------------------
           DriveSuma -com  recorder_cont -save_as allanimgif.agif \
                     -com  recorder_cont -save_as lastone.jpg -save_last \
                     -com  recorder_cont -save_as three.jpg -save_index 3 \
                     -com  recorder_cont -save_as some.png -save_range 3 6
    
     o object_cont: Apply settings to object controller.
     o surf_cont: Apply settings to surface controller.
         Note that for most cases, the use of object_cont and surf_cont is
         interchangeable.
         + Optional parameters for action surf_cont:
           (Parameter names reflect GUI labels.)
           -surf_label S_LABEL: A label to identify the target surface
           -load_dset DSET: Load a dataset
               ! NOTE: When using -load_dset you can follow it
                       with -surf_label in order to attach
                       the dataset to a particular target surface.
           -view_surf y/n: Show or hide surface S_LABEL
           -RenderMode V/F/L/P/H: Set the render mode for surface S_LABEL.
           -TransMode V/0/../16: Set the transparency mode for surface S_LABEL.
           -load_col COL: Load a colorfile named COL.
                          Similar to what one loads under
                          SUMA-->ctrl+s-->Load Col
                          COL contains 4 columns, of
                          the following format:
                          n r g b
                          where n is the node index and 
                          r g b are thre flooat values between 0 and 1
                          specifying the color of each node.
           -view_surf_cont y/n: View surface controller
           -view_object_cont y/n: View object controller
           -masks: Equivalent of pressing 'Masks' in tract controller
           -2xmasks: Equivalent of pressing 'Masks' twice in tract controller
           -delete_all_masks: Well, delete all the masks.
           -load_masks: Equivalent of pressing 'Load Masks' in masks controller
           -save_masks: Equivalent of pressing 'Save Masks' in masks controller
           -switch_surf S_LABEL: switch state to that of surface 
                               labeled S_LABEL and make that surface 
                               be in focus.
           -switch_dset DSET: switch dataset to DSET
           -view_dset y/n: Set view toggle button of DSET
           -1_only y/n: Set 1_only toggle button of DSET
           -switch_cmap CMAP: switch colormap to CMAP
           -switch_cmode CMODE: switch color mapping mode to CMODE
           -load_cmap CMAP.1D.cmap: load and switch colormap in 
                                    file CMAP.1D.cmap
           -I_sb ISB: Switch intensity to ISBth column (sub-brick)
           -I_range IR0 IR1: set intensity range from IR0 to IR1.
                             If only one number is given, the range
                             is symmetric from -|IR0| to |IR0|.
           -shw_0 y/n      or 
           -show_0 y/n: Set shw 0 toggle button of DSET.
           -Dsp MODE: Set the viewing mode of the current DSET.
                      MODE is one of XXX, Con, Col, or 'C&C' 
                          (single quotes necessary for 'C&C' MODE).
                      This is equivalent to setting the 'Dsp' menu button
                      in the surface controller. The option is applied
                      to the current DSET on the selected surface.
           -T_sb TSB: Switch threshold to TSBth column (sub-brick)
                      Set TSB to -1 to turn off thresholding.
           -T_val THR: Set threshold to THR
           -B_sb BSB: Switch brightness modulation to BSBth column (sub-brick)
           -B_range BR0 BR1: set brightness clamping range from BR0 to BR1.
                             If only one number is given, the range
                             is symmetric from -|BR0| to |BR0|.
           -B_scale BS0 BS1: Modulate brightness by BS0 factor for BR0 or lower
                             by BS1 factor for BR1 or higher, and linearly 
                             interpolate scaling for BR0 < values < BR1
           -Dim DIM: Set the dimming factor.
           -Opa OPA: Set the opacity factor.
           -Clst RAD AREA: Set the clustering parameters
           -UseClst y/n: Turn on/off clustering
           -setSUMAenv "'ENVname=ENVvalue'": Set an ENV in SUMA. Note that
                          most SUMA env need to be set at SUMA's launch time. 
                          Setting the env from DriveSuma may not achieve what 
                          you want, so consider using suma's -setenv instead.
           -write_surf_cont_help FILE.txt: Write help output for surface 
                          controller uses into file FILE.txt (in append mode)
                          Make sure the surface controller is open before you
                          use this command.
           -write_surf_cont_sphinx_help FILE.rst: Same as -write_surf_cont_help,
                          but write SPHINX formatted RST file.
           -snap_surf_cont_widgets FROOT: Takes snapshots of various widget 
                                          groupings and save them under FROOT*
           Also, in the same vein as -write_surf_cont_help, 
           -write_surf_cont_sphinx_help, and -snap_surf_cont_widgets you have:
           -write_vol_cont_help
           -write_vol_cont_sphinx_help 
           -snap_vol_cont_widgets
           -write_tract_cont_help
           -write_tract_cont_sphinx_help 
           -snap_tract_cont_widgets
           -write_mask_cont_help
           -write_mask_cont_sphinx_help 
           -snap_mask_cont_widgets
           -write_graph_cont_help
           -write_graph_cont_sphinx_help 
           -snap_graph_cont_widgets
           -write_roi_cont_help
           -write_roi_cont_sphinx_help 
           -snap_roi_cont_widgets
           -write_suma_cont_help
           -write_suma_cont_sphinx_help 
           -snap_suma_cont_widgets
           -write_mouse_keyb_help FILE.txt: Write help output for mouse and 
                          keyboard shortcuts.
           -write_mouse_keyb_sphinx_help FILE.rst: Same as -write_mouse_keyb_help
                          , but write SPHINX formatted RST file.
           -write_mouse_cmap_keyb_help FILE.txt: Write help output for mouse and 
                          keyboard shortcuts.
           -write_mouse_cmap_keyb_sphinx_help FILE.rst: Same
                          as -write_mouse_cmap_keyb_help, but write SPHINX 
                          formatted RST file.
    
         + Example surf_cont (assumes all previous examples have
           been executed and suma is still running).
           - Obvious chicaneries to follow:
           --------------------------------
           echo 1 0 0 > bbr.1D.cmap; echo 1 1 1 >> bbr.1D.cmap; \
           echo 0 0  1 >> bbr.1D.cmap
           IsoSurface -shape 4 128 -o_ply blooby.ply
           quickspec -spec blooby.spec -tn ply blooby.ply
           SurfaceMetrics -curv -spec blooby.spec \
                          -surf_A blooby -prefix blooby      
           DriveSuma -com show_surf -surf_label blooby \
                          -i_ply blooby.ply -surf_winding cw \
                          -surf_state la_blooby
           DriveSuma -com surf_cont -load_dset blooby.curv.1D.dset \
                          -surf_label blooby -view_surf_cont y
           DriveSuma -com surf_cont -I_sb 7 -T_sb 8 -T_val 0.0
           DriveSuma -com surf_cont -I_range 0.05 -T_sb -1
           DriveSuma -com surf_cont -I_sb 8 -I_range -0.1 0.1 \
                          -T_val 0.02 -Dim 0.4
           DriveSuma -com surf_cont -B_sb 7 -B_range 0.5 -B_scale 0.1 0.9
           DriveSuma -com surf_cont -switch_dset Convexity -1_only y
           DriveSuma -com surf_cont -switch_cmap roi64 -1_only n
           DriveSuma -com surf_cont -switch_cmode Dir 
           DriveSuma -com surf_cont -view_dset n
           DriveSuma -com surf_cont -switch_dset blooby.curv.1D.dset \
                          -view_surf_cont n -I_range -0.05 0.14
           DriveSuma -com surf_cont -load_cmap bbr.1D.cmap
    
         + Example for loading masks onto tracts
           -------------------------------------
           #This uses one of the tract files output by FATCAT's demo.
           #and some tracts mask called triplets.niml.do
    
           suma -tract DTI/o.NETS_OR_000.niml.tract &
           DriveSuma -com object_cont -view_object_cont y          \
                     -com object_cont -2xmasks                     \
                     -com object_cont -delete_all_masks            \
                     -com object_cont -load_masks triplets.niml.mo   
    
     o kill_suma: Close suma and quit.
    
    Advice:
    -------
       If you get a colormap in your recorded image, it is
       because the last thing you drew was the surface controller
       which has an openGL surface for a colormap. In such cases,
       Force a redisplay of the viewer with something like:
          -key:r2:d m 
                      where the m key is pressed twice (nothing)
                      changes in the setup but the surface is 
                      redisplayed nonetheless because of the 'd'
                      key option.
       Crashes: It is possible for SUMA to crash under certain combinations
                of commands that involve opening X windows followed by
                some command. For example, suma might crash with:
             DriveSuma   -com viewer_cont  -viewer_size 600 600 -key 'ctrl+n'
                Splitting such a command into two DriveSuma instances gets
                around the problem:
             DriveSuma   -com viewer_cont  -viewer_size 600 600 
             DriveSuma   -com viewer_cont  -key 'ctrl+n'
    
    Options:
    --------
       -echo_edu: Echos the entire command line (without -echo_edu)
                  for edification purposes
       -echo_nel_stdout: Spit out the NIML object being sent to SUMA for 
       -echo_nel_stderr: edification purposes. These two options are meant
                         to help motivate the example in HalloSuma.
                         You need to have SUMA up and listening for this option
                         to take effect.
                Example: DriveSuma -echo_nel_stdout -com viewer_cont '-key:v28' j
       -echo_nel FILE: Write the elements to FILE.
                       You can also use stdout or stderr for FILE.
       -examples: Show all the sample commands and exit
       -help: All the help, in detail.
           ** NOTE: You should also take a look at scripts @DO.examples and 
              @DriveSuma for examples. Suma's interactive help (ctrl+h) for
              the kinds of controls you can have with -key option.
       -h: -help, with slightly less detail
       -help_nido: Show the help for NIML Displayable Objects and exit.
                   Same as suma -help_nido
       -C_demo: execute a preset number of commands
                which are meant to illustrate how one
                can communicate with SUMA from one's 
                own C code. Naturally, you'll need to
                look at the source code file SUMA_DriveSuma.c
          Example:
          suma -niml &
          DriveSuma -C_demo
    
     Specifying input surfaces using -i or -i_TYPE options: 
        -i_TYPE inSurf specifies the input surface,
                TYPE is one of the following:
           fs: FreeSurfer surface. 
               If surface name has .asc it is assumed to be
               in ASCII format. Otherwise it is assumed to be
               in BINARY_BE (Big Endian) format.
               Patches in Binary format cannot be read at the moment.
           sf: SureFit surface. 
               You must specify the .coord followed by the .topo file.
           vec (or 1D): Simple ascii matrix format. 
                You must specify the coord (NodeList) file followed by 
                the topo (FaceSetList) file.
                coord contains 3 floats per line, representing 
                X Y Z vertex coordinates.
                topo contains 3 ints per line, representing 
                v1 v2 v3 triangle vertices.
           ply: PLY format, ascii or binary.
                Only vertex and triangulation info is preserved.
           stl: STL format, ascii or binary.
                This format of no use for much of the surface-based
                analyses. Objects are defined as a soup of triangles
                with no information about which edges they share. STL is only
                useful for taking surface models to some 3D printing 
                software.
           mni: MNI .obj format, ascii only.
                Only vertex, triangulation, and node normals info is preserved.
           byu: BYU format, ascii.
                Polygons with more than 3 edges are turned into
                triangles.
           bv: BrainVoyager format. 
               Only vertex and triangulation info is preserved.
           dx: OpenDX ascii mesh format.
               Only vertex and triangulation info is preserved.
               Requires presence of 3 objects, the one of class 
               'field' should contain 2 components 'positions'
               and 'connections' that point to the two objects
               containing node coordinates and topology, respectively.
           gii: GIFTI XML surface format.
           obj: OBJ file format for triangular meshes only. The following
                primitives are preserved: v (vertices),  (faces, triangles
                only), and p (points)
     Note that if the surface filename has the proper extension, 
     it is enough to use the -i option and let the programs guess
     the type from the extension.
    
     You can also specify multiple surfaces after -i option. This makes
     it possible to use wildcards on the command line for reading in a bunch
     of surfaces at once.
    
         -onestate: Make all -i_* surfaces have the same state, i.e.
                    they all appear at the same time in the viewer.
                    By default, each -i_* surface has its own state. 
                    For -onestate to take effect, it must precede all -i
                    options with on the command line. 
         -anatomical: Label all -i surfaces as anatomically correct.
                    Again, this option should precede the -i_* options.
    
     More variants for option -i:
    -----------------------------
     You can also load standard-mesh spheres that are formed in memory
     with the following notation
         -i ldNUM:  Where NUM is the parameter controlling
                    the mesh density exactly as the parameter -ld linDepth
                    does in CreateIcosahedron. For example: 
                        suma -i ld60
                    create on the fly a surface that is identical to the
                    one produced by: CreateIcosahedron -ld 60 -tosphere
         -i rdNUM: Same as -i ldNUM but with NUM specifying the equivalent
                   of parameter -rd recDepth in CreateIcosahedron.
    
     To keep the option confusing enough, you can also use -i to load
     template surfaces. For example:
               suma -i lh:MNI_N27:ld60:smoothwm 
     will load the left hemisphere smoothwm surface for template MNI_N27 
     at standard mesh density ld60.
     The string following -i is formatted thusly:
         HEMI:TEMPLATE:DENSITY:SURF where:
         HEMI specifies a hemisphere. Choose from 'l', 'r', 'lh' or 'rh'.
              You must specify a hemisphere with option -i because it is 
              supposed to load one surface at a time. 
              You can load multiple surfaces with -spec which also supports 
              these features.
         TEMPLATE: Specify the template name. For now, choose from MNI_N27 if
                   you want to use the FreeSurfer reconstructed surfaces from
                   the MNI_N27 volume, or TT_N27
                   Those templates must be installed under this directory:
                     /Users/discoraj/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /Users/discoraj/.afni/data/
         DENSITY: Use if you want to load standard-mesh versions of the template
                  surfaces. Note that only ld20, ld60, ld120, and ld141 are in
                  the current distributed templates. You can create other 
                  densities if you wish with MapIcosahedron, but follow the
                  same naming convention to enable SUMA to find them.
         SURF: Which surface do you want. The string matching is partial, as long
               as the match is unique. 
               So for example something like: suma -i l:MNI_N27:ld60:smooth
               is more than enough to get you the ld60 MNI_N27 left hemisphere
               smoothwm surface.
         The order in which you specify HEMI, TEMPLATE, DENSITY, and SURF, does
         not matter.
         For template surfaces, the -sv option is provided automatically, so you
         can have SUMA talking to AFNI with something like:
                 suma -i l:MNI_N27:ld60:smooth &
                 afni -niml /Users/discoraj/.afni/data/suma_MNI_N27 
    
     Specifying surfaces using -t* options: 
       -tn TYPE NAME: specify surface type and name.
                      See below for help on the parameters.
       -tsn TYPE STATE NAME: specify surface type state and name.
            TYPE: Choose from the following (case sensitive):
               1D: 1D format
               FS: FreeSurfer ascii format
               PLY: ply format
               MNI: MNI obj ascii format
               BYU: byu format
               SF: Caret/SureFit format
               BV: BrainVoyager format
               GII: GIFTI format
            NAME: Name of surface file. 
               For SF and 1D formats, NAME is composed of two names
               the coord file followed by the topo file
            STATE: State of the surface.
               Default is S1, S2.... for each surface.
     Specifying a surface specification (spec) file:
        -spec SPEC: specify the name of the SPEC file.
         As with option -i, you can load template
         spec files with symbolic notation trickery as in:
                        suma -spec MNI_N27 
         which will load the all the surfaces from template MNI_N27
         at the original FreeSurfer mesh density.
      The string following -spec is formatted in the following manner:
         HEMI:TEMPLATE:DENSITY where:
         HEMI specifies a hemisphere. Choose from 'l', 'r', 'lh', 'rh', 'lr', or
              'both' which is the default if you do not specify a hemisphere.
         TEMPLATE: Specify the template name. For now, choose from MNI_N27 if
                   you want surfaces from the MNI_N27 volume, or TT_N27
                   for the Talairach version.
                   Those templates must be installed under this directory:
                     /Users/discoraj/.afni/data/
                   If you have no surface templates there, download
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz
                   and/or
                     http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz
                   and untar them under directory /Users/discoraj/.afni/data/
         DENSITY: Use if you want to load standard-mesh versions of the template
                  surfaces. Note that only ld20, ld60, ld120, and ld141 are in
                  the current distributed templates. You can create other 
                  densities if you wish with MapIcosahedron, but follow the
                  same naming convention to enable SUMA to find them.
                  This parameter is optional.
         The order in which you specify HEMI, TEMPLATE, and DENSITY, does
         not matter.
         For template surfaces, the -sv option is provided automatically, so you
         can have SUMA talking to AFNI with something like:
                 suma -spec MNI_N27:ld60 &
                 afni -niml /Users/discoraj/.afni/data/suma_MNI_N27 
    
       [-novolreg]: Ignore any Rotate, Volreg, Tagalign, 
                    or WarpDrive transformations present in 
                    the Surface Volume.
       [-noxform]: Same as -novolreg
       [-setenv "'ENVname=ENVvalue'"]: Set environment variable ENVname
                    to be ENVvalue. Quotes are necessary.
                 Example: suma -setenv "'SUMA_BackgroundColor = 1 0 1'"
                    See also options -update_env, -environment, etc
                    in the output of 'suma -help'
      Common Debugging Options:
       [-trace]: Turns on In/Out debug and Memory tracing.
                 For speeding up the tracing log, I recommend 
                 you redirect stdout to a file when using this option.
                 For example, if you were running suma you would use:
                 suma -spec lh.spec -sv ... > TraceFile
                 This option replaces the old -iodbg and -memdbg.
       [-TRACE]: Turns on extreme tracing.
       [-nomall]: Turn off memory tracing.
       [-yesmall]: Turn on memory tracing (default).
      NOTE: For programs that output results to stdout
        (that is to your shell/screen), the debugging info
        might get mixed up with your results.
    
    
    Global Options (available to all AFNI/SUMA programs)
      -h: Mini help, at time, same as -help in many cases.
      -help: The entire help output
      -HELP: Extreme help, same as -help in majority of cases.
      -h_view: Open help in text editor. AFNI will try to find a GUI editor
      -hview : on your machine. You can control which it should use by
               setting environment variable AFNI_GUI_EDITOR.
      -h_web: Open help in web browser. AFNI will try to find a browser.
      -hweb : on your machine. You can control which it should use by
              setting environment variable AFNI_GUI_EDITOR. 
      -h_find WORD: Look for lines in this programs's -help output that match
                    (approximately) WORD.
      -h_raw: Help string unedited
      -h_spx: Help string in sphinx loveliness, but do not try to autoformat
      -h_aspx: Help string in sphinx with autoformatting of options, etc.
      -all_opts: Try to identify all options for the program from the
                 output of its -help option. Some options might be missed
                 and others misidentified. Use this output for hints only.
      
    
