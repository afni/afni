.. _edu_startup_tips:

************************
List of all startup tips
************************

This is a list of *possibly* useful tips that the AFNI gurus have
thought up, which may inform you, Dear User, of cool features within
AFNI. By default, one gets displayed every time you start up the AFNI
GUI (unless you have set an environment variable in "~/.afnirc" to
turn off this delightful feature).  Hence the name of this list.

We might/should add more over time.

|

**Tip 1**

    .. code-block:: none

        If you set environment variable AFNI_GLOBAL_SESSION to the name
        of a directory with datasets, then those datasets will be visible
        in the UnderLay and OverLay choosers. For example, copy the MNI
        template MNI152_2009_template.nii.gz to this directory, and then
        you'll always be able to use it as an underlay dataset.

**Tip 2**

    .. code-block:: none

        If the aspect ratio (width/height) of an image viewer window looks
        bad, you can fix it by typing the 'a' key into the image, or by
        clicking the left mouse button in the intensity grayscale bar at
        the right of the image.

**Tip 3**

    .. code-block:: none

        The right-click popup menu on the intensity grayscale bar to the right
        of an image viewer has several useful controls, including:
        * choosing the numerical Display Range for the underlay
        * drawing a coordinate Label over the image
        * applying an Automask to the overlay (e.g.,hide the non-brain stuff)
        * choosing the color for Zero values in the overlay (e.g., black or white)

**Tip 4**

    .. code-block:: none

        Looking at venography or arteriography datasets? The image viewer 'Disp'
        control panel 'Project' menu lets you look at projections of the underlay
        dataset through a slab of slices, including Minimum and Maximum. The
        slab half-thickness is given by the 'Slab +-' control below 'Project'.

**Tip 5**

    .. code-block:: none

        If you crop an image, you can move the crop window around by pressing
        the Shift key plus one of the keyboard arrow keys.

**Tip 6**

    .. code-block:: none

        The 'Disp' button in an image viewer pops up a control panel with many
        useful buttons, including:
        * Project   = combine multiple slices into one underlay
        * Tran 0D   = transform values of the underlay pixelwise
        * Tran 2D   = transform underlay image globally (e.g., blurring)
        * Rowgraphs = graph the underlay numerical values in 1-9 pixel rows

**Tip 7**

    .. code-block:: none

        The 'BHelp' button lets you click on some other button in the GUI
        to get more information about what that button does.

**Tip 8**

    .. code-block:: none

        The right-click popup menu on the coordinate display in the AFNI
        controller has several useful functions, including:
        * controlling the coordinate display order
        * jumping to x,y,z (mm) or i,j,k (voxel index) coordinates

**Tip 9**

    .. code-block:: none

        The right-click popup menu on the label above the threshold slider
        lets you control the threshold in various ways:
        * pin the Threshold sub-brick to equal the OLay or OLay+1 sub-brick
        (OLay+1 is very useful for Coef/t-statistic sub-brick pairs)
        * set the threshold slider to have a given voxelwise p-value
        (based on the statistical properties of the current Thr sub-brick)
        * control Alpha fading for colorization of sub-threshold voxels
        * see only Positive or Negative values, with respect to the threshold
        (which will affect the p-value, as being 1- or 2-sided)

**Tip 10**

    .. code-block:: none

        The right-click popup menu on the label above the color overlay bar
        lets you control colorization from the OLay sub-brick in several ways:
        * you can jump crosshairs to the largest OLay value above threshold
        * you can write the current color palette out to a file for editing,
        or to an image for use in a figure
        * you can apply pixelwise or 2D spatial transformations to the
        OLay values before they are turned into colors

**Tip 11**

    .. code-block:: none

        You can run InstaCorr on several subjects at the same time, using
        multiple AFNI controllers opened with the 'New' button.

**Tip 12**

    .. code-block:: none

        The 'New' button (lower left corner of AFNI controller) lets you open
        another AFNI controller. The UnderLay and OverLay datasets will be
        listed in the controller window title bar.

**Tip 13**

    .. code-block:: none

        Image viewer keypress: q = close window (works in graph viewer too)

**Tip 14**

    .. code-block:: none

        Image viewer keypress: S = save image (works in graph viewer too)

**Tip 15**

    .. code-block:: none

        Image viewer keypress: o = turn OLay color on or off

**Tip 16**

    .. code-block:: none

        Image viewer keypress: u = make underlay image from the OLay dataset
        press u again to make underlay image from ULay

**Tip 17**

    .. code-block:: none

        Image viewer keypress: 4 or 5 or 6 = meld ULay and OLay images
        (controlled by a slider on top of the image)
        * 4 = OLay on left side, ULay on right side, slider moves boundary
        * 5 = OLay on top side, ULay on bottom side, slider moves boundary
        * 6 = ULay and OLay intensity mixed, slider controls mixing fraction
        (slider to left = more ULay; to right = more OLay)

**Tip 18**

    .. code-block:: none

        Image viewer keypress: z/Z = zoom out or in
        Zooming is limited to factors of 1-4

**Tip 19**

    .. code-block:: none

        Graph viewer keypress: < or > = move focus time down or up 1 TR

**Tip 20**

    .. code-block:: none

        Graph viewer keypress: 1 or L = move focus time to first or last TR

**Tip 21**

    .. code-block:: none

        Graph viewer keypress: v/V = video the focus time up or down
        This is how you can make a video of subject
        head movement, by looking at the image viewers
        while the graph viewer is doing 'v'.

**Tip 22**

    .. code-block:: none

        Graph viewer keypress: m/M = decrease/increase matrix size of graphs
        Also can do this from the 'Opt->Matrix' menu.

**Tip 23**

    .. code-block:: none

        Graph viewer keypress: w = write time series from central sub-graph to a file
        Set prefix for file from the 'Opt' menu.

**Tip 24**

    .. code-block:: none

        The image viewer 'Mont' button (along bottom) will let you make a montage
        from multiple slices, which can be Saved to a .jpg or .png file.
        NOTE: you might want to turn the crosshairs off from the 'Xhairs' menu
        in the main AFNI controller.

**Tip 25**

    .. code-block:: none

        If the image editing program 'gimp' is in your path, then the image viewer
        Save control panel will include an option to start gimp on your image, so
        you can further edit it immediately. See https://www.gimp.org/

**Tip 26**

    .. code-block:: none

        The graph viewer 'Tran 1D' function Dataset#N (from the 'Opt' main menu)
        lets you plot extra dataset time series on top of the UnderLay dataset's
        time series graphs.

**Tip 27**

    .. code-block:: none

        You can change the way the graph viewer shows its plots by using the
        'Colors, Etc.' sub-menu from the main 'Opt' menu (lower right corner):
        * Boxes     = color of the boxes around each sub-graph
        * BackG     = color of background
        * Grid      = color of vertical grid lines
        * Text      = color of text
        * Data      = color of data
        graph points only, or points+lines, or boxes
        * Graph Gap = how many pixels spacing between sub-graphs
        * Thick     = how many pixels wide for 'Thick' lines
        Most of these settings can also be selected by AFNI environment
        settings in your .afnirc file; with some work, you can setup the
        graph viewer to look the way you want it to be permanently.

**Tip 28**

    .. code-block:: none

        The graph viewer 'Opt->Detrend' menu item lets you choose a polynomial degree
        for detrending the graph data. This can help you visualize the features of the
        data you want to see without be distracted by long term trends up or down.
        -1 = no detrending ; 0 = remove mean ; 1 = remove linear trend ; et cetera

**Tip 29**

    .. code-block:: none

        The graph viewer 'Opt->Tran 1D->Despike' function will despike the time series
        graphs, which can be useful when you trying to figure out what's going on
        in a dataset.

**Tip 30**

    .. code-block:: none

        Right-clicking in a graph viewer plot will popup a window with some statistics
        about the data being shown.

**Tip 31**

    .. code-block:: none

        The README.environment text file lists many Unix 'environment' variables that
        can be used to control the way AFNI appears and operates.

**Tip 32**

    .. code-block:: none

        The Define Datamode control panel lets you control how the OLay dataset is
        resampled to fit the ULay dataset (that defines the basis for the pixel grid
        on which the images are displayed). The options are:
        * NN = Nearest Neighbor  * Li = Linear
        * Cu = Cubic             * Bk = Blocky (between NN and Li)
        When the OverLay is at a coarser resolution than the UnderLay (common in FMRI),
        Li will produce 'nicer' looking maps, but NN will be more 'honest' looking.

**Tip 33**

    .. code-block:: none

        'Define Datamode->Lock' lets you turn the xyz coordinate lock between AFNI
        controllers off, if you want. Or, you can turn on 'Time Lock', so that the
        TR index is locked between controllers, as well as the crosshair location.

**Tip 34**

    .. code-block:: none

        Normally, the grid size of the pixel image created for display in an
        AFNI image viewer is take from the grid size of the Underlay dataset.
        But you can change that using the 'Datamode' control panel, by choosing
        'Warp ULay on Demand', then setting the grid resampling mode below.
        Sometimes using this to make the display grid more fine is useful for
        creating nicer looking functional images, especially when 'Alpha' is
        turned on (to outline above-threshold clusters and at the same time
        show below-threshold in faded-out translucent colors).

**Tip 35**

    .. code-block:: none

        Normally, voxels whose threshold value is below the slider setting will
        not be colorized. 'Alpha' fading allows them to get a faded color, while
        the above-threshold voxel clusters will get a black outline drawn around
        them. Alpha can be turned on from the right-click popup menu above the
        threshold slider, or via the AFNI_FUNC_ALPHA environment variable in
        your .afnirc file.

**Tip 36**

    .. code-block:: none

        The InstaCalc function (from the InstaCorr drop-down menu) lets you
        calculate the overlay dataset on the fly, from multiple inputs,
        using the same expression syntax as 3dcalc, 1deval, etc.

**Tip 37**

    .. code-block:: none

        You can right-click on the label to the left of a drop-down menu
        (e.g., 'ULay', 'Xhairs', 'Color') to get a chooser panel that lets you
        control the menu choice in a different way, with a separate chooser.

**Tip 38**

    .. code-block:: none

        The 'Rota' arrows (in Define Overlay) lets you rotate the color bar,
        one color step per click -- if you use Shift+click, it takes 5
        color steps per click.
        The 'F' button to the right will flip the color bar top-to-bottom.

**Tip 39**

    .. code-block:: none

        The image viewer right-click popup menu has several useful functions:
        * Jumpback        = take crosshairs to their previous location
        * Where Am I?     = show atlas information about the current location
        * Image Display   = hide GUI controls
        * Draw ROI Plugin = activate the Drawing plugin

**Tip 40**

    .. code-block:: none

        Right-click on the 'Disp' button (lower left) of an image viewer will
        raise the corresponding AFNI controller to the top.
        Right-click on the AFNI logo (lower left) of a graph viewer does the same.
        These functions are here in case you lose the controller somewhere on
        the screen, and want to get it back.

**Tip 41**

    .. code-block:: none

        Right-click on the 'Save' button in an image viewer will popup the list
        of possible image save formats, and let you choose one. You can do this
        from the 'Disp' control panel also, but this right-click method is faster.

**Tip 42**

    .. code-block:: none

        The 'Rec' button in an image viewer pops up a menu that lets you choose
        different options for saving image snapshots to a special 'Record' viewer.
        Once you have recorded the set of images you like, you can save them
        from the 'Record' viewer. This is one way to make a video of how the
        overlay image changes as the threshold slider moves, for example.
        * Next One = record the next image displayed
        * Stay On  = record each new image displayed (until turned Off)

**Tip 43**

    .. code-block:: none

        Left-click in the square right of 'Etc->' in an AFNI controller will
        popup a copy of the splash screen again. Another left-click there will
        pop the splash window down again. Clicking in the reincarnated splash screen
        may give funny results.
        Right-click in that square will give a menu with some fun choices.
        Middle-click in that square will popup a random insult.

**Tip 44**

    .. code-block:: none

        Set environment variable AFNI_DATASET_BROWSE to YES and then when you
        click on a dataset name in the OverLay or UnderLay popup chooser, AFNI
        will switch to viewing that dataset immediately (rather than waiting for
        you to press 'Set'). You can also browse through datasets in these
        choosers using the keyboard up/down arrows.

**Tip 45**

    .. code-block:: none

        You can adjust the brightness and contrast of the underlay (grayscale)
        image by using the 'b' and 'c' arrows at the right of an image viewer.
        A more interactive method is to press and hold down the left mouse button,
        then drag the cursor around up/down (brightness) or left/right (contrast).
        With this method, you just wiggle the mouse around while left-click is
        down, and you can adjust the image grayscale until it looks good.
        The 'Norm' button will reset the grayscale contrast to the startup setting,
        in case you make things look terrible.

**Tip 46**

    .. code-block:: none

        Set environment variable AFNI_CREEPTO to YES, and then the 'Jump to' button
        will move the crosshairs to the chosen location incrementally, rather than
        in one big jump.  The reasons for using this feature are (a) to help
        get a feel for the transit, and (b) just plain fun.

**Tip 47**

    .. code-block:: none

        Right-click on the color bar in Define Overlay, and you can change the color
        scale that is used.
        You can switch the color bar to a discrete set of solid colors by using the
        menu labeled '#' just beneath the color bar.
        You can save an image of the color bar by right-clicking on the label above
        it, and choosing 'Save to PPM' from the popup menu.

**Tip 48**

    .. code-block:: none

        You can crop an image by left-clicking the 'crop' button in an image viewer,
        then selecting the crop region by clicking+dragging in the image.
        You can Montage cropped images (all will be cropped the same way).
        Right-clicking on 'crop' will give a chooser where you can specify the
        cropping region size exactly.

**Tip 49**

    .. code-block:: none

        You can use keyboard shortcuts to precisely adjust the threshold slider.
        Put the mouse over the slider, and then
        * down/up arrows    for tiny adjustments
        * page up/page down for larger adjustments

**Tip 50**

    .. code-block:: none

        In a graph viewer, you can restrict the plotting to a subset of the time
        points by using the 'Opt->Grid->Index Pin' menu item. This feature is most
        useful when viewing very lengthy datasets.

**Tip 51**

    .. code-block:: none

        In a graph viewer, the default plotting method has the bottom of each graph
        using a separate value (the minimum in that voxel). You can also make them
        have a common baseline (minimum among all voxels in the graph window) or
        a global baseline (set by you) by using the 'Opt->Baseline' menu items.

**Tip 52**

    .. code-block:: none

        At the bottom of a graph viewer is a bunch of text showing various
        information about what is being shown.

**Tip 53**

    .. code-block:: none

        When looking at FMRI data graphs with a regular stimulus timing, it is
        helpful to set the graph grid lines to match the stimulus timing spacing.
        You can do this from the 'Opt->Grid->Choose' menu item.

**Tip 54**

    .. code-block:: none

        You can have graphs drawn as box plots rather than as connected line segments,
        by using the 'Opt->Colors, Etc.->(Data) Boxes' menu item, or by pressing the
        'B' key when the mouse cursor is over the graph viewer window.

**Tip 55**

    .. code-block:: none

        In the graph viewer 'Opt' and 'FIM' menus, items that have keyboard shortcuts
        have the key inside square brackets, as in 'Opt->Scale->Down [-]', meaning
        the '-' key will cause the graph to scaled down (vertically).

**Tip 56**

    .. code-block:: none

        Advanced graphing: you can change the x-axis values from being 0,1,2,... to be
        anything you want, chosen from a 1D text file (applies to all voxels) or from
        a 3D dataset (per voxel x-coordinates). The x-axis for the central sub-plot will
        be displayed as a vertical graph at the left of the graph viewer window. See
        the 'Opt->X-axis' menu items to do strange things.

**Tip 57**

    .. code-block:: none

        The 'Define Datamode->Misc' menu has a lot of choices, a few of which are:
        * Voxel Coords? = show voxel indexes instead of mm coordinates in AFNI GUI
        * ULay Info     = show information from the UnderLay dataset header
        * Purge Memory  = eject datasets from memory, forcing reloads when viewed

**Tip 58**

    .. code-block:: none

        When saving an image (or a montage), you might want to turn the crosshairs off.
        You can do this from the 'Xhairs' menu in the AFNI controller.
        If you want all the sub-images in a montage to have crosshairs (instead of
        just the central image), turn the 'X+' button on.

**Tip 59**

    .. code-block:: none

        Just below the slider bar in an image viewer is a label, such as
        'Axial: left=Left'. This label indicates that you are looking at an axial
        image and the software thinks that the left side of the image viewer is
        the subject's Left. Similarly, the sagittal viewer label would normally
        say 'Sagittal: left=Anterior'. However, these labels will change if you
        alter the image viewing orientation in the 'Disp' control panel.

**Tip 60**

    .. code-block:: none

        When saving from the image viewer, the saved image is on the matrix of the
        dataset. It is NOT a screen capture; that is, the image saved will not depend
        on the size of the image viewer window. A montage image will be the full size
        of all the base images catenated together. You can also choose a 'Blowup'
        factor to scale the image size upward: factors from 2 to 8 are available.

**Tip 61**

    .. code-block:: none

        You can tell the graph viewer to ignore the first few time points when plotting.
        Menu item 'FIM->Ignore' lets you choose how many to ignore by mouse clicks.
        Keypress 'I' increases the ignore count by 1, 'i' decreases by 1.
        Ignored points are plotted with little blue circles which take the value of
        the first non-ignored point.

**Tip 62**

    .. code-block:: none

        If you have a complicated AFNI window layout you want to save, you can use
        'Define Datamode->Misc->Save Layout' to save a startup script that will
        be used when you re-start AFNI in the same directory to restore the AFNI
        windows to (approximately) the same state they had before.

**Tip 63**

    .. code-block:: none

        Did you know that AFNI can display datasets stored with various data types?
        * byte (unsigned 8 bit integers)  * short (signed 16 bit integers)
        * float (32 bit values)           * complex (pairs of floats)
        * RGB  (triples of bytes)

**Tip 64**

    .. code-block:: none

        AFNI will read in .jpg and .png image files as 2D 'datasets'. Is this useful?
        It depends on who you ask! If you don't like this, set Unix environment
        variable AFNI_IMAGE_DATASETS to NO (in your ~/.afnirc file).

**Tip 65**

    .. code-block:: none

        The AFNI program 'aiv' (AFNI Image Viewer) can be used for a quick display
        of images in various formats (.jpg, .png, plus datasets). The interface
        is the same as the slicer viewer built into the AFNI GUI.

**Tip 66**

    .. code-block:: none

        The AFNI GUI now 'knows' about the BIDS file hierarchy. You can open all
        the datasets from a given subject in a single session, even though BIDS
        scatters them over several subdirectories. To do this, use the '-bysub'
        option. See the output of 'afni -help' for the details.

**Tip 67**

    .. code-block:: none

        Obscure AFNI GUI buttons:
        EditEnv = Lets you edit some AFNI environment settings interactively;
        useful when you need to change something and don't want
        to quit and re-start AFNI. For example, setting
        AFNI_LEFT_IS_POSTERIOR will flip the usual Sagittal
        image and graph viewers so that the display's left
        corresponds to the subject's posterior, rather than
        the default anterior.
        NIML+PO = Starts NIML and Plugout socket listening; useful when you
        meant to do one (or both) of these one the command line
        (options '-niml' and '-yesplugouts'), but forgot.
        For example, NIML is needed for 3dGroupInCorr to connect.

**Tip 68**

    .. code-block:: none

        The 'Render Dataset' plugin allows you to do 3D volume rendering
        in the AFNI GUI, with color overlays, animations, and cutouts.
        (The SUMA GUI also has a volume rendering mode.)

**Tip 69**

    .. code-block:: none

        Want your picture in the AFNI splash window at startup? Send us a JPEG
        image, formatted to be square 128x128, and we can include it!

**Tip 70**

    .. code-block:: none

        Questions about AFNI? Problems with a program? Try our Message Board at
        https://afni.nimh.nih.gov/afni/community/board/
        Please be specific and focused, as generic questions without
        details are very hard to answer well on a Web forum.

**Tip 71**

    .. code-block:: none

        If you are doing complicated twisted things with AFNI programs, ASK US
        (on the message board). Often, there is an easier way to do a task!

**Tip 72**

    .. code-block:: none

        REMEMBER: afni_proc.py is your friend when doing time series analyses!
        In particular, if you are still using custom hand-written scripts for
        resting-state preprocessing or time series regression, you need to
        learn to use afni_proc.py (unless you are doing something unusual).

**Tip 73**

    .. code-block:: none

        Skull stripping T1-weighted datasets? Programs and scripts include:
        * 3dSkullStrip     - surface expansion program (many options)
        * @NoisySkullStrip - when the dataset is very noisy
        * @SSwarper        - nonlinear warping to MNI space combined
        with skull stripping (and then the warp can be
        used in afni_proc.py for time series analyses)

**Tip 74**

    .. code-block:: none

        Program 3dUnifize can make the image intensity of a T1-weighted dataset
        more uniform in space. As a bonus, can also contrast-invert a dataset
        prior to the uniform-ization, which might be useful for T2-weighted
        datasets. It also has an experimental option for processing EPI data.

**Tip 75**

    .. code-block:: none

        Program 3dcalc does voxelwise calculations on datasets. Doesn't sound
        exciting to you? Once you get to know it, you will find that 3dcalc is
        your FRIEND! (: And then you can get to know the interactive InstaCalc :)

**Tip 76**

    .. code-block:: none

        AFNI has a lot of downloadable demonstrations; you can find them in your
        abin directory (if that's where AFNI is for you) by doing
        ls ~/abin/@Install_*
        A few examples:
        @Install_InstaCorr_Demo = data and instructions for using InstaCorr
        @Install_ClustScat_Demo = data and instructions for interactively plotting
        time series extracted from Clusterize ROIs
        @Install_FATCAT_DEMO    = data and instructions for using the AFNI FATCAT
        programs for DTI tractography (etc.)

**Tip 77**

    .. code-block:: none

        Program 3drefit can be used to change parameters in a dataset
        header (e.g., slice timing). Program 3dinfo can be used to
        display information from a dataset header.

**Tip 78**

    .. code-block:: none

        Are you using 3dcalc to compute the average of a bunch of datasets?
        You should use program 3dMean instead! It is faster and doesn't
        have the artificial limitation of 26 input datasets.

**Tip 79**

    .. code-block:: none

        Want to calculate summary values (e.g., mean, standard deviation)
        at each voxel in a time series dataset? Program 3dTstat is the
        tool you need.

**Tip 80**

    .. code-block:: none

        Programs for extracting information from spatial ROIs:
        * 3dmaskave  = get average across the ROI, one value per time point
        * 3dROIstats = like 3dmaskave, but for multiple ROIs
        * 3dmaskSVD  = like 3dmaskave, but gives the principal singular
        vector (time series) across the ROI instead of the mean
        * 3dmaskdump = just dumps out ALL the values from the ROI at
        all time points (presumably you will do something
        fun with these in your own software?)

**Tip 81**

    .. code-block:: none

        Programs for computing some local statistics in a neighborhood around
        each voxel (e.g., a ball):
        * 3dLocalstat   = various statistics from the neighborhood
        (e.g., mean, median, variance)
        * 3dLocalBistat = various 2-sample statistics from the neighborhood,
        calculated between 2 datasets
        (e.g., Pearson or Spearman correlation)
        * 3dLocalPV     = compute the principal vector (time series) from
        all the dataset time series inside the neighborhood
        (a fancy way to 'smooth' the data)

**Tip 82**

    .. code-block:: none

        Are you using nonlinear warping to align your subjects' T1-weighted
        datasets? If not, you should give it a try. The brain images will line
        up better than using affine alignment (3dAllineate, @auto_tlrc, etc.)
        and you can get better FMRI results at the group level when you use
        the nonlinear warps in afni_proc.py. Programs for this:
        * 3dQwarp      = the foundational nonlinear warping program
        * @SSwarper    = uses 3dQwarp and 3dSkullStrip together to align
        volumes to the MNI template and skull strip them
        * auto_warp.py = runs 3dQwarp for you, so you don't have to read
        that program's lengthy help output

**Tip 83**

    .. code-block:: none

        Want to create a 'junk' dataset on the command line, just to test to
        see if something works? AFNI programs can create a dataset in memory
        from a string; try this example:
        afni jRandomDataset:64,64,32,96
        to create and view a random dataset with 64x64x32 3D volumes, and
        96 time points. If you want to create and SAVE such a dataset, try
        3dcalc -a jRandomDataset:64,64,32,96 -expr 'a' -prefix Fred.nii
        If you want a zero-filled dataset, replace 'a' with '0'.

**Tip 84**

    .. code-block:: none

        Did you know that AFNI's time series analysis program 3dREMLfit can
        include voxelwise regressors (a different time series for each voxel)?
        We use this capability in our Anaticor model for de-noising datasets
        during activation or resting state analyses.

**Tip 85**

    .. code-block:: none

        AFNI programs for individual dataset time series correlation-ing:
        * 3dTcorr1D        = correlate each voxel with a small set of 1D files
        * 3dTcorrelate     = correlate each voxel between 2 datasets
        * 3dTcorrMap       = make of map of how correlated each voxel is
        to every other voxel in the same dataset
        * 3dAutoTcorrelate = correlate each voxel to every other voxel
        in the same dataset and save everything (HUGE)

**Tip 86**

    .. code-block:: none

        Program 3dGroupInCorr can be used for Group Instant Correlation
        interactively via the AFNI GUI. It can also be used in batch mode.
        The '-batchGRID' option lets you scan over a grid of seed voxels,
        compute the individual datasets' correlations with their seeds,
        then compute the t-tests among these correlation maps, and save
        the results to a collection of datasets.

**Tip 87**

    .. code-block:: none

        Want to convert each statistic in a dataset to a (voxelwise) p-value?
        Use program 3dPval.
        More complicated statistical conversions can be done with 3dcalc, using
        the cdf2stat() and stat2cdf() functions. You can explore those interactively
        using the ccalc program, to make sure you are giving 3dcalc the correct
        expression.

**Tip 88**

    .. code-block:: none

        Want to test dataset values voxelwise for normality (Gaussianity)?
        Program 3dNormalityTest will apply the Anderson-Darling test and
        give you a dataset with the voxelwise measure of non-Gaussianity.

**Tip 89**

    .. code-block:: none

        Program 1dCorrelate will compute the pairwise correlation coefficient
        between 2 columns of numbers, AND give you the bootstrap confidence
        interval for the result. When you have relatively few samples
        (say, less than 25), bootstrap confidence intervals are more robust
        than the standard parametric intervals based on the Gaussian assumption.

**Tip 90**

    .. code-block:: none

        Program 1dplot is useful for quick-and-dirty plotting of columns
        of numbers (.1D files). An example, creating a response model
        function with 3dDeconvolve and sending the time series directly
        into 1dplot:
        3dDeconvolve -num_stimts 1 -polort -1 -nodata 81 0.5         \
        -stim_times 1 '1D: 0' 'TWOGAMpw(3,6,0.2,10,12)' \
        -x1D stdout: | 1dplot -stdin -THICK -del 0.5

**Tip 91**

    .. code-block:: none

        Program 1dNLfit does a nonlinear fit of an expression with free
        parameters to a column of numbers. For example:
        1dNLfit -depdata sc.1D -indvar x '1D: 100%0:0.1' \
        -expr 'a*sin(b*x)+c*cos(b*x)'            \
        -param a=-2:2 -param b=1:3 -param c=-2:2  > ff.1D
        fits a sine+cosine model with 3 free parameters (a,b,c) to the
        data in file sc.1D, where the 'time' parameter is x.

**Tip 92**

    .. code-block:: none

        You can use make_random_timing.py to make AFNI-compatible random
        stimulus timing files. You can impose various constraints on the
        times generated.
        You can use timing_tool.py to manipulate stimulus timing files in
        various ways.

**Tip 93**

    .. code-block:: none

        Program 1dTsort lets you sort .1D columns of numbers.
        Program 3dTsort lets you sort each voxel's time series (separately),
        and can also randomize them if you want.

**Tip 94**

    .. code-block:: none

        Program 'count' will generate lists of numbers, which is surprisingly
        useful in scripting various things. For example,
        count -dig 1 -comma 0 99 S6
        will produce a list of 9 distinct random numbers from 0..99 (inclusive),
        separated by commas; for example: '31,18,60,62,7,95'. This list could
        be used to select a random subset of dataset sub-bricks for analysis
        3dttest++ -setA Fred.nii[`count -dig 1 -comma 0 333 S20`]
        (in the above command, the quotes are the single backquote ` and not
        the single frontquote ').

**Tip 95**

    .. code-block:: none

        Most AFNI command line programs accept a common set of options, such
        as sub-brick selectors. See this page for the details:
        https://afni.nimh.nih.gov/pub/dist/doc/program_help/common_options.html

**Tip 96**

    .. code-block:: none

        Want to resample a dataset to a different grid spacing? Programs:
        * 3dresample  = older program with NN, Linear, and Cubic interpolation
        * 3dAllineate = for aligning datasets and then resampling, but with
        the -1Dparam_apply option can just do the resampling
        function; has more interpolation options, including
        quintic polynomials and tapered sinc.
        * 3dUpsample  = resamples a dataset to a finer grid in the time
        direction (the other programs change spatial grids).

**Tip 97**

    .. code-block:: none

        Want to blur/smooth a dataset? Programs:
        * 3dmerge -1blur_fwhm = Gaussian smoothing across whole volume
        * 3dBlurInMask        = smoothing only inside a mask
        * 3dLocalPV           = 'smooth' a time series dataset by computing
        the local principal vector around each
        voxel, instead of the average vector (slow)

**Tip 98**

    .. code-block:: none

        Want to create a 3D dataset from a table of numbers? This can be
        done with program 3dUndump.

**Tip 99**

    .. code-block:: none

        Want to slice up a dataset? Or glue datasets together?
        * 3dZcutup  = cut a section of slices out of a dataset
        * 3dZcat    = glue datasets together in the slice direction
        * 3dXYZcat  = glue datasets together in any (spatial) direction
        * 3dZeropad = add (or subtract) slices to (or from) a dataset
        * 3dTcat    = glue datasets together in the time direction

**Tip 100**

    .. code-block:: none

        Did someone give you a dataset with the spatial orientation 'flipped',
        so that the subject's left is marked as being the right? Program
        3dLRflip can flip the data rows to rearrange the data so the dataset
        header information and the actual data match again.

**Tip 101**

    .. code-block:: none

        Want to write an AFNI '3d' program? If you know C, the code 3dToyProg.c
        is a good starting point. It shows how to read, calculate, and write
        datasets, and is heavily commented.

**Tip 102**

    .. code-block:: none

        Almost all AFNI command line programs take '-help' as an option; for example
        3dTstat -help
        This method is how you can get the most up-to-date information about using
        an AFNI program. All '-help' output are also formatted as Web pages and
        are available here:
        https://afni.nimh.nih.gov/afni/doc/program_help/index.html

