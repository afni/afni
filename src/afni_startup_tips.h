#ifndef _AFNI_STARTUP_TIP_HEADER_
#define _AFNI_STARTUP_TIP_HEADER_

/***** This file contains startup tips AND the goodbye messages *****/

/*---- startup tips here ----*/

#undef  NTIP
#define NTIP ((int)(sizeof(tip)/sizeof(char *)))
static char *tip[] = {

/*-- Tips for the AFNI GUI --*/

   "If you set environment variable AFNI_GLOBAL_SESSION to the name\n"
   "of a directory with datasets, then those datasets will be visible\n"
   "in the UnderLay and OverLay choosers. For example, copy the MNI\n"
   "template MNI152_2009_template.nii.gz to this directory, and then\n"
   "you'll always be able to use it as an underlay dataset."
 ,
   "If the aspect ratio (width/height) of an image viewer window looks\n"
   "bad, you can fix it by typing the 'a' key into the image, or by\n"
   "clicking the left mouse button in the intensity grayscale bar at\n"
   "the right of the image."
 ,
   "The right-click popup menu on the intensity grayscale bar to the right\n"
   "of an image viewer has several useful controls, including:\n"
   " * choosing the numerical Display Range for the underlay\n"
   " * drawing a coordinate Label over the image\n"
   " * applying an Automask to the overlay (e.g.,hide the non-brain stuff)\n"
   " * choosing the color for Zero values in the overlay (e.g., black or white)"
 ,
   "Looking at venography or arteriography datasets? The image viewer 'Disp'\n"
   "control panel 'Project' menu lets you look at projections of the underlay\n"
   "dataset through a slab of slices, including Minimum and Maximum. The\n"
   "slab half-thickness is given by the 'Slab +-' control below 'Project'."
 ,
   "If you crop an image, you can move the crop window around by pressing\n"
   "the Shift key plus one of the keyboard arrow keys."
 ,
   "The 'Disp' button in an image viewer pops up a control panel with many\n"
   "useful buttons, including:\n"
   " * Project   = combine multiple slices into one underlay\n"
   " * Tran 0D   = transform values of the underlay pixelwise\n"
   " * Tran 2D   = transform underlay image globally (e.g., blurring)\n"
   " * Rowgraphs = graph the underlay numerical values in 1-9 pixel rows"
 ,
   "The 'BHelp' button lets you click on some other button in the GUI\n"
   "to get more information about what that button does."
 ,
   "The right-click popup menu on the coordinate display in the AFNI\n"
   "controller has several useful functions, including:\n"
   " * controlling the coordinate display order\n"
   " * jumping to x,y,z (mm) or i,j,k (voxel index) coordinates"
 ,
   "The right-click popup menu on the label above the threshold slider\n"
   "lets you control the threshold in various ways:\n"
   " * pin the Threshold sub-brick to equal the OLay or OLay+1 sub-brick\n"
   "   (OLay+1 is very useful for Coef/t-statistic sub-brick pairs)\n"
   " * set the threshold slider to have a given voxelwise p-value\n"
   "   (based on the statistical properties of the current Thr sub-brick)\n"
   " * control Alpha fading for colorization of sub-threshold voxels\n"
   " * see only Positive or Negative values, with respect to the threshold\n"
   "   (which will affect the p-value, as being 1- or 2-sided)"
 ,
   "The right-click popup menu on the label above the color overlay bar\n"
   "lets you control colorization from the OLay sub-brick in several ways:\n"
   " * you can jump crosshairs to the largest OLay value above threshold\n"
   " * you can write the current color palette out to a file for editing,\n"
   "   or to an image for use in a figure\n"
   " * you can apply pixelwise or 2D spatial transformations to the\n"
   "   OLay values before they are turned into colors"
 ,
   "You can run InstaCorr on several subjects at the same time, using\n"
   "multiple AFNI controllers opened with the 'New' button."
 ,
   "The 'New' button (lower left corner of AFNI controller) lets you open\n"
   "another AFNI controller. The UnderLay and OverLay datasets will be\n"
   "listed in the controller window title bar."
 ,
   "Image viewer keypress: q = close window (works in graph viewer too)"
 ,
   "Image viewer keypress: S = save image (works in graph viewer too)"
 ,
   "Image viewer keypress: o = turn OLay color on or off"
 ,
   "Image viewer keypress: u = make underlay image from the OLay dataset\n"
   "                       press u again to make underlay image from ULay"
 ,
   "Image viewer keypress: 4 or 5 or 6 = meld ULay and OLay images\n"
   "                       (controlled by a slider on top of the image)\n"
   " * 4 = OLay on left side, ULay on right side, slider moves boundary\n"
   " * 5 = OLay on top side, ULay on bottom side, slider moves boundary\n"
   " * 6 = ULay and OLay intensity mixed, slider controls mixing fraction\n"
   "       (slider to left = more ULay; to right = more OLay)"
 ,
   "Image viewer keypress: z/Z = zoom out or in\n"
   "                       Zooming is limited to factors of 1-4"
 ,
   "Graph viewer keypress: < or > = move focus time down or up 1 TR"
 ,
   "Graph viewer keypress: 1 or L = move focus time to first or last TR"
 ,
   "Graph viewer keypress: v/V = video the focus time up or down\n"
   "                       This is how you can make a video of subject\n"
   "                       head movement, by looking at the image viewers\n"
   "                       while the graph viewer is doing 'v'."
 ,
   "Graph viewer keypress: m/M = decrease/increase matrix size of graphs\n"
   "                       Also can do this from the 'Opt->Matrix' menu."
 ,
   "Graph viewer keypress: w = write time series from central sub-graph to a file\n"
   "                       Set prefix for file from the 'Opt' menu."
 ,
   "The image viewer 'Mont' button (along bottom) will let you make a montage\n"
   "from multiple slices, which can be Saved to a .jpg or .png file.\n"
   "NOTE: you might want to turn the crosshairs off from the 'Xhairs' menu\n"
   "      in the main AFNI controller."
 ,
   "If the image editing program 'gimp' is in your path, then the image viewer\n"
   "Save control panel will include an option to start gimp on your image, so\n"
   "you can further edit it immediately. See https://www.gimp.org/"
 ,
   "The graph viewer 'Tran 1D' function Dataset#N (from the 'Opt' main menu)\n"
   "lets you plot extra dataset time series on top of the UnderLay dataset's\n"
   "time series graphs."
 ,
   "You can change the way the graph viewer shows its plots by using the\n"
   "'Colors, Etc.' sub-menu from the main 'Opt' menu (lower right corner):\n"
   " * Boxes     = color of the boxes around each sub-graph\n"
   " * BackG     = color of background\n"
   " * Grid      = color of vertical grid lines\n"
   " * Text      = color of text\n"
   " * Data      = color of data\n"
   "               graph points only, or points+lines, or boxes\n"
   " * Graph Gap = how many pixels spacing between sub-graphs\n"
   " * Thick     = how many pixels wide for 'Thick' lines\n"
   "Most of these settings can also be selected by AFNI environment\n"
   "settings in your .afnirc file; with some work, you can setup the\n"
   "graph viewer to look the way you want it to be permanently."
 ,
   "The graph viewer 'Opt->Detrend' menu item lets you choose a polynomial degree\n"
   "for detrending the graph data. This can help you visualize the features of the\n"
   "data you want to see without be distracted by long term trends up or down.\n"
   " -1 = no detrending ; 0 = remove mean ; 1 = remove linear trend ; et cetera"
 ,
   "The graph viewer 'Opt->Tran 1D->Despike' function will despike the time series\n"
   "graphs, which can be useful when you trying to figure out what's going on\n"
   "in a dataset."
 ,
   "Right-clicking in a graph viewer plot will popup a window with some statistics\n"
   "about the data being shown."
 ,
   "The README.environment text file lists many Unix 'environment' variables that\n"
   "can be used to control the way AFNI appears and operates."
 ,
   "The Define Datamode control panel lets you control how the OLay dataset is\n"
   "resampled to fit the ULay dataset (that defines the basis for the pixel grid\n"
   "on which the images are displayed). The options are:\n"
   " * NN = Nearest Neighbor  * Li = Linear\n"
   " * Cu = Cubic             * Bk = Blocky (between NN and Li)\n"
   "When the OverLay is at a coarser resolution than the UnderLay (common in FMRI),\n"
   "Li will produce 'nicer' looking maps, but NN will be more 'honest' looking."
 ,
   "'Define Datamode->Lock' lets you turn the xyz coordinate lock between AFNI\n"
   "controllers off, if you want. Or, you can turn on 'Time Lock', so that the\n"
   "TR index is locked between controllers, as well as the crosshair location."
 ,
   "* Normally, the grid size of the pixel image created for display in an\n"
   "   AFNI image viewer is take from the grid size of the Underlay dataset.\n"
   "* But you can change that using the 'Datamode' control panel, by choosing\n"
   "   'Warp ULay on Demand', then setting the grid resampling mode below\n"
   "   (e.g., to Li=Linear or Cu=Cubic interpolation).\n"
   "* Sometimes using this to make the display grid more fine is useful for\n"
   "   creating nicer looking functional images, especially when 'Alpha' is\n"
   "   turned on (to outline above-threshold clusters and at the same time\n"
   "   show below-threshold in faded-out translucent colors)."
 ,
   "Normally, voxels whose threshold value is below the slider setting will\n"
   "not be colorized. 'Alpha' fading allows them to get a faded color, while\n"
   "the above-threshold voxel clusters will get a black outline drawn around\n"
   "them. Alpha can be turned on from the right-click popup menu above the\n"
   "threshold slider, or via the AFNI_FUNC_ALPHA environment variable in\n"
   "your .afnirc file."
 ,
   "The InstaCalc function (from the InstaCorr drop-down menu) lets you\n"
   "calculate the overlay dataset on the fly, from multiple inputs,\n"
   "using the same expression syntax as 3dcalc, 1deval, etc."
 ,
   "You can right-click on the label to the left of a drop-down menu\n"
   "(e.g., 'ULay', 'Xhairs', 'Color') to get a chooser panel that lets you\n"
   "control the menu choice in a different way, with a separate chooser."
 ,
   "The 'Rota' arrows (in Define Overlay) lets you rotate the color bar,\n"
   " one color step per click -- if you use Shift+click, it takes 5\n"
   " color steps per click.\n"
   "The 'F' button to the right will flip the color bar top-to-bottom."
 ,
   "The image viewer right-click popup menu has several useful functions:\n"
   " * Jumpback        = take crosshairs to their previous location\n"
   " * Where Am I?     = show atlas information about the current location\n"
   " * Image Display   = hide GUI controls\n"
   " * Draw ROI Plugin = activate the Drawing plugin"
 ,
   "Right-click on the 'Disp' button (lower left) of an image viewer will\n"
   " raise the corresponding AFNI controller to the top.\n"
   "Right-click on the AFNI logo (lower left) of a graph viewer does the same.\n"
   "These functions are here in case you lose the controller somewhere on\n"
   " the screen, and want to get it back."
 ,
   "Right-click on the 'Save' button in an image viewer will popup the list\n"
   "of possible image save formats, and let you choose one. You can do this\n"
   "from the 'Disp' control panel also, but this right-click method is faster."
 ,
   "The 'Rec' button in an image viewer pops up a menu that lets you choose\n"
   "different options for saving image snapshots to a special 'Record' viewer.\n"
   "Once you have recorded the set of images you like, you can save them\n"
   "from the 'Record' viewer. This is one way to make a video of how the\n"
   "overlay image changes as the threshold slider moves, for example.\n"
   " * Next One = record the next image displayed\n"
   " * Stay On  = record each new image displayed (until turned Off)"
 ,
   "Left-click in the square right of 'Etc->' in an AFNI controller will\n"
   " popup a copy of the splash screen again. Another left-click there will\n"
   " pop the splash window down again. Clicking in the reincarnated splash screen\n"
   " may give funny results.\n"
   "Right-click in that square will give a menu with some fun choices.\n"
   "Middle-click in that square will popup a random insult."
 ,
   "Set environment variable AFNI_DATASET_BROWSE to YES and then when you\n"
   "click on a dataset name in the OverLay or UnderLay popup chooser, AFNI\n"
   "will switch to viewing that dataset immediately (rather than waiting for\n"
   "you to press 'Set'). You can also browse through datasets in these\n"
   "choosers using the keyboard up/down arrows."
 ,
   "You can adjust the brightness and contrast of the underlay (grayscale)\n"
   " image by using the 'b' and 'c' arrows at the right of an image viewer.\n"
   "A more interactive method is to press and hold down the left mouse button,\n"
   " then drag the cursor around up/down (brightness) or left/right (contrast).\n"
   " With this method, you just wiggle the mouse around while left-click is\n"
   " down, and you can adjust the image grayscale until it looks good.\n"
   "The 'Norm' button will reset the grayscale contrast to the startup setting,\n"
   " in case you make things look terrible."
 ,
   "Set environment variable AFNI_CREEPTO to YES, and then the 'Jump to' button\n"
   "will move the crosshairs to the chosen location incrementally, rather than\n"
   "in one big jump.  The reasons for using this feature are (a) to help\n"
   "get a feel for the transit, and (b) just plain fun."
 ,
   "Right-click on the color bar in Define Overlay, and you can change the color\n"
   " scale that is used.\n"
   "You can switch the color bar to a discrete set of solid colors by using the\n"
   " menu labeled '#' just beneath the color bar.\n"
   "You can save an image of the color bar by right-clicking on the label above\n"
   " it, and choosing 'Save to PPM' from the popup menu."
 ,
   "You can crop an image by left-clicking the 'crop' button in an image viewer,\n"
   " then selecting the crop region by clicking+dragging in the image.\n"
   "You can Montage cropped images (all will be cropped the same way).\n"
   "Right-clicking on 'crop' will give a chooser where you can specify the\n"
   " cropping region size exactly."
 ,
   "You can use keyboard shortcuts to precisely adjust the threshold slider.\n"
   "Put the mouse over the slider, and then\n"
   " * down/up arrows    for tiny adjustments \n"
   " * page up/page down for larger adjustments"
 ,
   "In a graph viewer, you can restrict the plotting to a subset of the time\n"
   "points by using the 'Opt->Grid->Index Pin' menu item. This feature is most\n"
   "useful when viewing very lengthy datasets."
 ,
   "In a graph viewer, the default plotting method has the bottom of each graph\n"
   "using a separate value (the minimum in that voxel). You can also make them\n"
   "have a common baseline (minimum among all voxels in the graph window) or\n"
   "a global baseline (set by you) by using the 'Opt->Baseline' menu items."
 ,
   "At the bottom of a graph viewer is a bunch of text showing various\n"
   "information about what is being shown."
 ,
   "When looking at FMRI data graphs with a regular stimulus timing, it is\n"
   "helpful to set the graph grid lines to match the stimulus timing spacing.\n"
   "You can do this from the 'Opt->Grid->Choose' menu item."
 ,
   "You can have graphs drawn as box plots rather than as connected line segments,\n"
   "by using the 'Opt->Colors, Etc.->(Data) Boxes' menu item, or by pressing the\n"
   "'B' key when the mouse cursor is over the graph viewer window."
 ,
   "In the graph viewer 'Opt' and 'FIM' menus, items that have keyboard shortcuts\n"
   "have the key inside square brackets, as in 'Opt->Scale->Down [-]', meaning\n"
   "the '-' key will cause the graph to scaled down (vertically)."
 ,
   "Advanced graphing: you can change the x-axis values from being 0,1,2,... to be\n"
   "anything you want, chosen from a 1D text file (applies to all voxels) or from\n"
   "a 3D dataset (per voxel x-coordinates). The x-axis for the central sub-plot will\n"
   "be displayed as a vertical graph at the left of the graph viewer window. See\n"
   "the 'Opt->X-axis' menu items to do strange things."
 ,
   "The 'Define Datamode->Misc' menu has a lot of choices, a few of which are:\n"
   " * Voxel Coords? = show voxel indexes instead of mm coordinates in AFNI GUI\n"
   " * ULay Info     = show information from the UnderLay dataset header\n"
   " * Purge Memory  = eject datasets from memory, forcing reloads when viewed"
 ,
   "When saving an image (or a montage), you might want to turn the crosshairs off.\n"
   " You can do this from the 'Xhairs' menu in the AFNI controller.\n"
   "If you want all the sub-images in a montage to have crosshairs (instead of\n"
   " just the central image), turn the 'X+' button on."
 ,
   "Just below the slider bar in an image viewer is a label, such as\n"
   "'Axial: left=Left'. This label indicates that you are looking at an axial\n"
   "image and the software thinks that the left side of the image viewer is\n"
   "the subject's Left. Similarly, the sagittal viewer label would normally\n"
   "say 'Sagittal: left=Anterior'. However, these labels will change if you\n"
   "alter the image viewing orientation in the 'Disp' control panel."
 ,
   "When saving from the image viewer, the saved image is on the matrix of the\n"
   "dataset. It is NOT a screen capture; that is, the image saved will not depend\n"
   "on the size of the image viewer window. A montage image will be the full size\n"
   "of all the base images catenated together. You can also choose a 'Blowup'\n"
   "factor to scale the image size upward: factors from 2 to 8 are available."
 ,
   "You can tell the graph viewer to ignore the first few time points when plotting.\n"
   "Menu item 'FIM->Ignore' lets you choose how many to ignore by mouse clicks.\n"
   "Keypress 'I' increases the ignore count by 1, 'i' decreases by 1.\n"
   "Ignored points are plotted with little blue circles which take the value of\n"
   "the first non-ignored point."
 ,
   "If you have a complicated AFNI window layout you want to save, you can use\n"
   "'Define Datamode->Misc->Save Layout' to save a startup script that will\n"
   "be used when you re-start AFNI in the same directory to restore the AFNI\n"
   "windows to (approximately) the same state they had before."
 ,
   "Did you know that AFNI can display datasets stored with various data types?\n"
   " * byte (unsigned 8 bit integers)  * short (signed 16 bit integers)\n"
   " * float (32 bit values)           * complex (pairs of floats)\n"
   " * RGB  (triples of bytes)"
 ,
   "AFNI will read in .jpg and .png image files as 2D 'datasets'. Is this useful?\n"
   "It depends on who you ask! If you don't like this, set Unix environment\n"
   "variable AFNI_IMAGE_DATASETS to NO (in your ~/.afnirc file)."
 ,
   "The AFNI program 'aiv' (AFNI Image Viewer) can be used for a quick display\n"
   "of images in various formats (.jpg, .png, plus datasets). The interface\n"
   "is the same as the slicer viewer built into the AFNI GUI."
 ,
   "The AFNI GUI now 'knows' about the BIDS file hierarchy. You can open all\n"
   "the datasets from a given subject in a single session, even though BIDS\n"
   "scatters them over several subdirectories. To do this, use the '-bysub'\n"
   "option. See the output of 'afni -help' for the details."
 ,
   "Obscure AFNI GUI buttons:\n"
   " EditEnv = Lets you edit some AFNI environment settings interactively;\n"
   "           useful when you need to change something and don't want\n"
   "           to quit and re-start AFNI. For example, setting\n"
   "           AFNI_LEFT_IS_POSTERIOR will flip the usual Sagittal\n"
   "           image and graph viewers so that the display's left\n"
   "           corresponds to the subject's posterior, rather than\n"
   "           the default anterior.\n"
   " NIML+PO = Starts NIML and Plugout socket listening; useful when you\n"
   "           meant to do one (or both) of these one the command line\n"
   "           (options '-niml' and '-yesplugouts'), but forgot.\n"
   "           For example, NIML is needed for 3dGroupInCorr to connect."
 ,
   "The 'Render Dataset' plugin allows you to do 3D volume rendering\n"
   "in the AFNI GUI, with color overlays, animations, and cutouts.\n"
   "(The SUMA GUI also has a volume rendering mode.)"
 ,
  "Want your picture in the AFNI splash window at startup? Send us a JPEG\n"
  "image, formatted to be square 128x128, and we can include it!\n"
 ,
  "Do you want ALL the AFNI plugins to be visible in the Plugins menu?\n"
  "Set environment variable AFNI_ALLOW_ALL_PLUGINS to YES in your\n"
  ".afnirc file."
 ,
  "Setting environment variable AFNI_GRAPH_ALLOW_SHIFTN to YES in your .afnirc\n"
  "file will allow you to set the graph viewer matrix size directly using\n"
  "keyboard presses, as in\n"
  "  N7<Enter>\n"
  "which will make the graph window have a matrix of 7x7 sub-graphs. It is\n"
  "important to press the <Enter> (or <Return>) key at the end of the digit(s)\n"
  "after N, otherwise the graph window will not respond to any other key presses."
 ,
  "Set environment variable AFNI_STARTUP_SOUND to YES to hear the AFNI startup\n"
  "sound when the GUI opens. Or use the right click popup menu in the logo\n"
  "square right of the 'done' button and select the 'Play startup sound' item.\n"
  " - But whatever you do, DO NOT use the 'Activate Omega-13' menu item!\n"
  " - Sound playing requires the 'sox' software.\n"
  " - To find out if sox is on your system, type the command 'which sox'."
 ,
  "If the 'sox' software is installed on your system, you can play sounds\n"
  "from the AFNI graph viewer window.\n"
  " - Keypress 'p' will play a sequence of tones based on the central sub-graph.\n"
  " - Keypress 'P' will play based on the average of all sub-graphs.\n"
  " - Filtering and detrending the graphs will affect the notes played.\n"
  " - Sound can only be played if you are displaying locally, not remotely.\n"
  " - Environment variable AFNI_SOUND_NOTE_TYPE is used to set the note type:\n"
  "     sine square triangle sawtooth trapezium pluck\n"
  "     ('pluck' sounds halfway between guitar and piano notes)\n"
  " - See README.environment for a few more details.\n"
  " - To find out if sox is on your system, type the command 'which sox'."
 ,
  "Want bigger fonts in AFNI, for a high resolution screen? Set environment\n"
  "variable AFNI_FONTSIZE to BIG (preferably in your .afnirc setup file)."

/*-- tips below here are for non-GUI programs --*/
 ,
   "Questions about AFNI? Problems with a program? Try our Message Board at\n"
   "  https://afni.nimh.nih.gov/afni/community/board/\n"
   "Please be specific and focused, as generic questions without\n"
   "details are very hard to answer well on a Web forum."
 ,
   "If you are doing complicated twisted things with AFNI programs, ASK US\n"
   "(on the message board). Often, there is an easier way to do a task!"
 ,
   "REMEMBER: afni_proc.py is your friend when doing time series analyses!\n"
   "In particular, if you are still using custom hand-written scripts for\n"
   "resting-state preprocessing or time series regression, you need to\n"
   "learn to use afni_proc.py (unless you are doing something unusual)."
 ,
   "Skull stripping T1-weighted datasets? Programs and scripts include:\n"
   " * 3dSkullStrip     - surface expansion program (many options)\n"
   " * @NoisySkullStrip - when the dataset is very noisy\n"
   " * @SSwarper        - nonlinear warping to MNI space combined\n"
   "                      with skull stripping (and then the warp can be\n"
   "                      used in afni_proc.py for time series analyses)"
 ,
   "Program 3dUnifize can make the image intensity of a T1-weighted dataset\n"
   "more uniform in space. As a bonus, can also contrast-invert a dataset\n"
   "prior to the uniform-ization, which might be useful for T2-weighted\n"
   "datasets. It also has an experimental option for processing EPI data."
 ,
   "Program 3dcalc does voxelwise calculations on datasets. Doesn't sound\n"
   "exciting to you? Once you get to know it, you will find that 3dcalc is\n"
   "your FRIEND! (: And then you can get to know the interactive InstaCalc :)"
 ,
   "AFNI has a lot of downloadable demonstrations; you can find them in your\n"
   "abin directory (if that's where AFNI is for you) by doing\n"
   "  ls ~/abin/@Install_*\n"
   "A few examples:\n"
   "  @Install_InstaCorr_Demo = data and instructions for using InstaCorr\n"
   "  @Install_ClustScat_Demo = data and instructions for interactively plotting\n"
   "                            time series extracted from Clusterize ROIs\n"
   "  @Install_FATCAT_DEMO    = data and instructions for using the AFNI FATCAT\n"
   "                            programs for DTI tractography (etc.)"
 ,
   "Program 3drefit can be used to change parameters in a dataset\n"
   "header (e.g., slice timing). Program 3dinfo can be used to\n"
   "display information from a dataset header."
 ,
   "Are you using 3dcalc to compute the average of a bunch of datasets?\n"
   "You should use program 3dMean instead! It is faster and doesn't\n"
   "have the artificial limitation of 26 input datasets."
 ,
   "Want to calculate summary values (e.g., mean, standard deviation)\n"
   "at each voxel in a time series dataset? Program 3dTstat is the\n"
   "tool you need."
 ,
   "Programs for extracting information from spatial ROIs:\n"
   " * 3dmaskave  = get average across the ROI, one value per time point\n"
   " * 3dROIstats = like 3dmaskave, but for multiple ROIs\n"
   " * 3dmaskSVD  = like 3dmaskave, but gives the principal singular\n"
   "                vector (time series) across the ROI instead of the mean\n"
   " * 3dmaskdump = just dumps out ALL the values from the ROI at\n"
   "                all time points (presumably you will do something\n"
   "                fun with these in your own software?)"
 ,
   "Programs for computing some local statistics in a neighborhood around\n"
   "each voxel (e.g., a ball):\n"
   " * 3dLocalstat   = various statistics from the neighborhood\n"
   "                   (e.g., mean, median, variance)\n"
   " * 3dLocalBistat = various 2-sample statistics from the neighborhood,\n"
   "                   calculated between 2 datasets\n"
   "                   (e.g., Pearson or Spearman correlation)\n"
   " * 3dLocalPV     = compute the principal vector (time series) from\n"
   "                   all the dataset time series inside the neighborhood\n"
   "                   (a fancy way to 'smooth' the data)"
 ,
   "Are you using nonlinear warping to align your subjects' T1-weighted\n"
   "datasets? If not, you should give it a try. The brain images will line\n"
   "up better than using affine alignment (3dAllineate, @auto_tlrc, etc.)\n"
   "and you can get better FMRI results at the group level when you use\n"
   "the nonlinear warps in afni_proc.py. Programs for this:\n"
   " * 3dQwarp      = the foundational nonlinear warping program\n"
   " * @SSwarper    = uses 3dQwarp and 3dSkullStrip together to align\n"
   "                  volumes to the MNI template and skull strip them\n"
   " * auto_warp.py = runs 3dQwarp for you, so you don't have to read\n"
   "                  that program's lengthy help output"
 ,
   "Want to create a 'junk' dataset on the command line, just to test to\n"
   "see if something works? AFNI programs can create a dataset in memory\n"
   "from a string; try this example:\n"
   "  afni jRandomDataset:64,64,32,96\n"
   "to create and view a random dataset with 64x64x32 3D volumes, and\n"
   "96 time points. If you want to create and SAVE such a dataset, try\n"
   "  3dcalc -a jRandomDataset:64,64,32,96 -expr 'a' -prefix Fred.nii\n"
   "If you want a zero-filled dataset, replace 'a' with '0'."
 ,
   "Did you know that AFNI's time series analysis program 3dREMLfit can\n"
   "include voxelwise regressors (a different time series for each voxel)?\n"
   "We use this capability in our Anaticor model for de-noising datasets\n"
   "during activation or resting state analyses."
 ,
   "AFNI programs for individual dataset time series correlation-ing:\n"
   " * 3dTcorr1D        = correlate each voxel with a small set of 1D files\n"
   " * 3dTcorrelate     = correlate each voxel between 2 datasets\n"
   " * 3dTcorrMap       = make of map of how correlated each voxel is\n"
   "                      to every other voxel in the same dataset\n"
   " * 3dAutoTcorrelate = correlate each voxel to every other voxel\n"
   "                      in the same dataset and save everything (HUGE)"
 ,
   "Program 3dGroupInCorr can be used for Group Instant Correlation\n"
   "interactively via the AFNI GUI. It can also be used in batch mode.\n"
   "The '-batchGRID' option lets you scan over a grid of seed voxels,\n"
   "compute the individual datasets' correlations with their seeds,\n"
   "then compute the t-tests among these correlation maps, and save\n"
   "the results to a collection of datasets."
 ,
   "Want to convert each statistic in a dataset to a (voxelwise) p-value?\n"
   " Use program 3dPval.\n"
   "More complicated statistical conversions can be done with 3dcalc, using\n"
   " the cdf2stat() and stat2cdf() functions. You can explore those interactively\n"
   " using the ccalc program, to make sure you are giving 3dcalc the correct\n"
   " expression."
 ,
   "Want to test dataset values voxelwise for normality (Gaussianity)?\n"
   "Program 3dNormalityTest will apply the Anderson-Darling test and\n"
   "give you a dataset with the voxelwise measure of non-Gaussianity."
 ,
   "Program 1dCorrelate will compute the pairwise correlation coefficient\n"
   "between 2 columns of numbers, AND give you the bootstrap confidence\n"
   "interval for the result. When you have relatively few samples\n"
   "(say, less than 25), bootstrap confidence intervals are more robust\n"
   "than the standard parametric intervals based on the Gaussian assumption."
 ,
   "Program 1dplot is useful for quick-and-dirty plotting of columns\n"
   "of numbers (.1D files). An example, creating a response model\n"
   "function with 3dDeconvolve and sending the time series directly\n"
   "into 1dplot:\n"
   " 3dDeconvolve -num_stimts 1 -polort -1 -nodata 81 0.5         \\\n"
   "              -stim_times 1 '1D: 0' 'TWOGAMpw(3,6,0.2,10,12)' \\\n"
   "              -x1D stdout: | 1dplot -stdin -THICK -del 0.5"
 ,
   "Program 1dNLfit does a nonlinear fit of an expression with free\n"
   "parameters to a column of numbers. For example:\n"
   " 1dNLfit -depdata sc.1D -indvar x '1D: 100%0:0.1' \\\n"
   "         -expr 'a*sin(b*x)+c*cos(b*x)'            \\\n"
   "         -param a=-2:2 -param b=1:3 -param c=-2:2  > ff.1D\n"
   "fits a sine+cosine model with 3 free parameters (a,b,c) to the\n"
   "data in file sc.1D, where the 'time' parameter is x."
 ,
   "You can use make_random_timing.py to make AFNI-compatible random\n"
   " stimulus timing files. You can impose various constraints on the\n"
   " times generated.\n"
   "You can use timing_tool.py to manipulate stimulus timing files in\n"
   " various ways."
 ,
   "Program 1dTsort lets you sort .1D columns of numbers.\n"
   "Program 3dTsort lets you sort each voxel's time series (separately),\n"
   " and can also randomize them if you want."
 ,
   "Program 'count' will generate lists of numbers, which is surprisingly\n"
   "useful in scripting various things. For example,\n"
   "  count -dig 1 -comma 0 99 S6\n"
   "will produce a list of 9 distinct random numbers from 0..99 (inclusive),\n"
   "separated by commas; for example: '31,18,60,62,7,95'. This list could\n"
   "be used to select a random subset of dataset sub-bricks for analysis\n"
   "  3dttest++ -setA Fred.nii[`count -dig 1 -comma 0 333 S20`]\n"
   "(in the above command, the quotes are the single backquote ` and not\n"
   "the single frontquote ')."
 ,
   "Most AFNI command line programs accept a common set of options, such\n"
   "as sub-brick selectors. See this page for the details:\n"
   "  https://afni.nimh.nih.gov/pub/dist/doc/program_help/common_options.html"
 ,
   "Want to resample a dataset to a different grid spacing? Programs:\n"
   " * 3dresample  = older program with NN, Linear, and Cubic interpolation\n"
   " * 3dAllineate = for aligning datasets and then resampling, but with\n"
   "                 the -1Dparam_apply option can just do the resampling\n"
   "                 function; has more interpolation options, including\n"
   "                 quintic polynomials and tapered sinc.\n"
   " * 3dUpsample  = resamples a dataset to a finer grid in the time\n"
   "                 direction (the other programs change spatial grids)."
 ,
   "Want to blur/smooth a dataset? Programs:\n"
   " * 3dmerge -1blur_fwhm = Gaussian smoothing across whole volume\n"
   " * 3dBlurInMask        = smoothing only inside a mask\n"
   " * 3dLocalPV           = 'smooth' a time series dataset by computing\n"
   "                         the local principal vector around each\n"
   "                         voxel, instead of the average vector (slow)"
 ,
   "Want to create a 3D dataset from a table of numbers? This can be\n"
   "done with program 3dUndump."
 ,
   "Want to slice up a dataset? Or glue datasets together?\n"
   " * 3dZcutup  = cut a section of slices out of a dataset\n"
   " * 3dZcat    = glue datasets together in the slice direction\n"
   " * 3dXYZcat  = glue datasets together in any (spatial) direction\n"
   " * 3dZeropad = add (or subtract) slices to (or from) a dataset\n"
   " * 3dTcat    = glue datasets together in the time direction"
 ,
   "Did someone give you a dataset with the spatial orientation 'flipped',\n"
   "so that the subject's left is marked as being the right? Program\n"
   "3dLRflip can flip the data rows to rearrange the data so the dataset\n"
   "header information and the actual data match again."
 ,
   "Want to write an AFNI '3d' program? If you know C, the code 3dToyProg.c\n"
   "is a good starting point. It shows how to read, calculate, and write\n"
   "datasets, and is heavily commented."
 ,
  "Almost all AFNI command line programs take '-help' as an option; for example\n"
  "  3dTstat -help\n"
  "This method is how you can get the most up-to-date information about using\n"
  "an AFNI program. All '-help' output are also formatted as Web pages and\n"
  "are available here:\n"
  "  https://afni.nimh.nih.gov/afni/doc/program_help/index.html"
 ,
  "Script @grayplot will read the errts (regression residuals) time series\n"
  "datasets from an afni_proc.py results directory, and make PNG-formatted\n"
  "grayplots, partitioned into gray matter, white matter, and CSF segments.\n"
  "The plots are good for looking at the structure of the residuals -- in\n"
  "an ideal world (alas, hard to find), there would be little spatial or\n"
  "temporal structure in the errts datasets, which are the 'noise' from\n"
  "which the variance parts of single subject t/F statistics are computed."

} ;

/*---- goodbye messages here ----*/

#undef  NGBY
#define NGBY ((int)(sizeof(gby)/sizeof(char *)))
static char *gby[] = {

     "Farewell, my friend"                                           ,
     "Farewell?  A long farewell to all my greatness"                ,
     "Sweet is the memory of distant friends"                        ,
     "A memory lasts forever, never does it die - Adieu"             ,
     "Fate ordains that dearest friends must part"                   ,
     "We shall meet again, when the fields are white with daisies"   ,
     "We part as friends, to meet again in some happy hour"          ,
     "Parting is such sweet sorrow"                                  ,
     "Gone, and a cloud in my heart"                                 ,
     "Happy trails to you, until we meet again"                      ,
     "Only in the agony of parting do we see the depths of love"     ,
     "Goodbye isn't painful, unless we'll never say hello again"     ,
     "The pain of parting is nothing to the joy of meeting again"    ,
     "Be well, do good work, and keep in touch"                      ,
     "In the hope to meet shortly again"                             ,
     "May the wind be ever at your back"                             ,
     "Fare thee well, and if forever, still forever fare thee well"  ,
     "Don't cry because it's over; smile because it happened"        ,
     "Farewell! Thou art too dear for my possessing"                 ,
     "Farewell, farewell, you old rhinoceros"                        ,
     "Is that you, Jerzy? Do widzenia"                               ,
     "A farewell is necessary before we can meet again"              ,
     "Absent from thee I languish; return speedily if thee can"      ,
     "The return makes one love the farewell"                        ,
     "Every goodbye makes the next hello closer"                     ,
     "The song is ended, but the melody lingers on"                  ,
     "A star will shine upon the hour of our next meeting"           ,
     "May we meet again in happier times"                            ,
     "Adieu, auf Wiedersehen, Adios, Cheerio, and Bon Voyage"        ,
     "Ta ta, Hooroo, Catch ya 'round"                                ,
     "Meeting again is certain for those who are friends"            ,
     "Au revoir, Ciao, Ma'alsalam, Hasta luego, Czesc, and Zai jian" ,
     "Don't cry -- a farewell is necessary before we can meet again" ,
     "We part, but only to meet again"                               ,
     "How lucky I am to have someone that makes saying goodbye hard" ,
     "Goodbyes are not forever"                                      ,
     "Dearest friends, alas, must part"                              ,
     "True goodbyes are the ones never said or explained"            ,
     "Let the party begin"                                           ,
     "Let us cross over the river and rest on the other side"        ,
     "Good night, Mrs Calabash, wherever you are"                    ,
     "Onen i Estel Edain, u-chebin estel anim"                       ,
     "I will not say 'do not weep', for not all tears are an evil"   ,
     "Calo anor na ven -- May the sun shine upon your road"          ,
     "Little by little, one travels far"                             ,
     "Beyond all hope, set free to light"                            ,
     "Divide By Cucumber Error; Please Reinstall Universe and Reboot",
     "Please re-inflate the multiverse and try again later"          ,
     "Out of Cheese Error; Please Install Wensleydale and Try Again" ,
     "Out of Cheese Error; Please Install Stilton and Try Again"     ,
     "Out of Wine Error: Please Install Merlot and Try Again"        ,
     "Out of Wine Error: Please Install Chardonnay and Try Again"    ,
     "Out of Beer Error: No Further Progress Can Be Expected"        ,
     "Out of Benedictine Error: How do you expect me get work done?" ,
     "More cheese, Gromit!"                                          ,
     "Life can be tough sometimes -- so have a chocolate (or two)"   ,
     "Sweet sweet caffeine -- is there anything it can't do?"        ,
     "If at first you don't succeed -- call it version 1.0"          ,
     "Never trust a statistic you haven't faked yourself"            ,
     "May your teeth never be replaced by damp woolen socks"         ,
     "Hasta la vista, Au revoir, and so long for now"                ,
     "Farewell, and may an elephant never sit on your computer"      ,
     "Ta ta, and may an elephant caress you gently with his toes"    ,
     "So long, and may the bluebird of happiness fly up your nose"   ,
     "The Square Root of -1 said to Pi, 'Be Rational'"               ,
     "Pi told the Square Root of -1 to 'Get Real'"                   ,
     "And the world begins to tremble"                               ,
     "By Grapthar's Hammer, you SHALL be avenged"                    ,
     "We live to tell the tale"                                      ,
     "Are we there yet?"                                             ,
     "Never give up, never surrender"                                ,
     "Mathesar, Activate the Omega-13!"                              ,
     "No time for pleasantries, Kyle; we have a Level 5 emergency!"  ,
     "Digitize me, Fred"                                             ,
     "Well, nobody's perfect"                                        ,
     "Drink to me only with thine eyes, and I will drink with mine"  ,
     "O Captain, My Captain, rise up and hear the bells"             ,
     "O Captain, My Captain, our fearful trip is done"               ,
     "I recall the experience sweet and sad"                         ,
     "Ever returning spring, trinity sure to me you bring"           ,
     "If thou wast not grant to sing, thou would'st surely die"      ,
     "Here, user that slowly passes, I give you my sprig of lilac"   ,
     "What a long strange trip it's been"                            ,
     "Sometime the light shines on me, other times I can barely see" ,
     "When life looks like Easy Street, there is danger at your door",
     "Like the morning sun I come, like the wind I go"               ,
     "What I want to know is, where does the time go?"               ,
     "Every silver lining's got a touch of grey"                     ,
     "A friend of the devil is a friend of mine"                     ,
     "Well, I ain't often right, but I never been wrong"             ,
     "If the horse don't pull, you got to carry the load"            ,
     "Hang it up and see what tomorrow brings"                       ,
     "The flower that once has blown, for ever dies"                 ,
     "Drink! for you know not why you go, or where"                  ,
     "Tomorrow we feast with us at home"                             ,
     "Forgive your enemies; but never forget their names"            ,
     "A friend is one who has the same enemies as you have"          ,
     "Am I not destroying my enemies when I make friends of them?"   ,
     "True friends will stab you in the FRONT"                       ,
     "I am so clever I don't understand a word of what I'm saying"   ,
     "Some cause happiness wherever they go; others whenever they go",
     "A man who does not think for himself does not think at all"    ,
     "Whenever people agree with me, I think I must be wrong"        ,
     "We are each our own devil, and make this world our hell"       ,
     "I have nothing to declare except my genius"                    ,
     "In matters of opinion, all my adversaries are insane"          ,
     "The fewer the facts, the stronger the opinions"                ,
     "Research is what I'm doing when I don't know what I'm doing"   ,
     "Everything is a matter of opinion: mine matters, yours doesn't",
     "It's not a phase, it's a lifestyle"                            ,
     "Go to Heaven for the climate, Hell for the company"            ,
     "Am I the crazy one, or is it everyone else on Earth?"          ,
     "Everyone's crazy but you and me (and I'm not sure about you)"  ,
     "We live in crazy times"                                        ,
     "Just because your idea is crazy does not mean it is wrong"     ,
     "There are no facts, only interpretations (at p < 0.05 level)"  ,
     "The best weapon against an enemy is another enemy"             ,
     "Paris is always a good idea"                                   ,
     "A good decision is based on knowledge, not on numbers"         ,
     "If you can't get good results, at least make them LOOK good"   ,
     "If everyone is thinking alike, then somebody isn't thinking"   ,
     "Be sure to put your feet in the right place, then stand firm"  ,
     "May your cupcakes always have lots of rich creamy frosting"    ,
     "Never take a chocolate cupcake from an eel"                    ,
     "Is it time to give your moose a bubble bath?"                  ,
     "Do you prefer white chocolate or dark chocolate?"              ,
     "Is it lunchtime yet?"                                          ,
     "Meet me down the pub later"                                    ,
     "Let's blow this place and grab us some vino"                   ,
     "Let's blow this place and grab some brewskis"                  ,
     "Are you ready for a coffee break? I am"                        ,
     "I'd like a strong cup of Lapsang Souchong about now"           ,
     "Make mine a tall skinny Earl Grey vanilla latte, if you please",
     "What's your favorite ice cream? I like French vanilla"         ,
     "What's your favorite kind of bagel? I like pumpernickel"       ,
     "What's your favorite kind of cookie? I like white chocolate"   ,
     "What's your favorite kind of cake? I like chocolate"           ,
     "Do you like chardonnay? I do"                                  ,
     "Step slowly away from the keyboard, and remain calm"           ,
     "Put your computer's mouse down slowly, and breathe deeply"     ,
     "Time for a nice walk, don't you think?"                        ,
     "Meet me at the Leshan Dafo in Sichuan at 3pm next Wednesday"   ,
     "Let's meet at the Xuankong Si in Shanxi on Thursday week"      ,
     "Meet me the Namche Bazaar gompa next Thursday"                 ,
     "Meet me at Dashashwamedh Ghat in Varanasi for Agni puja"       ,
     "Meet me at the top of Renjo La in the next snowstorm"          ,
     "See you in Dingboche next Christmas"                           ,
     "I'll see you at Angkor Wat at midnight next Saturday"          ,
     "Buy property on Neptune now, and avoid the rush"               ,
     "NEVER buy a 3 humped camel in Samarkand"                       ,
     "Never buy a 7 hump Wump from Gump"                             ,
     "May the odds be ever in your favor"                            ,
     "I weep for Adonais -- he is dead! Oh, weep for Adonais"        ,
     "An echo and a light unto eternity"                             ,
     "Did I mention that we're doomed? Horribly horribly doomed?"    ,
     "I could go for some momos right now, how about you?"           ,
     "I really like woh for a filling dinner, don't you?"            ,
     "Dal bhat power, 24 hour"                                       ,
     "Hodor Hodor Hodor Hodor"                                       ,
     "I wake and feel the fell of dark, not day"                     ,
     "Love all, trust a few, do wrong to none"                       ,
     "The wheel is come full circle"                                 ,
     "Brains and AFNI make the hours seem short"                     ,
     "I am not bound to please thee with my statistics"              ,
     "I will praise any man that will praise me"                     ,
     "If you have tears, prepare to shed them now"                   ,
     "Man, those solar neutrinos are killing me"                     ,
     "Are you ready for the explosion of Eta Carinae?"               ,
     "He who will deceive will always find a willing victim"         ,
     "How quick come the reasons for approving what we like"         ,
     "This is your only chance at building a disreputable past"      ,
     "O Brave New World, that has such software in it"               ,
     "When I ask for advice, what I really want is an accomplice"    ,
     "Above all -- Don't let your brain lie to you"                  ,
     "I like nonsense -- it shakes the neurons out of their naps"    ,
     "I'm glad you finished up now -- I'm ready for a quick nap"     ,
     "Using it is like going to the gym for your brain"              ,
     "We are all mad here"                                           ,
     "Working for improved brain-ology not just 24/7 but 25/8!"      ,
     "If Ziad were here, we'd be going for gelato just about now"    ,
     "Trust me, I know what I'm doing"                               ,
     "Never tell me the odds"                                        ,
     "If you're good at something, never do it for free"             ,
     "I'm Spartacus"                                                 ,
     "Abandon all hope, ye who leave here"                           ,
     "From a little spark may burst a flame"                         ,
     "I love to doubt as well as to know"                            ,
     "At this moment, ability fails my capacity to describe"         ,
     "I'll miss you -- come back soon"                               ,
     "Be careful out there"                                          ,
     "Yesterday, all my troubles seemed so far away"                 ,
     "Stochastic delights have deterministic ends"                   ,
     "Remember -- The laws of physics always win"                    ,
     "Remember -- To prolong doubt is to prolong hope"               ,
     "Remember -- Time and tide wait for no brain imaging software"  ,
     "Remember -- AFNI is free, but worth at least 1000 times more"  ,
     "Remember -- Nothing is always absolutely so"                   ,
     "Remember -- 90% of everything is cr*p"                         ,
     "Remember -- Good things always take longer than you expect"    ,
     "Remember -- 'New and Improved' is neither"                     ,
     "Remember -- Murphy was an optimist"                            ,
     "Remember -- Statistics are no substitute for judgment"         ,
     "Remember -- A thing can be true, and still be desperate folly" ,
     "Remember -- Aquaman cares"                                     ,
     "Remember -- She who laughs, lasts"                             ,
     "Remember -- He who laughs, lasts"                              ,
     "Remember -- The innocent have everything to fear"              ,
     "Remember -- Memory is long but time is tricky"                 ,
     "Remember -- Men are always willing to believe what they wish"  ,
     "Remember -- What I tell you three times is true"               ,
     "Remember -- A monad is the same as an endofunctor"             ,
     "Remember -- Things aren't always what they seem"               ,
     "Remember -- eggs cannot be unscrambled"                        ,
     "Remember -- a closed mouth gathers no feet"                    ,
     "Fools give you reasons, wise men never try"                    ,
     "People willingly trust the statistics they wish to believe"    ,
     "Heaven's last best gift, my ever new delight"                  ,
     "Long is the way and hard, that out of Data leads to Light"     ,
     "They also serve, who only stand and process data"              ,
     "Farewell happy software, where joy forever dwells"             ,
     "He who destroys a good book, destroys reason itself"           ,
     "Wild above rule or art, enormous bliss"                        ,
     "Yet from those flames no light, but rather darkness visible"   ,
     "Think of all the beauty around you, and be happy"              ,
     "Experience is a hard teacher, but fools will have no other"    ,
     "By failing to prepare, you are preparing to fail"              ,
     "We are all born ignorant, but must work hard to remain stupid" ,
     "Whatever is begun in anger ends in shame"                      ,
     "Life's tragedy is that we get old too soon and wise too late"  ,
     "I didn't fail the test, I just found 100 ways to do it wrong"  ,
     "Wise men don't need advice; fools won't take it"               ,
     "Half a truth is often a great lie"                             ,
     "Will you help 'Make AFNI Great Again'?"                        ,
     "If I can find the man calling me ruthless, I'll destroy him"   ,
     "'It remains to be seen' == 'When pigs fly'"                    ,
     "Do not scorn pity that is the gift of a gentle heart"          ,
     "Do not go gentle into that good abend"                         ,
     "The best laid statistics of mice and men gang aft agley"       ,
     "A thousand farewells pass in one moment"                       ,
     "Did you see hyperconnectivity in the disconnected fibers?"     ,
     "Out out, brief candle"                                         ,
     "A poor player that struts and frets its hour upon the screen"  ,
     "Is it just me, or is gravity extra strong today?"              ,
     "All this Dark Matter whizzing around makes it hard to think"   ,
     "Thank you so so so very much"                                  ,
     "Will you miss me?"                                             ,
     "It wasn't me that did it. It was my brain"                     ,
     "Was there life before Google?"                                 ,
     "If you can't be good, be careful"                              ,
     "What sweet madness has seized me?"                             ,
     "Which is more accurate: Haruspicy or Statistical Inference?"   ,
     "Forgive your enemy -- but remember the bastard's name"         ,
     "If the facts don't fit the theory, change the facts"           ,
     "All generalizations are false: including this one"             ,
     "Facts are stubborn, but statistics are pliable"                ,
     "I can prove anything with statistics, except the truth"        ,
     "The chief function of the body is to carry the brain around"   ,
     "The chief function of the brain is to hold the ears apart"     ,
     "Humor is the most significant activity of the human brain"     ,
     "Tears are not a sign of weakness, but a sign of a pure heart"  ,
     "Eventually, everything goes away"                              ,
     "You never know what you had until you've lost it"              ,
     "Gone. The saddest word in any language"                        ,
     "An ounce of practice is worth more than a ton of preaching"    ,
     "Even if you a minority of one, the truth is still the truth"   ,
     "Money talks, but usually just to say 'Goodbye'"                ,
     "Are you a Bayesian Heretic or a Frequentist True Believer?"    ,
     "Please tell me you don't believe in Fuzzy Logic"               ,
     "Are you ready for the Big Rip?"                                ,
     "I hereby declare the Null Hypothesis to be ..... Falsified"    ,
     "I'm sick of thinking about p-values -- how about you?"         ,
     "To (mis)quote Han Solo: Never tell me the p-value!"            ,
     "Did you fail to negate the opposite of the null hypothesis?"   ,
     "I'd like to live as a poor man with lots of money"             ,
     "Wine is proof that God loves us and wants to see us happy"     ,
     "If two wrongs don't make a right, then try three; then four"   ,
     "Life is hard; after all, it kills you"                         ,
     "I'm sorry; if you were right, I'd agree with you"              ,
     "Like dreams, statistics are a form of wish fulfillment"        ,
     "I wish I were in Lobuche right now, eating momos"              ,
     "A thermos of hot tea in Pangboche would be a nice pit stop"    ,
     "Next stop: Bora Bora and Rangiroa"                             ,
     "Do you still miss the NIH Bear? I do"                          ,
     "Always be patient with the rich and powerful"                  ,
     "Better to visit hell in your lifetime than afterwards"         ,
     "Halfway is 12 miles, when you are on a 14 mile hike"           ,
     "How beautiful it is to do nothing, then rest afterwards"       ,
     "When the sky falls, hold up your hands"                        ,
     "If you can't bite, don't show your teeth"                      ,
     "Three statisticians ==> Four opinions on data analysis"        ,
     "A fool and his p-value are soon non-replicated"                ,
     "What do you do all day? I do very little, and do it slowly"    ,
     "Did you find a paradigm shift today?"                          ,
     "Was it the silver bullet you were hoping for?"                 ,
     "Why is 'gold' the standard for data analysis, anyway?"         ,
     "Did you find the Holy Grail of neuroimaging yet?"              ,
     "Don't you wish it had a 'Write Nature Paper' button?"          ,
     "Coming REAL soon: the 'Write Science Paper' interface"         ,
     "And flights of angels sing thee to thy rest"                   ,
     "Hast seen the White Whale?"                                    ,
     "Our sweetest songs are those that tell of saddest thought"     ,
     "The more we study, the more we discover our ignorance"         ,
     "Nothing wilts faster than laurels that have been rested upon"  ,
     "Fear not for the future; weep not for the past"                ,
     "AFNI, when soft images fade, vibrates in the memory forever"   ,
     "Nothing ever becomes real until it is experienced"             ,
     "No bird soars too high if he soars with his own wings"         ,
     "Great things are done when men and mountains meet"             ,
     "A fool sees not the same tree that a wise man sees"            ,
     "What is now proved was once only imagined"                     ,
     "It is easier to forgive an enemy than to forgive a friend"     ,
     "The true method of knowledge is experiment"                    ,
     "The flower that smells the sweetest is shy and lowly"          ,
     "He knows not his own strength, that has not met adversity"     ,
     "Weigh the meaning, and look not at the words"                  ,
     "Statistics are no substitute for judgment"                     ,
     "There's never enough time to do all the nothing you want"      ,
     "When life gives you lemons, throw them right back at it"       ,
     "Happiness isn't good enough for me; I demand euphoria"         ,
     "Judge a person by her questions, rather than her answers"      ,
     "I have not failed; I've just found 10,000 ways that don't work",
     "Statistics are good, but dark chocolate is better"             ,
     "Espresso chocolate -- mmmmmm -- good"                          ,
     "After every tempest comes the calm"                            ,
     "I've got MY story about the brain; what's yours?"              ,
     "I came, I saw, I got confused"                                 ,
     "Computers are useless -- they can only give you answers"       ,
     "If nothing else, this software is a great toy"                 ,
     "Remember to take your brain out and polish it"                 ,
     "What in God's Holy Name are you blathering on about?"          ,
     "Are you a Dada-ist or a Dude-ist?"                             ,
     "Believe those who seek the truth; doubt those who find it"     ,
     "There is more to truth than just the facts"                    ,
     "There is more to truth than a small p-value"                   ,
     "If you think you are free, no escape is possible"              ,
     "If you chase two rabbits at once, you will not catch either"   ,
     "It is better to know the questions than the answers"           ,
     "Inventing Hell is easy, but inventing Heaven impossible"       ,
     "When you are climbing the ladder, do not forget the rungs"     ,
     "You have to do it yourself, but you cannot do it alone"        ,
     "When you look into the abyss, the abyss looks back at you"     ,
     "Just say NO -- to arbitrary p-value thresholds"                ,
     "Did you have fun with your data? I had fun showing it to you"  ,
     "Are you ready to drink from the Big Data fire hose?"           ,
     "In God we trust; all others must have Big Data"                ,
     "Torment the data enough and it will tell you anything you want",
     "p-hacking? Bah -- I'll take a chainsaw to your p-values"       ,
     "Did you like your p-values? If not, I can 'fix' them for you"  ,
     "Honesty is the best policy, but insanity is a better defense"  ,
     "A desk is a dangerous place from which to view the world"      ,
     "If you can't be kind, at least be vague"                       ,
     "Give into temptation; it might not come again"                 ,
     "Insanity is my best and only means of relaxation"              ,
     "The three 'Ups' of life: Grow Up, Shut Up, Lighten Up"         ,
     "I am not a hexadecimal number, I am a free software!"          ,
     "Correlation isn't causation, but what else do we have?"        ,
     "Are you indeed there, my skylark?"                             ,
     "Life is an experiment; please reject the dull hypothesis"      ,
     "Hard work pays off in the future, but laziness pays off now"   ,
     "We learn from our mistakes -- I've learned a lot today"        ,
     "If a turtle loses its shell, is it naked or homeless?"         ,
     "There is no mistake so great as being right ahead of time"     ,
     "We give you tools; You build things with them"                 ,
     "Do not take life too seriously. You won't get out of it alive" ,
     "For great justice. Move ZIG."                                  ,

     /* bastardizations of Shakespeare */

     "No longer mourn for me when I am crashed"                      ,
     "If you read this line, remember not the bits that rendered it" ,
     "Not from the stars do I my statisticks pluck"                  ,
     "No more be grieved at that which thou hast computed"           ,
     "Is it thy will thy brain image should stay open?"              ,

     /* Innocence */

     "I'm completely innocent. Within reason"                          ,
     "I'm completely innocent. I was just doing what they told me"     ,
     "I'm completely innocent. It was someone else who looked like me" ,
     "I'm completely innocent. Or at least, you can't prove anything"  ,
     "I'm completely innocent. Of what, I'm not saying"                ,
     "I'm completely innocent. What are we talking about?"             ,

     /* paraprosdokians */

     "If I agreed with you, we'd both be wrong"                           ,
     "I didn't say it was your fault; I said I was blaming you"           ,
     "I used to be indecisive, but now I'm not so sure"                   ,
     "You're never too old to learn something stupid"                     ,
     "Borrow money from a pessimist; he won't expect it back"             ,
     "I used to be conceited, but now I'm perfect"                        ,
     "We never really grow up; we only learn how to act in public"        ,
     "I didn't say it was your fault; I said I was blaming you"           ,
     "Money can't buy happiness, but it makes misery easier to live with" ,

     /* self referential */

     "The 'Lead Standard' for neuroimaging since 1994"               ,
     "Shedding new light on the brain since 1994"                    ,
     "Brain-ology at the cutting edge since 1994"                    ,
     "Putting the 'wit' in 'twit' since 1994"                        ,
     "Confusing neuroscientists successfully since 1994"             ,

     "Returning control of your brain (images) back to yourself"     ,
     "Returning your endofunctors back to their co-monads"           ,
     "Returning you from brain-blob land to actual thinking land"    ,

     /* This set of quotes is from Paradise Lost,
        by John Milton (a very Very early AFNI user) */

     "With hideous ruin and combustion, down to bottomless perdition"                    ,
     "The mind and spirit remains invincible"                                            ,
     "The thought both of lost happiness and lasting pain"                               ,
     "Still clothed with transcendent brightness"                                        ,
     "All is not lost: the unconquerable will, and courage never to submit or yield"     ,
     "Too well I see and rue the dire event that hath lost us Heaven"                    ,
     "Happy state here swallowed up in endless misery"                                   ,
     "What reinforcement we may gain from hope, if now what resolution from despair"     ,
     "Farewell happy fields where Joy for ever dwells"                                   ,
     "The mind is its own place, and itself can make a Heaven of Hell, a Hell of Heaven" ,
     "No light, but rather darkness visible"                                             ,
     "Find yourself not lost in loss itself"                                             ,
     "Through the gloom were seen ten thousand banners rise in the air"                  ,
     "Let tears such as angels weep burst forth"                                         ,
     "To set itself in glory above its peers"                                            ,
     "Hurled headlong flaming from the ethereal sky"                                     ,
     "Here in the heart of Hell to work in fire"                                         ,
     "Ceases now to bellow through the vast and boundless deep"                          ,
     "The seat of desolation, void of light"                                             ,
     "Left at large to its own dark designs"                                             ,
     "Whom reason has equalled, force has made supreme above his equals"                 ,
     "Resume new courage and revive"                                                     ,
     "After the toil of battle, repose your wearied virtue"                              ,
     "From eternal splendours flung"                                                     ,
     "Long is the way and hard, that out of Hell leads up to light"                      ,
     "Wild above rule or art, enormous bliss"                                            ,
     "Of what darkness do we dread?"                                                     ,
     "Free and to none accountable"                                                      ,
     "Designing or exhorting glorious statisticks"                                       ,
     "Those thoughts that wander through eternity"                                       ,
     "Now fiercer by despair"                                                            ,
     "Celestial Virtues rising will appear"                                              ,
     "Fit to bear the weight of mightiest monarchies"                                    ,
     "This horror will grow mild, this darkness will light"                              ,
     "Whose eye views all things at one view"                                            ,
     "Thus uplifted high beyond hope"                                                    ,
     "Returning you to the dark illimitable ocean without bound"                         ,
     "With thoughts inflamed of highest design"                                          ,
     "Flying far off into a Limbo large and broad"                                       ,
     "Ascending by degrees magnificent"                                                  ,

     /* adapted from Marcus Aurelius */

     "Don't try to be in the majority, but try not to be in the ranks of the insane"       ,
     "Everything we hear is an opinion, not a fact"                                        ,
     "Everything we see is a perspective, not the truth"                                   ,
     "The best revenge is not to be like your enemy"                                       ,
     "The happiness of your life depends upon the quality of your thoughts"                ,
     "How much more grievous are the consequences of anger than the causes of it"          ,
     "We love ourselves more than others, but care more about their opinions than our own" ,

     /* From Seneca */

     "Even the lowly can earn true praise; only the strong can receive false praise"  ,
     "Luck is a matter of preparation meeting opportunity"                            ,
     "If you don't know where you are sailing, no wind will be favorable"             ,
     "The greatest remedy for anger is delay"                                         ,
     "It is more fitting to laugh at life than to lament over it"                     ,

     /* From Phaedo, by Plato (a very Very VERY early AFNI user) */

     "Is not existence revealed to us in thought, if at all?"                            ,
     "Is there or is there not an absolute justice?"                                     ,
     "Is there an absolute beauty and absolute good?"                                    ,
     "Attains to knowledge in highest purity with mind (and statistics) alone"           ,
     "The wise person will want to be ever with her who is better than himself"          ,
     "There is no greater evil one can suffer than to hate reasonable discourse"         ,

     /* These are to make it clear that Cox is not to be blamed for ANYTHING */

     "If you have any problems with AFNI, blame goes to ... Mike Beauchamp :)" ,
     "If you have any problems with AFNI, blame goes to ... Ziad Saad :)"      ,
     "If you have any problems with AFNI, blame goes to ... Pat Bellgowan :)"  ,
     "If you have any problems with AFNI, blame goes to ... Kyle Simmons :)"   ,
     "If you have any problems with AFNI, blame goes to ... Jerzy Bodurka :)"  ,
     "All suspicion points to a Frost-Bellgowan plot"                          ,
     "All signs points to a Frost-Bellgowan conspiracy"                        ,
     "If you have any questions about AFNI, ask ... Daniel Glen :)"            ,
     "If you have any questions about AFNI, ask ... Rick Reynolds :)"          ,
     "If you have any questions about AFNI, ask ... Paul Taylor :)"            ,
     "If you have any questions about AFNI, ask ... Gang Chen :)"              ,
     "If you have any questions about AFNI, ask ... Justin Rajendra :)"        ,
     "AFNI user's mantra: Bob, Bob, there is one Bob, He spells it B-O-B"      ,

     /* The Manchurian Candidate */

     "The kindest, bravest, warmest, most wonderful software you've ever used" ,
     "Your brains have not only been washed, but have been dry cleaned"        ,
     "Why don't you pass the time by playing a little solitaire?"              ,

     /* Carrie Fisher */

     "Resentment is like drinking poison and waiting for the other person to die" ,
     "I'm not happy about getting old, but what are the options?"                 ,
     "I'm very sane about how crazy I am"                                         ,
     "Instant gratification takes too long"                                       ,

     /* Oscar Wilde */

     "We are all in the gutter, but some of us are looking at the stars"        ,
     "Always forgive your enemies - nothing annoys them so much"                ,
     "Experience is the name men give to their mistakes"                        ,
     "The truth is rarely pure and never simple"                                ,
     "Be yourself; everyone else is already taken"                              ,
     "I have simple tastes: I am easily satisfied with the best"                ,
     "Remember -- Everything popular is wrong"                                  ,
     "Experience is one thing you can't get for nothing"                        ,
     "A thing is not necessarily true because a man dies for it"                ,
     "Moderation is fatal - nothing succeeds like excess"                       ,
     "The world is a stage - but the play is badly cast"                        ,
     "An idea that is not dangerous is unworthy of being called an idea at all" ,

     /* Longer quotes */

     "It is a poem in our eyes, its ample analyses dazzle the imagination"            ,
     "Do not go where the path leads; go instead where there is no path"              ,
     "Be yourself, in a world that is always trying to make you something else"       ,
     "Beauty is the handwriting of God"                                               ,
     "Glory lies not in never failing, but in rising every time we fail"              ,
     "A hero is no braver than others: she is just braver 5 minutes longer"           ,
     "It is not length of life, but depth of life, that matters"                      ,
     "Foolish consistency is the hobgoblin of little minds"                           ,
     "The years teach much that the days never know"                                  ,
     "Keep your face to the sunshine, and the shadows will fall behind you"           ,
     "Be curious, but not judgmental"                                                 ,
     "And my very code shall be a great poem"                                         ,
     "Either define the moment, or the moment will define you"                        ,
     "Let your soul stand cool and composed before a million universes"               ,
     "I cannot travel the road for you; you must travel it by yourself"               ,
     "The truth is simple. If it was complicated, everyone would understand it"       ,
     "I hate, commas, in the wrong, place"                                            ,
     "The only true wisdom is in knowing you know nothing"                            ,
     "There is no harm in repeating a good thing! There is no harm in ...."           ,
     "It is in our darkest moments that we must focus to see the light"               ,
     "Dignity does not consist of possessing honors, but in deserving them"           ,
     "What is a billion years, when compared to the lifespan of AFNI?"                ,
     "In a billion years, the sun gets so hot Earth will be fried. Are you ready?"    ,

     "I look to that which is, and beyond, to that which will ever be"                ,
     "To steal ideas from one person is plagiarism; to steal from many is research"   ,
     "The early bird gets the worm, but the second mouse gets the cheese"             ,
     "I believe in giving everybody a fair and equal chance to foul things up"        ,
     "Remember -- at least half of all the brains on Earth belong to women"           ,
     "When human judgment and big data interact, peculiar things happen"              ,
     "FMRI is at best like reading source code with blurring goggles over your eyes"  ,
     "Do you prefer red blobs or blue blobs? That's the real FMRI question"           ,
     "I wish we had a taste interface -- I'd make my blobs cherry-chocolate flavor"   ,
     "We cannot solve our problems with the same thinking that created them"          ,
     "My brain starts working when I wake up, and stops when I have to give a talk"   ,
     "Biology gives you a brain. Life turns it into a mind"                           ,
     "Your thoughts, your actions, your experiences, are the sculptor of your brain"  ,
     "Why isn't there an award for getting dressed and out of the house?"             ,
     "I like the word 'indolence': it makes my laziness seem classy"                  ,
     "Laziness is just the habit of resting before you get tired"                     ,
     "Laziness takes work and it isn't easy, but look at the rewards!"                ,
     "A practical truth: no man has eaten an entire elephant in one day"              ,
     "From now on, let's just reject the null hypothesis, and then have a beer"       ,
     "Home is the sailor, home from the sea; And the hunter home from the hill"       ,
     "Glad did I run, and gladly end, and I turn me off with a will"                  ,
     "I'm not as smart as you, but I'm not as dumb as you think I am"                 ,
     "If you haven't anything nice to say about anybody, come sit next to me"         ,
     "Hmmm -- I think your p-value is 0.050001 -- better luck next time"              ,
     "Wow! Your p-value is 0.049999 -- you are incredibly lucky"                      ,
     "Are you a special snowflake, or a normal cloddish lump of ice?"                 ,
     "Remember -- You are absolutely incredibly unique. Just like everone else"       ,
     "Remember -- Belief is not Truth. No matter how much you want it to be"          ,
     "Remember -- Truth is not always believed, even when it is under your nose"      ,
     "Remember -- Screaming is the next best thing to solving a problem"              ,
     "Remember -- Swearing is almost as good as solving a problem"                    ,
     "Remember -- Closure operators are monads on preorder categories"                ,
     "Remember -- There is more to life than getting a small p-value"                 ,
     "Always read something that will make you look good if you die in the middle"    ,
     "When I die, I hope to go to Heaven -- wherever the Hell that is"                ,
     "How about you and me climb Mt Belford next weekend? Meet me at the trailhead"   ,
     "The difference between stupidity and genius is that genius has its limits"      ,
     "If we knew what we are doing, it wouldn't be called research, would it?"        ,
     "Mathematics is the only place where truth and beauty mean the same thing"       ,
     "I may be going to hell in a bucket, but at least I'm enjoying the ride"         ,
     "Next time, just for fun, I'll toss in some extra blobs in CSF for you"          ,
     "Next time, just for fun, I'll toss in some extra blobs in air just for you"     ,
     "What do you mean, you don't believe all those clusters in white matter?"        ,
     "What do you mean, you don't believe all those clusters in empty space?"         ,
     "Those results scream 'ARTIFACT' to me, but what do I know?"                     ,
     "For an extra pumpernickel bagel, I'll put a blob wherever you want it"          ,
     "For a Torcik Wedlowski, I'll colorize TWO extra regions for you -- anywhere"    ,
     "I don't know about you, but my amygdala is lighting up like it's on fire"       ,
     "My hippocampus stopped working years ago -- what did you say?"                  ,
     "Will all great Neptune's ocean wash this modeling error from my regression?"    ,
     "Data which passes through so many steps can hardly have much truth left"        ,
     "One man's way may be as good as another's, but we all like our own best"        ,
     "Some ideas are so wrong that only an intelligent person could believe them"     ,
     "Life's a lot more fun when you aren't responsible for your actions"             ,
     "I'm not dumb. I just have command of thoroughly useless algorithms"             ,
     "A software's reach should exceed its CPU, or what's a supercomputer for?"       ,
     "There are 2 kinds of statistics: those you compute and those you just make up"  ,
     "It is the mark of a truly intelligent person to be moved by statistics"         ,
     "Dreams are true while they last, and do we not live in dreams?"                 ,
     "Have you made your long term (trillion year) research plan yet? Get busy now"   ,
     "Why is 'Gold Standard' used in science? Gold is pretty but almost useless"      ,
     "Oh well, you can always end your paper with 'Further research needed'"          ,
     "It's not true my youth was wild and crazy -- only half of that is true"         ,
     "Your theory is crazy, just not crazy enough to be true"                         ,
     "Not yet quite as powerful as the totalized and integrated mind of Arisia"       ,
     "Are you testing for the Dull Hypothesis? It's never significant"                ,
     "For every complex problem there is an answer that is clear, simple, and wrong"  ,
     "For every simple problem there is an answer that is murky, complex, and wrong"  ,
     "If something is 'New and Improved', was the last version 'Old and Decrepit'?"   ,
     "The important things about a statistical model are what it does NOT include"    ,
     "You're going to like the way your brain activation maps look -- I guarantee it" ,
     "A p-value of 0.05 means the null hypothesis is 29% likely to be correct"        ,
     "There are lots of people who mistake their imagination for their memory"        ,
     "I'm off to get some hot chocolate in Warszawa -- want to join me?"              ,
     "Money can't buy happiness -- but I'm willing to give it a fair chance"          ,
     "In ancient times, there were no statistics, so they just had to lie"            ,
     "If your experiment needs statistics, you need a better experiment"              ,
     "Wirth's law -- software gets slower quicker than hardware gets faster"          ,
     "How wouldst thou worst, I wonder, than thou dost, defeat, thwart me?"           ,
     "O the mind, mind has mountains, cliffs of fall frightful"                       ,
     "All life death does end and each day dies with sleep"                           ,
     "Let me be fell, force I must be brief"                                          ,
     "Meet me at the Torre Pendente di Pisa on the feast of St Rainerius"             ,
     "One martini is just right; two is too many; three is never enough"              ,
     "Martinis -- not just for breakfast anymore"                                     ,
     "If you can't explain it simply, you don't understand it well enough"            ,
     "Even the Universe bends back on itself, but stupidity goes on forever"          ,
     "Get your statistics first, then you can distort them as you please"             ,
     "A man who carries a cat by the tail learns something he can learn no other way" ,
     "Three things cannot long be hidden: the Sun, the Moon, and the Truth"           ,
     "The truth does not change, just because you don't want to hear it"              ,
     "Everything we see is a perspective, not the truth"                              ,
     "The truth will set you free, but first it will make you miserable"              ,
     "We live in a world of illusion; the great task of life is to find reality"      ,
     "Better than a thousand hollow words is one word that brings peace"              ,
     "May the Dark Side of the Force get lost on the way to your data"                ,
     "The Andromeda Galaxy is on a collision course with us -- be prepared"           ,
     "Stellar formation will cease in just a trillion years -- what will we do then?" ,
     "We are very user friendly -- we are just selective about who our friends are"   ,
     "May it be a light to you in dark places, when all other lights go out"          ,
     "No in elenath hilar nan had gin -- May the stars shine upon your path"          ,
     "There is a time for departure even when there is no place to go"                ,
     "Sometimes you've got to let go to see if there was anything worth holding onto" ,
     "Remember me and smile, for it's better to forget than remember me and cry"      ,
     "So now I say goodbye, but I feel sure we will meet again sometime"              ,
     "If you're anything like me, you're both smart and incredibly good looking"      ,
     "In battle we may yet meet again, though all the hosts of Mordor stand between"  ,
     "Repeat after me: Om Mani Padme Hum, Om Mani Padme Hum, Om Mani Padme Hum ...."  ,
     "Let us therefore study the incidents of this as philosophy to learn wisdom from",
     "Analyze your data rigorously -- you can fake the conclusions all you want later",
     "O wad some Pow'r the giftie gie us, To see oursels as ithers see us"            ,
     "One half the world cannot understand the statistics of the other"               ,
     "It is better to create than to learn. Creating is the essence of life"          ,
     "Events of importance are often the result of trivial causes"                    ,
     "Men worry more about what they can't see than about what they can"              ,
     "The best revenge is to be unlike him who performed the injury"                  ,
     "The art of living is more like wrestling than dancing"                          ,
     "If the genome is the source code, it should have come with comments"            ,
     "So the days float through my eyes, but still the days seem the same"            ,
     "I can't believe what you say, because I see what you do"                        ,
     "What you do speaks so loudly I cannot hear what you say"                        ,
     "Those who say it can't be done are usually interrupted by those doing it"       ,
     "Spending any mental energy looking back cannot help you move forward"           ,
     "The supra-ultracrepidarian-est software you'll ever need"                       ,

     "The problem with science: answering 1 question creates 20 new (and harder) ones"        ,
     "When all you have is a computer, every problem looks like it needs linear algebra"      ,
     "You know you're in trouble when it takes a 64 bit integer to count your unread emails"  ,
     "Once you've done what you have to do, no one will let you do what you want to do"       ,
     "My name is AFNImandias, Brain of Brains; Look on my Statistics, ye Clever, and Despair" ,
     "Statistically Significant is NOT the same as Significant -- they're not even similar"   ,
     "If you drink a liquid that has p=0.06 of being poison, do you feel significantly safe?" ,
     "You must accept finite disappointments, but never lose your infinite hopes"             ,
     "We may all have come on different ships, but we're all in the same boat now"            ,
     "You can always find me out on the Long Line -- I hang out by the Church-Kleene ordinal" ,
     "Outside of a dog, a book is Man's best friend; inside of a dog, it's too dark to read"  ,
     "I've narrowed the answer to your hypothesis down to two possibilities: right and wrong" ,
     "If there was closed-book test on how to be YOU, would you get a passing grade?"         ,
     "An alternative fact: truth isn't truth"                                                 ,

     "Someday I'll tell you of the Giant Rat of Sumatra, a tale for which the world is not prepared"    ,
     "People have to learn to live with newly-discovered facts; if they don't, they die of them"        ,
     "It is the pardonable vanity of lonely people everywhere to assume that they have no counterparts" ,

     /* Multi-line quotes */
     "\n  A journey of a thousand miles begins with a single stride,\n"
     "  and then continues on and on and on and on and on,\n"
     "  with a million more plodding steps to trudge through endlessly"  ,

     "\n  Ever returning spring, trinity sure to me you bring\n"
     "  Lilac blooming perennial, drooping star in the West,\n"
     "  And thought of him I love"                                                            ,

     "\nIn the words of H Beam Piper:\n"
     "   If you don't like the facts, ignore them.\n"
     "   And if you need facts, dream up some you DO like"                                    ,

     "\n  We shall not cease from exploration\n"
     "  And the end of all our exploring\n"
     "  Will be to arrive where we started\n"
     "  And know the place for the first time"                                                ,

     "Remember:\n"
     "  To argue with those who have renounced the use and authority\n"
     "  of reason is as futile as to administer medicine to the dead"                         ,

     "Remember: Don't light the fire while you are standing in the gasoline"                  ,

     "Remember the Manager's Mantra:\n"
     "  * Work Harder.\n"
     "  * Work Smarter.\n"
     "  * Work Faster.\n"
     "  * Do More with Less.\n"
     "  * You're screwed (this part isn't spoken aloud)"                                      ,

     "\n  I don't want any 'downs' in my life:\n"
     "  I just want 'ups', and then 'upper ups'"                                              ,

     "\n  It is a truth universally acknowledged, that a single scientist\n"
     "  in possession of a large FMRI data collection, is in need of an AFNI"                 ,

     "\n  The great thing about the human condition:\n"
     "  No matter how bad it is, it can always get worse"                                     ,

     "\n  When someone says: I'm going to simplify things.\n"
     "  They mean:         Be confused. Be very, very confused"                               ,

     "\n  When someone says: I'm going to name the elephant in the room.\n"
     "  They mean:         My next observation will be startlingly banal"                     ,

     "\n  When someone says: We need to show leadership.\n"
     "  They mean:         I should be in charge"                                             ,

     "\n  When someone says: There needs to be a bottom-up process.\n"
     "  They mean:         Nobody asked me about this"                                        ,

     "\n  When someone says: The perfect is the enemy of the good.\n"
     "  They mean:         Ignore everyone else's ideas and just use mine"                    ,

     "\n  When someone says: Any other comments on this?\n"
     "  They mean:         Will everyone please, for the love of all that is holy, shut up?"  ,

     "\n  When someone says: I agree 100% with your concept,\n"
     "  They mean:         I am implacably opposed to your proposal"                          ,

     "\n  If 2 reasonable priors lead to different conclusions, then it's time to\n"
     "  look for more data, think harder, mumble inaudibly, or have a strong drink"           ,

     "\n  To be stupid, selfish, and have good health are three requirements\n"
     "  for happiness; though if stupidity is lacking, all is lost"                           ,

     "\n  May the following be true for you:\n"
     "   'Work is about a search for daily meaning as well as daily bread,\n"
     "    for recognition as well as cash, for astonishment rather than torpor;\n"
     "    in short, for a sort of life rather than a Monday through Friday sort of dying.'\n"
     "  ... especially if you are working on a Saturday!"                                     ,

     "\n xkcd's translation of p-values into words:\n"
     "     0.001  = Highly significant\n"
     "     0.01   = Highly significant\n"
     "     0.02   = Highly significant\n"
     "     0.03   = Highly significant\n"
     "     0.04   = Significant\n"
     "     0.049  = Significant\n"
     "     0.050  = Oh cr*p, redo calculations\n"
     "     0.051  = On the edge of significance\n"
     "     0.06   = On the edge of significance\n"
     "     0.07   = Highly suggestive\n"
     "     0.08   = Highly suggestive\n"
     "     0.09   = Significant at the p < 0.1 level\n"
     "     0.099  = Significant at the p < 0.1 level\n"
     "     > 0.1  = Hey! Look at this interesting subgroup analysis"   ,

     "\n Possible answers to a binary question:\n"
     "     Yes\n"
     "     No\n"
     "     Maybe\n"
     "     I don't know\n"
     "     I know but I'm not telling you\n"
     "     I need to talk to my lawyer\n"
     "     I need to talk to my dentist\n"
     "     Please repeat the question\n"
     "     Could you clarify what you mean, exactly?\n"
     "     Quantum indeterminacy makes any answer uncertain\n"
     "     That depends on the truth of the Riemann Hypothesis\n"
     "     Is there an odd perfect number?\n"
     "     Go Fish\n"
     "     I like eggs\n"
     "     Look, a squirrel" ,

     "\n There comes a time when you look into the mirror and you realize\n"
     " what you see is all that you will ever be. And then you accept it.\n"
     " Or you stop looking in mirrors" ,

     "\n  Next time you are afraid to share your ideas, remember that someone\n"
     "  once said in a meeting 'Let's make a film with a tornado full of sharks'" ,

     /* from Quentin Crisp */

     "Don't clean your house. After 4 years, the dirt doesn't get any worse" ,
     "Don't keep up with the Joneses; drag them down to your level" ,
     "To know all is not to forgive all; it is to despise everybody" ,
     "If at first you don't succeed, failure may just be your style" ,

     /* from John Tukey */

     "\n  Better an approximate answer to the right question,\n"
     "  than an exact answer to the wrong question"                                    ,

     "\n  The combination of some data and an aching desire for an answer does not\n"
     "  ensure that a reasonable answer can be extracted from a given body of data"    ,

     /* from Crooked Timber */

     "\n The following contradictory statements are all DEEP and PROFOUND:\n"
     "    Forget about the afterlife, about the Elsewhere, seize the day, enjoy life\n"
     "      fully here and now, it's the only life you've got!\n"
     "    Do not get trapped in the illusory and vain pleasures of earthly life;\n"
     "      money, power, and passions are all destined to vanish into thin air\n"
     "      -- think about eternity!\n"
     "    Bring eternity into your everyday life, live your life on this earth as\n"
     "      if it is already permeated by Eternity!\n"
     "    Do not try in vain to bring together eternity and your terrestrial life,\n"
     "      accept humbly that you are forever split between Heaven and Earth!\n"
     "    Life is an enigma, do not try to penetrate its secrets, accept the\n"
     "      beauty of its unfathomable mystery!\n"
     "    Do not allow yourself to be distracted by false mysteries that just\n"
     "      dissimulate the fact that, ultimately, life is very simple - it is\n"
     "      what it is, it is simply here without reason and rhyme!\n"
     "    The ultimate, unfathomable mystery of life resides in its very simplicity,\n"
     "    in the simple fact that there is life.\n"
} ;

#endif
