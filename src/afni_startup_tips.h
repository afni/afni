#ifndef _AFNI_STARTUP_TIP_HEADER_
#define _AFNI_STARTUP_TIP_HEADER_

#undef  NTIP
#define NTIP (sizeof(tip)/sizeof(char *))
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
   "  choosing the numerical Display Range for the underlay\n"
   "  drawing a coordinate Label over the image\n"
   "  applying an Automask to the overlay\n"
   "  choosing the color for Zero values in the overlay"
 ,
   "If you crop an image, you can move the crop window around by pressing\n"
   "the Shift key plus one of the keyboard arrow keys."
 ,
   "The 'Disp' button in an image viewer pops up a control panel with many\n"
   "useful buttons, including:\n"
   "  Project   = combine multiple slices into one underlay\n"
   "  Tran 0D   = transform values of the underlay pixelwise\n"
   "  Tran 2D   = transform underlay image globally (e.g., blurring)\n"
   "  Rowgraphs = graph the underlay numerical values in 1-9 pixel rows"
 ,
   "The 'BHelp' button lets you click on some other button in the GUI\n"
   "to get more information about what that button does."
 ,
   "The right-click popup menu on the coordinate display in the AFNI\n"
   "controller has several useful functions, including:\n"
   "  controlling the coordinate display order\n"
   "  jumping to x,y,z (mm) or i,j,k (voxel index) coordinates"
 ,
   "The right-click popup menu on the label above the threshold slider\n"
   "lets you control the threshold in various ways:\n"
   "  pin the Threshold sub-brick to equal the OLay or OLay+1 sub-brick\n"
   "  set the threshold slider to have a given voxelwise p-value\n"
   "  control Alpha fading for colorization of sub-threshold voxels\n"
   "  see only Positive or Negative values (with respect to the threshold)\n"
 ,
   "The right-click popup menu on the label above the color overlay bar\n"
   "lets you control colorization from the OLay sub-brick in several ways:\n"
   "   you can jump crosshairs to the largest OLay value above threshold\n"
   "   you can write the current color palette out to a file for editing,\n"
   "    or to an image for use in a figure\n"
   "   you can apply pixelwise or 2D spatial transformations to the\n"
   "    OLay values before they are turned into colors"
 ,
   "You can run InstaCorr on several subjects at the same time, using\n"
   "multiple AFNI controllers opened with the 'New' button."
 ,
   "REMEMBER: afni_proc.py is your friend when doing time series analyses!"
 ,
   "The 'New' button (lower left corner of AFNI controller) lets you open\n"
   "another AFNI controller. The UnderLay and OverLay datasets will be\n"
   "listed in the controller window title bar."
 ,
   "Image viewer keypress: q = close window"
 ,
   "Image viewer keypress: S = save image (works in graph viewer too)"
 ,
   "Image viewer keypress: o = turn OLay color on or off"
 ,
   "Image viewer keypress: u = make underlay image from the OLay dataset"
 ,
   "Image viewer keypress: 4 or 5 or 6 = meld ULay and OLay images"
 ,
   "Image viewer keypress: z/Z = zoom out or in"
 ,
   "Graph viewer keypress: < or > = move focus time down or up 1 TR"
 ,
   "Graph viewer keypress: 1 or L = move focus time to first or last TR"
 ,
   "Graph viewer keypress: v/V = video the focus time up or down"
 ,
   "Graph viewer keypress: m/M = decrease/increase matrix size of graphs"
 ,
   "Graph viewer keypress: w = write time series from central sub-graph to a file"
 ,
   "The image viewer 'Mont' button (along bottom) will let you make a montage\n"
   "from multiple slices, which can be Saved to a .jpg or .png file."
 ,
   "If the image editing program 'gimp' is in your path, then the image viewer\n"
   "Save control panel will include an option to start gimp on your image, so\n"
   "you can further edit it immediately. See https://www.gimp.org/"
 ,
   "The graph viewer 'Tran 1D' function Dataset#N (from the 'Opt' main menu) lets\n"
   "you plot extra dataset time series on top of the UnderLay dataset's time\n"
   "series graphs."
 ,
   "You can change the way the graph viewer shows it plots using the 'Colors, Etc.'\n"
   "sub-menu from the main 'Opt' menu (lower right corner)."
 ,
   "The graph viewer 'Opt' menu item 'Detrend' lets you choose a polynomial degree\n"
   "for detrending the graph data. This can help you visualize the features of the\n"
   "data you want to see without be distracted by long term trends up or down."
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
   "  NN = Nearest Neighbor    Li = Linear\n"
   "  Cu = Cubic               Bk = Blocky (between NN and Li)\n"
   "When the OverLay is at a coarser resolution than the UnderLay (common in FMRI),\n"
   "Li will produce 'nicer' looking maps, but NN will be more 'honest' looking."
 ,
   "Define Datamode -> Lock lets you turn the xyz coordinate lock between AFNI\n"
   "controllers off, if you want. Or, you can turn on 'Time Lock', so that the\n"
   "TR index is locked between controllers, as well as the crosshair location."
 ,
   "Normally, the grid size of the pixel image created for display in an\n"
   "AFNI image viewer is take from the grid size of the Underlay dataset.\n"
   "But you can change that using the 'Datamode' control panel, by choosing\n"
   "'Warp ULay on Demand', then setting the grid resampling mode below.\n"
   "Sometimes this is useful for creating nicer looking functional images,\n"
   "especially when 'Alpha' is turned on."
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
   "(e.g., 'ULay') to get a chooser panel that lets you control the\n"
   "menu choice in a different way."
 ,
   "The 'Rota' arrows (in Define Overlay) lets you rotate the color bar,\n"
   " one color step per click -- if you use Shift+click, it takes 5\n"
   " color steps per click.\n"
   "The 'F' button to the right will flip the color bar top-to-bottom."
 ,
   "The image viewer right-click popup menu has several useful functions:\n"
   "  Jumpback        = take crosshairs to their previous location\n"
   "  Where Am I?     = show atlas information about the current location\n"
   "  Image Display   = hide GUI controls\n"
   "  Draw ROI Plugin = activate the Drawing plugin"
 ,
   "Right-click on the 'Disp' button (lower left) of an image viewer will\n"
   " raise the corresponding AFNI controller to the top.\n"
   "Right-click on the AFNI logo (lower left) of a graph viewer does the same.\n"
   "These functions are here in case you lose the controller somewhere on\n"
   "the screen, and want to get it back."
 ,
   "Right-click on the 'Save' button in an image viewer will popup the list\n"
   "of possible image save formats."
 ,
   "Left-click in the square right of 'Etc->' in an AFNI controller will\n"
   " popup a copy of the splash screen again. Another left-click there will\n"
   " pop the splash window down again. Clicking in the reincarnated splash screen\n"
   " may give funny results.\n"
   "Right-click in that square will give a menu with some fun choices.\n"
   "Middle-click in that square will popup a random insult.\n"
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
   " down, and you can adjust the image grayscale until it looks good."
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
   "You can crop an image by left-clicking the 'crop' button in an image viewer.\n"
   "You can Montage cropped images (all will be cropped the same way).\n"
   "Right-clicking on 'crop' will give a chooser where you can specify the\n"
   " cropping region size exactly."
 ,
   "You can use keyboard shortcuts to precisely adjust the threshold slider.\n"
   "Put the mouse over the slider, and then\n"
   " down/up arrows    for tiny adjustments \n"
   " page up/page down for larger adjustments"
 ,
   "In a graph viewer, you can restrict the plotting to a subset of the time\n"
   "points by using the Opt -> Grid -> Index Pin menu item."
 ,
   "In a graph viewer, the default plotting method has the bottom of each graph\n"
   "using a separate value (the minimum in that voxel). You can also make them\n"
   "have a common baseline (minimum among all voxels in the graph window) or\n"
   "a global baseline (set by you) by using the Opt -> Baseline menu items."
 ,
   "At the bottom of a graph viewer is a bunch of text showing various\n"
   "information about what is being shown."
 ,
   "When looking at FMRI data graphs with a regular stimulus timing, it is\n"
   "helpful to set the graph grid lines to match the stimulus timing spacing.\n"
   "You can do this from the Opt -> Grid -> Choose menu item."
 ,
   "You can have graphs drawn as box plots rather than as connected line segments,\n"
   "by using the Opt -> Colors, Etc. -> Boxes menu item, or by pressing the 'B'\n"
   "key when the mouse cursor is over the graph viewer window."
 ,
   "In the graph viewer Opt and FIM menus, items that have keyboard shortcuts have\n"
   "the key inside square brackets, as in Opt -> Scale -> Down [-], meaning the '-'\n"
   "key will cause the graph to scaled down (vertically)."
 ,
   "Advanced graphing: you can change the x-axis values from being 0,1,2,... to be\n"
   "anything you want, chosen from a 1D text file (applies to all voxels) or from\n"
   "a 3D dataset (per voxel x-coordinates). The x-axis for the central sub-plot will\n"
   "be displayed as a vertical graph at the left of the graph viewer window. See\n"
   "the Opt -> X-axis menu items to do strange things."
 ,
   "The Define Datamode -> Misc menu has a lot of choices, a few of which are:\n"
   " Voxel Coords? = show voxel indexes instead of mm coordinates in AFNI GUI\n"
   " ULay Info     = show information from the UnderLay dataset header\n"
   " Purge Memory  = eject datasets from memory, forcing reloads when viewed"
 ,
   "When saving an image (or a montage), you might want to turn the crosshairs off.\n"
   " You can do this from the 'Xhairs' menu in the AFNI controller.\n"
   "If you want all the sub-images in a montage to have crosshairs (instead of\n"
   " just the central image), turn the 'X+' button on."
 ,
   "When saving from the image viewer, the saved image is on the matrix of the\n"
   "dataset. It is NOT a screen capture; that is, the image saved will not depend\n"
   "on the size of the image viewer window. A montage image will be the full size\n"
   "of all the base images catenated together. You can also choose a 'Blowup'\n"
   "factor to scale the image size upward: factors from 2 to 8 are available."
 ,
   "You can tell the graph viewer to ignore the first few time points when plotting.\n"
   "Menu item FIM -> Ignore lets you choose how many to ignore by mouse clicks.\n"
   "Keypress 'I' increases the ignore count by 1, 'i' decreases by 1.\n"
   "Ignored points are plotted with little blue circles which take the value of\n"
   "the first non-ignored point."
 ,
   "If you have a complicated AFNI window layout you want to save, you can use\n"
   "Define Datamode -> Misc -> Save Layout to save a startup script that will\n"
   "be used when you re-start AFNI in the same directory to restore the AFNI\n"
   "windows to (approximately) the same state they had before."
 ,
   "If you are doing complicated twisted things with AFNI programs, ASK US\n"
   "(on the message board). Often, there is an easier way to do a task!"
 ,

/*-- tips below here are for non-GUI programs --*/

   "Questions about AFNI? Try our Message Board at\n"
   "  https://afni.nimh.nih.gov/afni/community/board/\n"
   "Please be specific and focused, as generic questions without\n"
   "details are hard to answer well on a Web forum."
 ,
   "Skull stripping T1-weighted datasets? Programs and scripts include:\n"
   " 3dSkullStrip     - surface expansion program (many options)\n"
   " @NoisySkullStrip - when the dataset is very noisy\n"
   " @SSwarper        - nonlinear warping to MNI space combined\n"
   "                    with skull stripping (and then the warp can be\n"
   "                    used in afni_proc.py for time series analyses)"
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
   "  3dmaskave  = get average across the ROI, one value per time point\n"
   "  3dROIstats = like 3dmaskave, but for multiple ROIs\n"
   "  3dmaskSVD  = like 3dmaskave, but gives the principal singular\n"
   "               vector (time series) across the ROI instead of the mean\n"
   "  3dmaskdump = just dumps out ALL the values from the ROI at\n"
   "               all time points (presumably you will do something\n"
   "               fun with these in your own software?)"
 ,
   "Programs for computing some local statistics in a neighborhood around\n"
   "each voxel (e.g., a ball):\n"
   "  3dLocalstat   = various statistics from the neighborhood\n"
   "                  (e.g., mean, median, variance)\n"
   "  3dLocalBistat = various 2-sample statistics from the neighborhood,\n"
   "                  calculated between 2 datasets\n"
   "                  (e.g., Pearson or Spearman correlation)\n"
   "  3dLocalPV     = compute the principal vector (time series) from\n"
   "                  all the dataset time series inside the neighborhood\n"
   "                  (a fancy way to 'smooth' the data)"
 ,
   "Are you using nonlinear warping to align your subjects' T1-weighted\n"
   "datasets? If not, you should give it a try. The brain images will line\n"
   "up better than using affine alignment (3dAllineate, @auto_tlrc, etc.)\n"
   "and you can get better FMRI results at the group level when you use\n"
   "the nonlinear warps in afni_proc.py. Programs for this:\n"
   "  3dQwarp      = the foundational nonlinear warping program\n"
   "  @SSwarper    = uses 3dQwarp and 3dSkullStrip together to align\n"
   "                 volumes to the MNI template and skull strip them\n"
   "  auto_warp.py = runs 3dQwarp for you, so you don't have to read\n"
   "                 that programs lengthy help output"
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
   "during activation analysis."
 ,
   "AFNI programs for individual dataset time series correlation-ing:\n"
   "  3dTcorr1D        = correlate each voxel with a small set of 1D files\n"
   "  3dTcorrelate     = correlate each voxel between 2 datasets\n"
   "  3dTcorrMap       = make of map of how correlated each voxel is\n"
   "                     to every other voxel in the same dataset\n"
   "  3dAutoTcorrelate = correlate each voxel to every other voxel\n"
   "                     in the same dataset and save everything (HUGE)"
 ,
   "Program 3dGroupInCorr can be used for Group Instant Correlation\n"
   "interactively via the AFNI GUI. It can also be used in batch mode.\n"
   "The '-batchGRID' option lets you scan over a grid of seed voxels,\n"
   "compute the individual datasets' correlations with their seeds,\n"
   "then compute the t-tests among these correlation maps, and save\n"
   "the results to a collection of datasets."
 ,
   "Want to convert each statistic in a dataset to a (voxelwise) p-value?\n"
   "Use program 3dPval.\n"
   "More complicated statistical conversions can be done with 3dcalc, using\n"
   "the cdf2stat() and stat2cdf() functions. You can explore those interactively\n"
   "using the ccalc program, to make sure you are giving 3dcalc the correct\n"
   "expression."
 ,
   "Want to test dataset values voxelwise for normality (Gaussianity)?\n"
   "Program 3dNormalityTest will apply the Anderson-Darling test and\n"
   "give you a dataset with the voxelwise measure of non-Gaussianity."
 ,
   "Program 1dCorrelate will compute the pairwise correlation coefficient\n"
   "between 2 columns of numbers, AND give you the bootstrap confidence\n"
   "interval for the result."
 ,
   "Program 1dplot is useful for quick-and-dirty plotting of columns\n"
   "of numbers (.1D files). An example, creating a response model\n"
   "function with 3dDeconvolve and sending the time series directly\n"
   "into 1dplot:\n"
   "  3dDeconvolve -num_stimts 1 -polort -1 -nodata 81 0.5         \\\n"
   "               -stim_times 1 '1D: 0' 'TWOGAMpw(3,6,0.2,10,12)' \\\n"
   "               -x1D stdout: | 1dplot -stdin -THICK -del 0.5"

} ;

#endif
