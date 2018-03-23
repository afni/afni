.. _ahelp_Ifile:

*****
Ifile
*****

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: Ifile [Options] <File List> 
    
    	[-nt]: Do not use time stamp to identify complete scans.
    	       Complete scans are identified from 'User Variable 17'
    	       in the image header.
    	[-sp Pattern]: Slice acquisition pattern.
    	               Sets the slice acquisition pattern.
    	               The default option is alt+z.
    	               See to3d -help for acceptable options.
    	[-od Output_Directory]: Set the output directory in @RenamePanga.
    	                        The default is afni .
    
    	<File List>: Strings of wildcards defining series of
    	              GE-Real Time (GERT) images to be assembled
    	              as an afni brick. Example:
    	              Ifile '*/I.*'
    	          or  Ifile '083/I.*' '103/I.*' '123/I.*' '143/I.*'
    
    	The program attempts to identify complete scans from the list
    	of images supplied on command line and generates the commands
    	necessary to turn them into AFNI bricks using the script @RenamePanga.
    	If at least one complete scan is identified, a script file named GERT_Reco
    	is created and executing it creates the afni bricks placed in the afni directory.
    
    How does it work?
    	With the -nt option: Ifile uses the variable 'User Variable 17' in the 
    	I file's header. This option appears to be augmented each time a new
    	scan is started. (Thanks to S. Marrett for discovering the elusive variable.)
    	Without -nt option: Ifile first examines the modification time for each image and 
    	infers from that which images form a single scan. Consecutive images that are less 
    	than T seconds apart belong to the same scan. T is set based on the mean
    	time delay difference between successive images. The threshold currently
    	used works for the test data that we have. If it fails for your data, let us
    	know and supply us with the data. Once a set of images is grouped into a 
    	scan the sequence of slice location is analysed and duplicate, missing slices,
    	and incomplete volumes are detected. Sets of images that do not pass these tests
    	are ignored.
    
    Preserving Time Info: (not necessary with -nt option but does not hurt to preserve anyway)
    	It is important to preserve the file modification time info as you copy or untar
    	the data. If you neglect to do so and fail to write down where each scan ends
    	and/or begins, you might have a hell of a time reconstructing your data.
    	When copying image directories, use:
    	   cp -rp ???/* TARGET/ 
    	and when untaring the archive on linux use:
    	   tar --atime-preserve -xf Archive.tar 
    	On Sun and SGI, tar -xf Archive.tar preserves the time info.
    
    Future Improvements:
    	Out of justifiable laziness, and for other less convincing reasons, I have left 
    	Ifile and @RenamePanga separate. They can be combined into one program but it's usage
    	would become more complicated. At any rate, the user should not notice any difference
    	since all they have to do is run the script GERT_reco that is created by Ifile.
    
    	   Dec. 12/01 (Last modified July 24/02) SSCC/NIMH 
    	Robert W. Cox(rwcox@nih.gov) and Ziad S. Saad (saadz@mail.nih.gov)
