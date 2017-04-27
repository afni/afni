#!/usr/bin/env python

import time, sys, os
from   optparse  import  OptionParser
import subprocess
import string


"""

This script should for the basis for unwarping data collected with EPI-
-based techniques, such as tasking or resting state functional MRI, or
DTI, with calibration data acquired with opposite phase polarity.

It will be designed as a library, so that it will be call-able from
afni_proc.py, and also as its own stand-alone program.

The inputs will be:

   - Data set(s) to be corrected. The convention in this script is that
     for data to be corrected, acquired with a single polarity (such as
     resting state EPI data), is called the FORWARD polarity.

   - Conversely, there will be at least one other data set acquired with
     the opposing polarity.  These will be denoted as REVERSE polarity.

Example usage:
     unWarpEPI.py -f run1+orig'[0..5]' -r blip_down+orig -d 'run1,run2' \
       -a anat+orig -s unwarp_folder


It should be noted here that it is technically possible for the FORWARD
calibration data to be a subset of the data itself to be corrected (say
the first 5 or 10 volumes of that data set).  So the same data set could
be specified twice, both as the FORWARD calibration data set, and the
data to be corrected. However, in the case where the FORWARD calibration
data is part of the time-series of data being corrected, it should be a
subset of those data, of equal number of time points / volumes, to the
REVERSE polarity calibration data.

Optionally, an anatomical data set can be added to the list as another
target.

Questions and suggestions:
Daniel Glen, Vinai Roopchansingh

"""



class unWarpWithBlipUpBlipDownEPI:

   def __init__ (self, options,parser):

      print "Unwarp function initialized"

      ### Define the subject prefix for all datasets
      if options.subjID:
         self.subjectID = options.subjID
      else:
         self.subjectID = 'TS'

      ### set number of time points to ignore in 3dTshift
      self.tshift_ignore = '0'

      ### set 3dNwarpApply interpolation mode (quintic or wsinc5)
      ### quintic is about twice as fast as wsinc5
      self.interp_mode = 'quintic'

      ### set gzip output (ignore AFNI_COMPRESSOR and AFNI_AUTOGZIP settings)
      self.compress = ".gz"
      
      
      ### Set this to YES to skip unwarping the median-ized
      ### calibration runs (which aren't needed)

      self.skip_medianized_unwarp = 'YES'

      ### Define the standard space template to use at the end
      ### (set template = NONE or such nonsense to avoid this step)

      # template = MNI152_T1_2009c_uni+tlrc
      # template = infant-1yr.nii.gz      # template for toddlers
      self.template = 'NONE'
      self.template_minpatch = 17

      # Get data sets required to do unwarping
      if not options.forward:
         print "!!! Required forward calibration data missing - exiting !!!"
         parser.print_usage()
         sys.exit (-1)
      else:
         self.forwardCalibrationData = options.forward
         print "Forward calibration data is " + self.forwardCalibrationData

      if not options.reverse:
         print "!!! Required reverse calibration data missing - exiting !!!"
         parser.print_usage()
         sys.exit (-1)
      else:
         self.reverseCalibrationData = options.reverse
         print "Reverse calibration data is " + self.reverseCalibrationData

      if not options.data:
         print "!!! Required data to be corrected missing - exiting !!!"
         parser.print_usage()
         sys.exit (-1)
      else:
         self.dataToCorrect = list(dataSet for dataSet in string.split (options.data, ','))

         print "Data set(s) to be corrected is(are) %s" % self.dataToCorrect

      if not options.anat4warp:
         self.dataAnat = 'NONE'
         print "Anatomical data not provided - continuing without it."
      else:
         self.dataAnat = options.anat4warp
         print "Anatomical data set is " + self.dataAnat

      if options.giant_move:
         self.giant_move = True
      else:
         self.giant_move = False



   def unWarpData (self):

      # print "Value of subject ID is " + self.subjectID

      outputDir = "unWarpOutput_" + self.subjectID

      if os.path.isdir (outputDir):
         print "!!! Output directory " + outputDir + " already exists !!!  Script ends now !!!"
#         sys.exit (-1)
      else:
         os.mkdir (outputDir, 0755)

      # Use subprocess.call instead of Popen to wait for this data set
      # to be finished copying before proceeding.
      dataToCorrectList =  list()
      for dataSet in self.dataToCorrect:
         dataToCorrectList.append(self.subjectID + "_" + dataSet)
      calibForwardName =  self.subjectID + "_calibForwardData"
      calibReverseName =  self.subjectID + "_calibReverseData"
      if self.dataAnat != 'NONE':
         anatBaseName = self.subjectID + "_anat.nii"  + self.compress

      # indent copying section to skip for testing - need better bypass
      # if 0:
      # Copy the relevant data into the working directory

      if self.dataAnat != 'NONE':
         subprocess.Popen (['3dcopy', self.dataAnat,
                             outputDir + "/" + anatBaseName],
                             stdout=subprocess.PIPE)

      # Use 3dcalc instead of 3dcopy to allow sub-brick data selection for
      # calibration data.
      subprocess.call  (['3dcalc', "-a", self.forwardCalibrationData,
                          "-expr", "a", "-prefix",
                          outputDir + "/" + calibForwardName],
                          stdout=subprocess.PIPE)

      subprocess.call  (['3dcalc', "-a", self.reverseCalibrationData,
                          "-expr", "a", "-prefix",
                          outputDir + "/" + calibReverseName],
                          stdout=subprocess.PIPE)
      for dataSet in self.dataToCorrect:
         subprocess.call  (['3dcopy', dataSet,
                             outputDir + "/" + self.subjectID + "_" + dataSet],
                             stdout=subprocess.PIPE)



      # Now that we have dataset copied into the working directory (outputDir)
      # change to that for our working directory
      os.chdir (outputDir)

      # indent processing sections to skip for testing - need better bypass
      # if 0:
      
      # Get median of each calibration data set

      processedCalibDataName01R = "01_" + calibReverseName + "Median.nii" \
                         + self.compress
      subprocess.call  (['3dTstat', "-prefix", processedCalibDataName01R,
                          "-median", calibReverseName + "+orig"],
                          stdout=subprocess.PIPE)

      processedCalibDataName01F = "01_" + calibForwardName + "Median.nii" \
                         + self.compress
      subprocess.call  (['3dTstat', "-prefix", processedCalibDataName01F,
                          "-median", calibForwardName + "+orig"],
                          stdout=subprocess.PIPE)

      # Now skull-strip each median calibration data set.

      processedCalibDataName02R = "02_" + calibReverseName + "SkullStripped.nii" \
                         + self.compress
      subprocess.call  (['3dAutomask', "-apply_prefix", processedCalibDataName02R,
                          processedCalibDataName01R])

      processedCalibDataName02F = "02_" + calibForwardName + "SkullStripped.nii" \
                         + self.compress
      subprocess.call  (['3dAutomask', "-apply_prefix", processedCalibDataName02F,
                          processedCalibDataName01F])

      # Now - 3dQwarp the 2 calibration images together
      #   MINUS => 'For' image goes with the base   = Forward dataset
      #   PLUS  => 'Rev' image goes with the source = Reverse dataset
      # The 'other' EPI datasets are assumed later to be warped
      # in the same way that the 'For' datasets are warped.

      # The parameters below were chosen by trial and error to give
      # decent-looking results for the sample datasets provided by
      # JGC + SI.
      #
      # 3dQwarp options explained:
      #
      #   -plusminus  means find the intermediate position between the 2
      #                 inputs
      #
      #   -pmNAMES    means give the source warp-to-middle the name 'Rev',
      #                 and give the base warp-to-middle the name 'For'
      #
      #   -noweight   means that a binary automask is used for weighting,
      #                 rather than the weight being proportional to image
      #                 intensity
      #
      #   -minpatch 9 means the smallest patch is 9x9x9 voxels (27 mm here)
      #                 -- this is the smallest patch allowed by 3dQwarp
      #                 -- a larger minpatch (say 13) runs faster but
      #                    warping results don't look so good in a few
      #                    places
      #
      #   -base and -source are the input dataset names
      #
      #   -prefix gives the output dataset names -- which will have
      #           the -pmNAMES suffixes attached appropriately
      #
      # 3dQwarp does allow the user to specify warping only in one direction;
      # I did not try that here, so the warps produced are true 3D warps.
      #
      # Each patch has 2 cubic polynomials (in each direction), so the
      # "resolution" of the warp is 27/2 = 13.5 mm in some sense -- that is,
      # there is one warp parameter (for each axis x,y,z) every 13.5 mm.

      processedCalibDataName03 = "03_" + self.subjectID + "_MidWarped.nii" \
                               + self.compress
      
      subprocess.call  (['3dQwarp', "-plusminus",
                          "-pmNAMES", "Reverse", "Forward",
                          "-pblur", "0.05", "0.05",
                          "-blur", "-1", "-1",
                          "-noweight", "-minpatch", "9",
                          "-source", processedCalibDataName02R,
                          "-base",   processedCalibDataName02F,
                          "-prefix", processedCalibDataName03])

      # The previous C-shell script had some instructions for unwarping the
      # median Calibration data sets.  For now - this will not be implemented
      # here, but the basic shell commands were:
      #
      #      3dNwarpApply -nwarp ${processedCalibDataName03}_Forward_WARP.nii   \
      #                   -source forwardCalibrationBaseDataSetName             \
      #                   -prefix base_name.WMid.nii -$(self.interp_mode)
      #
      # and a similar command run on the reverse calibration data set, with the
      # _Reverse_WARP.nii data set.

      # =================================================================================
      # ========== Process the other EPI datasets (i.e., in the Forward mode) ===========

      # ---- loop over other forward datasets = those that need fixing for analysis ----
      #
      # The procedure is
      #
      #  (1) time shift each 'other' dataset                       == suffix _H
      #
      #  (2) unwarp each of these datasets                         == suffix _HW
      #
      #  (3) volume register the _HW datasets to get the
      #      alignment parameters for each set of echoes
      #      -- the purpose of doing things in this order is to
      #         avoid doing separate registrations for each echo in
      #         a set of datasets that were acquired simultaneously
      #      -- we do NOT save this registered dataset, just the
      #         registration matrix (.aff12.1D) for use below
      #
      #  (4) apply the warp AND the registration matrix
      #      from (3) to get the individual echo aligned datasets == suffix _HWV
      #
      # this array is used for warping directly to a template at the end

      #-- (1) time shift each EPI dataset [Should be just a single time series
      #       here] --
      #-- (old-2) combine echoes from a set of datasets (skipped here)
      #-- (2) unwarp the result of (1)              --
      #-- (3) then 3dAllineate the output of (2)    --
      #-- (4) then 3dNwarpApply to base data        --

      # Put loop here to iterate over datasets in dataToCorrectList
      for dataToCorrectName in dataToCorrectList:
         print("dataToCorrectName is %s" % dataToCorrectName)
         processedData04 = dataToCorrectName + "_H"
         subprocess.call  (['3dTshift', "-tzero", "0", "-quintic",
                             "-ignore", self.tshift_ignore,
                             "-prefix", "04_" + processedData04 + ".nii"  + self.compress,
                             dataToCorrectName + "+orig"],
                             stdout=subprocess.PIPE)

         # Have to check and then implement on from here

         # Now warp time-shifted data
         processedData05 = processedData04 + "W"
         subprocess.call  (['3dNwarpApply', "-nwarp",
                            "03_" + self.subjectID + "_MidWarped_Forward_WARP.nii" \
                            + self.compress,
                            "-source", "04_" + processedData04 + ".nii" \
                            + self.compress,
                            "-prefix", "05_" + processedData05 + ".nii" \
                            + self.compress,
                            "-short",
                            "-" + self.interp_mode])

         # 3dAllineate is used here instead of 3dvolreg since it gives better
         # results -- my guess is that the difference in contrast/shading
         # between the base and source datasets gives 3dvolreg troubles
         subprocess.call  (['3dAllineate',
                            "-base",   "03_" + self.subjectID +
                                       "_MidWarped_Forward.nii" + self.compress,
                            "-source", "05_" + processedData05 + ".nii" + self.compress,
                            "-prefix", "NULL",
                            "-1Dmatrix_save", processedData04 + "WV.aff12.1D",
                            "-1Dparam_save",  processedData04 + "WV.motion.1D",
                            "-warp", "shift_rotate", "-onepass",
                            "-fineblur", "2", "-lpa", "-norefinal",
                            "-final", "quintic", "-automask+2", "-quiet"])

            #-- The next command warps all data at once using the combined
            #-- affine warp for each time point (from 3dAllineate) and
            #-- nonlinear warp (from 3dQwarp); the output datasets get the
            #-- same name as the inputs with the suffix '_HWV': these are
            #-- one major result of this script!
            #
            #-- The order of warps is in order from final space to original
            #-- space, since they are applied to the final space x-coordinates
            #-- in the order given. In this case, the data are registered after
            #-- being unwarped, so the transformation back from output space to
            #-- input space is un-register first, then un-unwarp.
            #
            #-- No '-master' option is given here, since the source 3D grid is
            #-- reasonable for these outputs.

         warpMatrices = (processedData04 + "WV.aff12.1D" + " " + "03_" +
                         self.subjectID +  "_MidWarped_Forward_WARP.nii"
                         + self.compress)
         subprocess.call  (['3dNwarpApply',
                            "-nwarp", warpMatrices,
                            "-source", "04_" + processedData04 + ".nii" 
                            + self.compress,
                            "-prefix", "06_" + processedData05 + "V.nii" \
                            + self.compress,
                            "-short",
                            "-" + self.interp_mode])

            # Make sure orientation and obliquity information is preserved after
            # the 3dNwarpApply command.

         subprocess.call (['3drefit',
                           '-atrcopy', calibForwardName + '+orig',
                           'IJK_TO_DICOM_REAL', '06_' + processedData05 + 'V.nii' \
                           + self.compress])

      if self.dataAnat != 'NONE':
         print("Aligning anat dataset to %s" % dataToCorrectList[0])
         dataToCorrectName =  dataToCorrectList[0]
         
         # Unifize EPI for alignment and visualization.  Do only on
         # 2nd sub-brick because of profile of excitation RF pulse,
         # and striping between slices in 1st sub-brick.

         # Fix/amend to unifize to the corrected/unwarped EPI data set
         subprocess.call  (['3dUnifize', "-GM", "-clfrac" , "0.22" ,
                            "-prefix", "07_" + dataToCorrectName + "_unif.nii" \
                            + self.compress ,
                            "06_" + processedData05 + "V.nii" \
                            + self.compress + "[1]"])

         # remove bias from (unifize/uniformize/bias correction) anatomical dataset
         subprocess.call  (['3dUnifize', "-GM", "-clfrac" , "0.22" ,
                            "-prefix", ("08_" + self.subjectID + "_anat"
                            + "_unif" + ".nii" + self.compress),
                            "-input",  self.subjectID + "_anat.nii" \
                            + self.compress])

         # convert data to short integers to save disk and memory space
         subprocess.call  (['3dcalc', 
                            "-a", ("08_" + self.subjectID + "_anat"
                            + "_unif" + ".nii" + self.compress),
                            "-prefix", ("08_" + self.subjectID + "_anat"  \
                            + "_unif_short" + ".nii" + self.compress),
                            "-datum", "short", "-nscale", "-expr", "a"])

         # create temporary obliquified anat (not aligned) for output grid
         # create temporary obliquified anat (not aligned) for output grid
         subprocess.call  (['3dWarp', 
                         "-card2oblique", ("07_" + dataToCorrectName
                         + "_unif.nii" + self.compress),
                         "-prefix", ("08_" + self.subjectID + "_anat"  \
                         + "_ob_temp" + ".nii" + self.compress),
                         ("08_" + self.subjectID + "_anat"  \
                         + "_unif_short" + ".nii" + self.compress)])

         # align unifized, short integer data to EPI, with output on oblique anat grid
         alignCmds = ['align_epi_anat.py',
                      "-anat", ("08_" + self.subjectID + "_anat"
                      + "_unif_short" + ".nii" + self.compress),
                      "-epi", "07_" + dataToCorrectName + "_unif.nii" \
                      + self.compress,
                      "-epi_base", "0", "-epi_strip", "3dAutomask",
                      "-suffix", "_aligned", "-cost", "lpc+ZZ", "-master_anat",
                      ("08_" + self.subjectID + "_anat"  \
                            + "_ob_temp" + ".nii" + self.compress)]
         # these skullstrip options worked well for a particular subject,
         # but not so great for another.
         # This is not critical for most alignments. Affine alignment
         # register over the whole dataset, so bits of extra skull or missing brain
         # generally won't change the alignment
         #          "-skullstrip_opts", 
         #          "-push_to_edge", "-shrink_fac", "0.1", "-shrink_fac_bot_lim", "0.01" ]

         # Exercise user option for giant_move if required.
         if self.giant_move == True:
            alignCmds.append("-giant_move")

         subprocess.call  (alignCmds)

      # If we're all happy, then exit that way!
      #   but who's ever "all happy" anyway?
      sys.exit (0)



def main():

   """Will return an instance of the option list"""

   usage = "%prog [options]"
   description = ("Routine to unwarp EPI data set using another "
                  "data set with opposite polarity or B0 field map" )
   usage =       ("  %prog -f run1+orig'[0..5]' -r blip_down+orig -d 'run1,run2' -a anat+orig -s unwarp_folder" )
   epilog =      ("For questions, suggestions, information, please contact Vinai Roopchansingh, Daniel Glen")

   parser = OptionParser(usage=usage, description=description, epilog=epilog)

   parser.add_option ("-f", "--forward",  action="store",
                                          help="calibration matching data to be corrected")

   parser.add_option ("-r", "--reverse",  action="store",
                   help="calibration with opposing polarity to data to be corrected")

   parser.add_option ("-a", "--anat4warp",action="store",
                                          help="reference anatomical data set")

   parser.add_option ("-d", "--data",     action="store",
                                          help="data to be corrected (same polarity as forward calibration data). Separate with commas if specifying multiple datasets. Do NOT put +orig at the end of these dataset names, or the script will fail!")

   parser.add_option ("-s", "--subjID",   action="store",
                                          help="ID of subject to be corrected")

   parser.add_option ("-g", "--giant_move", action="store_true",
                                          help="Set giant_move option for align_epi_anat if final align of anatomy to corrected EPI fails if datasets are far apart in space.")

   options, args = parser.parse_args()


   unwarpJob = unWarpWithBlipUpBlipDownEPI(options,parser)

   unwarpJob.unWarpData()



if __name__ == '__main__':
   sys.exit(main())

