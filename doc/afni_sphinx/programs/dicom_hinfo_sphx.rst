.. contents:: 
    :depth: 4 

***********
dicom_hinfo
***********

.. code-block:: none

    Usage: dicom_hinfo [options] fname [...]
    Prints selected information from the DICOM file 'fname' to stdout.
    Multiple files can be given on the command line; see the examples
    below for useful ideas.
    
    --------
    OPTIONS:
    --------
     -tag aaaa,bbbb = print the specified tag.
                      -- multiple tags may follow the '-tag' option.
                      -- a tag consists of 4 hexadecimal digits,
                         followed by a comma, followed by 4 more
                         hexadecimal digits
                      -- any string that doesn't match this format
                         will end the list of tags
    
     -namelast      = Put the filename last on each output line,
     *OR* -last       instead of first.
    
     -no_name       = Omit any filename output.
    
    * The purpose of this program is to be used in scripts to figure out
      which DICOM files to process for various purposes -- see Example #2.
    
    * One line is output (to stdout) for each DICOM file that the program reads.
    * Files that can't be read as DICOM will be ignored (silently).
    * Tags that aren't found in a file will get their value printed as 'null'.
    
    * How do you know what hexadecimal tags you need?  You can start with using
      dicom_hdr on a single file to get the full list of tags (with names) and
      then experiment to see which tags can be used to meet your whims.
    
    * Some tags that might be useful for figuring out which DICOM files belong
      together and which ones are from separate acquisitions:
        0008,0030  =  Study Time  (might be the same for all images in one session)
        0008,0031  =  Series Time (will usually be different between imaging runs)
        0008,0032  =  Acquisition Time (might be different for EVERY file)
        0018,0050  =  Slice Thickness
        0020,0011  =  Series Number (if present, identifies different imaging runs)
        0028,0010  =  Number of Rows
        0028,0011  =  Number of Columns
        0028,0030  =  Pixel Spacing
      In the examples below, I use 0008,0031 as a way to distinguish between
      different acquisitions in the same imaging session.  For the data used
      here, the value of this tag was different for each distinct scan --
      localizers, structural, EPI -- so it worked as good way to find the
      break points between set of files that should go together.  However,
      I have seen DICOM files that lacked this tag, so you might have to
      experiment (using dicom_hdr) to find a good tag for this purpose.
    
    ---------
    EXAMPLES:
    ---------
    #1: The command below prints out the acquisition start time and the number
        of rows for each valid DICOM file in the directories below the current one:
    
          dicom_hinfo -tag 0008,0031 0028,0010 */*.dcm
    
        One sample output line would be
    
    TASK-A/image-00102-004243.dcm 141255.882508 256
    
    ---------
    #2: A more complicated example searches all the directories below the current one,
        then prints out a list of summaries of what look like unique acquisitions.
        This could be used to figure out what kind of data you have when someone gives
        you a bunch of DICOM files with no obvious structure to their filenames.
    
          find . -type f | xargs dicom_hinfo -tag 0008,0031 0028,0010 | uniq -f 1 -c
    
        The output from the above example was
    
       9 ./A/A/A/Z01 154116 256
       9 ./A/A/A/Z10 154210 256
      38 ./A/A/A/Z19 154245 64
     126 ./A/A/C/Z05 154326 256
    6000 ./A/A/H/Z01 154854 64
    2400 ./A/J/D/Z21 155617 64
     126 ./A/M/S/Z03 160228 256
      40 ./A/M/W/Z25 160304 64
      40 ./A/M/Y/Z13 160332 64
     126 ./A/N/A/Z01 160404 256
     126 ./A/N/E/Z23 160411 256
     126 ./A/N/J/Z19 160417 256
       1 ./A/N/O/Z15 161252 960
       1 ./A/N/O/Z16 161403 640
       9 ./A/N/O/Z17 150935 256
       9 ./A/N/P/Z00 151039 256
      37 ./A/N/P/Z10 151122 64
     120 ./A/N/Q/Z21 151203 256
    6000 ./A/N/V/Z11 151624 64
    2400 ./A/W/S/Z05 153010 64
    
        My goal was the find the structural and FMRI collections of images mixed
        in with various localizers and other 'junk'.  Based on the above, it seems:
          * the 126 files starting with ./A/A/C/Z05 are a structural set
          * the 6000 files starting with ./A/A/H/Z01 are an FMRI set
          * the 2400 files starting with ./A/J/D/Z21 are an FMRI set
          * the 126 files starting with ./A/M/S/Z03 are a structural set
        and so on.  This information makes it possible to extract the desired files
        from the giant collection of un-informative filenames, create AFNI datasets
        (using program Dimon and its '-infile_list' option appropriately), and then
        look at them to make final decisions about what to keep.
    
    ---------
    #3: Continuing the above example with actual creation of AFNI dataset
        from the collection of files, a script (in csh syntax):
    
          #!/bin/tcsh
          \rm -f qq*.out
    
          find . -type f \
            | xargs dicom_hinfo -tag 0008,0031 0028,0010 0028,0011 \
            | awk '$3 == $4' >> qqa.out
    
          uniq -f 1 -c qqa.out | awk '$1 > 99' > qqb.out
    
          foreach ddd ( `cat qqb.out | awk '{print $3}'` )
            echo 'Organizing files with time stamp $ddd'
            grep $ddd qqa.out | awk '{print $1}' > qqc_${ddd}.out
            Dimon -infile_list qqc_${ddd}.out -dicom_org -GERT_Reco \
                  -gert_create_dataset -gert_to3d_prefix ACQT_${ddd} -quit
          end
    
        As before, the find command gets all the DICOM files under the current
        In this case, the awk command also filters out images that are not square.
        The output of 'find' is piped into xargs to prevent creating a gigantic
        command line, since there are over 17,000 files in this directory tree.
    
        The uniq command finds files with unique time stamps, and the
        awk command filters out those lines that don't have more than 99
        such files.
    
        The foreach loop over variable ddd (the time stamp) creates a file list
        that matches the given value, then runs Dimon to create an AFNI dataset.
          [Not all of these datasets are actually useful, but it is easy to]
          [delete the ones that are not relevant to the research underway. ]
    
        Note the use of the '-dicom_org' option to Dimon to have it organize
        the files to be in the correct order -- otherwise, it would take the
        files in their filename alphabetical order, which is not always right.
        This ordering is done using various DICOM fields, including
          0054,1330  =  Image Index
          0020,0013  =  Instance Number
    
        This example solved a real problem with image files dumped from a PACS.
        You might have to change things around to solve your problem, but I
        hope that this sample script will give you an idea of how to start.
    
    ---------------------------
    --- RWCox - 15 Nov 2011 ---
