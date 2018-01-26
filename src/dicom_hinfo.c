#include "mrilib.h"

int main(int argc, char **argv)
{
   char *str ;
   int ii, iarg=1 , nposn=-1 ;  /* default name position is first */
   int ntag=0 ; char **tag=NULL ;
   int full_entry=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: dicom_hinfo [options] fname [...] ~1~\n"
            "Prints selected information from the DICOM file 'fname' to stdout.\n"
            "Multiple files can be given on the command line; see the examples\n"
            "below for useful ideas.\n"
            "\n"
            "--------\n"
            "OPTIONS: ~2~\n"
            "--------\n"
            " -tag aaaa,bbbb = print the specified tag.\n"
            "                  -- multiple tags may follow the '-tag' option.\n"
            "                  -- a tag consists of 4 hexadecimal digits,\n"
            "                     followed by a comma, followed by 4 more\n"
            "                     hexadecimal digits\n"
            "                  -- any string that doesn't match this format\n"
            "                     will end the list of tags\n"
            "\n"
            " -namelast      = Put the filename last on each output line,\n"
            " *OR* -last       instead of first.\n"
            "\n"
            " -no_name       = Omit any filename output.\n"
            "\n"
            " -full_entry    = Output the full entry if it is more than\n"
            "                  one word or contains white space. If the entry is\n"
            "                  REALLY long, this may be truncated.\n"
            "\n"
            " ~4~\n"
            "* The purpose of this program is to be used in scripts to figure out\n"
            "  which DICOM files to process for various purposes -- see Example #2.\n"
            "\n"
            "* One line is output (to stdout) for each DICOM file that the program reads.\n"
            "* Files that can't be read as DICOM will be ignored (silently).\n"
            "* Tags that aren't found in a file will get their value printed as 'null'.\n"
            "\n"
            "* How do you know what hexadecimal tags you need?  You can start with using\n"
            "  dicom_hdr on a single file to get the full list of tags (with names) and\n"
            "  then experiment to see which tags can be used to meet your whims.\n"
            "\n"
            "* Some tags that might be useful for figuring out which DICOM files belong\n"
            "  together and which ones are from separate acquisitions:\n"
            "    0008,0030  =  Study Time  (might be the same for all images in one session)\n"
            "    0008,0031  =  Series Time (will usually be different between imaging runs)\n"
            "    0008,0032  =  Acquisition Time (might be different for EVERY file)\n"
            "    0018,0050  =  Slice Thickness\n"
            "    0020,0011  =  Series Number (if present, identifies different imaging runs)\n"
            "    0028,0010  =  Number of Rows\n"
            "    0028,0011  =  Number of Columns\n"
            "    0028,0030  =  Pixel Spacing\n"
            "  In the examples below, I use 0008,0031 as a way to distinguish between\n"
            "  different acquisitions in the same imaging session.  For the data used\n"
            "  here, the value of this tag was different for each distinct scan --\n"
            "  localizers, structural, EPI -- so it worked as good way to find the\n"
            "  break points between set of files that should go together.  However,\n"
            "  I have seen DICOM files that lacked this tag, so you might have to\n"
            "  experiment (using dicom_hdr) to find a good tag for this purpose.\n"
            "\n"
            "---------\n"
            "EXAMPLES: ~1~\n"
            "---------\n"
            "#1: The command below prints out the acquisition start time and the number\n"
            "    of rows for each valid DICOM file in the directories below the current one:\n"
            "\n"
            "      dicom_hinfo -tag 0008,0031 0028,0010 */*.dcm\n"
            "\n"
            "    One sample output line would be\n"
            "\n"
            "TASK-A/image-00102-004243.dcm 141255.882508 256\n"
            "\n"
            "---------\n"
            "#2: A more complicated example searches all the directories below the current one,\n"
            "    then prints out a list of summaries of what look like unique acquisitions.\n"
            "    This could be used to figure out what kind of data you have when someone gives\n"
            "    you a bunch of DICOM files with no obvious structure to their filenames.\n"
            "\n"
            "      find . -type f | xargs dicom_hinfo -tag 0008,0031 0028,0010 | uniq -f 1 -c\n"
            "\n"
            "    The output from the above example was\n"
            "\n"
            "   9 ./A/A/A/Z01 154116 256\n"
            "   9 ./A/A/A/Z10 154210 256\n"
            "  38 ./A/A/A/Z19 154245 64\n"
            " 126 ./A/A/C/Z05 154326 256\n"
            "6000 ./A/A/H/Z01 154854 64\n"
            "2400 ./A/J/D/Z21 155617 64\n"
            " 126 ./A/M/S/Z03 160228 256\n"
            "  40 ./A/M/W/Z25 160304 64\n"
            "  40 ./A/M/Y/Z13 160332 64\n"
            " 126 ./A/N/A/Z01 160404 256\n"
            " 126 ./A/N/E/Z23 160411 256\n"
            " 126 ./A/N/J/Z19 160417 256\n"
            "   1 ./A/N/O/Z15 161252 960\n"
            "   1 ./A/N/O/Z16 161403 640\n"
            "   9 ./A/N/O/Z17 150935 256\n"
            "   9 ./A/N/P/Z00 151039 256\n"
            "  37 ./A/N/P/Z10 151122 64\n"
            " 120 ./A/N/Q/Z21 151203 256\n"
            "6000 ./A/N/V/Z11 151624 64\n"
            "2400 ./A/W/S/Z05 153010 64\n"
            "\n"
            "    My goal was the find the structural and FMRI collections of images mixed\n"
            "    in with various localizers and other 'junk'.  Based on the above, it seems:\n"
            "      * the 126 files starting with ./A/A/C/Z05 are a structural set\n"
            "      * the 6000 files starting with ./A/A/H/Z01 are an FMRI set\n"
            "      * the 2400 files starting with ./A/J/D/Z21 are an FMRI set\n"
            "      * the 126 files starting with ./A/M/S/Z03 are a structural set\n"
            "    and so on.  This information makes it possible to extract the desired files\n"
            "    from the giant collection of un-informative filenames, create AFNI datasets\n"
            "    (using program Dimon and its '-infile_list' option appropriately), and then\n"
            "    look at them to make final decisions about what to keep.\n"
            "\n"
            "---------\n"
            "#3: Continuing the above example with actual creation of AFNI dataset\n"
            "    from the collection of files, a script (in csh syntax):\n"
            "\n"
            "      #!/bin/tcsh\n"
            "      \\rm -f qq*.out\n"
            "\n"
            "      find . -type f \\\n"
            "        | xargs dicom_hinfo -tag 0008,0031 0028,0010 0028,0011 \\\n"
            "        | awk '$3 == $4' >> qqa.out\n"
            "\n"
            "      uniq -f 1 -c qqa.out | awk '$1 > 99' > qqb.out\n"
            "\n"
            "      foreach ddd ( `cat qqb.out | awk '{print $3}'` )\n"
            "        echo 'Organizing files with time stamp $ddd'\n"
            "        grep $ddd qqa.out | awk '{print $1}' > qqc_${ddd}.out\n"
            "        Dimon -infile_list qqc_${ddd}.out -dicom_org -GERT_Reco \\\n"
            "              -gert_create_dataset -gert_to3d_prefix ACQT_${ddd} -quit\n"
            "      end\n"
            "\n"
            "    As before, the find command gets all the DICOM files under the current\n"
            "    In this case, the awk command also filters out images that are not square.\n"
            "    The output of 'find' is piped into xargs to prevent creating a gigantic\n"
            "    command line, since there are over 17,000 files in this directory tree.\n"
            "\n"
            "    The uniq command finds files with unique time stamps, and the\n"
            "    awk command filters out those lines that don't have more than 99\n"
            "    such files.\n"
            "\n"
            "    The foreach loop over variable ddd (the time stamp) creates a file list\n"
            "    that matches the given value, then runs Dimon to create an AFNI dataset.\n"
            "      [Not all of these datasets are actually useful, but it is easy to]\n"
            "      [delete the ones that are not relevant to the research underway. ]\n"
            "\n"
            "    Note the use of the '-dicom_org' option to Dimon to have it organize\n"
            "    the files to be in the correct order -- otherwise, it would take the\n"
            "    files in their filename alphabetical order, which is not always right.\n"
            "    This ordering is done using various DICOM fields, including\n"
            "      0054,1330  =  Image Index\n"
            "      0020,0013  =  Instance Number\n"
            "\n"
            "    This example solved a real problem with image files dumped from a PACS.\n"
            "    You might have to change things around to solve your problem, but I\n"
            "    hope that this sample script will give you an idea of how to start.\n"
            "\n"
            "---------------------------\n"
            "--- RWCox - 15 Nov 2011 ---\n"
            "---------------------------\n"
           );
     exit(0);
   }

   mainENTRY("dicom_hinfo main") ; machdep() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-last") == 0 || strcasecmp(argv[iarg],"-namelast") == 0 ){
       nposn = 1 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-no_name") == 0 ){
       nposn = 0 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-tag") == 0 ){
       char *ttt ;
       if( ++iarg >= argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       for( ; iarg < argc ; iarg++ ){
         ttt = argv[iarg] ;
         if( strlen(ttt) != 9 ) break ;
         if( ! ( isxdigit(ttt[0]) && isxdigit(ttt[1]) &&
                 isxdigit(ttt[2]) && isxdigit(ttt[3]) &&
                 isxdigit(ttt[5]) && isxdigit(ttt[6]) &&
                 isxdigit(ttt[7]) && isxdigit(ttt[8]) &&
                 (ttt[4] == ' ' || ttt[4] == ',')     ) ) break ;
         tag = (char **)realloc( tag , sizeof(char *)*(ntag+1) ) ;
         tag[ntag++] = strdup(argv[iarg]) ;
       }
       continue ;
     }

     if( strcasecmp(argv[iarg],"-full_entry") == 0 ){
       full_entry = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   if( iarg >= argc )
     ERROR_exit("No files on command line?!") ;

   if( ntag == 0 )
     WARNING_message("No tags given -- just echoing names of DICOM files") ;

   for( ii=iarg ; ii < argc ; ii++ ){
     if( full_entry == 0 )
        str = mri_dicom_hdrinfo( argv[ii] , ntag , tag , nposn ) ;
    else
        str = mri_dicom_hdrinfo_full( argv[ii] , ntag , tag , nposn ) ;

     if( str == NULL ) continue ;
     printf("%s\n",str) ;
     free(str) ;
   }

   exit(0) ;
}
