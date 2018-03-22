******
Dimon1
******

.. _ahelp_Dimon1:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Dimon1 - monitor real-time acquisition of DICOM image files
        (or GEMS 5.x I-files, as 'Imon')
    
        This program is intended to be run during a scanning session
        on a scanner, to monitor the collection of image files.  The
        user will be notified of any missing slice or any slice that
        is acquired out of order.
    
        When collecting DICOM files, it is recommended to run this
        once per run, only because it is easier to specify the input
        file pattern for a single run (it may be very difficult to
        predict the form of input filenames runs that have not yet
        occurred.
    
        This program can also be used off-line (away from the scanner)
        to organize the files, run by run.  If the DICOM files have
        a correct DICOM 'image number' (0x0020 0013), then Dimon can
        use the information to organize the sequence of the files, 
        particularly when the alphabetization of the filenames does
        not match the sequencing of the slice positions.  This can be
        used in conjunction with the '-GERT_Reco' option, which will
        write a script that can be used to create AFNI datasets.
    
        See the '-dicom_org' option, under 'other options', below.
    
        If no -quit option is provided (and no -no_wait), the user should
        terminate the program when it is done collecting images according
        to the input file pattern.
    
        Dimon can be terminated using <ctrl-c>.
    
      ---------------------------------------------------------------
      comments for using Dimon with various image file types
    
         DICOM : this is the intended and default use
                 - provide at least -infile_prefix
    
         GEMS 5x. : GE Medical Systems I-files
                 - requires -start_dir and -file_type GEMS
                 - works as the original Imon program
    
         AFNI : AFNI/NIfTI volume datasets
                 - requires -file_type AFNI
                 - use -sp to specify slice timing pattern
                 - if datasets are 4D, please use rtfeedme
    
      ---------------------------------------------------------------
      realtime notes for running afni remotely:
    
        - The afni program must be started with the '-rt' option to
          invoke the realtime plugin functionality.
    
        - If afni is run remotely, then AFNI_TRUSTHOST will need to be
          set on the host running afni.  The value of that variable
          should be set to the IP address of the host running Dimon1.
          This may set as an environment variable, or via the .afnirc
          startup file.
    
        - The typical default security on a Linux system will prevent
          Dimon1 from communicating with afni on the host running afni.
          The iptables firewall service on afni's host will need to be
          configured to accept the communication from the host running
          Dimon1, or it (iptables) will need to be turned off.
      ---------------------------------------------------------------
      usage: Dimon1 [options] -infile_prefix PREFIX
         OR: Dimon1 [options] -infile_pattern "PATTERN"
         OR: Dimon1 [options] -infile_list FILES.txt
    
      ---------------------------------------------------------------
      notes regarding Siemens mosaic images:
    
        - Final run slices will be reported as 1 (since there is only 1
          actual image), but mos_nslices will show the mosaic slice count.
    
        - Acquisition timing for the slices will depend on the number of
          slices (parity), as well as the mosiac ordering.  So users may
          need to rely on reading slice timing from the DICOM headers.
    
        - If slice timing is detected, 
    
      ---------------------------------------------------------------
      examples:
    
      A. no real-time options:
    
        Dimon1 -infile_prefix   s8912345/i
        Dimon1 -infile_pattern 's8912345/i*'
        Dimon1 -infile_list     my_files.txt
        Dimon1 -help
        Dimon1 -infile_prefix   s8912345/i  -quit
        Dimon1 -infile_prefix   s8912345/i  -nt 120 -quit
        Dimon1 -infile_prefix   s8912345/i  -debug 2
        Dimon1 -infile_prefix   s8912345/i  -dicom_org -GERT_Reco -quit
    
      A2. investigate a list of files: 
    
        Dimon1 -infile_pattern '*' -dicom_org -show_sorted_list -quit
    
      A3. save a sorted list of files and check it later: 
    
        Dimon1 -infile_prefix data/im -dicom_org -save_file_list sorted.files
        Dimon1 -infile_list sorted.files ... 
    
      B. for GERT_Reco:
    
        Dimon1 -infile_prefix run_003/image -gert_create_dataset
        Dimon1 -infile_prefix run_003/image -dicom_org -GERT_Reco -no_wait
        Dimon1 -infile_prefix 'run_00[3-5]/image' -GERT_Reco -quit
        Dimon1 -infile_prefix anat/image -GERT_Reco -no_wait
        Dimon1 -infile_prefix epi_003/image -dicom_org -no_wait \
              -GERT_Reco -gert_to3d_prefix run3 -gert_nz 42
    
      B2. Deal with Philips data (names are not sorted, and image numbers
          are in slice-major order).  Sort by acq time, then inst num.
          See -sort_by_acq_time in help output for details.
    
        Dimon1 -infile_pattern 'data/*.dcm' -GERT_Reco -quit \
              -use_last_elem -use_slice_loc -dicom_org -sort_by_acq_time
    
      B2. Simple examples for NIH scanners (GE or Siemens).
    
          o  create GERT_Reco script to put data into AFNI format
          o  create GERT_Reco script AND execute it (running to3d)
             (-gert_create_dataset implies -GERT_Reco and -quit)
          o  create and execute script, but make a NIfTI dataset
          o  also, store the datasets under a 'MRI_dsets' directory
    
        Dimon1 -infile_pattern 'mr_0015/*.dcm' -GERT_Reco -quit 
        Dimon1 -infile_prefix 'mr_0003/image' -gert_create_dataset
        Dimon1 -infile_pattern 'mr_0003/*.dcm' -gert_create_dataset
              -gert_write_as_nifti 
        Dimon1 -infile_pattern 'mr_0003/*.dcm' -gert_create_dataset
              -gert_outdir MRI_dsets -gert_write_as_nifti
    
      C. with real-time options:
    
        Dimon1 -infile_prefix s8912345/i -rt 
    
        Dimon1 -infile_pattern 's*/i*' -rt 
        Dimon1 -infile_pattern 's*/i*' -rt -nt 120
        Dimon1 -infile_pattern 's*/i*' -rt -quit
        Dimon1 -infile_prefix s8912345/i -rt -num_chan 2 -quit
    
        ** detailed real-time example:
    
        Dimon1                                    \
           -infile_pattern 's*/i*'               \
           -rt -nt 120                           \
           -host some.remote.computer            \
           -rt_cmd "PREFIX 2005_0513_run3"     \
           -num_slices 32                        \
           -max_quiet_trs 3                      \
           -sleep_frac 0.4                       \
           -quit                                 
    
        This example scans data starting from directory 003, expects
        120 repetitions (TRs), and invokes the real-time processing,
        sending data to a computer called some.remote.computer.name
        (where afni is running, and which considers THIS computer to
        be trusted - see the AFNI_TRUSTHOST environment variable).
        The time to wait for new data is 1.1*TR, and 32 slices are
        required for a volume
    
        Note that -num_slices can be important in a real-time setup,
        as scanners do not always write the slices in order.   Slices
        from volume #1 can appear on disk before all slices from volume
        #0, in which case Dimon might determine an incorrect number of
        slices per volume.
    
      -------------------------------------------
        Multiple DRIVE_AFNI commands are passed through '-drive_afni'
        options, one requesting to open an axial image window, and
        another requesting an axial graph, with 160 data points.
    
        Also, '-drive_wait' options may be used like '-drive_afni',
        except that the real-time plugin will wait until the first new
        volume is processed before executing those DRIVE_AFNI commands.
        One advantage of this is opening an image window for a dataset
        _after_ it is loaded, allowing afni to approriately set the
        window size.
    
        See README.driver for acceptable DRIVE_AFNI commands.
    
        Also, multiple commands specific to the real-time plugin are
        passed via '-rt_cmd' options.  The PREFIX command sets the
        prefix for the datasets output by afni.  The GRAPH_XRANGE and
        GRAPH_YRANGE commands set the graph dimensions for the 3D
        motion correction graph (only).  And the GRAPH_EXPR command
        is used to replace the 6 default motion correction graphs with
        a single graph, according to the given expression, the square
        root of the average squared entry of the 3 rotation params,
        roll, pitch and yaw, ignoring the 3 shift parameters, dx, dy
        and dz.
    
        See README.realtime for acceptable DRIVE_AFNI commands.
    
      example D (drive_afni):
    
        Dimon1                                                   \
           -infile_pattern 's*/i*.dcm'                         \
           -nt 160                                             \
           -rt                                                 \
           -host some.remote.computer.name                     \
           -drive_afni 'OPEN_WINDOW axialimage'                \
           -drive_afni 'OPEN_WINDOW axialgraph pinnum=160'     \
           -rt_cmd 'PREFIX eat.more.cheese'                    \
           -rt_cmd 'GRAPH_XRANGE 160'                          \
           -rt_cmd 'GRAPH_YRANGE 1.02'                         \
           -rt_cmd 'GRAPH_EXPR sqrt(d*d+e*e+f*f)'
    
      -------------------------------------------
    
      example E (drive_wait):
    
        Close windows and re-open them after data has arrived.
    
        Dimon                                                    \
           -infile_prefix EPI_run1/8HRBRAIN                      \
           -rt                                                   \
           -drive_afni 'CLOSE_WINDOW axialimage'                 \
           -drive_afni 'CLOSE_WINDOW sagittalimage'              \
           -drive_wait 'OPEN_WINDOW axialimage geom=+20+20'      \
           -drive_wait 'OPEN_WINDOW sagittalimage geom=+520+20'  \
           -rt_cmd 'PREFIX brie.would.be.good'                   \
    
      -------------------------------------------
      example F (for testing complete real-time system):
    
        ** consider AFNI_data6/realtime.demos/demo.2.fback.*
    
        Use Dimon to send volumes to afni's real-time plugin, simulating
        TR timing with Dimon's -pause option.  Motion parameters and ROI
        averages are then sent on to realtime_receiver.py (for subject
        feedback).
        
        a. Start afni in real-time mode, but first set some environment
           variables to make it explicit what might be set in the plugin.
           Not one of these variables is actually necessary, but they 
           make the process more scriptable.
        
           See Readme.environment for details on any variable.
        
               setenv AFNI_TRUSTHOST              localhost
               setenv AFNI_REALTIME_Registration  3D:_realtime
               setenv AFNI_REALTIME_Graph         Realtime
               setenv AFNI_REALTIME_MP_HOST_PORT  localhost:53214
               setenv AFNI_REALTIME_SEND_VER      YES
               setenv AFNI_REALTIME_SHOW_TIMES    YES
               setenv AFNI_REALTIME_Mask_Vals     ROI_means
        
               afni -rt
        
           Note: in order to send ROI averages per TR, the user must
                 choose a mask in the real-time plugin.
        
        b. Start realtime_receiver.py to show received data.
        
               realtime_receiver.py -show_data yes
        
        c. Run Dimon from the AFNI_data3 directory, in real-time mode,
           using a 2 second pause to simulate the TR.  Dicom images are
           under EPI_run1, and the files start with 8HRBRAIN.
        
               Dimon -rt -pause 2000 -infile_prefix EPI_run1/8HRBRAIN
        
           Note that Dimon can be run many times at this point.
    
        --------------------
    
        c2. alternately, set some env vars via Dimon
    
             Dimon -rt -pause 2000 -infile_prefix EPI_run1/8          \
               -drive_afni 'SETENV AFNI_REALTIME_Mask_Vals=ROI_means' \
               -drive_afni 'SETENV AFNI_REALTIME_SEND_VER=Yes'        \
               -drive_afni 'SETENV AFNI_REALTIME_SHOW_TIMES=Yes'
    
           Note that plugout_drive can also be used to set vars at
           run-time, though plugouts must be enabled to use it.
    
    
      -------------------------------------------
      example G: when reading AFNI datasets
    
        Note that single-volume AFNI datasets might not contain the.
        TR and slice timing information (since they are not considered
        to be time series).  So it may be necessary to specify such
        information on the command line.
    
        Dimon1 -rt                                                  \
           -infile_pattern EPI_run1/vol.*.HEAD                     \
           -file_type AFNI -sleep_vol 1000 -sp alt+z -tr 2.0 -quit
    
      ---------------------------------------------------------------
      notes:
    
        - Once started, unless the '-quit' option is used, this
          program exits only when a fatal error occurs (single
          missing or out of order slices are not considered fatal).
          Otherwise, it keeps waiting for new data to arrive.
    
          With the '-quit' option, the program will terminate once
          there is a significant (~2 TR) pause in acquisition.
    
        - To terminate this program, use <ctrl-c>.
    
      ---------------------------------------------------------------
      main options:
    
        For DICOM images, either -infile_pattern or -infile_prefix
        is required.
    
        -infile_pattern PATTERN : specify pattern for input files
    
            e.g. -infile_pattern 'run1/i*.dcm'
    
            This option is used to specify a wildcard pattern matching
            the names of the input DICOM files.  These files should be
            sorted in the order that they are to be assembled, i.e.
            when the files are sorted alphabetically, they should be
            sequential slices in a volume, and the volumes should then
            progress over time (as with the 'to3d' program).
    
            The pattern for this option must be within quotes, because
            it will be up to the program to search for new files (that
            match the pattern), not the shell.
    
        -infile_prefix PREFIX   : specify prefix matching input files
    
            e.g. -infile_prefix run1/i
    
            This option is similar to -infile_pattern.  By providing
            only a prefix, the user need not use wildcard characters
            with quotes.  Using PREFIX with -infile_prefix is
            equivalent to using 'PREFIX*' with -infile_pattern (note
            the needed quotes).
    
            Note that it may not be a good idea to use, say 'run1/'
            for the prefix, as there might be a readme file under
            that directory.
    
            Note also that it is necessary to provide a '/' at the
            end, if the prefix is a directory (e.g. use run1/ instead
            of simply run1).
    
        -infile_list MY_FILES.txt : filenames are in MY_FILES.txt
    
            e.g. -infile_list subject_17_files
    
            If the user would rather specify a list of DICOM files to
            read, those files can be enumerated in a text file, the
            name of which would be passed to the program.
    
      ---------------------------------------------------------------
      real-time options:
    
        -rt                : specify to use the real-time facility
    
            With this option, the user tells 'Dimon1' to use the real-time
            facility, passing each volume of images to an existing
            afni process on some machine (as specified by the '-host'
            option).  Whenever a new volume is acquired, it will be
            sent to the afni program for immediate update.
    
            Note that afni must also be started with the '-rt' option
            to make use of this.
    
            Note also that the '-host HOSTNAME' option is not required
            if afni is running on the same machine.
    
        -drive_afni CMND   : send 'drive afni' command, CMND
    
            e.g.  -drive_afni 'OPEN_WINDOW axialimage'
    
            This option is used to pass a single DRIVE_AFNI command
            to afni.  For example, 'OPEN_WINDOW axialimage' will open
            such an axial view window on the afni controller.
    
            Note: the command 'CMND' must be given in quotes, so that
                  the shell will send it as a single parameter.
    
            Note: this option may be used multiple times.
    
            See README.driver for more details.
    
        -drive_wait CMND   : send delayed 'drive afni' command, CMND
    
            e.g.  -drive_wait 'OPEN_WINDOW axialimage'
    
            This option is used to pass a single DRIVE_AFNI command
            to afni.  For example, 'OPEN_WINDOW axialimage' will open
            such an axial view window on the afni controller.
    
            This has the same effect as '-drive_afni', except that
            the real-time plugin will wait until the next completed
            volume to execute the command.
    
            An example of where this is useful is so that afni 'knows'
            about a new dataset before opening the given image window,
            allowing afni to size the window appropriately.
    
        -fast              : process data very quickly
    
            short for:  -sleep_init 50 -sleep_vol 50
    
        -host HOSTNAME     : specify the host for afni communication
    
            e.g.  -host mycomputer.dot.my.network
            e.g.  -host 127.0.0.127
            e.g.  -host mycomputer
            the default host is 'localhost'
    
            The specified HOSTNAME represents the machine that is
            running afni.  Images will be sent to afni on this machine
            during the execution of 'Dimon1'.
    
            Note that the environment variable AFNI_TRUSTHOST must be
            set on the machine running afni.  Set this equal to the
            name of the machine running Imon (so that afni knows to
            accept the data from the sending machine).
    
        -num_chan CHANNELS : specify number of channels to send over
    
            e.g.  -num_chan 8
    
            This option tells the realtime plugin how many channels to
            break incoming data into.  Each channel would then get its
            own dataset.
    
            Note that this simply distributes the data as it is read
            across multiple datasets.  If 12 volumes are seen in some
            directory and -num_chan 2 is specified, then volumes 0, 2,
            4, 6, 8 and 10 would go to one dataset (e.g. channel 1),
            while volumes 1,3,5,7,9,11 would go to another.
    
            A sample use might be for multi-echo data.  If echo pairs
            appear to Dimon sequentially over the TRs, then -num_chan
            could be used to send each echo type to its own dataset.
            This is why the option was added, for J Evans.
    
            Currently, -num_chan only affects the realtime use.
    
        -pause TIME_IN_MS : pause after each new volume
    
            e.g.  -pause 200
    
            In some cases, the user may wish to slow down a real-time
            process.  This option will cause a delay of TIME_IN_MS
            milliseconds after each volume is found.
    
        -rev_byte_order   : pass the reverse of the BYTEORDER to afni
    
            Reverse the byte order that is given to afni.  In case the
            detected byte order is not what is desired, this option
            can be used to reverse it.
    
            See the (obsolete) '-swap' option for more details.
    
        -rt_cmd COMMAND   : send COMMAND(s) to realtime plugin
    
            e.g.  -rt_cmd 'GRAPH_XRANGE 120'
            e.g.  -rt_cmd 'GRAPH_XRANGE 120 \n GRAPH_YRANGE 2.5'
    
            This option is used to pass commands to the realtime
            plugin.  For example, 'GRAPH_XRANGE 120' will set the
            x-scale of the motion graph window to 120 (repetitions).
    
            Note: the command 'COMMAND' must be given in quotes, so
            that the shell will send it as a single parameter.
    
            Note: this option may be used multiple times.
    
            See README.realtime for more details.
    
        -show_sorted_list  : display -dicom_org info and quit
    
            After the -dicom_org has taken effect, display the list
            of run index, image index and filenames that results.
            This option can be used as a simple review of the files
            under some directory tree, say.
    
            See the -show_sorted_list example under example A2.
    
        -sleep_init MS    : time to sleep between initial data checks
    
            e.g.  -sleep_init 500
    
            While Dimon searches for the first volume, it checks for
            files, pauses, checks, pauses, etc., until some are found.
            By default, the pause is approximately 3000 ms.
    
            This option, given in milliseconds, will override that
            default time.
    
            A small time makes the program seem more responsive.  But
            if the time is too small, and no new files are seen on
            successive checks, Dimon may think the first volume is
            complete (with too few slices).
    
            If the minimum time it takes for the scanner to output
            more slices is T, then 1/2 T is a reasonable -sleep_init
            time.  Note: that minimum T had better be reliable.
    
            The example shows a sleep time of half of a second.
    
            See also -fast.
    
        -sleep_vol MS     : time to sleep between volume checks
    
            e.g.  -sleep_vol 1000
    
            When Dimon finds some volumes and there still seems to be
            more to acquire, it sleeps for a while (and outputs '.').
            This option can be used to specify the amount of time it
            sleeps before checking again.  The default is 1.5*TR.
    
            The example shows a sleep time of one second.
    
            See also -fast.
    
        -sleep_frac FRAC  : new data search, fraction of TR to sleep
    
            e.g.  -sleep_frac 0.5
    
            When Dimon finds some volumes and there still seems to be
            more to acquire, it sleeps for a while (and outputs '.').
            This option can be used to specify the amount of time it
            sleeps before checking again, as a fraction of the TR.
            The default is 1.5 (as the fraction).
    
            The example shows a sleep time of one half of a TR.
    
        -swap  (obsolete) : swap data bytes before sending to afni
    
            Since afni may be running on a different machine, the byte
            order may differ there.  This option will force the bytes
            to be reversed, before sending the data to afni.
    
            ** As of version 3.0, this option should not be necessary.
               'Dimon1' detects the byte order of the image data, and then
               passes that information to afni.  The realtime plugin
               will (now) decide whether to swap bytes in the viewer.
    
               If for some reason the user wishes to reverse the order
               from what is detected, '-rev_byte_order' can be used.
    
        -zorder ORDER     : slice order over time
    
            e.g. -zorder alt
            e.g. -zorder seq
            the default is 'alt'
    
            This options allows the user to alter the slice
            acquisition order in real-time mode, similar to the slice
            pattern of the '-sp' option.  The main differences are:
                o  only two choices are presently available
                o  the syntax is intentionally different (from that
                   of 'to3d' or the '-sp' option)
    
            ORDER values:
                alt   : alternating in the Z direction (over time)
                seq   : sequential in the Z direction (over time)
    
      ---------------------------------------------------------------
      other options:
    
        -debug LEVEL       : show debug information during execution
    
            e.g.  -debug 2
            the default level is 1, the domain is [0,3]
            the '-quiet' option is equivalent to '-debug 0'
    
        -dicom_org         : organize files before other processing
    
            e.g.  -dicom_org
    
            When this flag is set, the program will attempt to read in
            all files subject to -infile_prefix or -infile_pattern,
            determine which are DICOM image files, and organize them
            into an ordered list of files per run.
    
            This may be necessary since the alphabetized list of files
            will not always match the sequential slice and time order
            (which means, for instance, that '*.dcm' may not list
            files in the correct order.
    
            In this case, if the DICOM files contain a valid 'image
            number' field (0x0020 0013), then they will be sorted
            before any further processing is done.
    
            Notes:
    
            - This does not work in real-time mode, since the files
              must all be organized before processing begins.
    
            - The DICOM images need valid 'image number' fields for
              organization to be possible (DICOM field 0x0020 0013).
    
            - This works will in conjunction with '-GERT_Reco', to
              create a script to make AFNI datasets.  There will be
              a single file per run that contains the image filenames
              for that run (in order).  This is fed to 'to3d'.
    
            - This may be used with '-save_file_list', to store the
              list of sorted filenames in an output file.
    
            - The images can be sorted in reverse order using the
              option, -rev_org_dir.
    
        -epsilon EPSILON   : specify EPSILON for 'equality' tests
    
            e.g.  -epsilon 0.05
            the default is 0.01
    
            When checking z-coordinates or differences between them
            for 'equality', a check of (difference < EPSILON) is used.
            This option lets the user specify that cutoff value.
    
        -file_type TYPE    : specify type of image files to be read
    
            e.g.  -file_type AFNI
            the default is DICOM
    
            Dimon will currently process GEMS 5.x or DICOM files
            (single slice or Siemens mosaic).
    
            possible values for TYPE:
    
               GEMS      : GE Medical Systems GEMS 5.x format
               DICOM     : DICOM format, possibly Siemens mosaic
               AFNI      : AFNI or NIfTI formatted datasets
    
        -help              : show this help information
    
        -hist              : display a history of program changes
    
        -max_images NUM    : limit on images (slices per volume)
    
            e.g.  -max_images 256
            default = 3000
    
            This variable is in case something is very messed up with
            the data, and prevents the program from continuing after
            failing to find a volume in this number of images.
    
        -max_quiet_trs TRS : max number of TRs without data (if -quit)
    
            e.g.  -max_quiet_trs 4
            default = 2
    
            This variable is to specify the number of TRs for which
            having no new data is okay.  After this number of TRs, it
            is assumed that the run has ended.
    
            The TR (duration) comes from either the image files or
            the -tr option.
    
        -nice INCREMENT    : adjust the nice value for the process
    
            e.g.  -nice 10
            the default is 0, and the maximum is 20
            a superuser may use down to the minimum of -19
    
            A positive INCREMENT to the nice value of a process will
            lower its priority, allowing other processes more CPU
            time.
    
        -no_wait           : never wait for new data
    
            More forceful than -quit, when using this option, the
            program should never wait for new data.  This option
            implies -quit and is implied by -gert_create_dataset.
    
            This is appropriate to use when the image files have
            already been collected.
    
        -nt VOLUMES_PER_RUN : set the number of time points per run
    
            e.g.  -nt 120
    
            With this option, if a run stalls before the specified
            VOLUMES_PER_RUN is reached (notably including the first
            run), the user will be notified.
    
            Without this option, Dimon1 will compute the expected number
            of time points per run based on the first run (and will
            allow the value to increase based on subsequent runs).
            Therefore Dimon1 would not detect a stalled first run.
    
        -num_slices SLICES  : slices per volume must match this
    
            e.g.  -num_slices 34
    
            Setting this puts a restriction on the first volume
            search, requiring the number of slices found to match.
    
            This prevents odd failures at the scanner, which does not
            necessarily write out all files for the first volume
            before writing some file from the second.
    
        -quiet             : show only errors and final information
    
        -quit              : quit when there is no new data
    
            With this option, the program will terminate once a delay
            in new data occurs (an apparent end-of-run pause).
    
            This option is implied by -no_wait.
    
        -rev_org_dir       : reverse the sort in dicom_org
    
            e.g.  -rev_org_dir
    
            With the -dicom_org option, the program will attempt to
            organize the DICOM files with respect to run and image
            numbers.  Normally that is an ascending sort.  With this
            option, the sort is reversed.
    
            see also: -dicom_org
    
        -rev_sort_dir      : reverse the alphabetical sort on names
    
            e.g.  -rev_sort_dir
    
            With this option, the program will sort the input files
            in descending order, as opposed to ascending order.
    
        -save_file_list FILENAME : store the list of sorted files
    
            e.g.  -save_file_list dicom_file_list
    
            With this option the program will store the list of files,
            sorted via -dicom_org, in the output file, FILENAME.  The
            user may wish to have a separate list of the files.
    
            Note: this option requires '-dicom_org'.
    
        -sort_by_acq_time  : sort files by acquisition time
    
            e.g.  -dicom_org -sort_by_acq_time
    
            When this option is used with -dicom_org, the program will
            sort DICOM images according to:
               run, acq time, image index and image number
    
            For instance, Philips files may have 0020 0013 (Inst. Num)
            fields that are ordered as slice-major (volume minor).
            But since slice needs to be the minor number, Acquisition
            Time may be used for the major sort, before Instance Num.
            So sort first by Acquisition Num, then by Instance.
    
            Consider example B2.
    
        -sort_by_num_suffix : sort files according to numerical suffix
    
            e.g.  -sort_by_num_suffix
    
            With this option, the program will sort the input files
            according to the trailing '.NUMBER' in the filename.  This
            NUMBER will be evaluated as a positive integer, not via
            an alphabetic sort (so numbers need not be zero-padded).
    
            This is intended for use on interleaved files, which are
            properly enumerated, but only in the filename suffix.
            Consider a set of names for a single, interleaved volume:
    
              im001.1  im002.3  im003.5  im004.7  im005.9  im006.11
              im007.2  im008.4  im009.6  im010.8  im011.10
    
            Here the images were named by 'time' of acquisition, and
            were interleaved.  So an alphabetic sort is not along the
            slice position (z-order).  However the slice ordering was
            encoded in the suffix of the filenames.
    
            NOTE: the suffix numbers must be unique
    
        -start_file S_FILE : have Dimon1 process starting at S_FILE
    
            e.g.  -start_file 043/I.901
    
            With this option, any earlier I-files will be ignored
            by Dimon1.  This is a good way to start processing a later
            run, if it desired not to look at the earlier data.
    
            In this example, all files in directories 003 and 023
            would be ignored, along with everything in 043 up through
            I.900.  So 043/I.901 might be the first file in run 2.
    
        -tr TR             : specify the TR, in seconds
    
            e.g.  -tr 5.0
    
            In the case where volumes are acquired in clusters, the TR
            is different than the time needed to acquire one volume.
            But some scanners incorrectly store the latter time in the
            TR field.
            
            This option allows the user to override what is found in
            the image files, which is particularly useul in real-time
            mode, though is also important to have stored properly in
            the final EPI datasets.
    
            Here, TR is in seconds.
    
        -use_imon          : revert to Imon functionality
    
            ** This option is deprecated.
               Use -file_type GEMS, instead.
    
        -use_last_elem     : use the last elements when reading DICOM
    
            In some poorly created DICOM image files, some elements
            are listed incorrectly, before being listed correctly.
    
            Use the option to search for the last occurrence of each
            element, not necessarily the first.
    
        -use_slice_loc     : use REL Slice Loc for z offset
    
            REL Slice Location, 0020 1041, is sometimes used for the
            z offset, rather than Image Position.
            
            Use this option to set slice offsets according to SLoc.
    
        -version           : show the version information
    
      ---------------------------------------------------------------
      GERT_Reco options:
    
        -GERT_Reco        : output a GERT_Reco_dicom script
    
            Create a script called 'GERT_Reco_dicom', similar to the
            one that Ifile creates.  This script may be run to create
            the AFNI datasets corresponding to the I-files.
    
        -gert_create_dataset     : actually create the output dataset
    
            Execute any GERT_Reco script, creating the AFNI or NIfTI
            datasets.
    
            This option implies -GERT_Reco and -quit.
    
            See also -gert_write_as_nifti.
    
        -gert_filename FILENAME : save GERT_Reco as FILENAME
    
            e.g. -gert_filename gert_reco_anat
    
            This option can be used to specify the name of the script,
            as opposed to using GERT_Reco_dicom.
    
            By default, if the script is generated for a single run,
            it will be named GERT_Reco_dicom_NNN, where 'NNN' is the
            run number found in the image files.  If it is generated
            for multiple runs, then the default it to name it simply
            GERT_Reco_dicom.
    
        -gert_nz NZ        : specify the number of slices in a mosaic
    
            e.g. -gert_nz 42
    
            Dimon happens to be able to write valid to3d commands
            for mosaic (volume) data, even though it is intended for
            slices.  In the case of mosaics, the user must specify the
            number of slices in an image file, or any GERT_Reco script
            will specify nz as 1.
    
        -gert_outdir OUTPUT_DIR  : set output directory in GERT_Reco
    
            e.g. -gert_outdir subject_A7
            e.g. -od subject_A7
            the default is '-gert_outdir .'
    
            This will add '-od OUTPUT_DIR' to the @RenamePanga command
            in the GERT_Reco script, creating new datasets in the
            OUTPUT_DIR directory, instead of the 'afni' directory.
    
        -sp SLICE_PATTERN  : set output slice pattern in GERT_Reco
    
            e.g. -sp alt-z
            the default is 'alt+z'
    
            This options allows the user to alter the slice
            acquisition pattern in the GERT_Reco script.
    
            See 'to3d -help' for more information.
    
        -gert_to3d_prefix PREFIX : set to3d PREFIX in output script
    
            e.g. -gert_to3d_prefix anatomy
    
            When creating a GERT_Reco script that calls 'to3d', this
            option will be applied to '-prefix'.
    
            The default prefix is 'OutBrick_run_NNN', where NNN is the
            run number found in the images.
    
          * Caution: this option should only be used when the output
            is for a single run.
    
        -gert_write_as_nifti     : output dataset should be in NIFTI format
    
            By default, datasets created by the GERT_Reco script will be in 
            afni format.  Use this option to create them in NIfTI format,
            instead.  These merely appends a .nii to the -prefix option of
            the to3d command.
    
            See also -gert_create_dataset.
    
        -gert_quit_on_err : Add -quit_on_err option to to3d command
                            which has the effect of causing to3d to 
                            fail rather than come up in interactive
                            mode if the input has an error.
      ---------------------------------------------------------------
    
      Author: R. Reynolds - version 3.18 (August 3, 2015)
