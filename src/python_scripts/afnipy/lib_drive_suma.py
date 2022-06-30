#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os
import copy

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL     
from afnipy import afni_base     as AB     
from afnipy import lib_apqc_tcsh as lat      # for str formatting

# ----------------------------------------------------------------------
# globals

g_help_string = """

Help string


"""

g_history = """
   history for: chauffeur_suma.py  

   0.0   Mar 17, 2022    - initial version
   0.01  Jun 30, 2022    - tweak opt/error handling
"""

# watch formatting of g_history
g_version = g_history.split('\n')[-2]

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

def make_abs_head_tail_from_fname(fname):
    '''
    Parameters
    ----------
    dirname : str
              relative or abs path

    Return
    ------
    head    : str
              head of abs path (i.e., abs path of fname dir)
    tail    : str
              tail of abs path (i.e., last item in path, just file)
    '''

    if not os.path.isfile(fname) :
        AB.WP("{} is not a file".format(fname))
        return '', ''

    apath = os.path.abspath(fname)
    head  = '/'.join(apath.split('/')[:-1])
    tail  = apath.split('/')[-1]

    if not os.path.isdir(head) :
        AB.WP("estimated '{}' is not a valid dir".format(head))
        return '', ''

    if not( len(tail) ):
        AB.WP("cannot find tail from dir: {}\n"
              "   abspath = {}".format(fname, apath))
        return '', ''

    return head, tail

def make_abs_path(fd):
    '''
    Parameters
    ----------
    fd      : str
              relative or abs path; can be dirname or fname

    Return
    ------
    abs_fd  : str
              the input dir/file as abs path
    fd_type : str
              descriptor of whether input was dir ('dir'), file ('file'),
              or other ('')
    '''

    abs_fd = os.path.abspath(fd)

    fd_type = ''
    if os.path.isfile(abs_fd) :
        fd_type = 'file'
    elif os.path.isdir(abs_fd) :
        fd_type = 'dir'

    return abs_fd, fd_type

def make_text_xvfb_frame():
    """Make incantation to get Xvfb started, following what is done in
@chauffeur_afni.

    This should just need to be displayed at the top of the drive suma
    script.

    """

    ### A comment on the $AFNI_DRIVE_OPTS_XVFB:
    # allow user to set extra options, such as "-nolisten inet6", in
    # case IPv6 is not configured; consider a test like:
    #     if -d /proc/net && [insert exclamation point] -f /proc/net/if_inet6
    #        then add: -nolisten inet6
    #     or maybe if -d /proc/sys/net/ipv6

    ### Another comment: the following is unused in SUMA driving:
    # set OW = "OPEN_WINDOW"

    text = """
# start the X virtual frame buffer on display, a la bob

if ( 1 ) then 
    if ( $?AFNI_DRIVE_OPTS_XVFB ) then
        set opts_xvfb = ( $AFNI_DRIVE_OPTS_XVFB )
    else
        set opts_xvfb = ( )
    endif

    set ranval = `count -dig 1 1 999999 R1`

    if ( $?xdisplay == 0 ) then
        set killX     = 1
        set ntry      = 1
        set Xnotfound = 1
        while( $Xnotfound )
            set xdisplay = `count -dig 1 3 999 R1`
            if ( -e /tmp/.X${xdisplay}-lock ) continue

            echo " -- trying to start Xvfb :${xdisplay} $opts_xvfb"
            Xvfb :${xdisplay} $opts_xvfb -screen 0 1024x768x24 &
            sleep 1

            jobs > zzerm.$ranval.txt
            grep -q Xvfb zzerm.$ranval.txt
            set Xnotfound = $status
            \\rm -f zzerm.$ranval.txt
            if ( $Xnotfound == 0 ) break ;

            @ ntry++
            if ( $ntry > 99 ) then
                echo "** ERROR: can't start Xvfb -- exiting"
                goto BAD_EXIT
            endif
        end
    endif

    setenv DISPLAY :${xdisplay}
endif
"""
    return text

def make_text_good_exit():
    """Simple exiting with correct code.

    """

    text = """
# ---------------------------------- 

GOOD_EXIT:
    # A la bob:  stop Xvfb if we started it ourselves
    if ( $?killX ) kill %1

    echo ""
    echo "++ DONE (good exit)"
    echo "   See:  ${opref}"
    echo ""
    exit 0
"""
    return text

def make_text_bad_exit():
    """Simple exiting with correct code.

    """

    text = """
# ---------------------------------- 

BAD_EXIT:
    # A la bob:  stop Xvfb if we started it ourselves
    if ( $?killX ) kill %1

    echo ""
    echo "++ DONE (bad exit): check for errors"
    echo ""
    exit 1
"""
    return text


def make_text_loop_hemi(pars, sleep=2, ntoggle_spec=0):
    """
    
    """

    text = """\\mkdir -p ${wdir}\n\n"""

    if 1 :
        cmd = """set list_imgs = ( )\n\n"""
        text+= cmd 

    text+= """foreach ii ( `seq 1 1 ${nspec}` )
    set spec = "${all_spec[${ii}]}"
    set hemi = "${all_hemi[${ii}]}"
    set ldv  = "${all_ldv[${ii}]}"
    """

    cmd = """
    set turns = ( "left" "right" )
    """
    text+= cmd 

    cmd = """
    # start SUMA with desired surface
    suma                                                    \\
        -npb   ${portnum}                                   \\
        -niml                                               \\
        -spec  "${surf_dir}/${spec}"                        \\
        -sv    "${surf_anat}"   &
    """
    text+= cmd

    cmd = """
    echo "++ sleep"
    sleep {sleep}
    """.format(sleep=sleep)
    text+= cmd

    cmd = """
    # set SUMA underlay props
    DriveSuma -echo_edu                                    \\
        -npb ${portnum}                                    \\
        -com surf_cont   -switch_cmap ngray20              \\
        -com surf_cont   -I_sb  0  -I_range -0.75 0.75     \\
                         -T_sb -1 
    """
    text+= cmd 

    if ntoggle_spec :
        str_toggle = """-key '.' """*ntoggle_spec
        cmd = """
    # toggle selected surface
    DriveSuma -echo_edu                                    \\
        -npb ${{portnum}}                                    \\
        -com viewer_cont {}
    """.format(str_toggle)
        text+= cmd 


    if 1 :
        cmd = """
    # Zoom in, crosshair off, node off, faceset off, label off
    DriveSuma -echo_edu                                                \\
        -npb ${portnum}                                                \\
        -com viewer_cont -key 'Z' -key 'Z' -key 'Z' -key 'Z' -key 'Z'  \\
        -com viewer_cont -key 'F3' -key 'F4' -key 'F5' -key 'F9'

    sleep 1
    """
        text+= cmd 

    #### !!!! ADD DSETS TO LOAD/VIEW HERE!
    ###  FIX SLABELS?
    if pars.dset_list_lh or pars.dset_list_rh :
        cmd = """
    if ( "${hemi}" == "lh" ) then
        foreach jj ( `seq 1 1 ${ndset_lh}` )
            set jjj    = `printf "%03d" $jj`
            set slabel = slab_"${hemi}"_${jjj}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -surf_label ${slabel}                \\
                -com surf_cont -load_dset ${all_dset_lh[$jj]}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -switch_surf ${slabel}               \\
                -com surf_cont -I_sb 7 -I_range -1 1                \\
                               -T_sb 8 -T_val 3.313                 \\
                               -switch_cmap Reds_and_Blues_Inv

        end
    else if ( "${hemi}" == "rh" ) then
        foreach jj ( `seq 1 1 ${ndset_rh}` )
            set jjj    = `printf "%03d" $jj`
            set slabel = slab_"${hemi}"_${jjj}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -surf_label ${slabel}                \\
                -com surf_cont -load_dset ${all_dset_rh[$jj]}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -switch_surf ${slabel}               \\
                -com surf_cont -I_sb 7 -I_range -1 1                \\
                               -T_sb 8 -T_val 3.313                 \\
                               -switch_cmap Reds_and_Blues_Inv

        end
    endif

    """
        text+= cmd



    for jj in range(1,3):
        if 1 :
            cmd = """
    # sagittal profile {jj}, SNAP
    set tmp1 = "tmp1_${{hemi}}_${{turns[{jj}]}}"
    DriveSuma -echo_edu                                            \\
        -npb ${{portnum}}                                            \\
        -com viewer_cont -autorecord "${{wdir}}/${{tmp1}}.jpg"         \\
        -com viewer_cont -key "Ctrl+${{turns[{jj}]}}"                   \\
        -com viewer_cont -key 'Ctrl+r'

    """.format(jj=jj)
            text+= cmd 

        # later, will add more options about not necessarily cropping
        # and/or concatenating
        if 1 :
            cmd = """
    cd ${{wdir}}

    # image proc: crop and clean
    set tmp2 = "tmp2_${{hemi}}_${{turns[{jj}]}}.jpg"
    2dcat -echo_edu                                                \\
        -autocrop_atol 0                                           \\
        -nx 1 -ny 1                                                \\
        -prefix  ${{tmp2}}                                           \\
        ${{tmp1}}.*.jpg

    \\rm ${{tmp1}}.*.jpg

    # pad back a bit
    set tmp3 = "tmp3_${{hemi}}_${{turns[{jj}]}}.jpg"
    3dZeropad -echo_edu                                            \\
        -A 20 -P 20 -L 20 -R 20                                    \\
        -prefix ${{tmp3}}                                            \\
        ${{tmp2}}

    cd -

    # store image name for later concatenation
    set list_imgs = ( ${{list_imgs}} ${{wdir}}/${{tmp3}} )
    """.format(jj=jj)
            text+= cmd 

    #set tmp2 = "${{odir}}/${{oname}}_${{turns[{jj}]}}.${{img_ext}}"

    cmd = """
    # close suma
    @Quiet_Talkers -npb_val ${portnum}
    """
    text+= cmd + "\n"


    text+= """end
    """

    return text

def make_text_cat_images():
    """After both hemisphere loops
    """

    text = ''

    cmd = """
    set gwid = 5
    set gcol = ( 150 150 150 )
    set nx = ${#list_imgs}
    """

    text+= lat.commandize( cmd, cmdindent=0, 
                           ALIGNASSIGN=True, ALLEOL=False,
                           padpost=2 )

    cmd = '''
    echo "++ list of imgs: ${list_imgs}"
    '''

    text+= lat.commandize( cmd, padpost=2 )

    cmd = """
    2dcat              
    -gap     ${gwid}   
    -gap_col ${gcol}   
    -nx ${nx}          
    -ny 1              
    -prefix ${opref}   
    ${list_imgs}
    """

    text+= lat.commandize( cmd, padpost=1 )

    text+= """
if ( $status ) then
    goto BAD_EXIT
else
    goto GOOD_EXIT
endif
"""

    return text







# ----------------------------------------------------------------------------
# created ordered dictionaries of default

# note: the env and var distinctions come from whether the created
# tcsh script will use set or setenv.

# default env: environment (setenv) values 
DBENV = {
    'LIBGL_ALWAYS_SOFTWARE'  : 1,
    'AFNI_ENVIRON_WARNINGS'  : 'NO',
    'SUMA_Position_Original' : '50 50 800 500',
    'SUMA_DriveSumaMaxWait'  :     10,
    'SUMA_DriveSumaMaxCloseWait' :  20,
}

# default bvar: background (set) variables
DBVAR = {
    'use_xvfb'  : 1,
    'dispnum'   : -1,
    'portnum '  : '`afni -available_npb_quiet`',
}

# default svar: subject (set) variables
DSVAR = {
    'odir'      : None,
    'oname'     : 'image',
    'img_ext'   : 'jpg',
    'opref'     : '${odir}/${oname}.${img_ext}',
    'tmpcode'   : '`3dnewid -fun11`',
    'wdir'      : None,
}

# defaults for overlay viewing
DOLAY = {
    'I_sb'      : 0,
    'I_range'   : [None, None],        # always use 2 vals
    'T_sb'      : None,
    'T_val'     : None,
    'cmap'      : 'Reds_and_Blues_Inv',
}

# ----------------------------------------------------------------------------

class InOpts:
    """interface class for lib_drive_suma.py

    This class stores the command line options, and will be used to
    populate another class, containing the actually drive variables
    (some copied, others derived, from here).

    """

    def __init__(self, verb=0):
        # main variables
        self.status          = 0                      # exit value
        self.valid_opts      = None
        self.user_opts       = None

        # env
        self.all_benv               = DBENV           # top bkgd 'setenv' vars
        self.all_bvar               = DBVAR           # top bkgd 'set' vars
        self.all_svar               = DSVAR           # top subj 'set' vars

        # file info
        self.subj                   = None            # cd come from SUMA dir
        self.surf_spec_list         = []
        self.surf_anat              = None            
        self.surf_dir               = None

        self.dset_list_lh           = []
        self.dset_list_rh           = []

        self.ldv                    = None

        # general variables
        self.verb                   = verb

        # initialize valid_opts
        self.init_options()
        self.finalize_attributes()


    def init_options(self):
        self.valid_opts = OL.OptionList('valid opts')

        # required parameters 
        # **** CHANGE
        self.valid_opts.add_opt('-surf_spec', -1, [],
                                req=1, okdash=0,
            helpstr='spec file(s) for underlaying')

        self.valid_opts.add_opt('-surf_anat', 1, [], 
                                req=1,
            helpstr='anat file(s) accompanying surfs')

        self.valid_opts.add_opt('-prefix', 1, [], 
                                req=1,
            helpstr='set the output prefix for final image (without ext)')

        #self.valid_opts.add_opt('-dir_fs_suma', 1, [], 
        #    helpstr='(req) location of FS-created SUMA/ dir')

        self.valid_opts.add_opt('-subj', 1, [], 
            helpstr='(opt) can input subject ID (otherwise it will be guessed)')

        # opt params (primary)
        self.valid_opts.add_opt('-dset_lh', -1, [], 
            helpstr='main dataset(s) to display/overlay on the ' +
                    'left hemisphere.  For now, can be only one dset')

        self.valid_opts.add_opt('-dset_rh', -1, [], 
            helpstr='main dataset(s) to display/overlay on the ' +
                    'right hemisphere.  For now, can be only one dset')

        # env control **have to add user opts for these eventually...

        self.valid_opts.add_opt('-libgl_software', 1, [], 
            helpstr='use software to render (prob do not alter)? ' +
                    '(def: {})'.format(DBENV['LIBGL_ALWAYS_SOFTWARE']))

        self.valid_opts.add_opt('-position_original', 1, [], 
            helpstr='set the windows size and width ' +
                    '(see ENV: SUMA_Position_Original) (def: ' +
                    '{})'.format(DBENV['SUMA_Position_Original']))

        self.valid_opts.add_opt('-img_ext', 1, [], 
            helpstr='set the output extension for images ' +
                    '(see ENV: SUMA_AutoRecordPrefix) (def: ' +
                    '{})'.format(DSVAR['img_ext']))

        self.valid_opts.add_opt('-environ_warnings', 1, [], 
            helpstr='see warns when resetting ~/.afnirc env? ' +
                    '(see AENV: AFNI_ENVIRON_WARNINGS) (def: ' +
                    '{})'.format(DBENV['AFNI_ENVIRON_WARNINGS']))

        # short, terminal arguments
        self.valid_opts.add_opt('-help', 0, [],           \
            helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],           \
            helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [],\
            helpstr='display all valid options')
        self.valid_opts.add_opt('-show_valid_opts_only', 0, [],\
            helpstr='display all valid options, simple list')
        self.valid_opts.add_opt('-ver', 0, [],            \
            helpstr='display the current version number')

        self.valid_opts.add_opt('-verb', 1, [], 
            helpstr='set the verbose level (def: ' +
                    '{})'.format(self.verb))

        return 0

    def process_options(self):
        """
        return  1 on valid and exit        (e.g. -help)
        return  0 on valid and continue    (e.g. do main processing)
        return -1 on invalid               (bad things, panic, abort)
        """

        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # process terminal options without the option_list interface
        # (so that errors are not reported)

        # if no arguments are given, apply -help
        if len(sys.argv) <= 1 or '-help' in sys.argv or '-h' in sys.argv:
            print(g_help_string)
            return 1

        if '-hist' in sys.argv:
            print(g_history)
            return 1

        if '-show_valid_opts' in sys.argv:
            self.valid_opts.show(mesg='', verb=1)
            return 1

        if '-show_valid_opts_only' in sys.argv:
            self.valid_opts.show(mesg='', verb=0, show_count=0)
            return 1

        if '-ver' in sys.argv:
            print(g_version)
            return 1

        # ============================================================
        # read options specified by the user
        self.user_opts = OL.read_options(sys.argv, self.valid_opts)
        uopts = self.user_opts            # convenience variable
        if not uopts: return -1           # error condition

        # ------------------------------------------------------------
        # process non-chronological options, verb comes first

        val, err = uopts.get_type_opt(int, '-verb')
        if val != None and not err: self.verb = val

        # ------------------------------------------------------------
        # process options sequentially, to make them like a script

        for opt in uopts.olist:

            # main options
            if opt.name == '-surf_spec':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return 1
                for vvv in val:
                    if self.add_surf_spec(vvv): return 1

            if opt.name == '-surf_anat':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_surf_anat(val): return -1

            #if opt.name == '-dir_fs_suma':
            #    val, err = uopts.get_string_opt('', opt=opt)
            #    if val != None and err: return 1
            #    if self.set_dir_fs_suma(val): return 1

            if opt.name == '-subj':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_subj(val): return -1

            if opt.name == '-dset_lh':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                for vvv in val:
                    if self.add_hemi_dset(vvv, 'lh'): return -1

            if opt.name == '-dset_rh':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                for vvv in val:
                    if self.add_hemi_dset(vvv, 'rh'): return -1

            if opt.name == '-prefix':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_prefix(val): return -1

            if opt.name == '-img_ext':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_img_ext(val): return -1


            if opt.name == '-libgl_software':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")
            if opt.name == '-position_original':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")
            if opt.name == '-environ_warnings':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")

            # general options

            elif opt.name == '-verb':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: return -1
                else: self.verb = val
                continue

        return 0

    def finalize_attributes(self):

        if self.all_svar['odir'] == None :
            self.all_svar['odir'] = '${surf_dir}'

        # tmp working dir
        if self.all_svar['wdir'] == None :
            self.all_svar['wdir'] = '${odir}/__tmp_suma_${tmpcode}'

    def set_prefix(self, pref):
        """Set output prefix, which means setting odir and oname"""

        if '/' in pref :
            split_pref = pref.split('/')
            self.all_svar['odir']  = '/'.join(split_pref[:-1])
            self.all_svar['oname'] = split_pref[-1]
        else:
            self.all_svar['odir']  = '.'
            self.all_svar['oname'] = pref

    def set_img_ext(self, ext):
        """Set output ext. 

        Maybe someday check if it exists in a valid list"""

        self.img_ext = ext

    def set_subj(self, subj):

        """Set subject ID (might be guessed or input explicitly)"""

        self.subj = subj

        return 0

    def add_surf_spec(self, fff):
        """Verify spec file name, and get its abs path
        """

        if '/' in fff :
            split_fff = fff.split('/')
            tail = '/'.join(split_fff[:-1])
            head = split_fff[-1]
        else:
            tail = '.'
            head = fff

        tpath, ttype = make_abs_path( tail )
        hpath, htype = make_abs_path( fff )
        if ttype != 'dir' or htype != 'file' :
            print(ttype != 'dir')
            print(htype != 'file')
            AB.EP1("problem with spec file name {}".format(fff))

        if self.surf_dir != None :
            if self.surf_dir != tail :
                AB.WP("spec files do not have same suma dirs?")

        self.surf_dir = tail
        self.surf_spec_list.append( head )

        return 0

    def set_surf_anat(self, fname):
        """Verify anatomical dset file name, and get its abs path
        """

        fpath, ftype = make_abs_path(fname)
        if ftype != 'file' :
            AB.EP1("problem with surf_anat file name")

        self.surf_anat = fname

        return 0

    def add_hemi_dset(self, dset, hemi=''):
        """Load list of dsets (for overlay) for a given hemisphere (could be
        'lh' or 'rh')"""

        lhemi = ['lh', 'rh']
        if not(hemi in lhemi) :
            AB.EP1("** ERROR: hemi '{}' is not one of: {}"
                  "".format(hemi, ', '.join(lhemi)))

        A, B = make_abs_path(dset)
        if not(B == 'file') :
            AB.EP1("dset '{}' is not a valid file".format(dset))

        if hemi == 'lh' :
            self.dset_list_lh.append(dset)
        elif hemi == 'rh' :
            self.dset_list_rh.append(dset)
        else: # should never be reached
            AB.EP1("dset '{}'... not recognized hemi?".format(dset))

        return 0


# =========================================================================
# =========================================================================

class suma_chauffeur_pars:
    """interface class for lib_drive_suma.py

    This class stores the command line options, and will be used to
    populate another class, containing the actually drive variables
    (some copied, others derived, from here).

    """

    def __init__(self, cmd_opts=None, verb=0):

        # env
        self.all_benv               = {}              # top bkgd 'setenv' vars
        self.all_bvar               = {}              # top bkgd 'set' vars
        self.all_svar               = {}              # top subj 'set' vars

        # file info
        self.subj                   = None            # cd come from SUMA dir
        self.surf_spec_list         = None
        self.surf_anat              = None
        self.surf_dir               = None   
         
        self.surf_nspec             = None
        self.surf_list_std          = []              # len matches spec_list
        self.surf_list_ldv          = []              # len matches spec_list
        self.surf_list_hemi         = []              # len matches spec_list

        self.dset_list_lh           = []
        self.dset_list_rh           = []

        # general variables
        self.verb                   = verb

        # populate from command line opts, if given
        if cmd_opts != None :
            self.set_all_from_cmd(cmd_opts)
            self.finalize_surf()
            self.check_props_surf()


    def set_all_from_cmd(self, CO):
        print("set all from cmd")

        self.all_benv = CO.all_benv
        self.all_bvar = CO.all_bvar
        self.all_svar = CO.all_svar

        self.surf_dir       = CO.surf_dir
        self.surf_anat      = CO.surf_anat
        self.surf_spec_list = copy.deepcopy(CO.surf_spec_list)

        self.dset_list_lh   = copy.deepcopy(CO.dset_list_lh)
        self.dset_list_rh   = copy.deepcopy(CO.dset_list_rh)

        return 0

    def finalize_surf(self, verb=1):
        """Perform some surf_* specific tasks

        """

        self.surf_nspec = len(self.surf_spec_list) 

        # go through each spec, and get some important properties to
        # record in a list that will be equal length:
        # is_std, ldv, and hemisphere
        for ii in range(self.surf_nspec):
            spec = self.surf_spec_list[ii]
            split_spec = spec.split('.')
            if len(split_spec) < 2 :
                AB.EP1("no extension on spec file? can't parse")

            # is it standard? (tested more; recorded below)
            is_std = int(spec.startswith('std'))

            # if std, which mesh density? 
            if is_std :
                try:
                    N = split_spec[1]   # NB: this is a str, for convenience
                except:
                    N = "0"
                    is_std = 0
                self.surf_list_ldv.append(N)
            else:
                self.surf_list_ldv.append("0")

            # which hemi is it?
            if is_std :
                self.surf_list_hemi.append(split_spec[-2].split('_')[-1])
            else:
                FOUND = 0
                for x in ['both', 'lh', 'rh']:
                    if x in spec:
                        self.surf_list_hemi.append(x)
                        FOUND = 1
                        break
                if not(FOUND):
                    self.surf_list_hemi.append('NA')
            
            # record final guess of 'is_std'
            self.surf_list_std.append(is_std)

        if verb :
            std_as_str = [str(x) for x in self.surf_list_std]
            AB.IP("Surf list spec  : {}".format(', '.join(self.surf_spec_list)))
            AB.IP("Surf list hemis : {}".format(', '.join(self.surf_list_hemi)))
            AB.IP("Surf list is_std: {}".format(', '.join(std_as_str)))
            AB.IP("Surf list ldv   : {}".format(', '.join(self.surf_list_ldv)))



        return 0

    def check_props_surf(self):
        """Verify consistency of some properties

        """
        
        for ii in range(1, self.surf_nspec):
            if self.surf_list_std[ii] != self.surf_list_std[ii-1]:
                AB.EP1("spec files are a mix of std and non-std??")
            if self.surf_list_ldv[ii] != self.surf_list_ldv[ii-1]:
                AB.EP1("ldv is not constant across spec files?")

        return 0


