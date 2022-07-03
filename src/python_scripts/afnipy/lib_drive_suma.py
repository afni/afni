#!/usr/bin/env python

# python3 status: compatible

### NTS: may have to stick "axi" in fname for QC HTML building.  Or
### some other nomenclature to recognize these images.  Well, probably
### the latter.


# system libraries
import sys, os
import copy

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL     
from afnipy import afni_base     as AB     
from afnipy import lib_apqc_tcsh as lat         # for str formatting
from afnipy import lib_format_cmd_str as lfcs   # for str formatting

# ----------------------------------------------------------------------
# globals

g_help_string = """

Help string


"""

g_history = """
   history for: chauffeur_suma.py  

   0.0   Mar 17, 2022    - initial version
   0.01  Jun 30, 2022    - tweak opt/error handling
   0.02  Jun 30, 2022    - change how spec files and their dir are handled
   0.03  Jul 01, 2022    - better scripting of image making, now have either
                           8 images in a row (both hemi) or 4 (single hemi)
   0.04  Jul 01, 2022    - output partnered text file describing views
   0.05  Jul 01, 2022    - add in DULAY item control (variables for ulay)
   0.06  Jul 01, 2022    - add in DOLAY item control (variables for olay)
                           -> though most not used yet
   0.07  Jul 02, 2022    - more closely link ulay and olay opts with drive
                           cmds, and fix how env vars are set
   0.08  Jul 02, 2022    - elif for opt list;  add -xvfb_off
   0.09  Jul 03, 2022    - start adding -*_?_sb, -*_i_range, -*_t_val opts
   0.10  Jul 03, 2022    - opt to leave suma open (and moved portnum for this)
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

if ( ${use_xvfb} ) then 
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


def make_text_loop_hemi(pars, drive_ulay='', drive_olay='',
                        sleep=2, ntoggle_spec=0):
    """
    
    """

    text = """\\mkdir -p ${wdir}\n\n"""

    #if 1 :
    #    cmd = """set list_imgs = ( )\n\n"""
    #    text+= cmd 

    text+= """foreach ii ( `seq 1 1 ${nspec}` )
    set portnum = "`afni -available_npb_quiet`"

    set spec = "${all_spec[${ii}]}"
    set hemi = "${all_hemi[${ii}]}"
    set ldv  = "${all_ldv[${ii}]}"
    """

    cmd = """
    # ------------------ setup: mesh/underlay ------------------
    """
    text+= cmd 

    cmd = """
    # start SUMA with desired surface
    suma                                                    \\
        -npb   ${portnum}                                   \\
        -niml                                               \\
        -spec  "${surf_spec_dir}/${spec}"                   \\
        -sv    "${surf_vol}"   &
    """
    text+= cmd

    cmd = """
    echo "++ sleep"
    sleep {sleep}
    """.format(sleep=sleep)
    text+= cmd
    text+= '\n'

    # DriveSuma surf_cont opts for mesh/ulay comes from pre-built
    # command of default/user opts
    if drive_ulay :
        cmd = '''DriveSuma -echo_edu -npb ${portnum} '''
        cmd+= drive_ulay
        a, cmd = lfcs.afni_niceify_cmd_str( cmd, comment_start=' '*4,
                                            list_cmd_args=DS_CMD_ARGS )

    text+= cmd 
    text+= "\n"

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
    # ------------------ setup: window/view ------------------
    """
        text+= cmd 

        cmd = """
    # Zoom in, crosshair off, node off, faceset off, label off
    DriveSuma -echo_edu                                                \\
        -npb ${portnum}                                                \\
        -com viewer_cont -key 'Z' -key 'Z' -key 'Z' -key 'Z' -key 'Z'  \\
        -com viewer_cont -key 'F3' -key 'F4' -key 'F5' -key 'F9'

    sleep 1
    """
        text+= cmd 

    cmd = """
    # ------------------ hemisphere-dependent items ------------------

    if ( "${hemi}" == "lh" ) then
        # directions to turn, and enumeration for image labels+order
        set turns = ( "left" "right" "Shift+up" "Shift+down" )
        set nums  = ( 00 01 03 04 )
    """
    text+= cmd 

    if pars.dset_list_lh :
        cmd = """
        # setup: overlay 
        foreach jj ( `seq 1 1 ${ndset_lh}` )
            set jjj    = `printf "%03d" $jj`
            set slabel = slab_"${hemi}"_${jjj}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -surf_label ${slabel}                \\
                -com surf_cont -load_dset ${all_dset_lh[$jj]}
        """

        text+= cmd
        text+= '\n'
        
        if drive_olay :

            cmd = """DriveSuma -echo_edu -npb ${portnum}  
                          -com surf_cont -switch_surf ${slabel} """ 
            cmd+= drive_olay
            a, cmd = lfcs.afni_niceify_cmd_str( cmd, 
                                                comment_start=' '*12,
                                                list_cmd_args=DS_CMD_ARGS )

        cmd+= """
        end
        """
        text+= cmd 

    cmd = """
    else if ( "${hemi}" == "rh" ) then
        # directions to turn, and enumeration for image labels+order
        set turns = ( "Shift+up" "Shift+down" "left" "right" )
        set nums  = ( 02 05 06 07 )
    """
    text+= cmd 

    if pars.dset_list_rh :
        cmd = """
        # setup: overlay 
        foreach jj ( `seq 1 1 ${ndset_rh}` )
            set jjj    = `printf "%03d" $jj`
            set slabel = slab_"${hemi}"_${jjj}

            DriveSuma -echo_edu                                     \\
                -npb ${portnum}                                     \\
                -com surf_cont -surf_label ${slabel}                \\
                -com surf_cont -load_dset ${all_dset_rh[$jj]}
        """

        text+= cmd
        text+= '\n'
        
        if drive_olay :

            cmd = """DriveSuma -echo_edu -npb ${portnum}  
                          -com surf_cont -switch_surf ${slabel} """ 
            cmd+= drive_olay
            a, cmd = lfcs.afni_niceify_cmd_str( cmd, 
                                                comment_start=' '*12,
                                                list_cmd_args=DS_CMD_ARGS )

        cmd+= """
        end
        """
        text+= cmd 


    cmd = """
    endif  # done with hemisphere-dependent setup
    """
    text+= cmd 

    cmd = """
    # ------------------ make individual profile images ------------------
    """
    text+= cmd 

    cmd = """
    # go through each turn, snapshot, crop and pad
    foreach jj ( `seq 1 1 ${#turns}` )
        set labjj = ${nums[$jj]}_${hemi}_${turns[$jj]}  # label for jj-th view

        # SNAP
        set tmp1 = "tmp1_${labjj}"
        DriveSuma -echo_edu                                            \\
            -npb ${portnum}                                            \\
            -com viewer_cont -autorecord "${wdir}/${tmp1}.jpg"         \\
            -com viewer_cont -key "Ctrl+${turns[$jj]}"                 \\
            -com viewer_cont -key 'Ctrl+r'

        cd ${wdir}

        # image proc: crop
        set tmp2 = "tmp2_${labjj}.jpg"
        2dcat -echo_edu                                                \\
            -autocrop_ctol 0                                           \\
            -nx 1 -ny 1                                                \\
            -prefix  ${tmp2}                                           \\
            ${tmp1}.*.jpg

        # pad back a bit
        set tmp3 = "tmp3_${labjj}.jpg"
        3dZeropad -echo_edu                                            \\
            -A 5 -P 5 -L 20 -R 20                                      \\
            -prefix ${tmp3}                                            \\
            ${tmp2}

        # clean a bit
        \\rm ${tmp1}.*.jpg ${tmp2}

        cd -
    end
    """
    text+= cmd 

    cmd = """
    # close suma
    if ( ${do_quit_suma} || ${use_xvfb} ) then
        @Quiet_Talkers -npb_val ${portnum}
    endif
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
# ------------------ glue cor views, copy sag views ------------------
    """
    text+= cmd 

    # [PT] This set of conditions is semi-complicated.  Why is it here?
    # + Firstly, we have to merge images of very different dimensions
    #   (the cor vs sag views); 2dcat will pad the cor views a lot,
    #   which we don't want. So, we "preconcatenate" each cor pair,
    #   with the result just happening to have about the same dims as
    #   the sag view.
    # + Secondly, the structure is complicated, because we might have
    #   only rh views, or only lh views, or both---and we want the
    #   ordering in each case to be sensical and consistent.  Hence,
    #   the different scenarios based on number of images (both vs
    #   single hemi) and even knowing which hemi it is, if only one is
    #   used.
    # + In the future, I might make an adjunct program to do this.
    #   There are several inputs actually needed here, so I want to
    #   avoid doing that until everything else is settled.
    #   -> or maybe the Python prog here will just take care of this.

    cmd = """
# NB: this odd intermediate step is to keep all 2dcat calls between
# images of approx. the same dimensions, to reduce unwanted image padding

set list_imgs = ( ${wdir}/tmp3*jpg )
set text_info = ""

if ( ${#list_imgs} == 8 ) then
    # just copy the sag views
    foreach jj ( 1 2 7 8 )
        \cp ${list_imgs[$jj]}  ${list_imgs[$jj]:gas/tmp3/tmp4/}   
    end

    # glue together each coronal view pair
    set tmp4 = "${wdir}/tmp4_02_corA.jpg"
    2dcat                                             \\
        -nx 2 -ny 1                                   \\
        -prefix ${tmp4}                               \\
        ${list_imgs[3]} ${list_imgs[4]}

    set tmp4 = "${wdir}/tmp4_04_corP.jpg"
    2dcat                                             \\
        -nx 2 -ny 1                                   \\
        -prefix ${tmp4}                               \\
        ${list_imgs[5]} ${list_imgs[6]}

    set text_info = "views: left, ant, post, right"

else if ( ${#list_imgs} == 4 && ${hemi} == "lh" ) then
    # just copy the sag views
    foreach jj ( 1 2 )
        \cp ${list_imgs[$jj]}  ${list_imgs[$jj]:gas/tmp3/tmp4/}   
    end

    # glue together each coronal view pair
    set tmp4 = "${wdir}/tmp4_02_corA.jpg"
    2dcat                                             \\
        -nx 2 -ny 1                                   \\
        -prefix ${tmp4}                               \\
        ${list_imgs[3]} ${list_imgs[4]}

    set text_info = "views: lat, med, ant, post (lh)"

else if ( ${#list_imgs} == 4 && ${hemi} == "rh" ) then
    # just copy the sag views
    foreach jj ( 3 4 )
        \cp ${list_imgs[$jj]}  ${list_imgs[$jj]:gas/tmp3/tmp4/}   
    end

    # glue together each coronal view pair
    set tmp4 = "${wdir}/tmp4_08_corA.jpg"
    2dcat                                             \\
        -nx 2 -ny 1                                   \\
        -prefix ${tmp4}                               \\
        ${list_imgs[1]} ${list_imgs[2]}

    set text_info = "views: med, lat, ant, post (rh)"

else if ( ${#list_imgs} == 0 ) then
    echo "** ERROR: no images to concatenate?  Badness.  Quitting."
    exit 1

else 
    echo "+* WARNING: unrecognized number of images to deal with: ${#list_imgs}"
    echo "   will just copy all of them to concatenate. But note that I"
    echo "   don't know what they mean"

    foreach jj ( `seq 1 1 ${#list_imgs}` )
        \cp ${list_imgs[$jj]}  ${list_imgs[$jj]:gas/tmp3/tmp4/}   
    end

    set text_info = "views: unknown"
endif

    """
    text+= cmd


    cmd = """
# ------------------ final image concatenation ------------------
    """
    text+= lat.commandize( cmd, cmdindent=0, padpost=2 )

    cmd = """
    set gwid = 0
    set list_imgs = ( ${wdir}/tmp4*jpg )
    set nx = ${#list_imgs}
    """

    text+= lat.commandize( cmd, cmdindent=0, 
                           ALIGNASSIGN=True, ALLEOL=False,
                           padpost=1 )

    cmd = '''
# associated text file info
echo "${text_info}" > ${otxt}
    '''
    text+= cmd


    cmd = """
    2dcat              
    -gap     ${gwid}   
    -nx ${nx}          
    -ny 1              
    -prefix ${opref}   
    ${list_imgs}
    """

    text+= lat.commandize( cmd, indent=0, 
                           padpre=1, padpost=1 )

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
    'use_xvfb'     : 1,
    'do_quit_suma' : 1,
    'dispnum'      : -1,
}

# default svar: subject (set) variables
DSVAR = {
    'odir'      : None,
    'oname'     : 'image',
    'img_ext'   : 'jpg',
    'opref'     : '${odir}/${oname}.${img_ext}',
    'otxt'      : '${odir}/${oname}.txt',
    'tmpcode'   : '`3dnewid -fun11`',
    'wdir'      : None,
}

# defaults for underlay viewing
DULAY = {
    'switch_cmap' : 'ngray20',
    'I_sb'        : 0,
    'I_range'     : [-0.75, 0.75],        # always use 2 vals
    'T_sb'        : None,
    'T_val'       : None,
}

# defaults for overlay viewing
DOLAY = {
    'switch_cmap' : 'Reds_and_Blues_Inv',
    'I_sb'      : 0,
    'I_range'   : None,                 # will always have 2 vals if used
    'T_sb'      : -1,
    'T_val'     : None,
#    'I_sb'        : 7,
#    'I_range'     : [-1, 1],                 # will always have 2 vals if used
#    'T_sb'        : 8,
#    'T_val'       : 3.313,
}

# for nicefying string commands later; just pick a subset that we know
# might be used; could do a fuller set later.
DS_CMD_ARGS = ['echo_edu', '-npb', '-com'] # surf_cont']

def build_cmds_from_dict(ddd, var_pre='', build_drive_cmd=False, 
                         use_setenv=False):
    """From a dictionary ddd, build up a a new string, that contains a set
    of line-by-line commands (for a tcsh script).  Dictionary keys
    with value None are not used.

    This program can also build a DriveSuma command from the entered
    parameters, because that is useful with a couple dictionaries.
    When not building a DriveSuma cmd, then ignore the second string
    output.

    Parameters
    ----------
    ddd: dict
         input dictionary of items to write 'set ... = ...' (or setenv)
         tcsh syntax variable definitions for; often multiline
    var_pre: str
         default variable name is the key ddd; alternatively, one can 
         prepend a string to each variable name (useful for ulay and olay
         settings that otherwise overlap in name)
    build_drive_cmd: bool
         some of the ddd dicts are opts to provide to DriveSuma commands;
         if this opt is set to True, then build that command here at the 
         same time.  This is useful because if a key has value None, then
         we can skip its inclusion in the DriveSuma cmd.
    use_setenv: bool
         by default, the command string is variable assignment with
         'set ... = ...'; this opt will switch it to be 'setenv ... ...'.
         This opt should probably never be used with build_drive_cmd

    Return
    ------
    sss: str
         final output string of the one or more vars being set (tcsh syntax)
    ds_cmd: str
         the DriveSuma cmd string using these vars, if build_drive_cmd is True

    """

    if type(ddd) != dict :  return '', ''
    if not len(ddd) :       return '', ''

    if use_setenv and build_drive_cmd :
        AB.WP("Odd to use both 'use_setenv' and 'build_drive_cmd' together")

    sss    = ''
    ds_cmd = '-com surf_cont '
    count  = 0

    for dkey in ddd.keys():
        USE  = True
        # shell var name to be used
        dvar_name = '''{}{}'''.format(var_pre, dkey); 
        dvar = '${'+dvar_name+'}'
        # value to assign
        dval = ddd[dkey]

        if dval == None :
            USE = False
        if type(dval) == str :
            sitem = '''"{}"'''.format(dval)
            dvar  = '''"{}"'''.format(dvar)
        elif type(dval) == list :
            ritem = ' '.join([str(x) for x in dval])
            sitem = '''( {} )'''.format(ritem)
        else:
            sitem = '''{}'''.format(dval)

        if USE :
            count+= 1
            if use_setenv :
                sss+= '''setenv {:35s} {}\n'''.format(dvar_name, sitem)  
            else:
                sss+= '''set {} = {}\n'''.format(dvar_name, sitem)
            if build_drive_cmd :
                ds_cmd+= ''' -{} {} '''.format(dkey, dvar)

    if count == 0 :
        AB.WP("NB: no opts added to DriveSuma cmd; nullifying it")

    return sss, ds_cmd


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

        self.all_ulay               = DULAY           # underlay settings
        self.all_olay               = DOLAY           # overlay settings

        # file info
        self.subj                   = None            # cd come from SUMA dir
        self.surf_spec_inp          = []              # tmp only: may have paths
        self.surf_spec_list         = []              # final: only fnames
        self.surf_vol               = None            
        self.surf_spec_dir          = None

        self.dset_list_lh           = []
        self.dset_list_rh           = []

        self.ldv                    = None

        # general variables
        self.verb                   = verb

        # initialize valid_opts
        self.init_options()

    def init_options(self):
        self.valid_opts = OL.OptionList('valid opts')

        # required parameters 
        # **** CHANGE

        self.valid_opts.add_opt('-surf_spec', -1, [],
                                req=1, okdash=0,
            helpstr='(req) spec file(s) for underlaying; may or may not ' +
                    'have path')

        self.valid_opts.add_opt('-surf_vol', 1, [], 
                                req=1,
            helpstr='(req) anatomical file accompanying surfs; include ' +
                    'path, if nec')

        self.valid_opts.add_opt('-prefix', 1, [], 
                                req=1,
            helpstr='(req) set the output prefix for final image (without ext)')

        # opt params (primary)

        self.valid_opts.add_opt('-subj', 1, [], 
            helpstr='can input subject ID (otherwise it will be guessed)')

        self.valid_opts.add_opt('-surf_spec_dir', 1, [], 
            helpstr='way to specify dir holding surf_spec file(s)')

        self.valid_opts.add_opt('-dset_lh', -1, [], 
            helpstr='main dataset(s) to display/overlay on the ' +
                    'left hemisphere (now, only one)')

        self.valid_opts.add_opt('-dset_rh', -1, [], 
            helpstr='main dataset(s) to display/overlay on the ' +
                    'right hemisphere (now, only one)')

        # bkgd env: **have to add user opts for these eventually...

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

        # bkgd var: **have to add user opts for these eventually...

        self.valid_opts.add_opt('-xvfb_off', 0, [], 
            helpstr='turn off running SUMA in bkgd; GUI will pop up')

        self.valid_opts.add_opt('-quit_suma_off', 0, [], 
            helpstr='turn off quitting SUMA at end; only works with -xvfb_off')

        # ulay var: 

        self.valid_opts.add_opt('-ulay_i_sb', 1, [], 
            helpstr='switch ulay intensity to ISBth column (sub-brick) ' +
                    '(see DS: -I_sb) (def: ' +
                    '{})'.format(DULAY['I_sb']))

        # 1 or 2 vals
        self.valid_opts.add_opt('-ulay_i_range', -1, [], 
            helpstr='set ulay intensity range from IR0 to IR1; sym range ' +
                    'if 1 val ' +
                    '(see DS: -I_range) (def: ' +
                    '{})'.format(DULAY['I_range']))

        self.valid_opts.add_opt('-ulay_t_sb', 1, [], 
            helpstr='Switch ulay threshold to TSBth column (sub-brick); '
                    'set to -1 to turn off thr ' +
                    '(see DS: -T_sb) (def: ' +
                    '{})'.format(DULAY['T_sb']))

        self.valid_opts.add_opt('-ulay_t_val', 1, [], 
            helpstr='set threshold to THR; can append p or % for ' +
                    'pvalue or percentile thr ' +
                    '(see DS: -T_val) (def: ' +
                    '{})'.format(DULAY['T_val']))

        self.valid_opts.add_opt('-ulay_cmap', 1, [], 
            helpstr='switch ulay colormap to CMAP ' +
                    '(see DS: -switch_cmap) (def: ' +
                    '{})'.format(DULAY['switch_cmap']))

        # olay var: 

        self.valid_opts.add_opt('-olay_i_sb', 1, [], 
            helpstr='switch olay intensity to ISBth column (sub-brick) ' +
                    '(see DS: -I_sb) (def: ' +
                    '{})'.format(DOLAY['I_sb']))

        # 1 or 2 vals
        self.valid_opts.add_opt('-olay_i_range', -1, [], 
            helpstr='set olay intensity range from IR0 to IR1; sym range ' +
                    'if 1 val ' +
                    '(see DS: -I_range) (def: ' +
                    '{})'.format(DOLAY['I_range']))

        self.valid_opts.add_opt('-olay_t_sb', 1, [], 
            helpstr='Switch olay threshold to TSBth column (sub-brick); '
                    'set to -1 to turn off thr ' +
                    '(see DS: -T_sb) (def: ' +
                    '{})'.format(DOLAY['T_sb']))

        self.valid_opts.add_opt('-olay_t_val', 1, [], 
            helpstr='set threshold to THR; can append p or % for ' +
                    'pvalue or percentile thr ' +
                    '(see DS: -T_val) (def: ' +
                    '{})'.format(DOLAY['T_val']))

        self.valid_opts.add_opt('-olay_cmap', 1, [], 
            helpstr='switch olay colormap to CMAP ' +
                    '(see DS: -switch_cmap) (def: ' +
                    '{})'.format(DOLAY['switch_cmap']))


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

            elif opt.name == '-surf_vol':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_surf_vol(val): return -1

            elif opt.name == '-surf_spec_dir':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return 1
                if self.set_surf_spec_dir(val): return 1

            elif opt.name == '-subj':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_subj(val): return -1

            elif opt.name == '-dset_lh':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                for vvv in val:
                    if self.add_hemi_dset(vvv, 'lh'): return -1

            elif opt.name == '-dset_rh':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                for vvv in val:
                    if self.add_hemi_dset(vvv, 'rh'): return -1

            elif opt.name == '-prefix':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_prefix(val): return -1

            elif opt.name == '-img_ext':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_img_ext(val): return -1

            # bkgd env

            elif opt.name == '-libgl_software':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")
            elif opt.name == '-position_original':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")
            elif opt.name == '-environ_warnings':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                print("++ Sorry, ignoring you right now!")
                
            # bkgd var

            elif opt.name == '-xvfb_off' :
                self.all_bvar['use_xvfb'] = 0

            elif opt.name == '-quit_suma_off' :
                self.all_bvar['do_quit_suma'] = 0

            # ulay opts

            elif opt.name == '-ulay_i_sb':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_i_sb(val, 'ulay'): return -1

            elif opt.name == '-ulay_i_range':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                if self.set_i_range(val, 'ulay'): return -1

            elif opt.name == '-ulay_t_sb':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_t_sb(val, 'ulay'): return -1

            elif opt.name == '-ulay_t_val':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_t_val(val, 'ulay'): return -1

            elif opt.name == '-ulay_cmap':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_cmap(val, 'ulay'): return -1

            # olay opts

            elif opt.name == '-olay_i_sb':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_i_sb(val, 'olay'): return -1

            elif opt.name == '-olay_i_range':
                val, err = uopts.get_string_list('', opt=opt)
                if val != None and err: return -1
                if self.set_i_range(val, 'olay'): return -1

            elif opt.name == '-olay_t_sb':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_t_sb(val, 'olay'): return -1

            elif opt.name == '-olay_t_val':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_t_val(val, 'olay'): return -1

            elif opt.name == '-olay_cmap':
                val, err = uopts.get_string_opt('', opt=opt)
                if val != None and err: return -1
                if self.set_cmap(val, 'olay'): return -1


            # general options

            elif opt.name == '-verb':
                val, err = uopts.get_type_opt(int, '', opt=opt)
                if val != None and err: return -1
                else: self.verb = val
                continue

        # finalize/merge some info about spec files and paths
        self.merge_surf_spec_info()
        self.finalize_attributes()

        return 0

    def finalize_attributes(self):

        if self.all_svar['odir'] == None :
            self.all_svar['odir'] = '${surf_spec_dir}'

        # tmp working dir
        if self.all_svar['wdir'] == None :
            self.all_svar['wdir'] = '${odir}/__tmp_suma_${tmpcode}'

    def set_cmap(self, cmap, dset=None):
        """Set cmap as the colormap, for either dset = 'ulay' or 'olay' (must
        be given).

        """

        if dset == None :     
            AB.EP1("Must specify 'ulay' or 'olay' to set i_sb index")
        elif dset == 'ulay' :
            self.all_ulay['switch_cmap'] = cmap
        elif dset == 'olay' :
            self.all_olay['switch_cmap'] = cmap
        else:
            AB.EP1("Unrecognized dset type for cmap:  '{}'".format(dset))
 
        return 0


    def set_t_val(self, thr, dset=None):
        """Set thr as the threshold value, for either dset = 'ulay' or 'olay'
        (must be given).

        """

        try:
            thr = float(thr)
        except:
            AB.EP1("T_val value must be numeric, not '{}'".format(thr))

        if dset == None :     
            AB.EP1("Must specify 'ulay' or 'olay' to set i_sb index")
        elif dset == 'ulay' :
            self.all_ulay['T_val'] = thr
        elif dset == 'olay' :
            self.all_olay['T_val'] = thr
        else:
            AB.EP1("Unrecognized dset type for T_val:  '{}'".format(dset))
 
        return 0

    def set_i_sb(self, idx, dset=None):
        """Set idx to be the col/sub-brick for intensity, for either
        dset = 'ulay' or 'olay' (must be given)."""

        try:
            idx = int(idx)
        except:
            AB.EP1("i_sb index must be integer, not '{}'".format(idx))

        if dset == None :     
            AB.EP1("Must specify 'ulay' or 'olay' to set i_sb index")
        elif dset == 'ulay' :
            self.all_ulay['I_sb'] = idx
        elif dset == 'olay' :
            self.all_olay['I_sb'] = idx
        else:
            AB.EP1("Unrecognized dset type for I_sb:  '{}'".format(dset))
 
        return 0

    def set_i_range(self, list_vals, dset=None):
        """Set list_vals values to be the range for intensity, for either dset
        = 'ulay' or 'olay' (must be given).

        If list_vals has one element A, then create range of symmetric
        vals:  -|A| to |A|.

        """

        nval = len(list_vals)

        if nval == 1 :
            aval = abs(float(list_vals[0]))
            vals = [-aval, aval]
        elif nval == 2 :
            aval = abs(float(list_vals[0]))
            vals = [float(x) for x in list_vals]
        else:
            AB.EP1("need 2 list_vals for the range, not {}".format(nval))

        if dset == None :     
            AB.EP1("Must specify 'ulay' or 'olay' to set i_sb index")
        elif dset == 'ulay' :
            self.all_ulay['I_range'] = vals
        elif dset == 'olay' :
            self.all_olay['I_range'] = vals
        else:
            AB.EP1("Unrecognized dset type for I_range:  '{}'".format(dset))
 
        return 0

    def set_t_sb(self, idx, dset=None):
        """Set idx to be the col/sub-brick for thresholding, for either
        dset = 'ulay' or 'olay' (must be given)."""

        try:
            idx = int(idx)
        except:
            AB.EP1("t_sb index must be integer, not '{}'".format(idx))

        if dset == None :     
            AB.EP1("Must specify 'ulay' or 'olay' to set t_sb index")
        elif dset == 'ulay' :
            self.all_ulay['T_sb'] = idx
        elif dset == 'olay' :
            self.all_olay['T_sb'] = idx
        else:
            AB.EP1("Unrecognized dset type for T_sb:  '{}'".format(dset))
 
        return 0


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

    def merge_surf_spec_info(self):
        """Part of finalizing attributes (so after all opts have been read
        in).  

        The spec files paths can be either given as part of the
        spec_file names, or via surf_spec_dir.  Check all these for
        consistency, depending on what is given.

        """

        if not len(self.surf_spec_inp) :
            AB.EP1("No surf spec files provided???")

        # did user specify dir path?
        if self.surf_spec_dir != None :
            # if so, make sure no path parts in spec file names
            for fff in self.surf_spec_inp:
                if '/' in fff :
                    AB.EP1("Cannot have path in '-surf_spec ..' inputs "
                           "when using '-surf_spec_dir ..'.\n"
                           "This spec file breaks that rule: {}".format(fff))
            
                # also make sure each spec file exists
                file_spec = '/'.join([self.surf_spec_dir, fff])
                fpath, ftype = make_abs_path( file_spec )
                if ftype != 'file' :
                    print("type '{}' != 'file'".format(ftype))
                    AB.EP1("final spec name not a file: {}".format(file_spec))

                # and if we get here, it is OK to add to list
                self.surf_spec_list.append( fff )
        else:
            # in this case, any path info must be in each spec_file name
            for fff in self.surf_spec_inp:
                # separate any path info and file name
                if '/' in fff :
                    split_fff = fff.split('/')
                    tail = '/'.join(split_fff[:-1])
                    head = split_fff[-1]
                else:
                    tail = '.'
                    head = fff

                # make sure each spec file exists
                tpath, ttype = make_abs_path( tail )
                hpath, htype = make_abs_path( fff )
                if ttype != 'dir' or htype != 'file' :
                    print("type '{}' != 'dir'".format(ttype))
                    print("type '{}' != 'file'".format(htype))
                    AB.EP1("problem with spec file name {}".format(fff))

                if self.surf_spec_dir != None :
                    if self.surf_spec_dir != tail :
                        AB.EP("spec files do not have same suma dirs? "
                              "not allowed")
                else:
                    self.surf_spec_dir = tail

                # and if we get here, it is OK to add to list
                self.surf_spec_list.append( head )


        # Finally, verify that the dir exists

        return 0


    def add_surf_spec(self, fff):
        """simply add; everything gets checked later (because path info may
        come from separate source)

        """

        self.surf_spec_inp.append( fff )

        return 0

    def set_surf_spec_dir(self, ddd):
        """Verify dir holding spec file(s), and get its absolute path
        """
        
        # verify ddd exists, and is a dir
        dddpath, dddtype = make_abs_path( ddd )
        if dddtype != 'dir' :
            print("type '{}' != 'dir'".format(dddtype))
            AB.EP1("surf_spec_dir not a dir: '{}'".format(ddd))

        # remove any final slash
        if ddd[-1] == '/' :
            # make sure not *only* slash for path
            if len(ddd) == 1 :
                AB.EP1("impossible surf_spec_dir: '{}'".format(ddd))
            self.surf_spec_dir = ddd[:-1]
        else:
            self.surf_spec_dir = ddd
        
        return 0

    def set_surf_vol(self, fname):
        """Verify anatomical dset file name, and get its abs path
        """

        fpath, ftype = make_abs_path(fname)
        if ftype != 'file' :
            AB.EP1("problem with surf_vol file name")

        self.surf_vol = fname

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

        self.all_ulay               = {}              # underlay settings
        self.all_olay               = {}              # overlay settings

        # file info
        self.subj                   = None            # cd come from SUMA dir
        self.surf_spec_list         = None
        self.surf_spec_dir          = None   
        self.surf_vol               = None
         
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
        print("++ set all items from cmd")

        self.all_benv = CO.all_benv
        self.all_bvar = CO.all_bvar
        self.all_svar = CO.all_svar

        self.all_ulay = CO.all_ulay
        self.all_olay = CO.all_olay

        self.surf_spec_list = copy.deepcopy(CO.surf_spec_list)
        self.surf_spec_dir  = CO.surf_spec_dir
        self.surf_vol       = CO.surf_vol

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
            AB.IP("Surf spec dir   : {}".format(self.surf_spec_dir))
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


