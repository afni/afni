import sys, os
import copy
import json
from   afnipy  import lib_physio_opts    as lpo
from   afnipy  import lib_format_cmd_str as lfcs

def make_retobj_oname(suffix, retobj, ext='txt'):
    """Construct an output filename for log/text info, from the retobj
obj.

Parameters
----------
suffix : str
    label for type of time series: 'card', 'resp', etc.
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs
ext : str
    file extension name; if 'None', then no do not concatenate: 
    '.' + ext.

Returns
-------
oname : str
    name of file to be written

    """

    odir   = retobj.out_dir
    prefix = retobj.prefix
    oname  = suffix

    # add pieces
    if prefix :  oname = prefix + '_' + oname
    if odir   :  oname = odir.rstrip('/') + '/' + oname
    if ext    :  oname = oname + '.' + ext

    return oname

def get_physio_arg_str(argv, do_niceify=True):
    """Parse the command line call argv, which is a list of all the pieces
therein.  The pieces can just be joined together with whitespace, or
more nicely spaced out across multiple lines and vertically aligned
with do_niceify.

Parameters
----------
argv: list
    list of strings, being the pieces of the command line command
do_niceify: bool
    should we space things out nicely across multiple lines?
    (def: of course we should!)

Returns
-------
arg_str : str
    The single string that is the full command line command

    """

    arg_str = ' '.join(argv)

    if do_niceify :
        # might be simpler way to get from parser?
        all_opts = ["-" + key for key in lpo.DEF.keys()]

        # create str
        is_diff, arg_str = \
            lfcs.afni_niceify_cmd_str(arg_str, list_cmd_args = all_opts)

    return arg_str
        
def make_cmd_logs(args_dict, retobj):
    """Output text and json log files of input params and parsed input
dict, respectively.  These fils are written in the output dir, with the
output prefix.

Parameters
----------
args_dict : dict
    dictionary whose keys are option names and values are the
    user-entered values (which might still need separate interpreting
    later)
retobj : retro_obj class
    object with all necessary input time series info; will also store
    outputs from here; contains dictionary of phys_ts_objs

Returns
-------
0 upon successful completion

    """

    # ----- log the command used

    # get a copy of niceified cmd str (argv) and output name
    cmd_str   = get_physio_arg_str(args_dict['argv'])
    oname_cmd = make_retobj_oname('cmd', retobj, ext='txt')

    # write out
    fff = open(oname_cmd, 'w')
    fff.write(cmd_str)
    fff.close()

    # ----- log the parsed inputs

    # output a copy of parsed info: everything in dict *except* argv
    args_dict_log = copy.deepcopy(args_dict)
    args_dict_log.pop('argv')
    oname_info = make_retobj_oname('info', retobj, ext='json')

    # write out
    ojson = json.dumps( args_dict_log, indent=4 )
    fff = open( oname_info, "w" )
    fff.write( ojson )
    fff.close()

    return 0
