from pathlib import Path
import sys
import os
from pprint import pformat
# sys.path.append('/data/NIMH_SSCC/template_making/scripts')

# AFNI modules
import afni_python.afni_base as ab
import afni_python.afni_util as au
from afni_python.option_list import OptionList, read_options
# from align_epi_anat import RegWrap

def get_test_data():
    """
    Eventually should use caches and workspace storage on ci servers could
    just mount it to this target dir"""
    TEST_DIR = Path('/tmp/afni_testdata')
    TEST_ANAT_FILE = TEST_DIR / "anat.nii.gz"
    if not TEST_ANAT_FILE.exists():
        TEST_ANAT_FILE.parent.mkdir()
        import urllib.request
        print('Beginning file download with urllib2...')
        url = 'https://s3.amazonaws.com/fcp-indi/data/Projects/CORR/RawData/HNU_1/0025427/session_1/anat_1/anat.nii.gz'
        urllib.request.urlretrieve(url, TEST_ANAT_FILE)

    PICKLE_PATH = TEST_DIR / "template_config.pklz"

    return (TEST_DIR, TEST_ANAT_FILE, PICKLE_PATH)



@contextlib.contextmanager
def working_directory(path):
    """Changes working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)

def setup_exceptionhook():
    """
    Overloads default sys.excepthook with our exceptionhook handler.

    If interactive, our exceptionhook handler will invoke pdb.post_mortem;
    if not interactive, then invokes default handler.
    """
    def _pdb_excepthook(type, value, tb):
        if sys.stdin.isatty() and sys.stdout.isatty() and sys.stderr.isatty():
            import traceback
            import pdb
            traceback.print_exception(type, value, tb)
            # print()
            pdb.post_mortem(tb)
        else:
            print(
                "We cannot setup exception hook since not in interactive mode")

    sys.excepthook = _pdb_excepthook


def get_dict_diffs(a,b):

    b_not_in_a: Dict[str, Any] = {k: b[k] for k in set(b) - set(a)}
    a_not_in_b: Dict[str, Any] = {k: a[k] for k in set(a) - set(b)}
    # b_not_in_a: Dict[str, Any] = {k: b[k] for k in set(b) - set(a)}
    # a_not_in_b: Dict[str, Any] = {k: a[k] for k in set(a) - set(b)}
    common_dict_a: Dict[str, Any] = {k: a[k] for k in set(a) & set(b)}
    common_dict_b: Dict[str, Any] = {k: b[k] for k in set(a) & set(b)}
    return b_not_in_a, a_not_in_b


def prepare_afni_output(dset, suffix, view=None,path=None):
    """
    prepare the output for an afni function make AFNI dataset structure based
    on input name.
    """
    assert(dset is not None)
    if not dset.is_strict:
        raise ValueError("Inputs to prepare_afni_output must be strict"
        " datasets. This is created using "
        "afni_python.afni_base.afni_name(<name>,strict=True).% s is not."
        % dset.rel_input())

    if path and not Path(path).exists():
        # raise ValueError("Directories must exist to run the pipeline in any mode.")
        os.makedirs(Path(dset.initpath, path))
        

    if not view:
        view = dset.view
    if not suffix.startswith('_'):
        suffix = '_' + suffix
    if not path:
        path = dset.rel_dir()
    filename = Path(path) / (dset.bn + suffix + view + dset.extension)
    o = dset.new(str(filename), strict=True)
    assert(o.is_strict)
    return o
    



def run_check_afni_cmd(cmd_str, ps, in_dict, message=""):
    """
    run afni command and check if afni output dataset exists
    return the same output dataset if it exists, otherwise return None
    could have list of outputs
    """
    print("command:\n %s" % cmd_str)
    keys = in_dict.keys()

    # Get a list of expected output files and their existence. Each value in
    # the dict should have an exist, delete, path, and ppve methods like
    # afni_base.afni_name objects. With that satisfied, any object could be
    # added to the expect_files list here
    expected_files = {k:v for k,v in in_dict.items() if k.startswith('dset_')}
    files_status = {k:v.exist() for k,v in expected_files.items()}

    # Set work directory for command execution if provided
    possible_chdirs = [v.initpath for k,v in expected_files.items()]
    if "chdir" in keys:
        chdir = in_dict["chdir"]
    elif len(possible_chdirs) == 1:
        chdir = possible_chdirs.pop()
    else:
        raise ValueError("chdir (the directory for the command execution) is "
        "ambiguous. If all output files are not in the same directory chdir "
        "must be explicitly specified")

    # if stdout from cmd_str exec is parsed, result is stored in shell_obj.
    # Providing a shell object in the in_dict triggers the behavior. Without it
    #  the shell object is not stored in the output dictionary and command
    #  rerun criteria is altered (no longer always rerun)
    if "shell_obj" in keys:
        shell_obj = in_dict["shell_obj"]
        need_stdout = True
        # Check that our specified execution mode is unambiguous
        if shell_obj.eo != ps.oexec:
            raise ValueError("shell_obj was defined in the calling function."
            "It's value for execution mode (the eo attribute) does not match "
            "the user option defined by ps.oexec. This should never happen "
            "and leads to an ambiguous execution mode.")

    else:
        # generate the shell_com object that executes the command:
        shell_obj = ShellComFuture(cmd_str, eo=ps.oexec)
        need_stdout = False

    # Set the criteria for "executing" the command:
     # (dry-run only reports it doesn't really execute).
    run_criteria = (
        ps.rewrite or
        ps.dry_run() or
        need_stdout or
         not (all(files_status.values())))

    # Execution of the command is required:
    if run_criteria:
        if len(expected_files) > 0:
            for f in expected_files.values():
                f.delete(ps.oexec)

        # Run the command string:
        print("Running in %s" % chdir)
        shell_obj.run(chdir=chdir)

        # Expected files should now exist:
        if len(expected_files) > 0:
            files_status = {k:v.exist() for k,v in expected_files.items()}
        if not (ps.dry_run() or all(files_status.values())) :
            missing_files = [
            v.ppve() for k,v in expected_files.items() if not files_status[k]]
            raise RuntimeError(
                "%s \n After running the above command the following "
                "expected files were not found: \n %s" %
                 (cmd_str, '\n'.join(missing_files)))
    # Command was run and outputs exist. All is good.
    elif ps.ok_to_exist  and all(files_status.values()):
        print(
            "The following output files exist. That's ok:\n",
            '\n'.join([v.ppve() for k,v in expected_files.items()]))
    # Files exist and this was not expected or allowed:
    else:
        raise RuntimeError(
            "The following output files exist. Remove them or allow rewrite:\n",
            '\n'.join([v.ppve() for k,v in expected_files.items()]))

    #return the outputs as a dict
    out_dict = {k:v for k,v in expected_files.items()}
    if need_stdout:\
        out_dict.update({'shell_obj':shell_obj})
    return out_dict




class ShellComFuture(ab.shell_com):
    """
    eo sets execution setting dry_run or ""
    """

    def __init__(self, com, eo="", capture=1, default_text="Text from future",trim_length=2000):
        if eo not in ['', 'dry_run']:
            raise ValueError(
                'The only options for eo (execution type) in pipelines are "dry_run" or ""')
        self.default_text = default_text
        super().__init__(
            com,
            eo=eo,
            capture = capture,
            save_hist=0,
            trim_length=trim_length
        )

    def future_text(self, i, j=-1):
        """return the jth string from the ith line of output. if j=-1, return all
        ith line.
        If default text is being used the typical val indexing does not apply.
        """
        if self.eo == "dry_run":
            out_text = self.default_text
        elif self.exc == 1:
            out_text = self.val(i, j)
        else:
            self.run()
            out_text = self.val(i,j)

        # if not out_text: dont think there is any way to know what this is?
        #     raise ValueError(
        #         "Incorrect output text was received for the following "
        #         "command: %s: \n %s" % (self.com, out_text))
        return out_text

    def exist(self):
        return self.exc


class PipelineConfig():
    def __init__(self, label):
        self.output_dir = ''
        self.ok_to_exist = 0 #Fail if weight data exists
        self.label = label
        self.valid_opts = None
        self.user_opts = None
        self.verb = 1    # a little talkative by default
        self.save_script = ''  # save completed script into given file
        self.rewrite = 0  # Do not recreate existing volumes
        self.oexec = ""  # dry_run is an option
        self.rmrm = 1   # remove temporary files
        self.prep_only = 0  # do preprocessing only
        self.odir = os.getcwd()
        self.bokeh_port = 8787 # default port to show graphical Bokeh debugging info with Dasks

        self.resizebase = [] # dataset to resize nonlinear means to

        self.max_workers = 0   # user sets maximum number of workers
        self.max_threads = 0   # user sets maximum number of threads
        self.cluster_queue = []
        self.cluster_memory = []
        self.cluster_constraint = []
        self.cluster_walltime = []
        return

    def init_opts(self):
        self.valid_opts = OptionList('init_opts')

        # input datasets
        self.valid_opts.add_opt('-ok_to_exist', 0, [],
        helpstr="For running make_template_dask in parallel. This\n"
               "flag allows a previous output of the weight dataset \n"
               "to be used.")
        self.valid_opts.add_opt('-dsets', -1, [],
                                helpstr="Names of datasets")
        self.valid_opts.add_opt('-init_base', 1, [],
                                helpstr="Name of initial base dataset")
        self.valid_opts.add_opt('-resize_base', 1, [],
                                helpstr="Name of resizing dataset")

        self.valid_opts.add_opt('-keep_rm_files', 0, [],
                                helpstr="Don't delete any of the temporary files created here")
        self.valid_opts.add_opt('-prep_only', 0, [],
                                helpstr="Do preprocessing steps only without alignment")
        self.valid_opts.add_opt('-help', 0, [],
                                helpstr="The main help describing this program with options")
        self.valid_opts.add_opt('-limited_help', 0, [],
                                helpstr="The main help without all available options")
        self.valid_opts.add_opt('-option_help', 0, [],
                                helpstr="Help for all available options")
        self.valid_opts.add_opt('-version', 0, [],
                                helpstr="Show version number and exit")
        self.valid_opts.add_opt('-ver', 0, [],
                                helpstr="Show version number and exit")
        self.valid_opts.add_opt('-verb', 1, [],
                                helpstr="Be verbose in messages and options")
        self.valid_opts.add_opt('-save_script', 1, [],
                                helpstr="save executed script in given file")

        self.valid_opts.add_opt('-no_center', 0, [],
                                helpstr="Do not align centers of datasets to initial base")
        self.valid_opts.add_opt('-no_strip', 0, [],
                                helpstr="Do not remove skull")
        self.valid_opts.add_opt('-no_unifize', 0, [],
                                helpstr="Do not unifize data intensities across subjects")
        self.valid_opts.add_opt('-unifize_template', 0, [],
                                helpstr="Unifize mean templates")
        self.valid_opts.add_opt('-anisosmooth', 0, [],
                                helpstr="anisotropically smooth mean templates")

        self.valid_opts.add_opt('-no_rigid', 0, [],
                                helpstr="Do not do rigid alignment step,\n"
                                        "use base as input to affine stage")
        self.valid_opts.add_opt('-no_affine', 0, [],
                                helpstr="Do not do affine alignment step,\n"
                                        "use base as input to nonlinear stage")
        self.valid_opts.add_opt('-no_nonlinear', 0, [],
                                helpstr="Do not do nonlinear alignment step,\n"
                                        "use base as input to nonlinear stage")

        self.valid_opts.add_opt('-rigid_only', 0, [],
                                helpstr="Only do rigid alignment step")
        self.valid_opts.add_opt('-affine_only', 0, [],
                                helpstr="Only do affine alignment step")
        self.valid_opts.add_opt('-nl_only', 0, [],
                                helpstr="Only do nonlinear alignment step")
        self.valid_opts.add_opt('-nl_level_only', 1, [],
                                helpstr="Only do a single nonlinear level alignment step\n"
                                        "providing a level from 0 to 4 to do")
        self.valid_opts.add_opt('-upsample_level', 1, [],
                                helpstr="Upsample base and warp starting at a single\n"
                                        "nonlinear alignment level providing a level from 0 to 4")
        self.valid_opts.add_opt('-findtypical_level', 1, [],
                                helpstr="Search for a typical subject as intermediate target\n"
                                        "at a single nonlinear alignment level [1 to 4]")

        self.valid_opts.add_opt('-overwrite', 0, [],
                                helpstr="Overwrite existing files")
        self.valid_opts.add_opt('-dask_mode', 1, ['None'], ['None', 'SLURM', 'localcluster'],
                                helpstr="set Dask parallelization type")
        self.valid_opts.add_opt('-bokeh_port', 1, ['8787'],
                                helpstr="port for Bokeh visual debugging info with Dask")
        self.valid_opts.add_opt('-max_workers', 1, [],
                                helpstr="maximum number of cpus used for this Dask process")
        self.valid_opts.add_opt('-max_threads', 1, [],
                                helpstr="maximum number of threads used for this Dask process")
        self.valid_opts.add_opt('-cluster_queue', 1, [],
                                helpstr="SLURM queue partition (norm,nimh,...)")
        self.valid_opts.add_opt('-cluster_constraint', 1, [],
                                helpstr="SLURM node constraints (10g, iband,...)")
        self.valid_opts.add_opt('-cluster_walltime', 1, [],
                                helpstr="SLURM walltime limit (36:0:0,...)")
        self.valid_opts.add_opt('-cluster_memory', 1, [],
                                helpstr="SLURM cluster node memory minimum (20g)")
        self.valid_opts.add_opt('-warpsets', -1, [],
                                helpstr="Names of warp datasets if doing a specified nonlinear level")
        self.valid_opts.add_opt('-fs_seg_sets', -1, [],
                                helpstr="Names of FreeSurfer segmentation datasets")
        self.valid_opts.add_opt('-aniso_iters', 1, [],
                                helpstr="Number of iterations for anisotropical smoothing")


    def dry_run(self):
        if self.oexec != "dry_run":
            return 0
        else:
            return 1

    def apply_initial_opts(self, opt_list):
        opt1 = opt_list.find_opt('-version')  # user only wants version
        opt2 = opt_list.find_opt('-ver')
        if ((opt1 != None) or (opt2 != None)):
            # self.version()
            self.ciao(0)   # terminate
        opt = opt_list.find_opt('-verb')    # set and use verb
        if opt != None:
            self.verb = int(opt.parlist[0])

        opt = opt_list.find_opt('-save_script')  # save executed script
        if opt != None:
            self.save_script = opt.parlist[0]

        # user says it's okay to overwrite existing files
        opt = self.user_opts.find_opt('-overwrite')
        if opt != None:
            print("setting option to rewrite")
            self.rewrite = 1

        opt = opt_list.find_opt('-ex_mode')    # set execute mode
        if opt != None:
            self.oexec = opt.parlist[0]

        opt = opt_list.find_opt('-keep_rm_files')    # keep temp files
        if opt != None:
            self.rmrm = 0

        opt = opt_list.find_opt('-prep_only')    # preprocessing only
        if opt != None:
            self.prep_only = 1

        opt = opt_list.find_opt('-help')    # does the user want help?
        if opt != None:
            self.self_help(2)   # always give full help now by default
            self.ciao(0)  # terminate

        opt = opt_list.find_opt('-limited_help')  # less help?
        if opt != None:
            self.self_help()
            self.ciao(0)  # terminate

        opt = opt_list.find_opt('-option_help')  # help for options only
        if opt != None:
            self.self_help(1)
            self.ciao(0)  # terminate

        opt = opt_list.find_opt('-suffix')
        if opt != None:
            self.suffix = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Cannot have blank suffix")
                self.ciao(1)

        opt = opt_list.find_opt('-bokeh_port')
        if opt != None:
            self.bokeh_port = int(opt.parlist[0])
            if((opt == "") or (opt == " ")):
                self.error_msg("Must provide a port number for bokeh port")
                self.ciao(1)

        opt = opt_list.find_opt('-max_workers')
        if opt != None:
            self.max_workers = int(opt.parlist[0])
            if((opt == "") or (opt == " ")):
                self.error_msg("Must provide an integer for number of Dask worker CPUs")
                self.ciao(1)

        opt = opt_list.find_opt('-max_threads')
        if opt != None:
            self.max_threads = int(opt.parlist[0])
            if((opt == "") or (opt == " ")):
                self.error_msg("Must provide an integer for number of Dask threads")
                self.ciao(1)

        opt = opt_list.find_opt('-dask_mode')
        if opt != None:
            self.daskmode = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Must provide SLURM, localcluster or None for dask_mode")
                self.ciao(1)

        opt = opt_list.find_opt('-cluster_queue')
        if opt != None:
            self.cluster_queue = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Must provide name of queue/partition to use in SLURM, e.g norm, nimh, quick, ...")
                self.ciao(1)

        opt = opt_list.find_opt('-cluster_constraint')
        if opt != None:
            self.cluster_constraint = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Must specify which kinds of constraints for cluster nodes, e.g. 10g (default)")
                self.ciao(1)

        opt = opt_list.find_opt('-cluster_walltime')
        if opt != None:
            self.cluster_walltime = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Must specify walltime limit for cluster nodes, e.g 36:0:0")
                self.ciao(1)

        opt = opt_list.find_opt('-cluster_memory')
        if opt != None:
            self.cluster_memory = opt.parlist[0]
            if((opt == "") or (opt == " ")):
                self.error_msg("Must specify minimum memory per cluster node, e.g 20g for 20 gigabytes or RAM/node")
                self.ciao(1)

        opt = opt_list.find_opt('-aniso_iters')
        if opt != None:
            self.aniso_iters = opt.parlist[0]
            try:
               tempiters = int(opt.parlist[0])
            except:
                self.error_msg("Must provide an integer for number of Dask worker CPUs")
                self.ciao(1)


    def get_user_opts(self,help_str):
        self.valid_opts.check_special_opts(sys.argv)  # ZSS March 2014
        self.user_opts = read_options(sys.argv, self.valid_opts)
        self.help_str = help_str
        if self.user_opts == None:
            return 1  # bad
        # no options: apply -help
        if (len(self.user_opts.olist) == 0 or len(sys.argv) <= 1):
            self.self_help()
            self.ciao(0)  # terminate
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt:
                print("** ERROR: seem to have trailers, but cannot find them!")
            else:
                print("** ERROR: have invalid trailing args: %s", opt.show())
            return 1  # failure

        # apply the user options
        if self.apply_initial_opts(self.user_opts):
            return 1

        if self.verb > 3:
            self.show('------ found options ------ ')

        return

    def show(self, mesg=""):
        print('%s: %s' % (mesg, self.label))
        if self.verb > 2:
            self.valid_opts.show('valid_opts: ')
        self.user_opts.show('user_opts: ')

    def info_msg(self, mesg=""):
        if(self.verb >= 1):
            print("#++ %s" % mesg)

    def error_msg(self, mesg=""):
        print("#**ERROR %s" % mesg)

    def error_ex(self, mesg=""):
        print("#**ERROR %s" % mesg)
        self.ciao(1)

    def exists_msg(self, dsetname=""):
        print("** Dataset: %s already exists" % dsetname)
        print("** Not overwriting.")
        if(not self.dry_run()):
            self.ciao(1)
    def exists_msg_python(self, dsetname=""):
        raise ValueError(
            "** Dataset: %s already exists ** Not overwriting." % dsetname)

    def ciao(self, i):
        if i > 0:
            print("** ERROR - script failed")
        elif i == 0:
            print("")

        os.chdir(self.odir)

        if self.save_script:
            au.write_afni_com_history(self.save_script)

        # return status code
        sys.exit(i)

        # save the script command arguments to the dataset history
    def save_history(self, dset, exec_mode):
        self.info_msg("Saving history")  # sounds dramatic, doesn't it?
        cmdline = au.args_as_command(sys.argv,
                                  '3dNotes -h "', '" %s' % dset.input())
        com = ab.shell_com("%s\n" % cmdline, exec_mode)
        com.run()

        # show help
        # if help_level is 1, then show options help only
        # if help_level is 2, then show main help and options help
    def self_help(self, help_level=0):
        if(help_level!=1) :
            print(self.help_str)
        if(help_level):
            print("A full list of options for %s:\n" % self.label)
            for opt in self.valid_opts.olist:
                print("   %-20s" % (opt.name))
                if (opt.helpstr != ''):
                    print("   %-20s   %s" %
                          ("   use:", opt.helpstr.replace("\n", "\n   %-20s   " % ' ')))
                if (opt.acceptlist):
                    print("   %-20s   %s" %
                          ("   allowed:", str.join(', ', opt.acceptlist)))
                if (opt.deflist):
                    print("   %-20s   %s" %
                          ("   default:", str.join(' ', opt.deflist)))
        return 1

    def version(self):
        self.info_msg("make_template_dask: %s" % self.make_template_version)

    # test for OpenMP variable setting
    def report_omp(self):
        try:
           omt = os.environ["OMP_NUM_THREADS"]
           print("OMP_NUM_THREADS = %s" % omt)
        except:
           print("OMP_NUM_THREADS not set. OK")

        # copy dataset 1 to dataset 2
        # show message and check if dset1 is the same as dset2
        # return non-zero error if can not copy

    def copy_dset(self, dset1, dset2, message, exec_mode):
        self.info_msg(message)
        if(dset1.input() == dset2.input()):
            print("# copy is not necessary")
            return 0
        #      if((os.path.islink(dset1.p())) or (os.path.islink(dset2.p()))):
        if(dset1.real_input() == dset2.real_input()):
            print("# copy is not necessary")
            return 0
        ds1 = dset1.real_input()
        ds2 = dset2.real_input()
        ds1s = ds1.replace('/./', '/')
        ds2s = ds2.replace('/./', '/')
        if(ds1s == ds2s):
            print("# copy is not necessary - both paths are same")
            return 0
        print("copying from dataset %s to %s" % (dset1.input(), dset2.input()))
        dset2.delete(exec_mode)
        com = ab.shell_com(
            "3dcopy %s %s" % (dset1.input(), dset2.out_prefix()), exec_mode)
        com.run()
        if ((not dset2.exist())and (exec_mode != 'dry_run')):
            print("** ERROR: Could not rename %s\n" % dset1.input())
            return 1
        return 0

        # BEGIN script specific functions
    def process_input(self):
        # Do the default test on all options entered.
        # NOTE that default options that take no parameters will not go
        # through test, but that is no big deal
        for opt in self.user_opts.olist:
            if (opt.test() == None):
                self.ciao(1)

        # user says it's okay if output dataset exists
        opt = self.user_opts.find_opt('-ok_to_exist')
        if opt != None:
            self.ok_to_exist = 1

        # center alignment is on by default
        opt = self.user_opts.find_opt('-no_center')
        if opt != None:
            self.center = 0

        # unifize is on by default
        opt = self.user_opts.find_opt('-no_unifize')
        if opt != None:
            self.do_unifize = 0

        # skull stripping is on by default
        opt = self.user_opts.find_opt('-no_strip')
        if opt != None:
            self.do_skullstrip = 0

        # anisotropically smooth is off by default now
        opt = self.user_opts.find_opt('-anisosmooth')
        if opt != None:
            self.do_anisosmooth = 1

        # unifize template off by default
        opt = self.user_opts.find_opt('-unifize_template')
        if opt != None:
            self.do_unifize_template = 1

        # rigid alignment is on by default
        opt = self.user_opts.find_opt('-no_rigid')
        if opt != None:
            self.do_rigid = 0

        # affine alignment is on by default
        # turning affine off implies rigid off too for now
        opt = self.user_opts.find_opt('-no_affine')
        if opt != None:
            self.do_rigid = 0
            self.do_affine = 0

        # nonlinear alignment is on by default
        # I'm going to take this to mean do the rigid and affine parts only
        opt = self.user_opts.find_opt('-no_nonlinear')
        if opt != None:
            self.do_nonlinear = 0

        # rigid alignment only
        opt = self.user_opts.find_opt('-rigid_only')
        if opt != None:
            self.do_rigid_only = 1

        # affine alignment only
        opt = self.user_opts.find_opt('-affine_only')
        if opt != None:
            self.do_affine_only = 1

        # nonlinear alignment only
        opt = self.user_opts.find_opt('-nl_only')
        if opt != None:
            self.do_nl_only = 1

        # nonlinear alignment only
        opt = self.user_opts.find_opt('-nl_level_only')
        if opt != None:
            self.do_nl_only = 1  # implied superset
            try:
                self.nl_level_only = int(opt.parlist[0])
            except:
                self.error_msg("Must provide a number from 0 to 4 for a specific nonlinear level")
                self.ciao(1)
            if((self.nl_level_only<0) or (self.nl_level_only>4)):
                self.error_msg("Must provide a number from 0 to 4 for a specific nonlinear level")
                self.ciao(1)

        # upsample base and then subsequent output starting at a specified nonlinear level
        opt = self.user_opts.find_opt('-upsample_level')
        if opt != None:
            try:
                self.upsample_level = int(opt.parlist[0])
            except:
                self.error_msg("Must provide a number from 0 to 4 for a specific nonlinear level")
                self.ciao(1)
            if((self.upsample_level<0) or (self.upsample_level>4)):
                self.error_msg("Must provide a number from 0 to 4 for a specific nonlinear level")
                self.ciao(1)

        # find typical subject at a specified nonlinear level
        opt = self.user_opts.find_opt('-findtypical_level')
        if opt != None:
            try:
                self.findtypical_level = int(opt.parlist[0])
            except:
                self.error_msg("Must provide a number from 1 to 4 for a specific nonlinear level for findtypical_level")
                self.ciao(1)
            if((self.findtypical_level<1) or (self.findtypical_level>4)):
                self.error_msg("Must provide a number from 1 to 4 for a specific nonlinear level for findtypical_level")
                self.ciao(1)

        opt = self.user_opts.find_opt('-dsets')
        if opt == None:
            print("** ERROR: Must use -dsets option to specify input datasets\n")
            self.ciao(1)
        self.dsets = self.user_opts.find_opt('-dsets')
        for dset_name in self.dsets.parlist:
            check_dset = ab.afni_name(dset_name)
            if not check_dset.exist():
                self.error_msg("Could not find dset\n %s "
                               % check_dset.input())
            else:
                self.info_msg(
                    "Found dset %s\n" % check_dset.input())

        opt = self.user_opts.find_opt('-warpsets')
 #       if((opt==None) and (self.nl_level_only>0)):
 #           self.error_msg("Must provide initial warp datasets if doing nonlinear levels> 0")
 #           self.ciao(1)
        if(opt):
            self.warpsets = self.user_opts.find_opt('-warpsets')
            for warpset_name in self.warpsets.parlist:
                check_dset = ab.afni_name(warpset_name)
                if not check_dset.exist():
                    self.error_msg("Could not find warp dset\n %s "
                                   % check_dset.input())
                else:
                    self.info_msg(
                        "Found warp dset %s\n" % check_dset.input())


        opt = self.user_opts.find_opt('-init_base')
        if opt == None:
             print(
                "** ERROR: Must use -init_base option to specify an initial base\n")
             self.ciao(1)

        self.basedset = ab.afni_name(opt.parlist[0])
        if not self.basedset.exist():
             self.error_msg("Could not find initial base dataset\n %s "
                           % self.basedset.input())
        else:
             self.info_msg(
                "Found initial base dset %s\n" % self.basedset.input())

        opt = self.user_opts.find_opt('-resize_base')
        if opt != None:
            self.resizebase = ab.afni_name(opt.parlist[0])
            if not self.resizebase.exist():
                 self.error_msg("Could not find resize base dataset\n %s "
                           % self.resizebase.input())
            else:
                 self.info_msg(
                     "Found resize base dset %s\n" % self.resizebase.input())

        opt = self.user_opts.find_opt('-fs_seg_sets')
        if(opt):
            self.fs_seg_sets = self.user_opts.find_opt('-fs_seg_sets')
            for fs_seg_set_name in self.fs_seg_sets.parlist:
                check_dset = ab.afni_name(fs_seg_set_name)
                if not check_dset.exist():
                    self.error_msg("Could not find FreeSurfer segmentation dset\n %s "
                                   % check_dset.input())
                else:
                    self.info_msg(
                        "Found FreeSurfer segmentation dset %s\n" % check_dset.input())

    def __str__(self):
        return pformat(self.__dict__)

    def __repr__(self):
        return '<%s %s label=%r>' % (
            self.__class__.__name__, hex(id(self)), self.label)

    def __eq__(self, other):
        obj_dict = {k:v for k,v in self.__dict__.items() if k != 'odir'}
        other_dict = {k:v for k,v in other.__dict__.items() if k != 'odir'}
        diffs = get_dict_diffs(obj_dict, other_dict)
        return not(bool(diffs[0]) or bool(diffs[0]))
        # End of Pipeline_opt class



class TemplateConfig(PipelineConfig):
    def __init__(self, label):
        # software version (update for changes)
        self.make_template_version = "0.05"
        self.daskmode = "None" # which kind of Dask parallelization, none by default

        self.do_skullstrip = 1  # steps to do
        self.do_unifize = 1
        self.do_rigid = 1
        self.do_affine = 1
        self.do_nonlinear = 1
        self.do_anisosmooth = 0
        self.do_unifize_template = 0
        self.do_freesurf_mpm = 0
        self.do_center = 1

        self.do_rigid_only = 0   # major stages to do when doing just one
        self.do_affine_only = 0
        self.do_nl_only = 0
        self.nl_level_only = -1  # do only one level of nonlinear alignment

        self.warpsets = []
        self.aniso_iters = "1"
        self.upsample_level = [] # no upsampling by default
        self.findtypical_level =[] # no typical subject intermediates by default in nonlinear

        super().__init__(label = label)

