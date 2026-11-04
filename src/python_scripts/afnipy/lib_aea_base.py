#!/usr/bin/env python

"""Step-by-step alignment processing for align_epi_anat.py.

The explicit _parent reference replaces the original global ps.  A shallow
child copy deliberately retains its parent's reference: accesses formerly
made through ps must not silently become accesses to child-specific state.
"""

import copy
import os
import sys

from afnipy import afni_base    as ab
from afnipy import afni_util    as au
from afnipy import lib_aea_defs as DEF

# =========================================================================
# some verbose messages

msg_01e = "Child epis / dset2 are not processed unless aligning epi\n"
msg_01e+= "to anat datasets with -epi2anat / -dset2to1 or a save\n"
msg_01e+= "preprocessing option such as -save_vr is also selected"

msg_02i = "Child epis / dset2 will be processed although not aligning\n"
msg_02i+= "epi to anat datasets with -epi2anat / -dset2to1 because a\n"
msg_02i+= "save preprocessing option such as -save_vr is selected"

msg_03e = "child anat datasets are not processed unless aligning anat\n"
msg_03e+= "to epi or dset1to2 datasets"

# =========================================================================

class MainObj:
    """Own alignment state and execute the original stages in order."""

    def __init__(self, user_inobj=None, args_orig=None):
        """Initialize defaults, then run only when an input object is
        supplied.

        Inputs must already have passed process_options() and check_options().
        Keep the first nonzero stage status in self.status.  Legacy ciao()
        exits are caught here after their messages, directory restoration
        and optional script saving have occurred; other exceptions propagate.

        """

        self.status = 0
        self.label = 'align_epi_anat.py'
        self.user_inobj = user_inobj
        self.args_orig = list(sys.argv if args_orig is None else args_orig)
        self.argv = self.args_orig[:]
        for key, value in DEF.DOPTS.items():
            setattr(self, key, copy.deepcopy(value))
        self.odir = os.getcwd()
        self.valid_opts = None
        self.user_opts = None
        self.input_options = {}
        self._parent = self

        if user_inobj is not None:
            try:
                self.status = self.load_from_inopts()
                if self.status:
                    return

                self.status = self.basic_setup()
                if self.status:
                    return

                self.status = self.prepare_datasets()
                if self.status:
                    return

                if self.prep_only:
                    self.ciao(0)

                self.status = self.align_datasets()
                if self.status:
                    return

                self.status = self.finish()
                if self.status:
                    return
            except SystemExit as exc:
                self.status = exc.code

    def load_from_inopts(self):
        """Copy parsed controls and option objects without sharing containers.

        Copy the state as one graph so AFNI option/default references retain
        their relationships.  Keep this object's parent reference and status.
        Return 0 after loading.
        """

        fields = {key: value for key, value in vars(self.user_inobj).items()
                  if key != 'status'}
        self.__dict__.update(copy.deepcopy(fields))
        return 0

    def basic_setup(self):
        """Resolve derived settings and dataset/output setup in the legacy order.

        Translate process_input()'s historical 1-for-success result into
        the stage convention of 0 for success.  Preserve its fatal exit path.
        """

        if not self.process_input():
            self.ciao(1)

        return 0

    def prepare_datasets(self):
        """Clean old temporaries, prepare anatomy/EPI, then construct weights.

        Preserve the original preprocessing failure checks and return 0 after
        preparation.  Weight generation still precedes the prep-only exit.
        """

        self.cleanup()

        if not self.process_anat():
            self.ciao(1)
        self.epi_ts, self.epi_vr, self.epi_ns = self.process_epi(
            use_ss=self.epi_strip_method)

        if not self.epi_ns:
            self.ciao(1)
        self.epi_alnd_tlrc = None

        if not self.edge:
            self.epi_wt = self.create_weight(
                self.epi_ns, float(self.sqmask), self.boxmask,
                self.binmask, self.perc, -1, suf='_wt')
        else:
            self.epi_wt = self.epi_ns

        return 0

    def align_datasets(self):
        """Run each requested cost, its QC/output steps and child processing.

        Preserve cost order and suffixes, transform direction, output order,
        and child cleanup.  Return 0 after all requested costs complete.
        """

        e = self.epi_ns
        for mcost in self.multicost:
            if (mcost==self.cost):
                suff = self.suffix
            else:
                suff = "{!s}_{!s}".format(self.suffix, mcost)

            a = self.anat_ns

            self.anat_alnd, self.anat_alndwt = \
                self.align_anat2epi(e, a, 
                                    self.epi_wt, self.AlOpt, 
                                    suff, mcost)
            if (not self.anat_alnd):
                self.ciao(1)

            # does the user want the epi aligned to the anat
            if (self.epi2anat) :
                # compute transformation just from applying inverse
                # (especially important for oblique datasets)
                a = self.anat0_master
                self.epi_alnd, self.epi_alnd_tlrc = \
                    self.align_epi2anat(self.epi_ts, a, self.AlOpt, suf=suff)
                if (not self.epi_alnd):
                    self.ciao(1)
            else:
                self.epi_alnd = ''

            if (not self.anat2epi):
                self.anat_alnd = ''

            if self.AddEdge:
                self._listlog_a2e, self._listlog_e2a = \
                    self.prepare_addedge(e, suff)

            #Create final results
            self.create_output(self.anat_alnd, 
                               self.anat_alndwt, 
                               self.epi_alnd,
                               self.epi_alnd_tlrc, 
                               "{!s}".format(suff), e, a)

            # process the children
            if self.child_epis != None:
                for child_epi_name in self.child_epis.parlist:
                    child_epi = ab.afni_name(child_epi_name)
                    self.process_child_epi(child_epi, suff)
                    # cleanup after the children, remove any extra files
                    if (self.rmrm):
                        self.fresh_start(child_epi.out_prefix(), apref="",
                            rmold=0,
                           epipath=("{!s}".format(self.output_dir)) )

        return 0

    def prepare_addedge(self, e, suff):
        """Prepare optional @AddEdge QC for the current alignment cost.

        e is the representative EPI and suff the cost-specific output suffix.
        Preserve resampling/matrix selection and return both examination-list
        names for the final viewing instructions.
        """

        if self.output_dir == "./" :
            cmd = "\\rm -f AddEdge/*"
            com = ab.shell_com(cmd, self.oexec)
        else:
            cmd = "cd {!s}; \\rm -f AddEdge/*".format(self.output_dir)
            com = ab.shell_com(cmd, self.oexec)

        stat = com.run()
        

        # @AddEdge requires single sub-brick, resampled data (grids
        # must match) and skullstripped data to avoid extracranial and
        # cranial edges so using data as it went into 3dAllineate
        # here.  Check if resampling is turned off, and do that here
        # for epi2anat, resample epi_alnd to anat if skullstripping is
        # off, we'll have to assume it's already done

        # Note 3dAllineate does not require resampling. This script
        # provides an option to avoid resampling.

        # If @AddEdge does require resampling though, so do it now if
        # it hasn't been done yet
        if(self.resample_flag):
            ein_rs = e
        else:
            baseviewext = "{!s}{!s}".format(self.epi.view, self.epi.extension)
            ein_rs = self.resample_epi(e, "","{!s}__tt_{!s}_rs_in{!s}".format(
                self.output_dir,
                self.epi.out_prefix(),
                baseviewext
            ))

        if (self.epi2anat and self.anat2epi):
            listlog_a2e = "a2e_examine_list.log"
            listlog_e2a = "e2a_examine_list.log"
        else :
            listlog_a2e = ""
            listlog_e2a = ""

        # if aligning epi to anatomical
        if (self.epi2anat):    
            # for the edge image, we need a dataset at the resolution
            # of the anatomical and the specific epi sub-brick we used
            # as the epi_base Use 3dAllineate to create the resampled,
            # skullstripped EPI from the same representative sub-brick
            # used to align anat to epi

            if((self.output_dir == "./") or (self.output_dir == " ")):
                outdir = self.epi.p()
            else:
                outdir = self.output_dir

            self.epi_addedge = e.new("__tt_{!s}_addedge".format(
                self.epi.out_prefix()
            ))
            epi_mat = "{!s}{!s}{!s}_mat.aff12.1D".format(
                outdir,
                self.epi.out_prefix(),
                self.suffix
            )
            if((self.volreg_flag) and (self.epi_base.isdigit())):
                epi_mat = "{!s}{!s}{!s}_reg_mat.aff12.1D{{{!s}}}".format(
                    outdir,
                    self.epi.out_prefix(),
                    suff,
                    self.epi_base
                )

            msg = "Applying transformation for "
            msg+= "{!s} to ".format(self.dset2_generic_name)
            msg+= "{!s} for @AddEdge".format(self.dset1_generic_name)
            ab.IP(msg)                

            cmd = "3dAllineate "
            cmd+= "-base {!s} ".format(self.anat0.input())
            cmd+= "-1Dmatrix_apply '{!s}' ".format(epi_mat)
            cmd+= "-prefix {!s}{!s} ".format(
                outdir,
                self.epi_addedge.out_prefix()
            )
            cmd+= "-input {!s}  ".format(ein_rs.input())
            cmd+= "-master BASE"
            com = ab.shell_com(cmd, self.oexec)

            stat = com.run()
            if stat :
                ab.EP1("Failed at a 3dAllineate stage")
                self.ciao(stat)

            self.add_edges(self.anat_ns, ein_rs, self.epi_addedge,\
                      "{!s}_ns".format(self.anat0.out_prefix()),     \
                      "{!s}_ns".format(self.epi.out_prefix()),       \
                      "{!s}{!s}".format(self.epi.out_prefix(), suff), \
                       listlog = listlog_e2a)

        if (self.anat2epi):
            self.add_edges(ein_rs, self.anat_ns, self.anat_alnd,  \
                          "{!s}_ns".format(self.epi.out_prefix()),     \
                          "{!s}_ns".format(self.anat0.out_prefix()),   \
                          "{!s}{!s}".format(self.anat0.out_prefix(), suff), \
                          listlog = listlog_a2e)

        return listlog_a2e, listlog_e2a

    def finish(self):
        """Perform requested final cleanup and print completion/QC
        instructions.

        Complete through ciao(0), preserving directory restoration and
        optional command-history saving.

        """

        if (self.rmrm):
            self.cleanup()


        ab.IP("Finished alignment successfully")
        if(self.AddEdge):
            ab.IP("To view edges produced by @AddEdge, type:")
            print("    cd AddEdge")
            if (0):  #ZSS  @AddEdge now launches AFNI
                print("    afni -niml -yesplugouts &")

            if (self.epi2anat and self.anat2epi):
                print("    @AddEdge -examinelist {!s}".format(
                    self._listlog_a2e
                ))
                print("    @AddEdge -examinelist {!s}".format(
                    self._listlog_e2a
                ))
            else:
                print("    @AddEdge")

        self.ciao(0)

    def dry_run(self):
        """Return 1 when commands should only be displayed, otherwise 0."""

        if self.oexec != "dry_run":
            return 0
        else:
            return 1


    def show(self, mesg=""):
        """Print the parsed options with an optional message.

        Include the valid option table when verbosity exceeds two.
        """

        ab.IP('{}: {}'.format(mesg, self.label))
        if self.verb > 2: self.valid_opts.show('valid_opts: ')
        self.user_opts.show('user_opts: ')


    def info_msg(self, mesg=""):
        """Print mesg with the informational prefix when verbosity permits."""

        if(self.verb >= 1) :
            ab.IP(mesg)




    def exists_msg(self, dsetname=""):
        """Report an existing output and terminate unless in dry-run mode.

        dsetname is the output name displayed to the user.
        """

        msg = "Dataset: {!s} already exists".format(dsetname)
        ab.WP(msg)
        ab.WP("Not overwriting.")
        if not(self._parent.dry_run()):
            self.ciao(1)


    def ciao(self, i):
        """Finish with status i after restoring the original working
        directory.

        Print the historical completion/error message and, when requested,
        write the accumulated AFNI command history.  Raise SystemExit(i);
        the entry-point/processing boundary records that status.

        """

        if i > 0:
            ab.EP1("script failed")
        elif i==0:
            print("")

        os.chdir(self.odir)

        if self.save_script:
            au.write_afni_com_history(self.save_script)

        # return status code
        sys.exit(i)

    # save the script command arguments to the dataset history
    def save_history(self, dset, exec_mode):
        """Add this invocation to dset's history with 3dNotes.

        dset is an afni_name and exec_mode controls command execution.
        Use the saved argument list so main(argv) records the
        invocation it ran."""

        ab.IP("Saving history")  # sounds dramatic, doesn't it?
        cmdline = au.args_as_command(self.argv, \
                   '3dNotes -h "', "\" {!s}".format(dset.input()))
        cmd = "{!s}\n".format(cmdline)
        com = ab.shell_com(cmd, exec_mode)

        stat = com.run()
        if stat :
            ab.EP1("Failed at a 3dNotes stage")
            self.ciao(stat)

    # show help
    # if help_level is 1, then show options help only
    # if help_level is 2, then show main help and options help
    def self_help(self, help_level=0):
        """Print the requested help level using the unchanged option table.

        Level 0 prints main help, 1 option help, and 2 both.  Return 1.
        """

        if(help_level!=1) :
            print(DEF.g_help_string)
        if(help_level):
            print("A full list of options for {!s}: ~1~\n".format(
                self._parent.label
            ))
            for opt in self.valid_opts.olist:
                print("{!s:<20}".format(opt.name))
                if (opt.helpstr != ''):
                    print("{!s:<20}   {!s}".format(
                        "   use:",
                        opt.helpstr.replace("\n","\n   {!s:<20}   ".format(
                            ' '
                        ))
                    ))
                if (opt.acceptlist):
                    print("{!s:<20}   {!s}".format(
                        "   allowed:",
                        str.join(', ', opt.acceptlist)
                    ))
                if (opt.deflist):
                    print("{!s:<20}   {!s}".format(
                        "   default:",
                        str.join(' ', opt.deflist)
                    ))
        return 1



    # remove all the temporary files for epi and anat base names
    def cleanup(self, rmold=0):
        """Remove matching temporary files for the original input datasets.

        Resolve the epi/anat aliases and delegate to fresh_start().  rmold
        also selects the legacy removal of previous aligned outputs.
        Return 1 after issuing the cleanup commands.
        """

        opt = self.input_options.get('-epi')
        if opt == None :
            opt = self.input_options.get('-dset2')
        e = ab.afni_name(opt.parlist[0])
        e.to_afni(new_view=au.dset_view(e.ppve()))
        opt = self.input_options.get('-anat')
        if opt == None :
            opt = self.input_options.get('-dset1')
        a = ab.afni_name(opt.parlist[0])
        a.to_afni(new_view=au.dset_view(a.ppve()))

        self.fresh_start( \
             ("{!s}".format(e.out_prefix())), \
             ("{!s}".format(a.out_prefix())), rmold = rmold, \
             epipath = self._parent.output_dir,
            anatpath = self._parent.output_dir)
        return 1


    def version(self):
        """Print the unchanged alignment version at the current verbosity."""

        ab.IP("align_epi_anat version: {}".format(self.align_version))


    # copy dataset 1 to dataset 2
    # show message and check if dset1 is the same as dset2
    # return non-zero error if can not copy
    def copy_dset(self, dset1, dset2, message, exec_mode):
        """Copy dset1 to dset2 using the selected execution mode.

        Both datasets are afni_name objects.  Skip equivalent paths; otherwise
        delete the destination and run 3dcopy.  Return 0 on success or a skipped
        copy, and 1 when the output is missing outside dry-run mode.
        """

        ab.IP(message)
        if(dset1.input()==dset2.input()):
            print("# copy is not necessary")
            return 0

        if(dset1.real_input() == dset2.real_input()):
            print("# copy is not necessary")
            return 0

        ds1 = dset1.real_input()
        ds2 = dset2.real_input()
        ds1s = ds1.replace('/./','/')
        ds2s = ds2.replace('/./','/')
        if(ds1s == ds2s):
            print("# copy is not necessary - both paths are same")
            return 0
        print("copying from dataset {!s} to {!s}".format(
            dset1.input(),
            dset2.input()
        ))

        dset2.delete(exec_mode)

        cmd = "3dcopy {!s} {!s}".format(dset1.input(), dset2.out_prefix())
        com = ab.shell_com(cmd, exec_mode)
        stat = com.run()

        if stat :
            ab.EP1("Failed at a 3dcopy stage")
            self.ciao(stat)

        if ((not dset2.exist())and (exec_mode!='dry_run')):
            msg = "Could not rename {!s}\n".format(dset1.input())
            ab.EP1(msg)
            return 1
        return 0


    ## BEGIN script specific functions
    def process_input(self):

        # skull stripping is on by default for anat/dset1
        """Resolve alignment settings and perform the legacy setup checks.

        Use the parsed option lookup without reparsing command-line arguments.
        Preserve option precedence, dataset probes, grid calculations and output
        directory creation in their original order.  Return 1 on success or 0
        for a missing EPI input option; other fatal checks call ciao().
        """

        opt = self.input_options.get('-anat_has_skull')
        if opt != None and opt.parlist[0] == 'no':
            self._parent.skullstrip = 0
        else:
            self._parent.skullstrip = 1
            self._parent.skullstrip_method = '3dSkullStrip'
            opt = self.input_options.get('-dset1_strip')
            if(opt!=None):
                self._parent.skullstrip_method = opt.parlist[0]
                if(self._parent.skullstrip_method=='None'):
                    self._parent.skullstrip = 0

        # skull stripping is on by default for epi / dset2
        opt = self.input_options.get('-epi_strip')
        if opt != None :
            self._parent.epi_strip_method = opt.parlist[0]
        else:
            self._parent.epi_strip_method = '3dSkullStrip'
            opt = self.input_options.get('-dset2_strip')
            if(opt!=None):
                self._parent.epi_strip_method = opt.parlist[0]

        #Allineate extras
        opt = self.input_options.get('-Allineate_opts')
        if opt != None:
            self._parent.AlOpt = str.join(' ', opt.parlist)
        else:
            self._parent.AlOpt = ''

        # rigid body alignment
        opt = self.input_options.get('-rigid_body')
        if opt != None:
            self._parent.AlOpt = self._parent.AlOpt.replace('-warp aff',
                '-warp shift_rotate')

        # rigid body alignment
        opt = self.input_options.get('-rigid_equiv')
        if opt != None:
            self._parent.rigid_equiv = 1

        opt = self.input_options.get('-feature_size')
        if opt != None:
            featuresize = float(opt.parlist[0])
            blursize = 2.0 * featuresize
            aljoin = '-twoblur {:f} -blok "RHDD({:f})"'.format(
                blursize,
                featuresize
            )
            self._parent.AlOpt = "{!s} {!s}".format(self._parent.AlOpt, 
                                                    aljoin)
        else:
            featuresize = 0.0

        opt = self.input_options.get('-rat_align')
        if opt != None:
            if featuresize == 0.0 :
                featuresize = 0.5
                blursize = 1.0
                aljoin = "-twoblur {:f} -blok \"RHDD({:f})\"".format(
                    blursize,
                    featuresize
                )
                self._parent.AlOpt = "{!s} {!s}".format(
                    self._parent.AlOpt,
                    aljoin
                )
            self._parent.skullstrip_opt = "-rat"

        #big_move?
        opt1 = self.input_options.get('-big_move')
        #giant_move?
        opt2 = self.input_options.get('-giant_move')
        #large move? (same as giant_move, but without cmass part)
        opt2b = self.input_options.get('-large_move')
        #ginormous move?
        opt3 = self.input_options.get('-ginormous_move')
        if(not (opt1 or opt2 or opt2b or opt3)):
            self._parent.AlOpt = "{!s} -onepass ".format(self._parent.AlOpt)
        else:
            # resampling has the potential to cut off data
            # the cmass flag is used by 3dAllineate to align the center of mass
            # first and then resamples
            self.resample_flag = 0

        if(opt1):
            self._parent.AlOpt = "{!s} -twopass ".format(self._parent.AlOpt)

        if(opt2 or opt2b or opt3):
            if featuresize  == 0.0 :
                fsize = 1
            else:
                fsize = featuresize

            self._parent.AlOpt =  \
            (
                "{!s} -twobest 11 -twopass -maxrot 45 -maxshf 40 "
                "-fineblur {!s} -source_automask+2"
            ).format(
                self._parent.AlOpt,
                fsize
            )
            if not(opt2b):
                self._parent.cmass = "cmass"
            self._parent.giant_move = 1
        else :
            self._parent.giant_move = 0
        if(opt3): # ginormous option
            self._parent.align_centers = 1

        #supersize - allow for large scale changes
        opt = self.input_options.get('-supersize')
        if(opt):
            self._parent.AlOpt =  \
            "{!s} -maxscl 1.5".format(self._parent.AlOpt)

        #giant_move?
        opt = self.input_options.get('-flip_giant')
        if(opt): self._parent.flip_giant = 1;

        #add edges
        opt = self.input_options.get('-AddEdge')
        if opt == None:
            self._parent.AddEdge = 0
        else:
            self._parent.AddEdge = 1


        #get anat and epi

        opt = self.input_options.get('-epi')
        if opt == None:
            opt = self.input_options.get('-dset2')
            if opt == None:
                ab.EP1("Must use -epi or -dset2 option\n")
                return 0
            if self.cost == '':
                # make default cost lpa for dset1/2 terminology
                self.cost = 'lpa'
            self.dset_prep_off()
            self.epi_base = "0"      # align to 0th sub-brick
            self.dset1_generic_name = 'dset1'
            self.dset2_generic_name = 'dset2'

        e = ab.afni_name(opt.parlist[0])
        self._parent.epi = e
        opt = self.input_options.get('-anat')
        if opt == None:
            opt = self.input_options.get('-dset1')
            if opt == None:
                ab.EP1("Must use -anat or -dset1 options\n")
                self._parent.ciao(1)
            if self.cost == '':
                # make default cost lpa for dset1/2 terminology
                self.cost = 'lpa'
            # assume no motion correction, slice timing correction,...
            self.dset_prep_off()
            self.epi_base = "0"      # align to 0th sub-brick
            self.dset1_generic_name = 'dset1'
            self.dset2_generic_name = 'dset2'

        a = self._parent.anat0 = ab.afni_name(opt.parlist[0])

        # if cost has not been set by using dset1/2 or cost options
        if self.cost == '' :
            self.cost = 'lpc'      # set default cost to lpc

    #      if ps.user_opts.find_opt('-fresh'):
    #         ps.fresh_start(e.out_prefix(), a.out_prefix())

        #epi input
        if not e.exist():
            msg = "Could not find epi / dset2 dataset\n   {!s} ".format(
                e.input()
            )
            ab.EP1(msg)
            self._parent.ciao(1)

        #anat input
        if not a.exist():
            msg = "Could not find anat / dset1 dataset\n   {!s} ".format(
                a.input()
            )
            ab.EP1(msg)
            self._parent.ciao(1)

        #get 3dTshift options
        opt = self.input_options.get('-tshift_opts')
        if opt != None:
            self._parent.tshift_opt = str.join(' ', opt.parlist)
        else:
            self._parent.tshift_opt = '-cubic'

        #get 3dvolreg options
        opt = self.input_options.get('-volreg_opts')
        if opt != None:
            self._parent.reg_opt = str.join(' ', opt.parlist)
        else:
            self._parent.reg_opt = ''

        #get 3dSkullstrip options
        opt = self.input_options.get('-skullstrip_opts')
        if opt != None:
            self._parent.skullstrip_opt = str.join(' ',opt.parlist)

        opt = self.input_options.get('-dset1strip_opts')
        if opt != None:
            self._parent.skullstrip_opt = str.join(' ',opt.parlist)

        opt = self.input_options.get('-epistrip_opts')
        if opt != None:
            self._parent.epistrip_opt = str.join(' ',opt.parlist)

        opt = self.input_options.get('-dset2strip_opts')
        if opt != None:
            self._parent.epistrip_opt = str.join(' ',opt.parlist)

        #get epi base type for alignment (specific sub-brick/median/mean)
        opt = self.input_options.get('-epi_base')
        if opt == None:
            opt = self.input_options.get('-dset2_base')
            if opt == None:
                if(self.epi_base==None):
                    ab.EP1("Must use -epi_base or -dset2_base options")
                    self._parent.ciao(1)
            else:
                self._parent.epi_base = opt.parlist[0]
        else:
            self._parent.epi_base = opt.parlist[0]

        #get volreg_base (matches epi_base by default)
        opt = self.input_options.get('-volreg_base')
        if opt != None:
            self.volreg_flag = 1
            self._parent.volreg_base = opt.parlist[0]
        else:
            self._parent.volreg_base = self._parent.epi_base

    # may not need this and only the epi_base parameter instead
        #get 3dTstat options
        opt = self.input_options.get('-tstat_opts')
        if opt != None:
            self._parent.tstat_opt = str.join(' ', opt.parlist)
        else:
            self._parent.tstat_opt = ''

        #check for various center of mass options
        optc = self.input_options.get('-cmass')
        if optc == None :
            #if no cmass option entered, partial coverage?
            cmass_opts = 0
            opt = self.input_options.get('-partial_coverage')
            if opt != None:
                self._parent.cmass = 'cmass+a'
                cmass_opts += 1
            opt = self.input_options.get('-partial_axial')
            if opt != None:
                self._parent.cmass = 'cmass+xy'
                cmass_opts += 1
            opt = self.input_options.get('-partial_coronal')
            if opt != None:
                self._parent.cmass = 'cmass+xz'
                cmass_opts += 1
            opt = self.input_options.get('-partial_sagittal')
            if opt != None:
                self._parent.cmass = 'cmass+yz'
                cmass_opts += 1
            if cmass_opts > 1:
                ab.EP1("Can only use a single partial_xxx coverage option")
        else:
            self._parent.cmass = optc.parlist[0]

        #get talairached anatomical dataset
        opt = self.input_options.get('-tlrc_apar')
        if opt != None:
            anat_tlrc = ab.afni_name(opt.parlist[0])
            self._parent.tlrc_apar = anat_tlrc
            if not anat_tlrc.exist():
                msg = "Could not find anat talairach "
                msg+= "template dataset\n  {!s} ".format(anat_tlrc.out_prefix())
                ab.EP1(msg)
            else:
                msg = "Talairach transformed anatomical: "
                msg+= "{}".format((anat_tlrc.input()))
                ab.IP(msg)
            if not self._parent.epi2anat:
                msg = "No talairach transformation performed "
                msg+= "unless aligning epi2anat"
                ab.EP1(msg)
        else :
            self._parent.tlrc_apar = ""
        opt = self.input_options.get('-tlrc_epar')
        if opt != None:
            at = ab.afni_name(opt.parlist[0])
            self._parent.tlrc_epar = at
            if not at.exist():
                msg = "Could not find epi talairach template dataset:\n"
                msg+= "{}".format(at.input())
                ab.EP1(msg)
        else :
            self._parent.tlrc_epar = ""


        #get pre-transformation matrix
        opt = self.input_options.get('-pre_matrix')
        if opt != None:
            # [PT: Dec 1, 2022] get abs path version of each path
            # (expanduser deals with '~'); needed if using output_dir
            abs_path = os.path.abspath(os.path.expanduser(opt.parlist[0]))
            if not(os.path.isfile(abs_path)) :
                msg = "pre_matrix filename '{}' ".format(opt.parlist[0])
                msg+= "does not exist here: {}".format(abs_path)
                ab.EP1(msg)
            self._parent.pre_matrix = abs_path
        else :
            self._parent.pre_matrix = ""

        #align_centers to get pre-transformation matrix
        opt = self.input_options.get('-align_centers')
        if opt != None:  # shouldn't happen, defaults to no
            if((opt.parlist[0]=='yes') or (opt.parlist[0]=='on')):
                self._parent.align_centers = 1
                self.info_msg("turning on align_centers")
                if self._parent.pre_matrix != "" :
                    msg = "Cannot use both align_centers and "
                    msg+= "pre-matrix transformations"
                    ab.EP1(msg)

        #get post-transformation matrix
        opt = self.input_options.get('-post_matrix')
        if opt != None:
            self._parent.post_matrix = opt.parlist[0]
        else :
            self._parent.post_matrix = ""

        # check on the children
        self._parent.child_epis = self.input_options.get('-child_epi')
        if (self._parent.child_epis == None):
            self._parent.child_epis = self.input_options.get('-child_dset2')

        if self._parent.child_epis != None:
            self.info_msg("-child_epi or child_dset2 option given")
            if not(self._parent.epi2anat) :
                if self.save_Al_in    or self.save_tsh or \
                   self.save_vr       or self.save_rep or \
                   self.save_resample or self.save_epi_ns:
                    ab.IP(msg_02i)
                    for child_epi_name in self._parent.child_epis.parlist:
                        child_epi = ab.afni_name(child_epi_name)
                        # it's 11:00, do you know where your children are?
                        if not child_epi.exist():
                            msg = "Could not find child epi/dset2 "
                            msg+= "{}".format(child_epi.input())
                            ab.EP1(msg)
                        else:
                            msg = "Found child epi/dset2 "
                            msg+= "{}".format(child_epi.input())
                            ab.IP(msg)

                else :
                    ab.EP1(msg_01e)
                    self._parent.child_epis = None

        self._parent.child_anats = self.input_options.get('-child_anat')
        if self._parent.child_anats == None:
            self._parent.child_anats = self.input_options.get('-child_dset1')

        if self._parent.child_anats != None:
            self.info_msg("-child_anat option given")
            if not(self._parent.anat2epi) :
                ab.EP1(msg_03e)
                self._parent.child_anats = None
            else :
                for child_anat_name in self._parent.child_anats.parlist:
                    child_anat = ab.afni_name(child_anat_name)
                    # it's 11:00, do you know where your children are?
                    if not child_anat.exist():
                        msg = "Could not find child anat / dset1\n"
                        msg+= "{}".format(child_anat.input())
                        ab.EP1(msg)
                    else:
                        msg = "Found child anat / dset1 "
                        msg+= "{}".format(child_anat.input())
                        ab.IP(msg)

        # output resolution options
        # EPI output bounding box and voxel size
        min_d =  self.min_dim_dset(self._parent.epi)
        mast_dxyz = "" # set default output
        mast_dset = "SOURCE"
        if self._parent.giant_move == 1 :
            mast_dset = "BASE"
            mast_dxyz = "-mast_dxyz {:f}".format(min_d)

        # epi to anat resolution
        opt = self.input_options.get('-master_epi')
        if opt == None:
            # dset2to1 resolution
            opt = self.input_options.get('-master_dset2')

        if opt != None:
            if(opt.parlist[0]!='MIN_DXYZ'):
                if(ab.isFloat(opt.parlist[0])):
                    min_d = float(opt.parlist[0])
                    mast_dxyz = "-mast_dxyz {:f}".format(min_d)
                else:
                    mast_dset = opt.parlist[0]
                    if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                        mast_dxyz = ''
                    else:
                        if(opt.parlist[0]=='SOURCE'):
                            mast_dxyz = ''
                        else:
                            # for BASE, resample by def to min.dimension too
                            mast_dxyz = "-mast_dxyz {:f}".format(min_d)


        opt = self.input_options.get('-master_epi_dxyz')
        if opt == None:
            opt = self.input_options.get('-master_dset2_dxyz')
        if opt != None:
            if(ab.isFloat(opt.parlist[0])):
                mast_dxyz = "-mast_dxyz {!s}".format(opt.parlist[0])
            else:
                ab.IP("Cannot use dxyz setting. Use numbers only ++")
                ab.IP("Using default minimum dimension spacing")

        # set up 3dAllineate output grid - master dataset for bounding
        # box and grid resolution
        if((mast_dset=="SOURCE") and self._parent.align_centers):
            self.master_epi_option = "{!s}".format(mast_dxyz)
            self.master_epi_3dAl_center = 1
        else:
            self.master_epi_option = \
                "-master {!s} {!s}".format(mast_dset, mast_dxyz)
            self.master_epi_3dAl_center = 0
        self.master_epi_dset = mast_dset

        # output resolution options
        # EPI output in TLRC dataset's bounding box
        # voxel size is minimum dimension of EPI by default
        min_d =  self.min_dim_dset(self._parent.epi)
        mast_dxyz = "-mast_dxyz {:f}".format(min_d)
        mast_dset = "BASE"

        # epi to tlrc resolution
        opt = self.input_options.get('-master_tlrc')

        if opt != None:
            if(opt.parlist[0]!='MIN_DXYZ'):
                if(ab.isFloat(opt.parlist[0])):
                    min_d = float(opt.parlist[0])
                    mast_dxyz = "-mast_dxyz {:f}".format(min_d)
                else:
                    mast_dset = opt.parlist[0]
                    if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                        mast_dxyz = ''
                    else:
                        if(opt.parlist[0]=='SOURCE'):
                            mast_dxyz = ''
                        else:
                            # for BASE, resample by def to min.dimension too
                            mast_dxyz = "-mast_dxyz {:f}".format(min_d)

        opt = self.input_options.get('-master_tlrc_dxyz')
        if opt != None:
            if(ab.isFloat(opt.parlist[0])):
                mast_dxyz = "-mast_dxyz {!s}".format(opt.parlist[0])
            else:
                ab.IP("Cannot use dxyz setting. Use numbers only ++")
                ab.IP("Using default minimum dimension spacing")

        # set up 3dAllineate output grid - master dataset for bounding
        # box and grid resolution
        self.master_tlrc_option = \
            "-master {!s} {!s}".format(mast_dset, mast_dxyz)
        self.master_tlrc_dset = mast_dset

        # anat oblique and 3dAllineate output resolution and bounding box
        min_d =  self.min_dim_dset(self._parent.anat0)
        mast_dxyz = ""    # set default output is same as original
        mast_dset = "SOURCE"
        if self._parent.giant_move:
            mast_dset = "BASE"
            mast_dxyz = "-mast_dxyz {:f}".format(min_d)

        self.master_anat_option = "-newgrid {:f}".format(min_d)
        # self.info_msg("Spacing for anat to EPI deobliquing is %f mm"
        # % min_d)

        opt = self.input_options.get('-master_anat')
        if  opt == None:
            opt = self.input_options.get('-master_dset1')

        if opt != None:
            if(opt.parlist[0]!='MIN_DXYZ'):
                if(ab.isFloat(opt.parlist[0])):
                    min_d = float(opt.parlist[0])
                    self.master_anat_option = "-newgrid {:f}".format(min_d)
                    mast_dxyz = "-mast_dxyz {:f}".format(min_d)
                    msg = "Spacing for {} ".format(self.dset1_generic_name)
                    msg+= "to {} ".format(self.dset2_generic_name)
                    msg+= "obliquing is {:f} mm".format(min_d)
                    ab.IP(msg)
                else: # master is not a voxel size, but a dataset
                    mast_dset = opt.parlist[0]
                    # master is a specific dataset name
                    if(opt.parlist[0]!='BASE') and (opt.parlist[0]!='SOURCE'):
                        ttt = "-gridset {}".format(opt.parlist[0])
                        self.master_anat_option = ttt
                        mast_dxyz = ''
                    # master is 'SOURCE' or 'BASE'
                    else:
                        msg = "master anat option is "
                        msg+= "{}".format(opt.parlist[0])
                        ab.IP(msg)
                        msg2 = "Cannot apply BASE or SOURCE as master "
                        msg2+= "for deobliquing. Using default min dim "
                        msg2+= "spacing"
                        ab.IP(msg2)

                        if(opt.parlist[0]=='SOURCE'):
                            mast_dxyz = ''
                        else:
                            mast_dxyz = "-mast_dxyz {:f}".format(min_d)

        opt = self.input_options.get('-master_anat_dxyz')
        if opt == None:
            opt = self.input_options.get('-master_dset1_dxyz')
        if opt != None:
            if(ab.isFloat(opt.parlist[0])):
                mast_dxyz = "-mast_dxyz {!s}".format(opt.parlist[0])
            else:
                msg = "Cannot use dxyz setting. Use numbers only +*"
                ab.WP(msg)
                ab.IP("Using default min dim spacing")

        # set up 3dAllineate output grid - master dataset for bounding
        # box and grid resolution
        if((mast_dset=="SOURCE") and self._parent.align_centers):
            self.master_anat_3dAl_option = "{!s}".format(mast_dxyz)
            self.master_anat_3dAl_center = 1
        else:
            self.master_anat_3dAl_option = \
                "-master {!s} {!s}".format(mast_dset, mast_dxyz)
            self.master_anat_3dAl_center = 0
        self.master_anat_dset = mast_dset

        #get deobliquing options
        opt = self.input_options.get('-deoblique_opts')
        if opt != None:
            self._parent.deoblique_opt = str.join(' ',opt.parlist)
        else:
            self._parent.deoblique_opt = ''

        # user says it's okay to overwrite existing files
        opt = self.input_options.get('-overwrite')
        if opt != None:
            self._parent.rewrite = 1

        # use internal edges to drive alignment
        opt = self.input_options.get('-edge')
        if (opt != None):
            self.edge = 1
            opt = self.input_options.get('-edge_erodelevel')
            if opt == None:
                self._parent.erodelevel = 5
            else:
                self._parent.erodelevel = opt.parlist[0]

            # the cost function is not as important here because the
            # preprocessing steps accommodate for dataset differences
            # - least squares, mutual information or local pearson
            # correlation are all good choices
            opt = self.input_options.get('-cost')
            # local Pearson absolute correlation
            if opt == None: self.cost = 'lpa'

            # set feature size if not already set
            if(featuresize==0):
                featuresize = 0.5
                blursize = 2.0 * featuresize
                aljoin = "-twoblur {} ".format(blursize)
                aljoin+= '-blok "RHDD({})"'.format(featuresize)
                self._parent.AlOpt = "{} {}".format(self._parent.AlOpt,
                                                    aljoin)

        opt = self.input_options.get('-check_cost')
        if opt != None:

            self.checkcost = str.join(' ', opt.parlist)
        else:
            self.checkcost = ""

        opt = self.input_options.get('-multi_cost')
        if opt != None:
            self.multicost = opt.parlist
        else:
            self.multicost = []

        # only the original cost
        self.multicost.append(self.cost)
        for mcost in self.multicost:
            self.info_msg("Multi-cost is {!s}".format(mcost))

        # set alternative output directory
        opt = self.input_options.get('-output_dir')
        if opt != None:
            self.output_dir = opt.parlist[0]
            # end with a slash
            self.output_dir = "{!s}/".format(
                os.path.realpath(self.output_dir)
            )
            msg = "User has selected a new output directory: "
            msg+= "{}".format(self.output_dir)
            ab.IP(msg)

            cmd = "\\mkdir -p {}".format(self.output_dir)
            com = ab.shell_com(cmd, self.oexec)
            stat = com.run()

            print("cd {!s}".format(self.output_dir))
            if not self.dry_run() :
                os.chdir(self.output_dir)
        else :
            self.output_dir = "./"  # just the current directory
        self.anat_dir = self.output_dir
        self.epi_dir = self.output_dir

        # all inputs look okay  - this goes after all inputs. ##########
        return 1


    # turn off all preprocessing steps
    def prep_off(self) :
        """Disable deobliquing, slice timing, volume registration and
        resampling."""

        self.deoblique_flag = 0
        self.tshift_flag = 0
        self.volreg_flag = 0
        self.resample_flag = 0
        msg = "turning off deobliquing tshift, volume registration, "
        msg+= "resampling"
        ab.IP(msg)
        return


    # turn off most preprocessing steps - keep deobliquing though
    def dset_prep_off(self) :
        """Disable timing, volume registration and resampling for
        generic datasets."""

        self.tshift_flag = 0
        self.volreg_flag = 0
        self.resample_flag = 0
        self.info_msg("turning off tshift, volume registration, resampling")
        return


    # find smallest dimension of dataset in x,y,z
    def min_dim_dset(self, dset=None) :
        """Return the smallest absolute voxel dimension of dset.

        Read DELTA with 3dAttribute.  Preserve the dry-run placeholder and
        the fallback to 1.0 when the minimum dimension is zero.
        """

        cmd = "3dAttribute DELTA {}".format(dset.input())
        com = ab.shell_com(cmd, self._parent.oexec,capture=1)
        stat = com.run()

        if  self._parent.dry_run():
            return (1.234567)

        # new purty python way (donated by rick)
        min_dx = min([abs(float(com.val(0,i))) for i in range(3)])

        # old non-pythonesque way
        if 0:
            # some high number to start
            min_dx = 1000.0

            # find the smallest of the 3 dimensions
            i = 0
            while i < 3 :
                dx = abs(float(com.val(0,i)))
                if (dx<min_dx):
                    min_dx = dx
                i += 1

        if min_dx == 0.0 :
            min_dx = 1.0

        return (min_dx)


    # determine if dataset has time shifts in slices
    def tshiftable_dset( self, dset=None) :
        """Return whether dset has slice timing offsets.

        Inspect TAXIS_OFFSETS with 3dAttribute; return 1 in dry-run mode.
        """

        cmd = "3dAttribute TAXIS_OFFSETS {}".format(dset.input())
        com = ab.shell_com(cmd, self._parent.oexec,capture=1)
        stat = com.run()

        if  self._parent.dry_run():
            return (1)

        if len(com.so) : status = 1
        else:            status = 0

        return (status)

    # determine if dataset is oblique:
    def oblique_dset( self, dset=None) :
        """Report and return whether dset has oblique axes.

        Inspect the existing 3dinfo pipeline.  Return 1 for oblique data or
        dry-run mode, otherwise 0.
        """

        cmd = "3dinfo {} | ".format( dset.input())
        cmd+= "\\grep 'Data Axes Tilt:'| \\grep 'Oblique'"
        com = ab.shell_com(cmd, self._parent.oexec, capture=1)
        stat = com.run()

        if self._parent.dry_run() :
            return (1)   # default for dry run is to assume oblique

        if len(com.so) :
            ab.IP( "Dataset {} is ***oblique****".format(dset.input()))
            return (1)

        ab.IP( "Dataset {} is not oblique".format(dset.input()))

        return (0)  # if here, then not oblique


    # parse 1D file of multiple costs for a particular cost
    # the data are arranged like this
    ## 3dAllineate -allcostX1D results:
    ##  ___ ls   ___  ___ sp   ___  ___ mi   ___  ___ crM  ___  ___ nmi  ___  ___ je   ___  ___ hel  ___  ___ crA  ___  ___ crU  ___  ___ lss  ___  ___ lpc  ___  ___ lpa  ___  ___ lpc+ ___  ___ ncd  ___
    #      0.778644     0.410080    -0.108398     0.778821     0.989730    10.462562    -0.018096     0.882749     0.862151     0.221356    -0.009732     0.990268     0.522726     0.999801
    def get_cost(self, costfilename ,costfunction):
        # lpc+ZZ (or lpc+zz) evaluated at end of alignment as lpc
        """Read costfunction from a 3dAllineate allcostX1D result file.

        Treat lpc+ZZ as lpc, and return the parsed float.  Preserve
        the error messages and the legacy fallback value of 1000 on
        read/lookup failure.

        """

        if costfunction.lower() == "lpc+zz" :
            costfunction = "lpc"

        try:
            costfile = open(costfilename,'r')
            # ignore first line. That should just say "3dAllineate
            # -allcostX1D results"
            costfile.readline()
            # read the list of costnames in the second line
            costnames = costfile.readline()
            # remove the underscores and the leading ##
            costnamelist = [(x) for x in costnames.split() \
                            if x != "___" and x != "#" and x != "##" ]
            # read the list of cost values
            costlist = costfile.readline()
            # convert into list structure
            costs = costlist.split()
            costfile.close()
            # make dictionary of names and costs
            costdict = dict(list(zip(costnamelist, costs)))

            # be sure cost function is in the dictionary
            if not costfunction in costnamelist:
                msg = "processing cost list file {!s}".format(costfilename)
                ab.EP1(msg)
                print("   cost '{!s}' not found in dict\n : {!s}".format(
                    costfunction,
                    costdict
                ))

            # get cost value from dictionary (error handling if it
            # doesn't exist in list)
            costvalue = float(costdict[costfunction])
            return (costvalue)
        except:
            ab.EP1("error reading cost list file")

        # find the cost name and then the corresponding value
        # for i, name in enumerate(costnamelist):
        #   if name == costfunction:
        #      costvalue = costs[i]
        #      return costvalue
        return (1000)

    # LONG NOTE
    #
    # align the anatomical data to the epi data using 3dAllineate
    # this is the real meat of the program
    #
    # note for some of the reasoning:
    # The output dataset is the result of the alignment of the
    # anatomical to the EPI.  This is the default and the preferred
    # output for several reasons despite its not being standard to the
    # usual processing in the past.  First, this does not require the
    # EPI data to be resampled other than volume registration.  For
    # medium to large differences of alignment, the EPI data resampled
    # to its original grid may lose effective resolution. This is
    # caused by the typical large difference in slice thickness
    # relative to the EPI x,y voxel size within slice resolution.
    # Secondly, the anatomical data is usually higher resolution and
    # relatively isotropic in voxel dimensions.  Thirdly, the
    # anatomical dataset is typically used for structural reference
    # while the EPI voxel values matter for the analysis. Slight
    # blurring of the anatomical data is relatively unimportant.
    # Fourthly, the anatomical dataset's higher resolution allows for
    # finer structural alignment (versus downsampling the anatomical
    # to match the EPI).  One could get around these various issues by
    # using the inverse transform as done in the epi2anat method and
    # then resampling the output grid to a finer resolution, but this
    # will not usually be necessary.

    def align_anat2epi(  self, e=None, a=None, m=None, \
                             alopt=" -onepass -weight_frac 1.0 -maxrot 6 " \
                                    "-maxshf 10 -warp aff ",\
                             suf = "_alnd_epi", costfunction = "lpc"):
                             #m is the weight brick
        """Compute anatomy-to-EPI alignment and any requested child outputs.

        e and a are the reference EPI and source anatomy afni_name objects;
        m is an optional weight dataset.  alopt supplies additional alignment
        options, suf names outputs, and costfunction selects the cost.
        Save/compose matrices and honor flip, grid and overwrite controls.
        Return aligned anatomy and weight datasets, or (None, None) when the
        primary 3dAllineate command fails.
        """

        msg = "Aligning {} data ".format(self._parent.dset1_generic_name)
        msg+= "to %s data".format(self._parent.dset2_generic_name)
        ab.IP(msg)

        # for oblique data or pre and post transformed data, save anat
        # to epi transformation matrix in separate temporary 1D file
        if ( self._parent.obl_a2e_mat != "" or \
             self._parent.pre_matrix != "" ) or self._parent.edge :
            o = a.new("{!s}_temp{!s}".format(a.out_prefix(), suf))

            self.anat_mat = "{}{}{}_e2a_only_mat.aff12.1D".format(
                o.p(), self._parent.anat0.out_prefix(), suf)
        else:
            # save transformation matrix with original anatomical
            # name,suf,...
            self.anat_mat = "{}{}{}_mat.aff12.1D".format(
                a.p(), self._parent.anat0.out_prefix(),suf)
            if (self._parent.anat2epi) or (self._parent.flip):
                # save the permanent data
                o = a.new("{}{}".format(self._parent.anat0.out_prefix(),
                                        suf))
            else:
                # save temporary copy
                o = a.new("__tt_{}{}".format(self._parent.anat0.out_prefix(),
                                             suf))

        ow = a.new("{}{}_wtal".format(a.out_prefix(),
                                      suf))
        of = a.new("{}_flip{}".format(self._parent.anat0.out_prefix(),
                                      suf))
        olr = a.new("__tt_{}_lr{}".format(self._parent.anat0.out_prefix(),
                                          suf))

        if(self.master_anat_dset=='BASE'):
            o.view   = e.view
            ow.view  = e.view
            of.view  = e.view
            olr.view = e.view
        else:
            if(self.master_anat_dset=='SOURCE'):
                o.view   = a.view
                ow.view  = a.view
                of.view  = a.view
                olr.view = a.view
            else:
                manat    = ab.afni_name(self.master_anat_dset)
                o.view   = manat.view
                ow.view  = manat.view
                of.view  = manat.view
                olr.view = manat.view

        anatview = o.view

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            ow.delete(self._parent.oexec)
            if m:
                wtopt = "-wtprefix {}{} ".format(ow.p(), ow.out_prefix())
                wtopt+= "-weight {}".format(m.input())
            else:
                wtopt = "-wtprefix {}{} ".format(ow.p(), ow.out_prefix())
            if(self._parent.cmass==""):
                cmass = ""
            else:
                cmass = "-{!s}".format(self._parent.cmass)
            if(self._parent.checkcost==""):
                checkstr = ""
            else:
                checkstr = "-check {!s}".format(self._parent.checkcost)
            # costfunction       \
            # weighting          \
            # center of mass options (cmass) \
            # master grid, other 3dAllineate options \
            #   (may be user specified)   \
            cmd = "3dAllineate "
            cmd+= "-{!s} {!s} ".format(costfunction, wtopt)
            cmd+= "-source {!s} ".format(a.input())
            cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
            cmd+= "-base {!s} {!s} ".format(e.input(), cmass)
            cmd+= "-1Dmatrix_save {!s} {!s} {!s} {!s} ".format(
                self.anat_mat,
                self.master_anat_3dAl_option,
                alopt,
                checkstr
            )
            com = ab.shell_com(cmd, self._parent.oexec)
            # if this fails, notify the user
            stat = com.run()

            if stat :
                ab.EP1("3dAllineate failure")
                return None, None

            e2a_mat = self.anat_mat

            if (self._parent.flip):
                cmd = "3dLRflip "
                cmd+= "-prefix {!s}{!s} ".format(olr.p(), olr.out_prefix())
                cmd+= "-overwrite {!s}".format(a.input())
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dLRflip stage")
                    self.ciao(stat)

                if self._parent.flip_giant and not(self._parent.giant_move) :
                    alopt =  \
                    (
                        "{!s} -twobest 11 -twopass -maxrot 45 "
                        "-maxshf 40 -source_automask+2"
                    ).format(
                        self._parent.AlOpt
                    )
                    self._parent.cmass = "cmass"
                # flips don't need weight
                if m:
                    wtopt = "-weight {!s}".format(m.input())
                else:
                    wtopt = ""

                # save transformation matrix with original anatomical
                # name,suf,...
                self.flip_mat = "{!s}{!s}_flip_{!s}_mat.aff12.1D".format(
                    a.p(),
                    self._parent.anat0.out_prefix(),
                    suf
                )

                # costfunction       \
                # weighting          \
                # center of mass options (cmass) \
                # master grid, other 3dAllineate options \
                #   (may be user specified)   \
                cmd = "3dAllineate "
                cmd+= "-{!s} {!s} ".format(costfunction, wtopt)
                cmd+= "-source {!s} ".format(olr.input())
                cmd+= "-prefix {!s}{!s} ".format(of.p(), of.out_prefix())
                cmd+= "-base {!s} {!s} ".format(e.input(), cmass)
                cmd+= "-1Dmatrix_save {!s} {!s} {!s} {!s} ".format(
                    self.flip_mat,
                    self.master_anat_3dAl_option,
                    alopt,
                    checkstr
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dAllineate stage")
                    self.ciao(stat)

                # weighting          \
                # center of mass options (cmass) \
                # master grid, other 3dAllineate options \
                cmd = "3dAllineate "
                cmd+= (
                    "-allcostX1D IDENTITY __tt_lr_noflipcosts.1D {!s} "
                ).format(wtopt)
                cmd+= "-source {!s} ".format(o.input())
                cmd+= "-base {!s} {!s} {!s} {!s} {!s} ".format(
                    e.input(),
                    cmass,
                    self.master_anat_3dAl_option,
                    alopt,
                    checkstr
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dAllineate allcostX1D stage")
                    self.ciao(stat)

                # weighting          \
                # center of mass options (cmass) \
                # master grid, other 3dAllineate options \
                cmd = "3dAllineate "
                cmd+= "-allcostX1D IDENTITY __tt_lr_flipcosts.1D {!s} ".format(
                    wtopt
                )
                cmd+= "-source {!s} ".format(of.input())
                cmd+= "-base {!s} {!s} {!s} {!s} {!s} ".format(
                    e.input(),
                    cmass,
                    self.master_anat_3dAl_option,
                    alopt,
                    checkstr
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dAllineate stage")
                    self.ciao(stat)

                noflipcost = self.get_cost("__tt_lr_noflipcosts.1D",
                    costfunction)
                print("No flip cost is {:f} for {!s} cost function".format(
                    noflipcost,
                    costfunction
                ))
                flipcost = self.get_cost("__tt_lr_flipcosts.1D",costfunction)
                print("Flip cost is {:f} for {!s} cost function".format(
                    flipcost,
                    costfunction
                ))
                outname = 'aea_checkflip_results.txt'
                try:
                    f = open(outname, 'w')
                    f.write("flip_cost_orig : {:f}\n".format(noflipcost))
                    f.write("flip_cost_flipped : {:f}\n".format(flipcost))
                    f.write("flip_cost_func : {!s}\n".format(costfunction))
                    # [PT: Jun 4, 2023] provide full prefix name (with
                    # '.HEAD'), for possible file loading in NiiVue later
                    f.write("flip_base: {!s}\n".format(e.shortinput(head=1)))
                    f.write("flip_dset_orig : {!s}\n".format(
                        o.shortinput(head=1)
                    ))
                    f.write("flip_dset_flipped : {!s}\n".format(
                        of.shortinput(head=1)
                    ))
                    if(flipcost < noflipcost):
                        print(
                            "WARNING: ************ flipped data aligns "
                            "better than original data\n"
                              "Check for left - right flipping in the GUI "
                            "************************")
                        f.write("flip_guess : DO_FLIP\n")
                    else:
                        f.write("flip_guess : NO_FLIP\n")
                        print("Data does not need flipping")
                    f.close()
                except:
                    print("WARNING: Could not write to flip text file")

            # if rigid_equiv, then save only rigid equivalent alignment
            # extract with cat_matvec
            if(self._parent.rigid_equiv):
                self.info_msg( \
                    "Reducing alignment transformation to rigid equivalent")
                cmd = "cat_matvec "
                cmd+= "-ONELINE {!s} ".format(self.anat_mat)
                cmd+= (
                    "-P > __tt_temp.1D ; sleep 1;                   mv "
                    "__tt_temp.1D {!s}"
                ).format(
                    self.anat_mat
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run();

                if stat :
                    ab.EP1("Failed at a cat_matvec stage")
                    self.ciao(stat)

            # if not doing alignment for anat2epi, just return now,
            # and use the xform matrix later
            if (not(self._parent.anat2epi)):
                return o, ow

            if((self._parent.obl_a2e_mat!="")  or self._parent.edge ) :
                # save the permanent data
                o = ab.afni_name("{!s}{!s}{!s}{!s}".format(
                    self._parent.anat_dir,
                    self._parent.anat0.out_prefix(),
                    suf,
                    self._parent.anat0.view
                ))
                if (not o.exist() or self._parent.rewrite
                    or self._parent.dry_run()):
                    o.delete(self._parent.oexec)
                else:
                    self.exists_msg(o.input())

                # for oblique and edge data,
                # need to apply matrix to skullstripped anat
                if(self._parent.obl_a2e_mat!=""):
                    # overall transformation A to E is (E2A^-1 PreShift/Oblique)
                    # 1Dmatrix_apply takes E to A as input so inverse
                    #    E to A = Obl^-1 E2A
                    obl_mat = "{!s} -I".format(self._parent.obl_a2e_mat)
                    e2a_mat = "{!s}{!s}{!s}_mat.aff12.1D".format(
                        a.p(),
                        self._parent.anat0.out_prefix(),
                        suf
                    )
                    # combine transformations
                    # earliest transformation is last as input to cat_matvec
                    cmd = "cat_matvec "
                    cmd+= "-ONELINE {!s} {!s} > {!s}".format(
                        self.anat_mat,
                        obl_mat,
                        e2a_mat
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run()

                    if stat :
                        ab.EP1("Failed at a cat_matvec stage")
                        self.ciao(stat)

                    self.info_msg( \
                        "Combining {!s} to {!s} and oblique "
                        "transformations".format(
                        self._parent.dset1_generic_name,
                        self._parent.dset2_generic_name
                    ))

                else:   # just apply the matrix to the original data (edges)
                    self.info_msg(
                        "Applying transformation to skullstripped {!s}".format(
                        self._parent.anat_ns0.input()
                    ))
                    e2a_mat = "{!s}{!s}{!s}_mat.aff12.1D".format(
                        a.p(),
                        self._parent.anat0.out_prefix(),
                        suf
                    )
                    # input to cat_matvec
                    cmd = "mv {!s} {!s}".format(self.anat_mat, e2a_mat)
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run();

                    if stat :
                        ab.EP1("Failed at a mv stage")
                        self.ciao(stat)

                    self.info_msg( \
                        "renaming e2a transformation matrix is standard "
                        "matrix (no obliquity)" )
                    self.anat_mat = e2a_mat

                cmd = "3dAllineate "
                cmd+= "-base {!s} ".format(e.input())
                cmd+= "-1Dmatrix_apply {!s} ".format(e2a_mat)
                cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
                cmd+= "-input {!s}  {!s} {!s}".format(
                    self._parent.anat_ns0.input(),
                    self.master_anat_3dAl_option,
                    alopt
                )
                com = ab.shell_com(cmd, self._parent.oexec)

                stat = com.run()
        else:
            self.exists_msg(o.input())
        # process the children
        if self._parent.child_anats != None:
            if (self._parent.anat2epi):
                for child_anat_name in self._parent.child_anats.parlist:
                    child_anat = ab.afni_name(child_anat_name)

                    # skip the parent if it's included
                    if(child_anat.input()==self._parent.anat0.input()) :
                        child_anat_out = ab.afni_name(
                            "{!s}{!s}_child{!s}{!s}".format(
                            child_anat.p(),
                            child_anat.prefix,
                            suf,
                            child_anat.view
                        ))
                    else:
                        child_anat_out=ab.afni_name("{!s}{!s}{!s}{!s}".format(
                            child_anat.p(),
                            child_anat.prefix,
                            suf,
                            child_anat.view
                        ))

                    child_anat_out.view = anatview     # child_anat.view
                    self.info_msg("Processing child {!s}: {!s}".format(
                        self._parent.dset1_generic_name,
                        child_anat.ppv()
                    ))
                    if (self._parent.rewrite) :
                        overwritestr = "-overwrite"
                        child_anat_out.delete(self._parent.oexec)
                    else :
                        overwritestr = ""
                    cmd = "3dAllineate "
                    cmd+= "-base {!s} ".format(e.input())
                    cmd+= "-1Dmatrix_apply {!s} ".format(e2a_mat)
                    cmd+= "-prefix {!s}{!s} ".format(
                        child_anat_out.p(),
                        child_anat_out.out_prefix()
                    )
                    cmd+= "-input {!s}  {!s} {!s} {!s}".format(
                        child_anat.input(),
                        self.master_anat_3dAl_option,
                        alopt,
                        overwritestr
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)

                    stat = com.run()

                    if stat :
                        ab.EP1("Failed at a 3dAllineate stage")
                        self.ciao(stat)

        return o, ow


    # Some notes on this alignment matrix mechanics are sorely needed
    # because it gets somewhat involved and takes a good deal of time to explain
    #  and to understand all the possible combinations of cases.
    #  Let's first define a series of "spaces" or coordinate systems where
    #  datasets are in alignment with that particular space's original dataset.
    #
    # ac = AnatCard (the usual original anatomical dataset without any obliquity
    # transformation applied, in what we call cardinal or cardinalized space )
    #  ao = AnatReal (the deobliqued version of the above transformed by its
    #                     IJK_TO_DICOM_REAL transformation matrix)
    # ec = EPICard  (similar to AnatCard but the EPI cardinalized dataset, our
    # starting dset)
    #  eo = EPIReal  (similar to AnatReal but the deobliqued EPI dataset)
    #
    # The usual case we calculate is the transformation from A_ac to A_ec for
    # the anatomical dataset
    # or the reverse E_ec to E_ac for the epi dataset where the "subscript"
    # signifies the space
    # Each transformation is computed for a specific space:
    # A2E = A_ec2E_ec_base = the transformation computed by 3dAllineate to
    # transform the anatomical in
    # the EPI's cardinal space to be in alignment with the EPI base sub-brick
    # E_ec_base = the EPI in its own cardinalized space. This is the sub-brick
    # used by 3dAllineate
    # and the one used for 3dAllineate and the one used as a reference for
    # 3dvolreg
    #
    # The "oblique" transformation of the anatomical dataset that transforms the
    # original cardinalized
    # anatomical dataset to the EPI's cardinalized space is done inside 3dWarp
    # as
    #  Obl = A_ac2A_ec = (T_eo T_ec^-1)^-1 (T_ao T_ac^-1)
    #

    # The time-series motion correction volume registration can be described as
    # a transformation
    #  of the EPI sub-bricks to the EPI base sub-brick transformation
    #  VR = E_ec_i2E_ec_base

    # An additional initial shift or pre-transformation on the original
    # anatomical dataset
    #  (such as produced by @Align_Centers)
    # In this implementation, we'll treat this transformation as a substitute
    # for the oblique
    #  transformation A_ac2A_ec

    # One may also choose an additional post-transformation matrix like the one
    # produced by
    # 3dTagalign usually instead of a tlrc transformation. In this case, the
    # tlrc transformation
    # would be defined by the post transformation matrix instead of the Warp
    # transformation
    # in the tlrc_apar dataset's header.

    # Both the pre and  post transformation matrices could potentially be
    # applied
    # many different ways, but until there's a need for something different,
    # let's use it only as
    # a substitute for the obliquity and tlrc warp transformation
    # matrices,respectively.

    # There are the complications that the 1Dmatrix_save and 1Dmatrix_apply use
    # the reverse of what
    # you might expect, but at least they're consistent. The matrices that come
    # from those options
    # (used by both 3dvolreg and 3dAllineate) are the transformation of base to
    # source
    # (output to input). So each of these transformations is the inverse of the
    # transformation
    # performed or how to get back to where you were. In this program, there are
    # two usages
    #   3dvolreg saves the VR^-1 transformation (Ebase->Eiec)
    #   3dAllineate saves the E2A transformation (A2E^-1 or E_ec2A_ac)
    # Applying also uses the same format of base to source,
    #  so needs to be inverted from source to base
    # A linear algebra reminder here is the inverse of the product of matrices
    # is equal to
    #  the reverse order with each matrix inverted
    # (A B)^-1 = B^-1 A^-1
    # Also the order of operations is first transformation is on right, then
    # proceeds to left
    #   before inverting for 1Dmatrix_apply

    # So the usual case (-anat2epi without any extra shift) is A_ac to A_ec
    #   and may include oblique transformation
    # Case 1: A_ac to E_ec
    #  A_ac  (A_ac2A_ec) A_ec (A_ec2E_ec_base) A_ec_base = A_ec2E_ec A_ac2A_ec
    #        = A2E Obl
    # matrix_apply (inverse) = Obl^-1 E2A
    #    E2A is the output of 3dAllineate of A2E

    # Case 2: E_ec to A_ac (-epi2anat)
    #  may also include obliquity and volume registration
    # E_ec_i _E_ec_i2E_ec_base_ E_ec_base _A_ec2E_ec^-1_ A_ec _A_ac2A_ec^-1_
    # A_ac
    #    = A_ac2A_ec^-1  A_ec2E_ec^-1 E_ec_i2E_ec_base
    #    = Obl^-1 A2E^-1 VR
    # matrix_apply (inverse) = VR^-1 A2E Obl = VR^-1 E2A^-1 Obl
    #    E2A is the output of 3dAllineate of A2E, VR^-1 is output of 3dvolreg

    #
    # Case 3: E_ec_i to A_at (-epi2anat -tlrc_apar ...)
    # talairach transformation of follower EPI to talairach anat)
    # E_ec_i (E_ec_i2E_ec_base) E_ec_base (A_ec2E_ec^-1) A_ec (A_ac2A_ec^-1)
    # A_ac (A_ac2A_at) A_at
    #    = A_ac2A_at A_ac2A_ec^-1  A_ec2E_ec^-1 E_ec_i2E_ec_base
    #    = Tlrc Obl^-1 A2E^-1 VR
    # matrix_apply (inverse) = VR^-1 A2E Obl Tlrc^-1 = VR^-1 E2A^-1 Obl Tlrc^-1
    #    E2A is the output of 3dAllineate of A2E, VR^-1 is output of 3dvolreg
    #


       # align the epi to the anatomical but do it using the inverse
       # transformation of the alignment of anat to epi
    def align_epi2anat(  self, e=None, a=None, \
          alopt="",\
          suf = "_alnd_anat"):
        # tlrc space output null by default
        """Apply the inverse anatomy alignment to EPI and optional template space.

        e and a are source EPI and reference anatomy afni_name objects.  Use
        alopt and suf for command options and output names.  Compose the parent
        alignment with this instance's volume-registration matrix and optional
        oblique/post/template transforms.  Return the EPI and template outputs;
        the latter retains the legacy empty-list value when not produced.
        """

        t = []
        self.info_msg(" Applying alignment for {!s} to {!s}".format(
            self._parent.dset2_generic_name,
            self._parent.dset1_generic_name
        ))

        o = e.new("{!s}{!s}".format(self.epi_afniformat.out_prefix(), suf))
        o.path = self._parent.output_dir

        if(self.master_epi_dset == 'SOURCE'):
            o.view = "{!s}".format(e.view)
            if(not(o.view)) :
                o.view = au.dset_view(self.master_epi_dset)

        else:
            if(self.master_epi_dset == 'BASE'):
                o.view = "{!s}".format(a.view)
                if(not(o.view)) :
                    o.view = au.dset_view(a.ppve())


            else:
                mepi = ab.afni_name(self.master_epi_dset)
                o.view = mepi.view
                if(not(o.view)) :
                    o.view = au.dset_view(self.master_epi_dset)

        if(not(o.view)) :
            o.view = '+orig'
        eview = "{!s}".format(o.view)

        # allow overwrite in AFNI commands
        if (self._parent.rewrite) :
            owrite = "-overwrite"
        else:
            owrite = ""

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            o.view = eview
            # anat_mat = "%s%s_mat.aff12.1D" %  (ps.anat0.out_prefix(),suf)
            epi_mat = "{!s}{!s}{!s}_mat.aff12.1D".format(
                o.p(),
                self.epi_afniformat.out_prefix(),
                suf
            )
            self.info_msg("Inverting {!s} to {!s} matrix".format(
                self._parent.dset1_generic_name,
                self._parent.dset2_generic_name
            ))

            if(self._parent.obl_a2e_mat!="") :
                oblique_mat = "{!s}".format(self._parent.obl_a2e_mat)
            else :
                oblique_mat = ""

            cmd = "cat_matvec "
            cmd+= "-ONELINE {!s} {!s} ".format(
                oblique_mat,
                self._parent.anat_mat
            )
            cmd+= "-I > {!s}".format(epi_mat)
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();

            if stat :
                ab.EP1("Failed at a cat_matvec stage")
                self.ciao(stat)

            # concatenate volume registration from epi data
            if(self._parent.volreg_flag):
                self.info_msg(
                    "Concatenating volreg and {!s} to {!s} "
                    "transformations".format(
                    self._parent.dset2_generic_name,
                    self._parent.dset1_generic_name
                ))

                epi_mat = "{!s}{!s}{!s}_reg_mat.aff12.1D".format(
                    o.p(),
                    self.epi_afniformat.out_prefix(),
                    suf
                )
                cmd = "cat_matvec "
                cmd+= "-ONELINE {!s} {!s} ".format(
                    oblique_mat,
                    self._parent.anat_mat
                )
                cmd+= "-I {!s} > {!s}".format(self.reg_mat, epi_mat)
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run();

                if stat :
                    ab.EP1("Failed at a cat_matvec stage")
                    self.ciao(stat)

            self.info_msg( "Applying transformation of {!s} to {!s}".format(
                self._parent.dset2_generic_name,
                self._parent.dset1_generic_name
            ))
            cmd = "3dAllineate "
            cmd+= "-base {!s} ".format(a.input())
            cmd+= "-1Dmatrix_apply {!s} ".format(epi_mat)
            cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
            cmd+= "-input {!s}  {!s} {!s} {!s}".format(
                e.input(),
                self.master_epi_option,
                alopt,
                owrite
            )
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()

            if stat :
                ab.EP1("Failed at a 3dAllineate stage")
                self.ciao(stat)

            # mark as not oblique if deobliqued
            if oblique_mat != "" :
                o.view = eview
                cmd = "3drefit "
                cmd+= "-deoblique {!s}".format(o.input())
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3drefit stage")
                    self.ciao(stat)

            # concatenate talairach or post transformation
            if((self._parent.tlrc_apar != "") or (
                self._parent.post_matrix !="")):

                self.info_msg( (
                    "Concatenating talairach/post, volume "
                    "registration, {!s} to {!s} transformations"
                ).format(
                    self._parent.dset2_generic_name,
                    self._parent.dset1_generic_name
                ))


                if(self._parent.post_matrix != ""):
                    anat_tlrc_mat = self._parent.post_matrix
                    epi_mat = "{!s}{!s}{!s}_post_mat.aff12.1D".format(
                        o.p(),
                        self.epi_afniformat.out_prefix(),
                        suf
                    )

                # tlrc parent trumps post_matrix
                if(self._parent.tlrc_apar != ""):
                    cmd = "3dAttribute WARP_TYPE {!s}".format(
                        self._parent.tlrc_apar.input()
                    )
                    com = ab.shell_com(cmd, self._parent.oexec, capture=1)
                    stat = com.run();

                    if stat :
                        ab.EP1("Failed at a 3dAttribute stage")
                        self.ciao(stat)

                    if(com.status != 0) :
                        msg = "Warp type not defined for "
                        msg+= "this dataset: {!s}".format(
                            self._parent.tlrc_apar.input()
                        )
                        ab.EP1(msg)
                        #return o,t
                        self.ciao(1)

                    tlrc_type = int(com.val(0,0))
                    if(tlrc_type != 0) :
                        msg = "Cannot compute "
                        msg+= "transformations for manually "
                        msg+= "talairached data"
                        ab.EP1(msg)
                        return o,t

                    anat_tlrc_mat = "{!s}::WARP_DATA".format(
                        self._parent.tlrc_apar.input()
                    )
                    epi_mat = "{!s}{!s}{!s}_tlrc_mat.aff12.1D".format(
                        o.p(),
                        self.epi_afniformat.out_prefix(),
                        suf
                    )

                # note registration matrix, reg_mat, can be blank and ignored
                cmd = "cat_matvec "
                cmd+= "-ONELINE {!s} ".format(anat_tlrc_mat)
                cmd+= "-I {!s} {!s} ".format(oblique_mat, self._parent.anat_mat)
                cmd+= "-I {!s}  > {!s}".format(self.reg_mat, epi_mat)
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run();

                if stat :
                    ab.EP1("Failed at a cat_matvec stage")
                    self.ciao(stat)

                if(self._parent.tlrc_apar!=""):
                    tlrc_dset = ab.afni_name("{!s}_tlrc{!s}+tlrc".format(
                        self.epi_afniformat.prefix,
                        suf
                    ))
                    tlrc_dset.path = o.p()
                    # tlrc_dset.view = ps.tlrc_apar.view  '+tlrc'
                    if(self.master_tlrc_dset=='SOURCE'):
                        tlrc_dset.view = e.view
                    else:
                        if(self.master_tlrc_dset=='BASE'):
                            tlrc_dset.view = self._parent.tlrc_apar.view
                        else:
                            mtlrc = ab.afni_name(self.master_tlrc_dset)
                            tlrc_dset.view = mtlrc.view

                    if tlrc_dset.exist():
                        tlrc_dset.delete(self._parent.oexec)
                    atlrcpost = tlrc_dset
                    self.info_msg(  \
                        (
                        "Applying transformation of {!s} to {!s} "
                        "tlrc parent"
                    ).format(
                        self._parent.dset2_generic_name,
                        self._parent.dset1_generic_name
                    ))

                    cmd = "3dAllineate "
                    cmd+= "-base {!s} ".format(self._parent.tlrc_apar.input())
                    cmd+= "-1Dmatrix_apply {!s} ".format(epi_mat)
                    cmd+= "-prefix {!s}{!s} ".format(
                        atlrcpost.p(),
                        atlrcpost.prefix
                    )
                    cmd+= "-input {!s} ".format(e.input())
                    cmd+= "-verb {!s} {!s} {!s}".format(
                        self._parent.master_tlrc_option,
                        alopt,
                        owrite
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)

                else:
                    # [pt] !!!! This string is missing something:
                    # there are 3 %s, but only 2 strings provided !!!!
                    tlrc_orig_dset = ab.afni_name("%s%s_post%s" % (
                        self.epi_afniformat.out_prefix(), suf))
                    tlrc_orig_dset = o.p()
                    tlrc_orig_dset.view = '+orig'
                    base_dset = a
                    if(self.master_tlrc_dset=='SOURCE'):
                        tlrc_orig_dset.view = e.view
                    else:
                        if(self.master_tlrc_dset=='BASE'):
                            tlrc_orig_dset.view = a.view
                        else:
                            mtlrc = ab.afni_name(self.master_tlrc_dset)
                            tlrc_orig_dset.view = mtlrc.view
                            base_dset = mtlrc

                    if tlrc_orig_dset.exist():
                        tlrc_orig_dset.delete(self._parent.oexec)
                    atlrcpost = tlrc_orig_dset
                    self.info_msg(
                        "Applying post transformation matrix to {!s}".format(
                        self._parent.dset2_generic_name
                    ))

                    cmd = "3dAllineate "
                    cmd+= "-base {!s} ".format(base_dset.input())
                    cmd+= "-1Dmatrix_apply {!s} ".format(epi_mat)
                    cmd+= "-prefix {!s}{!s} ".format(
                        atlrcpost.p(),
                        atlrcpost.input()
                    )
                    cmd+= "-input {!s} ".format(e.input())
                    cmd+= "-verb {!s} {!s} {!s}".format(
                        self._parent.master_tlrc_option,
                        alopt,
                        owrite
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)

                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dAllineate stage")
                    self.ciao(stat)

                # remove obliquity from output
                if(atlrcpost.type == 'BRIK'):
                    # force +tlrc output for master SOURCE option -
                    # 3dAllineate; saves this as +orig
                    if((self._parent.master_tlrc_dset=="SOURCE") and (
                        self._parent.tlrc_apar!="")):
                        cmd = "3drefit "
                        cmd+= "-deoblique "
                        cmd+= "-view tlrc {!s}{!s}+orig".format(
                            atlrcpost.p(),
                            atlrcpost.prefix
                        )
                        com = ab.shell_com(cmd, self._parent.oexec)
                        stat = com.run()

                        if stat :
                            ab.EP1("Failed at a 3drefit stage")
                            self.ciao(stat)

                    else:
                        if(oblique_mat!=""):
                            cmd = "3drefit "
                            cmd+= "-deoblique {!s}{!s}+tlrc".format(
                                atlrcpost.p(),
                                atlrcpost.prefix
                            )
                            com = ab.shell_com(cmd, self._parent.oexec)
                            stat = com.run()

                            if stat :
                                ab.EP1("Failed at a 3drefit stage")
                                self.ciao(stat)

                t = atlrcpost
        else:
            self.exists_msg(o.input())

        return o, t


    # reduce EPI dataset to a single representative sub-brick
    def tstat_epi(self, e=None, tstat_opt="", prefix = "temp_ts"  ):
        """Select the representative EPI sub-brick or temporal statistic.

        Use the configured epi_base with e, tstat_opt and output prefix.
        Return the output afni_name or None when output creation fails.
        """

        o = ab.afni_name(prefix)
        o.to_afni(new_view=au.dset_view(o.ppve()))
        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            # if more than 1 sub-brick
            if (self._parent.dry_run() or \
                ((not self._parent.dry_run() and ab.dset_dims(e.input())[
                3] > 1))):
            # could be: if number choose bucket else use that as stat
            # choose a statistic as representative
                self.info_msg("Creating representative {!s} sub-brick".format(
                    self._parent.dset2_generic_name
                ))

                # if an integer, choose a single sub-brick
                if(self._parent.epi_base.isdigit()):
                # if an integer, choose a single sub-brick
                    cmd = "3dbucket "
                    cmd+= "-prefix {!s} {!s}'[{!s}]'".format(
                        o.pp(),
                        e.input(),
                        self._parent.epi_base
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                else:
                    if((self._parent.epi_base=='median') or (
                        self._parent.epi_base=='max') or
                    (self._parent.epi_base=='mean')):
                        cmd = "3dTstat "
                        cmd+= "-{!s} ".format(self._parent.epi_base)
                        cmd+= "-prefix {!s} {!s}".format(o.pp(), e.input())
                        com = ab.shell_com(cmd, self._parent.oexec)
                    else:
                        self.info_msg(
                          "using 0th sub-brick - assuming epi_base is "
                            "dataset name" )
                        cmd = "3dbucket "
                        cmd+= "-prefix {!s} {!s}'[0]'".format(o.pp(), 
                                                              e.input())
                        com = ab.shell_com(cmd, self._parent.oexec)
            else:   # choose a single sub-brick (sub-brick 0)
                self.info_msg("using 0th sub-brick because only one found")
                cmd = "3dbucket "
                cmd+= "-prefix {!s} {!s}'[0]'".format(o.pp(), e.input())
                com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()

            if stat :
                ab.EP1("Failed at a 3dbucket or 3dTstat stage")
                self.ciao(stat)

            if (not o.exist() and not self._parent.dry_run()):
                o.show
                ab.EP1("Could not 3dTstat epi")
                return None
        else:
            self.exists_msg(o.input())

        return o


    # create edge dataset of internal brain structure edges
    def edge_dset(self, e=None, prefix = "temp_edge", binarize=0,erodelevel=2):
        """Create internal edges from dataset e at the requested prefix.

        Use erodelevel for the outer-layer removal and binarize for optional
        binary edges.  Return the edge dataset or None on a checked failure.
        """

        o = ab.afni_name(prefix)
        o.view = e.view
        m = ab.afni_name("{!s}_edge_mask".format(o.prefix))
        m.view = e.view

        if (not m.exist() or self._parent.rewrite or self._parent.dry_run()):
            m.delete(self._parent.oexec)
            self.info_msg("Creating edge dataset")
            cmd = "3dAutomask "
            cmd+= "-overwrite "
            cmd+= "-erode {!s} ".format(erodelevel)
            cmd+= "-prefix {!s} {!s}".format(m.prefix, e.input())
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()

            if stat :
                ab.EP1("Failed at a 3dAutomask stage")
                self.ciao(stat)

            if(binarize):
                lprefix = "{!s}_edge_cvar".format(o.prefix)
            else:
                lprefix = o.prefix

            cmd = "3dLocalstat "
            cmd+= "-overwrite "
            cmd+= "-mask {!s} ".format(m.ppv())
            cmd+= "-nbhd 'RECT(-2,-2,-1)' "
            cmd+= "-stat cvar "
            cmd+= "-prefix {!s} {!s}".format(lprefix, e.input())
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();

            if stat :
                ab.EP1("Failed at a 3dLocalstat stage")
                self.ciao(stat)

            if(binarize):
                cmd = "3dhistog "
                cmd+= "-omit 0 "
                cmd+= "-max 1 "
                cmd+= (
                    "-nbins 1000 {!s}_edge_cvar{!s}  > {!s}_edge_histo.1D"
                ).format(
                    prefix,
                    o.view,
                    prefix
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dhistog stage")
                    self.ciao(stat)

                cmd = "3dTstat "
                cmd+= "-argmax "
                cmd+= (
                    "-prefix {!s}_edge_histomax "
                    "{!s}_edge_histo.1D'[1]'\\' "
                ).format(
                    prefix,
                    prefix
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dTstat stage")
                    self.ciao(stat)

                cmd = "1dcat {!s}_edge_histomax.1D".format(prefix)
                com = ab.shell_com(cmd, self._parent.oexec, capture=1)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 1dcat stage")
                    self.ciao(stat)

                if(self._parent.dry_run()): edgeindex = 123
                else:  edgeindex = int(com.val(0,0))

                cmd = "1dcat {!s}_edge_histo.1D'[0]{{{:d}}}'".format(
                    prefix,
                    edgeindex
                )
                com = ab.shell_com(cmd, self._parent.oexec, capture=1)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 1dcat stage")
                    self.ciao(stat)

                if(self._parent.dry_run()): edgevalue = 0.0123
                else:  edgevalue = float(com.val(0,0))

                # threshold anatomical and EPI edge data
                self.info_msg("Thresholding edges at {:f}".format(edgevalue))
                cmd = "3dcalc "
                cmd+= "-a {!s}_edge_cvar{!s} ".format(prefix, o.view)
                cmd+= "-overwrite "
                cmd+= "-expr \"step(a-{:f})\" ".format(edgevalue)
                cmd+= "-prefix {!s}".format(o.out_prefix())
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dcalc stage")
                    self.ciao(stat)

            if (not o.exist() and not self._parent.dry_run()):
                msg = "Could not create edge dataset {!s}".format(o.ppv())
                ab.EP1(msg)
                return None
        else:
            self.exists_msg(o.input())


        return o



    # deoblique epi dataset
    def deoblique_epi(self, e=None, deoblique_opt="", prefix="temp_deob"):
        """Deoblique EPI dataset e with 3dWarp and deoblique_opt.

        Write prefix and return its afni_name, or None on a checked failure.
        """

        o = e.new(prefix)
        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            self.info_msg( "Deobliquing")
            cmd = "3dWarp "
            cmd+= "-deoblique "
            cmd+= "-prefix {!s}{!s} {!s} {!s} ".format(
                o.p(),
                o.out_prefix(),
                deoblique_opt,
                e.input()
            )
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();

            if not(o.exist()) and not(self._parent.dry_run()) :
                ab.EP1("Could not deoblique epi data\n")
                return None
        else:
            self.exists_msg(o.input())

        return o


    # oblique anat to epi dataset
    # even if neither is really oblique, this shouldn't hurt.
    # if a pre-transformation matrix is defined, apply it here
    def oblique_anat2epi(self,a=None,e=None,oblique_opt="",suffix="_ob"):
        """Transform anatomy a to the geometry of EPI e.

        Use oblique_opt and suffix while preserving center alignment and
        pre-matrix handling.  Save the matrix for later composition and return
        the transformed afni_name, or None on a checked failure.
        """

        o = a.new("{!s}{!s}".format(a.out_prefix(), suffix))

        if(self.pre_matrix!=""):
            # use pre-shift transformation matrix instead
            self.obl_a2e_mat = self.pre_matrix
            warp_str = (
                "3dWarp -verb -matvec_in2out {!s} -prefix {!s}{!s} "
                "{!s} {!s} {!s} "
            ).format(
                self.pre_matrix,
                o.p(),
                o.out_prefix(),
                self.master_anat_option,
                oblique_opt,
                a.input()
            )
        else:
            if(self._parent.align_centers):
                # use shift transformation of centers between grids as initial
                # transformation. @Align_Centers (3drefit) instead of 3dWarp
                copy_cmd = "3dcopy {!s} {!s}{!s}".format(
                    a.input(),
                    o.p(),
                    o.out_prefix()
                )
                warp_str = (
                    "{!s}; @Align_Centers -base {!s} "
                    "-dset {!s} -no_cp"
                ).format(
                    copy_cmd,
                    e.input(),
                    o.input()
                )
            else:
                # get obliquity matrix from 3dWarp output and oblique anat to
                # epi
                # tempmat = "__tt_%s_obla2e_mat.1D" % a.out_prefix()
                self.obl_a2e_mat = "{!s}{!s}_obla2e_mat.1D".format(
                    a.p(),
                    a.out_prefix()
                )
                self.info_msg( "Matching obliquity of {!s} to {!s}".format(
                    self._parent.dset1_generic_name,
                    self._parent.dset2_generic_name
                ))

                warp_str = (
                    "3dWarp -verb -card2oblique {!s} -prefix "
                    "{!s}{!s} {!s} {!s} {!s}   | \\grep  -A 4 '# "
                    "mat44 Obliquity Transformation ::'  > {!s}"
                ).format(
                    e.input(),
                    o.p(),
                    o.out_prefix(),
                    self.master_anat_option,
                    oblique_opt,
                    a.input(),
                    self.obl_a2e_mat
                )

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            cmd = warp_str
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();

            if not(o.exist()) and not(self._parent.dry_run()) :
                ab.EP1("Could not oblique/shift anat to epi data\n")
                return None
        else:
            self.exists_msg(o.input())

        if(self._parent.align_centers):
            # align_centers saves the shift transformation, but
            # we use the opposite direction for obliquity, so invert
            # the shift transformation too and use that like the
            # obliquity matrix in later concatenations
            self.obl_a2e_mat = "{!s}{!s}_shft_I.1D".format(
                a.p(),
                a.out_prefix()
            )
            catcom = "cat_matvec {!s}{!s}_shft.1D -I > {!s}".format(
                o.p(),
                o.out_prefix(),
                self.obl_a2e_mat
            )
            cmd = catcom
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();
            if(self.master_anat_3dAl_center == 1):
                self.info_msg(
                    "Using align_centered {!s} as anatomical master".format(
                    o.prefix
                ))
                self.master_anat_3dAl_option = "-master {!s} {!s}".format(
                    o.input(),
                    self.master_anat_3dAl_option
                )
            if(self.master_epi_3dAl_center == 1):
                tempshft = e.new("{!s}_shft{!s}".format(
                    e.out_prefix(),
                    suffix
                ))

                copy_cmd = "3dcopy {!s} {!s}{!s}".format(
                    e.input(),
                    tempshft.p(),
                    tempshft.out_prefix()
                )
                warp_str = (
                    "{!s}; @Align_Centers -base {!s} "
                    "-dset {!s} -no_cp"
                ).format(
                    copy_cmd,
                    e.input(),
                    tempshft.input()
                )

                if (not tempshft.exist() or self._parent.rewrite
                    or self._parent.dry_run()):
                    tempshft.delete(self._parent.oexec)
                    cmd = warp_str
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run();
                    if (not tempshft.exist() and not self._parent.dry_run()):
                        ab.EP1("Could not oblique/shift anat to epi data\n")
                        return None
                else:
                    self.exists_msg(tempshft.input())
                self.info_msg("Using align_centered {!s} as epi master".format(
                    tempshft.prefix
                ))
                self.master_epi_option = "-master {!s} {!s}".format(
                    tempshft.input(),
                    self.master_epi_option
                )

        return o


    # do time shifting of EPI dataset
    def tshift_epi(  self, e=None, tshift_opt="-cubic", prefix="temp_tsh"):
        """Apply slice timing correction to EPI e with 3dTshift.

        Use tshift_opt and prefix.  Return the output afni_name, or retain e
        when the output is missing outside dry-run mode.
        """

        o = ab.afni_name(prefix)

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            self.info_msg( "Correcting for slice timing")
            cmd = "3dTshift "
            cmd+= "-prefix {!s} {!s} {!s} ".format(
                o.pp(),
                tshift_opt,
                e.input()
            )
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run();
            if (not o.exist() and not self._parent.dry_run()):
                ab.EP1("Could not do time shifting of epi data\n")
                return e
        else:
            self.exists_msg(o.input())

        return o



    # do volume registration of EPI dataset
    def register_epi(self, e=None, reg_opt="-quintic", prefix="temp_vr", \
                             motion_prefix = "temp_vr", childflag=0):
        """Volume-register EPI e using the configured registration method.

        reg_opt and prefix control dataset output; motion_prefix names motion
        parameters and matrices.  childflag selects the parent-derived reference
        for child runs.  Store this instance's registration matrix and return
        the output afni_name, or None on a checked failure.
        """

        o = ab.afni_name(prefix)

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            # save the volreg output to file names based on original epi name
            # (not temporary __tt_ names)

            # motion parameters output
            self.mot_1D = "{!s}_motion.1D".format(motion_prefix)
            # volreg transformation matrix
            self.reg_mat = "{!s}{!s}_mat.aff12.1D".format(
                o.p(),
                o.out_prefix()
            )
            self.info_msg( "Volume registration for {!s} data".format(
                self._parent.dset2_generic_name
            ))

            # user option for which registration program
            # (3dvolreg,3dWarpDrive,...)
            opt = self.input_options.get('-volreg_method')
            if opt != None:
                vrcom = opt.parlist[0]
            else:
                vrcom = '3dvolreg'

            if (vrcom == '3dWarpDrive'):
                vrcom = '3dWarpDrive -affine_general'
            if (vrcom == '3dAllineate'):
                vrcom = '3dAllineate -cost lpa -automask -source_automask'
            # find base for registration
            # could be: if number just use that as base
            # if((ps.volreg_base=='median') or (ps.volreg_base=='max')
            #   or (ps.volreg_base=='mean')):
            # choose a statistic as representative
            # if an integer, choose a single sub-brick
            if(childflag) :   # or (vrcom != "3dvolreg") :
                base = "{!s}'[{!s}]'".format(
                    self._parent.epi.input(),
                    self._parent.volreg_base
                )
            elif(self._parent.volreg_base.isdigit()):
                base = "{!s}".format(self._parent.volreg_base)
                if(vrcom != '3dvolreg'):
                    base = "{!s}'[{!s}]'".format(
                        self._parent.epi.input(),
                        self._parent.volreg_base
                    )

            # otherwise median, mean or max
            else:
                # if more than 1 sub-brick, compute stat, otherwise use 0th
                if 0:  # (dset_dims(e.ppve())[3] < 2 ):
                    self.info_msg(
                        "Not enough sub-bricks to compute {!s}".format(
                        self._parent.volreg_base
                    ))
                    base = "0"
                else:
                    ots = e.new("{!s}_ts_tempalpha".format(o.out_prefix()))
                    base = "{!s}'[0]'".format(ots.input())
                    if (not ots.exist() or self._parent.rewrite):
                        ots.delete(self._parent.oexec)

                    # compute stats, volreg, then recompute stats
                    cmd = "3dTstat "
                    cmd+= "-{!s} ".format(self._parent.volreg_base)
                    cmd+= "-prefix {!s}{!s} {!s}".format(
                        ots.p(),
                        ots.out_prefix(),
                        e.input()
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run()

                    if(not ots.exist() and not self._parent.dry_run()):
                        msg = "Could not create intermediate "
                        msg+= "datafor time series "
                        msg+= "registration"
                        ab.EP1(msg)
                        self._parent.ciao(1)

                    ovr_alpha = e.new("{!s}_vr_tempalpha".format(
                        o.out_prefix()
                    ))

                    if((vrcom != '3dvolreg') and (
                        self._parent.volreg_base.isdigit())):
                        base = "{!s}'[{!s}]'".format(
                            self._parent.epi.input(),
                            self._parent.volreg_base
                        )
                    cmd = "{!s} ".format(vrcom)
                    cmd+= "-prefix {!s}{!s} ".format(
                        ovr_alpha.p(),
                        ovr_alpha.out_prefix()
                    )
                    cmd+= "-base {!s} {!s} {!s} ".format(
                        base,
                        reg_opt,
                        e.input()
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run()

                    if stat :
                        ab.EP1("Failed at a 3dvolreg stage (I think)")
                        self.ciao(stat)

                    ots = e.new("{!s}_vrt".format(o.out_prefix()))
                    base = "{!s}'[0]'".format(ots.input())
                    if (not ots.exist() or self._parent.rewrite):
                        ots.delete(self._parent.oexec)

                        cmd = "3dTstat "
                        cmd+= "-{!s} ".format(self._parent.volreg_base)
                        cmd+= "-prefix {!s}{!s} {!s}".format(
                            ots.p(),
                            ots.out_prefix(),
                            ovr_alpha.input()
                        )
                        com = ab.shell_com(cmd, self._parent.oexec)
                        stat = com.run()

                    if(not ots.exist() and not self._parent.dry_run()):
                        msg = "Could not create intermediate "
                        msg+= "datafor time series "
                        msg+= "registration"
                        ab.EP1(msg)
                        self._parent.ciao(1)

            cmd = "{!s} ".format(vrcom)
            cmd+= "-1Dfile {!s} ".format(self.mot_1D)
            cmd+= "-1Dmatrix_save {!s} ".format(self.reg_mat)
            cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
            cmd+= "-base {!s} {!s} {!s} ".format(base, reg_opt, e.input())
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()

            if (not o.exist() and not self._parent.dry_run()):
                ab.EP1("Could not do volume registration")
                return None
        else:
            self.exists_msg(o.input())

        return o


    # resample EPI data to match higher resolution anatomical data
    def resample_epi(  self, e=None, resample_opt="", prefix="temp_rs", \
          subbrick=""):
        """Resample EPI e onto the anatomical grid with 3dresample.

        Use resample_opt, prefix and the optional subbrick selector.
        Return the output afni_name, or None on a checked failure.
        """

        o = ab.afni_name(prefix)
        o.view = self._parent.anat_ns.view

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            self.info_msg( "resampling {!s} to match {!s} data".format(
                self._parent.dset2_generic_name,
                self._parent.dset1_generic_name
            ))
            if (subbrick == ""):
                sb = ""
            else:
                if(subbrick.isdigit()):
                    sb = "[{!s}]".format(subbrick)
                else:
                    sb = "[0]"

            cmd = "3dresample "
            cmd+= "-master {!s} ".format(self._parent.anat_ns.ppv())
            cmd+= "-prefix {!s} ".format(o.pp())
            cmd+= "-inset {!s}'{!s}' ".format(e.input(), sb)
            cmd+= "-rmode Cu"
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()
            if (not o.exist() and not self._parent.dry_run()):
                ab.EP1("Could not resample\n")
                return None
        else:
            self.exists_msg(o.input())

        return o



    # remove skull or outside brain area
    def skullstrip_data(self, e=None, use_ss='3dSkullStrip', \
         skullstrip_opt="", prefix = "temp_ns"):
        """Remove nonbrain voxels from e with the requested use_ss method.

        Pass skullstrip_opt to 3dSkullStrip or 3dAutomask and write prefix.
        For other methods retain e.  Return the dataset or None on a checked
        output-creation failure.
        """

        self.info_msg( "removing skull or area outside brain")
        if (use_ss == '3dSkullStrip'):     #skullstrip epi
            n = ab.afni_name(prefix)
            n.view = e.view
            if (not n.exist() or self._parent.rewrite or self._parent.dry_run(
                )):
                n.delete(self._parent.oexec)
                cmd = "3dSkullStrip "
                cmd+= "-orig_vol {!s} ".format(skullstrip_opt)
                cmd+= "-input {!s} ".format(e.input())
                cmd+= "-prefix {!s}".format(n.pp())
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()
                if (not n.exist() and not self._parent.dry_run()):
                    ab.EP1("Could not strip skull\n")
                    return None
            else:
                self.exists_msg(n.input())
        elif use_ss == '3dAutomask': #Automask epi
            n = ab.afni_name(prefix)
            n.view = e.view
            j = ab.afni_name("{!s}__tt_am_{!s}".format(n.p(), n.pve()))

            if (not n.exist() or self._parent.rewrite or self._parent.dry_run(
                )):
                n.delete(self._parent.oexec)
                cmd = "3dAutomask {!s} ".format(skullstrip_opt)
                cmd+= "-apply_prefix {!s} {!s}".format(n.pp(), e.input())
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()
                if (not n.exist() and not self._parent.dry_run()):
                    ab.EP1("Could not strip skull with automask\n")
                    return None
                j.delete(self._parent.oexec)
            else:
                self.exists_msg(n.input())
        else:
            n = e;
        return n


    # create weighting volume for matching (upper 10% by default)
    def create_weight(self, e, sq=1.0, box='no', \
                        binit = 'no', perci = -1.0, \
                        fati = -1, suf="_wt"):
    #e is a preprocessed epi, no skull
    # box, bin and fat mask are not used
        """Build a percentile-based alignment weight from preprocessed EPI e.

        Use perci (90 when negative), power sq and output suffix suf.
        Retain the currently unused box, binit and fati parameters.
        Return the weight afni_name, or None on a checked failure.
        """

        a = self._parent.anat_ns

        o = ab.afni_name("{!s}{!s}{!s}{!s}".format(
            self._parent.output_dir,
            e.out_prefix(),
            suf,
            e.view
        ))
        if perci < 0:
            perci = 90.0;
        self.info_msg( "Computing weight mask")
        com_str = "3dBrickStat -automask -percentile {:f} 1 {:f} {!s}".format(
            perci,
            perci,
            e.input()
        )
        if(not self._parent.dry_run()):
            cmd = com_str
            com = ab.shell_com(cmd, self._parent.oexec, capture=1)
            stat = com.run()

            if stat :
                ab.EP1("Failed at a 3dBrickStat stage")
                self.ciao(stat)

            th = float(com.val(0,1))
            self.info_msg( "Applying threshold of {:f} on {!s}".format(
                th,
                e.input()
            ))
        else:
            cmd = com_str
            com = ab.shell_com(cmd, "dry_run")
            stat = com.run()
            th = -999
            self.info_msg( "Would be applying threshold for real run here")

        if sq == 1.0:
            cmd = "3dcalc "
            cmd+= "-datum float "
            cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
            cmd+= "-a {!s} ".format(e.input())
            cmd+= "-expr 'min(1,(a/{:f}))'".format(th)
            com = ab.shell_com(cmd, self._parent.oexec)
        else:
            cmd = "3dcalc "
            cmd+= "-datum float "
            cmd+= "-prefix {!s}{!s} ".format(o.p(), o.out_prefix())
            cmd+= "-a {!s} ".format(e.input())
            cmd+= "-expr 'min(1,(a/{:f}))**{:f}'".format(th, sq)
            com = ab.shell_com(cmd, self._parent.oexec)

        if (not o.exist() or self._parent.rewrite or self._parent.dry_run()):
            o.delete(self._parent.oexec)
            stat = com.run()
        else:
            self.exists_msg(o.input())

        if (not o.exist() and not self._parent.dry_run()):
            ab.EP1("Could not create weight dset\n")
            return None

        return o


    # Create double edge images for later viewing
    def add_edges(self, d1, d2, d3, d1name, d2name, d3name, listlog = ""):
        # specified examinelist for output name or use default in @AddEdge
        """Prepare the three supplied datasets and run @AddEdge for QC.

        d1, d2 and d3 are afni_name objects; their corresponding names select
        the copied prefixes.  An empty name uses that dataset's prefix.
        listlog optionally supplies the examination-list filename.
        """

        if (listlog == ""):
            aelistlog = ""
        else :
            aelistlog = "-examinelist {!s}".format(listlog)

        if (d1name==""):
            d1AE = ab.afni_name("{!s}{!s}".format(d1.prefix, d1.view))
        else:
            d1AE = ab.afni_name("{!s}{!s}".format(d1name, d1.view))

        if (d2name==""):
            d2AE = ab.afni_name("{!s}{!s}".format(d2.prefix, d2.view))
        else:
            d2AE = ab.afni_name("{!s}{!s}".format(d2name, d2.view))

        if (d3name==""):
            d3AE = ab.afni_name("{!s}{!s}".format(d3.prefix, d3.view))
        else:
            d3AE = ab.afni_name("{!s}{!s}".format(d3name, d3.view))

        cmd = "\\mkdir -p {}AddEdge".format(self._parent.output_dir)
        com = ab.shell_com(cmd, self._parent.oexec)
        stat = com.run()

        if stat :
            ab.EP1("Failed at a mkdir stage")
            self.ciao(stat)

        cmd = "3dcopy "
        cmd+= "-overwrite {!s} {!s}AddEdge/{!s}".format(
            d1.input(),
            self._parent.output_dir,
            d1AE.prefix
        )
        com = ab.shell_com(cmd, self._parent.oexec)
        stat = com.run()

        if stat :
            ab.EP1("Failed at a 3dcopy stage")
            self.ciao(stat)

        cmd = "3dcopy "
        cmd+= "-overwrite {!s} {!s}AddEdge/{!s}".format(
            d2.input(),
            self._parent.output_dir,
            d2AE.prefix
        )
        com = ab.shell_com(cmd, self._parent.oexec)
        stat = com.run()

        if stat :
            ab.EP1("Failed at a 3dcopy stage")
            self.ciao(stat)

        cmd = "3dcopy "
        cmd+= "-overwrite {!s} {!s}AddEdge/{!s}".format(
            d3.input(),
            self._parent.output_dir,
            d3AE.prefix
        )
        com = ab.shell_com(cmd, self._parent.oexec)
        stat = com.run()

        if stat :
            ab.EP1("Failed at a 3dcopy stage")
            self.ciao(stat)

        # changed .input to .shortinput           3 Feb 2012 [rickr,dglen]
        cmd = "cd {!s}AddEdge; @AddEdge ".format(self._parent.output_dir)
        cmd+= "-no_deoblique {!s} {!s} {!s} {!s}; cd - ".format(
            aelistlog,
            d1AE.shortinput(),
            d2AE.shortinput(),
            d3AE.shortinput()
        )
        com = ab.shell_com(cmd, self._parent.oexec)
        stat = com.run()

        if stat :
            ab.EP1("Failed at an AddEdge stage")
            self.ciao(stat)

    # do the preprocessing of the EPI data
    def process_epi(self, use_ss='3dSkullStrip', childflag=0):
        """Prepare this instance's EPI data in the existing processing order.

        Convert NIFTI when needed, time shift, volume-register, and, for the
        parent, select a representative volume, resample and strip/edge it.
        use_ss selects skull stripping.  With childflag, stop after registration.
        Return the timing, volume-registration and final preprocessing datasets;
        callers retain the original failure checks on those results.
        """

        basesuff = ""
        if self.epi_dir == '':
            basepath = self.epi.p()
        else:
            basepath = self.epi_dir

        if(self.epi.type == 'NIFTI'):
            #copy original epi to a temporary file
            o = ab.afni_name("{!s}__tt_{!s}{!s}".format(
                basepath,
                self.epi.out_prefix(),
                self.epi.view
            ))
            o.to_afni(new_view=au.dset_view(self.epi.ppve()))

            self.info_msg("Copying NIFTI EPI input to AFNI format")
            if (not o.exist() or self._parent.rewrite or self._parent.dry_run(
                )):
                cmd = "3dcopy {!s} {!s}{!s}".format(
                    self.epi.input(),
                    o.p(),
                    o.pv()
                )
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()

                if stat :
                    ab.EP1("Failed at a 3dcopy stage")
                    self.ciao(stat)

            basename = o.out_prefix()
            baseviewext = "{!s}{!s}".format(o.view, o.extension)
            basepathname = "{!s}{!s}".format(basepath, basename)
            tempbasepathname = basepathname
            #make filename output without .nii.gz
            self.epi_afniformat = ab.afni_name("{!s}{!s}{!s}".format(
                basepath,
                self.epi.out_prefix(),
                self.epi.view
            ))
            self.epi_afniformat.to_afni(new_view=au.dset_view(self.epi.ppve()))
        else:
            o = self.epi;
            basename = o.out_prefix()
            baseviewext = "{!s}{!s}".format(o.view, o.extension)
            basepathname = "{!s}{!s}".format(basepath, basename)
            tempbasepathname = "{!s}__tt_{!s}".format(basepath, basename)
            self.epi_afniformat = self.epi

        # time shift epi data, prepend a prefix
        if(self.tshift_flag):
            if(self.tshiftable_dset(o)) :
                basesuff = "{!s}_tsh".format(basesuff)
                if(self._parent.save_tsh):
                    prefix = "{!s}{!s}{!s}".format(
                        basepathname,
                        basesuff,
                        baseviewext
                    )
                else:
                    prefix = "{!s}{!s}{!s}".format(
                        tempbasepathname,
                        basesuff,
                        baseviewext
                    )
                o = self.tshift_epi( o, self._parent.tshift_opt, prefix=prefix)
            else:
                self.info_msg("Can not do time shifting of slices. "
                              "Data is already time shifted")
        # if timeshifting was done, this will be the final epi dataset
        # and then the concatenated volreg, 3dAllineate transformations will be
        # applied
        tshift_o = o
        # do volume registration
        if(self.volreg_flag):
            if(self._parent.dry_run() or \
             (not self._parent.dry_run() and (ab.dset_dims(o.input())[
                3] > 1))) :
                basesuff = "{!s}_vr".format(basesuff)

                if(self._parent.save_vr):
                    prefix = "{!s}{!s}{!s}{!s}".format(
                        basepath,
                        self.epi_afniformat.out_prefix(),
                        basesuff,
                        baseviewext
                    )

                else:
                    prefix = "{!s}{!s}{!s}".format(
                        tempbasepathname,
                        basesuff,
                        baseviewext
                    )
                # if aligning epi to anat or saving volreg output, save motion
                # parameters
                motion_prefix = "{!s}{!s}{!s}".format(
                    basepath,
                    self.epi_afniformat.out_prefix(),
                    basesuff
                )
                o = self.register_epi( o, self._parent.reg_opt, prefix,
                    motion_prefix,
                                        childflag=childflag)
            else:
                self.info_msg("Skipping time series volume registration. "
                              "Must have more than a single sub-brick.")
                self.reg_mat = "" # children can be skipped too

        volreg_o = o

        # if just processing child epi datasets, just go home
        #   and skip reduction, resampling, skullstripping
        if(childflag):
            return tshift_o, volreg_o, volreg_o

        # reduce epi to a single representative sub-brick
        basesuff = "{!s}_ts".format(basesuff)
        if(self._parent.save_rep):
            prefix = "{!s}{!s}{!s}{!s}".format(
                basepath,
                basename,
                basesuff,
                baseviewext
            )
        else:
            prefix = "{!s}{!s}{!s}".format(
                tempbasepathname,
                basesuff,
                baseviewext
            )

        o = self.tstat_epi(o, self._parent.tstat_opt, prefix)

        # resample epi to match anat
        if(self.resample_flag) :
            basesuff = "{!s}_rs".format(basesuff)
            if(self._parent.save_resample):
                prefix = "{!s}{!s}{!s}{!s}".format(
                    basepath,
                    basename,
                    basesuff,
                    baseviewext
                )
            else:
                prefix = "{!s}{!s}{!s}".format(
                    tempbasepathname,
                    basesuff,
                    baseviewext
                )

            e = self.resample_epi( o,"", prefix)
        else:
            e = o  # no need to resample

        # remove outside brain or skull
        basesuff = "{!s}_ns".format(basesuff)
        if(self.save_epi_ns) :
            prefix = "{!s}{!s}{!s}".format(
                basepathname,
                basesuff,
                baseviewext
            )
        else:
            prefix = "{!s}{!s}{!s}".format(
                tempbasepathname,
                basesuff,
                baseviewext
            )

        skullstrip_o = self.skullstrip_data( e, use_ss,
            self._parent.epistrip_opt, prefix)

        # use edges to align optionally
        if(self._parent.edge) :
            basesuff = "{!s}_edge".format(basesuff)
            if(self._parent.save_rep):
                prefix = "{!s}{!s}{!s}".format(
                    basepathname,
                    basesuff,
                    baseviewext
                )
            else:
                prefix = "{!s}{!s}{!s}".format(
                    tempbasepathname,
                    basesuff,
                    baseviewext
                )

            skullstrip_o = self.edge_dset(skullstrip_o, prefix, binarize=0,
                erodelevel=self._parent.erodelevel)

        return  tshift_o, volreg_o, skullstrip_o


    # do the preprocessing of the anatomical data
    def process_anat(self):
        """Prepare anatomy and record datasets required for final transforms.

        Copy/convert the input, optionally skullstrip, match obliquity/centers
        and compute edges.  Preserve the original command and existence checks.
        Return 1 on success, None for a checked skullstrip failure, or terminate
        through ciao() for a checked copy failure.
        """

        if self.anat_dir == '':
            basepath = self.anat0.p()
        else:
            basepath = self.anat_dir

        self.anat0_master = self.anat0

        #copy original anat to a temporary file
        if(not self._parent.flip):
            self.anat = ab.afni_name("{!s}__tt_{!s}{!s}".format(
                basepath,
                self.anat0.out_prefix(),
                self.anat0.view
            ))
        else:
            self.anat = ab.afni_name("{!s}{!s}_unflipped{!s}".format(
                basepath,
                self.anat0.out_prefix(),
                self.anat0.view
            ))

        self.anat.to_afni(new_view=au.dset_view(self.anat.ppve()))

        if (not self.anat.exist() or self._parent.rewrite
            or self._parent.dry_run()):
            cmd = "3dcopy {!s} {!s}{!s}".format(
                self.anat0.input(),
                self.anat.p(),
                self.anat.pv()
            )
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()
            if (not self.anat.exist() and not self._parent.dry_run()):
                msg = "Could not copy anat ({:d})".format(self.anat.exist())
                ab.EP1(msg)
                self._parent.ciao(1)
        else:
            self.exists_msg(self.anat.input())

        # now that we have the data in AFNI format, use the AFNI format copy
        # names
        self.anat0 = ab.afni_name("{!s}{!s}{!s}".format(
            basepath,
            self.anat0.out_prefix(),
            self.anat0.view
        ))

        self.anat0.to_afni(new_view=au.dset_view(self.anat.ppve()))

        a = self.anat;

        #do we need to strip ?
        if(self._parent.skullstrip):
            #don't use same type of input.
            n = ab.afni_name("{!s}{!s}_ns{!s}".format(
                a.p(),
                a.out_prefix(),
                self.anat.view
            ))
            if not(n.exist()) or self._parent.rewrite or \
               self._parent.dry_run() :
                n.delete(self._parent.oexec)
                self.info_msg( "Removing skull from {!s} data".format(
                    self._parent.dset1_generic_name
                ))

                if(self._parent.skullstrip_method=="3dSkullStrip"):
                    cmd = "{!s} ".format(self._parent.skullstrip_method)
                    cmd+= "-orig_vol {!s} ".format(self._parent.skullstrip_opt)
                    cmd+= "-input {!s} ".format(a.input())
                    cmd+= "-prefix {!s}{!s}".format(n.p(), n.out_prefix())
                    com = ab.shell_com(cmd, self._parent.oexec)
                else:
                    cmd = "{!s} {!s} ".format(
                        self._parent.skullstrip_method,
                        self._parent.skullstrip_opt
                    )
                    cmd+= "-apply_prefix {!s}{!s} {!s}".format(
                        n.p(),
                        n.out_prefix(),
                        a.input()
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)

                stat = com.run()
                if (not n.exist() and not self._parent.dry_run()):
                    ab.EP1("Could not strip skull\n")
                    return None
            else:
                self.exists_msg(n.input()) # not: o.input()
        else:
            n = a

        # pre-obliquing or edging skullstripped anat
        self._parent.anat_ns0 = n

        # match obliquity of anat to epi data
        if(self.deoblique_flag or self._parent.align_centers):
            # if either anat or epi is oblique or
            #   there is a pre-transformation matrix,
            #   move anat to match epi
            if((self.oblique_dset(n)) or (self.oblique_dset(self._parent.epi)) \
                 or (self.pre_matrix!="") or self._parent.align_centers) :
                # set default output spacing if not already set with user
                # options
                opt = self.input_options.get('-master_anat')
                if opt == None:
                    min_d =  self.min_dim_dset(self._parent.anat_ns0)
                    self.master_3dAl_option = "-mast_dxyz {:f}".format(min_d)
                    self.info_msg(
                      "Spacing for {!s} to oblique {!s} alignment is "
                        "{:f}".format(
                        self.dset1_generic_name,
                        self.dset2_generic_name,
                        min_d
                    ))

                n = self.oblique_anat2epi(n, self._parent.epi,
                    self._parent.deoblique_opt)

        # use edges to match optionally
        if(self.edge):
            prefix = "{!s}{!s}_edge{!s}".format(a.p(), a.out_prefix(), a.view)
            n = self.edge_dset(n, prefix,binarize=0,
                erodelevel=self._parent.erodelevel)

        #do we need to shift?
        #      optc = self.input_options.get('-align_centers')
        #      if optc != None and optc.parlist[0] == 'yes':

        #         com = shell_com(  \
        #               "@Align_Centers -cm -base %s -dset %s" \
        #               % (ps.epi.ppve(), n.ppve() ) , ps.oexec)
        #         stat = com.run()
        #         a_shft = n.new("%s_shft" % n.out_prefix(),"+orig")
        #         if (not a_shft.exist() and not ps.dry_run()):
        # print "** ERROR: Could not shift anat (%d)" % a_shft.exist()
        #            return None
        #         ps.anat_ns = a_shft
        #      else:
        self._parent.anat_ns = n;
        return 1


    # create final output files (non-temporary names)
    def create_output(self, aae, w, eaa, eaat, suf, epi_in=None, anat_in=None):

        #Create a properly named version of anatomy aligned to  EPI
        """Create the requested permanent outputs and record dataset history.

        aae, w, eaa and eaat are aligned anatomy, weight, EPI and template EPI
        outputs.  suf controls naming; epi_in and anat_in are optional saved
        alignment inputs.  Honor the existing save flags and copy behavior.
        """

        opt = self._parent.input_options.get('-anat')
        if opt == None :
            opt = self.input_options.get('-dset1')
        ain = ab.afni_name(opt.parlist[0])
        opt = self._parent.input_options.get('-epi')
        if opt == None:
            opt = self._parent.input_options.get('-dset2')

        ein = ab.afni_name(opt.parlist[0])

        # save skull stripped anat before alignment
        # Yikes! Rick noticed the saved skullstrip was the obliqued one
        # and not the original. Should be original ps.anat_ns0, not ps.anat_ns
        if(self._parent.skullstrip and self._parent.save_skullstrip):
            ao_ns = ab.afni_name("{!s}{!s}_ns{!s}".format(
                self._parent.anat_ns0.p(),
                ain.prefix,
                ain.view
            ))
            self.copy_dset( self._parent.anat_ns0, ao_ns,
             "Creating final output: skullstripped {!s} data".format(
                self.dset1_generic_name
            ), self._parent.oexec)

        # maybe save pre-obliqued skull stripped anat before alignment
        # 3 Aug 2011 [rickr]
        if(self._parent.skullstrip and self._parent.save_origstrip):
            ao_ons = ab.afni_name("{!s}{!s}{!s}{!s}".format(
                self._parent.anat_ns0.p(),
                ain.prefix,
                self._parent.save_origstrip,
                ain.view
            ))
            self.copy_dset( self._parent.anat_ns0, ao_ons,
             "Creating final output: skullstripped original {!s} data".format(
                self.dset1_generic_name
            ), self._parent.oexec)

        # save anatomy aligned to epi
        if (aae):
            # save aligned anatomy
            o = ab.afni_name("{!s}{!s}{!s}{!s}".format(
                aae.p(),
                ain.prefix,
                suf,
                aae.view
            ))
            o.view = aae.view
            self.copy_dset( aae, o,
             "Creating final output: {!s} data aligned to {!s}".format(
                self.dset1_generic_name,
                self.dset2_generic_name
            ) , self._parent.oexec)
            self.save_history(o,self._parent.oexec)

            if (self._parent.save_tsh):
                # save the timeshifted EPI data
                if (self.epi_ts and self.tshift_flag) :
                    eo = ab.afni_name("{!s}{!s}_tshft{!s}".format(
                        self.epi_ts.p(),
                        ein.prefix,
                        self.epi_ts.view
                    ))
                    self.copy_dset( self.epi_ts, eo,
                     "Creating final output: time-shifted {!s}".format(
                        self.dset2_generic_name
                    ), self._parent.oexec)

            # save the volume registered EPI data
            if (self._parent.save_vr):
                if (self.epi_vr and self.volreg_flag):
                    eo = ab.afni_name("{!s}{!s}_vr{!s}".format(
                        self.epi_vr.p(),
                        ein.prefix,
                        self.epi_vr.view
                    ))
                    self.copy_dset( self.epi_vr, eo,
                      (
                        "Creating final output: time series "
                        "volume-registered {!s}"
                    ).format(
                        self.dset2_generic_name
                    ), self._parent.oexec)

        # save Allineate input datasets
        if(self._parent.save_Al_in):
            # save weight used in 3dAllineate
            if w:
                ow = ab.afni_name("{!s}{!s}_wt_in_3dAl{!s}{!s}".format(
                    w.p(),
                    ein.prefix,
                    suf,
                    w.view
                ))
                self.copy_dset( w, ow,
                 "Creating final output: weighting data", self._parent.oexec)

            #save a version of the epi as it went into 3dAllineate
            if epi_in:
                eo = ab.afni_name("{!s}{!s}_epi_in_3dAl{!s}{!s}".format(
                    epi_in.p(),
                    ein.prefix,
                    suf,
                    epi_in.view
                ))
                self.copy_dset( epi_in, eo,     \
                 (
                    "Creating final output: {!s} representative data "
                    "as used by 3dAllineate"
                ).format(
                    self.dset2_generic_name
                ), self._parent.oexec)

            #save a version of the anat as it went into 3dAllineate
            if anat_in:
                ao = ab.afni_name("{!s}{!s}_anat_in_3dAl{!s}{!s}".format(
                    self._parent.anat_ns0.p(),
                    ain.prefix,
                    suf,
                    anat_in.view
                ))
                self.copy_dset( anat_in, ao,
                (
                    "Creating final output: {!s} data as used by "
                    "3dAllineate"
                ).format(
                    self.dset1_generic_name
                ), self._parent.oexec)

        #Now create a version of the epi that is aligned to the anatomy
        if (eaa):
            #save the epi aligned to anat
            o = ab.afni_name("{!s}{!s}{!s}{!s}".format(
                eaa.p(),
                ein.prefix,
                suf,
                eaa.view
            ))
            self.copy_dset( eaa, o,
             "Creating final output: {!s} data aligned to {!s}".format(
                self.dset2_generic_name,
                self.dset1_generic_name
            ), self._parent.oexec)
            self.save_history(o,self._parent.oexec)
        # And a version of the epi that is aligned to the anatomy in standard
        # space
        if (eaat):
            #save the epi aligned to anat in standard space
            #put note in header with command line
            self.save_history(eaat,self._parent.oexec)

        return


    # remove all the temporary files from a previous run
    # remove old results too optionally
    def fresh_start(self, epref="", apref="", rmold = 0, epipath="",
        anatpath="" ):
        """Issue the legacy removal commands for matching temporary files.

        epref/apref select EPI/anatomy prefixes and epipath/anatpath their
        directories.  rmold also removes matching previous aligned results.
        Commands follow the current execution mode, including dry-run.
        """

        self.info_msg("Removing all the temporary files")
        epref = epref.strip()
        apref = apref.strip()
        if epref == "" and apref == "":
            cmd = "\\rm -f {!s}__tt_*".format(self._parent.output_dir)
            com = ab.shell_com(cmd, self._parent.oexec)
            stat = com.run()
        else:
            if epref != "":
                cmd = "\\rm -f {!s}__tt_{!s}*".format(epipath, epref)
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()
                if(rmold):
                    cmd = "\\rm -f {!s}{!s}*{!s}*".format(
                        epipath,
                        epref,
                        self._parent.suffix
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run()

            if apref != "":
                cmd = "\\rm -f {!s}__tt_{!s}*".format(anatpath, apref)
                com = ab.shell_com(cmd, self._parent.oexec)
                stat = com.run()
                if(rmold):
                    cmd = "\\rm "
                    cmd+= "-f {!s}{!s}*{!s}*".format(
                        anatpath,
                        apref,
                        self._parent.suffix
                    )
                    com = ab.shell_com(cmd, self._parent.oexec)
                    stat = com.run()

        return


    # process children EPI as for parent but
    #   no alignment of anat to EPI and no representative EPI
    # Do time shifting, time series volume registration
    def process_child_epi(self, childepi, suff) :
        # do preprocessing of epi
        # do time shifting, volume registration of child epi data
        #   if requested; otherwise, just keep the input epi
        """Prepare childepi and optionally apply the parent's alignment.

        Skip the parent input itself.  Shallow-copy processing state for the
        child while keeping an explicit reference to the parent, preserving
        the original distinction between global parent and instance values.
        Retain the existing preprocessing and alignment failure behavior.

        Compared to original AEA program, this one adds suff as an
        additional arg, so that when called, the child suffixes would
        be distinct when called with different mcost values in the
        original, calling function (otherwise, they are either
        overwritten or skipped)

        """

        # skip the parent if it's included
        if(childepi.input()==self._parent.epi.input()) :
            return                              #   in child list
        child = copy.copy(self._parent)

        child.epi = childepi

        self.info_msg("Parent {!s}:  Child: {!s}".format(
            self._parent.epi.input(),
            child.epi.input()
        ))

        child.epi_ts, child.epi_vr, child.epi_ns = \
             child.process_epi(childflag=1)
        if (not child.epi_ns):
            child.ciao(1)

        e = child.epi_ns
        a = self._parent.anat0_master       # use parent anat

        if(self._parent.prep_only):  # if preprocessing only, exit now
            return

        # does the user want the epi aligned to the anat?
        if (self._parent.epi2anat) :
            # compute transformation just from applying inverse (NB:
            # no longer use suf=self._parent.suffix, but instead the
            # new arg suf=suff)
            child.epi_alnd = \
                child.align_epi2anat(child.epi_ts, a, self._parent.AlOpt,
                suf=suff)

            if (not child.epi_alnd):
                self._parent.ciao(1)

        else:
            child.epi_alnd = ''

