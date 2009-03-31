#!/usr/bin/env python

# system libraries
import sys
import gc, math

# AFNI libraries
import option_list as OL
import lib_timing as LT
import afni_util as UTIL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
timing_tool.py    - for manipulating and evaluating stimulus timing files
                    (-stim_times format: where each row is a separate run)

   This program is meant to work with ascii files containing rows of floats
   ('*' characters are ignored).  This is the format used by 3dDeconvolve
   with the -stim_times option.  Some timing files do not need evaluation,
   such as those where the timing is very consistent.  However, it may be
   important to examine those files from a random timing design.

   Recall that an ISI (inter-stimulus interval) is the interval of time
   between the end of one stimulus and start of the next.

   The basic program operations include:

       o reporting ISI statistics, such as min/mean/max values per run
       o reporting overall ISI statistics for a set of timing files
       o adding a constant offset to time
       o combining multiple timing files into 1 (like '1dcat' + sort)
       o appending additional timing runs (like 'cat')
       o sort times per row (though 3dDeconvolve does not require this)

   A sample stimulus timing file having 3 runs with 4 stimuli per run
   might look something like the following.  Note that the file does not
   imply the durations of the stimuli, except that stimuli are generally
   not allowed to overlap.

      17.3 24.0 66.0 71.6
      11.0 30.6 49.2 68.5
      19.4 28.7 53.8 69.4

   The program works on either a single timing element (which can be modified),
   or a list of them (which cannot be modified).  The only real use of a list
   of timing elements is to show statistics (via -multi_show_isi_stats).

--------------------------------------------------------------------------
examples:

   0. Basic commands:

         timing_tool.py -help
         timing_tool.py -hist
         timing_tool.py -show_valid_opts
         timing_tool.py -ver

   1. Combine the timing of 2 files (extend one timing by another and sort).
      Write to a new timing file.

         timing_tool.py -timing stimesB_01_houses.1D         \\
                        -extend stimesB_02_faces.1D          \\
                        -sort                                \\
                        -write_timing stimesB_extended.1D

   2. Subtract 12 seconds from each stimulus time (to offset TRs dropped
      prior to the magnetization steady state).

         timing_tool.py -timing stimesB_01_houses.1D         \\
                        -add_offset -12.0                    \\
                        -write_timing stimesB1_offset12.1D

   3. Show timing statistics for the 3 timing files generated by example 3
      from "make_random_timing -help".  To be accurate, specify the run
      and stimulus durations.

         timing_tool.py -multi_timing stimesC_*.1D           \\
                        -run_len 200 -multi_stim_dur 3.5     \\
                        -multi_show_isi_stats

   4. Show timing statistics for the timing files generated by example 6
      from "make_random_timing -help".  Since both the run and stimulus
      durations vary, 4 run lengths and 3 stimulus durations are given.

         timing_tool.py -multi_timing stimesF_*.1D           \\
                        -run_len 200 190 185 225             \\
                        -multi_stim_dur 3.5 4.5 3            \\
                        -multi_show_isi_stats

--------------------------------------------------------------------------
Notes:

   1. Action options are performed in the order of the options.  If
      the -chrono option is given, everything (but -chrono) is.

   2. Either -timing or -multi_timing is required for processing.

   3. Option -run_len applies to single or multiple stimulus classes.

--------------------------------------------------------------------------
basic informational options:

   -help                        : show this help
   -hist                        : show the module history
   -show_valid_opts             : show all valid options
   -ver                         : show the version number

------------------------------------------
single/multiple timing options:

   -timing TIMING_FILE          : specify a stimulus timing file to load

        e.g. -timing stimesB_01_houses.1D

        Use this option to specify a single stimulus timing file.  The user
        can modify this timing via some of the action options listed below.

   -show_isi_stats              : display timing and ISI statistics

        With this option, the program will display timing statistics for the
        single (possibly modified) timing element.

   -show_timing_ele             : display info on the main timing element

        With this option, the program will display information regarding the
        single (possibly modified) timing element.

   -stim_dur DURATION           : specify the stimulus duration, in seconds

        e.g. -stim_dur 3.5

        This option allows the user to specify the duration of the stimulus,
        as applies to the single timing element.  The only use of this is
        in conjunction with -show_isi_stats.

            Consider '-show_isi_stats' and '-run_len'.

   --------------------
        
   -multi_timing FILE1 FILE2 ... : specify multiple timing files to load

        e.g. -timing stimesB_*.1D

        Use this option to specify a list of stimulus timing files.  The user
        cannot modify this data, but can display the overall ISI statistics
        from it.

        Options that pertain to this timing list include:

            -multi_show_isi_stats
            -multi_show_timing_ele
            -multi_stim_dur
            -run_len

   -multi_show_isi_stats        : display timing and ISI statistics

        With this option, the program will display timing statistics for the
        multiple timing files.

   -multi_show_timing_ele       : display info on the multiple timing elements

        With this option, the program will display information regarding the
        multiple timing element list.

   -multi_stim_dur DUR1 ...     : specify the stimulus duration(s), in seconds

        e.g. -multi_stim_dur 3.5
        e.g. -multi_stim_dur 3.5 4.5 3

        This option allows the user to specify the durations of the stimulus
        classes, as applies to the multiple timing elements.  The only use of
        this is in conjunction with -multi_show_isi_stats.

        If only one duration is specified, it is applied to all elements.
        Otherwise, there should be as many stimulus durations as files
        specified with -multi_timing.

            Consider '-multi_show_isi_stats' and '-run_len'.

------------------------------------------
action options (apply to single timing element, only):

   ** Note that these options are processed in the order they are read.
      See '-chrono' for similar notions.

   -add_offset OFFSET           : add OFFSET to every time in main element

        e.g. -add_offset -12.0

        Use this option to add a single offset to all of the times in the main
        timing element.  For example, if the user deletes 3 4-second TRs from
        the EPI data, they may wish to subtract 12 seconds from every stimulus
        time, so that the times match the modifed EPI data.

            Consider '-write_timing'.

   -add_rows NEW_FILE           : append these timing rows to main element

        e.g. -add_rows more_times.1D

        Use this option to append rows from NEW_FILE to those of the main
        timing element.  If the user then wrote out the result, it would be
        identical to using cat: "cat times1.txt times2.txt > both_times.txt".

            Consider '-write_timing'.

   -extend NEW_FILE             : extend the timing rows with those in NEW_FILE

        e.g. -extend more_times.1D

        Use this option to extend each row (run) with the times in NEW_FILE.
        This has an effect similar to that of '1dcat'.  Sorting the times is
        optional, done via '-sort'.  Note that 3dDeconvolve does not need the
        times to be sorted, though it is more understandable to the user.

            Consider '-sort' and '-write_timing'.

   -show_timing                 : display the current single timing data

        This prints the current (possibly modified) single timing data to the
        terminal.  If the user is making multiple modifications to the timing
        data, they may wish to display the updated timing after each step.

   -sort                        : sort the times, per row (run)

        This will cause each row (run) of the main timing element to be
        sorted (from smallest to largest).  Such a step may be highly desired
        after using '-extend', or after some external manipulation that causes
        the times to be unsorted.

        Note that 3dDeconvolve does not require sorted timing.

            Consider '-write_timing'.

   -transpose                   : transpose the data (only if rectangular)

        This works exactly like 1dtranspose, and requires each row to have
        the same number of entries (rectangular data).  The first row would
        be swapped with the first column, etc.

            Consider '-write_timing'.

   -write_timing NEW_FILE       : write the current timing to a new file

        e.g. -write_timing new_times.1D

        After modifying the timing data, the user will probably want to write
        out the result.  Alternatively, the user could use -show_timing and
        cut-and-paste to write such a file.

------------------------------------------
general options:

   -chrono                      : process options chronologically

        While the action options are already processed in order, general and
        -timing options are not, unless the chrono option is given.  This 
        allows one to do things like scripting a sequence of operations
        within a single command.

   -nplaces NPLACES             : specify # decimal places used in printing

        e.g. -nplaces 1

        This option allows the user to specify the number of places to the
        right of the decimal that are used when printing a stimulus time
        (to the screen via -show_timing or to a file via -write_timing).
        The default is 3.

            Consider '-show_timing' and '-write_timing'.

   -run_len RUN_TIME ...        : specify the run duration(s), in seconds

        e.g. -run_len 300
        e.g. -run_len 300 320 280 300

        This option allows the user to specify the duration of each run.
        If only one duration is provided, it is assumed that all runs are of
        that length of time.  Otherwise, the user must specify the same number
        of runs that are found in the timing files (one run per row).

        This option applies to both -timing and -multi_timing files.

        The run durations only matter for displaying ISI statistics.

            Consider '-show_isi_stats' and '-multi_show_isi_stats'.

   -verb LEVEL                  : set the verbosity level

        e.g. -verb 3

        This option allows the user to specify how verbose the program is.
        The default level is 1, 0 is quiet, and the maximum is (currently) 4.

-----------------------------------------------------------------------------
R Reynolds    December 2008
=============================================================================
"""

g_history = """
   timing_tool.py history:

   1.0  Dec 01, 2008 - initial/release version
"""

g_version = "timing_tool.py version 1.0, December 1, 2008"


class ATInterface:
   """interface class for AfniTiming"""
   def __init__(self, verb=1):
      # main variables
      self.status          = 0                       # exit value
      self.valid_opts      = None
      self.user_opts       = None

      # user options
      self.chrono          = 0          # are options processed chronologically
      self.nplaces         = 3          # num decimal places for writing
      self.run_len         = [0]        # time per run (for single/multi)
      self.verb            = verb

      # user options - single var
      self.timing          = None       # main timing element
      self.fname           = 'no file selected'
      self.stim_dur        = 0 

      # user options - multi var
      self.m_timing        = []
      self.m_fnames        = []
      self.m_stim_dur      = [] 

      # initialize valid_opts
      self.init_options()

   def set_timing(self, fname):
      """load a timing file, and init the main class elements"""

      self.status = 1 # init to failure
      timing = LT.AfniTiming(fname, dur=self.stim_dur, verb=self.verb)
      if not timing.ready:
         print "** failed to read timing from '%s'" % fname
         return 1

      # success, so nuke and replace the old stuff

      if self.timing:
         if self.verb > 0:
            print "-- replacing old timing with that from '%s'" % fname
         del(self.timing)
         del(self.fname)
         self.timing = None
         self.fname = None

      elif self.verb > 1: print "++ read timing from file '%s'" % fname

      self.timing = timing
      self.fname = fname
      self.status = 0

      return 0

   def multi_set_timing(self, flist):
      """load multiple timing files"""

      if type(flist) != type([]):
         print '** multi_set_timing: list of files required'
         return 1

      if len(flist) < 1: return 0

      sdl = len(self.m_stim_dur)
      if sdl == 0:
         sdurs = [0 for ind in range(len(flist))]
      elif sdl > 1 and sdl != len(flist):
         print '** length of stim times does not match # files (%d, %d)' % \
               (sdl, len(flist))
         # set all durations to 0
         sdurs = [0 for ind in range(len(flist))]
      elif sdl == 1:
         # duplicate duration for all stimuli
         sdurs = [self.m_stim_dur[0] for ind in range(len(flist))]
      else:
         # sdl > 1 and lengths are equal: so use what we have
         sdurs = self.m_stim_dur

      rdlist = []
      for ind in range(len(flist)):
         name = flist[ind]
         timing = LT.AfniTiming(name, dur=sdurs[ind], verb=self.verb)
         if not timing.ready:
            print "** (multi) failed to read timing from '%s'" % name
            return 1
         rdlist.append(timing)

      # success, so nuke and replace the old stuff

      if self.m_timing:
         if self.verb > 0:
            print "-- replacing multi timing from %d files" % len(flist)
         del(self.m_timing)
         del(self.m_fnames)
         self.m_timing = []
         self.m_fnames = []

      elif self.verb > 1: print "++ read timing from %d files" % len(flist)

      self.m_timing = rdlist
      self.m_fnames = flist

      return 0

   def set_stim_dur(self, dur):
      """apply the stim duration to the timing element"""

      if type(dur) != type(3.14):
         print "** set_stim_dur: float required, have '%s'" % type(dur)
         return 1

      self.stim_dur = dur
      if self.timing: self.timing.dur = dur

      if self.verb > 2: print '++ applying stim dur: %f' % dur

   def multi_set_stim_durs(self, durs):
      """apply the stim durations to any multi-timing list"""

      if type(durs) != type([]):
         print '** multi_set_stim_durs: list of durations required'
         return 1

      sdl = len(durs)
      stl = len(self.m_timing)

      # if we have no timing elements, just save the list
      if stl < 1:
         self.m_stim_dur = durs
         return 0

      # now be sure durs matches the list length
      if sdl < 1: return 0
      elif sdl == 1:
         # duplicate duration for all stimuli
         sdurs = [durs[0] for ind in range(stl)]
      elif sdl != stl:
         print '** length of durs list does not match # elements (%d, %d)' % \
               (sdl, stl)
         return 1
      else:
         sdurs = durs

      # now store the list
      self.m_stim_dur = sdurs

      if self.verb > 2: print '++ applying multi stim durs: %s' % sdurs

      for ind in range(stl):
         self.m_timing[ind].dur = sdurs[ind]

   def show_multi(self):
      print '==================== multi-timing list ====================\n' 
      for rd in self.m_timing: rd.show()

   def write_timing(self, fname):
      """write the current timing out, with nplaces right of the decimal"""
      if not self.timing:
         print '** no timing to write'
         return 1
      return self.timing.write_times(fname, nplaces=self.nplaces)

   def init_options(self):
      self.valid_opts = OL.OptionList('valid opts')

      # short, terminal arguments
      self.valid_opts.add_opt('-help', 0, [],           \
                         helpstr='display program help')
      self.valid_opts.add_opt('-hist', 0, [],           \
                         helpstr='display the modification history')
      self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                         helpstr='display all valid options')
      self.valid_opts.add_opt('-ver', 0, [],            \
                         helpstr='display the current version number')

      # action options - single data
      self.valid_opts.add_opt('-add_offset', 1, [], 
                         helpstr='offset all data by the given value')

      self.valid_opts.add_opt('-add_rows', 1, [], 
                         helpstr='append the rows (runs) from the given file')

      self.valid_opts.add_opt('-extend', 1, [], 
                         helpstr='extend the rows lengths from the given file')

      self.valid_opts.add_opt('-show_timing', 0, [], 
                         helpstr='display timing contents')

      self.valid_opts.add_opt('-sort', 0, [], 
                         helpstr='sort the data, per row')

      self.valid_opts.add_opt('-transpose', 0, [], 
                         helpstr='transpose timing data (must be rectangular)')

      self.valid_opts.add_opt('-write_timing', 1, [], 
                         helpstr='write timing contents to the given file')

      # (ending with matches for multi)
      self.valid_opts.add_opt('-timing', 1, [], 
                         helpstr='load the given timing file')
      self.valid_opts.add_opt('-show_isi_stats', 0, [], 
                         helpstr='show ISI stats for the main timing object')
      self.valid_opts.add_opt('-show_timing_ele', 0, [], 
                         helpstr='display info about the main timing element')
      self.valid_opts.add_opt('-stim_dur', 1, [], 
                         helpstr='provide a stimulus duration for main timing')

      # action options - multi
      self.valid_opts.add_opt('-multi_timing', -1, [], 
                         helpstr='load the given list of timing files')
      self.valid_opts.add_opt('-multi_show_isi_stats', 0, [], 
                         helpstr='show ISI stats for load_multi_timing objs')
      self.valid_opts.add_opt('-multi_show_timing_ele', 0, [], 
                         helpstr='display info about the multi timing elements')
      self.valid_opts.add_opt('-multi_stim_dur', -1, [], 
                         helpstr='provide stimulus durations for timing list')

      # general options (including multi)
      self.valid_opts.add_opt('-chrono', 0, [], 
                         helpstr='process options chronologically')
      self.valid_opts.add_opt('-nplaces', 1, [], 
                         helpstr='set number of decimal places for printing')
      self.valid_opts.add_opt('-run_len', -1, [], 
                         helpstr='specify the lengths of each run (in seconds)')
      self.valid_opts.add_opt('-verb', 1, [], 
                         helpstr='set the verbose level (default is 1)')

      return 0

   def process_options(self):

      # process terminal options without the option_list interface

      if len(sys.argv) <= 1 or '-help' in sys.argv:
         print g_help_string
         return 0

      if '-hist' in sys.argv:
         print g_history
         return 0

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 0

      if '-ver' in sys.argv:
         print g_version
         return 0

      # ============================================================
      # read options specified by the user
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # convenience variable
      if not uopts: return 1            # error condition

      # ------------------------------------------------------------
      # check general options, esp. chrono

      if uopts.find_opt('-chrono'): self.chrono = 1

      # if options are not chronological, process general options now
      # (so -show options are still in order)
      if not self.chrono:

         val, err = self.user_opts.get_type_opt(int, '-verb')
         if val != None and not err: self.verb = val

         val, err = uopts.get_type_opt(int, '-nplaces')
         if val and not err:
            self.nplaces = val

         val, err = uopts.get_type_list(float, '-run_len')
         if type(val) == type([]) and not err:
            self.run_len = val

         # main timing options

         val, err = uopts.get_string_opt('-timing')
         if val and not err:
            if self.set_timing(val): return 1

         val, err = uopts.get_type_opt(float, '-stim_dur')
         if val and not err:
            self.set_stim_dur(val)

         val, err = uopts.get_string_list('-multi_timing')
         if type(val) == type([]) and not err:
            if self.multi_set_timing(val): return 1

         val, err = uopts.get_type_list(float, '-multi_stim_dur')
         if type(val) == type([]) and not err:
            self.multi_set_stim_durs(val)


      # ------------------------------------------------------------
      # selection and process options:
      #    process sequentially, to make them like a script

      for opt in uopts.olist:

         # if all options are chronological, check load and general, too

         if self.chrono:

            # continue after any found option

            if opt.name == '-chrono': continue

            # main timing options
            if opt.name == '-timing':
               if self.set_timing(opt.parlist[0]): return 1
               continue

            elif opt.name == '-multi_timing':
               if self.multi_set_timing(opt.parlist): return 1
               continue

            elif opt.name == '-stim_dur':
               val, err = self.user_opts.get_type_opt(float, opt=opt)
               if val != None and err: return 1
               else: self.set_stim_dur(val)
               continue

            elif opt.name == '-multi_stim_dur':
               val, err = self.user_opts.get_type_list(float, opt=opt)
               if val != None and err: return 1
               else: self.multi_set_stim_durs(val)
               continue

            # general options

            elif opt.name == '-nplaces':
               val, err = self.user_opts.get_type_opt(int, '', opt=opt)
               if val != None and err: return 1
               else: self.nplaces = val
               continue

            elif opt.name == '-run_len':
               val, err = self.user_opts.get_type_list(float, opt=opt)
               if val != None and err: return 1
               else: self.run_len = val
               continue

            elif opt.name == '-verb':
               val, err = self.user_opts.get_type_opt(int, '', opt=opt)
               if val != None and err: return 1
               else: self.verb = val
               continue

         #---------- action options, always chronological ----------

         # check multi- options first, then require self.timing

         if opt.name == '-multi_show_timing_ele':
            self.show_multi()
            continue

         if opt.name == '-add_rows':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1

            newrd = LT.AfniTiming(val,verb=self.verb)
            if not newrd.ready: return 1

            self.timing.add_rows(newrd)

         elif opt.name == '-extend':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1

            newrd = LT.AfniTiming(val,verb=self.verb)
            if not newrd.ready: return 1

            self.timing.extend_rows(newrd)

         elif opt.name == '-add_offset':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            val, err = uopts.get_type_opt(float, opt=opt)
            if val != None and err: return 1
            self.timing.add_val(val)

         elif opt.name == '-sort':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            self.timing.sort()

         elif opt.name == '-show_timing':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            if self.verb > 0:
               print '++ timing (%d runs)\n' % len(self.timing.data)
            print self.timing.make_data_string(nplaces=self.nplaces)

         elif opt.name == '-show_isi_stats':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            self.show_isi_stats()

         elif opt.name == '-multi_show_isi_stats':
            if len(self.m_timing) < 1:
               print "** '%s' requires -multi_timing" % opt.name
               return 1
            self.multi_show_isi_stats()

         elif opt.name == '-transpose':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            self.timing.transpose()

         elif opt.name == '-show_timing_ele':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            self.timing.show()

         elif opt.name == '-write_timing':
            if not self.timing:
               print "** '%s' requires -timing" % opt.name
               return 1
            val, err = uopts.get_string_opt('', opt=opt)
            if val != None and err: return 1
            self.write_timing(val)

      return 0

   def show_isi_stats(self):
      if not self.timing:
         print '** no timing, cannot show stats'
         return 1

      amt = LT.AfniMarriedTiming(from_at=1, at=self.timing)

      rv = amt.show_isi_stats(mesg='single element', run_len=self.run_len)
      if rv and self.verb > 2:
         amt.make_data_string(nplaces=self.nplaces,mesg='SHOW ISI FAILURE')

      return 0

   def multi_show_isi_stats(self):
      if len(self.m_timing) < 1:
         print '** no multi-timing, cannot show stats'
         return 1

      amt = LT.AfniMarriedTiming(from_at=1, at=self.m_timing[0])

      nele = len(self.m_timing)
      for ind in range(1, nele):
         newamt = LT.AfniMarriedTiming(from_at=1, at=self.m_timing[ind])
         amt.extend_rows(newamt)

      if self.verb > 2: amt.show('final AMT')

      rv = amt.show_isi_stats(mesg='%d elements'%nele, run_len=self.run_len)
      if rv and self.verb > 2:
         print amt.make_data_string(nplaces=self.nplaces,mesg='ISI FAILURE')

      return 0

   def test(self, verb=3):
      # init
      print '------------------------ initial reads -----------------------'
      self.verb = verb
      # these should not fail, so quit if they do
      # first try AFNI_data4, then regression data
      if self.set_timing('X.xmat.1D'):
         return None

      # reset
      print '------------------------ reset files -----------------------'

      # failures
      print '------------------------ should fail -----------------------'
      self.set_timing('noxmat')

      # more tests
      return None

def main():
   ard = ATInterface()
   if not ard: return 1

   rv = ard.process_options()
   if rv > 0: return 1

   return ard.status

if __name__ == '__main__':
   sys.exit(main())


