#!/usr/bin/env python

import sys, os

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['signal', 'time']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import signal, time

# AFNI libraries (besides module_test_lib)
import option_list as OL
import lib_realtime as RT
import afni_util as UTIL

# ----------------------------------------------------------------------
# globals

g_help_string = """
=============================================================================
realtime_receiver.py - program to receive and display real-time plugin data

   This program receives motion parameters and optionally ROI averages
   or voxel data each TR from the real-time plugin to afni.  Which data
   will get sent is controlled by the real-time plugin.  All data is
   sent as floats.

   Motion parameters: 6 values per TR
   ROI averages:      N values per TR, where N is the number of ROIs
   All voxel data:    8 values per voxel per TR (might be a lot of data!)
                        The 8 values include voxel index, 3 ijk indices,
                        the 3 xyz coordinates, and oh yes, the data

   Examples:

     1. Run in test mode to just display data on the terminal window.

        realtime_receiver.py -show_data

     2. Provide a serial port, sending the Euclidean norm of the motion params.

        realtime_receiver.py -show_data yes -serial_port /dev/ttyS0  \\
                             -data_choice motion_norm

     3. Run a feedback demo.  Assume that the realtime plugin will send 2
        values per TR.  Request the receiver to plot (a-b)/(a+b), scaled
        to some small integral range.

        realtime_receiver.py -show_demo_gui yes -data_choice diff_ratio

   TESTING NOTE:

        This setup can be tested off-line using Dimon, afni and this
        realtime_receiver.py program.

   COMMUNICATION NOTE:

        This program listens for connections at TCP port 53214, unless an
        alternate port is specified.  The real-time plugin (or some other
        program) connects at that point, opening a new data socket.  There
        is a "handshake" on the data socket, and then data is recieved until
        a termination signal is received (or the socket goes bad).

        Data is sent per run, meaning the connection should be terminated
        and restarted at the end of each run.

        The handshake should be the first data on the data socket (per run).
        The real-time plugin (or other program) will send the hello bytes:
        0xabcdefab, where the final byte may be incremented by 0, 1 or 2
        to set the version number, e.g. use 0xabcdefac for version 1.

           Version 0: only motion will be sent
           Version 1: motion plus N ROI averages will be sent
           Version 2: motion plus all voxel data for N voxels will be sent

        If the version is 1 or 2, the 4-byte handshake should be followed
        by a 4-byte integer, specifying the value of N.  Hence, the 
        combination of the version number and any received N will determine
        how much data will be sent to the program each TR.

        At the end of the run, the sending program should send the 4-byte
        good-bye sequence: 0xdeaddead.

   This program is based on the structure of serial_helper, but because
   it is meant as a replacement, it will have different options.

   ------------------------------------------
   Options:

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -data_choice CHOICE       : pick which data to send as feedback
      -serial_port PORT         : specify serial port file for feedback data
      -show_data yes/no         : display incoming data in terminal window
      -show_demo_data           : display feedback data in terminal window
      -show_demo_gui            : demonstrate a feedback GUI
      -swap                     : swap bytes incoming data
      -tcp_port PORT            : specify TCP port for incoming connections
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    July 2009
=============================================================================
"""
g_history = """
   realtime_receiver.py history:

   0.0  Jul 06, 2009 : initial version (show data, no serial, little help)
   0.1  Jul 16, 2009 : includes optional serial connection
   0.2  Aug 04, 2009 : added basic demo interface and itemized exception traps
   0.3  Sep 08, 2009 : bind to open host (so /etc/hosts entry is not required)
"""

g_version = "realtime_receiver.py version 0.3, Sep 8, 2009"

g_RTinterface = None      # global reference to main class (for signal handler)

# ----------------------------------------------------------------------
# In this module, handing signals and options.  Try to keep other
# operations in separate libraries (e.g. lib_realtime.py).
# ----------------------------------------------------------------------

class ReceiverInterface:
   """main interface for realtime_receiver.py"""
   def __init__(self):

      self.valid_opts      = None
      self.user_opts       = None
      self.data_choice     = 'motion'
      self.TR_data         = []            # store computed TR data
      self.verb            = 1
      self.serial_port     = None          # serial port (filename)

      # lib_realtime.py class instances
      self.RTI             = None          # real-time interface RTInterface
      self.SER             = None          # serial port interface Serial

      # demo attributes
      self.show_demo_data  = 0
      self.demo_frame      = None          # for demo plot
      self.wx_app          = None          # wx App for demo plot

      self.valid_opts = self.init_options()

   def init_options(self):
      """return an option list instance"""

      valid_opts = OL.OptionList('valid opts')

      # short, terminal arguments
      valid_opts.add_opt('-help', 0, [],
                      helpstr='display program help')
      valid_opts.add_opt('-hist', 0, [],
                      helpstr='display the modification history')
      valid_opts.add_opt('-show_valid_opts', 0, [],
                      helpstr='display all valid options')
      valid_opts.add_opt('-ver', 0, [],
                      helpstr='display the current version number')

      # general options
      valid_opts.add_opt('-verb', 1, [],
                      helpstr='set the verbose level (default is 1)')

      valid_opts.add_opt('-data_choice', 1, [],
                      helpstr='which data to send (motion, motion_norm,...)')
      valid_opts.add_opt('-serial_port', 1, [],
                      helpstr='serial port filename (e.g. /dev/ttyS0 or COM1)')
      valid_opts.add_opt('-show_data', 1, [],
                      acplist=['no', 'yes'],
                      helpstr='whether to display received data in terminal')

      # demo options
      valid_opts.add_opt('-show_demo_data', 1, [],
                      acplist=['no', 'yes'],
                      helpstr='whether to display demo data in terminal')
      valid_opts.add_opt('-show_demo_gui', 1, [],
                      acplist=['no', 'yes'],
                      helpstr='whether to display demo data in a GUI')

      valid_opts.add_opt('-swap', 0, [],
                      helpstr='byte-swap numerical reads')
      valid_opts.add_opt('-tcp_port', 1, [],
                      helpstr='TCP port for incoming connections')

      # todo
      valid_opts.add_opt('-show_comm_times', 0, [],
                      helpstr='display communication times')

      return valid_opts


   def check_terminal_opts(self):
      """check argv for terminal options, start with a global library
         call to check_special_opts"""

      self.valid_opts.check_special_opts(sys.argv)

      # if no arguments are given, apply -help
      if len(sys.argv) < 2 or '-help' in sys.argv:
         print g_help_string
         return 1

      if '-hist' in sys.argv:
         print g_history
         return 1

      if '-show_valid_opts' in sys.argv:
         self.valid_opts.show('', 1)
         return 1

      if '-ver' in sys.argv:
         print g_version
         return 1

      return 0

   def process_options(self):
      """process all options, applying to interfaces where appropriate"""

      # ==================================================
      # first fire up the TCP interface

      self.RTI = RT.RTInterface()
      if not self.RTI: return None

      # and store globally
      global g_RTinterface
      g_RTinterface = self.RTI      # global for signal handler

      # ==================================================
      # gather and process the user options
      self.user_opts = OL.read_options(sys.argv, self.valid_opts)
      uopts = self.user_opts            # for convenience
      if not uopts: return 1

      # process -verb first
      val, err = uopts.get_type_opt(int, '-verb')
      if val != None and not err:
         self.verb = val
         self.RTI.verb = val

      # ==================================================
      # --- serial options ---

      # port first: if set, create SerialInterface
      val, err = uopts.get_string_opt('-serial_port')
      if val != None and not err:
         self.SER = RT.SerialInterface(val, verb=self.verb)
         if not self.SER: return 1

      val, err = uopts.get_string_opt('-data_choice')
      if val != None and not err: self.data_choice = val

      # ==================================================
      # --- tcp options ---

      val, err = uopts.get_string_opt('-show_data')
      if val != None and not err:
         if val == 'no': self.RTI.show_data = 0
         else:           self.RTI.show_data = 1

      if uopts.find_opt('-swap'): self.RTI.swap = 1

      val, err = uopts.get_type_opt(int, '-tcp_port')
      if val != None and not err: self.RTI.server_port = val

      # ==================================================
      # --- demo options ---

      val, err = uopts.get_string_opt('-show_demo_data')
      if val != None and not err:
         if val == 'no': self.show_demo_data = 0
         else:           self.show_demo_data = 1

      val, err = uopts.get_string_opt('-show_demo_gui')
      if val != None and not err:
         if val == 'yes': self.set_demo_gui()

      return 0  # so continue and listen

   def set_demo_gui(self):
      """create the GUI for display of the demo data"""
      testlibs = ['numpy', 'wx']
      if module_test_lib.num_import_failures(testlibs): sys.exit(1)
      import numpy as N, wx
      import lib_RR_plot as LPLOT

      self.wx_app = wx.App()
      self.demo_frame = LPLOT.CanvasFrame(title='receiver demo')
      self.demo_frame.EnableCloseButton(True)
      self.demo_frame.Show(True)
      self.demo_frame.style  = 'bar'
      self.demo_frame.xlabel = 'most recent 10 TRs'
      self.demo_frame.ylabel = 'scaled diff_ratio'

      # for the current demo, set an ranges for 10 numbers in [0,10]
      if self.demo_frame.style == 'graph':
         self.demo_frame.set_limits(0,9.1,-0.1,10.1)
      elif self.demo_frame.style == 'bar':
         self.demo_frame.set_limits(0,10.1,-0.1,10.1)

   def set_signal_handlers(self):
      """capture common termination signals, to properly close ports"""

      if self.verb > 1: print '++ setting signals'

      slist = [ signal.SIGHUP, signal.SIGINT, signal.SIGQUIT, signal.SIGTERM ]
      if self.verb > 2: print '   signals are %s' % slist

      for sig in slist: signal.signal(sig, clean_n_exit)

      return

   def close_data_ports(self):
      """close TCP and socket ports, except for server port"""

      if self.RTI: self.RTI.close_data_ports()
      if self.SER: self.SER.close_data_ports()

   def process_demo_data(self):

      length = len(self.TR_data)
      if length == 0: return

      if self.show_demo_data:
         print '-- TR %d, demo value: ' % length, self.TR_data[length-1][0]
      if self.demo_frame:
         if length > 10: bot = length-10
         else: bot = 0
         pdata = [self.TR_data[ind][0] for ind in range(bot,length)]
         self.demo_frame.plot_data(pdata)

   def process_one_TR(self):
      """return 0 to continue, 1 on valid termination, -1 on error"""

      if self.verb>2:
         print '-- process_one_TR, show_demo_data = %d,' % self.show_demo_data

      rv = self.RTI.read_TR_data()
      if rv:
         if self.verb > 3: print '** process 1 TR: read data failure'
         return rv

      rv, data = compute_TR_data(self)  # PROCESS DATA HERE
      if rv or len(data) == 0: return rv
      self.TR_data.append(data)

      if self.SER: self.SER.write_4byte_data(data)
      if self.show_demo_data or self.demo_frame: self.process_demo_data()

      return rv

   def process_one_run(self):
      """repeatedly: process all incoming data for a single run
         return  0 on success and 1 on error
      """

      # clear any old data
      if len(self.TR_data) > 0:
         del(self.TR_data)
         self.TR_data = []

      # wait for the real-time plugin to talk to us
      if self.RTI.wait_for_new_run(): return 1

      # possibly open a serial port
      if self.SER:
         if self.SER.open_data_port(): return 1

      # process one TR at a time until 
      rv = self.process_one_TR()
      while rv == 0: rv = self.process_one_TR()

      if self.verb > 1:
         print '-- processed %d TRs of data' % self.RTI.nread ,
         if rv > 0: print '(terminating on success)'
         else:      print '(terminating on error)'
      if self.verb > 0: print '-'*60

      if rv > 0: return 0               # success for one run
      else:      return 1               # some error

def clean_n_exit(signum, frame):

   verb = g_RTinterface.verb

   if verb > 1: print '++ signal handler called with signal', signum

   g_RTinterface.close_data_ports()

   # at last, close server port
   if g_RTinterface.server_sock:
      if g_RTinterface.verb > 1: print 'closing server port...'
      try: g_RTinterface.server_sock.close()
      except (RT.socket.error, RT.socket.timeout): pass

   if g_RTinterface.verb > 0: print '-- exiting on signal %d...' % signum
   sys.exit(signum)

def compute_TR_data(rec):
   """If writing to the serial port, this is the main function to compute
      results from rec.motion and/or rec.extras for the current TR and 
      return it as an array of floats.

      Note that motion and extras are lists of time series of length nread,
      so processing a time series is easy, but a single TR requires extracting
      the data from the end of each list.

      The possible computations is based on data_choice, specified by the user
      option -data_choice.  If you want to send data that is not listed, just
      add a condition.

   ** Please add each data_choice to the -help.  Search for motion_norm to
      find all places to edit.

      return 2 items:
        error code:     0 on success, -1 on error
        data array:     (possibly empty) array of data to send
   """

   rti = rec.RTI       # for convenience
   if not rec.data_choice: return 0, []

   # case 'motion': send all motion
   if rec.data_choice == 'motion':
      if rti.nread > 0:
         return 0, [rti.motion[ind][rti.nread-1] for ind in range(6)]
      else: return -1, []

   # case 'motion_norm': send Euclidean norm of motion params
   #                     --> sqrt(sum of squared motion params)
   elif rec.data_choice == 'motion_norm':
      if rti.nread > 0:
         motion = [rti.motion[ind][rti.nread-1] for ind in range(6)]
         return 0, [UTIL.euclidean_norm(motion)]
      else: return -1, []

   # case 'all_extras': send all extra data
   elif rec.data_choice == 'all_extras':
      if rti.nextra > 0:
         return 0, [rti.extras[i][rti.nread-1] for i in range(rti.nextra)]
      else: return -1, []

   # case 'diff_ratio': (a-b)/(abs(a)+abs(b))
   elif rec.data_choice == 'diff_ratio':
      npairs = rti.nextra//2
      if npairs > 0:
         vals = [rti.extras[i][rti.nread-1] for i in range(rti.nextra)]
         # modify vals array, setting the first half to diff_ratio
         for ind in range(npairs):
            a = vals[2*ind]
            b = vals[2*ind+1]
            if a == 0 and b == 0: newval = 0.0
            else: newval = (a-b)/float(abs(a)+abs(b))

            # now scale [bot,inf) to {0..10}, where val>=top -> 10
            bot = 0.008
            top = 0.031 - bot
            val = newval-bot
            if val < 0.0: val = 0.0
            ival = int((10*val/top))
            if ival > 10: ival = 10

            vals[ind] = ival

            if rti.verb > 1: '++ diff_ratio: ival = %d (from %g)'%(ival,newval)

            return 0, vals[0:npairs]    # return the partial list

      else:
         if rti.verb > 0 and rti.nread < 2:
            print '** no pairs to compute diff_ratio from...'
         return 0, []

   # failure!
   else:
      print "** invalid data_choice '%s', shutting down ..." % rec.data_choice
      return -1, []

def main():

   # create main interface
   receiver = ReceiverInterface()
   if receiver == None: return 1

   # set options and look for early termination
   if receiver.check_terminal_opts(): return 0  # then exit

   # read and process user options
   if receiver.process_options(): return 1

   # ----------------------------------------------------------------------
   # ready to rock: set signal handlers and look for data

   receiver.set_signal_handlers()                # require signal to exit

   # prepare for incoming connections
   if receiver.RTI.open_incoming_socket(): return 1

   # repeatedly: process all incoming data for a single run
   while 1:
      rv = receiver.process_one_run()
      if rv: time.sleep(1)              # on error, ponder life briefly
      receiver.close_data_ports()

   return -1                            # should not be reached

if __name__ == '__main__':
   sys.exit(main())

