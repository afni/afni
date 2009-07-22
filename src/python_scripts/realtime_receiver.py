#!/usr/bin/env python

import sys, os
if 1 :  # for testing, might add the current dir and ~/abin to the PATH
   try:    sys.path.extend(['.', '%s/abin' % os.getenv('HOME')])
   except: print '** cannot extend path!'

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
                             -serial_data_choice motion_norm

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

   terminal options:

      -help                     : show this help
      -hist                     : show module history
      -show_valid_opts          : list valid options
      -ver                      : show current version

   other options
      -verb LEVEL               : set the verbosity level

-----------------------------------------------------------------------------
R Reynolds    July 2009
=============================================================================
"""
g_history = """
   realtime_receiver.py history:

   0.0  Jul 06, 2009 : initial version (show data, no serial, little help)
   0.1  Jul 16, 2009 : includes optional serial connection
"""

g_version = "realtime_receiver.py version 0.1, Jul 16, 2009"

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
      self.verb            = 1
      self.serial_port     = None          # serial port (filename)

      # lib_realtime.py class instances
      self.RTI             = None          # real-time interface RTInterface
      self.SER             = None          # serial port interface Serial

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

      valid_opts.add_opt('-serial_data_choice', 1, [],
                      helpstr='which data to send (motion, motion_norm,...)')
      valid_opts.add_opt('-serial_port', 1, [],
                      helpstr='serial port filename (e.g. /dev/ttyS0 or COM1)')
      valid_opts.add_opt('-show_data', 1, [],
                      acplist=['no', 'yes'],
                      helpstr='whether to display received data in terminal')
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

      val, err = uopts.get_string_opt('-serial_data_choice')
      if val != None and not err: self.SER.data_choice = val

      # ==================================================
      # --- tcp options ---

      val, err = uopts.get_string_opt('-show_data')
      if val != None and not err:
         if val == 'no': self.RTI.show_data = 0
         else:           self.RTI.show_data = 1

      if uopts.find_opt('-swap'): self.RTI.swap = 1

      val, err = uopts.get_type_opt(int, '-tcp_port')
      if val != None and not err: self.RTI.server_port = val

      return 0  # so continue and listen

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

   def process_one_TR(self):
      """return 0 to continue, 1 on valid termination, -1 on error"""

      rv = self.RTI.read_TR_data()
      if rv == 0 and self.SER: 
         rv, data = compute_data_for_serial_port(self)  # PROCESS DATA HERE
         if rv == 0 and len(data) > 0:
            self.SER.write_4byte_data(data)

      return rv

   def process_one_run(self):
      """repeatedly: process all incoming data for a single run"""

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
      g_RTinterface.server_sock.close()

   sys.exit(signum)

def compute_data_for_serial_port(rec):
   """If writing to the serial port, this is the main function to compute
      results from rec.motion and/or rec.extras for the current TR and 
      return it as an array of floats.

      Note that motion and extras are lists of time series of length nread,
      so processing a time series is easy, but a single TR requires extracting
      the data from the end of each list.

      The possible computations is based on SER.data_choice, specified by the
      user option -serial_data_choice.  If you want to send data that is not
      listed, just add a condition.

   ** Please add each data_choice to the -help.  Search for motion_norm to
      find all places to edit.

      return 2 items:
        error code:     0 on success, -1 on error
        data array:     (possibly empty) array of data to send
   """

   rti = rec.RTI       # for convenience
   ser = rec.SER       # for convenience

   if not ser:             return 0, []
   if not ser.data_port:   return 0, []
   if not ser.data_choice: return 0, []

   # case 'motion': send all motion
   if ser.data_choice == 'motion':
      if rti.nread > 0:
         return 0, [rti.motion[ind][rti.nread-1] for ind in range(6)]
      else: return -1, []

   # case 'motion_norm': send Euclidean norm of motion params
   #                     --> sqrt(sum of squared motion params)
   elif ser.data_choice == 'motion_norm':
      if rti.nread > 0:
         motion = [rti.motion[ind][rti.nread-1] for ind in range(6)]
         return 0, [UTIL.euclidean_norm(motion)]
      else: return -1, []

   # case 'all_extras': send all extra data
   elif ser.data_choice == 'all_extras':
      if rti.nextra > 0:
         return 0, [rti.extras[i][rti.nread-1] for i in range(rti.nextra)]
      else: return -1, []

   # failure!
   else:
      print "** invalid data_choice '%s', shutting down ..." % ser.data_choice
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
      if receiver.process_one_run():
         time.sleep(1)                  # on error, ponder life briefly
      receiver.close_data_ports()

   return -1                            # should not be reached

if __name__ == '__main__':
   sys.exit(main())


