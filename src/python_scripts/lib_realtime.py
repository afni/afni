#!/usr/bin/env python

# ----------------------------------------------------------------------
# This module holds:
# 
# RTInterface: the real-time socket interface, for receiving data
#              from the real-time plugin to afni over a TCP port.
#
# SerialInterface: interface for writing to a serial port.
# ----------------------------------------------------------------------


import sys, os

# system libraries : test, then import as local symbols
import module_test_lib
testlibs = ['time', 'socket', 'struct']
if module_test_lib.num_import_failures(testlibs): sys.exit(1)
import time, socket, struct

# AFNI libraries (besides module_test_lib)
import option_list as OL
import afni_util as UTIL        # not actually used, but probably will be

# ----------------------------------------------------------------------
# globals

g_RTinterface = None    # global reference to main class
g_magic_hi    = [0xab, 0xcd, 0xef, 0xab]
g_magic_bye   = [0xde, 0xad, 0xde, 0xad]
g_magic_len   = 4

g_SER         = None    # for serial module, only if it is needed

# ----------------------------------------------------------------------
class RTInterface:
   """interface class to hold real-time information"""
   def __init__(self, verb=1):

      # general variables
      self.show_data       = 0                  # display data in terminal
      self.swap            = 0                  # byte-swap binary numbers
      self.verb            = verb

      # per-run variables
      self.version         = 0                  # 0,1,2: for extra data
      self.nextra          = 0                  # if version > 0
      self.nread           = 0                  # TRs of data read, per connect

      # per-run accumulated data variables
      self.motion          = []                 # accumulate: 6 lists of vals
      self.extras          = []                 # accumulate: nextra lists
                                                # (each of length nread)

      # TCP connection variables
      self.server_port     = 53214              # listen for connections here
      self.nconnects       = 0                  # number of connections made

      # sockets
      self.server_sock     = None               # serve connection requests
      self.data_sock       = None               # receive data
      self.data_address    = None               # address to receive data from

   def clear_run_vals(self):
      """clear variables that vary per run"""

      self.version      = 0
      self.nextra       = 0
      self.nread        = 0

      # nuke lists that could use a lot of memory
      del(self.motion)
      self.motion = []
      del(self.extras)
      self.extras = []

   def open_incoming_socket(self):
      """create a server port to listen for connections from AFNI"""

      if self.verb>2: print '-- make server socket, port %d...'%self.server_port
      elif self.verb>0: print 'waiting for connection...'

      errs = 0

      try: self.server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         self.server_sock = None
         print '** failed to create incoming socket'
         errs = 1
      if errs: return 1         # let's not return from within 'except'

      if self.verb > 2: print '-- bind()...'
      try: self.server_sock.bind(('', self.server_port))
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         print '** failed to bind incoming socket to port', self.server_port
         errs = 1
      if errs: return 1

      if self.verb > 2: print '-- listen()...'
      try: self.server_sock.listen(2)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         print '** failed to listen at incoming socket'
         errs = 1
      if errs: return 1

      if self.verb>1: print '== server socket is open at port',self.server_port

      return 0

   def read_nbytes_from_data_socket(self, nbytes, flag=socket.MSG_WAITALL):
      """try to read nbytes from data socket, reporting any errors"""
      errs = 0

      # It is important to specify the list of exceptions to trap here,
      # otherwise it would catch a ctrl-c and continue after sys.exit().
      try: data = self.data_sock.recv(nbytes, flag)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         if self.verb > 0: print '** recv() exception on data socket'
         errs = 1
      if errs: return None
      if not data:
         if self.verb > 0:
            print '** failed recv() of %d bytes from data socket' % nbytes
         return None

      if self.verb > 4:
         print "++ read %d bytes from socket: %s" \
               % (nbytes, UTIL.data_to_hex_str([ord(v) for v in data]))

      if len(data) != nbytes:
         print '** read only %d of %d bytes from data socket'%(len(data),nbytes)
         return None

      return data

   def read_ints_from_socket(self, nvals):
      """try to read nvals (4-byte) integers from data socket,
         possibly byte-swapping

         return a list of integers (None on error)"""

      if nvals <= 0: return []

      data = self.read_nbytes_from_data_socket(nvals*4)  # read all bytes
      if not data:
         if self.verb > 0: print "** failed to read %d int(s)" % nvals
         return None

      # swap one value at a time
      vals = []
      if self.swap: UTIL.swap4(data)

      vals = list(struct.unpack('i'*nvals,data))
      del(data)

      if self.verb > 3: print "++ read %d ints: %s" % (nvals, vals)

      return vals

   def read_floats_from_socket(self, nvals):
      """try to read nvals (4-byte) floats from data socket,
         possibly byte-swapping

         return a list of floats (None on error)"""

      if nvals <= 0: return []

      data = self.read_nbytes_from_data_socket(nvals*4)  # read all bytes
      if not data:
         if self.verb > 0: print "** failed to read %d floats(s)" % nvals
         return None

      # swap one value at a time
      vals = []
      if self.swap:
         for ind in range(nvals):
            off = ind*4
            v           = data[off]     # swap d[0] and d[3]
            data[off]   = data[off+3]
            data[off+3] = v
            v           = data[off+1]   # swap d[1] and d[2]
            data[off+1] = data[off+2]
            data[off+2] = v

      vals = list(struct.unpack('f'*nvals,data))

      if self.verb > 3: print "++ read %d floats: %s" % (nvals, vals)

      return vals

   def peek_at_next_bytes(self, nbytes, show=0):
      """peek at and print out the next nbytes of data"""
      data = self.read_nbytes_from_data_socket(nbytes,flag=socket.MSG_PEEK)
      if not data:
         print '** failed to peek ahead'
         return

      if show or self.verb > 4:
         odata = [ord(v) for v in data]
         print '== peek ahead data: %s' % UTIL.data_to_hex_str(odata)

      return data

   def read_magic_hi(self):
      """read and parse magic_hi from data socket, set version

         if version > 0, set nextra """

      # if self.verb > 3: self.peek_at_next_bytes(8)

      data = self.read_nbytes_from_data_socket(g_magic_len)
      if not data: return 1

      odata = [ord(v) for v in data]
      if self.verb > 2:
         print '++ recieved as magic_hi: %s' % UTIL.data_to_hex_str(odata)

      # test whether we have magic, start by ignoring the last byte
      for ind in range(g_magic_len-1):
         if odata[ind] != g_magic_hi[ind]:
            print '** HELLO string is not magic, want %s but have %s' \
               % (UTIL.data_to_hex_str(g_magic_hi),UTIL.data_to_hex_str(odata))
            return 1

      # now check the last byte for HELLO and version
      self.version = odata[g_magic_len-1] - g_magic_hi[g_magic_len-1]
      if self.verb > 2:
         print '-- hello version is %d' % self.version

      # and deal with the version number

      if self.version == 0: pass        # we're good to go

      elif self.version == 1 or self.version == 2:
         # ------------------------------------------------------------
         # read the next 4-byte int to determine the number of extra data
         # values received each TR
         #    version 1: receive num_extra int over socket
         #    version 2: receive num_voxels (for 8 vals each) int over socket

         ilist = self.read_ints_from_socket(1)
         if ilist == None: return 1

         if ilist[0] < 0: print '** received invalid num_extra = %d' % ilist[0]
         elif self.version == 1:
            self.nextra = ilist[0]
         else: # version = 2
            self.nextra = ilist[0] * 8

         if self.verb > 2:
            print '-- num extra = %d' % self.nextra

      else:     # bad, naughty version!
         print '** HELLO string trailer is not magic, want %s but have %s' \
            % (UTIL.data_to_hex_str(g_magic_hi),UTIL.data_to_hex_str(odata))
         return 1

      # todo - show_time()

      return 0

   def wait_for_socket(self):
      """wait for a talk request from the AFNI real-time plugin

         client should send magic_hi string"""

      self.data_sock, self.data_address = self.server_sock.accept()

      if self.data_sock == None:
         print '** failed accept(), closing and restarting...'
         return 1

      if self.verb > 0:
         hinfo = list(self.data_address)
         print 'connection established from host %s on port %d' \
               % (hinfo[0], hinfo[1])

      if self.read_magic_hi():
         print '** failed read magic_hi, closing and restarting...'
         return 1

      self.nconnects += 1       # we have a connection

      if self.verb>2: print '-- valid socket for run %d' % self.nconnects

      return 0

   def display_TR_data(self, tr=-1):
      """display motion and any extras for the given TR (last if not set)"""

      if tr < 0: tr = len(motion[0])-1

      if self.verb > 3: print '-- displaying data for TR %d' % tr

      mprefix = "++ recv motion:     "
      if self.version==1:   eprefix = "++ recv %d extras:   "%self.nextra
      elif self.version==2: eprefix = "++ recv %dx8 extras: "%(self.nextra//8)

      print UTIL.float_list_string([self.motion[i][tr] for i in range(6)],
                           nchar=9, ndec=5, nspaces=2, mesg=mprefix, left=1)
      
      # version 1, all on one line
      if self.version == 1 and self.nextra > 0:
         print UTIL.gen_float_list_string([self.extras[i][tr] for i in
                           range(self.nextra)], mesg=eprefix, nchar=10, left=1)
      # version 2, each voxel on one line
      elif self.version == 2 and self.nextra > 0:
         print eprefix,
         elen = len(eprefix)+1
         print UTIL.gen_float_list_string([self.extras[i][tr]
                   for i in range(8)], mesg='', nchar=10, left=1)
         for off in range(self.nextra//8 - 1):
            print UTIL.gen_float_list_string([self.extras[8*off+8+i][tr]
                  for i in range(8)], mesg=' '*elen, nchar=10, left=1)

   def socket_has_closed(self):
      """peek ahead for close message"""

      data = self.peek_at_next_bytes(g_magic_len)
      if not data:
         if self.verb > 0: print '** socket has gone dead, restarting...'
         return 1

      odata = [ord(v) for v in data]
      if self.verb > 3:
         print '++ testing as magic_bye: %s' % UTIL.data_to_hex_str(odata)

      # if not magic bye, return a negative
      for ind in range(g_magic_len-1):
         if odata[ind] != g_magic_bye[ind]: return 0

      if self.verb > 0: print '++ found close request for run %d, TRs = %d' \
                              % (self.nconnects, self.nread)

      return 1

   def read_TR_data(self):
      """read in motion and extras for the current TR, if show_data, do so"""

      if self.socket_has_closed(): return 1

      # read and append motion values
      values = self.read_floats_from_socket(6)
      if not values:
         print '** read socket error, abrupt close: run %d, TRs %d' \
               % (self.nconnects, self.nread)
         return 1
      for ind in range(6):
         self.motion[ind].append(values[ind])

      if self.verb > 4: print '%% current motion[0]: %s' % self.motion[0]

      # read and append extra values
      if self.nextra > 0:
         values = self.read_floats_from_socket(self.nextra)
         if not values:
            print '** failed to read %d extras for TR %d' \
                  % (self.nextra, self.nread+1)
            return 1
         for ind in range(self.nextra): self.extras[ind].append(values[ind])

      # possibly display TR data
      if self.show_data or self.verb > 2: self.display_TR_data(self.nread)

      self.nread += 1

      return 0

   def read_all_socket_data(self):

      # initialize lists
      self.motion = [[] for i in range(6)]              # array of 6 lists
      self.extras = [[] for i in range(self.nextra)]    # array of nextra lists

      rv = 0
      while rv == 0:
         rv = self.read_TR_data()
         if rv: break                   # for good or ill, we are done
         pass                           # PROCESS DATA HERE

      if self.verb > 1:
         print '-- processed %d TRs of data' % self.nread ,
         if rv > 0: print '(terminating on success)'
         else:      print '(terminating on error)'
      if self.verb > 0: print '-'*60

      if rv > 0: return 0               # success for one run
      else:      return 1               # some error

   def wait_for_new_run(self):

      if self.verb>1: print '++ waiting for run %d...' % (self.nconnects+1)

      # reset variables that vary per run
      self.clear_run_vals()

      # wait for the real-time plugin to talk to us
      if self.wait_for_socket(): return 1

      # initialize lists
      self.motion = [[] for i in range(6)]              # array of 6 lists
      self.extras = [[] for i in range(self.nextra)]    # array of nextra lists

      return 0

   def close_data_ports(self):

      if self.data_sock != None:
         try: self.data_sock.close()
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            pass
         self.data_sock = None

      if self.verb > 3: print '-- socket has been closed'

      return

# ----------------------------------------------------------------------
class SerialInterface:
   """interface class to deal with serial port information"""
   def __init__(self, sport, verb=1):
      global g_SER                      # global reference for serial library

      # see if the serial library is available
      if module_test_lib.num_import_failures(['serial']): return None
      import serial
      g_SER = serial

      # main variables
      self.verb         = verb          # verbose level
      self.port_file    = sport         # file for serial port
      self.data_port    = None          # serial data port
      self.swap         = 0             # whether to swap serial bytes

      if self.verb > 1: print '++ initializing serial interface %s...' % sport

   def open_data_port(self):
      if not self.port_file:
         print '** no file to open as serial port'
         return 1

      if self.verb > 3: print '-- opening serial port', self.port_file

      # open port_file at baud 9600, 8 bit N parity, 1 stop bit
      errs = 0
      try:
         port = g_SER.Serial()
         port.setPort(self.port_file)
         port.setBaudrate(9600)
         port.setByteSize(g_SER.EIGHTBITS)
         port.setParity(g_SER.PARITY_NONE)
         port.setStopbits(g_SER.STOPBITS_ONE)
         port.setXonXoff(0)             # enable software flow control
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         print sys.exc_info()[1]
         print '** failed to initialize serial port', self.port_file
         errs = 1

      if errs == 0:
         try: port.open()
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            print sys.exc_info()[1]
            print '** failed to open serial port', self.port_file
            errs = 1

      if errs == 0:
         self.data_port = port
         if self.verb > 2: print '++ serial port %s is open' % self.port_file

      return errs

   def close_data_ports(self):

      if self.data_port:
         try: self.data_port.close()
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            pass
         self.data_port = None

      if self.verb > 2: print '-- serial port has been closed'

      return 0

   def write_4byte_data(self, data):
      """write all floats/ints to the serial port"""

      if not self.data_port: return
      if not self.data_port.isOpen(): return

      if self.verb > 4: print '++ writing data to serial port:', data

      dstring = struct.pack('f'*len(data), *data)
      if self.swap: UTIL.swap4(dstring)

      if self.verb > 5: print '++ hex data to serial port:',    \
                        UTIL.data_to_hex_str([ord(v) for v in dstring])

      self.data_port.write(dstring)

      del(dstring)

      return 0

if __name__ == '__main__':
   print '** main is not supported in this library'


