
# ----------------------------------------------------------------------
# This module holds:
#
# RTInterface: the real-time socket interface, for receiving data
#              from the real-time plugin to afni over a TCP port.
#
# SerialInterface: interface for writing to a serial port.
# ----------------------------------------------------------------------

import sys
import time
import struct
import logging
import platform
import socket
# import datetime

log = logging.getLogger(__name__)
log.setLevel(logging.CRITICAL)
# dateTimeString = datetime.datetime.now().strftime("-%Y-%m-%d-%H-%M-%S") 
# logging.basicConfig(filename='realtime' + dateTimeString + '.log')

try:
    import serial
except ImportError:
    pass

# globals
g_magic_hi     = [0xab, 0xcd, 0xef, 0xab]
g_magic_bye    = [0xde, 0xad, 0xde, 0xad]
g_magic_len    = 4
g_start_time   = 0       # time for starting a new run
g_default_port = 53214



class RTInterface(object):

   """interface class to hold real-time information"""



   def __init__(self, port=g_default_port):

      # general variables
      self.show_data       = False              # display data in terminal
      self.show_times      = False              # display run times for data
      self.swap            = False              # byte-swap binary numbers

      # per-run variables
      self.version         = 0                  # 0,1,2: for extra data
      self.nextra          = 0                  # if version > 0
      self.nread           = 0                  # TRs of data read, per connect

      # per-run accumulated data variables
      self.motion          = []                 # accumulate: 6 lists of vals
      self.extras          = []                 # accumulate: nextra lists
      # (each of length nread)

      # TCP connection variables
      self.server_port     = port               # listen for connections here
      self.nconnects       = 0                  # number of connections made

      # sockets
      self.server_sock     = None               # serve connection requests
      self.data_sock       = None               # receive data
      self.data_address    = None               # address to receive data from



   def __del__(self):

      self.close_data_ports()



   def reset(self):

      """clear variables that vary per run"""

      self.version   = 0
      self.nextra    = 0
      self.nread     = 0

      # nuke lists that could use a lot of memory
      del(self.motion)
      self.motion = []
      del(self.extras)
      self.extras = []



   def open_incoming_socket(self):

      """create a server port to listen for connections from AFNI"""

      log.debug('-- make server socket, port %d...' % self.server_port)
      log.info('waiting for connection...')

      try:
         self.server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         self.server_sock = None
         log.error('** failed to create incoming socket')
         return 1

      log.debug('-- bind()...')
      try:
         self.server_sock.bind(('', self.server_port))
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         log.error('** failed to bind incoming socket to port %d' % self.server_port)
         return 1

      log.debug('-- listen()...')
      try:
         self.server_sock.listen(2)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         log.error('** failed to listen at incoming socket')
         return 1

      log.info('== server socket is open at port %d' % self.server_port)

      return 0



   def read_nbytes_from_data_socket(self, nbytes):

      """try to read nbytes from data socket, reporting any errors"""

      # It is important to specify the list of exceptions to trap here,
      # otherwise it would catch a ctrl-c and continue after sys.exit().

      # The following has to be done as MSG_WAITALL doesn't seem to be
      # defined for Windows.  This workaround was found at:
      #
      #    https://www.programcreek.com/python/example/63443/socket.MSG_WAITALL
      #
      if hasattr(socket, "MSG_WAITALL"):
         try:
            data = self.data_sock.recv(nbytes, socket.MSG_WAITALL)
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            log.error('** recv() exception on data socket')
            return None
      else:
         try:
            data = b''
            while len(data) < nbytes:
               data += self.data_sock.recv(nbytes - len(data))
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            log.error('** recv() exception on data socket')
            return None

      if not data:
         log.error('** failed recv() of %d bytes from data socket' % nbytes)
         return None

      # TODO: might be TOO verbose
      log.debug("++ read %d bytes from socket: %s" %
                (nbytes, data_to_hex_str([ord(v) for v in data])))

      if len(data) != nbytes:
         log.error('** read only %d of %d bytes from data socket' % (len(data), nbytes))
         return None

      return data



   def read_ints_from_socket(self, nvals):

      """try to read nvals (4-byte) integers from data socket,
         possibly byte-swapping

         return a list of integers (None on error)"""

      if nvals <= 0:
         return []

      data = self.read_nbytes_from_data_socket(nvals * 4)      # read all bytes

      if not data:
         log.error("** failed to read %d int(s)" % nvals)
         return None

      # swap one value at a time
      vals = []
      if self.swap:
         swap4(data)

      # Unpack returns a tuple, even if there is only a single value
      vals = list(struct.unpack('i' * nvals, data))
      del(data)

      log.debug("++ read %d ints: %s" % (nvals, vals))

      return vals



   def read_floats_from_socket(self, nvals):

      """try to read nvals (4-byte) floats from data socket,
         possibly byte-swapping

         return a list of floats (None on error)"""

      if nvals <= 0:
         return []

      data = self.read_nbytes_from_data_socket(nvals * 4)      # read all bytes

      if not data:
         log.error("** failed to read %d floats(s)" % nvals)
         return None

      # swap one value at a time
      vals = []
      if self.swap:
         swap4(data)

      vals = list(struct.unpack('f' * nvals, data))

      log.debug("++ read %d floats: %s" % (nvals, vals))

      return vals



   def peek_at_next_bytes(self, nbytes):

      """peek at and print out the next nbytes of data"""

      try:
         data = self.data_sock.recv(nbytes, socket.MSG_PEEK)
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         log.error('** recv() exception on PEEK on data socket')
         return None

      if not data:
         log.error('** failed to peek ahead')
         return None

      # Debug output
      # odata = [ord(v) for v in data]
      # log.debug('== peek ahead data: %s' % data_to_hex_str(odata))

      return data



   def read_magic_hi(self):

      """read and parse magic_hi from data socket, set version

         if version > 0, set nextra """

      # log.debug(self.peek_at_next_bytes(8))

      data = self.read_nbytes_from_data_socket(g_magic_len)
      if not data:
         return 1

      odata = [ord(v) for v in data]
      log.debug('++ recieved as magic_hi: %s' % data_to_hex_str(odata))

      # test whether we have magic, start by ignoring the last byte
      for ind in range(g_magic_len - 1):
         if odata[ind] != g_magic_hi[ind]:
            log.error('** HELLO string is not magic, want %s but have %s' \
                      % (data_to_hex_str(g_magic_hi), data_to_hex_str(odata)))
            return 1

      # now check the last byte for HELLO and version
      self.version = odata[g_magic_len - 1] - g_magic_hi[g_magic_len - 1]
      log.debug('-- hello version is %d' % self.version)

      # and deal with the version number

      if self.version == 0:
         pass        # we're good to go

      elif self.version == 1 or self.version == 2:

         # ------------------------------------------------------------
         # read the next 4-byte int to determine the number of extra data
         # values received each TR
         #    version 1: receive num_extra int over socket
         #    version 2: receive num_voxels (for 8 vals each) int over socket

         ilist = self.read_ints_from_socket(1)
         if ilist is None:
            return 1

         if ilist[0] < 0:
            log.warning('** received invalid num_extra = %d' % ilist[0])
         elif self.version == 1:
            self.nextra = ilist[0]
         else:  # version = 2
            self.nextra = ilist[0] * 8

         log.debug('-- num extra = %d' % self.nextra)

      else:     # bad, naughty version!
         log.error('** HELLO string trailer is not magic, want %s but have %s' \
                % (data_to_hex_str(g_magic_hi), data_to_hex_str(odata)))
         return 1

      # todo - show_time()
      return 0



   def wait_for_socket(self):

      """wait for a talk request from the AFNI real-time plugin

         client should send magic_hi string"""

      global g_start_time

      self.data_sock, self.data_address = self.server_sock.accept()

      if self.data_sock is None:
         log.error('** failed accept(), closing and restarting...')
         return 1

      hinfo = list(self.data_address)
      log.info('connection established from host %s on port %d' %
               (hinfo[0], hinfo[1]))

      if self.read_magic_hi():
         log.error('** failed read magic_hi, closing and restarting...')
         return 1

      self.nconnects += 1              # we have a connection
      log.debug('-- valid socket for run %d' % self.nconnects)
      g_start_time = time.time()       # call this the beginning of run

      return 0



   def display_run_time(self, tr=-1):

      """display time offset from beginning of current run"""

      global g_start_time

      if tr < 0:
         tr = len(self.motion[0]) - 1
      offtime = time.time() - g_start_time
      tstr = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())

      log.info('-- comm time for TR %d @ %s (offset %.3f)' % (tr, tstr, offtime))



   def display_TR_data(self, tr=-1):

      """display motion and any extras for the given TR (last if not set)"""

      if tr < 0:
            tr = len(self.motion[0]) - 1

      log.debug('-- displaying data for TR %d' % tr)

      mprefix = "++ recv motion:     "
      if self.version == 1:
         eprefix = "++ recv %d extras:   " % self.nextra
      elif self.version == 2:
         eprefix = "++ recv %dx8 extras: " % (self.nextra // 8)

      log.info(float_list_string([self.motion[i][tr] for i in range(6)],
                                 nchar=9, ndec=5, nspaces=2, mesg=mprefix, left=1))

      # version 1, all on one line
      if self.version == 1 and self.nextra > 0:
         log.info(gen_float_list_string([self.extras[i][tr] for i in
                                        range(self.nextra)], mesg=eprefix, nchar=10, left=1))
      # version 2, each voxel on one line
      elif self.version == 2 and self.nextra > 0:
         elen = len(eprefix) + 1
         log.info("%s %s" % (eprefix, gen_float_list_string([self.extras[i][tr]
                                                            for i in range(8)], mesg='', nchar=10, left=1)))
         for off in range(self.nextra // 8 - 1):
                log.info(gen_float_list_string([self.extras[8 * off + 8 + i][tr]
                                               for i in range(8)], mesg=' ' * elen, nchar=10, left=1))



   def socket_has_closed(self):

      """peek ahead for close message"""

      data = self.peek_at_next_bytes(g_magic_len)
      if not data:
         log.warning('** socket has gone dead, restarting...')
         return True

      odata = [ord(v) for v in data]
      log.debug('++ testing as magic_bye: %s' % data_to_hex_str(odata))

      # if not magic bye, return a negative
      for ind in range(g_magic_len - 1):
         if odata[ind] != g_magic_bye[ind]:
            return False

      log.info('++ found close request for run %d, TRs = %d' %
               (self.nconnects, self.nread))

      return True



   def read_TR_data(self):

      """read in motion and extras for the current TR, if show_data, do so"""

      if self.socket_has_closed():
         return None, None

      # read and append motion values
      motion = self.read_floats_from_socket(6)
      if not motion:
         log.error('** read socket error, abrupt close: run %d, TRs %d' %
                   (self.nconnects, self.nread))
         return None, None
      for ind in range(6):
         self.motion[ind].append(motion[ind])

      # read and append extra values
      extra = None
      if self.nextra > 0:
         extra = self.read_floats_from_socket(self.nextra)
         if not extra:
            log.error('** failed to read %d extras for TR %d' %
                      (self.nextra, self.nread + 1))
            return None, None
         for ind in range(self.nextra):
            self.extras[ind].append(extra[ind])

      # possibly display TR data
      if self.show_times:
         self.display_run_time(self.nread)
      if self.show_data:
         self.display_TR_data(self.nread)

      self.nread += 1

      return motion, extra



   def read_all_socket_data(self):

      # initialize lists
      self.motion = [[] for i in range(6)]            # array of 6 lists
      self.extras = [[] for i in range(self.nextra)]  # array of nextra lists

      while True:
         motion, extra = self.read_TR_data()
         if not motion:
            break   # for good or ill, we are done
         pass       # PROCESS DATA HERE

      log.info('-- processed %d TRs of data' % self.nread)
      log.info('-' * 60)

      return 0



   def wait_for_new_run(self):

      log.info('++ waiting for run %d...' % (self.nconnects + 1))

      # reset variables that vary per run
      self.reset()

      # wait for the real-time plugin to talk to us
      if self.wait_for_socket():
         return 1

      # initialize lists
      self.motion = [[] for i in range(6)]              # array of 6 lists
      self.extras = [[] for i in range(self.nextra)]    # array of nextra lists

      return 0



   def close_data_ports(self):

      if self.data_sock is not None:
         try:
            self.data_sock.close()
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            pass
         self.data_sock = None

      log.debug('-- socket has been closed')



class SerialInterface(object):

   """interface class to deal with serial port information"""

   def __init__(self, sport):

      self.port_file = sport   # file for serial port
      self.data_port = None    # serial data port
      self.swap      = False   # whether to swap serial bytes

      log.info('++ initializing serial interface %s...' % sport)



   def __del__(self):

      self.close_data_ports()



   def open_data_port(self):

      if not self.port_file:
         log.error('** no file to open as serial port')
         return 1

      log.debug('-- opening serial port' + self.port_file)

      # open port_file at baud 9600, 8 bit N parity, 1 stop bit
      try:
         port = serial.Serial()
         port.setPort(self.port_file)
         port.setBaudrate(9600)
         port.setByteSize(serial.EIGHTBITS)
         port.setParity(serial.PARITY_NONE)
         port.setStopbits(serial.STOPBITS_ONE)
         port.setXonXoff(0)             # enable software flow control
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         log.error(sys.exc_info()[1])
         log.error('** failed to initialize serial port %s' % self.port_file)
         return 1

      try:
         port.open()
      except(socket.error, socket.herror, socket.gaierror, socket.timeout):
         log.error(sys.exc_info()[1])
         log.error('** failed to open serial port %s' % self.port_file)
         return 1

      self.data_port = port
      log.debug('++ serial port %s is open' % self.port_file)
      return 0



   def close_data_ports(self):

      if self.data_port:
         try:
            self.data_port.close()
         except(socket.error, socket.herror, socket.gaierror, socket.timeout):
            pass
         self.data_port = None

      log.debug('-- serial port has been closed')
      return 0



   def write_4byte_data(self, data):

      """write all floats/ints to the serial port"""

      if not self.data_port:
         return
      if not self.data_port.isOpen():
         return

      # TODO: might be a bit too verbose
      log.debug('++ writing data to serial port: %s' % data)

      dstring = struct.pack('f' * len(data), *data)
      if self.swap:
         swap4(dstring)

      # Debug output
      # log.debug('++ hex data to serial port:' +
      #     data_to_hex_str([ord(v) for v in dstring]))

      self.data_port.write(dstring)
      del(dstring)
      return 0



def data_to_hex_str(data):

   """convert raw data to hex string in groups of 4 bytes"""

   if not data:
      return ''

   dlen = len(data)             # total length in bytes
   groups = (dlen + 3) // 4       # number of 4-byte blocks to create
   remain = dlen
   retstr = ''  # return string

   for group in range(groups):
      if group > 0:
         retstr += ' '
      retstr += '0x'
      if remain >= 4:
         llen = 4
      else:
         llen = remain

      for ind in range(llen):
         retstr += '%02x' % data[dlen - remain + ind]

      remain -= llen

   return retstr



def swap4(data):

   """swap data elements in groups of 4"""

   size = 4
   nsets = len(data) // size
   if nsets <= 0:
      return

   for ind in range(nsets):
      off = ind * size
      v = data[off]     # swap d[0] and d[3]
      data[off] = data[off + 3]
      data[off + 3] = v
      v = data[off + 1]   # swap d[1] and d[2]
      data[off + 1] = data[off + 2]
      data[off + 2] = v



def float_list_string(vals, nchar=7, ndec=3, nspaces=2, mesg='', left=False):

   """return a string to display the floats:
        vals    : the list of float values
        nchar   : [7] number of characters to display per float
        ndec    : [3] number of decimal places to print to
        nspaces : [2] number of spaces between each float
   """

   if left:
      format = '%-*.*f%*s'
   else:
      format = '%*.*f%*s'

   istr = mesg
   for val in vals:
      istr += format % (nchar, ndec, val, nspaces, '')

   return istr



def gen_float_list_string(vals, mesg='', nchar=0, left=False):

   """mesg is printed first, if nchar>0, it is min char width"""

   istr = mesg

   if left:
      format = '%-'
   else:
      format = '%'

   if nchar > 0:
      format += '*g '
      for val in vals:
         istr += format % (nchar, val)
   else:
      format += 'g '
      for val in vals:
         istr += format % val

   return istr

