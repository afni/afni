
import sys
import signal
import realtime as rt
import logging
import platform
# import datetime

log = logging.getLogger(__name__)
log.setLevel(logging.CRITICAL)
# dateTimeString = datetime.datetime.now().strftime("-%Y-%m-%d-%H-%M-%S")
# logging.basicConfig(filename='receiver' + dateTimeString + '.log')


class ReceiverInterface(object):

   """User-friendly interface to AFNI real-time receiver."""



   def __init__(self, port=None, swap=False, show_data=False, serial_port=None):

      # real-time interface RTInterface
      if port:
         self.RTI = rt.RTInterface(port=port)
      else:
         self.RTI = rt.RTInterface()
      if not self.RTI:
         return None

      self.RTI.show_data = show_data
      self.RTI.swap = swap

      # serial port interface
      if serial_port:
         self.SER = rt.SerialInterface(serial_port)
      else:
         self.SER = None

      # callbacks
      self.compute_TR_data = None



   def __del__(self):

      self.close_data_ports()



   def close_data_ports(self):

      """close TCP and socket ports, except for server port"""

      if self.RTI:
         self.RTI.close_data_ports()
      if self.SER:
         self.SER.close_data_ports()



   def set_signal_handlers(self):

      """capture common termination signals, to properly close ports"""

      log.info('++ setting signals')

      # there was an issue on windows system where it wasn't able to handle some
      # of the termination signals so the nf errored out added code to have
      # termination signals looked for dependent on type of system (windows or posix). -SJF

      if platform.system() == 'Windows':
         slist = [signal.SIGABRT,signal.SIGTERM]
      else: slist = [signal.SIGHUP, signal.SIGINT, signal.SIGQUIT, signal.SIGTERM]
      log.debug('   signals are %s' % slist)

      for sig in slist:
         signal.signal(sig, self.__exit_with_cleanup)



   def process_one_TR(self):

      """return 0 to continue, 1 on valid termination, -1 on error"""

      log.info("++ Entering process_one_TR()")

      motion, extra = self.RTI.read_TR_data()
      if not motion:
         log.error('** process 1 TR: read data failure')
         return 1

      log.debug("Motion: %s, Extra: %s" % (motion, extra))
      # if callback is registered
      data = None
      if self.compute_TR_data:
         data = self.compute_TR_data(motion, extra)  # PROCESS DATA HERE

      log.debug("Result: %s" % data)

      if not data:
         return 1

      if self.SER:
         self.SER.write_4byte_data(data)

      return 0



   def process_one_run(self):

      """repeatedly: process all incoming data for a single run
         return  0 on success and 1 on error
      """

      log.info("++ Entering process_one_run()")

      # wait for the real-time plugin to talk to us
      if self.RTI.wait_for_new_run():
         return 1

      # possibly open a serial port
      if self.SER:
         if self.SER.open_data_port():
            return 1

      # process one TR at a time until
      log.info('-- incoming data')

      rv = self.process_one_TR()
      while rv == 0:
         rv = self.process_one_TR()
      log.info("++ The life of this program is coming to an end....")

      if rv > 0:
         tail = '(terminating on success)'
      else:
         tail = '(terminating on error)'
      log.info('-- processed %d TRs of data %s' % (self.RTI.nread, tail))
      log.info('-' * 60)

      return rv



   def __exit_with_cleanup(self, signum, frame):

      log.info('++ signal handler called with signal %d' % signum)

      self.RTI.close_data_ports()

      # at last, close server port
      if self.RTI.server_sock:
         log.info('closing server port...')
         try:
            self.RTI.server_sock.close()
         except (rt.socket.error, rt.socket.timeout):
            pass

      log.info('-- exiting on signal %d...' % signum)
      sys.exit(signum)

