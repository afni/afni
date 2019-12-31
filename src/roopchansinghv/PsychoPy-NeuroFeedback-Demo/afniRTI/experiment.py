
from optparse import OptionParser
from . import add_stderr_logger, ReceiverInterface

class Experiment(object):

   def __init__(self, description):

      self.parser = OptionParser(usage="%prog [options]", description=description)
      self.parser.add_option("-d", "--debug", action="store_true",
             help="enable debugging output")
      self.parser.add_option("-v", "--verbose", action="store_true",
             help="enable verbose output")
      self.parser.add_option("-S", "--show_data", action="store_true",
             help="display received data in terminal if this option is specified")
      self.parser.add_option("-p", "--tcp_port",
             help="TCP port for incoming connections")
      self.parser.add_option("-w", "--swap", action="store_true",
             help="byte-swap numberical reads if set")



   def add_option(self, *args, **kwargs):

      self.parser.add_option(*args, **kwargs)



   def parse_args(self):

      opts, args = self.parser.parse_args()

      if opts.verbose and not opts.debug:
         add_stderr_logger(level=logging.INFO)
      elif opts.debug:
         add_stderr_logger(level=logging.DEBUG)

      self.tcp_port = opts.tcp_port
      self.swap = opts.swap
      self.show_data = opts.show_data
      return opts, args



   def run(self):

      # create main interface
      self.receiver = ReceiverInterface(port=self.tcp_port, swap=self.swap,
             show_data=self.show_data)
      if not self.receiver:
         return 1

      # set signal handlers and look for data
      self.receiver.set_signal_handlers()  # require signal to exit

      # set receiver callback (this will resolve to derived class' method)
      self.receiver.compute_TR_data = self.compute_TR_data

      # prepare for incoming connections
      if self.receiver.RTI.open_incoming_socket():
         return 1

      # process all incoming data for a single run
      rv = self.receiver.process_one_run()

      return rv



   def compute_TR_data(self, motion, extra):
      pass

