
import logging

# Set default logging handler to avoid "No handler found" warnings.
# from requests package
try: # Python 2.7+
   from logging import NullHandler
except ImportError:
   class NullHandler(logging.Handler):
      def emit(self, record):
         pass



logging.getLogger(__name__).addHandler(NullHandler())



# from urllib3 package
def add_stderr_logger(level=logging.DEBUG):

   """
   Helper for quickly adding a StreamHandler to the logger. Useful for
   debugging.

   Returns the handler after adding it.
   """

   # This method needs to be in this __init__.py to get the __name__ correct
   # even if urllib3 is vendored within another package.

   logger = logging.getLogger(__name__)
   handler = logging.StreamHandler()
   handler.setFormatter(logging.Formatter('%(asctime)s %(levelname)s %(message)s'))
   logger.addHandler(handler)
   logger.setLevel(level)
   logger.debug('Added a stderr logging handler to logger: %s' % __name__)

   return handler



from .receiver import ReceiverInterface
from .experiment import Experiment

