# emacs: -*- mode: python-mode; py-indent-offset: 4; indent-tabs-mode: nil -*-
# vi: set ft=python sts=4 ts=4 sw=4 et:
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
#
#   See COPYING file distributed along with the NiBabel package for the
#   copyright and license terms.
#
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
""" Defaults for images and headers

error_level is the problem level (see BatteryRunners) at which an error will be
raised, by the batteryrunners ``log_raise`` method.  Thus a level of 0 will
result in an error for any problem at all, and a level of 50 will mean no errors
will be raised (unless someone's put some strange problem_level > 50 code in).

``logger`` is the default logger (python log instance)

To set the log level (log message appears for problem of level >= log level),
use e.g. ``logger.level = 40``.

As for most loggers, if ``logger.level == 0`` then a default log level is used -
use ``logger.getEffectiveLevel()`` to see what that default is.

Use ``logger.level = 1`` to see all messages.
"""

import logging

error_level = 40
logger = logging.getLogger('nibabel.global')
logger.addHandler(logging.StreamHandler())
