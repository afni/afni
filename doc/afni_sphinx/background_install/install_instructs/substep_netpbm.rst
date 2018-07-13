
To install some useful image conversion and processing tools,
copy+paste these individually::

  bash
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < /dev/null 2> /dev/null

::
  brew install netpbm

::
  exit

**Purpose:** Netpbm has functionality for converting image formats
(such as to PNG) and is used in several programs like
``@snapshot_volreg``, ``@chauffeur_afni`` and others.
