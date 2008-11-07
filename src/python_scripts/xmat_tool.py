#!/usr/bin/env python

# basically everything is in ui_xmat.py and gui_xmat.py
#
# see "xmat_tool.py -help"

import sys
import ui_xmat as UIX

def test(test_gui=1):

   if test_gui:
      gui = XmatGUI()
      gui.init_gui(verb=1)
      gui.MainLoop()
   else:
      UIX.test()

   return None

def main():
   # test(1)

   XM = UIX.XmatInterface()
   if not XM: return 1

   if XM.use_gui:
      import gui_xmat
      gui = gui_xmat.XmatGUI()
      if not gui: return 1

      gui.init_gui(XM)
      gui.Gframe.apply_options()
      gui.MainLoop()

   return XM.status

if __name__ == '__main__':
   sys.exit(main())

