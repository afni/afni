#!/usr/bin/env python

# This is a reimplementation of the default real-time feedback experiment
# distributed with AFNI, implemented in realtime_receiver.py, using WX and
# Matplotlib for generating the GUI and plotting the results.
#
# This replaces the default GUI toolkit with PsychoPy, and will draw the
# same results and shapes to a PsychoPy window, in a manner synchronous
# with the old toolkit.
#
# This will serve as a basis or a template to build neuro-feedback type
# of experiment that can get data from AFNI (through the 'afniRTI' module,
# also distributed here).

import sys
import logging
from   optparse import OptionParser

import numpy as np

import afniRTI as nf

try:
   from   psychopy import visual, core # , sound
   psychopyInstalled = 1
except ImportError:
   psychopyInstalled = 0




class DemoExperiment(object):

   def __init__(self, options):

      self.TR_data      = []

      # dc_params = [P1, P2]
      # P1 = dr low limit, P2 = scalar -> [0,1]
      # result is (dr-P1)*P2  {applied in [0,1]}
      self.dc_params    = []

      self.show_data    = options.show_data

      print ("++ Initializing experiment stimuli")
      self.setupExperiment()



   def setupExperiment(self):

      """create the GUI for display of the demo data"""

      # self.exptWindow = visual.Window(fullscr=options.fullscreen, allowGUI=False)
      self.exptWindow = visual.Window([1280, 720], allowGUI=False)

      # For this demonstration experiement, set corners of the "active area" (where
      # we will "draw") to be a square in the middle of a 16:9 screen.
      self.nPlotPoints  = 10
      self.xMax         = 0.50625
      self.xMin         = self.xMax * -1.0
      self.xDelta       = (self.xMax - self.xMin) / (1.0 * self.nPlotPoints)
      self.yMax         = 0.9
      self.yMin         = self.yMax * -1.0
      self.yDelta       = (self.yMax - self.yMin) / (1.0 * self.nPlotPoints)

      # Now divide this area into a series of vertical rectangles that we will draw
      # to when we have results.
      self.stimAreaCorners    = [None] * self.nPlotPoints
      self.drawnCorners       = [None] * self.nPlotPoints

      for i in range(self.nPlotPoints):
         self.stimAreaCorners[i] = np.array ([[(self.xMin + (self.xDelta*(i+1))), self.yMin],
                                              [(self.xMin + (self.xDelta*(i+1))), self.yMin],
                                              [(self.xMin + (self.xDelta*(i+0))), self.yMin],
                                              [(self.xMin + (self.xDelta*(i+0))), self.yMin]])

         self.drawnCorners[i]    = self.stimAreaCorners[i]

         displayArea = visual.ShapeStim (self.exptWindow, vertices = self.stimAreaCorners[i],
                                         autoLog = False, fillColor = [1, 1, 1])

      self.exptWindow.flip()



   def runExperiment (self, data):

      """
         After data is received and processed by the 'compute_TR_data' routine,
         call this routine to update the display, or whatever stimulus is being
         generated for the experiment.  This update should be a consistent
         follow on to what was done in the 'setupExperiment' routine.
      """

      length = len(data)
      if length == 0:
         return

      if self.show_data:
         print('-- TR %d, demo value: %s' % (length, data[length - 1][0]))
      if True:
         if length > 10:
            bot = length - 10
         else:
            bot = 0
         pdata = [data[ind][0] for ind in range(bot, length)]

         # To update the rectangles to be drawn, with the results of the stimulus modeling, add
         # the new data to the base shapes (using the simple element-by-element addition done
         # by numpy's matrix opertions). Also, update the display area as every shape is updated
         # to avoid drawing artifacts, where vertices get incorrectly assigned to the area to be
         # drawn.
         for i in range(self.nPlotPoints):
            if (len(data) - 1 - i) > 0:
               plotIndex = self.nPlotPoints - 1 - i
               self.drawnCorners[plotIndex] = np.array ([
                                                 [0.0, (self.yDelta * data[len(data) - 1 - i][0])],
                                                 [0.0, 0.0],
                                                 [0.0, 0.0],
                                                 [0.0, (self.yDelta * data[len(data) - 1 - i][0])]
                                                       ]) + self.stimAreaCorners[plotIndex]

               displayArea = visual.ShapeStim (self.exptWindow, vertices = self.drawnCorners[plotIndex],
                                         autoLog = False, fillColor = [-1, -1, 1])
               displayArea.draw()

         self.exptWindow.flip()



   def compute_TR_data(self, motion, extra):

      """If writing to the serial port, this is the main function to compute
      results from motion and/or extras for the current TR and
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

      print("++ Entering compute TR data")

      # # case 'motion': send all motion
      # if rec.data_choice == 'motion':
      #     if rti.nread > 0:
      #         return 0, [rti.motion[ind][rti.nread - 1] for ind in range(6)]
      #     else:
      #         return -1, []

      # # case 'motion_norm': send Euclidean norm of motion params
      # #                     --> sqrt(sum of squared motion params)
      # elif rec.data_choice == 'motion_norm':
      #     if rti.nread > 0:
      #         motion = [rti.motion[ind][rti.nread - 1] for ind in range(6)]
      #         return 0  # , [UTIL.euclidean_norm(motion)]
      #     else:
      #         return -1, []

      # # case 'all_extras': send all extra data
      # elif rec.data_choice == 'all_extras':
      #     if rti.nextra > 0:
      #         return 0, [rti.extras[i][rti.nread - 1] for i in range(rti.nextra)]
      #     else:
      #         return -1, []

      # # case 'diff_ratio': (a-b)/(abs(a)+abs(b))
      # elif rec.data_choice == 'diff_ratio':

      npairs = len(extra) // 2
      print(npairs)
      if npairs <= 0:
          print('** no pairs to compute diff_ratio from...')
          return None

      # modify extra array, setting the first half to diff_ratio
      for ind in range(npairs):
         a = extra[2 * ind]
         b = extra[2 * ind + 1]
         if a == 0 and b == 0:
            newval = 0.0
         else:
            newval = (a - b) / float(abs(a) + abs(b))

         # --------------------------------------------------------------
         # VERY data dependent: convert from diff_ratio to int in {0..10}
         # assume AFNI_data6 demo                             15 Jan
         # 2013

         # now scale [bot,inf) to {0..10}, where val>=top -> 10
         # AD6: min = -0.1717, mean = -0.1605, max = -0.1490

         bot = -0.17         # s620: bot = 0.008, scale = 43.5
         scale = 55.0        # =~ 1.0/(0.1717-0.149), rounded up
         if len(self.dc_params) == 2:
            bot = self.dc_params[0]
            scale = self.dc_params[1]

         val = newval - bot
         if val < 0.0:
            val = 0.0
         ival = int(10 * val * scale)
         if ival > 10:
            ival = 10

         extra[ind] = ival

         print('++ diff_ratio: ival = %d (from %s), (params = %s)' %
               (ival, newval, self.dc_params))

         # save data and process
         self.TR_data.append(extra[0:npairs])
         self.runExperiment(self.TR_data)

         return extra[0:npairs]    # return the partial list



def processExperimentOptions (self, options=None):

   """
       Process command line options for on-going experiment.
       Customize as needed for your own experiments.
   """

   usage = "%prog [options]"
   description = "AFNI real-time demo receiver with demo visualization."
   parser = OptionParser(usage=usage, description=description)

   parser.add_option("-d", "--debug", action="store_true",
            help="enable debugging output")
   parser.add_option("-v", "--verbose", action="store_true",
            help="enable verbose output")
   parser.add_option("-p", "--tcp_port", help="TCP port for incoming connections")
   parser.add_option("-S", "--show_data", action="store_true",
            help="display received data in terminal if this option is specified")
   parser.add_option("-w", "--swap", action="store_true",
            help="byte-swap numerical reads if set")
   parser.add_option("-f", "--fullscreen", action="store_true",
            help="run in fullscreen mode")

   return parser.parse_args(options)



def main():

   opts, args = processExperimentOptions(sys.argv)

   if (psychopyInstalled == 0):
      print("")
      print("  *** This program requires the PsychoPy module.")
      print("  *** PsychoPy was not found in the PYTHONPATH.")
      print("  *** Please install PsychoPy before trying to use")
      print("      this module.")
      print("")
      return -1

   if opts.verbose and not opts.debug:
      nf.add_stderr_logger(level=logging.INFO)
   elif opts.debug:
      nf.add_stderr_logger(level=logging.DEBUG)

   print("++ Starting Demo...")
   demo = DemoExperiment(opts)

   # create main interface
   receiver = nf.ReceiverInterface(port=opts.tcp_port, swap=opts.swap,
                                   show_data=opts.show_data)
   if not receiver:
      return 1

   # set signal handlers and look for data
   receiver.set_signal_handlers()  # require signal to exit

   # set receiver callback
   receiver.compute_TR_data = demo.compute_TR_data

   # prepare for incoming connections
   if receiver.RTI.open_incoming_socket():
      return 1

   rv = receiver.process_one_run()
   return rv



if __name__ == '__main__':
   sys.exit(main())

