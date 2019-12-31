
import sys

class DemoExperiment(object):

   def __init__(self):

      print ("Object startup")

      baseSystem = sys.platform

      print ("Reported operating system is: %s", baseSystem)



   if 'linux' in sys.platform.lower():

      def printos (self):

         print ("Printing OS for linux")



   elif 'window' in sys.platform.lower():

      def printos (self):

         print ("Printing OS for Window")



   else:

      def printos (self):

         print ("Printing OS for Mac?")



def main ():

   objInstantiated = DemoExperiment()

   objInstantiated.printos()



if __name__ == '__main__':
   sys.exit(main())

