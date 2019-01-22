#!/usr/bin/env python

########################################################################
## 05/2018 Justin Rajendra
## do stuff with info from bids json file or any json file

## system libraries
import sys, os, glob, subprocess, csv, re, argparse, signal, textwrap, json
from  collections import OrderedDict
import afni_base

########################################################################
## functions and classes oh my?

##################################
## print in color
def printNcolor(message,message2="",color="0",bg="0",style="0"):
    """Print to terminal in some colors and style
       printNcolor("hello",color="White",bg="Blue",style="Bold")
       You can put 2 messages.  The first will have color, the second won't.
       Color options are below. And may be different on different systems.
       If the input colors are not matched in the dictionary, it will output
       as the standard.
    """

    ## add more colors later?
    color_dict = {"Black":"30","Red":"31","Green":"32","Yellow":"33",
                  "Blue":"34","Purple":"35","Cyan":"36","White":"37"}
    bg_dict = {"Black":"40","Red":"41","Green":"42","Yellow":"43",
               "Blue":"44","Purple":"45","Cyan":"46","White":"47"}
    style_dict = {"None":"0","Bold":"1","Underline":"2","Negative1":"3",
                  "Negative2":"5"}

    ## convert to strings
    color = str(color) ; style = str(style) ; bg = str(bg)

    ## look up or default to nothing
    color = color_dict.get(color,"0")
    bg = bg_dict.get(bg,"0")
    style = style_dict.get(style,"0")

    ## print to terminal
    print("\033["+style+";"+color+";"+bg+"m"+message+"\033[0;0m"+message2)


##################################
## check if a table is rectangular
def data_is_rect(mdata):
    """check if a table is rectangular
       return 1 for rect, 0 for not rect
    """
    if mdata == None: return 1
    if len(mdata) == 0: return 1
    rlen = len(mdata[0])
    for row in mdata:
        if len(row) != rlen: return 0
    return 1

##################################
## run some command or give error
def exec_or_error(cmd, error_msg="See above!!!", showerr=1, showcmd=1):
    """return text output from the given command
       if showcmd, print the cmd
       if showerr and there is a command error, show it
       This will exit on status other than 0 so BE CAREFULL!!!
    """

    status, cmd_output, error_out = afni_base.simple_shell_exec(cmd,capture=1)
    if showcmd:
        print(cmd)
    if showerr:
        print(error_out)
    if status != 0:
        if error_msg != "See above!!!":
            error_msg = error_msg+"!!!"
        printNcolor("** ERROR:"," "+error_msg,"Red","White","Bold")
        sys.exit(1)

    return status,cmd_output

##################################
def json_import(json_file):
    """ validate and load a json file, return dict"""
    if not os.path.isfile(json_file):
        print("\nError: "+os.path.basename(json_file)+ " not found!!\n")
        sys.exit(1)
    with open(json_file) as js:
        try:

            json_data = json.load(js,object_pairs_hook=OrderedDict)
        except:
            print("\nError: "+os.path.basename(json_file)+
                  " is not a valid json file!!\n")
            sys.exit(1)
    return json_data

##################################
class json_info(object):
    def __init__(self,name):
        if type(name) is not str:
            print("\nError: "+str(name)+ " must be of type 'str'!!\n")
            sys.exit(1)

        ## namey things
        self.name = name
        self.rel_path = os.path.dirname(name)
        self.full_path_name = os.path.abspath(name)
        self.full_path = os.path.dirname(self.full_path_name)
        self.full_path_prefix = os.path.splitext(self.full_path_name)[0]
        self.file_name = os.path.basename(name)
        self.prefix = os.path.splitext(self.file_name)[0]
        self.ext = os.path.splitext(self.file_name)[1]

        ## read in and get list of fields
        self.dict = json_import(name)
        self.fields = self.dict.keys()

        ## some useful fields
        if "EchoTime" in self.fields:
            self.te_sec = self.dict["EchoTime"]
            self.te = self.te_sec * 1000
        else:
            self.te_sec = None ; self.te = None
        if "RepetitionTime" in self.fields:
            self.tr = self.dict["RepetitionTime"]
        else:
            self.tr = ""
        if "SliceTiming" in self.fields:
            self.slice_list = self.dict["SliceTiming"]
            self.slice_str = ' '.join(map(str,self.slice_list))
        else:
            self.slice_list = None ; self.slice_str = None

        ## find matching nifti and return afni_name object
        if os.path.isfile(self.full_path_prefix+".nii.gz"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii.gz")
        elif os.path.isfile(self.full_path_prefix+".nii"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii")
        else:
            self.nii = afni_base.afni_name("None")

        return

    def field(self,field_in):
        """ check and return field_in in a json_info.dict"""
        if field_in not in self.dict:
            return None
        else:
            return self.dict[field_in]


##############################################################
## graveyard

## run some command or give error
# def exec_or_error(cmd_str,error_msg="ERROR!!!"):
#     """Execute subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
#        Return status and shell output.
#        On non 0 status print error_msg and exit.
#     """
#     print("")
#     print(cmd_str)
#     sys.stdout.flush()
#     p = subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
#     cmd_output = p.communicate()[0]
#     print("")
#     if p.returncode != 0:
#         print("\n"+error_msg+"\n\n"+cmd_output)
#         sys.exit(1)
#     return p.returncode,cmd_output