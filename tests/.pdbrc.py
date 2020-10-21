"""
Configuration for pdb and pdbpp

"""
import pdb
import rlcompleter

# # save this in .pdbrc.py in your home directory
# def complete(self, text, state):
#     """
#     Return the next possible completion for text, using the current frame's
#     local namespace.

#     This is called successively with state == 0, 1, 2, ... until it returns
#     None.  The completion should begin with 'text'.

#     Reference: https://code.activestate.com/recipes/498182/
#     """
#     # Attached a completer class and make sure it uses the current local scope
#     if not hasattr(self, "completer"):
#         self.completer = rlcompleter.Completer(self.curframe.f_locals)
#     else:
#         self.completer.namespace = self.curframe.f_locals
#     return self.completer.complete(text, state)


# Only load pdbpp configuration if pdbpp is installed
if hasattr(pdb, "DefaultConfig"):

    class Config(pdb.DefaultConfig):
        """
        Configuration object for for pdbpp.
        https://pypi.org/project/pdbpp/#description
        """

        # Start sticky mode automatically when pdbpp is installed
        sticky_by_default = True

        # Colors: https://gist.github.com/chrisopedia/8754917#background
        current_line_color = 7
