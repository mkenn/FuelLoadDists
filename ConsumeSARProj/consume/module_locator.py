import sys
import os

def we_are_frozen():
    # All of the modules are built-in to the interpreter, e.g., by py2exe
    return hasattr(sys, "frozen")

def module_path():
    encoding = sys.getfilesystemencoding()
    if we_are_frozen():
        frozen_dir = os.path.abspath(os.path.dirname(sys.executable))
        frozen_dir = os.path.join(frozen_dir, './consume')
        return frozen_dir
    else:
        regular_dir = os.path.abspath(os.path.dirname(__file__))
        return regular_dir