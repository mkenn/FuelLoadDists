#-------------------------------------------------------------------------------
# Name:        build.py
# Purpose:     Create the directory structure and copy files to make an installable
#               consume distribution
#
# Author:      kjells
#
# Created:     1/6/2012
#-------------------------------------------------------------------------------
import shutil
import os
import sys
from contextlib import contextmanager
from datetime import date

PKG_DIR = './batch'
PKG_SOURCE_DIR = PKG_DIR + '/consume'
PKG_DATAFILES_DIR = PKG_SOURCE_DIR + '/input_data'
REVISION_FILE = '../mercurial_rev.txt'
VERSION_FILE = '../consume/version.py'

def build_dirs():
    if os.path.exists(PKG_DIR):
        shutil.rmtree(PKG_DIR)
    for dir in [PKG_DIR, PKG_SOURCE_DIR, PKG_DATAFILES_DIR]:
        os.mkdir(dir)

def copy_files(file_list, dest):
    for file in file_list:
        #print("{}\t->\t{}".format(file, dest))
        shutil.copy(file, dest)

def copy_non_source():
    NON_SOURCE_FILES = [
        '../common_build/AUTHORS.txt',
        '../common_build/LICENSE.txt',
        '../common_build/README.txt',
    ]
    copy_files(NON_SOURCE_FILES, PKG_DIR)

def copy_consume_source():
    SOURCE_FILES = [
        '../consume/__init__.py',
        '../consume/con_calc_activity.py',
        '../consume/con_calc_natural.py',
        '../consume/data_desc.py',
        '../consume/emissions.py',
        '../consume/emissions_db.py',
        '../consume/fccs_db.py',
        '../consume/fuel_consumption.py',
        '../consume/input_variables.py',
        '../consume/module_locator.py',
        '../consume/util_consume.py',
        VERSION_FILE
    ]
    copy_files(SOURCE_FILES, PKG_SOURCE_DIR)
    all_py_files = [i for i in os.listdir('../consume') if i.endswith('.py')]
    for file in all_py_files:
        if file not in [i.split('/')[2] for i in SOURCE_FILES]:
            print("\n{} is not listed as a source file. It won't be included in the package.".format(file))

def copy_batch_source():
    BATCH_SOURCE_FILES = [
        '../cmdline.py',
        '../consume_batch.py',
        '../module_locator.py',
        '../custom_col.py'
    ]
    copy_files(BATCH_SOURCE_FILES, PKG_DIR)

def copy_datafiles():
    datafiles = ['../consume/input_data/' + i for i in os.listdir('../consume/input_data')]
    copy_files(datafiles, PKG_DATAFILES_DIR)

def build_version_file():
    with open(REVISION_FILE, 'r') as infile:
        revision = infile.readline().rstrip()
    with open(VERSION_FILE, 'w') as outfile:
        today = date.today()
        version = "    return \'Consume version 4.1 Revision {} Date {}\'\n".format(revision, today)
        outfile.write('def get_consume_version():\n')
        outfile.write(version)

def change_to_this():
    ''' Return the current working directory. Change to the root of
         the build_batch_zip directory
    '''
    here = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()
    os.chdir(here)
    return cwd

def cleanup(pop_to_directory):
    version_file = os.path.join('../{}'.format(PKG_DIR), VERSION_FILE)
    if os.path.exists(version_file):
        os.unlink(version_file)
    else:
        print("Error: unable to delete version file!!!")
        print(os.getcwd())
    os.chdir(pop_to_directory)

@contextmanager
def run_enclosed(setup, teardown):
    ''' Encapsulate directory changes
    '''
    cwd = setup()
    yield
    teardown(cwd)


#-------------------------------------------------------------------------------
# Start
#-------------------------------------------------------------------------------
def main():
    with run_enclosed(change_to_this, cleanup):
        build_dirs()
        build_version_file()
        copy_consume_source()
        copy_non_source()
        copy_datafiles()
        copy_batch_source()

if __name__ == '__main__':
    main()