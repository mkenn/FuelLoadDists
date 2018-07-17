#-------------------------------------------------------------------------------
# Name:        tester.py
# Purpose:
#
# Author:      kjells
#
# Created:     19/05/2011
# Copyright:   (c) kjells 2011
#-------------------------------------------------------------------------------
#!/usr/bin/env python
import csv
import sys
from ulp import WithinThisManyULP, HowManyULP
import decimal as dec
import re
import math

def isNumber(s):
    result = False
    try:
        converted = float(s)
        if converted != float('inf') and not math.isnan(converted):
            result =  True
    except ValueError:
        pass
    return result

class DataObj(object):
    """ Read a csv file and create a map keyed by lines and columns
        Allow for 2 of these objects to be compared."""
    def __init__(self, filename, console=True):
        self._filename= filename
        self._debug= False
        self._map = dict()
        self._cols= []
        self._console_output = console
        self.ReadFile()

    def ReadFile(self):
        """ Read in the file and create a map using the fuelbed number
            as the key."""
        reader = csv.DictReader(open(self._filename, 'r'), delimiter=',', quotechar='|')
        self._cols = reader.fieldnames
        for row in reader:
            self._map[row['fuelbeds']] = row

    def GetTolerance(self, number):
        if number < 0.01:
            return 1000000000
        elif number < 0.1:
            return 100000000
        else:
            check = number * dec.Decimal(0.05)
            dist = HowManyULP(number, number + check)
            return dist

    def CompareItems(self, a, b, key, column):
        """ Convert strings to Decimals and truncate sensibly. Compare with
            the specified tolerance factor """
        a = a if a else '0'
        b = b if b else '0'
        compare = True
        PLACES = dec.Decimal('0.0001')
        if isNumber(a) and isNumber(b):
            aa = dec.Decimal(a.lstrip('-')).quantize(PLACES)
            bb = dec.Decimal(b.lstrip('-')).quantize(PLACES)
            tolerance = self.GetTolerance(max(aa, bb))
            if not WithinThisManyULP(aa, bb, tolerance):
                if self._console_output:
                    print("{}:\t{}\t:\t{}\t:\t{}".format(key, column, aa, bb))
                compare = False
            else:
                if self._debug:
                    print("Good - {} : {} : {} : {}".format(key, column, aa, bb))
        elif(self._debug):
            print("Not compared {} : {}".format(a, b))
        return compare

    def CheckColumns(self, a, b):
        """ Primarily for ensuring that we are covering everything. May be
            eliminated as the testing process matures. """
        setA = set(a)
        setB = set(b)
        common = sorted(list(setA & setB))
        difference = sorted(list(setB ^ setA))
        diffMinusMarkerCols = []
        for i in difference:
            ### - eliminate 'marker' columns
            if not re.search('^[A-Z].*$', i.strip()):
                diffMinusMarkerCols.append(i)
        if self._console_output:
            print("\nColumns checked:\n\t{}\n".format("\n\t".join(common)))
            print("Columns not checked:\n\t{}\n".format("\n\t".join(diffMinusMarkerCols)))

    def GetCommonKeys(self, keysA, keysB):
        aa = set(keysA)
        bb = set(keysB)
        commonKeys = []
        for key in (aa & bb):
            commonKeys.append(key)
        return sorted(commonKeys)

    def Compare(self, other, debug=False):
        failures = 0
        comparisons = 0
        self._debug = debug
        self.CheckColumns(self._cols, other._cols)
        commonKeys = self.GetCommonKeys(self._map.keys(), other._map.keys())
        for key in commonKeys:
            key = str(key)
            for col in self._cols:
                if col in other._map[key]:
                    check = self.CompareItems(
                        self._map[key][col], other._map[key][col], key, col)
                    comparisons += 1
                    failures += 1 if not check else 0
        if self._console_output:
            print("\n{} comparisons {} failures".format(comparisons, failures))
            print("Left value = {}   Right values = {}".format(self._filename, other._filename))
            
        if len(commonKeys) != len(self._map.keys()):
            failures = 1
            
        return failures, comparisons

def main():
    retval = 1
    if len(sys.argv) > 2:
        one = DataObj(sys.argv[1])
        two = DataObj(sys.argv[2])
        retval, _ = one.Compare(two)
    else:
        print("\nError: Please specify the two files to compare.\n")
    exit(retval)

if __name__ == '__main__':
    main()
