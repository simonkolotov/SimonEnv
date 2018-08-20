#!/usr/bin/python

import sys,pickle
import pandas as pd

pd.set_option('display.max_columns', 9999)
pd.set_option('display.width', 999999)
print pickle.load(open(sys.argv[1]))