import pandas as pd
import numpy as np
import re

plastic = pd.read_csv('data/plastic.csv')

rows = []
with open('data/plastic.csv', 'r') as fh:
    for line in fh:
        rows.append(line)
 #   words = fh.read()

plastic_csv = words.split(',')

keys = plastic_csv[:4]

nkeys = []
for key in keys:
    nstr = ''
    for letter in key:
        if re.search('[A-Za-z]', letter):
            nstr += letter
    nkeys.append(nstr)

all_rows = []
idx = 1
row = {nkeys[0] : re.sub('[a-zA-z]','', plastic_csv[4])}
for cell in plastic_csv[5:]:
    if cell[3] == 'T':
        row[nkeys[3]] = re.sub('[0-9.]', '', cell)
        all_rows.append(row)
        idx = 1
        row = {nkeys[0] : re.sub('[a-zA-z]','', cell)}
    else:
        row[nkeys[idx]] = cell
        idx += 1

all_data = pd.DataFrame(all_rows)
all_data.iloc[0,3] = 47.94692945

all_data.to_csv('data/plastic_fixed.csv')
