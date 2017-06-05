#!/usr/bin/env python

import sys
import pandas as pd
import sqlite3
import argparse
from signal import signal, SIGPIPE, SIG_DFL
signal(SIGPIPE,SIG_DFL)

parser = argparse.ArgumentParser(description='Grep all contig\'s blastx hits & taxonomic assignments')
parser.add_argument('blastTab'  , metavar='diamondOutput.m8'  , help='blastTab format of the results from a diamond run')
parser.add_argument('--koi'     , metavar=('K00001', 'K00002') , help='vector of kos which you are interested in'          , nargs='+', default = ["K00001"])
parser.add_argument('--sqlite3' , metavar='sq3.db' , help='path to where the gi_taxid_prot_sqlite3 is'         , default='/export2/home/uesu/db/taxonomy_12sept2014/gi_taxid_prot_sqlite3')
args = parser.parse_args()

conn    = sqlite3.connect(args.sqlite3)
c = conn.cursor()
#df      = pd.read_csv(args.blastTab,sep='\t',header=None, nrows=1000)
df      = pd.read_csv(args.blastTab,sep='\t',header=None)

df2         = df.ix[:,[0,1]] #just (1) contigs|ko and (2) hit
df2.columns = ['query', 'subject']
df2         = df2[df2['subject'].str.contains("ref")]

df2['contig'], df2['ko'] = zip(*df2['query']
   .apply(lambda x: x.split('|', 1)))
df2['gi'] = [row[1] for row in df2['subject']
   .apply(lambda x: x.split('|',4))]

df3 = df2[df2['ko'].isin(args.koi)]

print "\t".join(['ko', 'contig', 'gi', 'taxid'])

for k in args.koi:
    for index, row in df3[df3['ko'] == k].iterrows():
        c.execute("SELECT * FROM gi_taxid WHERE gi =?", (row['gi'], ))
        line = "\t".join([k, df3['contig'][index]]+[str(item) for item in c.fetchone()])
        sys.stdout.write(line+'\n')
#for i, row in df3.iterrows():
    #c.execute("SELECT * FROM gi_taxid WHERE gi =?", (row['gi'], ))
    #print "\t".join([str(item) for item in c.fetchone()])

conn.close()
