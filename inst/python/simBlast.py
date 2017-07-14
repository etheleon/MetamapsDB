#!/usr/bin/env python
import re
import os
import argparse


from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord
from Bio.Alphabet import generic_dna
import Bio.pairwise2 as pw
#from Bio.Blast.Applications import NcbiblastxCommandline

import pandas as pd
from IPython.core.debugger import Tracer
import logging

logging.basicConfig(level=logging.INFO)

class Contigorigins:
    '''
    Method 1:
    Tells you which contigs are actually together based on their blast by mapping the genes back to the their origin KO on genome
    Method 2:
    Identify Redundant Contigs (this is done via R)
    '''
    def __init__(self, root, koi, outputDir, query, rangesFile = 'out/template.csv', genomesFile = "out/sim.0301.combined.fna"):
        '''
        Args:
            root (str): root directory
            koi (str): the ko of interest
            query (str): path to the fastaFiles
            genomesFile (str): path to fastA file containing concatenated genomes
            rangesFile (str): path to koi range
        '''
        self.root        = root
        self.koi         = koi
        self.outputDir   = "{}/{}/{}".format(self.root, outputDir, self.koi)
        self.query       = query
        self.rangesFile  = rangesFile
        self.genomesFile = genomesFile
        self.kois        = self.outputDir + "/justGene.fna"
        self.blastdb     = self.outputDir + "/justGeneDB"
        self.blastOUTPUT = self.outputDir + "/blast.tsv"
        self.blastOUTPUTFull = self.outputDir + "/blastFull.txt"
        logging.info(koi)
        logging.info(root)

    def doMapping(self):
        os.makedirs(self.outputDir, exist_ok=True)
        self._makedb()
        self._blastMapping()

    def mdrMapping(self, mdrDir = '/w/simulation_fr_the_beginning/reAssemble/everybodyelse/out/pAss11'):
        '''
        Some error from the alignment process
        '''
        file = "{}/{}.fna".format(mdrDir, self.koi)
        contigs = SeqIO.to_dict(SeqIO.parse(self.query, "fasta"))
        mdrs = SeqIO.to_dict(SeqIO.parse(file, "fasta"))
        #this is already in the description
        mdrInContig = []
        alignment = {}
        for contigID in mdrs:
            contig = str(contigs[contigID].seq).upper()
            mdr = str(mdrs[contigID].seq).upper()
            revcom = str(mdrs[contigID].seq.reverse_complement()).upper()
            #damn sian, i'm not getting paid
            norm = re.search("\(ntRev\)", mdrs[contigID].description)
            if norm != None:
                seq = mdr
                sign = '+'
                logging.debug("norm {}".format(contigID))
            else:
                seq = revcom
                sign = '-'
                logging.debug("revcom {}".format(contigID))
            if seq in contig:
                start = contig.find(seq)
                nGapsS = 0
                nGapsQ = 0
            else:
                logging.info("Complete alignment fail : {}".format(contigID))
                alignments = [a for a in pw.align.globalms(contig, seq,  5, -4, -10, -1)]
                alignment[contigID] = alignments[0]
                start = abs(len(re.sub("^-+", "", alignments[0][1])) - len(alignments[0][1]))
                #Tracer('Linux')()
                nGapsS = re.sub("-+$", "", re.sub("^-+", "", alignments[0][0])).count("-")
                nGapsQ = re.sub("-+$", "", re.sub("^-+", "", alignments[0][1])).count("-")
            df = pd.DataFrame({
                'contig': contigID,
                'start': start,
                'end' : start + len(seq),
                'length': len(seq),
                'strand': sign,
                'sGaps': nGapsS,
                'qGaps': nGapsQ,
            }, index=[1])
            mdrInContig.append(df)
        df = pd.concat(mdrInContig, ignore_index=True)
        df['ko'] = self.koi
        return {'df':df, 'alignments': alignment}

    def _parseTitle(self,seqrecord, rangeDF, koi):
        '''
        parseTitle extracts just the section in the genome which corresponds with the ko of interest
        Args:
            seqrecord (int):    .
            rangeDF (str):     .
            koi (str):     .
        Returns:
            bool: The return value. True for success, False otherwise.
        '''
        genusID = int(re.search("^taxid\|(\d+)|", seqrecord.id).group(1))
        roi = rangeDF[rangeDF['seqnames'] == genusID]
        if len(roi) > 0:
            count = 0
            rows = []
            for index, row in roi.iterrows():
                count = count + 1
                goi = str(seqrecord.seq)[int(row["start"]): int(row["end"])]
                seqid = "%s|%s" % (seqrecord.id, count)
                thedescription = "%s - %s" % (row["start"], row["end"])
                justGene = SeqRecord(Seq(goi, generic_dna), id = seqid, name=koi, description = thedescription)
                rows.append(justGene)
            logging.info("Taxa %s has gene: %s" % (genusID, koi))
            return rows
        else:
            logging.info("%s has no such gene: %s" % (genusID, koi))
            pass

    def _makedb(self):
        '''
        Extracts KOI in simulated genomes
        '''
        #rangeDF = pd.read_table(outputDir+ "/scg.0200.genusnrange.txt")
        logging.info("Preparing blastnDB for {} based on simulatedDataset".format(self.koi))
        rangeDF = pd.read_csv(self.rangesFile)
        rangeDF_ko = rangeDF.loc[(rangeDF['ko'] == re.sub(r'^(ko:)*', 'ko:', self.koi))]
        soi = []
        with open(self.genomesFile, "r") as handle:
            for record in SeqIO.parse(handle, "fasta"):
                returned = self._parseTitle(record, rangeDF_ko, self.koi)
                if returned is not None:
                    for ele in returned:
                        soi.append(ele)
            SeqIO.write(soi, "{}/justGene.fna".format(self.outputDir), "fasta")
        os.system("makeblastdb -dbtype nucl -in {} -out {}  > /dev/null 2>&1".format(self.kois, self.blastdb))

    def _blastMapping(self, tabular=True):
        '''
        Blasts contigs againsts the cut out genes
        '''
        #blastOUTPUTfull= outputDir + "/round1/blastfull.txt"
        preCMD = "blastn -db {} -query {} -evalue 1e-5 -max_target_seqs 20".format(self.blastdb, self.query)
        if(tabular):
            cmd = "{} -out {} -outfmt 6".format(preCMD, self.blastOUTPUT)
        else:
            cmd = "{} -out {}".format(preCMD, self.blastOUTPUTFull)
        logging.info("Runnding this command: {}".format(cmd))
        os.system("{} > /dev/null 2>&1".format(cmd))




if __name__ == "__main__":
    from simBlast import Contigorigins
    parser = argparse.ArgumentParser(description="BLASTN contigs against simulation's genomes")
    parser.add_argument('root', metavar = 'root', type=str, help="path to the root directory of the project")
    parser.add_argument('koi', metavar = 'ko', type=str, help="the ko of interest")
    parser.add_argument('outputDir', metavar = 'output', type=str, help="")
    parser.add_argument('query', metavar = 'query', type=str, help="path to query ie. the contigs")
    parser.add_argument('rangeFile', metavar = 'range', type=str, help="ranges information")
    parser.add_argument('genomes', metavar = 'genomes', type=str, help="")
    args = parser.parse_args()

    contig = simBlast.Contigorigins(args.root, args.koi, args.outputDir, args.query)
    mappingDF = contig.doMapping()

'''

contig = Contigorigins(
    root ='/w/simulation_fr_the_beginning/reAssemble/everybodyelse/',
    koi ='K00927',
    outputDir = 'out/simBlast',
    query = '/w/simulation_fr_the_beginning/reAssemble/everybodyelse/out/newbler/K00927/454AllContigs.fna',
    rangesFile = '/w/simulation_fr_the_beginning/out/template.csv',
    genomesFile = '/w/simulation_fr_the_beginning/out/sim.0301.combined.fna'
)

contig.doMapping()

    colnames = ["query","subject","perc.identity","alignment.length","mismatches","gap.openings","q.start","q.end","s.start","s.end","evalue","bitScore"]
    blastDF = pd.read_table("/w/simulation_fr_the_beginning/reAssemble/everybodyelse/out/simBlast/K00927/blast.tsv", header=None, names=colnames)
    blastDF.loc[blastDF['perc.identity'] == 100]
'''

