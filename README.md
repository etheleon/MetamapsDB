MetamapsDB
========
[![DOI](https://zenodo.org/badge/19045/etheleon/metamaps.svg)](https://zenodo.org/badge/latestdoi/19045/etheleon/metamaps)
[![Build Status](https://travis-ci.org/etheleon/MetamapsDB.svg?branch=master)](https://travis-ci.org/etheleon/MetamapsDB)
[![Gitter](https://badges.gitter.im/metamaps.png)](https://gitter.im/etheleon/metamaps)

R package for querying integrated -omics database.

# Introduction

MetamapsDB is a R package used for interfacing with such a database for

1. Gene centric queries 
2. Analyses of Integrated^ Microbiome datasets

^ Genomic and Transcriptomic

It is the final step of the 5 preprocessing steps used in carrying out our gene centric pipeline. (Unpublished)

1. Annotation _DIAMOND_ - Labelling of short reads using, _blastX-like_ against NR protein database (more for functional)
2. Binning _MEGAN6 CE_ - Functional (KEGG) binning of NGS short reads based on labels
3. Bin-based Assembly _NEWBLER_ - Gene Centric OLC Assembly of functional bins / KEGG Orthologs
    * Annotation _DIAMOND_ - Labelling (round2) more for taxonomic annotation 
    * MEGAN6 CE - Taxonomic binning of contigs based on labels
4. Gene centric analyses _pAss_ -
    * Identify Maximum Diversity Region (MDR)
    * Remove known KOs which fail process
        * Diversity analysis (gene count)
            * 31 Single Copy Genes
        * ID genera which are indistinguishable due to sequence conservation
4. mapBlat - Maps (using BLAT) gDNA and rRNA short onto 
    * the contigs
    * just the MDR Region

# Mentioned Packages

## OMICS

Docker wrapper for generating a KEGG + Taxnoomy + Contig neo4j graph database

[![MetamapsDB](./thumbnail.png)](https://github.com/etheleon/omics).

# MapBloat

R package for mapping reads onto contigs/MDR using Blat

[mapBloat](https://github.com/etheleon/mapblat)

# Usage

Functions

| Function | Description |
| --- | --- |
| connect | Connects with Neo4J database |
| dbquery| Sends query to Neo4J database |
| koname | Takes ko id as input and returns ko details |
| taxnam.sql | Takes NCBI taxonomy id as input and returns ko details |
| contractMetab | Simplifies KEGG metabolic graph |
| igraph2gexf | Encodes Igraph into gexf format |
| sigmaGraph| Generates an interactive graph representation of a subnetwork in html using the `htmlwidgets` package|
| grepgraph | given a set of KOs get the subgrap of metabolism |
| grepgraph.cpd | given a set of CPDs get the subgrap of metabolism |
| annotateContigs.taxonomy | |
| buildE||
| buildTree||
| extractFromPath||
| findK||
| findSeeds||
| findTrios||
| findtype||
| getContigs||
| gi2rank||
| ig2ggvis||
| ksCal||
| lca||
| make.data.frame | Utily function dbquery might return data.frame where each column is a nested list. Converts lilst to dataframe|
| trio||
| trio.local||
