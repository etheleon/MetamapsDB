# Resubmission

## Comments

> Thanks, please omit the redundant "R Package for" in your title.

Changed from "R Package for Querying Metagenomics Data Against Metabolic Pathways and Gene Centric Assemblies" to "R Interface with NEO4J based Graph Database for Network Analysis of Metabolic Pathways and Gene Centric Assemblies using Metagenomics Data"

> Please write database (db?).

Changed from "Essential functions for querying OMICS db and functional analyses, includes integration layer with neo4j graph database." to 
"Functions for path based queries against OMICS, a NEO4J database populated with data from metagenomic surveys and their accompanying metabolic and taxonomic annotations."

> Please add an URL for 'neo4j' graph database in your description text in the form <http:...> or <https:...> with angle brackets for auto-linking and no space after 'http:' and 'https:'. 
> Please write package names and software names in single quotes (e.g. 'neo4j').

Added into the submissions form

> Your examples are wrapped in \dontrun{}, hence nothing gets tested. Please unwrap the examples if that is feasible and if they can be executed in < 5 sec for each Rd file or create additionally small toy examples.

- Includes example for functions.
    - Following functions have toy examples
        - `contractMetab` (with example using included data)
        - `adjacentPairs` (with example using included data)
        - `ig2ggplot`
        - `prettifyGraph`

- if they can be executed in < 5 sec for each Rd
        - `taxnam.sql` is wrapped in \dontrun (First time running this function, the package will attempt to build download a file from (NCBI) National Center for Biotechnology Information's ftp and build a sqlite database)

    - Functions listed below are meant to interact with a running NEO4J database via its REST API, hence I've wrapped them in `\donttest`. [Official Driver](https://github.com/nicolewhite/RNeo4j) for     R's neo4j package also does this.
        - REST API Query Functions:
            1. koname
            2. cpdname
            3. contigInfo
            4. taxname
            5. taxnam.sql
            6. lookupTable
            7. path2kingdom
            8. buildTree
            9. grepgraph
            10. grepgraph.cpd
            11. surrNodes
            12. findTrio
            13. lca
            14. addContigProperty
            15. addKOProperty
            16. annotateContigs.taxonomy
            17. connect
            18. dbquery
            19. index
            110. listquery
