FROM jupyter/r-notebook
LABEL maintainer="Wesley GOI"

USER root
RUN ln -s /bin/tar /bin/gtar

USER jovyan
RUN conda install \
    r-tidyr \
    r-httr \
    r-devtools

RUN conda install \
    -c conda-forge \
    -c bioconda \
    bioconductor-shortread=1.40.0

RUN conda install libssh2 krb5

RUN Rscript -e 'install.packages("BiocManager", repos="https://cloud.r-project.org/", quiet=T)' \
    Rscript -e 'BiocManager::install("GenomeInfoDb", version = "3.8")'

COPY . /MetamapsDB/

RUN Rscript -e 'devtools::install_deps("/MetamapsDB/", dependencies=T, upgrade=T)'

RUN Rscript -e 'install.packages("gridExtra", repos="http://cran.us.r-project.org")'

RUN Rscript -e 'devtools::install("/MetamapsDB")'
