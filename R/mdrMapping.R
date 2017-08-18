#' grepReads greps for cDNA or gDNA reads cause they are both grouped togehter in the assembly input
#'
#' @param reads the ShortRead class object
#' @param type the pattern to look for
grepReads = function(reads, type="cDNA")
{
    whichIsMRNA = ShortRead::id(reads) %>% as.character %>% grepl(type, .)
    r = reads[whichIsMRNA] %>% sread
    names(r) = ShortRead::id(reads[whichIsMRNA])
    r
}

#' mapContig finds the location of the MDR on the contig
#
#' @param contigid the id of the contig
#' @param seq character string of the contig from the MSA
#' @param s starting loc
#' @param e ending loc
mapContig = function(contigid, seq, s, e)
{
    mdr = seq %>% as.character %>% substr(s, e) %>% gsub("-", "", .)
    full = seq %>% gsub("-", "", .)
    matchStart = regexpr(mdr, full) %>% as.integer
    matchLength = regexpr(mdr, full) %>% attr("match.length")
    matchEnd = matchStart + matchLength
    data.frame(
        id = contigid,
        matchStart = matchStart,
        matchEnd = matchEnd
    )
}

#' Returns contig ranges for those captured within the MDR
#'
#' @param ko the KO of interest
#' @param passDir path to pAss outputs
#' @importFrom GenomicRanges GRanges
#' @examples
#' \dontrun{
#' locsMDR = mdrRanges('K00927')
#' }
mdrRanges = function(ko, passDir)
{
    mdrPath = sprintf("%s/pAss11/%s.fna", passDir, ko)
    msaPath = sprintf("%s/pAss03/%s.msa", passDir, ko)

    info = ShortRead::id(readFasta(mdrPath)) %>% first %>% as.character
    msa.starting = info %>% regexec("msaStart:(\\d+)",.) %>% regmatches(info, .) %>% unlist %>% last %>% as.integer
    msa.ending = info %>% regexec("msaEND:(\\d+)",.) %>% regmatches(info, .) %>% unlist %>% last %>% as.integer

    msa = readFasta(msaPath)

    locs = mapply(mapContig, contigid = ShortRead::id(msa) %>% substr(1, 11), seq= lapply(sread(msa), function(x) x),
    MoreArgs = list(s = msa.starting, e = msa.ending),
    SIMPLIFY=FALSE
    ) %>% do.call(rbind,.) %>% tbl_df
    mdrContigs = ShortRead::id(readFasta(mdrPath)) %>% as.character %>% substr(1, 11)
    filter(locs, id %in% mdrContigs) %$% GRanges(seqnames = id, ranges = IRanges(matchStart, end=matchEnd))
}

#' blatting
#' @param ko the ko of interest
#' @param type the
#' @param newblerInput path to newbler dir eg. /root in /root/K0000X/input/K0000X.1.fq
#' @param blatoutput path to store blat output in tabular blast format
#'
#' @importFrom ShortRead readFastq
#' @importFrom GenomicRanges GRanges
#' @examples
#' \dontrun{
#'
#' }

blatting = function(ko, type, newblerInput)
{
    combined = lapply(c(1,2), function(readNum){
        read = sprintf("%s/%s/input/%s.%s.fq", newblerInput, ko, ko, readNum)
        if(file.exists(read))
            read.fq = read %>% readFastq %>% grepReads(type)
    }) %>% do.call(c,.)
    contigs = sprintf("%s/%s/454AllContigs.fna", newblerInput, ko)
    blast8 = combined %>% map(contigs) #map is a metamapsDB function
    blatRanges = GRanges(seqnames=blast8$subject, ranges=IRanges(blast8$newStart, blast8$newEnd), read=blast8$query)
}


#' overlaps
#' @param ko the ko of interest
#' @importFrom GenomicRanges countOverlaps
#' @export
#' @examples
#' \dontrun{
#' mapReads2MDR('K00927', passDir="./out", newblerInput="./newbler2")
#' }

mapReads2MDR = function(ko, passDir, newblerInput)
{
    ko = gsub("^ko:", "", ko)
    cDNA = blatting(ko, type="cDNA", newblerInput)
    gDNA = blatting(ko, type="gDNA", newblerInput)
    komdrRanges = mdrRanges(ko, passDir)
    data.frame(
        name = as.character(seqnames(komdrRanges)),
        count.gDNA = countOverlaps(komdrRanges, gDNA),
        count.cDNA = countOverlaps(komdrRanges, cDNA),
        ko = ko
    )
}
