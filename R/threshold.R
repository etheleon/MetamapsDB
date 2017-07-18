#' generates `lookupTable` for filtering raw data post gene centric assembly
#'
#' returns three tables, 1. gene centric assembly readOrigin identities, 2. homologySearch assignment, 3 superimposed
#'
#' @param genesOfInterest the kos of interest defaults to scg must be in the ko:KXXXXX format
#' @param annotations table containing the number of KOs for each taxID
#' @param rs processed readStatuses from newbler output
#'
#' @examples
#' \dontrun{
#' rs = lapply(dynList, function(x) x$rs %>% mutate(ko = x$ko)) %>% do.call(rbind,.)
#' output = lookupTable(genesOfInterest = scg, annotations = "~/simulation_fr_the_beginning/out/template.csv", rs)
#' e
#' }
#' @export
lookupTable = function(genesOfInterest = scg, annotations = "~/simulation_fr_the_beginning/out/template.csv", rs)
{
    # in fact there's 2 lookup tables
    # 1. for homology search - the homology search + LCA has correctly identified the contigs to the correct ID
    # 2. for assembly - the assembly process has successfuly put together reads to form the correct contig

    genesOfInterest %<>% gsub("^(ko:)*","ko:",.)

    message("Simulated taxa")
    suppressMessages({
        simTaxa = dbquery("match ()-[:SIMULATED]-(g:Taxon) return distinct g.taxid as taxid") %$%
        as.character(taxid)
    })
    message(sprintf("%s taxa simulated", length(simTaxa)))

    message("Calculating their abundances")
    suppressMessages({
        abuDF = lapply(simTaxa, function(x){
            dbquery("match (t:Taxon{taxid:{taxid}}) return t.abundance as abu, t.taxid as taxid", list(taxid = x))
        }) %>% do.call(rbind,.)
    })
    ##################################################
    # Homology search
    message("Querying for Homology search data")
    ##################################################

    findGenus="
    MATCH
        (k:ko{ko:{ko}})<-[:assignment]-(c:contigs)-[:taxomapped]->(t:Taxon)
    WHERE
        c.mdr=1 AND c.spanning=1 AND toInt(c.readnum) > k.threshold
    WITH
        t
    OPTIONAL MATCH
        (t)-[:childof*]->(t2:genus)
    RETURN
        CASE
        WHEN t:genus THEN t.taxid
        ELSE t2.taxid
        END AS taxid
    "
        homologyAssignments = genesOfInterest %>% lapply(function(koi){
        message(sprintf("Downloading Homology search data for %s", koi))
    suppressMessages({
            assignedDF = dbquery(findGenus, list(ko = koi))
                tryCatch(
                {
                    if(!is.na(assignedDF) && nrow(assignedDF) > 0){
                        #browser()
                        assignedDF %>% make.data.frame %$% table(taxid) %>% data.frame %>% setNames(c("taxid", "Freq")) %>% arrange(desc(as.integer(Freq))) %>% tbl_df %>% mutate(ko = koi)
                    }else{
                        NA
                    }
                },error = function(e) warning(sprintf("%s has problem", koi))
                )
    })
            }) %>% do.call(rbind,.)
    homologyAssignments$taxid %<>% as.factor
    homologyAssignments$Freq %<>% as.integer
    homologyAssignments = homologyAssignments[complete.cases(homologyAssignments),]
    wideDF = homologyAssignments  %>% group_by(taxid) %>% mutate(tot = sum(Freq)) %>% ungroup %>% spread(ko, Freq, fill=0) %>% tbl_df

    #summaryDF = homologyAssignments %>% group_by(taxid) %>% 
        #summarise(ave = mean(Freq), sd = sd(Freq)) %>% 
        #merge(abuDF, by="taxid", all=T) %>% rowwise %>% mutate(name = taxnam.sql(taxid)$name) %>%
        #arrange(desc(abu)) %>% tbl_df

    #summaryDF %<>% merge(wideDF, by="taxid") %>% arrange(desc(abu)) %>% tbl_df
    #colnames(summaryDF) = make.names(colnames(summaryDF))

    ##################################################
    # Assembly
    message("Querying for Assembly information")
    ##################################################
    message("Reading newbler assembly readStatus; only considering pure contigs")
    rs = rs %>% filter(ko %in% gsub("ko:", "", genesOfInterest)) %>% merge(thresholds, ., all=T)
    suppressMessages({
    thresholds = genesOfInterest %>% 
        lapply(function(ko){dbquery("match (k:ko{ko:{ko}}) return k.ko as ko, k.threshold as threshold", list(ko = ko))}) %>%
        do.call(rbind,.)
    })
    thresholds$ko %<>% gsub("ko:", "", .)


    ass = rs %>% filter(total > threshold, spanning == 1, status == 'pure') %>% mutate(ko = gsub("^", "ko:", ko)) %>%
        group_by(ko, readOrigin) %>% summarise(n=n()) %>%
        setNames(c("ko", "taxid", "Freq"))
    ass_wide = ass %>% spread(ko, Freq, fill = 0)

    ##################################################
    # Truth
    message("Who Matched")
    ##################################################
    df = data.table::fread("~/simulation_fr_the_beginning/out/template.csv") %>% as.data.frame
    annotationDF = df %>% filter(ko %in% genesOfInterest) %>% group_by(seqnames, ko) %>% summarise(Freq=n()) %>% setNames(c("taxid", "ko", "Freq")) %>% ungroup
    wideDF_truth = df %>% filter(ko %in% genesOfInterest) %>% group_by(ko, seqnames) %>% summarise(n=n()) %>% setNames(c("taxid", "ko", "Freq")) %>% spread(taxid, Freq, fill = 0)

    makeLookup = function(input, annotationDF, abuDF, type=c("wide", "long"))
    {
        comp = merge(annotationDF, input,by=c("taxid", "ko"), suffix=c(".truth", ".assigned"), all=T) %>% tbl_df
        comp[is.na(comp)] = 0
        if(type == 'wide'){
            wide = comp %>% rowwise %>%
                mutate(matched= case_when(
                    Freq.truth == 0 && Freq.assigned == 0 ~ 0,
                    Freq.truth == Freq.assigned ~ 1,
                    TRUE ~ 0
                )) %>%
                #mutate(matched = Freq.truth + Freq.assigned)  %>%
                select(taxid, ko, matched) %>% spread(ko, matched)
            #if missing should be
            wide[is.na(wide)] = 0
            #Add abundanceInfo
            wide %<>% merge(abuDF, by="taxid", all.y=T) %>% rowwise %>% mutate(name = taxnam.sql(taxid)$name) %>%
            arrange(desc(abu)) %>% tbl_df %>% select(taxid, name, abu, everything())
            colnames(wide) = make.names(colnames(wide))
            wide
        }else if(type=='long'){
            comp %>%
                rowwise %>%
                mutate(matched= case_when(
                    Freq.truth == 0 && Freq.assigned == 0 ~ 0,
                    Freq.truth == Freq.assigned ~ 1,
                    TRUE ~ 0
                )) %>%
                select(taxid,ko, matched)
        }else{
            warning("unknown type")
        }
    }

    wideDF_comparison_assembly = makeLookup(ass, annotationDF, abuDF, "wide")
    wideDF_comparison_homology = makeLookup(homologyAssignments, annotationDF, abuDF, "wide")

    comparison_assembly = makeLookup(ass, annotationDF, abuDF, "long")
    comparison_homology = makeLookup(homologyAssignments, annotationDF, abuDF, "long")

# need to change

    twoTables = merge(comparison_assembly , comparison_homology , by=c("taxid", "ko"), suffix=c(".assembly", ".homology")) %>%  rowwise %>%
       mutate(matched= case_when(
           matched.assembly == 0 && matched.homology == 0 ~ 0,
           matched.assembly == matched.homology ~ 1,
           TRUE ~ 0
       ))  %>% select(taxid, ko, matched)

    #by filling with zero i'm saying dun look at this one
    superImposed = twoTables %>% spread(ko, matched, fill=0)
    superImposed %<>% merge(abuDF, by="taxid") %>% arrange(desc(abu)) #%>% head(n=1)

    allKOs = list(
            homology    = wideDF_comparison_homology,
            assembly    = wideDF_comparison_assembly,
            superImposed= superImposed
    )
}


#' dynPlot Diagnostic plots
#'
#' @param dyn output from dynamicThreshold
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr "%<>%"
#' @importFrom forcats fct_rev
#' @examples
#' \dontrun{
#' connect("yourDomain", password="yourPassword")
#' newblerDir= "~/simulation_fr_the_beginning/reAssemble/everybodyelse/data/newbler/"
#' dynList = scg  %>% gsub("ko:", "", .) %>% mclapply(dynamicThreshold, root=newblerDir, mc.cores=20)
#' dynList = scg  %>% gsub("ko:", "", .) %>% head(n=1) %>% lapply(dynamicThreshold, root=newblerDir)
#' pdf("thresholdPlots.pdf", width=10)
#' plotDF = dynPlots(dynList, F)         
#' dev.off()                          
#' pdf("abundance.pdf", width=10)
#' lapply(plotDF$details,"[[", 1)      
#' dev.off()                           
#' plotDF$p %>% ggsave(file="summaryPlot.pdf", w=10)
#' plotDF$violin %>% ggsave(file="violin.pdf", w=10, h=4)
#' }
#' @export
dynPlots = function(dynList, indivPlots=T){
    repeats = function(df, oneSCG, cutoff, indivPlots=TRUE, spanning=TRUE){
        message(sprintf("Processing KO of interest: %s", oneSCG))
        #recovered = rs %>% filter(origin == 'inSimulation') %>% nrow
        contigs = contigInfo(ko=oneSCG) %>% filter(MDR == '1')
        if(spanning){
            contigs %<>% filter(spanning==1)
        }
        simulatedDF = simulated(oneSCG)
        df2 = df %>% mutate(
            ko               = oneSCG,
            cutoff           = cutoff,
            simulated        = sum(simulatedDF$Freq),
            remaining = sapply(df$threshold, function(x) nrow(filter(contigs, readnum > x))),
            remaining.cutoff = nrow(filter(contigs, readnum > cutoff))
        )
        # shows the number of repeats
        p1 = ggplot(df2)                                      +
            geom_line(aes(threshold, repeats, group=ko))      + # this repeats is based on num. of genomes being repeated
            geom_vline(aes(xintercept = cutoff), color="red") +
            ggtitle(koname(oneSCG)$ko.definition)             +
            ylab("Repeats")  +
            ggtitle("Repeats")

        p2 = ggplot(df2)                                                                                 +
            geom_line(aes(threshold, remaining, group=ko))                                               +
            geom_hline(aes(yintercept=simulated))                                                        +
            geom_hline(aes(yintercept = remaining.cutoff), color="red")                                  +
            geom_text(aes(0, simulated, label = "Simulated", vjust = -1))                                +
            geom_text(aes(50, remaining.cutoff, label = "After thresholding", vjust = -1), color="Red")  +
            ggtitle(koname(oneSCG)$ko.definition)                                                        +
            ylab("#genes (total)")

        # shows the number of simulated & not simulated upon thresholding
        p3 = df %>% gather(type, value, -threshold) %>% filter(type != 'repeats') %>% mutate(type = fct_rev(as.factor(type))) %>%
            ggplot() +
                geom_line(aes(x=threshold, y=value, color=type, group=type))                                               +
                facet_wrap(~type, scales="free", nrow=3)+
                #scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a"))
                theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))
        message("Lost value")

        if(indivPlots){
            message("Grid")
            grid.arrange(p1, p2) %>% print
            print(p3)
        }
        df2
    }

    kos = lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .)

    repeatDF = mapply(repeats,
        df        =  lapply(dynList, function(x) x$repeats),# %>% .[3],# %>% head(n=2),
        oneSCG    =  lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .),# %>% .[3],#%>% head(n=2),
        cutoff    =  lapply(dynList, function(x) x$thres),
        MoreArgs  =  list(indivPlots = indivPlots),
        SIMPLIFY  =  FALSE
    ) %>% do.call(rbind,.)

    repeatDF.thresholdOnly = mapply(repeats,
        df        =  lapply(dynList, function(x) x$repeats.noSpan),# %>% .[3],# %>% head(n=2),
        oneSCG    =  lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .),# %>% .[3],#%>% head(n=2),
        cutoff    =  lapply(dynList, function(x) x$thres.noSpan),
        MoreArgs  =  list(indivPlots = indivPlots, spanning=FALSE),
        SIMPLIFY  =  FALSE
    ) %>% do.call(rbind,.)

    message("Summary")
    #Naive
    errorTable.vanilla = lapply(dynList, function(x) x$koi) %>% gsub("ko:", "", .) %>% lapply(contigInfo) %>% do.call(rbind,.) %>% filter(MDR==1) %>% group_by(bin) %>% summarise(n=n()) %>% mutate(simulated = sapply(bin, function(ko) nrow(simulated(ko)))) %>% mutate(diff = n - simulated)
    mae.vanilla = signif(sum(errorTable.vanilla$diff)/nrow(errorTable.vanilla), 4)

    #Spanning only 26.65
    errorTable.b4 = repeatDF %>% filter(threshold == 1) %>% select(ko, simulated, remaining)  %>% mutate(diff = abs(remaining - simulated))
    mae.b4      = signif(sum(errorTable.b4$diff)/nrow(errorTable.b4), 4)

    #Threshold only (incl spanning and non-spanning)
    errorTable.thresholdOnly = repeatDF.thresholdOnly %>% select(ko, simulated, remaining.cutoff) %>% unique %>% mutate(diff = abs(remaining.cutoff - simulated))
    mae.threshold = signif(sum(errorTable.thresholdOnly$diff)/nrow(errorTable.thresholdOnly), 4)

    #Remove spanning, then apply threshold mae 21.84
    errorTable.after = repeatDF %>% select(ko, simulated, remaining.cutoff) %>% unique %>% mutate(diff = abs(remaining.cutoff - simulated))
    mae.after   = signif(sum(errorTable.after$diff)/nrow(errorTable.after), 4)

    #apply threshold, then remove spanning (worse) mae 24.45
    thresholdThenSpan = repeatDF.thresholdOnly %>% select(ko, cutoff) %>% unique
    koOrder = sapply(dynList, `[[`, 5)
    thresholdThenSpan$remaining.cutoff = thresholdThenSpan %>% apply(1, function(row){ nrow(filter(dynList[[which(koOrder %in% row['ko'])]]$rs , spanning == 1, total > as.integer(row['cutoff']))) })
    thresholdThenSpan$simulated =  thresholdThenSpan %>% apply(1, function(row){nrow(simulated(unname(row['ko'])))})
    thresholdThenSpan %<>% mutate(diff = abs(remaining.cutoff - simulated))
    mae.after2 = signif(sum(thresholdThenSpan$diff)/nrow(thresholdThenSpan), 4)

    mae.change = sprintf("Mean Absolute Error (MAE): %s (before threshold: %s | spanning-only: %s | threshold-only %s)", mae.after, mae.vanilla, mae.b4, mae.threshold)
    message(mae.change)

    uniqueGenomes = scg %>% lapply(simulated) %>% do.call(rbind,.) %$% unique(taxid) %>% length

    message(sprintf("Number of simulated genomes: %s", uniqueGenomes))
    estimate     = repeatDF %>% select(ko, remaining.cutoff) %>% unique %$% mean(remaining.cutoff)
    simulated    = repeatDF %>% select(ko, simulated) %>% unique %$% mean(simulated)
    prethreshold = repeatDF %>% filter(threshold == 1) %>% select(ko, remaining) %>% unique %$% mean(remaining)

    #hardcoded
    maxGenes = repeatDF$remaining %>% max + 50

    message("Preparing summary PlotDF")
    #naive
    plotdf = repeatDF.thresholdOnly %>% filter(threshold == 1) %>% select(ko, simulated, remaining, inSimulation:outsideSimulation) %>% unique %>% gather(type, value, -ko) %>% mutate(threshold = "naive")
    #with post-processing
    plotdf = repeatDF %>% group_by(ko) %>% filter(threshold == cutoff) %>% ungroup %>% select(inSimulation:ko, remaining.cutoff) %>% gather(type, value, -ko) %>% mutate(threshold="cutoff") %>% rbind(plotdf)

    plotdf$threshold[plotdf$type == 'simulated']= "truth"
    plotdf %<>% mutate(newtype = paste(type, threshold, sep="_"))

    groupType = function(x){
        if(unique(x$type) == 'inSimulation'){
            x %>% mutate(annotation = "in")
        }else if (unique(x$type) == 'outsideSimulation'){
            x %>% mutate(annotation = "out")
        }else{
            x %>% mutate(annotation = "norm")
        }
    }

    plotdf %<>% group_by(newtype) %>% do(groupType(.))
    message("summaryplot")
    p = ggplot(plotdf, aes(x=ko, y=value, group=newtype, 
                           color=fct_relevel(newtype, "remaining_naive", "inSimulation_naive", "outsideSimulation_naive", "remaining.cutoff_cutoff", "simulated_truth", "inSimulation_cutoff", "outsideSimulation_cutoff")))                                                          +
    #regression lines
    geom_line(stat="smooth", method="lm", alpha=0.05, linetype="dashed")                                                                                                                                                 +
    #are there SCGs which are not found in a given genome (yes most definitely; not checked though)
    geom_hline(aes(yintercept = uniqueGenomes), color="grey")+
    geom_line(data = plotdf %>% filter(newtype %in% c("remaining_naive", "simulated_truth", "remaining.cutoff_cutoff")), aes(x=ko, y=value, group=newtype), stat="smooth", method="lm", alpha=0.2, linetype="dashed")                                                                                                                                                 +
    geom_line(alpha=0.5, aes(linetype=fct_relevel(annotation, "norm", "in", "out")))                                                                                                                           +
    geom_point(alpha=0.8)                                                                                                                                                                                               +
    scale_color_manual("Processing",
        values=c("#de2d26", "#fc9272", "#fc9272", "#3182bd","grey", "#9ecae1", "#9ecae1"),
        labels=c(sprintf("Naive (MAE: %s)", mae.vanilla), sprintf("With processing (MAE: %s)", mae.after)),
        breaks = c("remaining_naive", "remaining.cutoff_cutoff"))                                                                                                                                                                    +
    scale_linetype_manual("In Genome Annotation", values=c("solid", "dotted", "dotdash"), label=c("Yes", "No"), breaks=c("in", "out")) +
    scale_y_continuous(breaks=c(0, 100, 200, 300, uniqueGenomes, 400), labels=c(0, 100, 200, 300, sprintf("Genomes: %s",uniqueGenomes), 400))+
    #ylim(c(0, maxGenes))                                                                                                                                                                                                +
    #aesthetics
    ylab(expression("N"[contigs]*" in MDR"))                                                                                                                                                                                             +
    xlab("KEGG Orthology Group")                                                                                                                                                                                        +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position="top")                                                                                                                                                       +
    guides(colour = guide_legend(order=1),
           linetype=guide_legend(order=2))+
    guides(col = guide_legend(ncol = 1, byrow = TRUE),linetype = guide_legend(ncol = 1, byrow = TRUE))
    #overallPlot
    message("Boxplot")
    simTaxa = scg %>% lapply(function(oneSCG) dbquery("match (k:ko{ko:{ko}})-[:SIMULATED]-(g:Taxon) return k.ko, g.taxid", list(ko=oneSCG))) %>% do.call(rbind,.) %$% unique(g.taxid) %>% as.character()
    abuDF = simTaxa %>% lapply(function(x) dbquery("match (t:Taxon{taxid:{taxid}}) return t.abundance as abu, t.taxid as taxid", list(taxid = x))) %>% do.call(rbind,.)
    abuDF$taxid %<>% as.character
    simulatedInfo = simTaxa %>% lapply(taxnam.sql) %>% do.call(rbind,.)
    abuDF %<>% merge(simulatedInfo, by="taxid")
    abuDF %<>% arrange(desc(abu)) %>% mutate(rank = 1:nrow(abuDF))
    abuDF$taxid %<>% as.factor
    abuDF %<>% mutate(cumsum.perc = cumsum(abu) / sum(abu))

    missingFunc <- function(oneDynList, abuDF)
    {
        df = oneDynList$rs
        oneSCG  = oneDynList$koi %>% gsub("ko:", "", .)
        cutoff = oneDynList$thres

        naive = abuDF$taxid %in% df$readOrigin
        thres = abuDF$taxid %in% filter(df, total > cutoff)$readOrigin

        abuDF$status = "kept"
        abuDF$status[!thres] = "lostAfterthres"
        abuDF$status[!naive] = "lostNaive"
        thresColor= "#3182bd"
        naiveColor= "#de2d26"
        #browser()
        p1 = ggplot(abuDF, aes(x=rank, y=abu, fill=as.factor(status)))                             +
            geom_bar(stat="identity")                            +
            geom_text(data = abuDF %>% filter(status!='kept'), aes(x=rank,y=abu, label=name, color=as.factor(status)), hjust=0, angle=45) +
            scale_color_manual(values=c(thresColor, naiveColor), guide=FALSE)                             +
            scale_fill_manual("Recovery Status", values=c("#00000020",thresColor, naiveColor), 
            labels=c("Recovered", "Not recovered (With Processing)", "Not recovered (Naive)"))             +
            scale_x_continuous(breaks = abuDF %>% filter(status!='kept') %$% rank)   +
            theme(axis.text.x=element_text(angle=90))                           +
            xlab("Abundance Rank of Genome") + ylab("Abundance")+
            ggtitle(sprintf("Gene recovery of %s: %s", oneSCG, koname(oneSCG)$ko.definition))
        da = data.frame(perc = seq(0.1, 1, 0.1), num.naive =  seq(0.1, 1, 0.1)%>% sapply(function(percentile) nrow(filter(abuDF, cumsum.perc < percentile, status == 'lostNaive'))), num.threshold = seq(0.1, 1, 0.1)  %>% sapply(function(percentile) nrow(filter(abuDF, cumsum.perc < percentile, status != 'kept'))), ko = oneSCG)
        list(plot=p1, data=da)
    }
    remainder = dynList %>% lapply(missingFunc, abuDF = abuDF)

    #browser()
    percDF = lapply(remainder, '[[', 2) %>% do.call(rbind,.) %>% gather(type, value, -perc, -ko)
    percDF$perc %<>% as.factor

    #summary(lm(data=percDF, value ~ perc))

    # Rohan's comments
    # need to show what happens as I continue to increase the thresholding
    # Why is there such variation in the genes (which can be solved by the shannon entropy)

    #summaryPlot - violin
    message("Final")
    violinPlot = percDF  %>% 
        ggplot(aes(x=perc, y=value, fill=as.factor(type), color=as.factor(type)))                    +
            geom_boxplot(position=position_dodge(1), alpha=0.5)                                                 +
            geom_point(position=position_jitterdodge(dodge.width=0.9))                                          +
            #scale_y_log10()+
            scale_color_manual("Processing", values=c("#de2d26", "#3182bd"), label=c("Naive","With processing")) +
            scale_fill_manual( "Processing", values=c("#de2d26", "#3182bd"), label=c("Naive","With processing")) +
            scale_x_discrete(labels=seq(10, 100, 10))+
            xlab("Top Genera (%)")+ylab(c(expression("N"[Genes]*' not recovered')))+
    theme(legend.position="top")+
    geom_vline(aes(xintercept = 1.5), linetype="dashed")+
    geom_vline(aes(xintercept = 2.5), linetype="dashed")+
    geom_vline(aes(xintercept = 3.5), linetype="dashed")+
    geom_vline(aes(xintercept = 4.5), linetype="dashed")+
    geom_vline(aes(xintercept = 5.5), linetype="dashed")+
    geom_vline(aes(xintercept = 6.5), linetype="dashed")+
    geom_vline(aes(xintercept = 7.5), linetype="dashed")+
    geom_vline(aes(xintercept = 8.5), linetype="dashed")+
    geom_vline(aes(xintercept = 9.5), linetype="dashed")

    list(df = repeatDF, mae = list(spanning=mae.b4, bothSpanningNThreshold=mae.after, naive=mae.vanilla), plot=p, details = remainder, violin = violinPlot)
}

#' dynamicThreshold tries to identify the lower bound converage value in order to remove low quality contigs.
#' Number of contigs tend to stablize at a value when removing reads below a certain readnum and we try to identify the min read required for that region of stability
#'
#' @param koi KO of interest
#' @param mode two options either readnum or coverage; coverage is experimental, use readnum
#' @param thresholding which type of thresholding method to use
#' @param root folder to find
#' @import dplyr
#' @importFrom zoo rollapply
#' @export
dynamicThreshold <- function(koi, root){
    message(sprintf("Processing %s", koi))
    rs = readStatusReader(root, koi, mdr=TRUE)

    repeatsDF.spanning =  contigsSurvive.repeats.readNum(filter(rs,spanning == 1), koi=koi)
    repeatsDF.noSpan =  contigsSurvive.repeats.readNum(rs, koi=koi)

    list(
        repeats        = repeatsDF.spanning,
        thres          = simpleThres(repeatsDF.spanning) %>% ceiling,
        repeats.noSpan = repeatsDF.noSpan,
        thres.noSpan   = simpleThres(repeatsDF.noSpan) %>% ceiling,
        koi            = koi,
        rs             = rs
    )
}

#' rolling A dynamic threshold for the killing redundant contigs
#'
#' outputs a data.frame with 2 columns 1.) rollingMean 2.) meanDepth
#' @param keptDF dataframe for the #repeats left after thresholding, columnnames thresholds, repeats
#' @param wind window size
#' @param interval interval size
#'
rolling <- function(keptDF, wind=15, interval = 10){
        data.frame(
            #should rename to rolling diff basically the averaged gradient change
            #in the number of repeats
            rollingMean = keptDF %$% rollapply(repeats, width=2, by=1, FUN = function(e){ abs(e[2] - e[1]) }) %>% rollapply(width = wind, by = interval, FUN = mean), 
            #the mean no. of #reads / coverage to keep in that 10 base window
            meanDepth   = keptDF %$% rollapply(threshold[-1], width = wind,  by = interval, FUN= mean)
        )
}


#' simpleThres
#'
#' internal cluster
# 
#' @param keptDF internal function
simpleThres = function(keptDF){
    #browser()
    myRLE = keptDF %>% filter(threshold <=50) %$% rollapply(repeats, width=2, by=1, FUN = function(e){ abs(e[2] - e[1]) }) %>% rle %>% .$length
    ceiling(mean(which(rep(myRLE == max(myRLE), myRLE))))
}

#' contigsSurvive.repeats.readNum
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param thresholds the threshold window
#' @importFrom tidyr spread
#'
contigsSurvive.repeats.readNum <- function(rs, thresholds = seq(1, 50, 1), koi){
    sim = simulated(koi)
    thresholds %>% lapply(function(readDepth){
        df1 = rs %>% filter(count > readDepth) %>% group_by(readOrigin) %>% 
        summarise(repeats = n())  %>% filter(repeats > 1) %$% sum(repeats) %>% 
        data.frame(threshold = readDepth, repeats = .)
        topHIT = function(x) { x %>% arrange(desc(perc)) %>% head(n=1) }
        df2 = rs %>% group_by(X5..Contig) %>% do(topHIT(.)) %>% ungroup %>%
        filter(count > readDepth) %>% group_by(origin) %>% 
        summarise(types = n()) %>% mutate(threshold = readDepth) %>% spread(origin, types)
        merge(df1, df2, by="threshold")
    }) %>% do.call(rbind,.)
}

#' simulated 
#'
#' pull all simulated genomes
#'
#' @param ko koi
#'
#' @export
simulated <- function(ko){
    message(sprintf("Universal truth: How many genes were simulated for %s", ko))
    simulated = dbquery("
        MATCH
            (k:ko{ko:{ko}})-[sim:SIMULATED]-(g:genus)
        RETURN
            k.ko as ko,
            g.taxid as taxid, 
            sim.genes as Freq", 
    list(ko=gsub("^(ko:)*", "ko:", ko))) %>% make.data.frame
    simulated$Freq %<>% as.character %>% as.integer
    message(sprintf("%s gene simulated from %s genera", sum(simulated$Freq), nrow(simulated)))
    simulated
}

#' contigsSurvive.repeats.rpk
#'
#' @param rs data.frame read.status, contig, readOrigin (taxid)
#' @param rpk reads per kilo
#' @param koi the KO of interest
#'
contigsSurvive.repeats.rpk <- function(rs, rpk = seq(0, 2, 0.01), koi='K00927'){
    query = "
        MATCH
            (c:contigs{bin:{koi}})
        WHERE
            c.mdr = 1
        RETURN
            c.contig as ID,
            toFloat(c.readnum) / toFloat(c.length) as rpk
    "
    coverageDF = dbquery(query, params=list(koi = gsub("^(ko:)*", "ko:", koi)))
    coverageDF$X5..Contig = as.character(coverageDF$ID) %>% strsplit(':') %>% lapply('[[', 2) %>% do.call(c,.)
    coverageDF = merge(coverageDF, rs, by="X5..Contig",all.y=T)

    rpk %>% lapply(function(threshold) {
        coverageDF %>% filter(rpk > threshold) %>%
        #true repeats not paralogs!!!!
        group_by(readOrigin) %>% summarise(repeats = n())  %>% filter(repeats > 1) %$% data.frame(threshold, repeats = sum(repeats))
    }) %>% do.call(rbind,.)
}


#' readStatusReader stores read assignment details from the simulation and assigns the contig a genome of origin
#'
#' taken from /rootDir/ko/454ReadStatus.txt from newbler. 
#' We remove stray reads from other taxa if it only attributes to less than 10% of the contig, output
#' data.frame has column spanning set to 0 1 to show if its spanning or not
#'
#' @param root the root directory
#' @param koi the ko directory
#' @importFrom data.table as.data.table
#' @export
readStatusReader <- function(root, koi, mdr=FALSE)
{
    message("Reading in read assignment data from NEWBLER output")
    readStatus = as.data.table(read.csv(sprintf("%s/%s/454ReadStatus.txt", root, koi), sep="\t"))
    message(sprintf("%s reads read", nrow(readStatus)))
    readStatus$Accno %<>% as.character
    regexstr = "^\\d+\\|(\\d+)"
    readAlloc = readStatus %$% regmatches(Accno, regexec(regexstr,Accno)) %>% sapply(`[`,2 )
    rs = readStatus %>%
        select(Read.Status, X5..Contig) %>%
        mutate(readOrigin = readAlloc)  %>%
        filter(Read.Status == 'Assembled')
    message(sprintf("%s reads Assembled", nrow(readStatus)))
    #removes readOrigins which account for 10% of the contig
    #there's contigs with same genome (readOrigin) but diff geneloc
    if(mdr){
        df = contigInfo(ko=koi) %>% make.data.frame %>% filter(MDR == '1')
        mdrContigs = df$contig %>% as.character %>% strsplit(':') %>% lapply(function(i) i[[2]]) %>% do.call(rbind,.)
        mdrSpanningContigs = df %>% filter(spanning == '1') %$% contig %>% as.character %>% strsplit(':') %>% lapply(function(i) i[[2]]) %>% do.call(rbind,.)
        rs %>% filter(X5..Contig %in% mdrContigs) %>%
            group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% ungroup %>% group_by(X5..Contig) %>% 
            mutate( total = sum(count),
                    spanning = ifelse(X5..Contig %in% mdrSpanningContigs, 1, 0)) %>%
        mutate(perc = count/total) %>% filter(perc > 0.1) %>% categorize(koi)
    }else{
        rs %>% group_by(X5..Contig, readOrigin) %>% summarise(count=n()) %>% ungroup %>% group_by(X5..Contig) %>% mutate(total = sum(count)) %>% mutate(perc = count/total) %>% filter(perc > 0.1) %>%
        categorize(koi)
    }
    message("Finished reading")
}


#' categorize
#'
#' Adds 2 more columns
#' 1. if the contig is chimeric or pure based on the read origins
#' 2. if the readOrigin aligns with annotations
#'
#' @param rs output from readStatusReader
#'
categorize <- function(rs, ko){
	chimeric = rs$X5..Contig %>% table %>% as.data.frame %>% arrange(desc(Freq)) %>% filter(Freq > 1) %$% . %>% as.character
	rs$status = "pure"
	rs[rs$X5..Contig %in% chimeric,]$status="chimeric"
	rs$origin = "unknown"
	pure = rs %>% filter(!X5..Contig %in% chimeric)
    simulatedDF = simulated(ko)
    rs[which(rs$readOrigin %in% simulatedDF$taxid),]$origin= "inSimulation"
	rs[which(!rs$readOrigin %in% simulatedDF$taxid),]$origin= "outsideSimulation"
    rs
}
