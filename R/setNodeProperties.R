#' addContigProperty
#'
#' Adds property to contig node
#'
#' @param CONTIG the contigID
#' @param KO the KO the contig belongs to
#' @param property list containing the property which you would want to insert, name of property in list is the name of the property in the
#' @export
addContigProperty <- function(contig, ko, property, all=FALSE, batchSize=10000, add=FALSE){
    name = names(property)
    if(all){
        if(add){
            query   = sprintf("
                MATCH
                    (c:contigs)
                WHERE
                    NOT HAS (c.%s)
                WITH
                    c
                LIMIT
                    %s
                SET
                    c.%s = {%s}
                RETURN
                    count(c) as COUNT
                ", name, batchSize, name, name)
            flog.info("The query")
            flog.info(query)
        }else{
            query   = sprintf("
                MATCH
                    (c:contigs)
                WITH
                    c
                LIMIT
                    %s
                SET
                    c.%s = {%s}
                RETURN
                    count(c) as COUNT
                ", batchSize, name, name)
            flog.info("The query")
            flog.info(query)
        }
        future({
                unfinished = TRUE
                while(unfinished){
                    df = dbquery(query=query, params=property)
                    flog.info(sprintf("Returned: %s", df$COUNT))
                    if (df$COUNT != batchSize) unfinished = FALSE
                }
            }) %plan% multiprocess
    }else{
        property$contigID = sprintf("%s:%s", ko, contig)
        query   = sprintf("
        MATCH
            (c:contigs)
        WHERE
            c.contig = {contigID}
        SET
            c.%s = {%s}
        ", name, name)
        result = dbquery(query=query, params=property, justPost=TRUE)
    }
}

#' addKOProperty
#'
#' Adds property to ko node
#'
#' @param ko the koID
#' @param property 
#' @export
addKOProperty <- function(ko, property){
    name = names(property)
    property$koID = sprintf("ko:%s", ko)
    query   = sprintf("
    MATCH
        (ko:ko{ko:{koID}}) 
    SET
        ko.%s = {%s}
    ", name, name)

    result = dbquery(query=query, params=property, justPost=TRUE)
}



