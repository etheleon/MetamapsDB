pathways<-structure(function #List Pathways
    (...){
   setNames(dbquery(query='match (pathway:pathway) return pathway.pathwayname, pathway.pathway'), c("pathwayName", "pathwayID"), ...)
    }, ex= function(){
#   pathwayDF = pathways();
   })
