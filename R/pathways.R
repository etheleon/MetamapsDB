pathways<-structure(
function #List Pathways
### Lists all metabolic pathways
(... ##<< extra arguments, eg cypherurl
 ){
setNames(
         dbquery(query='match (pathway:pathway) return pathway.pathwayname, pathway.pathway', ...), 
         c("pathwayName", "pathwayID")
         )
}, ex= function(){
#   pathwayDF = pathways();
})
