findSeeds <- structure(function(#To find all seed compounds in the metabolic graph
### Function finds strongly connected components within a bipartite metabolic network.
### And selects for components with at least 1 outgoing edge and no inbound edges.

     mbgraph = grepgraph(top500kos) ## the graph
    ){
        #allCompounds
        cpdNodes        = which(grepl("cpd:", V(mbgraph)$name))
       #Find all strongly connected components (SCC)
        scc             = clusters(mbgraph, mode="strong")


        #Find all links with non-members in each scc
        sourceCompDF = scc$membership %>% unique %>%
        lapply(function(memberID){
               members = which(scc$membership == memberID)
                #inbound only because
               neighborhood(mbgraph, 1, members, "all") %>%
               lapply(function(member){
                          anyNonMember = !V(mbgraph)[member[-1]]$name %in% V(mbgraph)[members]$name
                          gotNonMember  = sum(anyNonMember)
                          data.frame(   V(mbgraph)$name[member[1]],
                                        nonmember  = ifelse(gotNonMember,
                                            list(nonmember = V(mbgraph)[member[-1][which(anyNonMember)]]$name),
                                            NA),#this only chooses the first
                                        gotNonMember,
                                        memberID) %>% setNames(c("member", "nonmember", "freq", "membership"))#had to do this cause the list can be named
               }) %>% do.call(rbind,.)
        }) %>% do.call(rbind,.)
        clusterNSize = scc$membership %>% table %>% data.frame %>% setNames(c("membership", "clusterSize"))
        clusterNSize_cpdOnly = scc$membership[cpdNodes] %>% table %>% data.frame %>% setNames(c("membership", "clusterSize.cpd"))
        sourceCompDF = merge(merge(sourceCompDF, clusterNSize, by="membership", all=T), clusterNSize_cpdOnly, by="membership", all=T)

        #' Note in the PNAS paper, the nodes were purely just compounds
        possibleSCC =
            sourceCompDF                                                                     %>%
            filter(
                   clusterSize.cpd > 5,            # Component size > 5 members
                   !is.na(nonmember)    # Has connections with nodes outside of component
                   )
        possibleSCC = sourceCompDF                                            %>%
        filter(
               membership %in% unique(possibleSCC$membership),
               !is.na(nonmember)
               )

        possibleSCC = apply(possibleSCC, 1, function(x){
              outbound = sum(which(V(mbgraph)$name == x["nonmember"]) %in% neighborhood(mbgraph, 1, x["member"], "out")[[1]][-1])
              inbound  = sum(which(V(mbgraph)$name == x["nonmember"]) %in% neighborhood(mbgraph, 1, x["member"], "in" )[[1]][-1])
                     #check If the non-member is inbound or outbound node
                     data.frame(membership = x["membership"],
                                member     = x["member"],
                                nonmember  = x["nonmember"],
                                outbound,
                                inbound
                                )
               }) %>% do.call(rbind, .)
###    Data.frame of source component, entites, linker connection to non-member
    },
    ex = function() {
        ...
        #possibleSCC %>% group_by(membership) %>% summarise(sum(outbound), sum(inbound))
    }
)
