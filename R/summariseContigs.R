summariseContigs <- function(type=c("simulation", "empirical")){
summaryContigs = contigs          %>%
select(ko, mx)                    %>%
group_by(ko)                      %>%
    summarise(
              totalContigs = n(),
              mxContigs = sum(mx)
              )

contigs.sim %>% select (ko, simulation.gbff.annotation) %>% unique %$% is.na(simulation.gbff.annotation) %>% sum

}
