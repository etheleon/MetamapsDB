structure(function
### Users shd encode the values into the igraph object 
### Works with older version of IGraph, maybe someone can choose to upgrade this further

(igraphObj ##<< graph object
){

library(shiny)
library(igraph)
library(dplyr)
library(magrittr)

igraph2gexf(g)

#Options
colorType <- c  ("centrality","betweeness","loadScore") %>%
        setNames("Centrality","Betweeness","Load Score")


ui = navbarPage("Superzip", id="nav",
        
      # Shiny versions prior to 0.11 should use class="modal" instead.
,

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)




    ui <- fluidPage(
        #Control Panel
    sidebarLayout(position = "right",
              inputPanel(
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Metabolic Graph Explorer"),
        selectInput("color", "Color", colorType),
#        selectInput("size", "Size", vars, selected = "adultpop"),
#        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
#          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        )
    )
        #Plots

    )

    server = function(input, output){
    uniongraph = reactive({
            gexf.obj = combinedG_gexf[[which(seq(0.05,1, 0.2) == input$Perc)]]
            gexf.obj$graph
        })

    communityGraph = reactive({
        testList[[1]][[which(seq(0.05,1, 0.2) == input$Perc)]]$graph %>% prettifyGraph(., vsize=c(0.2, 0.5))%>% igraph2gexf %$% 
        graph
    })

    geneGraph = reactive({
        testList[[2]][[which(seq(0.05,1, 0.2) == input$Perc)]]$graph %>% prettifyGraph %>% igraph2gexf %$%
        graph
    }) 

    output$combined= renderSigma({
        sigma(gexf=uniongraph(), drawLabels=T, labelThreshold=20)
    })

    output$gene= renderSigma({
        sigma(gexf=geneGraph(), drawLabels=T, labelThreshold=20)
    })

    output$community= renderSigma({
        sigma(gexf=communityGraph(), drawLabels=T, labelThreshold=20)
    })
}

shinyApp(
    ui,
    server,
    options = list(height = 16000)
)
)




####################################################################################################

ui = fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),
    sigmaOutput("network", width="100%", height="1000"),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = 300,
        h2("KEGG explorer"),
        selectInput("labelType", "Labels", c("KO", "CPD")),
    #selectInput("color", "Color", vars),
    #selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("koID", "KEGG Ortholog ID", 'ko:K00001')
        )
    )
)

server = function(input, output){
    output$network = renderSigma({
        sigma(gexf = agexf$graph, drawLabels = T, labelThreshold = 20)
    })
}


shinyApp(ui, server)
