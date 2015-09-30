#' Takes mbgraph and outputs as a shinyApp Obj
#' Users should encode the values into the igraph object
#' Works with older version of IGraph, maybe someone can choose to upgrade this further
#' @param igraphObj graph object after running prettifyGraph
#'
#'
#' @import shiny sigma
#'
#' @export
sigmaGraph <- function(igraphObj){
agexf <- igraphObj %>% igraph2gexf %$% graph

colorType <- c("centrality", "betweeness", "loadScore") %>%
        setNames(c("Centrality", "Betweeness", "Load Score"))

columnBar <- shiny::absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = 300,
        h2("Metabolic Explorer"),

        selectInput("labelType", "Labels", c("KO", "CPD")),

        selectInput("color", "Color", colorType),
        #selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("koID", "KEGG Ortholog ID", "ko:K00001")
        )
    )

##################################################
    ui <- fluidPage(
        tags$head(
    tags$style(HTML("
                #controls {
                  background-color: #FFF;
                  padding: 0 20px 20px 20px;
                  cursor: move;
                  zoom: 0.9;
                  opacity: 0.5;
                   transition: opacity .25s ease-in-out;
                   -moz-transition: opacity .25s ease-in-out;
                   -webkit-transition: opacity .25s ease-in-out;
                }

                #controls:hover{
                    opacity: 1;
                  }
                "))
    ),
        #Plot
        sigma::sigmaOutput("network", width="100%", height="1000"),

        #Bar
        columnBar
    )
##################################################

##################################################
    server <- function(input, output){

        output$network <- renderSigma({
            sigma::sigma(gexf = agexf, drawLabels = TRUE, labelThreshold = 8)
        })
    }
##################################################
    shinyApp(ui, server)
}
