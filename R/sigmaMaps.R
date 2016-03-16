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
originalColor = V(igraphObj)$color
colorType <- c("centrality", "betweeness", "none") %>%
        setNames(c("Centrality", "Betweeness", "none"))

columnBar <- shiny::absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = 300,
        h2("Metabolic Explorer"),

        selectInput("labelType", "Labels", c("KO", "CPD")),

        selectInput("color", "Color", colorType, selected="none"),
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
                   transition: opacity .25s ease-in-out;[]
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
       
#        color.bar(colorRampPalette(c("#e41a1c", "#377eb8", "#4daf4a"))(100), -1)
        reactiveGraph = reactive({
            if(input$color == "betweeness"){
            values = sqrt(igraph::betweenness(igraphObj))
            V(igraphObj)$color = drawGradient(values, colors = c("#e41a1c", "#377eb8", "#4daf4a"))
            }else if(input$color == "centrality"){
                values = sqrt(igraph::closeness(igraphObj))
                V(igraphObj)$color = drawGradient(values, colors = c("#e41a1c", "#377eb8", "#4daf4a"))
            }else{
                V(igraphObj)$color = originalColor
            }
            igraphObj
        })

        output$network <- renderSigma({
            agexf <- reactiveGraph() %>% igraph2gexf %$% graph
            sigma::sigma(gexf = agexf, drawLabels = TRUE, labelThreshold = 8)
        })
    }
##################################################
    shinyApp(ui, server)
}

#' drawGradient gives the color gradient
#'
#' sets the middle color at the median
#'
#' @param values the score
#' @param colors the color
drawGradient = function(values, colors) {
    mvalue = median(values, na.rm=T)

    lowerMid = colorRampPalette(colors[1:2])
    midUpper= colorRampPalette(colors[2:3])
    #20 colors from btm to top; spread be taken

    lowerV = cut(values[which(values < mvalue)], breaks=10)
    levels(lowerV) = lowerMid(10)

    upperV = cut(values[which(values > mvalue)], breaks=10)
    levels(upperV) =  midUpper(10)

    colorVector = values
    colorVector[which(values <= mvalue)] = as.character(lowerV)
    colorVector[which(values > mvalue)] = as.character(upperV)


    colorVector[is.na(colorVector)] = "#000000"
    colorVector
}
