library(shiny)
library(arules)
library(arulesViz)

ui <- fluidPage(
  
  titlePanel("Hello Association Rules"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("sup", "Select the support value", choices = c(.5, .2, .1, .01,.05,.001,.005)),
      selectInput("conf", "Select the confidence", choices = c(.5,.6,.7,.8,.9)),
      selectInput("minlen", "Select the minlen", choices = c(1, 2, 3, 4))
    ),
    
    mainPanel(
      verbatimTextOutput("rulesTable"),
      plotOutput("rulesPlot")
    )
  )
)

server <- function(input, output) {
  
  #import and clean datasets
  
  data("AdultUCI", package = "arules")
  AU<- AdultUCI
  AU$age_grp <- discretize(AU$age, method = "frequency",
                           breaks = 4, labels = c("Young","Middle-aged","Senior","Old" ))
  AU$hrs_grp <- ordered(cut(AU[[ "hours-per-week"]],
                            c(0,25,40,60,168)),
                        labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
  AU$`education-num` = NULL 
  AU[["fnlwgt"]] = NULL
  AU[["age"]] = NULL
  AU[["hours-per-week"]] = NULL
  AU[["hpw"]] = NULL 
  AU$workclass = gsub ("Without-pay", "No-work", AU$workclass)
  AU$workclass = gsub ("Never-worked", "No-work", AU$workclass)
  AU$workclass = gsub ("Local-gov", "Other-gov", AU$workclass)
  AU$workclass = gsub ("State-gov", "Other-gov", AU$workclass)
  AU$workclass = gsub ("Self-emp-inc", "Self-employed", AU$workclass)
  AU$workclass = gsub ("Self-emp-not-inc", "Self-employed", AU$workclass)
  AU$education = gsub("10th","Drop-out",AU$education)
  AU$education = gsub("11th","Drop-out",AU$education)
  AU$education = gsub("12th","Drop-out",AU$education)
  AU$education = gsub("1st-4th","Drop-out",AU$education)
  AU$education = gsub("5th-6th","Drop-out",AU$education)
  AU$education = gsub("7th-8th","Drop-out",AU$education)
  AU$education = gsub("9th","Drop-out",AU$education)
  AU$education = gsub("^Preschool","Drop-out",AU$education)
  AU$education = gsub("HS-Grad","HS-Grad",AU$education)
  AU$education = gsub("Some-college","HS-Grad",AU$education)
  AU$education = gsub("Prof-school","Prof-School",AU$education)
  AU$education = gsub("Assoc-acdm","Associates",AU$education)
  AU$education = gsub("Assoc-voc","Associates",AU$education)
  AU$education = gsub("Bachelors","Bachelors",AU$education)
  AU$education = gsub("Doctorate","Doctorate",AU$education)
  AU$education = gsub("Masters","Masters",AU$education)
  names(AU)[3] <- "marital"
  AU[["capital-gain"]] <- ordered(cut(AU$`capital-gain`, c(-Inf, 0,
                                                           median(AU[["capital-gain"]][AU[["capital-gain"]]>0]),
                                                           Inf)), labels = c("None", "Low-gain", "High-gain"))
  AU[["capital-loss"]] <- ordered(cut(AU$`capital-loss`, c(-Inf, 0,
                                                           median(AU[["capital-loss"]][AU[["capital-loss"]]>0]),
                                                           Inf)), labels = c("None", "Low-loss", "High-loss"))
  AU_clean = na.omit(AU)
  AU_clean$workclass <- as.factor(AU_clean$workclass)
  AU_clean$education <- as.factor(AU_clean$education)
  
  #run association rule
  output$rulesTable <- renderPrint({
    rules<- apriori(AU_clean, parameter = list(support = as.numeric(input$sup), confidence = as.numeric(input$conf), minlen = as.numeric(input$minlen)))
    inspect(head(rules,10))
})
  
  output$rulesPlot <- renderPlot({
    rules<- apriori(AU_clean, parameter = list(support = as.numeric(input$sup), confidence = as.numeric(input$conf), minlen = as.numeric(input$minlen)))
    plot(rules, measure = c("support", "lift"), shading = "confidence")
  })
}

shinyApp(ui = ui, server = server)
