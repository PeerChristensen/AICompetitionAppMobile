library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(randomForest)
library(mlr)
library(shinyvalidate)
library(AzureStor)

# CONNECTING TO BLOB STORAGE
readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")
endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

### STYLING ###
gold  <- "#c39f5e"
theme <- bs_theme(
  bg = "black", fg = "white", primary = gold, secondary = gold,
  base_font = font_google("Open Sans"),
  heading_font = font_google("Ubuntu")
)

css <- HTML("
.html-widget.gauge svg {
  height: 400px;
  width: 800px;}

.js-irs-0 .irs-single, 
       .js-irs-0 .irs-bar-edge, 
       .js-irs-0 .irs-bar, 
       .js-irs-0 .irs-slider 
       {background: #c39f5e;}
       .irs-handle {background: #c39f5e !important;}")

### DATA ###
vars  <- read_rds("vars_attr.rds")
train <- read.csv("attr_train.csv") %>% 
  mutate_if(is.character,factor) 
test  <- read.csv("attr_test.csv") %>% 
  mutate_if(is.character,factor) 

### UI ###
ui <- fluidPage(theme = theme,
                useShinyjs(),
                tags$head(tags$style(css)),
                titlePanel("Kapacity AI Udfordring"),
                fluidRow(
                  h3("Kan du forudsige medarbejderflugt?")
                ),
                fluidRow(
                  p("Du er Data Scientist i en virksomhed der har et alvorligt problem med ansatte der forlader jer."),
                  p("Som et vigtigt led i jeres indsatser for at forebygge medarbejderflugt, skal du bygge en 'Random Forest' machine learning-model der forudsiger hvilke ansatte I risikerer at miste."),
                  p("Vælg en kombination af de rette variabler (features) der bedst forudsiger medarbejderflugt og indstil dine hyperparametre for at træne en model."),
                  p("Din score viser andelen af korrekte gæt på hvorvidt hver af de 616 ansatte forlader jeres virksomhed. På vores leaderboard, kan du se hvor god din model var i forhold til andres."),
                  p("Du er velkommen til at prøve flere gange, men kun dit første forsøg tæller med i konkurrencen.")
                ),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               pickerInput("vars", 
                                           label = tags$span(
                                             "Vælg features",
                                             helpText("Data er ikke gratis og der trækkes 1 procentpoint fra din score for hver ekstra variabel du inkluderer i din model efter de første tre.")),
                                           choices = vars, 
                                           multiple = TRUE
                                           #options = list(`max-options` = 4)
                               ),
                               sliderInput("mtry",
                                           label = tags$span(
                                             "max features/split", 
                                             helpText("Random Forest-algoritmen laver mange 'beslutningstræer' der består af regler. Her skal du bestemme hvor mange variabler træerne kan vælge imellem ved hver 'forgrening'. En forgrening fungerer som en regel og kunne f.eks. lyde: 'hvis ALDER ER OVER 50, SÅ...'"
                                             )),
                                           min = 1,
                                           max = 7,
                                           value = 3),
                               sliderInput("nodesize",
                                           label = tags$span(
                                             "min node size", 
                                             helpText("Her skal du beslutte hvor mange datapunkter der må være i hvert 'blad', dvs. efter sidste forgrening. Jo flere datapunkter, desto mindre bliver 'træerne'. For små træer kan lede til såkaldt 'underfitting', hvorimod for store træer kan lede til 'overfitting'."
                                             )),
                                           min = 1,
                                           max = 50,
                                           value = 25),
                               sliderInput("ntree",
                                           label = tags$span(
                                             "Antal træer", 
                                             helpText("Hvor mange beslutningstræer vil du bygge og aggregere? Flere træer kan øge din models performance. MEN, computerkraft koster, og der trækkes 0.01 procentpoint fra din score for hvert træ der bygges under træningen af din model."
                                             )),
                                           min = 1,
                                           max = 100,
                                           value = 50),
                               textInput("name", "Dit navn"),
                               textInput("mail", "E-mailadresse"),
                               textInput("company", "Virksomhed"),
                               textInput("initials", "Dine initialer (vises på leaderboard)"),
                               checkboxInput("confirm_mail_list", "Jeg accepterer at modtage relevante e-mails fra Kapacity *", FALSE),
                               checkboxInput("confirm", p("Ved at deltage, godkender jeg",tags$a(href="https://www.kapacity.dk/cookies/", "Kapacitys betingelser"),  "for opbevaring af mine data *"), FALSE),
                               column(12,
                                      # splitLayout(cellWidths = c("40%","40%"),
                                      actionButton("start","Start!"), 
                                      #actionButton("reset","Reset"),
                                      align = "center",
                                      style = "margin-top: 50px;")
                               #)
                  ), #sidebarPanel
                  mainPanel(column(4,
                    h3("Din score"),
                    gaugeOutput("gauge", height = "100%"),
                    fluidRow(
                      h3(textOutput("thanks",inline = TRUE),
                         align = "left",
                         style = "margin-left: 250px;")
                    )
                  ) # main panel
                  )
                ) # sidebar layout
) # page

### SERVER ###

server <- function(input, output, session) {
  
  
  # Validate input
  iv <- InputValidator$new()
  iv$add_rule("name", sv_required(message = "Påkrævet"))
  iv$add_rule("company", sv_required(message = "Påkrævet"))
  iv$add_rule("initials", sv_required(message = "Påkrævet"))
  iv$add_rule("mail", sv_email(message = "Indtast venligt en gyldig email"))
  iv$add_rule("confirm_mail_list", sv_equal(TRUE, 
                                            message_fmt = "Acceptér venligst for at deltage i konkurrencen"))
  iv$add_rule("confirm", sv_equal(TRUE, 
                                  message_fmt = "Godkend venligst for at deltage i konkurrencen"))
  iv$disable()
  
  observeEvent(input$start, {
    
    iv$enable()
    
    # Collect user data and score
    name       <- input$name
    company    <- input$company
    mail       <- input$mail
    initials   <- input$initials
    score      <- acc()
    permission_mail <- input$confirm_mail_list
    permission <- input$confirm
    time       <- now()
    
    
    data <- tibble(name,company,mail,initials,score, permission_mail, permission, time) %>%
      mutate(time = as.character(time)) %>%
      filter(permission==T, name != "", company!="",mail!="",initials!="")
    print(data)
    
    if (nrow(data) == 1) { 
      
      # write csv
      filename <- paste0(mail,".csv")
      write_csv(data, filename, col_names = F)
      
      # append to leaderboard
      storage_upload(container, src=filename, 
                     dest="leaderboard/leaderboard.csv", 
                     type="AppendBlob",
                     append=TRUE)
      
      # store backup file
      storage_upload(container, src=filename, dest=paste0("archive/", filename))
      
      # Send thanks
      delay(1500, 
            output$thanks <- renderText({"Tak for din deltagelse!"}))
      
      iv$disable()
      reset("vars")
      reset("mtry")
      reset("nodesize")
      reset("ntree")
      reset("name")
      reset("mail")
      reset("company")
      reset("initials")
      reset("confirm_mail_list")
      reset("confirm")
      
    } # if
  }) # observeEvent start
  
  # train model and get accuracy
  acc <- eventReactive(input$start,{
    
    set.seed(7223)
    train <- train %>% select(all_of(input$vars),Attrition)
    test <- test %>% select(all_of(input$vars),Attrition)
    task = makeClassifTask(data = train, target = "Attrition")
    base_clf = makeLearner("classif.randomForest", fix.factors.prediction = FALSE)
    tuned_clf = setHyperPars(base_clf, 
                             ntree = input$ntree,
                             mtry  = input$mtry, 
                             nodesize = input$nodesize)
    mod = mlr::train(tuned_clf, task)
    pred = predict(mod, newdata = test)
    n_vars = length(input$vars) - 8
    n_trees = input$ntree
    
    if (n_vars > 3) {
      (round(calculateROCMeasures(pred)$measures$acc,4)*100) - (n_vars * 1) - (n_trees * 0.01)
    } else {
      round(calculateROCMeasures(pred)$measures$acc,4)*100 - (n_trees * 0.01)
    }
    
  }) # eventReactive
  
  output$gauge <- renderGauge({
    
    gauge(acc(), 
          min = 0, 
          max = 100, 
          symbol="%",
          sectors = gaugeSectors(success = c(0, 40),
                                 warning = c(40, 60),
                                 danger = c(60, 100),
                                 colors = c(gold, gold, gold)
          )
    ) # gauge
  }) # renderGauge
  
  observeEvent(input$reset, {
    
    iv$disable()
    reset("vars")
    reset("mtry")
    reset("nodesize")
    reset("ntree")
    reset("name")
    reset("mail")
    reset("company")
    reset("initials")
    reset("confirm_mail_list")
    reset("confirm")
    
    output$gauge <- renderGauge({})
    
  }) # observeEvent reset
  
} # Server

# Run the application 
shinyApp(ui = ui, server = server)
