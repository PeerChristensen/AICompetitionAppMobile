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
endpoint <-
	storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas =
									 	sas_token)
container <- storage_container(endpoint, "aicompetition")

### STYLING ###
gold  <- "#c39f5e"
blue  <- "#85c8f0"
padding_sides <- "padding"

theme <- bs_theme(
	#bg = "white",
	#fg = "black",
	primary = blue,
	secondary = blue,
	#base_font = font_google("Open Sans"),
	#heading_font = font_google("Ubuntu"),
)


# colouring remaining gauge bavkground
# this currently colours the entire area

#.html-widget.gauge svg path {
#fill:#c39f5e;
#  fill: #f9f5ef}

css <- HTML(
	"
.html-widget.gauge svg {
  height: 300px;
  width: 400px;
	padding-left:-50px;
padding-right:50px;}

.js-irs-0 .irs-single,
       .js-irs-0 .irs-bar-edge,
       .js-irs-0 .irs-bar,
       .js-irs-0 .irs-slider
       {background: #85c8f0;}
       .irs-handle {background: #85c8f0 !important;}

#title_panel {
  background-color: black;
  background: black;
  color: white;
  font-size: 30px;
  margin:-1em;
  padding: 1em;
	padding-left:25px;}

.form-control {
  border-radius: 25px 25px 25px 25px;
  border: 2px solid black}

#sidebar {
  background-color: white;}
"
)

black_style <- "
padding:1em;
background-color:
black;color:white;
font-family: 'Open Sans';
font-size: 18px !important;
line-height: 1.5;
"

black_style_header <- "
padding:0em;
background-color:black;
color:white;
font-family: 'Ubuntu' !important;
font-weight:300 !important;
font-size: 50px !important;
padding-top: 1em;
"

black_style_title <- "
font-family: 'Ubuntu' !important;
font-weight:300 !important;
padding-left: 25px;
"

white_style <- "
margin:0em;
padding:2em;
background-color:white;
color:black;
font-family: 'Open Sans' !important;
font-size: 18px !important;
line-height: 1.5;
padding-bottom: 0em !important;
"

white_style_header <- "
margin:0em;
background-color:white;
color:black;
font-family: 'Ubuntu' !important;
font-weight: 300 !important;
font-size: 32px !important;
padding-top: 1em;
padding-bottom: 0em;
text-align:center;
"

white_style_header2 <- "
margin:0em;
padding:1em;
background-color:white;
color:black;
font-family: 'Ubuntu' !important;
font-weight:300 !important;
font-size: 32px !important;
padding-top: 1em;
"
rendered_text <- "
padding-top:-15px;
margin-bottom:20px;
background-color:white;
padding-left:10px;
color:black;
font-family: 'Open Sans' !important;
font-size: 20px !important;
line-height: 1.5;

"

button_style <- "
background-color: #85c8f0; 
border-color: #85c8f0; 
border-radius: 12px;
font-family: 'Open Sans';
font-size: 18px !important;
"


### DATA ###
vars  <- read_rds("vars_attr.rds")
train <- read.csv("attr_train.csv") %>%
	mutate_if(is.character, factor)
test  <- read.csv("attr_test.csv") %>%
	mutate_if(is.character, factor)

### UI ###
ui <- fluidPage(
	theme = theme,
	useShinyjs(),
	tags$head(tags$style(css),tags$title('kapacity AI konkurrence')),
	
	# TITLE
	titlePanel(p(id = "title_panel", "kapacity",style=black_style_title)),
	
	# HEADER
	fluidRow(
		column(12,align = "center",style = black_style_header,
					 p("AI konkurrence!")
		)
	),
	
	# INTRO
	fluidRow(
		column(12,align="center",style = black_style,
					 p("Kan du træne vores AI-model, så du stopper medarbejderflugten i din virksomhed?")
		)
	),
	fluidRow(
		column(12, align="center", style = black_style,
					 p("Vi belønner dagens bedste forsøg med en champagne-smagekasse til en værdi af kr. 2.899,-")
		)
	),
	
	# INSTRUCTIONS
	fluidRow(style="margin:0em:",
					 column(12,
					 			 p("Start her!",style = white_style_header)
					 			 ),
	),
	fluidRow(style="margin:0em:",
					 column(12,
					 			 style = white_style,
					 			 p(
					 			 	"Forestil dig at 40% af dine medarbejdere overvejer at opsige deres stilling. Din opgave er at træne en AI-model, der kan forudsige, hvilke ansatte det drejer sig om, så opsigelserne kan undgås."),
					 			 p(
					 			 	"Vælg en kombination af de rette variabler (features) der bedst forudsiger medarbejderflugt og indstil dine hyperparametre for at træne en model."    ),
					 			 p(
					 			 	"Din score viser andelen af korrekte gæt på hvorvidt hver af de 616 ansatte forlader jeres virksomhed. På vores leaderboard, kan du se hvor god din model var i forhold til andres."    ),
					 			 p(
					 			 	"Du er velkommen til at prøve flere gange, men kun det bedste af dine tre første forsøg tæller med i konkurrencen."
					 			 ),
					 			 p(
					 			 	"Deltagelse forudsætter tilmelding til Kapacitys nyhedsmail om AI."
					 			 ),
					 )
	),
	hr(),
	
	sidebarLayout(
		
		# SIDEBAR
		sidebarPanel(id="sidebar",
								 width = 12,
								 textInput("name", label=NULL, placeholder = "Dit navn"),
								 textInput("mail", label=NULL, placeholder = "E-mailadresse"),
								 textInput("initials", label = NULL, placeholder = "Dine initialer (vises på leaderboard)"),
								 checkboxInput(
								 	"confirm_mail_list",
								 	"Jeg accepterer at modtage relevante e-mails fra Kapacity *",
								 	FALSE
								 ),
								 checkboxInput(
								 	"confirm",
								 	p(
								 		"Jeg godkender",
								 		tags$a(href = "https://www.kapacity.dk/cookies/", "Kapacitys betingelser"),
								 		"for opbevaring af mine data *"
								 	),
								 	FALSE
								 ),
								 pickerInput(
								 	"vars",
								 	label = tags$span(
								 		"Vælg features",
								 		helpText("Data er ikke gratis og der trækkes 1 procentpoint fra din score for hver ekstra variabel du inkluderer i din model efter de første tre.")
								 	),
								 	choices = vars,
								 	multiple = TRUE,
								 	options = list(
								 		`none-selected-text` = "Intet valgt"
								 	)
								 	#options = list(`max-options` = 4)
								 ),
								 sliderInput(
								 	"mtry",
								 	label = tags$span(
								 		"Maksimum features/split",
								 		helpText("Random Forest-algoritmen laver mange 'beslutningstræer' der består af regler. Her skal du bestemme hvor mange variabler træerne kan vælge imellem ved hver 'forgrening'. En forgrening fungerer som en regel og kunne f.eks. lyde: 'hvis ALDER ER OVER 50, SÅ...'")
								 	),
								 	min = 1,
								 	max = 7,
								 	value = 3
								 ),
								 sliderInput(
								 	"nodesize",
								 	label = tags$span(
								 		"Minimum node size",
								 		helpText("Her skal du beslutte hvor mange datapunkter der må være i hvert 'blad', dvs. efter sidste forgrening. Jo flere datapunkter, desto mindre bliver 'træerne'. For små træer kan lede til såkaldt 'underfitting', hvorimod for store træer kan lede til 'overfitting'.")
								 	),
								 	min = 1,
								 	max = 50,
								 	value = 25
								 ),
								 sliderInput(
								 	"ntree",
								 	label = tags$span(
								 		"Antal træer",
								 		helpText("Hvor mange beslutningstræer vil du bygge og aggregere? Flere træer kan øge din models performance. MEN, computerkraft koster, og der trækkes 0.01 procentpoint fra din score for hvert træ der bygges under træningen af din model.")
								 	),
								 	min = 1,
								 	max = 100,
								 	value = 50
								 ),
								 column(
								 	12,
								 	splitLayout(
								 		cellWidths = c("40%", "40%"),
								 		actionButton("start", "Start!", style = button_style),
								 		actionButton("reset", "Nulstil", style = button_style),
								 		align = "center",
								 		style = "margin-top: 50px;"
								 	)
								 )
		), #sidebarPanel
		
		# OUTPUT
		mainPanel(
			fluidRow(
			column(4,align="left",
			 p(
				"Din score", style = white_style_header2
			 ),
			 gaugeOutput("gauge", height = "30%")
			),
			column(8)
		),
		fluidRow(column(12,
											p(
												textOutput("result_tp", inline = TRUE),
												#align = "center",
												style = rendered_text)
			)
			),
			fluidRow(column(12,
											p(
												textOutput("result_fp", inline = TRUE),
												#align = "center",
												style =  rendered_text)
			)
			),
			fluidRow(column(8,
											p(
												textOutput("thanks", inline = TRUE),
												#align = "center",
												style = rendered_text)
			)
			)
			
		) # main panel
	), # sidebar layout
	
	# add space at bottom
	fluidRow(h1("")), 
	fluidRow(h1(""))
) # page

### SERVER ###

server <- function(input, output, session) {
	# Validate input
	iv <- InputValidator$new()
	iv$add_rule("name", sv_required(message = "Påkrævet"))
	iv$add_rule("initials", sv_required(message = "Påkrævet"))
	iv$add_rule("mail", sv_email(message = "Indtast venligt en gyldig email"))
	iv$add_rule(
		"confirm_mail_list",
		sv_equal(TRUE,
						 message_fmt = "Acceptér venligst for at deltage i konkurrencen")
	)
	iv$add_rule(
		"confirm",
		sv_equal(TRUE,
						 message_fmt = "Godkend venligst for at deltage i konkurrencen")
	)
	iv$disable()
	
	observeEvent(input$start, {
		iv$enable()
		
		# Train model and get results
		set.seed(7223)
		train <- train %>% select(all_of(input$vars), Attrition)
		test  <- test  %>% select(all_of(input$vars), Attrition)
		task  <- makeClassifTask(data = train, target = "Attrition")
		base_clf  <- makeLearner("classif.randomForest", fix.factors.prediction = FALSE)
		tuned_clf <- setHyperPars(
			base_clf,
			ntree = input$ntree,
			mtry  = input$mtry,
			nodesize = input$nodesize
		)
		mod <- mlr::train(tuned_clf, task)
		pred <- predict(mod, newdata = test)
		n_vars <- length(input$vars)
		n_trees <- input$ntree
		
		# detailed results
		true_pos  <- pred %>% as_tibble() %>% filter(truth == "Yes" & response == "Yes") %>% nrow()
		false_pos <- pred %>% as_tibble() %>% filter(truth == "No"  & response == "Yes") %>% nrow()
		all_pos   <- pred %>% as_tibble() %>% filter(truth == "Yes") %>% nrow()
		all_neg   <- pred %>% as_tibble() %>% filter(truth == "No") %>% nrow()
		
		result_text_tp <- paste0("Din model var i stand til at finde ", true_pos, " ud af ", all_pos, " ansatte, der var på vej væk.")
		result_text_fp <- paste0("I ", false_pos, " ud af ", all_neg, " tilfælde, hvor medarbejderne blev i virksomheden, gættede modellen forkert")
		
		if (n_vars > 3) {
			acc <- (round(calculateROCMeasures(pred)$measures$acc, 4) * 100) - ((n_vars-3) * 1) - (n_trees * 0.01)
		} else {
			acc <- round(calculateROCMeasures(pred)$measures$acc, 4) * 100 - (n_trees * 0.01)
		}
		
		# Collect user data and score
		name       <- input$name
		mail       <- input$mail
		initials   <- input$initials
		score      <- acc
		permission_mail <- input$confirm_mail_list
		permission <- input$confirm
		time       <- now()
		
		data <-
			tibble(name,
						 mail,
						 initials,
						 score,
						 permission_mail,
						 permission,
						 time) %>%
			mutate(time = as.character(time)) %>%
			filter(permission == T, name != "", mail != "", initials != "")
		print(data)
		
		if (nrow(data) == 1) {
			
			# write csv
			filename <- paste0(mail, ".csv")
			write_csv(data, filename, col_names = F)
			
			# append to leaderboard
			storage_upload(
				container,
				src = filename,
				dest = "leaderboard/leaderboard.csv",
				type = "AppendBlob",
				append = TRUE
			)
			
			# store backup file
			storage_upload(container,
										 src = filename,
										 dest = paste0("archive/", filename))
			
			
			delay(1500,
						output$gauge <- renderGauge({
							gauge(
								acc,
								min = 0,
								max = 100,
								symbol = "%",
								sectors = gaugeSectors(
									success = c(0, 40),
									warning = c(40, 60),
									danger = c(60, 100),
									colors = c(gold, gold, gold)
								)
							) # gauge
						}) # renderGauge
			) # delay
			
			# show results text
			delay(3000,
						output$result_tp <-
							renderText({
								result_text_tp
							})
			)
			
			delay(4500,
						output$result_fp <-
							renderText({
								result_text_fp
							}))
			
			# Send thanks
			delay(6000,
						output$thanks <-
							renderText({
								"Tak for din deltagelse!"
							}))
			
		} # if nrow data == 1
	}) # observeEvent start
	
	observeEvent(input$reset, {
		iv$disable()
		reset("vars")
		reset("mtry")
		reset("nodesize")
		reset("ntree")
		reset("name")
		reset("mail")
		reset("initials")
		reset("confirm_mail_list")
		reset("confirm")
		
		output$gauge <- renderGauge({})
		
		output$result_tp <-renderText({})
		
		output$result_fp <-renderText({})
		
		output$thanks <- renderText({})
		
	}) # observeEvent reset
	
} # Server

# Run the application
shinyApp(ui = ui, server = server)
