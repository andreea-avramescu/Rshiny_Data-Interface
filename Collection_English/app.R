library(shiny)
library(shinyBS)
library(jsonlite)
library(shinymanager)
library(dplyr)
library(magrittr)
library(DT)
library(shinyWidgets)
library(shinyalert)
library(V8)

steps <- read.csv("www/help.csv")

countries <- list(
  "Algeria", "Australia", "Bangladesh", "Bosnia and Herzegovina", "Brazil", "Cambodia", "Colombia", "Congo", "Egypt", "El Salvador", "Germany", 
  "Greece", "India", "Israel", "Jordan", "Kenya", "Lebanon", "Morocco", "Mozambique", "Palestine",
  "Rwanda", "Senegal", "South Africa", "South Sudan",
  "Sudan", "Sweden", "Syria", "Tanzania", "Tunisia", "United Kingdom", "Uganda", "Venezuela", "Zambia", "Zimbabwe"
)
flags <- c("dz", "au", "bd", "ba", "br", "kh", "co", "cd", "eg", "sv", "de", "gr", "in", "il", "jo", "ke", "lb", "ma", "mz", "ps", "rw", "sn", "za", "ss", "sd", "se", "sy", "tz", "tn", "gb", "ug", "ve", "zm", "zw")
flags <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", flags)


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = c("user"),
  password = c("password"),
  stringsAsFactors = FALSE
)

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

outputDir <- "output"

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# Extend shinyjs to scroll
jsCode <- "shinyjs.scrolltop = function() {instructions_description.scrollTo(0, 0); actions_task.scrollTo(0, 0)};"


# In the below function we need the values for ALL questions
fieldsAll <- c("list_with_questions_values")

# In the below function we put the MANDATORY fields
fieldsMandatory <- c("organisation_name", "organisation", "country", "date_from", "gender", "age", "activity1", "functiona", "function_other",
                     "activity", "pers_income", "activity_other", "time", "currency", "fin_support_time", "fin_support_tot", "fin_support_year3",
                     "fin_support_year2", "fin_support_year1", "fin_support_1", "hours", "post_hours", "pers_income","opportunity1",
                     "volunteer", "other_funding", "ipow_funding_imp",
                     "ipow_funding_diff", "ipow_funding_comm", "ipow_funding_depend", "ipow_funding_time",
                     "ipow_funding_future", "sustainable", "activities", "activities_num", "activities_reason",
                     "activities_fund", "extra_funding", "activities_change", "part_men", "part_women",
                     "part_neutral", "part_age", "part_status",
                     "part_continue", "part_words", "part_change", "participation_benefit", "skills",
                     "skills_where", "resources", "continue_part", "apprentice", "needs_community", "needs_find",
                     "needs_met", "more_part",
                     "success_words", "participants_change", "training", "training_long_term")

ui = secure_app(head_auth = tags$script(inactivity),
                fluidPage(
                  id = "questionnaire",
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  shinyjs::extendShinyjs(text = jsCode, functions = c("pageCol")),
                  title = "--Company-- - Change Maker Questionnaire",
                  div(id = "header",
                      h2("--Company-- - Change Maker Questionnaire")
                  ),
                  fluidRow(
                    column(12,
                           div(
                             id = "form",
                             tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante."),
                             
                             tags$p(style = 'font-weight:bold; text-decoration: underline; font-size: medium;',
                                    "About the Research"
                             ),
							 
                             tags$p(style = 'font-weight:bold;', "What is the purpose of the research?"),
                             tags$div(
                               tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante."),
                               
                               
                               tags$p(style = 'font-weight:bold;', "Who was this developed by?"),
                               tags$div("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante.",
                                        tags$br(),
                                        tags$ol(
                                          tags$li("Mr ..."),
                                          tags$li("Ms ..."),
                                          tags$li("Ms ..."),
                                          tags$li("Ms ..."),
                                          tags$li("Ms ..."),
                                          tags$li("Ms ..."),
                                          tags$li("Ms ..."))),
                             
                             tags$p(style = 'font-weight:bold;',
                                    "Will the outcomes of the research be published?"),
                             tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante."),
                          
                           
                             tags$p(style = 'font-weight:bold; text-decoration: underline; font-size: medium;',
                                    "Your involvement"
                             ),
                             
                             tags$p(style = 'font-weight:bold;',
                                    "What would I be asked to do?"
                             ),
                             tags$div(
                               tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie."),
                               tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante."),
                               tags$p("purus a neque. Vivamus at consequat dolor. Cras at lectus tortor. Integer consectetur velit et urna commodo molestie. 
                                    Nunc velit magna, rutrum nec auctor quis, vulputate vel augue. Duis eu imperdiet turpis. Nulla facilisi. Orci varius 
                                    natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante.")),
                             
                             tags$p(style = 'font-weight:bold;',
                                    "What happens if I do not want to take part or if I change my mind? "),
                             tags$p("Nam sollicitudin sapien eu mauris lacinia, quis posuere tortor lacinia. Mauris pulvinar ut nisl at consequat. In imperdiet 
                                    sem non faucibus luctus. Proin quis nisi congue, varius ante id, gravida arcu. Duis risus odio, ullamcorper sit amet sodales 
                                    ut, condimentum quis felis. Maecenas hendrerit diam libero, vel congue mauris luctus sed. Sed semper tellus quis augue 
                                    fermentum lacinia a id neque. Cras justo erat, pellentesque ac cursus eu, tempor eu purus."),
                             
                             tags$p(style = 'font-weight:bold; text-decoration: underline; font-size: medium;',
                                    "Data Protection and Confidentiality"),
                             tags$p(style = 'font-weight:bold;',
                                    "What information will you collect about me?"),
                             tags$div(
                               tags$p("Proin interdum quam sed urna lobortis aliquam. Nam risus erat, dapibus pharetra tempor sed, maximus id risus. Vestibulum pharetra feugiat blandit. 
                                      Fusce eleifend massa vitae arcu volutpat, sed aliquam tellus maximus. Vivamus augue massa, convallis ut elit quis, mattis lacinia turpis."),
                               tags$ul(
                                 tags$li("Detail 1"),
                                 tags$li("Detail 2"),
                                 tags$li("Detail 3"),
                                 tags$li("Detail 4"),
                                 tags$li("Detail 5")),
                               tags$p(style = 'font-weight:bold;', "Under what legal basis are you collecting this information"),
                               tags$p("Proin interdum quam sed urna lobortis aliquam. Nam risus erat, dapibus pharetra tempor sed, maximus id risus. 
                                      Vestibulum pharetra feugiat blandit. Fusce eleifend massa vitae arcu volutpat, sed aliquam tellus maximus.
                                      Vivamus augue massa, convallis ut elit quis, mattis lacinia turpis.")),
                             
                             tags$p(style = 'font-weight:bold;', "What are my rights in relation to the information you will collect about me?"),
                             tags$div(
                               tags$p("Proin interdum quam sed urna lobortis aliquam. Nam risus erat, dapibus pharetra tempor sed, maximus id risus. Vestibulum 
                                      pharetra feugiat blandit. Fusce eleifend massa vitae arcu volutpat, 
                                      sed aliquam tellus maximus. Vivamus augue massa, convallis ut elit quis, mattis lacinia turpis."),
                               tags$p("If you would like to know more about your different rights or the way we use your personal information to ensure we follow the law, please consult our ", tags$a(href="http://documents.manchester.ac.uk/display.aspx?DocID=37095", "Privacy Notice for Research Participants", target="_blank"), ".")),
                             
                             tags$p(style = 'font-weight:bold;', "Will my participation in the study be confidential and my personal identifiable information be protected?"),
                             tags$div(
                               tags$p("Nullam tristique tellus ac posuere vulputate."),
                               tags$p("Ut et libero ex. Integer urna urna, volutpat eget tellus non, varius venenatis lectus. Praesent non ex commodo arcu tincidunt 
                               eleifend non nec nisi. Suspendisse sit amet consectetur neque, at rutrum nunc. Etiam eget magna gravida, sodales diam ultricies, euismod magna. 
                               Pellentesque eget arcu maximus, commodo orci sit amet, commodo enim. Ut vestibulum, lacus a sollicitudin egestas, nunc lorem sodales purus, 
                               id dignissim purus ipsum non purus. Nam a blandit lectus, eu egestas lacus. 
                                      Nullam in suscipit nulla, eget sagittis orci. Donec elementum leo odio, eget commodo dolor tempus quis.")),
                             
                             tags$p(style = 'font-weight:bold;',
                                    "What if I have a complaint?"),
                             tags$div(
                               tags$p("Contact details for complaints:"),
                               tags$p("Nullam tristique tellus ac posuere vulputate."),
                               tags$ul(
                                 tags$li(style = 'font-weight:bold;', "Details"),
                                 tags$li(style = 'font-weight:bold;',"Details")),
                               ),
                                      "Please read the following items and click the box at the bottom of the page if you agree."
                             ),
                             tags$br(),
                             tags$div(style = 'margin-bottom: 5px;',
                                      tags$ul(
                                        tags$li("Quisque iaculis augue quis porta luctus."),
                                        tags$li("Quisque iaculis augue quis porta luctus."),
                                        tags$li("Quisque iaculis augue quis porta luctus."),
                                        tags$li("Quisque iaculis augue quis porta luctus."),
                                        tags$li("Quisque iaculis augue quis porta luctus."),
                                        tags$li("Quisque iaculis augue quis porta luctus."))),
                             
                             tags$div(style = 'text-align: center; font-size: larger; font-weight:bold;',
                                      shinyWidgets::awesomeCheckbox(
                                        inputId = "agree",
                                        label = "I agree to take part in this study.",
                                        value = F
                                      )),
                             shiny::textInput('name_id', label= "Please enter your full name below", ""),
                             tags$br(),
                             tags$br(),
                             tags$div(style = "text-align: center; position: absolute; left: 20%; bottom: 10px; max-width: 300px;",
                                      #button to end trial
                                      shiny::actionButton(inputId = 'consent',
                                                          label = 'Submit Information and Continue',
                                                          width = '100%',
                                                          class="fixed-bottom",
                                                          style= "display: inline; color: white; font-weight: 400; font-size: 15px; background-color: #4C2984;"),
                                      #hidden message that only appears when the submit button is clicked and the checkbox is not clicked
                                      shinyjs::hidden(
                                        tags$div(id='error_msg_info',
                                                 tags$p(tags$span(style = "color:red",
                                                                  "You must agree (by clicking) the box in the 'Agreement' to continue"))
                                        )
                                        
                                      ),#ends hidden
                                      #hidden message that only appears when the submit button is clicked and the checkbox is not clicked
                                      shinyjs::hidden(
                                        tags$div(id='error_msg_signin',
                                                 tags$p(tags$span(style = "color:red",
                                                                  "You must enter your full name above to continue."))
                                        )
                                        
                                      ),#ends hidden
                                      
                                      #hidden message that only appears when the submit button is clicked and the checkbox is not clicked
                                      shinyjs::hidden(
                                        tags$div(id='error_msg_both_intro',
                                                 tags$p(tags$span(style = "color:red",
                                                                  "You must enter your Name above and click the box in the 'Agreement' to continue."))
                                        )
                                      ),
                             )
                             
                           ),
                           
                    ),
                    shinyjs::hidden(
                      div(id = "submit_info", align = "center",
                          # Include IntroJS styling
                          includeCSS("www/introjs.css"),
                          
                          # Include IntroJS library
                          includeScript("www/intro.js"),
                          
                          # Include javascript code to make shiny communicate with introJS
                          includeScript("www/app.js"),
                          
                          # Include IntroJS styling
                          includeCSS("www/introjs.min.css"),
                          
                          # Include IntroJS library
                          includeScript("www/intro.min.js"),
                          
                          setBackgroundColor("MintCream"),
                          #section 1 -----
                          tags$h2("Section 1", style = "font-family: 'Impact', cursive; font-weight: 500; line-height: 1.1; color: #209e9a;"),
                          
                          verbatimTextOutput(outputId = "text"),
                    
                          textInput("organisation_name", labelMandatory("1. Lorem ipsum dolor sit amet, consectetur adipiscing elit.")),
                          
                          textInput("organisation", labelMandatory("2. Lorem ipsum dolor sit amet, consectetur adipiscing elit.")),
                          selectInput(
                            inputId =  "date_from", labelMandatory("3. Lorem ipsum dolor sit amet, consectetur adipiscing elit?"), choices = 1950:as.numeric(format(Sys.Date(),"%Y"))),
                          
                          tags$div(id="step1",
                                   multiInput(
                                     inputId = "country", labelMandatory("4. Lorem ipsum dolor sit amet, consectetur adipiscing elit:"),
                                     choices = NULL,
                                     choiceNames = lapply(seq_along(countries),
                                                          function(i) tagList(tags$img(src = flags[i],
                                                                                       width = 20,
                                                                                       height = 15), countries[i])),
                                     choiceValues = countries
                                   )),
                          
                                   selectInput("gender", labelMandatory("5. Gender"),
                                               c("",  "Female", "Male", "Other", "Prefer not to say")),
                          
                          awesomeCheckboxGroup(
                            inputId = "functiona",
                            label = labelMandatory("8. Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                            choices = c("Choice 1",
                                        "Choice 2",
                                        "Choice 3",
                                        "Choice 4",
                                        "Choice 5",
                                        "Other")),
                          # theme styling ####
                          tags$head(tags$style(HTML('#functiona :after, #functiona :before{background-color:#209e9a;}'))),
                          
                          textInput("function_other", labelMandatory("If selected, please specify which 'other'")),
                          
                          # theme styling ####
                          tags$head(tags$style(HTML('#activity :after, #activity :before{background-color:#209e9a;}'))),
                          
                          textInput("activity_other",labelMandatory("If selected, please specify which 'other'")),
                          
                          tags$div(id="step2",
                          radioGroupButtons(
                            inputId = "time",
                            label = labelMandatory("10. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus?"),
                            choices =  c("Choice 1", "Choice 2", "Choice 3", "Choice 4", "Choice 5", "Choice 6", "Choice 7", "Choice 8"),
                            # status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))),
                          # theme styling ####
                          tags$head(tags$style(HTML('#time :after, #time :before{background-color:#209e9a;}'))),
                          
                          #section 2 -----
                          tags$h2("Section 2", style = "font-family: 'Impact', cursive; font-weight: 500; line-height: 1.1; color: #4B0082;"),
                          
                          textInput("fin_support_time", labelMandatory("1. Duis eu imperdiet turpis")),
                          
                          radioGroupButtons(
                            inputId = "time",
                            label = labelMandatory("10. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus?"),
                            choices =  c("Choice 1", "Choice 2", "Choice 3", "Choice 4", "Choice 5", "Choice 6", "Choice 7", "Choice 8"),
                            # status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
                          
                          radioGroupButtons(
                            inputId = "time",
                            label = labelMandatory("10. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus?"),
                            choices =  c("Choice 1", "Choice 2", "Choice 3", "Choice 4", "Choice 5", "Choice 6", "Choice 7", "Choice 8"),
                            # status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
                          
                          tags$div(id="step4",
                          switchInput(
                            inputId = "opportunity1",
                            label = labelMandatory("6a. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante?"),
                            onLabel = "Yes, please answer questions x and/or y below",
                            offLabel = "No, please skip to question z",
                            onStatus = "success",
                            offStatus = "danger"
                          )),
                          
                          # theme styling ####
                          tags$head(tags$style(HTML('#opportunity2 :after, #opportunity2 :before{background-color:#4B0082;}'))),
                          
                          
                          textInput("opportunity2_other", "If selected, please specify which 'other'"),
                          
                          tags$div(id="step5",
                          radioGroupButtons(
                            inputId = "income_change",
                            label = "6c. Vestibulum id justo ante?",
                            choices =  c("Choice 1", "Choice 2", "Choice 3", "Choice 4"),
                            status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))),
                          
                          textInput("volunteer_resp", "7d. Cras justo erat, pellentesque ac cursus eu, tempor eu purus?"),
                          
                          tags$b("8. Ut et libero ex?"),
                          
                         tags$div(id="step6",
                          knobInput(
                            inputId = "perc_employees", label = "First:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082")),
                          
                          knobInput(
                            inputId = "perc_rent", label = "Second:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                          
                          knobInput(
                            inputId = "perc_equipment", label = "Third:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                          
                          knobInput(
                            inputId = "perc_donation", label = "Fourth:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                          
                          knobInput(
                            inputId = "perc_events", label = "Fifth:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                          
                          knobInput(
                            inputId = "perc_training", label = "Sixth:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                          
                          textInput("perc_other", "Other"),
                          
                          textInput("budget", "9. Maecenas hendrerit diam libero, vel congue mauris luctus sed."),
                          fileInput("budget_upload", "Choose file"),
                          
                          #section 3 ------
                          tags$h2("Section 3", style = "font-family: 'Impact', cursive; font-weight: 500; line-height: 1.1; color: #DC143C;"),
                          
                         awesomeCheckboxGroup(
                           inputId = "functiona",
                           label = labelMandatory("8. Lorem ipsum dolor sit amet, consectetur adipiscing elit."),
                           choices = c("Choice 1",
                                       "Choice 2",
                                       "Choice 3",
                                       "Choice 4",
                                       "Choice 5",
                                       "Other")),
                          # theme styling ####
                          tags$head(tags$style(HTML('#other_funding :after, #other_funding :before{background-color:#DC143C;}'))),
                          
                         tags$div(id="step4",
                                  switchInput(
                                    inputId = "opportunity1",
                                    label = labelMandatory("6a. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante?"),
                                    onLabel = "Yes, please answer questions x and/or y below",
                                    offLabel = "No, please skip to question z",
                                    onStatus = "success",
                                    offStatus = "danger"
                                  )),
                          
                          textInput("ipow_funding_diff2", "3b. If yes, please explain how"),
                          
                         switchInput(
                           inputId = "opportunity1",
                           label = labelMandatory("6a. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante?"),
                           onLabel = "Yes, please answer questions x and/or y below",
                           offLabel = "No, please skip to question z",
                           onStatus = "success",
                           offStatus = "danger"
                         ),
                          
                          textInput("ipow_funding_comm2", "4b. If yes, please explain how"),
                          
                         switchInput(
                           inputId = "opportunity1",
                           label = labelMandatory("6a. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum id justo ante?"),
                           onLabel = "Yes, please answer questions x and/or y below",
                           offLabel = "No, please skip to question z",
                           onStatus = "success",
                           offStatus = "danger"
                         ),
                          
                          textInput("ipow_funding_help2", "5b. If yes, please tell us how."),
                          
                          knobInput(
                            inputId = "perc_employees", label = "First:", value = 0, min = 0,
                            displayPrevious = TRUE, lineCap = "round", fgColor = "#4B0082", inputColor = "#4B0082"),
                         
                          textInput("ipow_funding_depend2", "6b.Please explain your answer"),

                          tags$div(id="step8",
                          radioGroupButtons(
                            inputId = "sustainable",
                            label = labelMandatory("8a. Proin interdum quam sed urna lobortis aliquam?"),
                            choices =  c( "Choice 1", "Choice 2", "Choice 3"),
                            status = "primary",
                            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))),
                          
                          actionButton("submit_button", "Submit", class = "btn-primary")
                      )
                    ),
                    
                    shinyjs::hidden(
                      div(
                        id = "thankyou_msg",
                        h3("Thank you for your time! Your responses are very helpful for us."),
                        actionLink("submit_another", "Submit another response")
                      )
                    )
                  ),
                )
)

server = function(input, output, session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit_button", condition = mandatoryFilled)
  })    
  
  # Gather all the form inputs
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit_button, {
    saveData(formData())
  })
  
  #clicking button   
  observeEvent(input$consent, {
    
    #if they check the box that they agree and the input for the prolific id is not empty
    if(input$agree == T & input$name_id != '') {
      
      shinyjs::show("submit_info")
      shinyjs::hide("form")
      shinyjs::hide("consent")
      
      #this is storing the data from the login page
      assignment_data_start <- data.frame( name_id = input$name_id,
                                           agreement = input$agree,
                                           start_time = format(Sys.time(), "%H:%M:%S"))
      
      #this encrypts the data column with the prolific id.
      # encrypted_data_start = assignment_data_start %>% encryptr::encrypt(name_id)
      
      #location and file name to store the data
      file_start <- file.path(paste('Name','_',format(Sys.time(), "%H_%M_%S"),'.csv',sep=''))
      
      write.csv(file_start, row.names = FALSE)
      
    } else if (input$agree == F & input$name_id != ''){
      shinyjs::hide(id = 'error_msg_both_intro')
      shinyjs::show(id = 'error_msg_info')
      
    } else if (input$name_id == '' & input$agree == T){
      shinyjs::hide(id = 'error_msg_both_intro')
      shinyjs::show(id = 'error_msg_signin')
      
    } else if (input$agree == F & input$name_id == ''){
      shinyjs::show(id = 'error_msg_both_intro')
    }
  })
  
  # isolate data input
  values <- reactiveValues()
  
  create_table <- reactive({
    input$addButton
    
    Name_Consent <- input$name_id
    Consent <- input$agree
    organisation <- input$organisation
    organisation_name <- input$organisation_name
    date_from <- input$date_from
    country <- input$country
    gender <- input$gender
    age <- input$age
    activity1 <- input$activity1
    functiona <- input$functiona
    function_other <- input$function_other
    activity <- input$activity
    activity_other <- input$activity_other
    time <- input$time
    currency <- input$currency
    fin_support_time <- input$fin_support_time
    fin_support_tot <- input$fin_support_tot
    fin_support_year3 <- input$fin_support_year3
    fin_support_year2 <- input$fin_support_year2
    fin_support_year1 <- input$fin_support_year1
    fin_support_1 <- input$fin_support_1
    fin_support_2 <- input$fin_support_2
    fin_support_3 <- input$fin_support_3
    fin_support_4 <- input$fin_support_4
    fin_support_5 <- input$fin_support_5
    hours <- input$hours
    post_hours <- input$post_hours
    pers_income <- input$pers_income
    opportunity1 <- input$opportunity1
    opportunity2 <- input$opportunity2
    opportunity2_other <- input$opportunity2_other
    income_change <- input$income_change
    revenue_change <- input$revenue_change
    volunteer <- input$volunteer
    volunteer_num <- input$volunteer_num
    volunteer_hours <- input$volunteer_hours
    volunteer_resp <- input$volunteer_resp
    perc_employees <- input$perc_employees
    perc_rent <- input$perc_rent
    perc_equipment <- input$perc_equipment
    perc_donation <- input$perc_donation
    perc_events <- input$perc_events
    perc_training <- input$perc_training
    perc_other <- input$perc_other
    budget <- input$budget
    budget_upload <- input$budget_upload
    comment1 <- input$comment1
    other_funding <- input$other_funding
    other_funding1 <- input$other_funding1
    ipow_funding_imp <- input$ipow_funding_imp
    ipow_funding_imp2 <- input$ipow_funding_imp2
    ipow_funding_diff <- input$ipow_funding_diff
    ipow_funding_diff2 <- input$ipow_funding_diff2
    ipow_funding_comm <- input$ipow_funding_comm
    ipow_funding_comm2 <- input$ipow_funding_comm2
    ipow_funding_help <- input$ipow_funding_help
    ipow_funding_help2 <- input$ipow_funding_help2
    ipow_funding_depend <- input$ipow_funding_depend
    ipow_funding_depend2 <- input$ipow_funding_depend2
    ipow_funding_time <- input$ipow_funding_time
    ipow_funding_time2 <- input$ipow_funding_time2
    ipow_funding_future <- input$ipow_funding_future
    ipow_funding_future2 <- input$ipow_funding_future2
    sustainable <- input$sustainable
    sustainable_time <- input$sustainable_time
    sustainable3 <- input$sustainable3
    activities <- input$activities
    activities_other <- input$activities_other
    activities_num <- input$activities_num
    activities_reason <- input$activities_reason
    activities_fund <- input$activities_fund
    activities_new <- input$activities_new
    extra_funding <- input$extra_funding
    extra_funding2 <- input$extra_funding2
    activities_change <- input$activities_change
    activities_change2 <- input$activities_change2
    part_men <- input$part_men
    part_women <- input$part_women
    part_neutral <- input$part_neutral
    part_age <- input$part_age
    part_status <- input$part_status
    part_continue <- input$part_continue
    part_words <- input$part_words
    part_words2 <- input$part_words2
    part_change <- input$part_change
    part_change2 <- input$part_change2
    participation_benefit  <- input$participation_benefit
    participation_benefit2 <- input$participation_benefit2
    skills <- input$skills
    skills2 <- input$skills2
    skills_where <- input$skills_where
    skills_where2 <- input$skills_where2
    skills_details <- input$skills_details
    resources <- input$resources
    continue_part <- input$continue_part
    apprentice <- input$apprentice
    apprentice_yes <- input$apprentice_yes
    needs_community <- input$needs_community
    needs_find <- input$needs_find
    needs_met <- input$needs_met
    more_part <- input$more_part
    success_words <- input$success_words
    success_words2 <- input$success_words2
    participants_change <- input$participants_change
    training <- input$training
    training_type <- input$training_type
    training_type2 <- input$training_type2
    training_change <- input$training_change
    training_change2 <- input$training_change2
    training_others <- input$training_others
    training_others2 <- input$training_others2
    training_relevant <- input$training_relevant
    training_relevant2 <- input$training_relevant2
    training_long_term <- input$training_long_term
    
    df <-
      tibble(Name_Consent, Consent, organisation, date_from, country, gender, age, activity1, functiona, function_other, activity, activity_other, time, currency, fin_support_time, fin_support_tot, fin_support_year3,
             fin_support_year2, fin_support_year1, fin_support_1, fin_support_2, fin_support_3, fin_support_4, fin_support_5, hours, post_hours, pers_income,
             opportunity1, opportunity2, opportunity2_other, income_change, revenue_change, volunteer, volunteer_num, volunteer_hours, volunteer_resp, perc_employees, perc_rent, perc_equipment,
             perc_donation, perc_events, perc_training, perc_other, budget, budget_upload, comment1, other_funding, other_funding1, ipow_funding_imp, ipow_funding_imp2,
             ipow_funding_diff, ipow_funding_diff2, ipow_funding_comm, ipow_funding_comm2, ipow_funding_help, ipow_funding_help2, ipow_funding_depend, ipow_funding_depend2, ipow_funding_time,
             ipow_funding_time2, apprentice_yes, ipow_funding_future, ipow_funding_future2, sustainable, sustainable_time, sustainable3,
             activities, activities_other, activities_num, activities_reason, activities_fund, activities_new, extra_funding, extra_funding2, activities_change, activities_change2,
             part_men, part_women, part_neutral, part_age, part_status, part_continue, part_words, part_words2, part_change,
             part_change2, participation_benefit,
             participation_benefit2, skills, skills2, skills_where, skills_where2, skills_details, resources, continue_part,
             apprentice, needs_community, needs_find, needs_met, more_part, success_words, success_words2, participants_change,
             training, training_type, training_type2, training_change, training_change2, training_others, training_others2,
             training_relevant, training_relevant2, training_long_term)
    
    df
  })
  
  #************************Start Goal Adjustment ************************************************** 
  # -------------------- Set IntroJS data and event listener ---------------------#
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  observeEvent(input$consent,{
    
    # on click, send custom message to start help
    session$sendCustomMessage(type = 'consent', message = list(""))
    
  })
  
  # -------------------- END: Set IntroJS data and event listener ---------------------#
  
  #to reset the steps - start at step 1:
 # session$sendCustomMessage(type = 'consent', message = list(""))
  
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_button, {
    # User-experience stuff
    shinyjs::disable("submit_button")
    shinyjs::show("thankyou_msg")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
  })
  
}

shinyApp(ui = ui, server = server)