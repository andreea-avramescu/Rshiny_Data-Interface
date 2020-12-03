# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Developed with R version 3.3.2 (64-bit)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(shinythemes)
library(shinyauthr)
library(digest)
library(shiny)
library(markdown)

source("carouselPanel.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = img(src="logo.png", height = "30px"), id = "navBar",
                   theme = shinytheme('cyborg'),
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "--Company-- Data Management Interface",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 75px;}"),
                   
                   tabPanel("HOME", value = "home",
                            includeCSS("www/styles.css"),
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax'><center>--COMPANY--</center></h2>
                                     <p class='parallax', align='justify', 
                                     style = 'font-family:'Lucida Console', Charcoal;font-size:12px'>This interface is used to collect
                                     and analyse the data for --Company. If you are a volunteer, please click the <q>I am a volunteer </q>
                                     button. This will open a new tab which will let you choose
                                     the preferred language to complete the questionnaire. The two options are English and Spanish.
                                     Both will require a specific username and password that you can obtain from a --Company-- 
                                     staff member. If you want to access the data and the analysis tools 
                                     please press <q>I am part of the --Company--</q>. This section is username and password protected,
                                     and restricted only to --Comapny-- staff members.</p>
                                     
                                     <p class='parallax', align='justify',
                                     style = 'font-family:'Lucida Console', Charcoal;font-size:12px'> Esta interfaz se utilizará para recopilar y analizar los datos de In Place of War.
                                     Si es un voluntario, haga clic en <q> Soy un voluntario </q>  botón. 
                                     Esto abrirá una nueva pestaña que le permitirá elegir el idioma que prefiere para completar el cuestionario. 
                                     Las dos opciones son inglés y español. Ambos requerirán un nombre de usuario y contraseña específicos que 
                                     puede obtener de algún miembro del equipo de --Empresa--.                            
                                     Si desea acceder a los datos y las herramientas de análisis por favor presione <q> Soy parte de --Espresa-- </q>. 
                                     Esta sección también está protegida por nombre de usuario y contraseña y restringido únicamente a los 
                                     miembros del personal de --Espresa--.</p>
                                     </section>
                                     ")
                                ),

                            
                            # BUTTONS TO START
                            fluidRow(
                                column(3),
                                column(6,
                                       
                                       tags$div(class = "wrap",
                                                div(class = "center", 
                                                    style="display: inline-block;vertical-align:top; width: 225px;",
                                                    tags$a("I am a volunteer",
                                                           onclick = "fakeClick('change_makers')",
                                                           class="btn btn-primary btn-lg")
                                                ),
                                                div(class = "center",
                                                    style="display: inline-block; vertical-align:top; horizontal-align:middle; width: 75px;",
                                                    tags$br(), tags$h4("OR") ),
                                                div(class = "center",
                                                    style="display: inline-block;vertical-align:top; width: 225px;",
                                                    tags$a("I am part of the --Company--", 
                                                           onclick="window.open('https://avramescu97.shinyapps.io/Algorithms/', '_blank')", 
                                                           class="btn btn-primary btn-lg")
                                                )
                                       )
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(
                                HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax'><center>Video replaced with Blank Video</center></h2>
                                     ")
                            ),
                            
                            
                            # Embedded Video from Vimeo on how to use this tool
                            fluidRow(
                                column(3),
                                column(6,
                                       HTML("<iframe width='560' height='315' src='https://www.youtube.com/embed/8tPnX7OPo0Q' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>"
                                )
                                ),
                                column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(shiny::HTML("<br><br><center> <h1>Do you want to find more about --Company--?</h1> </center>
                                                 <br>")
                            ),
                            fluidRow(
                                column(3),
                                column(6,
                                       tags$div(align = "center", 
                                                tags$a("--Company--", 
                                                       onclick="window.open('https://en.wikipedia.org/wiki/Intentionally_blank_page/', '_blank')", 
                                                       class="btn btn-primary btn-lg")
                                       )
                                ),
                                column(3)
                            ),
                            fluidRow(style = "height:25px;"
                            )
                            
                            ),
                   
                   tabPanel("Data Collection", value = "change_makers",
                            shinyjs::useShinyjs(),                            
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax'><center>Data Collection</center></h2>
                                     <p class = 'parallax_description'> Please choose your preferred language. For the anonymised version - User: <b>user</b>; Password: <b>password</b> </p>
                                     <p class = 'parallax_description'> Elija su idioma preferido. </p>
                                     </section>
                                     ")
                            ),
                            
                            # BUTTONS TO START
                            fluidRow(
                                column(3),
                                column(6,
                                       
                                       tags$div(class = "wrap",
                                                div(class = "center", 
                                                    style="display: inline-block;vertical-align:top; width: 225px;",
                                                    tags$a("English",
                                                           onclick = "window.open('https://avramescu97.shinyapps.io/Collection_English/', '_blank')",
                                                           class="btn btn-primary btn-lg")
                                                ),
                                                div(class = "center",
                                                    style="display: inline-block; vertical-align:top; horizontal-align:middle; width: 75px;",
                                                    tags$br(), tags$h4("OR") ),
                                                div(class = "center",
                                                    style="display: inline-block;vertical-align:top; width: 225px;",
                                                    tags$a("Español", 
                                                           onclick="window.open('https://avramescu97.shinyapps.io/Collection_English/', '_blank')", 
                                                           class="btn btn-primary btn-lg")
                                                )
                                       )
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            
                   ),
                   
                   tabPanel("Contact", value = "contact",
                            
                            # TEAM BIO
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h5>In case of any problems with this interface
                                                   please contact one of the persons below</h5> </center><br>"),
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            fluidRow(
                                column(3),
                                
                                # Ruth
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                   tags$img(src = "woman.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Contact person"),
                                                   tags$h6( tags$i("Add position here"))
                                               ),
                                               div(
                                                   "Add e-mail here"
                                               )
                                           )
                                       )
                                ),
                                # Teresa
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "woman.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Contact person"),
                                                   tags$h6( tags$i("Add position here"))
                                               ),
                                               div(
                                                   "Add e-mail here"
                                               )
                                           )
                                       )
                                ),
                                # David
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "man.svg", 
                                                            width = "50px", height = "50px")),
                                               div(
                                                   tags$h5("Contact person"),
                                                   tags$h6( tags$i("Add position here"))
                                               ),
                                               div(
                                                   "Add e-mail here"
                                               )
                                           )
                                       )
                                ),
                                column(3)
                                
                            ),
                            fluidRow(style = "height:150px;")
                                )  # Closes Contact tab
                   
                            )
        
                   )