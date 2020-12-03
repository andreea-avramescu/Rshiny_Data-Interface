# create a user base then hash passwords with md5 algorithm
# then save to an rds file in app directory
#user_base_algo <- data.frame(
#    user = c("ThisIsTheUser", "user2"),
#    password = c("ThisIsThePasswordForTheUser", "pass2"), 
#    permissions = c("admin", "standard"),
#    name = c("User One", "User Two"),
#    stringsAsFactors = FALSE
#)

# data.frame with credentials info
#credentials <- data.frame(
#    user = c("1", "victor", "benoit"),
#    password = c("1", "12345", "azerty"),
    # comment = c("alsace", "auvergne", "bretagne"), %>% 
#    stringsAsFactors = FALSE
#)

# Initialize the user selections and tooltip (title)
selections <- vector(mode = "character", length = 0)
edgeLabels <- vector(mode = "character", length = 0)
tips <- vector(mode = "character", length = 0)

source("./www/getLink.R")
source("./www/make_breaks.R")

shinyServer(function(input, output, session) {
    
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
 
    # DT Options --------------------------------------------------------------
    options(DT.options = list( lengthMenu = c(10, 20),
                               dom = 'tl'
    ))  # table and lengthMenu options
    
    # Intro JS ----------------------------------------------------------------
    observeEvent(input$help,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit"))
    )
   
})