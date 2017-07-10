library(leaflet)
library(shiny)
library(RCurl)
library(stringr)
library(plotly)
library(DT)
library(shinyjs)

##
##  FUNCTIONS
##

autorunRchallenge <- function(n) {
  for (i in 1:n) {
    getURL(start_Rchallenge)
    Sys.sleep(2)
    getURL(play_battle)
    Sys.sleep(3)
  }
}

autorunNRchallenge <- function(n) {
  for (i in 1:n) {
    getURL(start_NRchallenge)
    Sys.sleep(2)
    getURL(play_battle)
    Sys.sleep(3)
  }
}

FarmingCampaign <- function(saved_energy, num_five_refills, num_ten_refills) {
  
  refills = num_five_refills + num_ten_refills
  
  while (saved_energy >= 8 || refills > 0) {
    
    while (saved_energy < 43 && refills > 0) {
      if (num_ten_refills > 0) {
        getURL(ten_refills)
        num_ten_refills = num_ten_refills - 1
        saved_energy = saved_energy + 10
        refills = refills - 1
        Sys.sleep(2)
      } else if (num_five_refills > 0) {
        getURL(five_refills)
        num_five_refills = num_five_refills - 1
        saved_energy = saved_energy + 5
        refills = refills - 1
        Sys.sleep(2)
      }
    }
    
    num_battles=floor(saved_energy/8)
    for (i in 1:num_battles) {
      getURL(start_battle)
      Sys.sleep(2)
      getURL(play_battle)
      Sys.sleep(3)
    }
    saved_energy = saved_energy - 8 * num_battles
  }
}

startMsg <- function() {
  message("Starting now...")
}

endMsg <- function() {
  message("It's done!")
}

##
##  DATA
##

# You need to add your ID and hashed password here!
my_id=""
my_password=""

start_Rchallenge = paste("https://cb-live.synapse-games.com/api.php?message=startChallenge&challenge_id=1101&user_id=", my_id, "&password=", my_password, sep="")
start_NRchallenge = paste("https://cb-live.synapse-games.com/api.php?message=startChallenge&challenge_id=1104user_id=", my_id, "&password=", my_password, sep="")
five_refills=paste('https://cb-live.synapse-games.com/api.php?message=useItem&item_id=1002&number=1&user_id=', my_id, '&password=', my_password, sep="")
ten_refills=paste('https://cb-live.synapse-games.com/api.php?message=useItem&item_id=1003&number=1&user_id=', my_id, '&password=', my_password, sep="")
start_battle=paste('https://cb-live.synapse-games.com/api.php?message=startMission&mission_id=175&user_id=', my_id, '&password=', my_password, sep="")
play_battle=paste('https://cb-live.synapse-games.com/api.php?message=playCard&skip=True&user_id=', my_id, '&password=', my_password, sep="")


##
##  CODE
##

ui = navbarPage("Automation Tools", id="nav",
                
                tabPanel("Auto Campaign",
                         hr(),
                         fluidRow(column(12,
                                         textInput("energy", "Energy:"),
                                         textInput("refill5", "Refills (5):"),
                                         textInput("refill10", "Refills (10):"))
                         ),
                         hr(),
                         fluidRow(column(12,
                                         shinyjs::useShinyjs(),
                                         actionButton("autocampaign", "Start"),
                                         textOutput("text1")))
                ),
                
                tabPanel("Auto Challenge",
                         hr(),
                         fluidRow(column(12,
                                         textInput("runs", "How many runs left?"))),
                         hr(),
                         fluidRow(column(12,
                                         shinyjs::useShinyjs(),
                                         actionButton("refillChallenge", "Start refill challenge"),
                                         actionButton("nonrefillChallenge", "Start non-refill challenge"),
                                         textOutput("text2")))
                )
)

server <- function(input, output, session) {
  observeEvent(input$autocampaign,
               {
                 energy = as.numeric(input$energy)
                 refill5 = as.numeric(input$refill5)
                 refill10 = as.numeric(input$refill10)
                 
                 if (is.na(energy))
                   energy = 0
                 
                 if (is.na(refill5))
                   refill5 = 0
                 
                 if (is.na(refill10))
                   refill10 = 0
                 
                 withCallingHandlers({
                   shinyjs::html("text1", "")
                   startMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text1", html = m$message, add = TRUE)
                 })
                 
                 FarmingCampaign(energy, refill5, refill10)
                 
                 withCallingHandlers({
                   shinyjs::html("text1", "")
                   endMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text1", html = m$message, add = TRUE)
                 })
               })
  
  observeEvent(input$nonrefillChallenge,
               {
                 n = input$runs
                 
                 if (is.na(n))
                   n = 0
                 
                 withCallingHandlers({
                   shinyjs::html("text2", "")
                   startMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text2", html = m$message, add = TRUE)
                 })
                 
                 autorunNRchallenge(n)
                 
                 withCallingHandlers({
                   shinyjs::html("text2", "")
                   endMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text2", html = m$message, add = TRUE)
                 })
               })
  
  observeEvent(input$refillChallenge,
               {
                 n = input$runs
                 
                 if (is.na(n))
                   n = 0
                 
                 withCallingHandlers({
                   shinyjs::html("text2", "")
                   startMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text2", html = m$message, add = TRUE)
                 })
                 
                 autorunRchallenge(n)
                 
                 withCallingHandlers({
                   shinyjs::html("text2", "")
                   endMsg()
                 },
                 message = function(m) {
                   shinyjs::html(id = "text2", html = m$message, add = TRUE)
                 })
               })
}

shinyApp(ui = ui, server = server)