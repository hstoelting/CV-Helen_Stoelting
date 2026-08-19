library(shiny)
library(bslib)
library(readxl)
library(curl)

#setwd("C:/Users/hsto0009/OneDrive - Monash University/Admin/CVs/Shiny_CV")
data.experience <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Experience")
data.education <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Education")

#UI ---- 
ui <- page_navbar(
  theme = bs_theme(
    version = 5, 
    primary = "#006DAE",
    secondary = "#862164", 
    base_font = "Arial Narrow",
    heading_font = "Arial Narrow"
    # base_font = font_google("Roboto Condensed", local = TRUE),
    # heading_font = font_google("Roboto", local = TRUE)
  ), 
  #CSS styles ----
  tags$head(
    tags$style(
      HTML("body {font-size: 12px; }
    h5 { font-size: 12px; font-weight: bold; }
    h4 { font-size: 14px; font-weight: bold; margin-top: 0px; margin-bottom: 0px; }
    h3 { font-size: 16px; font-weight: bold; }
    h2 { font-size: 18px;  font-weight: bold; }
    h1 { font-size: 20px;  font-weight: bold; }
    t14 { font-size: 14px; }
    .exptype {font-size: 16px;  text-transform: uppercase; color: #006dae; margin-bottom: 0.5rem; }
    .exptitle {font-size: 14px; font-weight: 400;  }
    .expdate {color: #7F7F7F; font-size: 14px; font-weight: 100; }
    .expplace {color: #7F7F7F; font-size: 14px; font-weight: 200; }
    .exptext {font-size: 14px; }
    .edutitle {font-size: 16px;  font-weight: 600; color: #006dae;  }
    p.msg {font-size: 14px; line-height: 2; margin-bottom: 14px; font-weight: 500;   }
    .shiny-input-container input {font-size: 12px; }
    .shiny-text-output {font-size: 12px; text-align: center; font-weight: bold; }
    .selectize-input {font-size: 12px; }
    .selectize-dropdown {font-size: 12px; }
    .control-label {font-size: 13px; font-weight: 600; }
    .form-check-label{font-size: 13px; font-weight: 600; }
    .card-header{text-align: center; display: flex; justify-content: center; align-items: center; font-weight: bold; font-size: 14px; }
    .card-body{text-align: center; display: flex; flex-direction: column; justify-content: center}
    .navbar .nav-link{color: black; font-weight: 400; }
    .navbar .nav-link:hover {color: #862164; }
    .navbar .nav-link.active {color: #862164; font-weight: 700 !important; }
    .navcard-custom .card {background-color: white ; border-radius: 0px; padding: 0px ; border: 1px solid #F6F6F6; box-shadow: 2px 5px 2px #F6F6F6;  }
    .navcard-custom .nav-link{color: black; font-weight: 400; }
    .navcard-custom .nav-link:hover {color: #006dae; }
    .navcard-custom .nav-link.active {color: #006dae; font-weight: 700 !important; }
     .card-custom-home {background-color: #F6F6F6; border-radius: 0px; padding: 5px ; border: 1px solid #bebebe; box-shadow: 2px 5px 2px #bebebe;  }
    .card-custom-home .card-body {color: black; font-size: 14px; text-align: center; display: flex; flex-direction: column; justify-content: center}
    .card-custom-home .card-header {text-align: center; display: flex; justify-content: center; align-items: center; }
    .card-custom {background-color: #F6F6F6; border-radius: 0px; padding: 5px ; border: 1px solid #bebebe; box-shadow: 2px 5px 2px #bebebe; align-self: stretch; }
    .card-custom .card-body {color: black; font-size: 14px; text-align: left; display: flex; flex-direction: column; justify-content: flex-start; }
    .card-custom .card-header {text-align: center; display: flex; justify-content: center; align-items: center; min-height: 120px}
    .card-content {background-color: white ; border-radius: 0px; padding: 0px ; border: 1px solid #F6F6F6; box-shadow: 2px 5px 2px #F6F6F6; 
                  min-height: 400px; height: auto;  }
    .card-content .card-body {color: black; font-size: 14px; text-align: center; padding: 5px;  }
    .card-content .card-header {text-align: center; }
    .card-content-sm {background-color: white ; border-radius: 0px; padding: 0px ; border: 1px solid #F6F6F6; box-shadow: 2px 5px 2px #F6F6F6; 
                  min-height: 200px; height: auto;  }
    .card-content-sm .card-body {color: black; font-size: 14px; text-align: center; padding: 5px;  }
    .card-content-sm .card-header {text-align: center; }
    .button-grey {background-color: #006dae; font-size: 14px; text-align: center; color: white; border: none; 
                  border-radius: 1px; font-weight: 600; height: 60px; }
    .button-grey-sm {background-color: #006dae; font-size: 12px; text-align: center; color: white; border: none; 
                  border-radius: 1px; font-weight: 600; height: 30px; margin-bottom: 16px;  }
    .dt-button {background-color: #006dae; font-size: 12px; text-align: center; color: white; border: none; 
                  border-radius: 1px; font-weight: 600; height: 30px; margin-bottom: 16px;  }
    .dataTables_wrapper .dt-buttons {display: inline-block; margin-right: 15px; }
    .dataTables_filter {display: inline-block; float: right; }
    .flip-card {background-color: #F6F6F6; perspective: 1000px; cursor: pointer; width: 100%; max-width: 700px; min-height: 350px; 
                border-radius: 0px; padding: 5px ; border: 1px solid #bebebe; box-shadow: 2px 5px 2px #bebebe;  }
    .flip-card-inner {position: relative; width: 100%; height: 100%; transition: transform 0.6s; transform-style: preserve-3d; }
    .flip-card.flipped .flip-card-inner {transform: rotateY(180deg); }
    .flip-card-front, .flip-card-back {position: absolute; width:  100%; height: 100%; backface-visibility: hidden; 
                  background-color: white ; border-radius: 0px; padding: 15px ; border: 1px solid #F6F6F6; box-shadow: 2px 5px 2px #F6F6F6; 
                  box-sizing: border-box; 
                  display: flex; align-items: center; justify-contents: center; }
    .flip-card-back {transform: rotateY(180deg); flex-direction: column; }
    .flip-img {width: 100%; height: auto; max-height: 100%; object-fit: contain; display: block; }

         ")
    ),#tags$style
    tags$script(
      HTML("
     $(document).on('click', '.flip-card', function() {
      $(this).toggleClass('flipped');
    });
         ")#HTML
    )#tags$script
  ), #$tags$head
  
  
  # App title ----
  title = "CV of Dr Helen Stölting",
  id = "main_nav",
  #tab 0: landing page ----
  nav_panel(
    title = "Home",
    fluidRow(
      layout_column_wrap(
        width = 1, 
        card(
          style = "padding-left: 20px; padding-right: 20px; padding-top: 10px; padding-bottom: 10px; ", 
          class = "card-custom-home", 
          card_header(
            style = "background-color: #006dae; color: white; font-size: 14px; font-weight: bold; border-radius: 0px;",
            "Curriculum Vitae of Dr Helen Stölting"),
          #height = 120,
          p("My CV is still under construction - please check back later! :) ")
        )#card
      ), #column 
      layout_column_wrap(
        width = "250px",
        gap = "15px", 
        card(
          class = "card-custom-home",
          height = 110,
          card_body(
            actionButton(
              "goexperience",
              "Research Experience",
              class = "button-grey",
              width = "100%", 
              icon = icon("vial-circle-check")
            )
            
          )#end of card_body
        ), #end of card
        card(
          class = "card-custom-home",
          height = 110,
          card_body(
            actionButton(
              "goeducation",
              "Education",
              class = "button-grey",
              width = "100%", 
              icon = icon("graduation-cap")
            )#end of actionButton
          )#end of card_body
        ), #end of card
        card(
          class = "card-custom-home", 
          card_body(
            height = 110,
            actionButton(
              "goemployment",
              "Employment History",
              class = "button-grey",
              width = "100%",
              icon = icon("briefcase")
            )#end of actionButton),
            
          )#card_body
        ),
        card(
          height = 110, 
          class = "card-custom-home", 
          card_body(
            actionButton(
              "gopublications", 
              "Publications", 
              class = "button-grey",
              width = "100%", 
              icon = icon("book")
            )
          )
        ), #card
        card(
          height = 50, 
          class = "card-custom-home", 
          card_body(
            actionButton(
              "gogrants", 
              "Grants, Awards and Prizes", 
              class = "button-grey",
              width = "100%", 
              icon = icon("trophy")
            )
          )
        ), #card
        card(
          height = 110, 
          class = "card-custom-home", 
          card_body(
            actionButton(
              "goconferences", 
              "Conferences and Meetings", 
              class = "button-grey",
              width = "100%", 
              icon = icon("person-chalkboard")
            )
          )
        ), #card
        
        card(
          height = 110,
          class = "card-custom-home", 
          card_body(
            actionButton(
              "goskills",
              "Skills",
              class = "button-grey",
              width = "100%", 
              icon = icon("wrench")
            )#end of actionButton
          )#card_body
          
        ), #card
        card(
          height = 110,
          class = "card-custom-home", 
          card_body(
            actionButton(
              "gocommitment",
              "Commitment",
              class = "button-grey",
              width = "100%", 
              icon = icon("hand-holding-heart")
            )#end of actionButton
          )#card_body
          
        ), #end of card#end of card
        card(
          height = 110,
          class = "card-custom-home", 
          card_body(
            actionButton(
              "gocontact",
              "Contact Info and References",
              class = "button-grey",
              width = "100%", 
              icon = icon("address-card")
            )#end of actionButton
          )#card_body
          
        )#end of card#end of card
      ) #end of column
      
    )#end of fluidRow
  ), #end of nav_panel 
  #tab 1: Research Experience ----
  nav_panel(
    title = "Research Experience", 
    div(
      uiOutput("experience")
    )
  ), #end of nav_panel
  nav_panel(
    title = "Education", 
    div(
      uiOutput("education")
    )
  ), #end of nav_panel
  nav_panel(
    title = "Employment"
  ), #end of nav_panel
  nav_panel(
    title = "Publications"
  ), 
  nav_panel(
    title = "Grants"
  ), 
  nav_panel(
    title = "Conferences"
  ), 
  nav_panel(
    title = "Skills"
  ), 
  nav_panel(
    title = "Commitment"
  ),
  nav_panel(
    title = "Contact and Refs"
  )
) #end of page_navbar



#server ---- 
server <- function (input, output, session){
  
  #actionButton observers ---- 
  observeEvent(input$goexperience, {
    updateNavbarPage(session, "main_nav", selected = "Research Experience")
  })
  observeEvent(input$goeducation, {
    updateNavbarPage(session, "main_nav", selected = "Education")
  })
  observeEvent(input$goemployment, {
    updateNavbarPage(session, "main_nav", selected = "Employment")
  })
  observeEvent(input$gopublications, {
    updateNavbarPage(session, "main_nav", selected = "Publications")
  })
  observeEvent(input$gogrants, {
    updateNavbarPage(session, "main_nav", selected = "Grants")
  })
  observeEvent(input$goconferences, {
    updateNavbarPage(session, "main_nav", selected = "Conferences")
  })
  observeEvent(input$goskills, {
    updateNavbarPage(session, "main_nav", selected = "Skills")
  })
  observeEvent(input$gocommitment, {
    updateNavbarPage(session, "main_nav", selected = "Commitment")
  })
  observeEvent(input$gocontact, {
    updateNavbarPage(session, "main_nav", selected = "Contact and Refs")
  })
  
  
  #research experience renderUI----
  make_exp_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        div(
          div(
            class = "exptype", 
            row$Type), 
          div(
            class = "exptitle", 
            row$Title), 
          div(
            class = "expdate",
            row$From, 
            " – ", 
            row$To)
          )
        ),
      
      card_body(

        div(class = "expplace", 
              HTML(gsub("\r?\n", "<br>", row$Place))
        ),
        p(
          row$Summary
        ),
        if(!is.na(row$Publications)){
          string <- strsplit(row$Publications, ";")[[1]]
          div(div(
            class = "exptext", 
            "Publications from this experience"), 
              tags$ul(
                lapply(seq_along(string), function(i) {
                  tags$li(
                    tags$a(
                      href = paste0("https://", string[i]),
                      string[i],
                      target = "_blank"
                    )
                  )
                })
              )
          )
        }
        
      )
    )
  }
  output$experience <- renderUI({
    
    expcards <- lapply(seq_len(nrow(data.experience)), function(i) {
      make_exp_entry(data.experience[i, ])
    })
    
    layout_column_wrap(
      width = 300,
      !!!expcards
    )
  })
  
  
  #education----
  make_edu_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        div(
          div(
              tags$img(src = row$Logo, height = "50px"))
       
        )
        
      ),
      
      card_body(
        
        div(
          div(style = "min-height: 48px; ", 
            class = "edutitle", 
            row$Type, 
            row$Title), 
            div(class = "exptext",
                style = "color: #000000; font-weight: 300; ", 
                row$Uni), 
            div(
              class = "expdate",
              row$From, 
              " – ", 
              row$To)
            ), 
        div(class = "exptext", 
            "Thesis title:", 
            row$Thesis, 
            div(class = "expdate", "Supervised by:", 
            row$Supervisor)
            ),

        if(!is.na(row$Grade)){
          div(class = "exptext", 
              "Grade:", 
              row$Grade
          )
        } 
        ,div(class = "exptext", 
            HTML(gsub("\r?\n", "<br>", row$Info))
        )
    )
    )
  }
  
  output$education <- renderUI({
    
    educards <- lapply(seq_len(nrow(data.education)), function(i) {
      make_edu_entry(data.education[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      !!!educards
    )
  })
}

shinyApp(ui = ui, server = server)