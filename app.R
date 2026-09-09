library(shiny)
library(bslib)
library(readxl)
library(curl)
library(magrittr)
library(dplyr)
library(purrr)


#setwd("C:/Users/hsto0009/OneDrive - Monash University/Admin/CVs/Shiny_CV")
data.experience <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Experience")
data.education <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Education")
data.employment <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Employment")
data.pubs <- read.csv(file = file.path("data", "CV_pubs.csv"))
data.conferences <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Conferences")
data.prizes <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Prizes")
data.skills <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Skills")
data.commitment <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Commitment")
data.contact <- read_xlsx(path = file.path("data", "CV_masterfile.xlsx"), sheet = "Contact")
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
                  min-height: 0px; height: auto;  }
    .card-content .card-body {color: black; font-size: 14px; text-align: left; padding: 5px;  }
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
    ),#tags$script
    tags$link(
      rel = "shortcut icon", href = "favicon.svg"
    )#tags$link
  ), #$tags$head
  
  
  # App title ----
  title = tags$a(
    href = "#",
    onclick = "Shiny.setInputValue('logo_click', Date.now()); return false;",
    tags$img(
      src = "favicon.svg",
      height = "50px"
    )
  ),
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
          div(
            class = "exptext", 
            style = "text-align: left; ",
            tags$ul(
              tags$li("Passionate postdoctoral immunologist with a proven publication record across mucosal immunology, infection, and metabolic disease"),
              tags$li("Skilled in bridging wet-lab disease models with exploratory bioinformatic pipelines (R, omics analysis, Shiny)"),
              tags$li("Actively contributing to the ECR community in committee chair and member roles"),
              tags$li("Open to postdoctoral, fellowship, and industry roles focused on exciting, hypothesis-led, and data-driven immunology projects")
              )
            )
            

          
          
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
    title = "Employment",
    div(
      uiOutput("employment")
    )
  ), #end of nav_panel
  nav_panel(
    title = "Publications", 
    div(
      style = "margin-bottom: -25px; ",
      card(
        class = "card-content", 
        style = "min-height: 0px; justify-content: left; ", 
        div(
          class = "exptext",
          style = "font-weight: 600; ", 
          "External links: ",
          icon("google-scholar"),
          tags$a(
            href = "https://scholar.google.com/citations?user=gGL4jeIAAAAJ&hl=en", 
            target = "_blank", 
            "Google Scholar"
          ),
          " · ",
          icon("orcid", class = "secondary"), 
          tags$a(
            href = "https://orcid.org/0000-0002-7830-2776", 
            target = "_blank", 
            "ORCiD"
          )
        )
          
      )
    ), 
    div(
      uiOutput("publications")
    )
  ), 
  nav_panel(
    title = "Grants, Awards and Prizes", 
    div(
      uiOutput("prizes")
    )
  ), 
  nav_panel(
    title = "Conferences", 
    div(
      uiOutput("conferences")
    )
  ), 
  nav_panel(
    title = "Skills", 
    div(
      uiOutput("skills")
    )
  ), 
  nav_panel(
    title = "Commitment", 
    div(
      uiOutput("commitment")
    )
  ),
  nav_panel(
    title = "Contact and Refs",
    div(
      uiOutput("contact")
    )
  )
) #end of page_navbar



#server ---- 
server <- function (input, output, session){
  
  #actionButton observers ---- 
  observeEvent(input$logo_click, {
    updateNavbarPage(session, "main_nav", selected = "Home")
  })
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
    updateNavbarPage(session, "main_nav", selected = "Grants, Awards and Prizes")
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
            tags$img(src = row$Logo, height = "50px"))
          
        )
        
      ),

      card_body(
        div(
          class = "edutitle", 
          row$Type), 
        div(
          class = "exptitle", 
          style = "font-weight: 600; ", 
          row$Title), 
        div(
          class = "expdate",
          row$From, 
          " – ", 
          row$To),

        div(class = "expplace", 
            style = "color: #000000; ",
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
      style = "grid-auto-rows: auto;",
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
      style = "grid-auto-rows: auto;",
      !!!educards
    )
  })
  #employment----
  make_emp_entry <- function(row) {
    
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
              row$Title), 
          div(class = "exptext",
              style = "color: #000000; font-weight: 300; ", 
              row$Employer), 
          div(
            class = "expdate",
            row$From, 
            " – ", 
            row$To)
        ), 
        div(class = "expdate", 
            "Supervised by:", 
                row$Supervisor
        
        )
      )
    )
  }
  
  output$employment <- renderUI({
    
    empcards <- lapply(seq_len(nrow(data.employment)), function(i) {
      make_emp_entry(data.employment[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!empcards
    )
  })
  
  #publications ----
  format_authors <- function(authors, my_name) {
    
    author_list <- trimws(strsplit(authors, "; ")[[1]])
    
    formatted <- sapply(author_list, function(author) {
      if (grepl(my_name, author) == TRUE) {
        paste0("<strong><u style = 'color: #000000'>", author, "</u></strong>")
      } else {
        author
      }
    })
    
    HTML(paste(formatted, collapse = "; "))
  }
  get_year <- function(x) {
    
    x <- trimws(x)
    
    if(nchar(x) == 4){
      return(x)
    }
    else if(grepl("^\\d{4}-\\d{2}-\\d{2}", x)) {
      return(substr(x, 1, 4))
    }
    else if (grepl("^\\d{4}(-\\d{2})?$(-\\d{2})?$", x)) {
      # yyyy-mm OR yyyy
      return(substr(x, 1, 4))
    }
    
    else if (grepl("^\\d{2}/\\d{2}/\\d{2}$", x)) {
      # dd/mm/yy
      return(paste0("20", substr(x, 7, 8)))
    }
    
   
    else{NA_character_}
    
  }
  
  make_pub_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        div(
          div(
            tags$img(src = row$logo, style = "max-width: 100%; max-height: 80px; "))
          
        )
        
      ),
      
      card_body(
        
        div(
          div(
            class = "exptext",
            style = "font-weight: 600;  ", 
            row$journal, " · ",
            get_year(row$publication_date)
            ),
          div(style = "min-height: 48px; margin-bottom: 0.5rem; ", 
              class = "edutitle", 
              row$title), 
          
          
          div(class = "exptext",
              style = "color: #000000; ", 
              format_authors(row$authors, "lting"))
         
        ) ,
        if(row$type != "preprint"){
          div(class = "exptext", 
              style = "font-weight: 600; ", 
             paste0("Citations: ", row$cited_by, " · FWCI: ", signif(row$FWCI, 3))
             )
        }
        ,div(class = "exptext", 
             style = "color: #006dae; font-weight: 600; ", 
                  tags$a(
                    href = paste0("https://doi.org/", row$doi),
                    paste0("DOI: ", row$doi), 
                    target = "_blank"
                  )

            
        )
      )
    )
  }
  
  output$publications <- renderUI({
    
    pubcards <- lapply(seq_len(nrow(data.pubs)), function(i) {
      make_pub_entry(data.pubs[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!pubcards
    )
  })
  
  #conferences ----
  make_conf_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        div(
          div(
            tags$img(src = row$Logo, style = "max-width: 100%; max-height: 80px; "))
          
        )
        
      ), 
      
      card_body(
        
        div(
          div(
            class = "edutitle",
            style = "margin-bottom: 0.5rem;  ", 
            row$Event
          ),
          div(
            class = "exptext",
            row$Date, " · ", row$Location
          ),
          div(style = "font-weight: 600; margin-bottom: 0.5rem;  ", 
              class = "edutext", 
              row$Role), 

          if(!is.na(row$`Additional Notes`)){
            div(class = "expdate", 
                row$`Additional Notes`
            )
          } 
      )
    )
    )
    
  }
  
  output$conferences <- renderUI({
    
    confcards <- lapply(seq_len(nrow(data.conferences)), function(i) {
      make_conf_entry(data.conferences[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!confcards
    )
  })
  
  #prizes ----
  make_prize_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        style = "min-height: 10px;",
        div(
          div(
            class = "exptext",
            row$Date, " · ", row$Type
          )
          
        )
        
      ),
      
      card_body(
        
        div(
          div(
            class = "edutitle",
            style = "margin-bottom: 0.5rem;  ", 
            row$Title
          ),
          if(!is.na(row$Description)){
            div(class = "exptext", 
                style = "margin-top: 0.5rem;  ", 
                row$Description
            )
          }, 
          div(class = "exptext", 
              style = "margin-top: 0.5rem; margin-bottom: 0.5rem;  ", 
              "Value: ", row$Value),
          
          if(!is.na(row$Image)){
            div(
              tags$img(src = row$Image, style = "max-width: 100%; max-height: 150px; "))
          }
          
          
        )
      )
    )
    
  }
  
  output$prizes <- renderUI({
    
    prizecards <- lapply(seq_len(nrow(data.prizes)), function(i) {
      make_prize_entry(data.prizes[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!prizecards
    )
  })
  
  #skills----
  output$skills <- renderUI({
    
    # 1. Group data by Category & Logo
    grouped_skills <- data.skills %>%
      mutate(Category = factor(Category, levels = .$Category %>% unique())) %>%
      group_by(Category, Logo) %>%
      summarise(Descriptions = list(Description), .groups = "drop")
    
    
    # 2. Map through each category to construct a bslib card
    cards_list <- pmap(grouped_skills, function(Category, Logo, Descriptions) {
      card(
        class = "card-custom", 
        card_header(
          div(
            div(
              tags$img(src = Logo, style = "max-width: 100%; height: 80px; "))
            
          )
          
        ),
        card_body(
          div(
            class = "edutitle", 
            Category
          ),
          tags$ul(
            lapply(Descriptions, tags$li)
          )
        )
      )
    })
    
    # 3. Render grid layout (responsive: 3 columns on wide screens, minimum width 300px)
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!cards_list
    )
  })
  
  #commitment----
  make_commitment_divs <- function(row) {
    
    div(
      if(!is.na(row$Description)){
        div(
          class = "exptitle", 
          style = "font-weight: 600; ", 
          row$Description
        )
      }, 
      if(!is.na(row$Date)){
        div(
          class = "expdate", 
          row$Date
        )
      }, 
      if(!is.na(row$Info)){
        div(class = "exptext", 
            style = "margin-top: 0.5rem;  ", 
            row$Info
        )
      },
      if(!is.na(row$Bullets)){
        string <- strsplit(row$Bullets, ";")[[1]]
        div( 
          tags$ul(
            lapply(seq_along(string), function(i) {
              tags$li(
                string[i]
              )
            })
          )
        )
      }, 
      if(!is.na(row$Link)){
        div(div(
          class = "exptext", 
          "Available at:"), 
                tags$a(
                  href = row$Link,
                  target = "_blank", 
                  row$Link
                )
              )
      }
    )
    
  }
  output$commitment <- renderUI({
    
    # 1. Group data 
    grouped_commitment <- data.commitment %>%
      mutate(Type = factor(Type, levels = .$Type %>% unique())) %>%
      split(., .$Type)
    
    
    cards <- map2(names(grouped_commitment), grouped_commitment, function(type_name, sub_df) {
      
      # Generate the list of row divs for this specific Type
      body_divs <- lapply(seq_len(nrow(sub_df)), function(i) {
        make_commitment_divs(sub_df[i, ])
      })
      
      # Construct the Card
      card(
        class = "card-custom", 
        card_header(
          style = "min-height: 0px; ",
          div(class = "edutitle", 
              type_name)
        ),
        card_body(
          body_divs
        )
      )
    })
    
    # Return the list of cards wrapped in a container
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!cards
    )
  })
  
  #contact ----
  make_cont_entry <- function(row) {
    
    card(
      class = "card-custom", 
      card_header(
        style = "min-height: 0px; ",
        div(class = "edutitle", 
            if(row$Type == "Reference"){
              paste0("Reference: ", row$Name)
            }
            else {
              row$Name 
            }
            )
      ), 
      
      card_body(
        
        div(
          div(
            class = "exptitle", 
            style = "font-weight: 600; margin-bottom: 0.5rem; ", 
            row$Title
          ), 
          div(class = "edutext", 
              lapply(seq_along(strsplit(row$Address, ";")[[1]]), function(i) {
                div(strsplit(row$Address, ";")[[1]][i])
              }) 
          
        ),
        div(
          class = "edutext", 
          style = "margin-top: 0.5rem; ", 
          tags$a(href = paste0("mailto:", row$Email), 
                 target = "_blank", 
                 row$Email)
        )
      )
    )
    )
    
  }
  
  output$contact <- renderUI({
    
    contcards <- lapply(seq_len(nrow(data.contact)), function(i) {
      make_cont_entry(data.contact[i, ])
    })
    
    layout_column_wrap(
      width = 250,
      style = "grid-auto-rows: auto;",
      !!!contcards
    )
  })
  
}

shinyApp(ui = ui, server = server)
