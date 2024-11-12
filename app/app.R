library(plotly)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyBS)
library(bslib)
library(fresh)
require(gt)

# Define Colors
# https://www.figma.com/design/TIFEm2kx0n8nA7IZI0viUG/Salt%3A-Color-Library-(Community)?node-id=3907-1614&node-type=canvas

green400 <- "#309C5A"
green10 <- "#D1F4C9"
green700 <- "#0C5D2E"

citrine400 <- "#B29C42"
cobalt100 <- "#EDF4FF"
cobalt400 <- "#7694CF"
fur400 <- "#C2906B"
lavender <- "#BA85BA"
ocean400 <- "#50A7BA"
slate100 <- "#E1E8F7"
slate400 <- "#8691AC"
slate700 <- "#404961"

salmon400 <- "#DE7878"
teal400 <- "#3095A6"
purple400 <- "#C074CB" #"#A961B5"
cider400 <- "#DB8A48"

# Define Optimum values for reference 
grp <- c("Overall", rep("Macronutrients", times = 3), "Acidity", "Soil composition", "Soil respiration")
elmts <- c("Overall soil health", "Nitrogen", "Phosphorus", "Potassium", "pH value", "Organic matter", "Soil respiration")
elmts_unit <- c("","N, mg/kg","P, ppm","K, 10ppm","1:1, H2O", "%", "CO2-C, ppm")
dt_ref <- data.frame(id = "optimal", group = grp, element = elmts, elmts_nm = paste0(elmts,"\n(",elmts_unit,")"), 
  value=c(100, 20, 8, 13, 7, 4, 150), low=c(NA, 10, 4, 10, 5.5, 3, 100), high=c(NA, 50,14,16, 8, 6, 200))

# Define UI ----
ui <- navbarPage(
  title = div(img(src = "Icon.png", height = "57.5px", width = "auto"), 'Soil Regeneration Hub'),
    theme = bs_theme(
      bootswatch = "sandstone", 
      bg = "#F2F5FA", #"#E1E8F7",
      fg = green700, ##D1F4C9",
      primary = slate700, 
      secondary = green400,#"#309C5A", 
      info = green400,
      base_font = font_google("Karla", local = TRUE)
    ), 
  windowTitle = 'Soil Regeneration Hub',
  # icon = "Icon.png",
  # icon = shiny::icon("Icon"),

  tabPanel("Your Soil Test Results", 
    sidebarLayout(
      
    sidebarPanel(
      
      selectInput("sel", "Select Data input", choices = c("manual", "csv file", "example data")),
      
      conditionalPanel("input.sel == 'csv file'",
        fileInput('data', 'Choose csv file:', accept='.csv'), 
        "Data must be provided as a comma-separated csv file.",
        tags$a(href="https://github.com/jmanitz/soil_testing_app/blob/main/example_data.csv", "See example data.")
      ), 
      conditionalPanel("input.sel == 'manual'",
        sliderInput("hval", "Overall Soil Value", 0, 100, 72),
        sliderInput("ph", "pH value", 1, 14, 6.7, step=0.1),
        numericInput('nitr', 'Nitrogen (N, ppm)', 8.5, min = 1, max = 100),
        numericInput('phos', 'Phosphorus (P, ppm)', 7.6, min = 1, max = 100),
        numericInput('pota', 'Potassium (K, ppm)', 23, min = 1, max = 100),
        sliderInput("orgm", "Organic Matter (%)", 0, 20, 5),
        numericInput('resp', 'Soil Respiration (CO2-C, ppm)', 123, min = 0, max = 500)
      )
      # textInput("txt", "Text input:", "text here"),
      # actionButton("action2", "Button2", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Soil Test Results"),
      layout_column_wrap(width = 1/2, height = 270,
        card(full_screen = TRUE, 
             card_header(class = "d-flex justify-content-between", "Overall Soil Health", icon("circle-info") |>
                         popover("Overall soil health is a composite measure of ... . Details to it calculation can be found <here>")), 
             plotOutput('HealthPlot')),
        card(full_screen = TRUE, 
             card_header(class = "d-flex justify-content-between", "pH Value", icon("circle-info") |>
                           popover("The acidity or alkalinity of soil is indicated by its pH measurement. The pH range is 0 (extremely acid) to 14 (extremely alkaline). Most plants doing well when the soil pH is between 6.2 and 6.8.")), 
             plotlyOutput('pHplot'))
      ), 
      
      
      card(full_screen = TRUE, height = 300, 
           card_header(class = "d-flex justify-content-between", "Macronutrients", icon("circle-info") |>
                         popover("The three primary macronutrients are nitrogen (N), phosphorus (P), and potassium (K), each playing a critical role: nitrogen supports leafy growth, phosphorus aids root development and energy transfer, and potassium enhances overall plant health and resilience.")), 
           
           plotlyOutput('NPKplot')), 
      
      layout_column_wrap(width = 1/2, height = 270,
                         
        card(full_screen = TRUE, 
             card_header(class = "d-flex justify-content-between", "Soil Composition", icon("circle-info") |>
                           popover("Soil organic matter is made up of decomposing plant and animal material, microbial cells, and compounds synthesized by microbes; higher levels are strongly linked to improved soil fertility.")), 
             plotOutput('OrgMplot')),
        
        card(full_screen = TRUE, 
             card_header(class = "d-flex justify-content-between", "Soil Respiration", 
                         popover(icon("circle-info"), "Soil respiration describes the process in which plant roots and microbes release carbon dioxide (CO2) into the atmosphere when soil organisms respire.")), 
             plotlyOutput('RespPlot'))
      ),
      
      card(full_screen = TRUE, height = 300, 
           card_header("Table View"), tableOutput('tab')), 
      
    )  
  )),
  
  tabPanel("Information About Your Location", 
    mainPanel(
      card(full_screen = TRUE, height = 600, #card_header("Information About Your Location"),
           imageOutput('locData'))
  )),
  
  tabPanel("Recommendations", 
           mainPanel(
             
      card(full_screen = TRUE, height = 600, width=600, card_header("Recommendations to Improve Your Soil Health"),
           tableOutput('recommendations'))           
  ))
  
) 

# Define server logic ----
  
server <- function(input, output) { #  function(input, output, session) {

  # <TODO> Mock-up for location data
  output$locData <- renderImage({
    list(src = "loc_data_mock.png", width = "100%", height = "150%")}, deleteFile = FALSE)

  # Combine the selected variables into a new data frame
  dt <- reactive({
    if(input$sel == 'manual'){
      dt <- data.frame(
        id = rep("sample", times=7), 
        group=c("Overall", rep("Macronutrients", times = 3), "Acidity", "Soil composition", "Soil respiration"), 
        element = elmts, elmts_nm = paste0(elmts,"\n(",elmts_unit,")"), 
        # value = c(72, 8.5, 7.6, 23, 5, 6.7, 123))
        value = c(input$hval, input$nitr, input$phos, input$pota, input$ph, input$orgm, input$resp)
      )
    }else{
      if(input$sel == "example data"){
        dt <- read.csv("../example_data.csv", header=TRUE)
      }else{
      validate(
        # need(ext == "csv", "Please upload a csv file"))
        need(input$data != "", "Please select a data set")
      )
      dt <- read.csv(input$data$name, header = TRUE)
      }
      dt[,c("group","elmts_nm")] <- dt_ref[match(dt$element, dt_ref$element), c("group","elmts_nm")]
    }
    return(dt)
  })
  
  # Overall Soil Health
  output$HealthPlot <- renderPlot({
    
    #hval <- input$hval
    hval <- dt() %>% filter(group == "Overall") %>% pull(value)

    if(length(hval)>1){
      
      dt() %>% filter(group == "Overall") %>% mutate(index=1:n()) %>% 
        ggplot(aes(x=index, y=value)) + 
        geom_line(col=green400, linetype="dotdash", size=1.2) + geom_point(col=green700)+
        labs(x = "Sample", y = "Soil Health Score") + ylim(0,100) + 
        theme_minimal()
      
    }else{
      df <- data.frame(ymin = c(0,hval), ymax = c(hval, 100), group=c("health", "other"))
      df %>% ggplot(aes(xmax = 4, xmin=2.2, ymin=ymin, ymax=ymax, fill=group)) +
        geom_rect() + coord_polar(theta="y") + xlim(c(-1, 4)) +
        geom_text(x=-1, y=0, label=hval, size=20, col=slate700) +
        scale_fill_manual(values=c(green400,green10)) +
        theme_void() + theme(legend.position = "none")
    }
  })
    
  # Macronutrients
  output$NPKplot <- renderPlotly({
    
    pp <- bind_rows(dt(), dt_ref) %>% filter(group == "Macronutrients") %>% 
      ggplot(aes(x=elmts_nm, y=value, fill=id, text=paste(element,"=",value))) +
      geom_bar(stat="identity", position=position_dodge(0.9), color="lightgray") +
      #geom_text(position=position_dodge()) + # add numbers to top of the bar
      geom_errorbar(aes(ymin=low, ymax=high), width = 0.3, position=position_dodge(.9), color=slate700) + 
      scale_fill_manual(values=c(slate400, green400, citrine400, fur400, ocean400))+ 
      labs(x="Macronutirent", y="Test Value", fill="" ) + 
      theme_minimal() 
    
    ggplotly(pp, tooltip = "text")
  })
  
  output$pHplot <- renderPlotly({
    
    #ph_val <- input$ph;     ph_ref <- c(5.5, 7.0)
    ph_val <- dt() %>% filter(group == "Acidity") %>% pull(value)
    
    if(length(ph_val)>1){
      
      pp <- dt() %>% filter(group == "Acidity") %>% mutate(index=1:n()) %>% 
        ggplot(aes(x=index, y=value)) + geom_point(size = 0.5, col=green700)+
        geom_line(col=green400, linetype="dotdash", size=0.5) + 
        geom_rect(xmin = -Inf, xmax = Inf, ymin = 5.5, ymax = 7, fill = green10, alpha = 0.1) +
        labs(x = "Sample", y = "pH Value") + ylim(0,15) + theme_minimal()
      
      ggplotly(pp)
        
    }else{
    
    plot_ly(
      type = "indicator", mode = "gauge+number+delta", 
      value = ph_val,
      delta = list(reference = 7, increasing = list(color = cobalt400), decreasing = list(color = salmon400)),
      gauge = list(
        axis = list(range = list(0, 14), tickwidth = 0.2, tickcolor = "lightgray"),
        bar = list(color = "lightgray", thickness = 0),bgcolor = "white", borderwidth = 1.2, bordercolor = "lightgray",
        threshold = list(line = list(color = slate700, width = 7), thickness = 1, value = ph_val),
        steps = list(
          list(range = c(0,1), color="tomato"),
          list(range = c(1,2), color="coral"),
          list(range = c(2,3), color="orange"),
          list(range = c(3,4), color="gold"),
          list(range = c(4,5), color="yellow"),
          list(range = c(5,6), color="greenyellow"),
          list(range = c(6,7), color="limegreen"),
          list(range = c(7,8), color=green400), #"forestgreen", "seagreen3"),
          list(range = c(8,9), color="darkcyan"), 
          list(range = c(9,10),color="steelblue"), #       
          list(range = c(10,11),color="royalblue"),
          list(range = c(11,12),color="slateblue"),
          list(range = c(12,13),color="RebeccaPurple"),
          list(range = c(13,14),color="indigo")))) %>%
      layout(margin = list(l=20,r=30), font = list(color = slate700, family = "Arial"))
    }
  })
  
  output$OrgMplot <- renderPlot({
    
    # org_m <- input$orgm; org_ref <- c(3,6)
    org_m <- dt() %>% filter(group == "Soil composition") %>% pull(value)
    
    if(length(org_m)>1){
      
      dt() %>% filter(group == "Soil composition") %>% mutate(index=1:n()) %>% 
        ggplot(aes(x=index, y=value)) + 
        geom_line(col=green400, linetype="dotdash", size=1.1) + geom_point(col=green700)+
        geom_rect(xmin = 0, xmax = Inf, ymin = 3, ymax = 6, fill = green10, alpha = 0.1) +
        labs(x = "Sample", y = "Organic Matter (%)") + ylim(0,10) + theme_minimal()
      
    }else{
      #create data frame
      dtm <- data.frame("category" = c('minerals', 'water', 'air', 'organic matter'),
                       "amount" = c(50-org_m, 25, 25, org_m), 
                       "color"=c(fur400, cobalt400, slate400, green400))
      
      dtm %>% ggplot(aes(x="", y=amount, fill=category)) +
        geom_bar(stat="identity", width=1, color="lightgray", size=1) +
        coord_polar("y", start=90) +
        scale_fill_manual(values=c(slate100, fur400, green400, cobalt400)) + 
        geom_text(aes(label = paste0(amount, "%")), position = position_stack(vjust=0.5), color=slate700, size=5) +
        theme_void() + labs(fill="" ) + theme(legend.position = "bottom")
      
    }
  })
  
  output$RespPlot <- renderPlotly({
    
    pp <- bind_rows(dt(), dt_ref) %>% filter(group == "Soil respiration") %>% 
      ggplot(aes(x=elmts_nm, y=value, fill=id, text=paste(element,"=",value))) +  
      geom_bar(stat="identity", position=position_dodge(0.9), color="lightgray") +
      geom_errorbar(aes(ymin=low, ymax=high), width = 0.1, position=position_dodge(.9), color=slate700) + 
      scale_fill_manual(values=c(slate400, green400, citrine400, fur400, ocean400))+ # scale_fill_paletteer_d("ggthemes::excel_Crop") + 
      labs(x="", y="Test Value", fill="" ) + 
      theme_minimal() 
    
    ggplotly(pp, tooltip = "text")
    
  })
  
  output$tab <- render_gt({
    
    tab_dt <- dt() %>% pivot_wider(names_from = id) %>% 
      left_join(dt_ref %>% select(-c(id, value, group, elmts_nm)),by="element") %>% 
      select(-c(element))
      
    tab_dt %>% 
      gt(groupname_col = "group") %>% fmt_number(decimals = 2, drop_trailing_zeros=TRUE) %>% 
      tab_spanner(label = "Optimal range", columns = c(low, high)) %>% 
      tab_style(style=cell_borders(sides="left"), locations = cells_body(columns=c(low)))
  })
  
  output$recommendations <- render_gt({
    
    rdt <- read.csv('../recommendations.csv', header=TRUE)
    rdt %>% 
      gt(groupname_col = "Problem") 

  })
}

# Run the app ----
# shinyAppDir(".")
shinyApp(ui = ui, server = server)
