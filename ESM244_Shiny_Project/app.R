library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(maps)
library(ggplot2)
library(dplyr)
library(maps)
library(reshape2)
library(RColorBrewer)
library(leaflet)
library(sf)
library(tidyr)
library(maptools)
library(sp)
library(plotly)
library(shinydashboard)
library(DT)
library(rnaturalearth)
library(sp)
library(GGally)
library(broom)
library(kableExtra)



## cleaning world lat and long data
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

world_data_clean <- world_data %>% select(region,lat,long) %>% group_by(region) %>%
  summarise(latm = max(lat, na.rm=TRUE),longm = max(long, na.rm=TRUE)) %>% rename("Country" = region)

# Read in the data
gender_data <- read_xlsx(here("data", "Gender.xlsx"))


# Tidying the data 
gender_mod <- gender_data %>% 
  rename("HDI Rank" = ...1,
         "Country" = ...2,
         "Gender Equality Index '18" = ...3,
         "Rank '18" = ...5,
         "Maternal Mortality Ratio '15" = SDG3.1,
         "Adolescent Birth Rate '15-'20" = SDG3.7,
         "Seats in Parliment '18" = SDG5.5,
         "Secondary Education (F)'10-'18" = SDG4.6,
         "Secondary Education (M)'10-'18" = ...15,
         "Labour Force Participation (F)'18" = ...17,
         "Labour Force Participation (M)'18" = ...19) %>% 
  select(-...4,-...6,-...8,-...10,-...12,-...14,-...16,-...18,-...20) %>% 
  filter(!row_number() %in% c(1, 2, 3, 4, 5, 228:261)) 


gender_mod <- merge(x = gender_mod, y = world_data_clean, by = "Country", all.x = TRUE)



## Creating a subset of the data to use to make the table
tab_data <- gender_mod %>% 
  janitor::clean_names()
## dropping rows that do not contain countries
tab_data <- tab_data %>% slice(-c(193, 200:222))
## need to drop some more--- 
tab_data <- tab_data %>% slice(-c(63, 118, 156))
## now putting in alphabetical order
tab_data <- tab_data %>% arrange(country)

# Interactive Data Table
### Need to clean up table
tab_data_table <- tab_data %>%
  group_by(country, gender_equality_index_18) 

tab_data_table <- tab_data_table %>% 
  mutate_at(c(3:11), as.numeric) %>% 
  drop_na()

tab_data_table %>% 
  mutate(across(is.numeric, round, digits = 2)) 

## changing names to make table look better
names(tab_data_table)[1] <- "Country"
names(tab_data_table)[2] <- "Human Development Index Rank"
names(tab_data_table)[3] <- "Gender Equality Index"
names(tab_data_table)[3] <- "Gender Equality Index"
names(tab_data_table)[4] <- "Country Rank"
names(tab_data_table)[5] <- "Maternal Mortality Ratio"
names(tab_data_table)[6] <- "Adolescent Birth Rate"
names(tab_data_table)[7] <- "Seats in Parliament"
names(tab_data_table)[8] <- "% of Women with Secondary Education"
names(tab_data_table)[9] <- "% of Men with Secondary Education"
names(tab_data_table)[10] <- "% of Female Labour Force Participation"
names(tab_data_table)[11] <- "% of Male Labour Force Participation"

tab_data_table <- tab_data_table %>% 
  select(c("Gender Equality Index", "Seats in Parliament", "% of Women with Secondary Education","% of Female Labour Force Participation"))


tab_data_table$Status <- NULL

## creating a subset of the data to use to make a map
## MAKING INTERACTIVE MAP
## creating a subset of the data to use to make a map
map_data <- gender_mod %>% 
  janitor::clean_names()
## dropping rows that do not contain countries
map_data <- map_data %>% slice(-c(193, 200:222))
## need to drop some more--- 
map_data <- map_data %>% slice(-c(63, 118, 156))
## now putting in alphabetical order
map_data <- map_data %>% arrange(country) 

## using rnatural earth to make an interactive map, hopefully
map <- ne_countries()
names(map)[names(map) == "iso_a3"] <- "ISO3"
names(map)[names(map) == "name"] <- "country"

head(map)
## cleaning the names so they match with map names
map_data[map_data == "Bolivia (Plurinational State of)"] <- "Bolivia"
map_data[map_data == "Russian Federation"] <- "Russia"
map_data[map_data == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
map_data[map_data == "Tanzania (United Republic of)"] <- "Tanzania"
map_data[map_data == "Iran (Islamic Republic of)"] <- "Iran"
map_data[map_data == "Congo (Democratic Republic of the)"] <- "Dem. Rep. Congo"
map_data[map_data == "Central African Republic"] <- "Central African Rep."
map_data[map_data == "South Sudan"] <- "S. Sudan"
map_data[map_data == "Viet Nam"] <- "Vietnam"
map_data[map_data == "Lao People's Democratic Republic"] <- "Lao PDR"
map_data[map_data == "United States"] <- "United States"
map_data[map_data == "Syrian Arab Republic"] <- "Syria"


# merging the data with our gender data
map_merged <- merge(x = map, y = map_data, by = "country", all.x = TRUE)
## changing indicators to class numeric so can add them to the polygons
map_merged$gender_equality_index_18 <- as.numeric(map_merged$gender_equality_index_18)


# slider map
map_data$gender_equality_index_18 <- as.numeric(map_data$gender_equality_index_18)




## Dashboard
header <- dashboardHeader(title = "Understanding the State of Gender Equality Globally",
                          titleWidth = 450)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Slider of GE Index", tabName = "slider"),
    menuItem("Scatter Plot", tabName = "scatterplot"),
    menuItem("Model Your Own Data", tabName = "self_model"),
    menuItem("Get Informed & Involved", tabName = "involved")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            fluidPage(
              box(title = "Status of Gender Equality Globally",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 15,
                  h1('Overview:'),
                  p("While Gender Equality has been identified as a critical global goal by the UN in its Millenium Development Goals, gained increased global attention with movements such as #MeToo and 
                  and recent large scale women's marches, research demonstrates that the general public is quite confused as to the current state of gender equality globally. Due to the ever-increasing visibility of this issue
                  some researchers have found a trend in which many individuals (but men in particular) believe gender equality to already be realized."),
                  p("This misconception is dangerous as it has been linked to increased resistance to 
                    ongoing efforts to promote gender equality globally, a phenomena feminist theorists have termed 'backlash'. With the plethora of (often conflicting) information on gender equality and women's empowerment
                    it is truly difficult to determine where are we at as a global society in achieving these goals?"),
                  p("The purpose of this app is to create a hub for all things gender equality so that both experts and lay people can engage with this topic and learn about ongoing events without having to search through academic articles
                    or evaluate difficult statistical models. Here we provide several simple and interactive widgers in which: users can explore major indicators of gender equality to better determine what this loaded term means,
                    world maps so users can examine how these indicators vary across the globe, a function to load in external data and create a model oneself, and information on current events concerning gender equality
                    in the news as well as ways to get involved!")), # end box 1
              box(title = "World Map of Gender Equality Indicators",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 20,
                  p("Explore the various gender equality indicators by country! Hover over the various countries on the map to get statistics on globally recognized gender equality indicators such as
                    the gender equality index, female education, maternal mortality, female seats in parliament, and more!"),
                  leafletOutput(outputId = "joemap", height = 400, width = 700)),
              box(title = "Statistics by World Region", 
                  status = "primary",
                  solidHeader = TRUE,
                  width = 20,
                  p("If you want to search one country in particular use the search bar below!"),
                  # shinyWidgets::pickerInput("incountry", "Select a Country", choices = tab_data_table$Country, options = list(`action-box` = TRUE), multiple = TRUE)
                  selectInput("incountry", "Select a Country", choices = tab_data_table$Country, multiple = TRUE)
              ), # end box1 stats
              box(dataTableOutput('table'),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 20),
              box(p("This is what those indicators mean"))
            ), # end fluid row "home"
            
              ), # end Tabitem home
    tabItem(tabName = "slider",
              column(
                sliderInput("mapsel", label = h3("Slider Range"), min = round(min(map_data$gender_equality_index_18,na.rm = T),2), 
                            max = round(max(map_data$gender_equality_index_18,na.rm = T),2), value = c(0.1, 0.6)), width = 1.5),
            # textOutput("selected_var"),
            # tableOutput("df")
            leafletOutput(outputId = "mymap", height = 500)
              # end mainpanel
             # end fluidrow
    ), # end tab item 
    tabItem(tabName = "scatterplot",
              box(title = "Scatterplot on Gender Equality Outcomes and Predictors",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 20,
                  p("alter this interactive plot to see how it affects outputs"),
                  selectInput(inputId = "plotcountry", label = "Country", choices = tab_data_table$Country, multiple = TRUE,
                              selected = "Tanzania"),
                  selectInput(inputId = 'xcol', label = "X", choices = c("gender_equality_index_18", "hdi_rank")),
                  selectInput(inputId = 'ycol', label = "Y", choices = c("rank_18", "maternal_mortality_ratio_15",
                                                                         "adolescent_birth_rate_15_20", "seats_in_parliament_18"),
                              multiple = TRUE, selected = "maternal_mortality_ratio_15")), # end box 1 scatter
            box(status = "primary",
                solidHeader = TRUE,
                width = 20,
              plotOutput("plot1",
                       click = "plot_click",
                       dblclick = "plot_dblclick",
                       hover = "plot_hover",
                       brush = "plot_brush",
                       inline = FALSE
            ),
            verbatimTextOutput("info")) # end box 2 scatter 
    ), # end scatter plot tab
    tabItem(tabName = "involved",
            fluidRow(
              box(title = "Get Informed & Involved",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 20,
                  h1("Gender Equality in the News"),
                  tags$a(href = "https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html", "New York Times- 'Across The Globe, a Serious Backlash Against Women's Rights"),
                  p(" "),
                  tags$a(href = "https://www.bbc.com/news/world-51751915", "'BBC-- 'Gender study finds 90% of people are biased against women",
                  ),
                  h2("Get Involved"),
                  p("See list of ongoing interventions and ways to get involved"),
                  tags$a(href = "https://promundoglobal.org/work/", "Promundo"),
                  p(" "),
                  tags$a(href = "https://menengage.org/", "Men Engage"),
                  p(" "),
                  tags$a(href = "https://www.technoserve.org/what-we-do/women/?post_type=project", "Techno Serve"),
                  p(" "),
                  tags$a(href = "https://www.globalgiving.org/search/?size=25&nextPage=1&sortField=sortorder&selectedThemes=gender&loadAllResults=true", "Global Giving"),
                  p(" "),
                  tags$a(href = "https://womenvoicenetwork.com/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15TX_8nrpABjwIieQdyJgTCRgwCC42rOqrwOa0OMwL_3iHk4PXtJnhoCe24QAvD_BwE", "Women Voice Network"),
                  p(" "),
                  tags$a(href = "https://www.womenforwomen.org/why-women?src=GGEV222A&ms=cpc_google_awarness&utm_medium=cpc&utm_source=google&utm_campaign=awarness&utm_content=gg+ad&gclid=CjwKCAiA6seQBhAfEiwAvPqu15YD5SrFnaJz-3sM3iKAcftHXb8qZJyfFvTu5r889wcdBEAYa4STQhoC7nwQAvD_BwE", "Women for Women International")
            ))
            
    ),
    tabItem(tabName = "self_model",
            fluidRow(box(title = "Build a Model",
                         status = "primary",
                         solidHeader = TRUE,
                         p("Use the 'browse' button to upload data of your choice then select the target variable you wish to predict!"), 
                         p("When you are ready hit the 'Run Model' 
                           button."),
                         p("Once you are happy with the model, you can download the results using the 'Download Summary' button.")), box(fileInput("v_fileinput", label = "Upload File"),
                         uiOutput("target_var"),
                         actionButton("v_button", "Run Model"),
                         textInput("v_filename", label = "Filename", value = "model_results.csv", placeholder = "model_results.csv"),
                         downloadButton("v_download", "Download Summary"))),
            fluidRow(box(plotOutput("explore_plot")), box(plotOutput("model_plot")),
            
                     ) # end fluidRow
            ) # end tabName self_model
    
  ) # end tabItems
 )# end dashboardBody

### Choose theme 
app_theme <- bs_theme(
  bg = '#0b3d91',
  fg = 'white',
  primary = '#FCC780',
  base_font = font_google("Righteous")
)

## define UI for application
ui <- dashboardPage(header, sidebar, body, skin = "blue")






### create server function:
server <- function(input, output) {
  ### start server function
  
  # INTERACTIVE MAP
  ######first creating palette by ge index
  pal <- colorBin(
    palette = "viridis", domain = map_merged$gender_equality_index_18,
    bins = seq(0, max(map_merged$gender_equality_index_18, na.rm = TRUE) + .1, by = .1)
  )
  ###adding labels
  map_merged$labels <- paste0(
    "<strong> Country: </strong> ",
    map_merged$country, "<br/> ",
    "<strong> Gender Equality Index: </strong> ",
    map_merged$gender_equality_index_18, "<br/> ",
    "<strong> Maternal Mortality Ratio: </strong> ",
    map_merged$maternal_mortality_ratio_15, "<br/> ",
    "<strong> Female Labour Force Participation: </strong> ",
    map_merged$labour_force_participation_f_18, "<br/> ",
    "<strong> Female Secondary Education: </strong> ",
    map_merged$secondary_education_f_10_18, "<br/> ",
    "<strong> Number of Female Seats in Parliament: </strong> ",
    map_merged$seats_in_parliment_18, "<br/> "
  ) %>%
    lapply(htmltools::HTML)
  ## output of joemap interactive 
  output$joemap <- renderLeaflet({
    leaflet(map_merged) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 1.25) %>%
      addPolygons(
        fillColor = ~ pal(gender_equality_index_18),
        color = "white",
        fillOpacity = 0.7,
        label = ~labels,
        highlight = highlightOptions(
          color = "black",
          bringToFront = TRUE
        )
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~gender_equality_index_18,
        opacity = 0.5, title = "Gender Equality Index",
        position = "bottomleft"
      ) #%>% 
    # leaflet(options = leafletOptions(attributionControl = FALSE))%>% 
    #   addScaleBar(position = "bottomleft") 
  })
  
  ## REACTIVE DATA TABLE
  output$table <- renderDataTable({ 
    tab_data_table %>% 
      filter(Country %in% input$incountry) 
    # observe({
    #   print(input$incountry)
    # })
  })
  

  #### Scatter Plot ### 
  
  plotdata <- reactive({
    req(input$plotcountry)
    df <- tab_data_table %>% filter(Country %in% input$plotcountry)
  })
  
  # x_react <- reactive({
  #   req(input$xcol)
  #   df_x <- tab_data %>% filter(tab_data == input$xcol)
  # })
  # 
  # y_react <- reactive({
  #   req(input$ycol)
  #   df_y <- tab_data %>% filter(tab_data == input$ycol)
  # })
  
  # df2 <- reactive({gender_mod[, c(input$xCol, input$yCol)]})
  
  output$plot1 <- renderPlot({
    g <- ggplot(plotdata(), aes(y = tab_data_table$maternal_mortality_ratio_15, x = tab_data_table$gender_equality_index))
    g + geom_point()
  })
  
  # plot(plotdata(), pch = 20, cex = 3, col = "blue",
  #      main = "Interactive Scatter Plot")
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0(x=2, round(e$x, 1), y=4, round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0(xmin=2, round(e$xmin, 1), xmax= 2, round(e$xmax, 1), 
  #             ymin=4, round(e$ymin, 1),  ymax= 4, round(e$ymax, 1))
  #   }
  #   
  #   paste0(
  #     "click: ", xy_str(input$plot_click),
  #     "dblclick: ", xy_str(input$plot_dblclick),
  #     "hover: ", xy_str(input$plot_hover),
  #     "brush: ", xy_range_str(input$plot_brush)
  #   )
  # }) ### end scatter plot

  ####Slider Map Portion### 
  filter_range <- reactive({
    map_data %>% 
      dplyr::filter(gender_equality_index_18 >= as.numeric(input$mapsel[1])) %>% 
      dplyr::filter(gender_equality_index_18 <= as.numeric(input$mapsel[2]))
  })
  
  maxLong = max(map_data$longm)
  maxLat = max(map_data$latm)
  minLong = min(map_data$longm)
  minLat = min(map_data$latm)
  
  output$map <- renderLeaflet({
    leaflet(filter_range()) %>% 
      addTiles() %>% 
      addMarkers(data = filter_range(), lng = ~longm, lat = ~latm,) %>%
      fitBounds(minLong,minLat,maxLong,maxLat)
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$mapsel[1])
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet(filter_range()) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addCircleMarkers(lng = ~longm, 
                       lat = ~latm 
                       #layerId = ~location,
                       # adding clusterOptions removes the group in observeEvent
                       #clusterOptions = markerClusterOptions() 
      )
  })
  
  output$df <- renderTable({filter_range()})

## Function for MODEL BUILD TAB
  file_data <- reactive({
    file <- input$v_fileinput
    
    if(!is.null(file)){read.csv(file$datapath)}
  })
  
  output$v_download <- downloadHandler(
    filename = function() {
      paste(input$v_filename)
    },
    content = function(file) {
      write_csv(modelRun(), file)
    }
  )
  
  modelRun <- eventReactive(input$v_button, {
    lm(as.formula(paste0(input$v_target, "~.")), data = file_data()) %>% 
      broom::tidy() 
  })
  
  output$explore_plot <- renderPlot({
    req(file_data())
    ggduo(file_data())
  })
  
  output$target_var <- renderUI({
    req(file_data())
    
    selectInput("v_target", "Target Variable", choices = colnames(file_data() %>% select_if(is.numeric)))
  })
  
  
  output$model_plot <- renderPlot({
    
    modelRun() %>%   
      ggplot(aes(x = term, y = estimate, color = p.value <= 0.05)) + 
      geom_segment(aes(xend = term, yend = 0)) + 
      geom_point() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste0("Coefficients for predicting ", input$v_target),
           subtitle = "Using a linear model")
  })
  
}

### combine into an app:
shinyApp(ui = ui, server = server) 




# ### create server function:
# server <- function(input, output) { ### start server function
#   
#   
#   ## function for data table
#   output$table <- renderDT({
#     tab_data_table
#   })
#   
#   ## function for interactive map
#   output$joemap <- renderLeaflet({
#     leaflet(options = leafletOptions(attributionControl = FALSE)) %>% 
#       addProviderTiles(providers$OpenStreetMap) %>% 
#       addScaleBar(position = "bottomleft") 
#   })
#   
#   ## Scatterplot function
#   output$info <- renderText({
#     xy_str <- function(e) {
#       if(is.null(e)) return("NULL\n")
#       paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
#     }
#     xy_range_str <- function(e) {
#       if(is.null(e)) return("NULL\n")
#       paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
#              " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
#     }
#     
#     paste0(
#       "click: ", xy_str(input$plot_click),
#       "dblclick: ", xy_str(input$plot_dblclick),
#       "hover: ", xy_str(input$plot_hover),
#       "brush: ", xy_range_str(input$plot_brush)
#     )
#   }) ### end scatter plot
# } ### end server function 






##### USEFUL CODE TO DRAW FROM 

## OLD INTERACTIVE MAP FUNCTION
## Interactive map function---- need to edit to make work for my data
# observe({
#   if(!is.null(input$year)){
#     map <- joinCountryData2Map(selected(), joinCode = "ISO3",
#                                nameJoinColumn = "ISO3")
#     leafletProxy("worldmap", data = map) %>%
#       addTiles() %>% 
#       clearShapes() %>% 
#       addPolygons(fillColor = ~pal(map$score),
#                   weight = 2,
#                   opacity = 1,
#                   color = "white",
#                   dashArray = "3",
#                   fillOpacity = 0.7,
#                   highlight = highlightOptions(
#                     weight = 5,
#                     color = "white",
#                     dashArray = "3",
#                     fillOpacity = .8,
#                     bringToFront = TRUE),
#                   label = ~paste(as.character(map$country),
#                                  "Total Index Score: ", as.character(map$score)))
#   }})


## OLD UI UPDATED ABOVE!
# ui <- 
#   navbarPage("Understanding the State of Gender Equality Globally", theme = app_theme,
#              tabPanel("Home",
#                       "While Gender Equality has been identified as a critical global goal by the UN and various multinational organizations, 
#                       Gender Equality remains far from realized.The purpose of this application is to increase awareness of the current state of 
#                       gender equality globally (i.e. across regions and countries) to increase awareness, give individuals resources to become changemakers in their own communities,
#                       and promote positive change.",
#                       fluidPage(
#                         mainPanel(
#                           img(src = "gender-page_v-08.jpeg", height = 350, width = 350)
#                         )
#                       )# end  fluid page "Home"
#              ), # end tab panel "home"
#              tabPanel("Statistics by Region", "Quick summary on state of affairs", # Can change later to make drop down with different outcomes to chose from
#                       fluidPage(
#                         tabsetPanel(
#                           tabPanel("Women's Empowerment Score", br(),
#                                    sidebarLayout(
#                                      sidebarPanel(
#                                        selectInput(inputId = "pt_color",
#                                                    label = "Choose World Region",
#                                                    choices = c("North America",
#                                                                "Central America", 
#                                                                "South America",
#                                                                "Asia",
#                                                                "Europe",
#                                                                "Africa")),
#                                        radioButtons(inputId = "gender_data",
#                                                     label = "Choose Variables",
#                                                     choices = c("Year", 
#                                                                 "Income", 
#                                                                 "Education"))
#                                      ), # end sidebar panel
#                                      mainPanel(
#                                        img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
#                                        plotOutput(outputId = "gender_map")) # end mainpanel
#                                    ) # end sidebarLayout
#                           ), # end tabPanel "women empowerment score"
#                           tabPanel("Acceptance of IPV", br(),
#                                    sidebarLayout(
#                                      sidebarPanel(
#                                        selectInput(inputId = "pt_color",
#                                                    label = "Choose World Region",
#                                                    choices = c("North America",
#                                                                "Central America", 
#                                                                "South America",
#                                                                "Asia",
#                                                                "Europe",
#                                                                "Africa")),
#                                        radioButtons(inputId = "gender_data",
#                                                     label = "Choose Variables",
#                                                     choices = c("Year", 
#                                                                 "Income", 
#                                                                 "Education"))
#                                      ), # end sidebar panel
#                                      mainPanel(img(src = "gender-page_v-08.jpeg", height = 350, width = 350),
#                                                plotOutput(outputId = "gender_map")) # end mainpanel
#                                    ) # end sidebarLayout
#                           ), # end tabPanel "Acceptance of IPV")
#                           
#                         ) # end tabsetPanel
#                       )),
#              tabPanel("Interactive Map", 
#                       fluidPage(
#                         
#                         # App title
#                         titlePanel("Gender Gap Index Data"),
#                         
#                         # Sidebar layout with input and output definitions
#                         sidebarLayout(
#                           
#                           # Sidebar panel for inputs 
#                           sidebarPanel(
#                             
#                             # First input: Type of data
#                             selectInput(inputId = "data_type",
#                                         label = "Choose the type of data you want to see:",
#                                         choices = list("Childlessness" = "Childlessness", "Gender Gap Index" = "Gender Gap Index")),
#                             
#                             # Second input (choices depend on the choice for the first input)
#                             uiOutput("secondSelection"),
#                             
#                             # Third input (choices depend on the choice for the first and second input)
#                             uiOutput("thirdSelection")
#                             
#                           ),
#                           
#                           # Main panel for displaying outputs
#                           mainPanel(
#                             
#                             # Hide errors
#                             tags$style(type = "text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"),
#                             
#                             # Output: interactive world map
#                             girafeOutput("distPlot")
#                             
#                           )
#                         ) #end sidebarlayout ##interactivemap
#                       ) ## end fluidpage "interactive map"
#              ),
#              tabPanel("Slider of GE Index", "Use the slider tool to select a value from the gender equality index and see which countries reflect these values",
#                       fluidRow(
#                         column(4,
#                                sliderInput("slider2", label = h3("Slider Range"), min = 0,
#                                            max = 100, value = c(40, 60))
#                         ) # end column
#                       ) # end fluid row
#              ), # end tab panel "Slider of ge"
#              tabPanel("Scatter Plot", "lookie here its a plot that you can play with",
#                       tabPanel("Scatter Plot", 
#                                plotOutput("plot1",
#                                           click = "plot_click",
#                                           dblclick = "plot_dblclick",
#                                           hover = "plot_hover",
#                                           brush = "plot_brush"
#                                ),
#                                verbatimTextOutput("info") 
#                       ) # end scatter plot
#              ), # END END 
#              tabPanel("Get Informed & Involved", "We understand that understanding the problem is only the start to working towards solutions. 
#                       The resources below provide opportunities for users to get informed on and involved in ongoing global movements towards gender equality. These resources include interventions
#                       targeted at women's empowerment, initiatives to engage men as changemakers and promoters of gender equality, and resources for those who themselves may need support.",
#                       fluidPage(
#                         tabsetPanel(
#                           tabPanel("Gender Equality in the News", 
#                                    "New York Times- 'Across The Globe, a Serious Backlash Against Women's Rights' 
#                                    --https://www.nytimes.com/2019/12/04/us/domestic-violence-international.html",
#                                    
#                                    "'BBC-- 'Gender study finds 90% of people are biased against women' ----- https://www.bbc.com/news/world-51751915",
#                                    ""
#                                    , br()),
#                           tabPanel("Current Interventions", "See list of ongoing interventions and ways to get involved",
#                                    "Promundo--- https://promundoglobal.org/work/",
#                                    "Men Engage--- https://menengage.org/",
#                                    "Techno Serve--- https://www.technoserve.org/what-we-do/women/?post_type=project",
#                                    "Global Giving---- https://www.globalgiving.org/search/?size=25&nextPage=1&sortField=sortorder&selectedThemes=gender&loadAllResults=true",
#                                    "Women Voice Network--- https://womenvoicenetwork.com/?gclid=CjwKCAiA6seQBhAfEiwAvPqu15TX_8nrpABjwIieQdyJgTCRgwCC42rOqrwOa0OMwL_3iHk4PXtJnhoCe24QAvD_BwE",
#                                    "Women for Women International--- https://www.womenforwomen.org/why-women?src=GGEV222A&ms=cpc_google_awarness&utm_medium=cpc&utm_source=google&utm_campaign=awarness&utm_content=gg+ad&gclid=CjwKCAiA6seQBhAfEiwAvPqu15YD5SrFnaJz-3sM3iKAcftHXb8qZJyfFvTu5r889wcdBEAYa4STQhoC7nwQAvD_BwE"
#                           ),  # End tabPanel "current interventions,
#                           tabPanel("Resources for Support", "Here is a list of organizations that you can reach out to if you are experiencing harm/in need of support",
#                                    "National Domestic Violence Hotline---- https://www.thehotline.org/",
#                                    "Safe Horizon---- https://www.womenindistress.org/we-can-help/24-hour-crisis-hotline/")
#                           
#                         )
#                       ) # end fluid page "get involved"
#              ) # end tab panel "get involved"
#              )

