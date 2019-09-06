
#-------------------------------
# Data Open
# Mark Erenberg
#-------------------------------

invisible(lapply(c("data.table", 
                   "tidyverse",
                   "ggplot2",
                   "zoo",
                   "leaflet",
                   "maps",
                   "shiny",
                   'shinydashboard',
                   'shinythemes',
                   'htmltools',
                   'RColorBrewer',
                   'housingData',
                   'plotly',
                   'highcharter',
                   'shinyjs',
                   "outliers",
                   "s20x",
                   "usmap",
                   "semantic.dashboard"
),
library, character.only=TRUE))

setwd("C:\\Users\\marke\\Downloads\\Data Open\\Datathon Materials 2")

chemicals <- fread("chemicals.csv")
droughts <- fread("droughts.csv")
industry <- fread("industry_occupation.csv")
water <- fread("water_usage.csv")

data <- list(chemicals,droughts,earnings,education,industry,water)
sapply(data,head)


# Count missing/NA values in train data
#sapply(df, function(x){sum(is.na(x))})
#sapply(df, function(x){length(which(x == ''))})
#sapply(df, function(x){length(which(x == 0))})


########################### Data Manipulation #############################

# Convert all chemical units to same measurement
chemicals$converted_value = ifelse(chemicals$unit_measurement == 'milligrams/L',
                                   chemicals$value * 1000,
                                   chemicals$value)
# Attach chemicals MCL values
chemicals <- chemicals %>% mutate(
  mcl = case_when(
    chemical_species == 'Uranium' ~ 30,
    chemical_species == 'Arsenic' ~ 10,
    chemical_species == 'DEHP' ~ 6,
    chemical_species == 'Nitrates' ~ 10000,
    chemical_species == 'Halo-Acetic Acid' ~ 60,
    chemical_species == 'Trihalomethane' ~ 80,
  )
)


#county_chem$ranking <- as.numeric(as.factor(county_chem$level))

# Create following features:
#   For each county, year, and chemical:
#     i)   Contaminated: If non-detect
#     ii)  Sum of CWS contaminated
#     iii) Sum of CWS with greater than MCL (severe contamination)
#     iv)  Sum of Population contaminated (population served by contaminated water)
#     v)   Sum of Population with greater than MCL (severe contamination)
county_lvl <- data.frame(
  chemicals %>%
    mutate(ranking = as.numeric(as.factor(contaminant_level)),
           rev_ranking = rev(ranking),
           contaminated = ifelse(contaminant_level == 'Greater than MCL',1,0)
           ) %>%
    group_by(county,state,state_fips,fips,year,chemical_species) %>%
    summarize(
      # mcl values
      mcl = mean(mcl),
      # total counties and total population served
      total_cws = n(),
      total_pop_served = sum(pop_served),
      # Does the county have a hazardous water supply?
      hazardous = ifelse(1 %in% contaminated,1,0),
      # How many water systems are hazardous (as sum and %)
      cws_haz = sum(contaminated == 1),
      cws_perc_haz = sum(contaminated == 1)/sum(contaminated!=-1) * 100,
      # What is the population of people using hazardous water supplies (as sum and %)
      pop_haz = sum(pop_served[contaminated==1]),
      pop_perc_haz = sum(pop_served[contaminated==1])/sum(pop_served[contaminated>=0])*100 ,
      # Minimum contaminant level in county (across all cws)
      min_level = min(contaminant_level),
      ranking = min(ranking),
      rev_ranking = max(rev_ranking),
      # Sum, mean, and median of chemical value in county
      sum_value = sum(converted_value),
      mean_value = mean(converted_value),
      med_value = median(converted_value)
    ) %>%
    subset(year >= 2010)
)

# Rename states column for highcarter map
names(county_lvl)[which(names(county_lvl) == 'state')] = 'region'
#names(county_lvl)[which(names(county_lvl) == 'value')] = 'cws_perc_haz'
names(county_lvl)[which(names(county_lvl) == 'med_value')] = 'value'



# Since it looks droughts only last 6 days (??), we summarize droughts by
# counting droughts by year
droughts$year = as.numeric(substring(droughts$valid_start,0,4))
droughts$valid_start <- as.Date(droughts$valid_start, format = "%Y-%m-%d")
droughts$valid_end <- as.Date(droughts$valid_end, format = "%Y-%m-%d")

# Group Droughts df by county, summarize by taking sum
county_dr <- data.frame(
  droughts %>%
    group_by(fips,county,state,year) %>%
    summarize(d0 = sum(d0 > 0,na.rm=T),
              d1 = sum(d1 > 0,na.rm=T),
              d2 = sum(d2 > 0,na.rm=T),
              d3 = sum(d3 > 0,na.rm=T),
              d4 = sum(d4 > 0,na.rm=T)) %>%
    subset(year >= 2010 & year <= 2016) 
)

# Group Droughts by severity, summarizing count of each drought
# Group Droughts df by county, summarize by taking sum
drought_severity <- county_dr %>%
  gather(key = "severity", value = "count",
         d0, d1, d2, d3, d4)

# Merging county_chemicals and county_droughts dfs for graphs, grouped by fips & year
county_chem_dr <- left_join(
  x = county_lvl,
  y = county_dr,
  by = c('fips', 'year')
)


# Merge county_chemicals and drought dataframes grouped by county, year
dr_county = inner_join(
  x = droughts,
  y = dplyr::select(county_lvl, fips, year, chemical_species, value, mcl),
  by = c('fips','year')
)

# Gather all columns in water to group them
# water_usage variables in predicting median chemical value
water_join <- inner_join(
  x = water,
  y = dplyr::select(county_lvl, state_fips, fips, year, chemical_species, value),
  by = c('state_fips','fips','year')
)

# Remove correlated/useless variables
water_join <- 
  water_join %>%
  dplyr::select(-state, -county, -county_fips, -ps_total, -d_totaluse,
                  -crop_ir_sprinkler, -crop_ir_microirrig, -ir_total, -crop_ir_surface,
                -crop_ir_total,-population, -ir_microirrig)


# Remove total_employed column from industry table
industry <- fread("industry_occupation.csv")

# Add state fips code
industry$state = gsub("^.*?, ","",industry$county)
industry$state_fips = as.integer(fips(state=industry$state,county=c()))

indus_gath <- dplyr::select(industry, -total_employed)

# Gather industy table
indus_gath <- gather(indus_gath, key = "industry", value = "working_pop",
                   -geo_id,-fips,-state,-state_fips,-county,-year)



####################### Shiny Dashboard ###########################

jscode <- '
$("#county_map").on("click", function(){
  Shiny.onInputChange("Clicked", 
  [event.point.name,event.point.region,event.point.fips,event.point.state_fips]);
})
'


ui <- shinyUI(
  dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(disable=T),
  dashboardBody(
    h1('Chemical Contaminant Interactive Dashboard'),
    fluidRow(
        box(title = 'Median Chemical Value Across Water Supplies (ug/L)',
          highchartOutput("county_map"),
          singleton(tags$script(HTML(jscode))),
          width = 7, height = "500px",
          ribbon = TRUE, color = 'red'),
        box(
         h3("Hello World!"), h3('\n'),
         h5("This dashboard is designed to visually identify key factors impacting chemical concentrations."),
         h5("These concentrations exist across US county water supplies."), 
         h5("The underlying data set contains info on contaminant levels, drought severities, industry occupations, and water usage from 2010 to 2016."),    
         h5("To get started, select:"),
         h5("    1) A year from the slider below"),
         h5("    2) A chemical substance from the dropdown below"),
         h5("    3) A county from the map on the left"), h5('\n'),
         sliderInput(inputId = "year", 
                     label = "Year", 
                     min = min(county_lvl$year),
                     max = max(county_lvl$year),
                     value = 2010, 
                     ticks = T,
                     round = T),
         selectInput(inputId = "chemical", 
                     label = "Chemical Contaminant", 
                     choices = sort(unique((county_lvl$chemical_species))),
                     selected = "Arsenic", 
                     multiple = F),
         width = 5, height = "500px")
        ),
    fluidRow(
      valueBoxOutput("total_box", width = 4),
      valueBoxOutput("median_box", width = 4),
      valueBoxOutput("pop_box", width = 4)),
    fluidRow(
      box(title = 'Time Series Plot of Chemical Contaminant vs Population Served',
        plotlyOutput('timeseries'),width=12,height='400px')),
    h3("Drought Severity Data"),
    fluidRow(
      box(plotlyOutput("severity"), width = 6, height = '400px'),
      box(plotlyOutput("sev_scatter"), width = 6, height = '400px')
    ),
    h3("Water Usage Data"),
    fluidRow(
      #box(dataTableOutput("public"), width = 4, height = '400px')
      box(plotlyOutput("public"), width = 4, height = '450px'),
      box(plotlyOutput("irrigation"), width = 4, height = '450px'),
      box(plotlyOutput("thermal"), width = 4, height = '450px')
    ),
    h3("Industry Occupation Data"),
    fluidRow(
      box(plotlyOutput("industry"), width = 12, height = '500px')
      )
    ),
  theme = 'lux'
  
  )
)


server <- function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  Data <- reactive({
    county_lvl[county_lvl$year == input$year &
                 county_lvl$chemical_species == input$chemical,]
  })
  
  output$county_map <- renderHighchart({
    
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
    
    highchart(type = "map") %>% 
      hc_add_series(mapData = uscountygeojson, 
                    data = Data(),
                    value = "value",
                    joinBy = "fips",
                    borderWidth = 0.1,
                    bordercolor = "#FAFAFA") %>% 
      hc_colorAxis(minColor = 'white', maxColor = 'red', max = max(Data()$mcl)) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = "",
                 pointFormat = "{point.name} - {point.region}: {point.value}") %>%
      hc_mapNavigation(enabled = TRUE)
    
  })
  
  makeReactiveBinding("clickText")
  
  observeEvent(input$Clicked, {
    clickText <<- paste0(input$Clicked)
  })
  
  
  # Reactive expression for the data subsetted to what the user selected
  county_stats <- reactive({
    county_lvl[county_lvl$fips == as.integer(clickText[3]) &
                 county_lvl$year == input$year &
                 county_lvl$chemical_species == input$chemical,]
  })

  # Value Boxes for county statistics
  output$total_box <- renderValueBox({
    if (length(county_stats()$total_cws) == 0){
      valueBox(
        paste0("0%"), "Contaminated Water Systems With Hazardous Levels", icon = icon("wrench"),
        color = "aqua")
    }
    else {
      val <- round(county_stats()$cws_perc_haz,0)
      if (val >= 50){
        valueBox(
          paste0(val, "%"), "Contaminated Water Systems With Hazardous Levels", icon = icon("wrench"),
          color = "red")
      }
      else if (val >= 25) {
        valueBox(
          paste0(val, "%"), "Contaminated Water Systems With Hazardous Levels", icon = icon("wrench"),
          color = "yellow")
      }
      else {
        valueBox(
          paste0(val, "%"), "Contaminated Water Systems With Hazardous Levels", icon = icon("wrench"),
          color = "green")
      }
    }
  })
  
  output$median_box <- renderValueBox({
    if (length(county_stats()$value) == 0){
      valueBox(
        paste0("0 Mg"), "Median Contaminant Value Across Water Systems", icon = icon("tint"),
        color = "aqua")
    }
    else {
      med <- round(county_stats()$value,0)
      mcl <- max(county_stats()$mcl)
      if (med > mcl){
        valueBox(
          paste0(med, " ug/L"), "Median Contaminant Value (Greater Than MCL)", icon = icon("tint"),
          color = "red")
      }
      else {
        valueBox(
          paste0(med, " ug/L"), "Median Contaminant Value (Less Than/Equal To MCL)", icon = icon("tint"),
          color = "green")
      }
    }
  })
  
  output$pop_box <- renderValueBox({
    if (length(county_stats()$pop_haz) == 0){
      valueBox(
        paste0("0"), "People Supplied With Contaminated Water", icon = icon("user"),
        color = "aqua")
    }
    else {
      valueBox(
        paste0(county_stats()$pop_haz), "People Supplied With Contaminated Water", icon = icon("user"))
    }
  })
  
  
  # Reactive expression for the time series data 
  TimeSeries <- reactive({
    county_lvl[county_lvl$chemical_species == input$chemical &
                 county_lvl$fips == as.integer(clickText[3]),]
  })
  
  output$timeseries <- renderPlotly({
    TimeSeries() %>%
      plot_ly() %>%
      add_trace(x = ~year, y = ~value, color = ~chemical_species, type = "bar") %>%
      add_trace(x = ~year, y = ~total_pop_served, mode = 'lines',type='scatter',
                name = 'Population Served', yaxis = 'y2', 
                line = list(color = '#45171D')) %>%
      layout(title = 'Chemical Content vs Population Served',
             xaxis = list(title = " Year"),
             yaxis = list(side = 'left', title = 'Median Chemical Value (ug/L)'),
             yaxis2 = list(side = 'right', overlaying = "y",
                           title = 'Total Population Served'))
  })
  
  # Reactive expression for the severity subset
  Severity_Data <- reactive({
    drought_severity[drought_severity$fips == as.integer(clickText[3]) &
                       drought_severity$year == input$year,]
  })
  
  output$severity <- renderPlotly({
    Severity_Data() %>%
      plot_ly(x = ~count, y = ~severity, type = 'bar', 
              orientation = 'h', color = ~severity) %>%
      layout(title = paste0('Drought Severity In ',clickText[1],' - ',clickText[2]),
             xaxis = list(title = 'Count of Droughts'),
             yaxis = list(title = 'Drought Severity'))
  })
  
  # Reactive expression for the severity subset
  Droughts_Resp <- reactive({
    dr_county[dr_county$fips == as.integer(clickText[3]) &
                  dr_county$chemical_species == input$chemical &
                  dr_county$year == input$year,]
    
  })
  
  output$sev_scatter <- renderPlotly({
     Droughts_Resp() %>%
      plot_ly(
        x = ~jitter(d0,2), y = ~value, type = 'scatter', mode = 'markers',
        marker = list(size = 10),
        #jitter = 0.2,
        # Hover text:
        text = ~paste("Median Chemical Value: ", value, '$<br>d0:', d0),
        color = ~value
      ) %>%
      layout(title = paste0('Drought Impact vs Chemical Value In ',
                            clickText[1],' - ',clickText[2]),
             xaxis = list(title = '% of Population Affected by Drought',
                          range = c(0,100)),
             yaxis = list(title = 'Median Chemical Value', 
                          range = c(0,max(Droughts_Resp()$mcl,
                                          max(Droughts_Resp()$value))+10)),
             shapes=list(type='line', x0= 0, x1= 100,
                         y0=max(Droughts_Resp()$mcl), y1=max(Droughts_Resp()$mcl),
                         line=list(dash='dot', width=1, 
                                   color = 'rgb(205, 12, 24)'))) %>%
      hide_colorbar()
  })
  
  # Reactive expression for Water Usage datasets
  Water <- reactive({
    water_join[water_join$state_fips == as.integer(clickText[4]) &
                  water_join$chemical_species == input$chemical & 
                  water_join$year == input$year,]
    
  })
  
  output$public <- renderPlotly({
     dplyr::select(Water(),ps_groundwater,ps_surfacewater,value) %>%
      plot_ly(x = ~log(ps_groundwater), y = ~log(ps_surfacewater), z = ~log(value),
              colors = "Blues", showscale = T,
              colorbar = list(title = 'Chemical Value (log)')) %>%
      add_trace(type='histogram2dcontour') %>%
      layout(title = 'Public Ground Water vs Surface Water',
             xaxis = list(title = 'Served by Ground (thousands population)'),
             yaxis = list(title = 'Served by Surface (thousands population)'))
  })
  
  output$irrigation <- renderPlotly({
     dplyr::select(Water(),ir_sprinkler,ir_surface,value) %>%
      plot_ly(x = ~log(ir_sprinkler), y = ~log(ir_surface), z = ~log(value),
              colors = "Blues", showscale = T,
              colorbar = list(title = 'Chemical Value (log)')) %>%
      add_trace(type='histogram2dcontour') %>%
      layout(title = 'Acres Irrigated By Sprinkler vs Surface Water',
             xaxis = list(title = 'Irrigated by Sprinkler (thousands acres)'),
             yaxis = list(title = 'Irrigated by Surface (thousands acres)'))
  })
  
  output$thermal <- renderPlotly({
     dplyr::select(Water(),therm_power,d_selfsupplied,value) %>%
      plot_ly(x = ~log(therm_power), y = ~log(d_selfsupplied), z = ~log(value),
              colors = "Blues", showscale = T, 
              colorbar = list(title = 'Chemical Value (log)')) %>%
      add_trace(type='histogram2dcontour') %>%
      layout(title = 'Self-Supplied Population vs Thermoelectric Power',
             xaxis = list(title = 'Self-supplied (thousands population)'),
             yaxis = list(title = 'Thermoelectric Power (thousands generated)'))
  })
  
  # Reactive expression for industry occupation data
  Industry <- reactive({
    indus_gath[indus_gath$state_fips == as.integer(clickText[4]) &
                 indus_gath$year == input$year,]
  })
  
  output$industry <- renderPlotly({
    Industry() %>%
      plot_ly(y = ~log(working_pop), color = ~industry, type = "box") %>%
        layout(title = paste0('Boxplot of Working Populations In ',clickText[2]),
               yaxis = list(title = 'Working Population (log)'))
  })
  
}

shinyApp(ui, server)



  

####################### Quick Analysis ###########################

# We run hypothesis testing using linear regression model to test significance of
# water_usage variables in predicting median chemical value

# Remove redundant variables, filter by chemical species
arsenic <- 
  water_join %>%
  filter(chemical_species == 'Arsenic')


# Run EDA on sections of variables
pairs20x(arsenic[,c(1:6, 18)],na.rm=T)
pairs20x(arsenic[,c(7:14, 18)],na.rm=T)
pairs20x(arsenic[,c(15:18)],na.rm=T)

# Can remove crop_ir, ps_total, d_total, variables
arsenic <- dplyr::select(arsenic, -ps_total, -d_totaluse,
                  -crop_ir_sprinkler, -crop_ir_microirrig,
                  -ir_total, -crop_ir_surface, -crop_ir_total,
                  -population, -ir_microirrig)
 
pairs20x(arsenic[,c(4:11,13)], na.rm=T)

# All variables have positive skew! Use Cook's distance to remove outliers:
features <- arsenic[,4:11]
for(feat in features){
  qnt <- quantile(feat, probs=c(.25, .75), na.rm = T)
  
}




# To adjust response:
resp <- arsenic$value
hist(resp^(1/5))


# Run stepwise regression
full.model <- lm(value ~., data = na.omit(log(arsenic)))
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# Plot diagnostic plots
par(mfrow=c(2,2))
plot(step.model,which=1:4)

# Plot studentized residuals
# Plot diagnostic plots
par(mfrow=c(2,2))
for(x in names(step.model$coefficients)[-1]){
  plot(na.omit((arsenic^0.1575))[,x], rstudent(step.model), ylim=c(-5, 5),
  xlab=x, ylab="Studendized Residuals")
  abline(2, 0)
  abline(-2, 0)
}



## Analyze industry dataset:
ind_a <- dplyr::select(industry,-geo_id,-fips,-county,-year)
pairs20x(ind_a,na.rm=T)



plot_ly(indus_gath, y = ~log(working_pop), color = ~industry, type = "box") %>%
  layout(title = 'Boxplot of State Working Populations By Industry',
         yaxis = list(title = 'Working Population (log)'))
