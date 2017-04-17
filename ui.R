#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MacroMenu", windowTitle = "MacroMenu"),
  
  sidebarLayout(
    sidebarPanel(
      ##Leitarskylyrði fyrir Spoonacular
      "*ATH: vegna tæknilegra erfiðleika (encoding from base 64) er ekki hægt að besta matseðilinn í gegnum appið.
        Hafið samband við okkur um að fá .mod og .run skrá og notið ásamt því .dat skránna sem appið skilar til þess að
        nota virkni appsins.*",
      "Welcome to MacroMenu!",
       "Please select your preferences for monday and follow the instructions.
       Your results will be dishes for each monday of the next four weeks. 
       Save your results and repeat for each day of the week.
       Put a + sign between words.",
      textInput("mashString",
                label = "Paste your MASHKEY here below",
                value = "JPyJiSw77smsh53peSzBvFOxgKRXp1uhNkljsn4o5SAoHJ11SL",
                width = '100%'),
      textInput("cuisineString",
                label = "cuisine=",
                value = "asian",
                width = '100%'),
      textInput("dietString",
                label = "diet=",
                value = "",
                width = '100%'),
      
      #Tegund disks fyrir hvern dag
      textInput("queryString",
                label = "query=",
                value = "fish",
                width = '100%'),

      
      sliderInput("fjoldi", label = h5("fjoldi uppskrifta sottar i uppskriftasafn"), min = 4, max = 100, value=10),
      


      #Buttons
      actionButton("seek", "1. Search Spoonacular", icon = icon("search"), width='100%'),
      actionButton("ampl", "2. Write AMPL data", icon = icon("pencil"), width='100%'),
      actionButton("neossend", "3. Send to Neos", icon = icon("send"), width='100%'),
      actionButton("neosget", "4. Try to get result from Neos", icon = icon("refresh"), width='100%'),
      
      #áherslur markfalls
      sliderInput("A", label = h5("Mikilvægi þess að rétturinn sé sem ódýrastur"), min = 1, max = 10, value=10),
      sliderInput("B", label = h5("Mikilvægi þess að rétturinn sé sem vinsælastur"), min = 1, max = 10, value=6),
      sliderInput("C", label = h5("Mikilvægi þess að eldunartími réttsins sé sem minnstur"), min = 1, max = 10, value=4),
      
      #Skorður Prótíns, fitu og carbs
      numericInput("maxProtein", label = strong("Hámarkspróteinmagn:"), step = 1, value = 500),
      numericInput("minProtein", label = strong("Lágmarkspróteinmagn:"), step = 1, value = 1),
      numericInput("maxFat", label = strong("Hámarksfitumagn:"), step = 1, value = 500),
      numericInput("minFat", label = strong("Lágmarksfitumagn:"), step = 1, value = 1),
      numericInput("maxCarbs", label = strong("Hámarkskolvetnismagn:"), step = 1, value = 500),
      numericInput("minCarbs", label = strong("Lágmarkskolvetnismagn:"), step = 1, value = 1),
      
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("urlString"),
      dataTableOutput("recipeString"),
      textOutput("writeString"),
      textOutput("optimizeString"),
      textOutput("neosString")
    )
  )
))