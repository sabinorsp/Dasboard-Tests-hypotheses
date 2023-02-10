# import packages
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinycssloaders)
library(tidyverse)
library(broom)
library(bslib)
library(thematic)
library(DT)
library(plotly)
options(warn = -1)

# Configurations about UI:
ui <- navbarPage(
  
  # Cria instância do shinyjs
  shinyjs::useShinyjs(),
  
  # Tema de cores do dashboard
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#3c0072",
                   secondary = "#5340f7",
                   success = "#dadeba"
  ),
  
  # Estilo
  tags$style(type = 'text/css', '#nometestedesc {white-space: pre-wrap;}'),
  
  # Título do Dashboard
  title = "Dashboard for Statistics Hypotheses Tests",
  tabPanel(
    title = "Home",
    sidebarLayout(
      
      # Painel lateral
      sidebarPanel(
        
        # Inputs:
        fileInput(
          inputId = 'file_input',
          label="Import Data File(.csv or .xlsx)",
          accept = c(".csv", ".xlsx"),
          width = NULL,
          buttonLabel = "Select File:",
          placeholder = "No file selected"
        ),
#=============================================================================
        selectInput("group_test",label="Select Test Objetive",
                    choice=c('For compare Means',
                             'For compare Distributions',
                             'For compare Variations'),
                    selectize = F
          ),
        uiOutput('select_test'),
        uiOutput('config_test'),
        uiOutput('select_data'),
        uiOutput('mu'),
        uiOutput("confidencelevel"),
        h5(
          actionButton(
            inputId = "generate",
            label = "Execute Test"
          ),
          align = "center"
        )
      ),
#=============================================================================
      
      # Painel principal
      mainPanel(
        fluidRow(
          uiOutput('table'),
          #'Data Head:',
          #tableOutput("table"),
          'Shape data (n_rows, n_cols)',
          verbatimTextOutput("dim"),
          'Mean about each colnms:',
          verbatimTextOutput("colmeans")
        ),
        fluidRow(
            h4(textOutput("testresulttitle")), 
            withSpinner(DTOutput("testresult"), type = 7), 
          ),
        fluidRow(br()),
        fluidRow(h4(textOutput("descriptiontitle")), align = "center"),
        fluidRow(
          withSpinner(verbatimTextOutput("nometestedesc"), type = 7)
        ),
        fluidRow(
            h4(textOutput("histogramtitle")), 
            withSpinner(plotlyOutput("hist", width = "100%"), type = 7), 
          )
      )
    )
  ),
  nav_item(a(href = "https://nosy-organ-7ad.notion.site/Testes-de-Hip-teses-721a847566574d988d19be59ad4e6c30", "Help"))
  
)
