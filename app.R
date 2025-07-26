#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# Australian Privacy Compliance Shiny App
# This app identifies sensitive fields in uploaded files according to 
# Australian Privacy Principles (APPs) and allows downloading of redacted versions

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(writexl)

# Define sensitive field patterns and their related privacy principles
sensitive_fields <- list()

# Personal identifiers
sensitive_fields[["name|first name|last name|surname|given name"]] <- 
  "Personal identifier under APP 6 (Use and disclosure) and APP 11 (Security)"

sensitive_fields[["dob|date of birth|birth date|birthday"]] <- 
  "Personal identifier under APP 3 (Collection) and APP 11 (Security)"

sensitive_fields[["address|street|suburb|postcode|zip|state|country"]] <- 
  "Personal identifier under APP 6 (Use and disclosure) and APP 11 (Security)"

sensitive_fields[["email|e-mail|mail"]] <- 
  "Contact information under APP 6 (Use and disclosure) and APP 11 (Security)"

sensitive_fields[["phone|mobile|telephone|cell|contact number"]] <- 
  "Contact information under APP 6 (Use and disclosure) and APP 11 (Security)"

# Sensitive information (special categories under APP 3)
sensitive_fields[["health|medical|diagnosis|treatment|medication|condition|disability"]] <- 
  "Health information (sensitive) under APP 3.3 and OVIC IPP 10 (Sensitive Information)"

sensitive_fields[["racial|ethnic|race|ethnicity|language|nationality|indigenous|aboriginal|torres"]] <- 
  "Racial/ethnic information (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["religion|religious|belief|church|faith|spirituality"]] <- 
  "Religious beliefs (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["political|politics|party|vote|voting|election"]] <- 
  "Political opinions (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["sexual|sexuality|lgbtq|orientation"]] <- 
  "Sexual orientation (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["gender|sex|legal sex"]] <- 
  "Sex/Gender information (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["criminal|offense|conviction|court|legal"]] <- 
  "Criminal record information (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["union|membership|association"]] <- 
  "Membership information (sensitive) under APP 3.3 and OVIC IPP 10"

sensitive_fields[["biometric|fingerprint|face|retina|voice|dna"]] <- 
  "Biometric information (sensitive) under APP 3.3 and OVIC IPP 10"

# Identifiers
sensitive_fields[["tfn|tax file number|tax"]] <- 
  "Tax file number regulated under the Privacy (Tax File Number) Rule 2015"

sensitive_fields[["id|identifier|identity|licence|license|passport|medicare"]] <- 
  "Government identifier under APP 9 (Adoption, use or disclosure of government identifiers)"

sensitive_fields[["bank|account|bsb|credit card|debit card|financial|payment"]] <- 
  "Financial information under APP 11 (Security)"

# Victorian-specific (OVIC)
sensitive_fields[["vic|victorian"]] <- 
  "Victorian information potentially covered by OVIC IPPs"

# Function to check if a column name contains sensitive information
is_sensitive <- function(col_name) {
  col_lower <- tolower(col_name)
  
  for (pattern_key in names(sensitive_fields)) {
    patterns <- unlist(strsplit(pattern_key, "\\|"))
    
    for (pattern in patterns) {
      if (grepl(pattern, col_lower)) {
        return(sensitive_fields[[pattern_key]])
      }
    }
  }
  return(NULL)
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Australian Privacy Compliance Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyze Data", tabName = "analyze", icon = icon("upload")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "analyze",
        fluidRow(
          box(
            title = "Upload Data File",
            width = 12,
            fileInput("file", "Choose CSV or Excel File",
                      accept = c(".csv", ".xls", ".xlsx")),
            helpText("Upload a CSV or Excel file to analyze for sensitive data fields")
          )
        ),
        fluidRow(
          box(
            title = "Sensitive Fields Identified",
            status = "warning",
            width = 12,
            uiOutput("sensitivity_results"),
            DT::dataTableOutput("sensitive_fields_table")
          )
        ),
        fluidRow(
          box(
            title = "Data Preview",
            width = 12,
            DT::dataTableOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "Download Options",
            width = 12,
            uiOutput("download_ui")
          )
        )
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This App",
            width = 12,
            includeMarkdown("about.md")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive value to store data
  data_values <- reactiveValues(
    df = NULL,
    sensitive_cols = NULL,
    file_name = NULL,
    file_ext = NULL
  )
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    
    # Get file extension
    file_ext <- tools::file_ext(input$file$name)
    data_values$file_name <- tools::file_path_sans_ext(input$file$name)
    data_values$file_ext <- file_ext
    
    # Read file based on extension
    if (file_ext == "csv") {
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
    } else if (file_ext %in% c("xls", "xlsx")) {
      df <- read_excel(input$file$datapath)
    } else {
      showNotification("Unsupported file format. Please upload CSV or Excel file.", type = "error")
      return(NULL)
    }
    
    # Store data
    data_values$df <- df
    
    # Find sensitive columns
    sensitive_cols <- list()
    for (col_name in names(df)) {
      reason <- is_sensitive(col_name)
      if (!is.null(reason)) {
        sensitive_cols[[col_name]] <- reason
      }
    }
    data_values$sensitive_cols <- sensitive_cols
  })
  
  # Display sensitivity results
  output$sensitivity_results <- renderUI({
    req(data_values$df)
    
    if (length(data_values$sensitive_cols) == 0) {
      div(
        class = "alert alert-success",
        icon("check-circle"), 
        "No sensitive fields detected based on column names."
      )
    } else {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        paste0("Found ", length(data_values$sensitive_cols), " potentially sensitive fields that may be subject to Australian privacy regulations.")
      )
    }
  })
  
  # Display table of sensitive fields
  output$sensitive_fields_table <- DT::renderDataTable({
    req(data_values$df)
    req(length(data_values$sensitive_cols) > 0)
    
    df <- data.frame(
      "Field" = names(data_values$sensitive_cols),
      "Sensitivity Reason" = unlist(data_values$sensitive_cols),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      df,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Display data preview
  output$data_preview <- DT::renderDataTable({
    req(data_values$df)
    
    df <- data_values$df
    
    # Highlight sensitive columns
    if (length(data_values$sensitive_cols) > 0) {
      sensitive_names <- names(data_values$sensitive_cols)
      highlight_cols <- which(names(df) %in% sensitive_names) - 1
      
      DT::datatable(
        df,
        options = list(
          pageLength = 5,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = sensitive_names,
          backgroundColor = "rgba(255, 204, 204, 0.5)",
          fontWeight = "bold"
        )
    } else {
      DT::datatable(
        df,
        options = list(
          pageLength = 5,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    }
  })
  
  # Display download options
  output$download_ui <- renderUI({
    req(data_values$df)
    
    if (length(data_values$sensitive_cols) > 0) {
      tagList(
        checkboxGroupInput(
          "fields_to_redact",
          "Select fields to redact:",
          choices = names(data_values$sensitive_cols),
          selected = names(data_values$sensitive_cols)
        ),
        downloadButton("download_redacted", "Download Redacted File")
      )
    } else {
      tagList(
        p("No sensitive fields detected to redact."),
        downloadButton("download_original", "Download Original File")
      )
    }
  })
  
  # Download handler for redacted file
  output$download_redacted <- downloadHandler(
    filename = function() {
      paste0(data_values$file_name, "_redacted.", data_values$file_ext)
    },
    content = function(file) {
      df_redacted <- data_values$df
      
      # Redact selected columns
      if (length(input$fields_to_redact) > 0) {
        for (field_name in input$fields_to_redact) {
          if (field_name %in% names(df_redacted)) {
            df_redacted[[field_name]] <- rep("[REDACTED]", nrow(df_redacted))
          }
        }
      }
      
      # Write to appropriate format
      if (data_values$file_ext == "csv") {
        write_csv(df_redacted, file)
      } else {
        write_xlsx(df_redacted, file)
      }
    }
  )
  
  # Download handler for original file (when no sensitive fields)
  output$download_original <- downloadHandler(
    filename = function() {
      paste0(data_values$file_name, "_reviewed.", data_values$file_ext)
    },
    content = function(file) {
      if (data_values$file_ext == "csv") {
        write_csv(data_values$df, file)
      } else {
        write_xlsx(data_values$df, file)
      }
    }
  )
}

# Create the about.md file content
about_content <- '
# Australian Privacy Compliance Tool

This Shiny application helps identify and redact sensitive personal information in data files according to:

1. **Australian Privacy Principles (APPs)** from the Privacy Act 1988 (Cth)
2. **Information Privacy Principles (IPPs)** from the Victorian Privacy and Data Protection Act 2014

## Key Privacy Principles

### Australian Privacy Principles (APPs)

- **APP 3: Collection of solicited personal information**
  - Only collect personal information that is reasonably necessary
  - Get consent to collect sensitive information

- **APP 6: Use or disclosure of personal information**
  - Only use or disclose personal information for the primary purpose it was collected
  - Secondary use requires consent or specific exemptions

- **APP 9: Adoption, use or disclosure of government related identifiers**
  - Organizations should not adopt, use or disclose a government related identifier

- **APP 11: Security of personal information**
  - Take reasonable steps to protect personal information from misuse, interference, loss, unauthorized access, modification or disclosure

### Victorian Information Privacy Principles (IPPs)

- **IPP 10: Sensitive Information**
  - An organization must not collect sensitive information about an individual unless specific conditions are met

## What This Tool Does

1. Analyzes uploaded CSV and Excel files
2. Identifies potentially sensitive fields based on column names
3. Provides explanation of which privacy principles apply
4. Enables creation of redacted versions of the data

## Limitations

This tool performs basic analysis based on column names and common patterns. It should be used as an aid only and not as definitive legal advice. For comprehensive privacy compliance, consult with a privacy professional.

## References

- [Office of the Australian Information Commissioner (OAIC)](https://www.oaic.gov.au/)
- [Office of the Victorian Information Commissioner (OVIC)](https://ovic.vic.gov.au/)
'

# Write the about.md file
writeLines(about_content, "about.md")

# Run the application
shinyApp(ui = ui, server = server)