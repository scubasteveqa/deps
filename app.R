# app.R
library(shiny)
library(reticulate)
library(DT)

# Configure Python path (adjust as needed)
#use_python("/usr/local/bin/python3.12")

# Function to get R packages and versions
get_r_packages <- function() {
  pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
  pkgs <- pkgs[order(pkgs$Package), ]
  return(pkgs)
}

# Function to get Python packages and versions
get_python_packages <- function() {
  py_run_string("
import pkg_resources
packages = sorted([(p.key, p.version) for p in pkg_resources.working_set])
  ")
  py_packages <- py$packages
  return(data.frame(
    Package = sapply(py_packages, function(x) x[[1]]),
    Version = sapply(py_packages, function(x) x[[2]])
  ))
}

ui <- fluidPage(
  titlePanel("Installed Packages for R and Python"),
  
  tabsetPanel(
    tabPanel("R Packages", 
      h3(paste0("R version: ", R.version$version.string)),
      DTOutput("r_packages")
    ),
    tabPanel("Python Packages",
      h3(paste0("Python version: ", py_config()$version)),
      DTOutput("python_packages")
    ),
    tabPanel("System Info",
      h3("System Information"),
      verbatimTextOutput("system_info")
    )
  )
)

server <- function(input, output) {
  output$r_packages <- renderDT({
    get_r_packages()
  })
  
  output$python_packages <- renderDT({
    get_python_packages()
  })
  
  output$system_info <- renderPrint({
    list(
      r_version = R.version,
      python_config = py_config()
    )
  })
}

shinyApp(ui = ui, server = server)
