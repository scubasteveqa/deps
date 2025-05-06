library(shiny)
library(bslib)
library(reticulate)
library(ggplot2)

# Use a Python environment - fixed by using a more flexible approach
# Instead of use_python_version() which requires a version parameter
# Use try-catch to make the app more robust
try({
  # First try to use the default Python
  use_python(Sys.which("python"), required = FALSE)
}, silent = TRUE)

# Define UI for application
ui <- page_sidebar(
  title = "R & Python Integration Demo",
  sidebar = sidebar(
    selectInput("dataset", "Select Dataset:", 
                choices = c("R Iris", "Python Generated Data")),
    sliderInput("points", "Number of Data Points:", 
                min = 50, max = 500, value = 200),
    checkboxInput("showAllPackages", "Show all installed packages", FALSE)
  ),
  
  card(
    card_header("Data Visualization"),
    plotOutput("dataPlot")
  ),
  
  card(
    card_header("R Packages"),
    uiOutput("rPackageList")
  ),
  
  card(
    card_header("Python Packages"),
    uiOutput("pyPackageList")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Generate Python data
  py_data <- reactive({
    tryCatch({
      py_run_string(paste0("
import numpy as np
import pandas as pd

# Create random data
np.random.seed(123)
n = ", input$points, "
x = np.random.normal(0, 1, n)
y = x * 2 + np.random.normal(0, 1, n)
data = pd.DataFrame({'x': x, 'y': y})
"))
      py$data
    }, error = function(e) {
      # Return an empty data frame if there's an error with Python
      data.frame(x = numeric(0), y = numeric(0))
    })
  })
  
  # Generate plot based on selected dataset
  output$dataPlot <- renderPlot({
    if(input$dataset == "R Iris") {
      ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal() +
        labs(title = "Iris Dataset (R)")
    } else {
      data <- py_data()
      if(nrow(data) > 0) {
        ggplot(data, aes(x = x, y = y)) +
          geom_point(color = "darkblue", alpha = 0.7) +
          geom_smooth(method = "lm", color = "red") +
          theme_minimal() +
          labs(title = "Python Generated Data")
      } else {
        # Fallback plot if Python isn't available
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Python data unavailable") +
          theme_minimal() +
          xlim(0, 1) + ylim(0, 1)
      }
    }
  })
  
  # R Packages list
  output$rPackageList <- renderUI({
    if (input$showAllPackages) {
      # Show all installed R packages
      installed_pkgs <- installed.packages()[, c("Package", "Version")]
    } else {
      # Show only specific R packages
      default_packages <- c("shiny", "reticulate", "ggplot2", "bslib")
      all_pkgs <- installed.packages()
      installed_pkgs <- all_pkgs[all_pkgs[, "Package"] %in% default_packages, c("Package", "Version")]
    }
    
    # Convert to list of HTML elements
    package_lines <- apply(installed_pkgs, 1, function(row) {
      p(paste(row["Package"], row["Version"]))
    })
    
    # Return a div containing all package lines
    div(
      package_lines
    )
  })
  
  # Python Packages list
  output$pyPackageList <- renderUI({
    # Get Python package information
    py_pkg_info <- tryCatch({
      py_run_string("
import pkg_resources
import sys

try:
    packages = []
    for package in pkg_resources.working_set:
        packages.append((package.key, package.version))

    # Filter packages if needed
    if not show_all:
        default_packages = ['numpy', 'pandas', 'matplotlib']
        packages = [pkg for pkg in packages if pkg[0] in default_packages]
except:
    packages = [('python-error', 'Package information unavailable')]
", local = list(show_all = input$showAllPackages))
    }, error = function(e) {
      list(packages = list(c("python-error", "Python unavailable")))
    })
    
    # Convert Python packages to HTML elements
    package_elements <- lapply(py_pkg_info$packages, function(pkg) {
      p(paste(pkg[[1]], pkg[[2]]))
    })
    
    # Return a div containing all package lines
    div(
      package_elements
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
