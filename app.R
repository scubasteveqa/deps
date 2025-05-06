library(shiny)
library(bslib)
library(reticulate)
library(ggplot2)

# Configure Python for both Posit Cloud and Connect environments
tryCatch({
  # Create a more robust Python configuration
  # In Connect, this will use the pre-configured Python environment
  reticulate::py_discover_config()
  
  # Use a specific virtual environment if available (Connect will set this up from requirements.txt)
  # This is a no-op if the environment doesn't exist
  virtualenv_dir <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = NA)
  if (!is.na(virtualenv_dir) && dir.exists(virtualenv_dir)) {
    use_virtualenv(virtualenv_dir)
  }
  
  # Explicitly load numpy and pandas if available
  tryCatch({
    np <- reticulate::import("numpy", convert = FALSE)
    pd <- reticulate::import("pandas", convert = FALSE)
  }, error = function(e) {
    # Packages might not be available, but Python might be
  })
}, error = function(e) {
  # Failed to initialize Python - app will handle this gracefully
})

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
    card_header("Python Environment Status"),
    verbatimTextOutput("pythonInfo"),
    card_header("Python Packages"),
    uiOutput("pyPackageList")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Display Python configuration info
  output$pythonInfo <- renderPrint({
    tryCatch({
      # Get more detailed Python info to help debug Connect issues
      python_info <- list(
        "Python Available" = py_available(),
        "Python Version" = py_version(),
        "Python Path" = py_config()$python,
        "Python Environment" = Sys.getenv("RETICULATE_PYTHON_ENV", "Not set"),
        "Python Modules Path" = py_eval("import sys; sys.path")
      )
      str(python_info)
    }, error = function(e) {
      cat("Python environment unavailable\nError:", conditionMessage(e))
    })
  })
  
  # Generate Python data
  py_data <- reactive({
    if (!py_available()) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
    
    tryCatch({
      # Create Python data frame directly using reticulate
      n <- input$points
      
      # Method 1: Try using imported modules
      tryCatch({
        np <- import("numpy", convert = FALSE)
        pd <- import("pandas", convert = FALSE)
        
        x <- np$random$normal(0, 1, as.integer(n))
        y <- x * 2 + np$random$normal(0, 1, as.integer(n))
        data <- pd$DataFrame(list(x = x, y = y))
        py_to_r(data)
      }, error = function(e) {
        # Method 2: Fall back to py_run_string if direct imports fail
        py_run_string(paste0("
import numpy as np
import pandas as pd

# Create random data
np.random.seed(123)
n = ", n, "
x = np.random.normal(0, 1, n)
y = x * 2 + np.random.normal(0, 1, n)
data = pd.DataFrame({'x': x, 'y': y})
        "))
        py$data
      })
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
          annotate("text", x = 0.5, y = 0.5, 
                  label = "Python data unavailable\nMake sure numpy and pandas are installed", 
                  hjust = 0.5) +
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
  
  # Python Packages list - Connect-friendly implementation
  output$pyPackageList <- renderUI({
    if (!py_available()) {
      return(div(
        p("Python is not available in this environment."),
        p("Check that you've included a requirements.txt file if publishing to Connect.")
      ))
    }
    
    # Get Python package information - more Connect-friendly
    py_pkg_info <- tryCatch({
      # Use a simpler approach that's more likely to work in Connect
      py_run_string("
import sys
import importlib.util

def check_package(package_name):
    spec = importlib.util.find_spec(package_name)
    if spec is None:
        return 'Not installed'
    
    try:
        mod = importlib.import_module(package_name)
        version = getattr(mod, '__version__', 'Unknown version')
        return version
    except:
        return 'Installed (version unknown)'

default_packages = ['numpy', 'pandas', 'matplotlib', 'scipy']
package_info = []

for pkg in default_packages:
    package_info.append((pkg, check_package(pkg)))

if show_all:
    try:
        import pkg_resources
        installed_packages = [(pkg.key, pkg.version) for pkg in pkg_resources.working_set]
        package_info.extend([p for p in installed_packages if p[0] not in default_packages])
    except:
        package_info.append(('Note', 'Unable to retrieve all packages'))
")
      py$package_info
    }, error = function(e) {
      # Most basic fallback
      default_pkgs <- c("numpy", "pandas", "matplotlib", "scipy")
      lapply(default_pkgs, function(pkg) {
        c(pkg, "Status check failed")
      })
    })
    
    # Convert Python packages to HTML elements
    package_elements <- lapply(py_pkg_info, function(pkg) {
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
