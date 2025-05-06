library(shiny)
library(bslib)
library(reticulate)

# Configure Python environment, preferring Python 3.12
tryCatch({
  # Try to use Python 3.12 specifically
  py_versions <- py_discover_config()
  python_path <- NULL
  
  # Check if Python 3.12 is available
  for (ver_path in py_versions$python_versions) {
    tryCatch({
      version_info <- system2(ver_path, "--version", stdout = TRUE)
      if (grepl("3\\.12", version_info)) {
        python_path <- ver_path
        break
      }
    }, error = function(e) {
      # Continue to the next path
    })
  }
  
  # If Python 3.12 found, use it
  if (!is.null(python_path)) {
    use_python(python_path, required = TRUE)
  } else {
    # Otherwise, use whatever Python is available
    reticulate::py_discover_config()
  }
}, error = function(e) {
  # Failed to initialize Python - app will handle this gracefully
})

# Define UI for application
ui <- page_fluid(
  title = "R & Python Environment Explorer",
  
  # Main content cards
  card(
    card_header("Python Version (Target: Python 3.12)"),
    verbatimTextOutput("python_version")
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
  # Display Python version information
  output$python_version <- renderPrint({
    tryCatch({
      python_version <- as.character(py_config()$version)
      python_path <- as.character(py_config()$python)
      cat("Python version:", python_version, "\n")
      cat("Python path:", python_path, "\n")
      cat("Python available:", py_available(), "\n")
      
      # Check if we have Python 3.12
      is_3_12 <- grepl("^3\\.12", python_version)
      if (is_3_12) {
        cat("✓ Using Python 3.12 as requested\n")
      } else {
        cat("✗ Python 3.12 not found. Using", python_version, "instead\n")
      }
    }, error = function(e) {
      cat("Python is not available in this environment.\n")
      cat("Error:", conditionMessage(e), "\n")
    })
  })
  
  # R Packages list - shows key packages
  output$rPackageList <- renderUI({
    # Show key R packages
    default_packages <- c("shiny", "reticulate", "bslib")
    all_pkgs <- installed.packages()
    installed_pkgs <- all_pkgs[all_pkgs[, "Package"] %in% default_packages, c("Package", "Version")]
    
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
    if (!py_available()) {
      return(div(
        p("Python is not available in this environment."),
        p("Check that you've included a requirements.txt file if publishing to Connect.")
      ))
    }
    
    # Get Python package information
    py_pkg_info <- tryCatch({
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

default_packages = ['numpy', 'pandas', 'matplotlib']
package_info = []

for pkg in default_packages:
    package_info.append((pkg, check_package(pkg)))
")
      py$package_info
    }, error = function(e) {
      # Most basic fallback
      default_pkgs <- c("numpy", "pandas", "matplotlib")
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
