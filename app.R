library(shiny)
library(reticulate)
library(DT)

# Initialize Python first
tryCatch({
  reticulate::py_discover_config()
}, error = function(e) {
  # Failed to initialize Python - app will handle this gracefully
})

# Function to get R packages and versions
get_r_packages <- function() {
  pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
  pkgs <- pkgs[order(pkgs$Package), ]
  return(pkgs)
}

# Function to get Python packages and versions - with error handling
get_python_packages <- function() {
  if (!py_available()) {
    return(data.frame(Package = "Python not available", Version = "Error"))
  }
  
  tryCatch({
    py_run_string("
try:
    import pkg_resources
    packages = sorted([(p.key, p.version) for p in pkg_resources.working_set])
except:
    # Fallback if pkg_resources doesn't work
    import sys
    import importlib.util
    
    def get_version(module_name):
        try:
            module = __import__(module_name)
            return getattr(module, '__version__', 'Unknown')
        except:
            return 'Unknown'
    
    common_packages = ['numpy', 'pandas', 'matplotlib', 'scipy']
    packages = [(pkg, get_version(pkg)) for pkg in common_packages]
    ")
    py_packages <- py$packages
    return(data.frame(
      Package = sapply(py_packages, function(x) x[[1]]),
      Version = sapply(py_packages, function(x) x[[2]])
    ))
  }, error = function(e) {
    return(data.frame(
      Package = c("Error retrieving packages"),
      Version = c(as.character(e))
    ))
  })
}

# Get Python version with error handling
get_python_version <- function() {
  tryCatch({
    return(py_config()$version)
  }, error = function(e) {
    return("Python not available")
  })
}

ui <- fluidPage(
  titlePanel("Installed Packages for R and Python"),
  
  tabsetPanel(
    tabPanel("R Packages", 
      h3(paste0("R version: ", R.version$version.string)),
      DTOutput("r_packages")
    ),
    tabPanel("Python Packages",
      h3(textOutput("python_version_text")),
      DTOutput("python_packages")
    ),
    tabPanel("System Info",
      h3("System Information"),
      verbatimTextOutput("system_info")
    )
  )
)

server <- function(input, output) {
  # Python version with error handling
  output$python_version_text <- renderText({
    paste0("Python version: ", get_python_version())
  })
  
  output$r_packages <- renderDT({
    get_r_packages()
  })
  
  output$python_packages <- renderDT({
    get_python_packages()
  })
  
  output$system_info <- renderPrint({
    r_info <- R.version
    
    py_info <- tryCatch({
      py_config()
    }, error = function(e) {
      list(error = "Python not available", message = as.character(e))
    })
    
    list(
      r_version = r_info,
      python_config = py_info
    )
  })
}

shinyApp(ui = ui, server = server)
