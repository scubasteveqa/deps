library(shiny)
library(reticulate)

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
  
  fluidRow(
    column(6,
      wellPanel(
        h3(paste0("R version: ", R.version$version.string)),
        uiOutput("r_packages_list")
      )
    ),
    column(6,
      wellPanel(
        h3(textOutput("python_version_text")),
        uiOutput("python_packages_list")
      )
    )
  )
)

server <- function(input, output) {
  # Python version with error handling
  output$python_version_text <- renderText({
    paste0("Python version: ", get_python_version())
  })
  
  # Display R packages as a list
  output$r_packages_list <- renderUI({
    pkgs <- get_r_packages()
    
    # Limit to 100 packages to avoid performance issues
    if(nrow(pkgs) > 100) {
      pkgs <- pkgs[1:100,]
    }
    
    pkgs_html <- lapply(1:nrow(pkgs), function(i) {
      p(HTML(paste0("<b>", pkgs$Package[i], "</b>: ", pkgs$Version[i])))
    })
    
    tagList(
      pkgs_html,
      if(nrow(get_r_packages()) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
  
  # Display Python packages as a list
  output$python_packages_list <- renderUI({
    pkgs <- get_python_packages()
    
    # Limit to 100 packages to avoid performance issues
    if(nrow(pkgs) > 100) {
      pkgs <- pkgs[1:100,]
    }
    
    pkgs_html <- lapply(1:nrow(pkgs), function(i) {
      p(HTML(paste0("<b>", pkgs$Package[i], "</b>: ", pkgs$Version[i])))
    })
    
    tagList(
      pkgs_html,
      if(nrow(get_python_packages()) > 100) {
        p("(Showing first 100 packages)")
      }
    )
  })
}

shinyApp(ui = ui, server = server)
