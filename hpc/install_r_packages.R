# install_r_packages.R
cat("Using library path:\n")
print(.libPaths())

# Clean up any existing lock files from previous failed installations
cat("Checking for lock files...\n")
lock_dirs <- list.files(.libPaths()[1], pattern = "^00LOCK", full.names = TRUE)
if (length(lock_dirs) > 0) {
  cat("Found lock directories - removing them:\n")
  for (lock_dir in lock_dirs) {
    cat(sprintf("  Removing: %s\n", basename(lock_dir)))
    unlink(lock_dir, recursive = TRUE)
  }
  cat("Lock files cleaned up.\n")
} else {
  cat("No lock directories found.\n")
}

# Set CRAN mirror and configure installation
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(install.packages.check.source = "no")  # Use binaries when available
options(install.packages.compile.from.source = "never")  # Avoid compilation issues

# Required packages and their dependencies in installation order
# required <- c("readxl", "apollo") # First time
required <- c("readxl", "dplyr", "tidyr", "tidyverse", "parallel")
update <- c("apollo")
apollo_deps <- c("Rsolnp", "RcppEigen")  # Known problematic dependencies

# Check what's already installed
installed_pkgs <- rownames(installed.packages(lib.loc = .libPaths()[1]))
to_install <- c(required[!required %in% installed_pkgs], update)

if (length(to_install) > 0) {
  cat("Installing packages:\n")
  print(to_install)
  
  # If apollo needs to be installed, try its dependencies first
  if ("apollo" %in% to_install) {
    cat("\n=== Installing apollo dependencies first ===\n")
    for (dep in apollo_deps) {
      if (!dep %in% installed_pkgs) {
        cat(sprintf("\n--- Installing dependency: %s ---\n", dep))
        result <- tryCatch({
          install.packages(dep, 
                          lib = .libPaths()[1], 
                          dependencies = TRUE,
                          verbose = TRUE,
                          force = TRUE,
                          repos = "https://cloud.r-project.org")
          
          if (dep %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
            cat(sprintf("SUCCESS: %s installed\n", dep))
            return("SUCCESS")
          } else {
            cat(sprintf("FAILED: %s not found after installation\n", dep))
            return("FAILED")
          }
        }, error = function(e) {
          cat(sprintf("ERROR installing %s:\n", dep))
          print(e)
          return("ERROR")
        })
        cat(sprintf("Dependency %s result: %s\n", dep, result))
      } else {
        cat(sprintf("Dependency %s already installed\n", dep))
      }
    }
  }
  
  # Now install the main packages
  cat("\n=== Installing main packages ===\n")
  for (pkg in to_install) {
    cat(sprintf("\n=== Installing %s ===\n", pkg))
    
    # Capture both success/failure and any output
    result <- tryCatch({
      # Try installation with verbose output
      install.packages(pkg, 
                      lib = .libPaths()[1], 
                      dependencies = TRUE,
                      verbose = TRUE,
                      quiet = FALSE)
      
      # Check if it actually installed
      if (pkg %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
        cat(sprintf("SUCCESS: Successfully installed %s\n", pkg))
        return("SUCCESS")
      } else {
        cat(sprintf("FAILED: Installation completed but %s not found in library\n", pkg))
        return("FAILED_NOT_FOUND")
      }
      
    }, error = function(e) {
      cat(sprintf("ERROR: Failed to install %s\n", pkg))
      cat("Error details:\n")
      print(e)
      return("ERROR")
    }, warning = function(w) {
      cat(sprintf("WARNING: Warning during %s installation:\n", pkg))
      print(w)
      return("WARNING")
    })
    
    cat(sprintf("Result for %s: %s\n", pkg, result))
  }
} else {
  cat("All required packages already installed.\n")
}

# Final verification with detailed output
cat("\n=== FINAL VERIFICATION ===\n")
for (pkg in required) {
  cat(sprintf("Checking %s: ", pkg))
  if (pkg %in% rownames(installed.packages(lib.loc = .libPaths()[1]))) {
    cat("INSTALLED\n")
    
    # Try to actually load it
    cat(sprintf("  Testing load: "))
    load_result <- tryCatch({
      require(pkg, character.only = TRUE, quietly = TRUE, lib.loc = .libPaths()[1])
      cat("LOADS SUCCESSFULLY\n")
    }, error = function(e) {
      cat("FAILS TO LOAD\n")
      cat("  Load error: ")
      print(e)
    })
    
  } else {
    cat("NOT INSTALLED\n")
  }
}