pkgname <- "renvvv"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('renvvv')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("renvvv_dep_add")
### * renvvv_dep_add

flush(stderr()); flush(stdout())

### Name: renvvv_dep_add
### Title: Add Packages to _dependencies.R for renv
### Aliases: renvvv_dep_add

### ** Examples

## Not run: 
##D # Add and install CRAN packages
##D renvvv_dep_add(c("dplyr", "ggplot2"))
##D 
##D # Add and install a GitHub package
##D renvvv_dep_add("hadley/httr")
## End(Not run)




cleanEx()
nameEx("renvvv_restore")
### * renvvv_restore

flush(stderr()); flush(stdout())

### Name: renvvv_restore
### Title: Restore or Update renv Lockfile Packages
### Aliases: renvvv_restore renvvv_update renvvv_restore_and_update

### ** Examples

## Not run: 
##D # Restore all packages
##D renvvv_restore()
##D 
##D # Update all packages
##D renvvv_update()
##D 
##D # Restore and then update all packages
##D renvvv_restore_and_update()
##D 
##D # Only restore non-GitHub packages
##D renvvv_restore(github = FALSE)
##D 
##D # Only update GitHub packages
##D renvvv_update(non_github = FALSE)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
