#!/usr/bin/env Rscript

#library(methods)
#options(warn=-1)

library(R6)

# Define R6 class to hold functions to be dynamically called
RenvCheck <- R6Class("RenvCheck",
    public = list(
        initialize = function(){
            maxwidth <- 21
            cat ("\n R Environment Test Results\n")
            # Iterate through function names
            for (i in 1:length(names(self))){
                # Get function name
                fname <- names(self)[[i]]
                
                # Exclude some built in functions
                if ( length(grep('^\\.',fname)) != 0 || length(grep('^clone$',fname)) != 0 || length(grep('^initialize$',fname)) != 0 ) { next }
                
                # Do some pretty formatting
                name <- sub("_"," ",paste0(toupper(substring(fname, 1,1)), substring(fname, 2)))        
                padding <- rep(" ", maxwidth-nchar(name))
                cat (paste("\t",name)); cat(paste(padding,collapse='')); cat("\t: ");
                
                # Convert function name string to function
                f <- eval(call("$",as.symbol('self'),as.symbol(fname)))
                
                # Call function
                cat(paste(as.character(do.call(f, list())), "\n"));
            }
            cat ("\n")
        },
    
        # Check for storage requirements
        check_storage = function(){
            quota_info <- list()
            try(
                quota_info <- suppressWarnings(strsplit(system('mmlsquota --block-size=G home 2>/dev/null',TRUE), "\\s+")),
                silent=TRUE
            )
            
            if (length(quota_info)>1) {
                hquota <- suppressWarnings(as.integer(quota_info[[3]][[4]]))
                husage <- suppressWarnings(as.integer(quota_info[[3]][[3]]))
                #bquota <- as.integer(strsplit(system('check_quota bigdata',TRUE), "\\s+")[[3]][[4]])

                if (!is.na(hquota)){
                    if (hquota - husage > 5) {
                        #as.integer(bquota[[3]][[4]])-as.ingeger(bquota[[3]][[3]])>5
                        return("Passed")
                    }
                }
            }
            return("Failed")
        },

        # Check for library requirements
        check_systempiper = function(){
            Rlibs <- library()

            # Check for systemPipeR library
            if (match("systemPipeR",Rlibs$results)){
                loaded <- suppressMessages(suppressWarnings(library(systemPipeR, quietly=TRUE)))
                if (match("systemPipeR",loaded)){
                    return("Passed")
                }
            }
            return("Failed")
        },

        # Check for individually installed libraries
        check_library = function(){
            RlibsUser <- Sys.getenv("R_LIBS_USER")
            Rlibs <- suppressMessages(suppressWarnings(library(lib.loc=RlibsUser)))
            if (length(Rlibs$results)==0){
                return("Passed")
            } else {
                return("Failed")
            }
        },

        # Check default R library paths
        check_default_library = function(){
            RlibsUser <- Sys.getenv("R_LIBS_USER")
            #RlibsPath<-.libPaths()
            RlibsPath <- .Library
            if (RlibsUser != RlibsPath){
                return("Passed")
            } else {
                return("Failed")
            }
        },

        # Check R path, "home"
        check_r_home = function(){
            Rhome <- Sys.getenv("R_HOME")
            if (Rhome == "/opt/linux/centos/7.x/x86_64/pkgs/R/4.0.1/lib64/R" || Rhome == "/opt/linux/centos/7.x/x86_64/pkgs/R/4.0.3_gcc-8.3.0/lib64/R"){
                return("Passed")
            } else {
                return("Failed")
            }
        },

        # Check if module system is functioning
        check_modules = function() {
            # Get list of available modules
            modules <- list()
            try(
                modules <- suppressWarnings(strsplit(system("bash -c \"module avail 2>&1\"", TRUE), "\\s+")),
                silent=TRUE
            )
            
            find_module <- function(module){
                grep(module, modules, perl=TRUE)
            }
            
            # Check if expected modules are in available list
            mlist <- c("slurm/19.05.0","bwa/0.7.17","hisat2/2.2.1")
            library("BiocParallel")
            found <- bplapply(mlist, find_module)
            
            # Return if found or not
            if (length(mlist[is.na(mlist[found != 0])])==0){
                return("Passed")
            }else {
                cat('\n ')
                return(paste("\t\tFailed::", mlist[is.na(mlist[found != 0])]))
            }
        },

        # Check if user has a .html directory
        check_html = function(){
            if (file.exists("~/.html") && file.info("~/.html")$mode > as.octmode("754")){
                return("Passed")
            } else {
                return("Failed")
            }
        },

        # Check if sbatch works
        check_sbatch = function(){
            sbatch <- NA
            try(
                sbatch <- suppressWarnings(strsplit(system("bash -c 'which sbatch' 2>/dev/null", TRUE),"\\s+")),
                silent=TRUE
            )
            if (length(sbatch)==1 && file.exists(sbatch[[1]])){
                return("Passed")
            } else {
                return("Failed")
            }
        }
    )
)

# Run all checks
.onLoad <- function(libname, pkgname){
    myChecks <- RenvCheck$new()
}
