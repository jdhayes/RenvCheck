#!/usr/bin/env Rscript

#library(methods)
#options(warn=-1)

# Define method for calling each check function
run_checks <- function(object){
    maxwidth <- 15
    cat ("\n R Environment Test Results\n")
    for (i in 1:length(object)){
        name <- substr(names(object)[i], 7, nchar(names(object)[i]))
        name <- sub("_"," ",paste0(toupper(substring(name, 1,1)), substring(name, 2)))        
        padding <- rep(" ", maxwidth-nchar(name))
        cat (paste("\t",name)); cat(paste(padding,collapse='')); cat("\t: ");
        cat(paste(as.character(do.call((object)[[i]], list())), "\n"));
    }
    cat ("\n")
}

# Define S3 class, A better way to handle dynamic calling of functions
myRenvCheck <- structure(list(), class="RenvCheck")

# Check for storage requirements
myRenvCheck$check_storage <- function(){
    quota_info <- suppressWarnings(strsplit(system('mmlsquota home --block-size=G',TRUE), "\\s+"))
    hquota <- suppressWarnings(as.integer(quota_info[[3]][[4]]))
    husage <- suppressWarnings(as.integer(quota_info[[3]][[3]]))
    #bquota <- as.integer(strsplit(system('check_quota bigdata',TRUE), "\\s+")[[3]][[4]])

    if (!is.na(hquota)){
        if (hquota - husage > 5) {
            #as.integer(bquota[[3]][[4]])-as.ingeger(bquota[[3]][[3]])>5
            return("Passed")
        }
    }
    return("Failed")
}

# Check for library requirements
myRenvCheck$check_systempiper <- function(){
    Rlibs <- library()

    # Check for systemPipeR library
    if (match("systemPipeR",Rlibs$results)){
        loaded <- suppressMessages(suppressWarnings(library(systemPipeR, quietly=TRUE)))
        if (match("systemPipeR",loaded)){
            return("Passed")
        }
    }
    return("Failed")
}

# Check for individually installed libraries
myRenvCheck$check_library <- function(){
    RlibsUser <- Sys.getenv("R_LIBS_USER")
    Rlibs <- suppressMessages(suppressWarnings(library(lib.loc=RlibsUser)))
    if (length(Rlibs$results)==0){
        return("Passed")
    } else {
        return("Failed")
    }
}

# Check default R library paths
myRenvCheck$check_default_library <- function(){
    RlibsUser <- Sys.getenv("R_LIBS_USER")
    #RlibsPath<-.libPaths()
    RlibsPath <- .Library
    if (RlibsUser != RlibsPath){
        return("Passed")
    } else {
        return("Failed")
    }
}

# Check R path, "home"
myRenvCheck$check_r_home <- function(){
    Rhome <- Sys.getenv("R_HOME")
    if (Rhome == "/opt/linux/centos/7.x/x86_64/pkgs/R/3.4.0/lib64/R"){
        return("Passed")
    } else {
        return("Failed")
    }
}

# Check if module system is functioning
myRenvCheck$check_modules <- function() {
    modulelist <- function(){
        strsplit(system("bash -c \"module avail 2>&1\"", TRUE), "\\s+")
    }
    modules <- modulelist()
    find_module <- function(module){
        grep(module, modules, perl=TRUE)
    }
    mlist <- c("slurm/16.05.4","bwa/0.7.12","tophat/2.1.1")
    library("BiocParallel")
    found <- bplapply(mlist, find_module)
    if (length(mlist[is.na(mlist[found != 0])])==0){
        return("Passed")
    }else {
        return(paste("Failed::", mlist[is.na(mlist[found != 0])]))
    }
}

# Check if user has a .html directory
myRenvCheck$check_html <- function(){
    if (file.exists("~/.html") && file.info("~/.html")$mode > as.octmode("754")){
        return("Passed")
    } else {
        return("Failed")
    }
}

# Check if sbatch works
myRenvCheck$check_sbatch <- function(){
    sbatch <- strsplit(system("bash -c \"which sbatch\"", TRUE),"\\s+")
    if (length(sbatch) > 0){
        return("Passed")
    } else {
        return("Failed")
    }
}

# Run all checks
.onLoad <- function(libname, pkgname){
    run_checks(myRenvCheck)
}

