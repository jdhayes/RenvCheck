#!/usr/bin/env Rscript

library(methods)
#options(warn=-1)

# Define R environment check S4 class
#   Every slot must be defined here, which turned out to be too restrictive
#setClass ("renvcheck",
#    representation(
#                   storage="character"
#    ),
#    prototype(
#              storage="Failed"
#    )
#)

# Define and create R environment S3 class
#   A better way to handle dynamic calling of functions
myRenvCheck <- structure(list(), class="RenvCheck")

# Define method for calling each check function
#setMethod("show", "RenvCheck", function(object){
show <- function(object){
    maxwidth <- 15
    cat ("\n R Environment Test Results\n")
    for (i in 1:length(object)){
        name <- substr(names(object)[i], 7, nchar(names(object)[i]))
        name <- sub("_"," ",paste0(toupper(substring(name, 1,1)), substring(name, 2)))        
        padding <- rep(" ", maxwidth-nchar(name))
        cat (paste("\t",name)); cat(paste(padding,collapse='')); cat("\t: ");
        cat(paste(as.character(do.call(names(object)[i], list())), "\n"));
    }
    cat ("\n")
}

# Create R environment check object
#  Only needed for S4 class, also remember that all values referencing differs "$" -> "@"
#myRenvCheck <- new("renvcheck")

# Check for storage requirements
check_storage <- function(){
    hquota <- suppressWarnings(as.integer(strsplit(system('check_quota home',TRUE), "\\s+")[[3]][[4]]))
    #bquota <- as.integer(strsplit(system('check_quota bigdata',TRUE), "\\s+")[[3]][[3]])

    if (!is.na(hquota)){
        if (hquota - hquota > 5) {
            #as.integer(bquota[[3]][[4]])-as.ingeger(bquota[[3]][[3]])>5
            return("Passed")
        }
    }
    return("Failed")
}
myRenvCheck$check_storage <- TRUE

# Check for library requirements
check_systempiper <- function(){
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
myRenvCheck$check_systempiper <- TRUE

# Check for individually installed libraries
check_library <- function(){
    RlibsUser <- Sys.getenv("R_LIBS_USER")
    Rlibs <- suppressMessages(suppressWarnings(library(lib.loc=RlibsUser)))
    if (length(Rlibs$results)==0){
        return("Passed")
    } else {
        return("Failed")
    }
}
myRenvCheck$check_library <- TRUE

# Check default R library paths
check_default_library <- function(){
    RlibsUser <- Sys.getenv("R_LIBS_USER")
    #RlibsPath<-.libPaths()
    RlibsPath <- .Library
    if (RlibsUser != RlibsPath){
        return("Passed")
    } else {
        return("Failed")
    }
}
myRenvCheck$check_default_library <- TRUE

# Check R path, "home"
check_r_home <- function(){
    Rhome <- Sys.getenv("R_HOME")
    if (Rhome == "/opt/linux/centos/7.x/x86_64/pkgs/R/3.2.2/lib64/R"){
        return("Passed")
    } else {
        return("Failed")
    }
}
myRenvCheck$check_r_home <- TRUE

# Check if module system is functioning
check_modules <- function() {
    modulelist <- function(){
        strsplit(system("bash -c \"module avail 2>&1\"", TRUE), "\\s+")
    }
    modules <- modulelist()
    find_module <- function(module){
        grep(module, modules, perl=TRUE)
    }
    mlist <- c("torque/5.1.0","bwa/0.7.12","tophat/2.0.14")
    library("BiocParallel")
    found <- bplapply(mlist, find_module)
    if (length(mlist[is.na(mlist[found != 0])])==0){
        return("Passed")
    }else {
        return(paste("Failed::", mlist[is.na(mlist[found != 0])]))
    }
}
myRenvCheck$check_modules <- TRUE

# Check if user has a .html directory
check_html <- function(){
    if (file.exists("~/.html") && file.info("~/.html")$mode > as.octmode("754")){
        return("Passed")
    } else {
        return("Failed")
    }
}
myRenvCheck$check_html <- TRUE

# Check if qsub works
check_qsub <- function(){
    qsub <- strsplit(system("bash -c \"which qsub\"", TRUE),"\\s+")
    if (length(qsub) > 0){
        return("Passed")
    } else {
        return("Failed")
    }
}
myRenvCheck$check_qsub <- TRUE

show(myRenvCheck)

