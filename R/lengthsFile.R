#' @title Retrieve correspondance tables lenghts for each level tables between classification from CELLAR and FAO repositories
#' @description The aim of this function is to provide a table showing the different levels of hierarchy for each classification and the length of each level.   
#' @param endpoint SPARQL endpoints provide a standardized way to access data sets, 
#' making it easier to retrieve specific information or perform complex queries on linked data.
#' The valid values are \code{"CELLAR"} or \code{"FAO"}. 
#' @param  prefix Prefixes are typically defined at the beginning of a SPARQL query 
#' and are used throughout the query to make it more concise and easier to read. 
#' Multiple prefixes can be defined in a single query to cover different namespaces used in the dataset.
#' The function 'classEndpoint()' can be used to generate the prefixes for the selected correspondence table.
#' @param conceptScheme Refers to a unique identifier associated to specific classification table. 
#' The conceptScheme can be obtained by utilizing the "classEndpoint()" function.
#' @param correction  The valid values are \code{FALSE} or \code{TRUE}. In both cases the lengths table as an R object.
#' If the output wants to have a correction for hierarchy levels \code{TRUE}. By default is set to "TRUE". 
#' @export
#' @return
#' \code{lenghtsFile()} returns a table containing the lengths for each hierarchical level of the classification.
#'    \itemize{
#'       \item charb: contains the length for each code for each hierarchical level
#'       \item chare: contains the concatenated length of char b for each code for each hierarchical level
#' }
#' @examples 
#' {
#' endpoint = "CELLAR"
#' prefix = "nace2"
#' conceptScheme = "nace2"
#' 
#' lengthsTable = lengthsFile(endpoint, prefix, conceptScheme, correction = TRUE)
#' 
#' #View lengthsTable
#' View(lengthsTable)
#' 
#' } 



lengthsFile = function(endpoint, prefix, conceptScheme, correction = TRUE) {
    
    ## Create 'lengths' table - 
    ## WE NEED TO CLARIFY IF THE LENGTHS FILE IS AUTOMATICALLY OBTAINED OR PROVIDED BY THE USER!
    level_dt = dataStructure(prefix, conceptScheme, endpoint)

    if (isTRUE(correction)) {
        # order (for prodcom) 
        if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022")) {
            level_dt = level_dt[c(2,1,3),]
        }
        # remove first level (for CN)  
        if (prefix %in% c("cn2017", "cn2018", "cn2019", "cn2020", "cn2021", "cn2021", "cn2022", "cn2023")) {
            level_dt = level_dt[-1,]
        }
        # order (for CPA)
        if (prefix %in% c("cpa21")) {
            level_dt = level_dt[c(1,2,3,6,4,5),]
        }
    }
    
    #vectors to store info
    level = length = start_pos = end_pos = numeric(nrow(level_dt))
    level = level_dt[,2]
    
    #level 1 not included (ASK!)
    dt = retrieveClassificationTable(prefix, endpoint, conceptScheme, level[1])$ClassificationTable
    if (isTRUE(correction)) {
        ## remove .0 for 10, 11 and 12 division (ecoicop)
        if (prefix %in% c("ecoicop")) {
            dt[,1][which(dt[,1] %in% c("10.0", "11.0", "12.0"))] = c("10", "11", "12") 
        }
        ## remove weird code 00.99.t and 00.99.t (for prodcom2019)
        if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022")) {
            dt = dt[-which(dt[,1] %in% c("00.99.t", "00.99.z")),]
        }
    }
    
    if (length(unique(nchar(dt[,1]))) > 1) {
        length[1] = 999
        start_pos[1] = NA
        end_pos[1] = NA
    } else {
        length[1] = unique(nchar(dt[,1]))
        start_pos[1] = sum(c(unique(sapply(dt[,1], function(x) regexpr("[^ .]", x)))), na.rm = T)
        if (is.numeric(dt[,1]) == TRUE) {
            rev_string = sapply(dt[,1], function(x) rev(x))
        } else {
            rev_string = sapply(dt[,1], function(x) intToUtf8(rev(utf8ToInt(x))))
        }
        end_pos[1] = sum(c(unique(nchar(dt[,1])), - unique(sapply(rev_string, function(x) regexpr("[^ .]", x))), 1), na.rm = T)
    }
    
    #other levels
    for (l in 2:nrow(level_dt)) {
        dt = retrieveClassificationTable(prefix, endpoint, conceptScheme, level[l])$ClassificationTable
        if (isTRUE(correction)) {
            ## add letter to code (for NACE, NACE 2.1, CPA and ISIC)
            if (prefix %in% c("nace2", "nace21", "cpa21", "ISICrev4")) {
                dt[,1] = paste0("A", dt[,1])
            }
            ## add leading zero for ICC_v11
            if (prefix %in% c("ICC_v11")) {
                if (l == 2){  dt[,1] = sprintf("%.2f", dt[,1])    }
            }
        }
        
        if (length(unique(nchar(dt[,1]))) > 1) {
            length[l] = 999
            start_pos[l] = NA
            end_pos[l] = NA
        } else {
            length[l] = unique(nchar(dt[,1])) 
            code = sapply(dt[,1], function(x) substring(x, first = end_pos[l-1] + 1, last = length[l]))
            start_pos[l] = sum(c(unique(sapply(code, function(x) regexpr("[^ .]", x))), end_pos[l-1]), na.rm = T)
            rev_string = sapply(code, function(x) intToUtf8(rev(utf8ToInt(x))))
            end_pos[l] = sum(c(unique(nchar(code)), - unique(sapply(rev_string, function(x) regexpr("[^ .]", x))), 1, end_pos[l-1]), na.rm = T)
        } 
    }

    #create length table 
    level_table = data.frame(level, length, start_pos, end_pos)
    
    charb = start_pos
    chare = end_pos
    lengths = data.frame(cbind(charb, chare))

    if (TRUE %in% is.na(lengths[,1]) | TRUE %in% is.na(lengths[,2])){
        warning("There is a problem with the given classification and the lenghts file produced should not be trusted. Please check the classification and correct any issue.")
    }
    
    if (isFALSE(correction)){
        warning("The lenghts file produced could be wrong. Please make sure the classification is correct.")
    }       
        
    return(lengths)
}



## Retrieve Classification 
#prefix = "nace2" 
#conceptScheme = "nace2"
#endpoint = "CELLAR"

#lengthsTable = lengthsFile(endpoint, prefix, conceptScheme, correction = TRUE)
#lengthsTable

#lengthsTable = lengthsFile(endpoint, prefix, conceptScheme, correction = FALSE)
#lengthsTable
