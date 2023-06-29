#' @title Retrieve classification table from CELLAR and FAO repositories. 
#' @description The aim of this function is to provide a table showing the different codes and labels for each classification 
#' @param classification it returns a dataframe with two columns corrected according to the classification of CELLAR & FAO.
#' @export
#' @return
#'  \code{correctionClassification()} returns a table with information needed to retrieve the classification table:
#'  \itemize{
#'    \item Classification Code name (e.g. nace2): the code of each object
#'    \item Classification Label:  corresponding name of each object
#'  }
#' @examples
#' {
#' #prefix = "nace2"
#' #conceptScheme = "nace2"
#' #endpoint = "CELLAR"
#' #classification = classification[,c(1,2)]
#' #colnames(classification)[1:2] = c("Code", "Label")
#' #classification = retrieveClassificationTable(prefix, endpoint, conceptScheme, level="ALL")$ClassificationTable
#' #correct_classification = correctionClassification(classification)
#' #View(correct_classification)
#' }


correctionClassification = function(classification){
    
    if(ncol(classification) != 2){
        stop("The classification must have only two colums corresponding to code and label.")
    }
    
    colnames(classification)[1:2] = c("Code", "Label")
    
    #add letter to code (for NACE - NACE 2.1 - CPA21 - and ISIC)
    if (prefix %in% c("nace2", "nace21", "cpa21", "ISICrev4")) {
        A_code = which(substr(classification$Code, 1, 2) %in% c("01", "02", "03"))
        classification$Code[A_code] = paste0("A", classification$Code[A_code])
        B_code = which(substr(classification$Code, 1, 2) %in% c("05", "06", "07", "08", "09"))
        classification$Code[B_code] = paste0("B", classification$Code[B_code])
        C_code = which(substr(classification$Code, 1, 2) %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
                                                                "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33"))
        classification$Code[C_code] = paste0("C", classification$Code[C_code])
        D_code = which(substr(classification$Code, 1, 2) %in% c("35"))
        classification$Code[D_code] = paste0("D", classification$Code[D_code])
        E_code = which(substr(classification$Code, 1, 2) %in% c("36", "37", "38", "39"))
        classification$Code[E_code] = paste0("E", classification$Code[E_code])
        F_code = which(substr(classification$Code, 1, 2) %in% c("41", "42", "43"))
        classification$Code[F_code] = paste0("F", classification$Code[F_code])
        G_code = which(substr(classification$Code, 1, 2) %in% c("45", "46", "47"))
        classification$Code[G_code] = paste0("G", classification$Code[G_code])
        H_code = which(substr(classification$Code, 1, 2) %in% c("49", "50", "51", "52", "53"))
        classification$Code[H_code] = paste0("H", classification$Code[H_code])
        I_code = which(substr(classification$Code, 1, 2) %in% c("55", "56"))
        classification$Code[I_code] = paste0("I", classification$Code[I_code])
        J_code = which(substr(classification$Code, 1, 2) %in% c("58", "59", "60", "61", "62", "63"))
        classification$Code[J_code] = paste0("J", classification$Code[J_code])
        K_code = which(substr(classification$Code, 1, 2) %in% c("64", "65", "66"))
        classification$Code[K_code] = paste0("K", classification$Code[K_code])
        L_code = which(substr(classification$Code, 1, 2) %in% c("68"))
        classification$Code[L_code] = paste0("L", classification$Code[L_code])
        M_code = which(substr(classification$Code, 1, 2) %in% c("69", "70", "71", "72", "73", "74", "75"))
        classification$Code[M_code] = paste0("M", classification$Code[M_code])
        N_code = which(substr(classification$Code, 1, 2) %in% c("77", "78", "79", "80", "81", "82"))
        classification$Code[N_code] = paste0("N", classification$Code[N_code])
        O_code = which(substr(classification$Code, 1, 2) %in% c("84"))
        classification$Code[O_code] = paste0("O", classification$Code[O_code])
        P_code = which(substr(classification$Code, 1, 2) %in% c("85"))
        classification$Code[P_code] = paste0("P", classification$Code[P_code])
        Q_code = which(substr(classification$Code, 1, 2) %in% c("86", "87", "88"))
        classification$Code[Q_code] = paste0("Q", classification$Code[Q_code])
        R_code = which(substr(classification$Code, 1, 2) %in% c("90", "91", "92", "93"))
        classification$Code[R_code] = paste0("R", classification$Code[R_code])
        S_code = which(substr(classification$Code, 1, 2) %in% c("94", "95", "96"))
        classification$Code[S_code] = paste0("S", classification$Code[S_code])
        T_code = which(substr(classification$Code, 1, 2) %in% c("97", "98"))
        classification$Code[T_code] = paste0("T", classification$Code[T_code])
        U_code = which(substr(classification$Code, 1, 2) %in% c("99"))
        classification$Code[U_code] = paste0("U", classification$Code[U_code])
    }
    
    #remove .0 for 10, 11 and 12 division (for ecoicop) --- THIS WAS CHANGED (but not for hicp)
    if (prefix %in% c("ecoicop")) {
        level1_code = which(classification$Code %in% c("10.0", "11.0", "12.0"))
        classification$Code[level1_code] = c("10", "11", "12") 
    }
    
    #remove weird code 00.99.t and 00.99.t (for prodcom2019)
    if (prefix %in% c("prodcom2019")) {
        level1_code = which(classification$Code %in% c("00.99.t", "00.99.z"))
        classification = classification[-level1_code,]
    }
    
    #remove section (for CN) - does not need correction NO SECTION
    if (prefix %in% c("cn2017", "cn2018", "cn2019", "cn2020", "cn2021", "cn2021", "cn2022", "cn2023")) {
        level1_code = which(gsub("[^a-zA-Z]", "", classification$Code)!= "")
        classification = classification[-level1_code,]
    }
    
    #remove . in the end of the code (for CBF)
    if (prefix %in% c("cbf10")) {
        classification[,1] = substr(classification[,1], 1, nchar(classification[,1])-1)
    }
    
    return(classification)
}
