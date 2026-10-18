#' @importFrom stringr regex str_detect
#' @importFrom dplyr rename filter select pull %>% bind_rows mutate
#' @importFrom tidyr separate_rows
#' @importFrom readxl read_excel

utils::globalVariables(c(
    "Code",
    "Preferred_Label",
    "Synonyms",
    "URI",
    "Definitions",
    "label",
    "Name"
))

NCIT_identifiers <- c("10.5281/zenodo.17545644", 
                      "Metadata_Mappings_Final_10_18.xlsx")

#' Function that searches for ontology terms.
#' 
#' Accepts a string to search by and which ontology field to search 
#'   ontology terms associated with that string. This could
#'   be the name of the term itself, the term's definition,
#'   the unique identifier (code) associated with that term,
#'   or its URL. The function returns a tibble with information
#'   about the matching ontology term(s).
#' 
#' @param term String to search by.
#' @param term_type Ontology field to search: Name, URI, Code, or
#'   Definition. When Name (default) is provided, it also searches
#'   based on synonyms.
#' @return A tibble with a row for each matching ontology term.
#' @examples searchOntologyTerms("Progesterone Receptor Status")
#' @export 
searchOntologyTerms <- function(term, term_type="Name") {
    acceptable_terms <- c("Name", "Definition", "URI", "Code")
    
    if (!(term_type %in% acceptable_terms)) {
        stop(term_type, 
                    " is not an acceptable search term.",
                    " Try: ", paste(acceptable_terms, collapse=", "))
    }
    
    def_path <- file.path(tempdir(), NCIT_identifiers[2])
    if (!file.exists(def_path)) {
        downloadZenFile(NCIT_identifiers[1], tempdir())
    }
    NCIT_defs <- read_excel(def_path, sheet='Ontology_Terms')
    
    if (term_type == "Name") {
        df <- searchNames(term, NCIT_defs)
    } else if (term_type == "URI") {
        df <- searchURIs(term, NCIT_defs) 
    } else if (term_type == "Code") {
        df <- searchCodes(term, NCIT_defs)
    } else {
        df <- searchDefinition(term, NCIT_defs)
    }
    
    return(df)
}


## helper function 1: searchNames, searches through Names in definitions df

searchNames <- function(term, df) {
    ncit_names <- df %>%
        select(Name = Preferred_Label, Code)
    
    ncit_synonyms <- df %>%
        select(Name = Synonyms, Code) %>%
        separate_rows(Name, sep = "\\|")
    
    codes <- bind_rows(ncit_names, ncit_synonyms) %>%
        filter(str_detect(Name, regex(term, ignore_case = TRUE))) %>%
        pull(Code) %>%
        unique()
    
    filter(df, Code %in% codes)
}


## helper function 2: searchURI, searches through URIs in definitions df

searchURIs <- function(term, df) {
    df_searched <- df %>%
        mutate(URI = paste0("http://purl.obolibrary.org/obo/NCIT_", Code)) %>%
        filter(URI==term)
    return(df_searched)
}


## helper function 3: searchCodes, searches through codes in definitions df

searchCodes <- function(term, df) {
    df_searched <- filter(df, Code==term)
    return(df_searched)
}


## helper function 4: searchDefinitions, searches through defs in definitions df

searchDefinition <- function(term, df) {
    df_searched <- filter(df, 
                          str_detect(Definitions, 
                                     regex(term, ignore_case = TRUE)))
    return(df_searched)
}