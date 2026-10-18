#' Function that searches breast cancer datasets based on ontology terms mapped to data value.
#' 
#' Accepts an ontology term code and searches for datasets based on
#'  metadata values that have been mapped to that ontology term.
#'  Returns a tibble with the dataset identifier, field name, and  
#'  code. Users can then pass the dataset identifiers to the getDataset
#'  function. 
#' 
#' @param code is an ontology term code retrieved using the searchOntologyTerms function.
#' @return a tibble providing information about any identified datasets.
#' @examples searchForDatasetsByValue("C15496")
#' @export
searchForDatasetsByValue <- function(code) {
    filtered_path <- file.path(tempdir(), 'filtered_mapped_ontology.tsv.gz')
    
    if (!file.exists(filtered_path)) {
        downloadZenFile('10.5281/zenodo.17583904', tempdir())
    }
    
    filtered_data <- read_tsv(filtered_path)
    print(filtered_data)
    df <- searchValues(code, filtered_data)
    return(df)
}