#' Function that searches breast cancer datasets based on ontology terms mapped to data fields.
#' 
#' Accepts an ontology term code and searches for datasets based on fields
#'  (metadata columns) that have been mapped to that ontology term.
#'  Returns a tibble with the dataset identifier, field name, and  
#'  code. Users can then pass the dataset identifiers to the getDataset
#'  function. 
#' 
#' @param code is an ontology term code retrieved using the searchOntologyTerms function.
#' @return a tibble providing information about any identified datasets.
#' @examples searchForDatasetsByField("C16149")
#' @export
searchForDatasetsByField <- function(code) {
    filtered_path <- file.path(tempdir(), 'filtered_mapped_ontology.tsv.gz')
    
    if (!file.exists(filtered_path)) {
        downloadZenFile('10.5281/zenodo.17583904', tempdir())
    }
    
    filtered_data <- read_tsv(filtered_path)
    df <- searchFields(code, filtered_data)
    return(df)
    
}