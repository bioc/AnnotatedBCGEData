#' @importFrom dplyr %>% rename select filter

utils::globalVariables(c(
    "accession_id",
    "publishing_platform",
    "experiment_type",
    "summary",
    "title",
    "overall_design")
)

#' A function that accepts a vector of dataset IDs and returns a tibble with 
#'  metadata about the overall study.
#'  
#' @param datasets A vector of strings consisting of dataset identifier(s).
#' @return Tibble with metadata.
#' @examples getDatasetMetadata(c("GSE41197", "GSE10797"))
#' @export 
getDatasetMetadata <- function(datasets) {
    dataset_meta_path <- file.path(tempdir(), 'dataset_meta.tsv')
    if (!file.exists(dataset_meta_path)) {
        downloadZenFile('10.5281/zenodo.17780657', tempdir())
    }
    for (i in seq_along(datasets)) {
        data <- datasets[i]
        if (data == "ICGC_KR") {
            next
        }
        if (data == "E_TABM_158") {
            datasets <- c(datasets, "E-TABM-158")
            next
        }
        result <- ifelse(grepl("_", data), sub("_.*", "", data), data)
        datasets <- c(datasets, result)
    }
    dataset_meta <- read_tsv(dataset_meta_path) %>%
        select(accession_id, 
               publishing_platform, 
               experiment_type, 
               title, 
               summary, 
               overall_design) %>%
        rename(Dataset_ID=accession_id) %>%
        rename(Source=publishing_platform) %>%
        rename(Experiment_Type=experiment_type) %>%
        rename(Title=title) %>%
        rename(Summary=summary) %>%
        rename(Overall_Design=overall_design) %>%
        filter(Dataset_ID %in% datasets)
    
    return(dataset_meta)
}