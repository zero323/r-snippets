library(jsonlite)

#' Download Drug and Medication Information for a given compound from PubChem
#'
#' @param cid
#' @return list of data.frames 
#' @export
pubchem_drug_and_medication_information <- function(cid) {
    stopifnot(is.numeric(cid))
    base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/%i/JSON"
    
    # Fetch data
    r <- fromJSON(sprintf(base_url, cid))
    
    # Extract record
    record <- r$Record
    stopifnot(!is.null(record))
    
    # Get index of "Drug and Medication Information" section
    dmi_index <- which(record$Section$TOCHeading == "Drug and Medication Information")
    dmi <- record$Section[dmi_index, "Section"][[1]]
    
    # Extract "Drug and Medication Information" 
    setNames(apply(dmi, 1, function(x) do.call(cbind, x[3])), dmi[, 1])
}
