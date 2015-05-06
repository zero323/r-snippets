#' Substitutes tags delimited by percent sign
#'
#' @param dirname - directory containing ".tpl" files
#' @param data - data frame with two - columns key and value
#'
template <- function(dirname, data) {
    library(stringi)
    
    stopifnot(is.character(dirname), length(dirname) == 1)
    stopifnot(is.data.frame(data), "key" %in% colnames(data), "value" %in% colnames(data))
    
    templates <- list.files(dirname, full.names = TRUE, pattern = "\\.tpl$")
    get_data <- function(template, data) {
        rbind(
            data,
            data.frame(
                key = c("curtime", "curdate", "filename", "filepath"),
                value = c(
                    format(Sys.time(), "%H:%M:%S"),
                    format(Sys.time(), "%Y-%m-%d"),
                    basename(template),
                    template
                )
            )
        )
    }
    
    process_template <- function(template, data) {
        pattern <- "(?<=%)([a-zA-Z]|\\.[a-zA-Z])[a-zA-Z0-9_\\.]*(?=%)"
        data <- get_data(template, data)
        
        lines <- readLines(template)
        tags <- Filter(function(x) !is.na(x), unlist(stri_extract_all_regex(lines, pattern)))
        for(tag in tags) {
            i <- which(data$key == tag)
            if (length(i) != 0) {
                lines <- stri_replace_all_regex(lines, stri_join("%", tag, "%"), data$value[i])
            } else {
                warning(paste(tag, "not found"))
            }
        }
        lines
    }
    
    lapply(
        templates,
        function(template) writeLines(process_template(template, data), stri_replace_last_regex(template, "tpl$", "txt"))
    )
    
    invisible()
}
