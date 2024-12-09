#' Occurrences per continent and time bin
#'
#' 'generate_occurrence_dataset()' sums the number of occurrences per region to
#' make a table of regions x bins.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param area_tables Tibble of occurrence table split by area
#' @param bins Time bins used in the analysis (should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training)
#' @returns A table of dimensions regions x bins where the number of occurrences
#' are summed per region per time bin and recorded.
#' @examples
#' generate_occurrence_dataset(dat=your_data, 
#' area_tables=your_data_split_by_area, bins=time_bins)
#' @export
generate_occurrence_dataset <- function(dat, area_tables, bins){
  list_areas <-  sort(unique(dat$Area))
  n_occurrences <- data.frame(matrix(0, length(list_areas), length(bins)-1))
  for (i in seq_len(length(list_areas))){
    indices_areas <- which(dat$Area == list_areas[i])
    total_occs_for_area <- length(indices_areas)
    area_dat <- dat[indices_areas,]
    age_occs <- area_dat$SampledAge
    h <- hist(x = -as.numeric(age_occs), breaks=bins, plot=F)
    n_occurrences[i,] <- h$counts
  }
  return(n_ocurrences)
}
