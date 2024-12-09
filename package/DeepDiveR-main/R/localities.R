#' Locality data set
#'
#' 'generate_locality_dataset()' makes a data frame of the summed number of
#' localities per area and time bin (output is a table of regions x bins).
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param bins Time bins used in the analysis (should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training).
#' @returns A table of dimensions regions x bins where the summed number of
#' localities in a time bin are recorded.
#' @examples
#' generate_locality_dataset(dat=your_data, bins=time_bins)
#' @export
generate_locality_dataset <- function(dat, bins){
  dat$Locality <- paste(dat$Area,dat$Locality)
  list_areas <- unique(dat$Area)
  list_areas <- sort(unique(dat$Area))
  localities <- data.frame(matrix(0, length(list_areas), length(bins)-1))
  bins <- sort(-abs(bins))
  for (i in seq_len(length(list_areas))){
    indices_areas <- which(dat$Area == list_areas[i])
    locality_ids <- dat[indices_areas,]$Locality
    uni_loc_ids <- unique(locality_ids)
    no_loc_in_area <- c()
    for(j in uni_loc_ids){
      t <- dat$SampledAge[which(dat$Locality == j)]
      no_loc_in_area <- c(no_loc_in_area, unique(t))
    }
    h <- hist(x = -as.numeric(no_loc_in_area), breaks=bins, plot=F)
    localities[i,] <- h$counts
  }
  colnames(localities) <- sprintf("t%d", seq(length(bins)-1))
  locs <- cbind(Type="locs", Area=list_areas, localities)
  return(data.frame(locs))
}
