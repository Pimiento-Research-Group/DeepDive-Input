#' Prepare DeepDive Input
#'
#' 'prep_dd_input()' makes a single input file for use in DeepDive in one line.
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param r The number of age assignment replicates
#' @param age_m The age assignment method
#' @returns A .csv file containing: start age, midpoint and duration of time
#' bins, counts of the number of localities per region for each bin, counts of
#' occurrences of each taxon per region per time bin, the data type (bins, locs,
#' occs) and replicate number (where age replicates are used, column R).
#' @examples
#' prep_dd_input(dat=your_data, bins=bins, r=100, age_m = "random_by_loc")
#' @export
prep_dd_input <- function(dat=dat, bins, r=replicate, age_m=age_method, 
                          output_file=NULL){

  deepdive_input <- data.frame()
  for(rep in 1:r){
    
    sampled_dat <- ages(dat, method=age_m) # Get ages and append
    area_tables <- split(sampled_dat, f = sampled_dat$Area)  # Split data by area

    # Get species or genera level data
    occs <- taxa_time_per_area(sampled_dat, area_tables, bins=bins)
    cnames <- c(colnames(occs))
    
    # Get locality data
    locs <- generate_locality_dataset(sampled_dat, bins=bins)

    # Get time bin data
    tbins <- data.frame(cbind(c("bin_start", "bin_mid", "bin_dur"), NA,
                              rbind(bins[-length(bins)], 
                                    rowMeans(cbind(bins[-length(bins)], bins[-1])), 
                                    bins[-length(bins)]-bins[-1])))
    names(tbins) <- cnames

    dd_input <- rbind(tbins, locs, occs)
    dd_input <- cbind(Replicate=rep, dd_input)
    deepdive_input <- rbind(deepdive_input, dd_input)
  }

  # Get DeepDive input file
  if(!is.null(output_file)){
    write.csv(deepdive_input, output_file,
              row.names=FALSE)
  }
  else{
    return(deepdive_input)
  }
}
