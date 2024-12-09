#' Taxa per time bin per area (occurrence data)
#'
#' 'taxa_time_per_area()' makes a table of shape taxon x region x time bin where
#' occurrences are recorded.
#' @param dat An occurrence data table including taxon id, Area, MinAge, MaxAge
#' and Locality columns.
#' @param area_tables Tibble of occurrence table split by area
#' @param bins Time bins used in the analysis (should reflect the empirical data
#' and be identical to the bins used in any corresponding DeepDive simulations
#' and training)
#' @returns A table of shape taxon x region x time bin where occurrences are
#' recorded.
#' @examples
#' taxa_time_per_area(dat, area_tables, bins)
#' @export
taxa_time_per_area <- function(dat, area_tables, bins){
  taxonomic_level <- "Taxon"
  occs_table <- data.frame()
  bins <- sort(-abs(bins))
  for(i in 1:length(area_tables)){
    area_table <- area_tables[[i]]
    area_name <- area_table$Area[1]
    area_taxa <- unique(area_table[[taxonomic_level]])
    global_taxa_list <- unique(dat[[taxonomic_level]])
    all_taxa <- length(global_taxa_list)
    taxa_time_table <- data.frame(matrix(0, all_taxa, length(bins)-1))
    for (j in 1:length(area_taxa)){
      indices_occurrences <- which(area_table[[taxonomic_level]] == area_taxa[j])
      age_occs <- area_table[indices_occurrences,]$SampledAge
      h <- hist(x = -as.numeric(age_occs), breaks=bins, plot=F)
      t_i <- which(global_taxa_list == area_taxa[j])
      taxa_time_table[t_i,] <- h$counts
    }
    colnames(taxa_time_table) <- sprintf("t%d", seq(length(bins)-1))
    taxa_time_table <- cbind(Type="occs", Area=area_name, taxa_time_table)
    row.names(taxa_time_table) <- global_taxa_list 
    occs_table <- rbind(occs_table, taxa_time_table)
  }
  return(data.frame(occs_table))
}
