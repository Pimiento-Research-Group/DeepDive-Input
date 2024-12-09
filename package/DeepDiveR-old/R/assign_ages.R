#' Ages function
#'
#' 'ages()' assigns sampled ages to each occurrence within the range on MinAge
#' to MaxAge. Either using the median, a random age drawn
#' from a uniform distribution, or a random age drawn from uniform distribution
#' that is kept the same for occurrences which are sampled from the same
#' locality and have the same MinAge and MaxAge.
#' @param dat Occurrence data table
#' @param method Age assignment method. Either "median", "random" or
#' "random_by_loc".
#' @returns Occurrence data table with additional SampledAge column
#' @examples
#' ages(dat=your_data, method="median")
#' @export
ages <- function(dat, method){
  if (method == "median") {
    dat <- mutate(rowwise(dat), SampledAge = median(c(MinAge, MaxAge)))
  }
  if (method == "random") {
    SampledAge <- runif(length(dat$Complete_name), min = dat$MinAge, max = dat$MaxAge)
    dat <- cbind(dat, SampledAge)
  }
  if (method == "random_by_loc") {
    library(dplyr)
    #dat$SampledAge <- NA
    locate_and_assign <- c()
    for (i in unique(dat$Locality)) {
      locate <- which(dat$Locality == i)
      loc <- dat[locate, ]
      loc_distinct_ages <- loc %>% distinct(MinAge, MaxAge, Locality)
      SampledAge <- c()
      for(j in 1:nrow(loc_distinct_ages)){
        Age <- runif(n = 1, min = loc_distinct_ages$MinAge[j], max=loc_distinct_ages$MaxAge[j])
        SampledAge <- append(SampledAge, Age)
      }
      loc_distinct_ages <- cbind(loc_distinct_ages, SampledAge)
#      if(length(unique(loc$MinAge)) == 1 && length(unique(loc$MaxAge)) == 1){
      locate_and_assign <- rbind(locate_and_assign, loc_distinct_ages)

#      dat$SampledAge[which(dat$Locality == i)] <- Age
#      } else{
#        Age <- runif(n = nrow(loc), min = loc$MinAge, max = loc$MaxAge)
#        dat$SampledAge[which(dat$Locality == i)] <- Age
    #}
    }
    dat <- left_join(dat, locate_and_assign, by = c("MinAge" = "MinAge", "MaxAge" = "MaxAge", "Locality" = "Locality"))
  }
  for(i in length(dat$SampledAge)){
    if(dat$SampledAge[i] > dat$MaxAge[i] || dat$SampledAge[i] < dat$MinAge[i]){
      print("Error: SampledAge greater than max or less than min - check input.")
    }
    if(is.na(dat$SampledAge[i])){
      print("Error: SampledAge failed to update, there are NAs.")
    }
  }
  return(dat)
}
