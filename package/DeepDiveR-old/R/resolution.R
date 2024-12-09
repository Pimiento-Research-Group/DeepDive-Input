#' Assign resolution
#'
#' 'assign_res()' creates an additional column on the dat object that identifies
#' an occurrences as having an age range entirely encompassed within a high
#' resolution time bin (HR) or exceeding the boundaries of high resolution time
#' bins (LR).
#' @param dat Data frame of occurrences with taxon ID, Area, MinAge, MaxAge and
#' Locality columns.
#' @param age_range_threshold Specify an age range cut-off between high and low
#' resolution fractions (default=NA)
#' @param all_lr When set to TRUE all occurrences are assigned LR. Consider
#' using if there is a very high frequency of low resolution occurrences in a
#' given data set (default FALSE).
#' @returns dat_res, the dat table with an additional column where the
#' resolution of occurrences are categorised as LR (low res) or HR (high res).
#' @examples
#' assign_res(dat)
#' assign_res(dat, age_range_threshold=NA, all_lr=T)
#' @export
assign_res <- function(dat, age_range_threshold=NA, all_lr=F){
  resolution <- rep(NA, dim(dat)[1])
  dat_res <- cbind(dat, resolution)
  if(all_lr == F){
    for(i in 1:length(dat$MinAge)){
      dat_res$resolution[i] <- "LR"
      for(j in 1:length(bins$start)){
        if (dat$MaxAge[i] <= bins$start[j] && dat$MinAge[i] >= bins$end[j]){
          dat_res$resolution[i] <- "HR"
        }
      }
    }
  }
  if(all_lr == T){
    dat_res$resolution <- "LR"
  }
  hr_dat <- dat_res[which(dat_res$resolution == "HR"),]
  lr_dat <- dat_res[which(dat_res$resolution == "LR"),]

  # check the frequency of high resolution and low resolution occurrences
  freq_low_res <- fraction_lr_hr_dat(dat = dat, r_dat = lr_dat, res="low", age_range_threshold=age_range_threshold)
  freq_high_res <- fraction_lr_hr_dat(dat = dat, r_dat = hr_dat, res="high", age_range_threshold=age_range_threshold)
  print(paste0("Frequency of low resolution occurrences is ", freq_low_res))
  print(paste0("Frequency of high resolution occurrences is ", freq_high_res))
  print("If the frequency of high resolution occurrences is low, consider moving all to the low res fraction using param all_lr=T")

  return(dat_res)
}


#' Split low and high resolution data     ########## THIS IS NO LONGER IN USE BUT CONSIDER COMBINING WITH ASSIGN_RES SO THERE ARE OPTIONS TO STILL USE AN AGE_RANGE_THRESHOLD
#'
#' 'get_lr_hr_dat()' splits occurrence data by resolution, where low resolution
#' data comprises occurrences with age ranges that do not cross low resolution
#' bin boundaries and high resolution data that exceed the low resolution bin
#' boundaries.
#' @param data Occurrence data table
#' @param res Specify whether to return "low" or "high" resolution data
#' @param age_range_threshold Specify an arbitrary cut off point at which to
#' split high and low resolution data (default = NA) - not based on bin
#' boundaries in Ma.
#' @returns Data frame of occurrences at either high or low resolution, with
#' the shape of the data frame held constant.
#' @examples
#' get_lr_hr_dat(data = dat_res, res = "low", age_range_threshold = NA)
#' get_lr_hr_dat(data = dat_res, res = NA, age_range_threshold = 2)
#' @export
get_lr_hr_dat <- function(data=dat, res="high", age_range_threshold = NA){
  if(!is.na(age_range_threshold)){
    print("Using age_range_threshold")
    age_range <- dat$MaxAge - dat$MinAge  # find age ranges

    # find low and high res data and their indices
    lr <- dat[which(age_range > age_range_threshold), ]
    lr_ind <- which(age_range > age_range_threshold)
    hr <- dat[which(age_range <= age_range_threshold), ]
    hr_ind <- which(age_range <= age_range_threshold)

    if(res == "low"){
      # make a low res data subset of same dimensions as original data
      r_dat <- dat
      for(i in r_dat) {
        r_dat[lr_ind, ] <- lr
        r_dat[hr_ind, 5:6] <- NA
      }
      colnames(r_dat) <- c('Complete_name', 'Genus', 'Species', 'Area',
                           'MinAge', 'MaxAge', 'Locality')

    }

    if(res == "high"){
      # make a high res data subset of same dimensions as original data
      r_dat <- dat
      for(i in r_dat) {
        r_dat[lr_ind, 5:6] <- NA
        r_dat[hr_ind, ] <- hr
      }
      colnames(r_dat) <- c('Complete_name', 'Genus', 'Species', 'Area',
                           'MinAge', 'MaxAge', 'Locality')

    }

  }

  else{
    print("Using assignment to lr or hr bins")
    hr <- dat_res[which(dat_res$resolution == "HR"),]
    lr <- dat_res[which(dat_res$resolution == "LR"),]
    hr_ind <- which(dat_res$resolution == "HR")
    lr_ind <- which(dat_res$resolution == "LR")

    if(res == "low"){
      # make a low res data subset of same dimensions as original data
      r_dat <- dat_res
      for(i in r_dat) {
        r_dat[lr_ind, ] <- lr
        r_dat[hr_ind, 5:6] <- NA
      }
      colnames(r_dat) <- c('Complete_name', 'Genus', 'Species', 'Area',
                           'MinAge', 'MaxAge', 'Locality', 'Resolution')

    }

    if(res == "high"){
      # make a high res data subset of same dimensions as original data
      r_dat <- dat_res
      for(i in r_dat) {
        r_dat[lr_ind, 5:6] <- NA
        r_dat[hr_ind, ] <- hr
      }
      colnames(r_dat) <- c('Complete_name', 'Genus', 'Species', 'Area',
                           'MinAge', 'MaxAge', 'Locality', 'Resolution')

    }

  }
  return(r_dat)
}


#' What fraction of the data is at high vs low resolution
#'
#' 'fraction_lr_hr_dat()' finds the fraction of occurrences which have an age
#' range that is entirely within either the specified low resolution or high
#' resolution time bins.
#' @param dat Occurrence data table
#' @param res Specify "high" or "low" fraction to be returned
#' @param r_dat Occurrence data table at either high or low resolution
#' @param age_range_threshold Specify an age range cut-off between high and low
#' resolution fractions (default=NA)
#' @returns Returns fraction of the occurrences at either high or low resolution
#' depending on parameters used.
#' @examples
#' fraction_lr_hr_dat(dat, r_dat = lr_dat, res="low")
#' @export
fraction_lr_hr_dat <- function(dat, res="low", r_dat=NA, age_range_threshold = NA){
  # check fraction of occurrences assigned to the high res and low res datasets
  if(!is.na(age_range_threshold == TRUE)){
    # print("Using age_range_threshold")
    age_range <- dat$MaxAge-dat$MinAge
    if(res == "high"){
      fraction_res <- nrow(dat[which(age_range <= age_range_threshold), ]) / (nrow(dat[which(age_range > age_range_threshold), ]) + nrow(dat[which(age_range <= age_range_threshold), ]))
    }
    if(res == "low"){
      fraction_res <- nrow(dat[which(age_range > age_range_threshold), ]) / (nrow(dat[which(age_range > age_range_threshold), ]) + nrow(dat[which(age_range <= age_range_threshold), ]))
    }
  }
  else{
    # print("Using assignment to lr or hr bins")
    fraction_res <- length(r_dat$MinAge)/length(dat$MinAge)
  }
  return(fraction_res)
}
