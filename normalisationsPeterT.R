#
# July 2016 Volker.Hilsenstein@embl.de
#
# custom normalisations for Peter T project based
# on htmImageNormalization
#
# In this layout, for every two wells there is one control well
# against which we want to normalise.
#
# on most plates it is quite simple. if a control
# well has well number i, the wells that need to be
# normalised against it are wells i, i+1
# the control wells all have the treatment name "Scramble".
#
# one exeption is plate 11 which has a slightly different
# layout, where for some control wells there is only a
# single treatment well

# this is to fit in to tischi's htm explorer tool as nicely
# as possible, i.e. it needs to take the selected feature
# columns, quality controls, and other settings from the
# htm@settings object


# note that this code does not loop over the measurements but only takes
# a single measurement. (keeping it that way)


#
# Generic data normalisation
#

htmPeterTNormalization <- function(htm) {

  print("")
  print("Image normalisation for PeterT layout")
  print("**************************************")
  print("")

  data <- htm@data

  # get all necessary information
  measurements <-  htmGetListSetting(htm,"statistics","featureSelection")
  #measurement <- htmGetListSetting(htm,"statistics","measurement")
  experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  normalisation <- htmGetListSetting(htm,"statistics","normalisation")
  negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))

  cat("\nMeasurements:\n")
  print(measurements)
  cat("\nNegative Control:\n")
  print(negcontrols)



  for(measurement in measurements) {
  cat("\nNormalising measurment ", measurement, "\n")
  htmSetListSetting(htm, "statistics", "measurement", measurement)
  #
  # Check
  #
  if(measurement=="None selected") {
    cat("\n\nError: please select a measurement!\n\n")
    return(htm)
  }
  if( ! (measurement %in% names(htm@data)) ) {
    cat(names(htm@data))
    cat("\nError: selected measurement is none of above data column names!\n")
    return(htm)
  }


  # init
  manipulation <- "__"
  input <- measurement

  # Log2
  if(transformation == "log2") {

    print("")
    print("Log2:")
    print(paste("  Input:", input))

    # compute log transformation
    # create new column name
    manipulation <- paste0(manipulation,"log2__")

    output = paste0("HTM_norm",manipulation,measurement)

    idsGtZero <- which(data[[input]]>0)
    idsSmEqZero <- which(data[[input]]<=0)
    data[idsGtZero,output] <- log2(data[idsGtZero,input])
    data[idsSmEqZero,output] <- NaN
    print(paste("  Output:", output))
    print(paste("  Number of data points:",length(data[[input]])))
    print(paste("  NaN's due to <=0:",length(idsSmEqZero)))

    input <- output

  } # if log transformation


  if(normalisation != "None selected") {

    print("")
    print("Per batch normalisation:")
    print(paste("  Method:",normalisation))
    print(paste("  Input:",input))

    # init columns
    manipulation <- paste0(manipulation,normalisation,"__")
    output = paste0("HTM_norm",manipulation,measurement)
    data[[output]] = NA
    print(paste("  Output:",output))

    # computation
    #cat("\nComputing normalisations...\n")

    for(experiment in experiments) {
      print(paste("  Experiment:",experiment))

      # find Control Wells
      wellcol <- htmGetListSetting(htm,"columns","wellnum")
      cat("Well column is called ", wellcol, "\n")
      treatcol <- htmGetListSetting(htm,"columns","treatment")
      cat("Treatment column is called ", treatcol, "\n")


      controlwells <- sort(unique(data[data[,treatcol] %in% negcontrols][,wellcol]))

      cat("Controlwells:\n")
      cat(controlwells)
      cat("\n")

      for (normwell in controlwells)
      {

         cat("Normalizing wells ", normwell+1, " and ", normwell+2 , " against ", normwell , "\n")

      currentwells = c(normwell, normwell+1, normwell+2)
      indices_all <- which(data[[htm@settings@columns$experiment]] == experiment & data[,wellcol] %in% currentwells)
      indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qc) & !is.na(data[[input]] & data[,wellcol] %in% currentwells))

      indices_controls_ok <- which((data[[htm@settings@columns$experiment]] == experiment)
                                     & !is.na(data[[input]])
                                     & (data$HTM_qc)
                                     & (data[,wellcol] %in% normwell))


      #print(paste("   Total", length(indices_all)))
      #print(paste("   Valid", length(indices_ok)))
      #print(paste("   Valid Control", length(indices_controls_ok)))

      # extract control values
      valuescontrol <- data[indices_controls_ok, input]
      #print(valuescontrol)

      nr_of_controls <-  length(valuescontrol)
      meancontrol <- mean(valuescontrol)
      sigmacontrol <- sd(valuescontrol)
      mediancontrol <- median(valuescontrol)
      madcontrol <- mad(valuescontrol)
      semcontrol <- sigmacontrol/sqrt(nr_of_controls)
      #print(paste("    Control Mean:", meancontrol))
      #print(paste("    Control SD:", sigmacontrol))
      #print(paste("    Control Median:", mediancontrol))
      #print(paste("    Control MAD:", madcontrol))

      # Volker's comment: all data are normalised, even those who passed QC
      # The exlusion of positions with failed QC only affects the negative controls

      if(normalisation == "z_score") {
        data[indices_all, output] <- ( data[indices_all, input] - meancontrol ) / sigmacontrol
      }
      else if(normalisation == "robust_z_score") {
        data[indices_all, output] <- ( data[indices_all, input] - mediancontrol ) / madcontrol
      }
      else if(normalisation == "subtract_mean_ctrl") {
        data[indices_all, output] <- data[indices_all, input] - meancontrol
      }
      else if(normalisation == "divide_by_mean_ctrl") {
        data[indices_all, output] <- data[indices_all, input] / meancontrol
      }
      else if(normalisation == "subtract_median_ctrl") {
        data[indices_all, output] <- data[indices_all, input] - mediancontrol
      }
      else if(normalisation == "divide_by_median_ctrl") {
        data[indices_all, output] <- data[indices_all, input] / mediancontrol
      }
     } # well loop
    } # experiment loop

        input <- output
     } # featureSelection loop
  }
  return(data)
}

