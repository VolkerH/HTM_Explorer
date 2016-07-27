# Calculate Well means
# (use with caution, column names are hard coded and don't use the column configuration from the HTM Explorer Settings)
#

# add the measuerments for which you want to calculate the mean
calcmeanmeasurements <- c("HTM_norm__z_score__Median_Cells_Math_EstimateGolgiTotalLong", "HTM_norm__z_score__Median_Cells_Math_EstimateNucleiTotalLong")

# add new column filles with zeros
for(m in calcmeanmeasurements)
{
    newcolumn = paste("peter_wellmean_",m,sep="")
    cat("Adding new column ", newcolumn, "\n")
    htm@data[, newcolumn] <- htm@data[,"Metadata_wellnr"]*0.0
}

#
# go through each experiment and calculate mean for each well and each measurement (exluding positions that failed QC)
#
for(experiment in unique(htm@data$Metadata_plate))
{
    cat(experiment, "\n")
    for (well in sort(unique(htm@data$Metadata_wellnr[htm@data$Metadata_plate == experiment])))
    {
        #cat(well, "\n")
        selection =  (htm@data$Metadata_plate == experiment) & (htm@data$Metadata_wellnr==well) & (htm@data$HTM_qc)
        for(m in calcmeanmeasurements)
        {
            cat("   Well:", well, "Mean for measuerment", m, " :")
            newcolumn = paste("peter_wellmean_",m,sep="")
            tmpmean =mean(htm@data[selection, m])
            measuerement_mean = tmpmean
            cat(tmpmean,"\n")
            htm@data[selection, newcolumn] <- measuerement_mean
        }
    }
}

