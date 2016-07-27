
# find all genes
genes <- unique(htm@data$Metadata_gene)


measurements <- c("HTM_norm__z_score__Median_Cells_Math_EstimateGolgiTotalLong", "HTM_norm__z_score__Median_Cells_Math_EstimateNucleiTotalLong")
max_zscore <- -0.7
number_of_subpositions_for_hit = 2

for(experiment in unique(htm@data$Metadata_plate))
{
    cat(experiment, "\n")

    generesults = data.frame(rep(0,length(genes)),row.names=genes)

    for(gene in genes) {


        siRNAs_targeting_gene <- unique(htm@data$Metadata_siRNA[htm@data$Metadata_gene == gene])

        for(siRNA in siRNAs_targeting_gene)
            {
                selection = (htm@data$Metadata_plate == experiment) & (htm@data$Metadata_gene == gene) & (htm@data$Metadata_siRNA == siRNA) & (htm@data$HTM_qc)
                pos_total = sum(selection)
                #cat("Number of positions for siRNA in total", pos_total)
                tmp = list()
                for( m in measurements)
                    {
                        selection = selection & (htm@data[,m] < max_zscore)
                    }
                pos_all_measuerements_hits = sum(selection)
                #generesults[[siRNA]] <- c(pos_total, pos_all_measuerements_hits)
                #print(pos_all_measuerements_hits)
                if(!is.na(pos_all_measuerements_hits) & pos_all_measuerements_hits>= number_of_subpositions_for_hit)
                    {
                       #print(pos_all_measuerements_hits)
                       generesults[gene,1] =  generesults[gene,1] + 1
                       cat("   ",gene, "targeted by", siRNA, ":", pos_all_measuerements_hits, "of", pos_total, "positions are hits for all measurements\n")
                       if(generesults[gene,1] > 1)
                            {
                              cat("                 ********",gene,"HIT BY MORE THAN 1 siRNA ********\n")
                            }
                    }
            }
    }
    #print(generesults)
}