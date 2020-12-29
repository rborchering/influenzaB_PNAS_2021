ha.gmrf.path <- "00-RawData/phylodynamic-data/600_seqs/HA_segment/HA_gmrf_data"
na.gmrf.path <- "00-RawData/phylodynamic-data/600_seqs/NA_segment/NA_gmrf_data"
ha.tree.path <- "00-RawData/phylodynamic-data/600_seqs/HA_segment/HA_segment_phylogeny.nex"
na.tree.path <- "00-RawData/phylodynamic-data/600_seqs/NA_segment/NA_segment_phylogeny.nex"

# Load data for GMRF Skyride plot
ha.df <- read.table(file = ha.gmrf.path,
                    header = TRUE,
                    sep = "\t",
                    stringsAsFactors = FALSE)

# Load data for GMRF Skyride plot
na.df <- read.table(file = na.gmrf.path,
                    header = TRUE,
                    sep = "\t",
                    stringsAsFactors = FALSE)

# Load phylogeny from nexus file
ha.tree <- read.nexus(ha.tree.path)

# Group tips by subclade label in the tip name
ha.taxa.grp <- list(V1A.3 = ha.tree$tip.label[grepl("V1A.3", ha.tree$tip.label)], 
                    V1A.2 = ha.tree$tip.label[grepl("V1A.2", ha.tree$tip.label)], 
                    V1A.1 = ha.tree$tip.label[grepl("V1A.1", ha.tree$tip.label)])
ha.tree <- groupOTU(ha.tree, ha.taxa.grp)

# Load phylogeny from nexus file
na.tree <- read.nexus(na.tree.path)

# Group tips by subclade label in the tip name
na.taxa.grp <- list(V1A.3 = na.tree$tip.label[grepl("V1A.3", na.tree$tip.label)], 
                    V1A.2 = na.tree$tip.label[grepl("V1A.2", na.tree$tip.label)], 
                    V1A.1 = na.tree$tip.label[grepl("V1A.1", na.tree$tip.label)])
na.tree <- groupOTU(na.tree, na.taxa.grp)