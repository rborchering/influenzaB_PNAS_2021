# BEAST README

Output data to produce figure 3 from the paper can be seen in the `600_seqs` folder. These data files were produced using BEAST with the analysis parameters detailed in the XML files seen in the current folder. Sequence data used in this analysis is detailed in `gisaid_acknowledge_table.csv`. Due to the [terms of use detailed by GISAID](https://www.gisaid.org/registration/terms-of-use/), we removed sequence alignments from the XML files but can be retrieved from GISAID using the accessions in this `.csv` file. While this may be inconvenient, it is relatively easy to overcome using the following steps.

1. After retrieving sequences in the acknowledgement table, align sequences using a utility such as [MAFFT](https://mafft.cbrc.jp/alignment/software/).
2. The program [BEAUti](https://beast.community/beauti) may be used to get the alignment from step 1 and the associated labels into the required XML format.
3. Place the sequence labels and alignment you have produced in the previous steps into the appropriate location in the XML files we have provided.
4. Run BEASTv1.10.4 with the XML files using this example code snippet: `beast -threads 4 -beagle -beagle_SSE -overwrite b-vic_ha_usa_20160101-20200315.xml`
