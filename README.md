# Prairie_Priority  [![DOI](https://zenodo.org/badge/391727042.svg)](https://doi.org/10.5281/zenodo.15346195)


Code for: Invasion timing affects multiple scales, metrics and facets of biodiversity outcomes in ecological restoration experiments

*Emma Ladouceur, Michael Wohlwend, Michele R. Schutzenhofer, Jonathan M. Chase, Tiffany M. Knight (2025) Invasion timing affects multiple scales, metrics and facets of biodiversity outcomes in ecological restoration experiments. Ecological Applications. Article DOI: [doi.org/10.1002/eap.70062](https://doi.org/10.1002/eap.70062) & Data DOI: [doi.org/10.6073/pasta/503e6ffe01cf9b94de4ac31d202f5e7a](https://doi.org/10.6073/pasta/503e6ffe01cf9b94de4ac31d202f5e7a)**


### R Scripts

* **1_Wrangle.R** <code> Field data cleaning and wrangling. Produces **sampled_dat.csv** provided. </code>

* **2_Presence_Cover_Combine.R** <code> Combines cover and presence surveys. Produces **pres_and_cover_plot.csv** provided. </code>

* **3_Taxonomy.R** <code> Cleans Taxonomy and harmonizes with TRY trait data. Produces **species_corrected_complete.csv** provided.  </code>

* **4_Traits.R** <code> Cleans TRY plant trait data. Produces **trait_matrix.csv** provided. </code>

* **5_Phylo.R** <code> Phylogenetic tree from species list. Produces **phylo.tree.txt** provided. </code>

* **6_Impute.R** <code> Imputes trait data. Produces **imputed_trait_matrix.csv** provided. </code>

* **7_Fig_1.R** <code> Uses iNEXT3D to estimate (rarefied & extrapolated) diversity across scales, metrics (Hill numbers q = 0, q = 2), and facets (Taxonomic, Phylogenetic, and Functional Diversity). Produces Figure 1 and associated data set's. All data set's provided, including: **Objects: TD_3D.RData, PD_3D.RData, FD_3D.RData; & Dataframes: prairie.hill.TD.csv, prairie.hill.PD.csv, prairie.hill.FD.csv** </code>

* **8_Fig_2_3_4_S1.R** <code> Calls **prairie.hill.3D.csv** data set's *listed above - TD, PD, FD*, pulls α-diversity & γ-diversity estimates, quantifies Whittaker's β-diversity & effect sizes for all scales, metrics and facets, visualizes results for Figures 2, 3, 4, S1. </code>

* **9_Fig_S2_S3.R** <code> Visualizes Figure S2 & S3. </code>