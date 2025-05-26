# Practice typology
This is the repo of a small project using rating-based data for visualization and clustering. The data consist of expert-based ratings from 12 interviews of the importance of 17 behavioural factors (Theoretical Domain Functions, TDF) for the adoption of 18 agri-environmental practices by farmers in Germany. Data collected by Nina Büttner. Code by Bartosz Bartkowski. For more background information, see the corresponding publication cited below.

# Overview of contents
You can find here the following files:
* data.csv: summarized data with average scores for each TDF/practice combination (used for clustering)
* data_interviews.csv: main dataset consisting of individual TDF (columns)/practice (rows) scores for each interview (used for visualization)
* tdf_com_b_mapping.csv: simple mapping of TDFs to broader COM-B (Capability, Opportunity, Motivation - Behaviour) categories, needed for visualization purposes
* visualization_clustering.R: code for visualization (basic bar charts etc.) and clustering of the scores

# Corresponding publication
Rode, J., Bartkowski, B., Bittner, N., Müller, B., accepted. Grouping agri-environmental practices in Germany along behavioural drivers for adoption. German Journal of Agricultural Economics.
