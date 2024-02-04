# DiscourseNetworks
SFI 2023, Discourse Network Project w/ Mirta Galesic
Some slight modifications (such as file path) must be made before using the code

## File Descriptions
**MergeNodes** -- [Input: site] Merges node files together for each site and creates a spreadsheet with all nodes across time (ideal for temporal comparison of nodes)
**NetVisNew** -- [Input: site, Topic of interest] Basic Topic of Interest Graph (TOIG) (Or "ego network") with a corresponding spreadsheet of network measures
**LocalNetPop** -- [Input: site, Topic of interest] creates a spreadsheet for the topic of interest and counts the presence of the nodes connected to it over time. It looks at the sum of the edge weights, the amount of times it appears, and the average edge weight. 
**LNP_Debug** -- Modified version of LocalNetPop, created due to an error in Atlantic's dataset; perhaps use this one instead
**ScreePlot** -- Failed attempt at PCA; made a scree plot of the most popular topics in each TOIG to decide how to simplify these networks. Decided to just do it arbitrarily, as it was more about aesthetics rather than analysis. 
**Simpl(AVG)_NetVis** -- a simplified network visualization using the highest average edge weights
**Simpl(SUM)_NetVis** -- a simplified network visualization using the highest sums of edge weights
**Simpl(COMB)_NetVis** -- a simplified network visualization using the highest amount of times each topic appears 
**MacroNet** -- A network visualization of all topics on a site for any given month. Colored and shaped according to the topics of interest (Abortion, Vaccines, Climate)
**NodePrevMatrix** -- [Input: site, Topic of interest] creates a matrix of the presence of each topic over time in the TOIGs *(Needs debugging)*
**CommunityDetection** -- Creates Community Detection graph for four months with colored communities as well as a spreadsheet of network metrics finding the central node in each community. 
**CommunityNodes** -- Created a table of communities with a list of each node within any given community. 
**CommunityOfInterest** -- Created a table of the mean shortest path from each community to the topics of interest (TOIs) to see which community is most related to each TOI.
**TempCommDect** -- Same as CommunityDetection, but for all months, and only generates the data Vis

*Experiment1-4 (ReadMe Soon)*
