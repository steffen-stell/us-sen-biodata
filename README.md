# Bioguide Data Set

[bioguide.congress.gov](https://bioguide.congress.gov) provides biographical information for all former and current members of the U.S. Congress. For another project, I require the start and end of each Senator's tenure since 1914, their caucus affiliation, which Senate class they belong to, and when they were appointed and elected and whether it was a special election. 

The bioguide data is provided as json files, one for each member of congress, which can be [downloaded in bulk](https://bioguide.congress.gov/bioguide/data/BioguideProfiles.zip). Thus, the first purpose of this repo is to clean this data, bringing it into rectangular format. However, there are some data quality issues, namely some missing data as well as contradictory data. Consequently, the second purpose is to identify these issues and report them, such that they can be fixed, which has already been done for a number of them.
