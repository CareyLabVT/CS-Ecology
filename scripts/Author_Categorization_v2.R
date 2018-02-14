# PSUEDOCODE!!
# - Install packages
# - Read in Ecology_FullRecords.CSV 
# - omit columns we don’t need, but keep the numeric identifier already in the full records CSV as each paper’s ID
# - read in long version of keywords CSV [this needs to be checked against Nicole’s original script, now on github] that has two columns: Keywords and Affiliations
# - use the for loop to assign disciplinary affiliations to each paper_ID (*not* the lumped categories of CS, MA, etc. but the level of Chemistry, Physics, etc)
# -within the for loop, create the author affiliation data frame. Importantly, this data frame also needs to be in long format, with four columns: “Paper_ID” (this is its number), “Affiliation_ID” (this is another numeric identifier (1, 2, 3…) for each affiliation listed in the order they are listed on the paper, even if they are repeats and they are later going to be excluded), Keyword, and Affiliation (with the two latter columns matching the keywords CSV columns). The Keyword is the character string from the affiliation that gets used to classify the affiliation a certain way, with the Affiliation the disciplinary affiliation.
# 
# (because there are three authors, and the listed keywords are the words in their affiliation that assigns them the Environmental Biology category from the keywords CSV)
# 
# -now we clean up the data frame: omit repeat affiliations within the same paper, omit government entities, omit papers that don’t have any affiliations, etc.
# -export this data frame as an “AuthorAffiliationDatabase.CSV”
# -now we look at each paper for our collaboration analysis- create a new for loop that goes through the data frame and assigns the lumped categories (CS, MA, etc.) [note: didn’t we get rid of ‘interdisciplinary computing’?] 
# - create a new data frame “InterdisciplinaryCollaboration.CSV” that looks similar to what Arianna created in the github output file, but with columns: “Paper_ID”, “MA”, “CS”, etc. and with the values in the categories the number of authors with those affiliations. There should also be ‘Total authors’ and ‘Total affiliations’ columns in this CSV so we have an easy gut-check about papers not having as many affiliations as authors (because we want each author per paper to have at least one affiliation). From this final data frame, we can then do the plotting (but in the separate plotting R script).