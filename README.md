# Spatial-points-to-daily-movement-patterns

Open code for calculating individual movement patterns from long-term GPS location data of vultures in Israel to understand the correlation between vulture movement and characteristics of the vultures like sex, disease status, origin, location of activity (Judean desert, Negev, Carmel, Golan) to inform management decisions of the Israel Nature and Parks Authority under the NSF-BSF grant.

    Data extraction from MoveBank to only work on a subset of the long-term data. Data extracted from the 'Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel' study

    Data filtering 
    a. for the breeding season December 2020-June 2021. 
    b. only locations that were in and around Israel so as to capture inter-individual interactions in a landscape with majority of tagged vultures. 
    c. Only including vultures that were tracked for a long enough duration during this period (at least 1/3 of the days) 
    d. removing data that did not meet confidence standards or had faulty observations for example a speed of >120 m/s
    e. checking compatible projections

    Data analysis 
    a. Create empty dataframes to store the outputs in
    b. Create a for loop to extract each individual's data
    c. Calculate the time difference between consecutive timestamps within a day 
    note: it gives NA if the timestamps are from different dates so find time difference by date.
    d.Add a loop within this loop to extract data of fixes for each individual within one of the dates that it was tracked on
    e.Make spatial points in UTM to sf object
    f. Find the max displacement of each individual from the beginning of each day from its roost for the previous night and
    g. Concatenate each time to the empty dataset created earlier for max daily displacement and total flight duration per day.
    
    
    
  EntireMetaDataWithAllNetworkCentralitiesIndividualCharacteristics:
  All the individuals' movement metrics 50% use area, 95% use area using KDE, movement personality = KDE50/KDE95, average daily displacement, average flight duration, Shannon roost fidelity for each individual as a measure of the diversity of roosts or fidelity to roosts that each individual displays within and across seasons,  multilayer network versatility, and network centralities including Degree, Strength, PageRank, Hub, Authority in each of the social situation (co-flight, co-roosting, co-feeding) and the population level aggregate network. The individual characteristics include tag numbers, origin, birth year, age, age class - Juvenile (<5 years) or adult (>4 years), sex, and deployment region. This can be used to understand the seasonal, sex-based, origin-based, or region (KDE)-based correlations to individual social connectivity using the code 'MovementPatternsAsAFunctionOfIndividualCharacteristics.R'
    
    
