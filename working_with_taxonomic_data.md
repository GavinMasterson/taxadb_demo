---
title: "Taxonomic Divergence"
author: "Gavin Masterson"
date: "22 March 2020"
output:
  html_document:
    keep_md: true
tags:
  - taxonomy
  - Rstats
  - reptile
---

## Setting the scene

During a hobby project using the Global Invasive Species Database (GISD), I encountered several issues that are common when working with taxonomic databases. Having searched the database and imported the data, I noticed that some species had missing values ("NA") for one or more variables. The missing values for the < 10 species were not difficult to determine, but what if I had been processing a species list of several hundred species? How would I approach the problem of taxonomic verification in an automated, reproducible manner? 

The second issue was that species in my list of invasive herpetofauna had undergone taxonomic reassignment, either at species, genus or even family level. How should a biologist deal with this issue when communicating the results of an analysis or data visualisation such as mine? These issues are the topic of this post.

## Reality check

In an **ideal world**, the GISD database (or whichever database you prefer) is updated as soon as taxonomic changes are accepted, and the search results can be treated as authoritative. Realistically, for any database with  vast number of taxa to consider it is unreasonable to expect the taxonomy of all species to be up-to-date and error-free. As I discovered in my invasive herpetofauna investigation, for some species the GISD was simultaneously ahead of and behind the other databases regarding taxonomic assignments and classifications.

For example, for *Norops grahami* the GISD appears to accept the generic reassignment from *Anolis*, while the [Reptile Database](http://www.reptile-database.org/) notes the change on it's *N. grahami* [page](http://reptile-database.reptarium.cz/species?genus=Anolis&species=grahami&search_param=%28%28search%3D%27anolis+grahami%27%29%29), but has not reassigned it to *Norops* in their database yet. For the same species though, the Reptile Database has the family assignment specified as Dactyloidae whereas the GISD still assigns it to the Polychrotidae. So for the genus, the GISD is ahead, but behind for the family.

Deciding what to do in situations like this is always tricky, but a simple solution is to be able to present these uncertainties to the readers of your research/communication. Thankfully there is an R package that can be used to query the world's major taxonomic databases from the comfort of your command line.

## Introducing {taxadb}

We install the `taxadb` packages as well as the `tidyverse` and our trusty `dplyr` for managing the post-query manipulations of the dataframes. Lastly I have used the `kableExtra` package to present the tabulated taxonomic data.


```r
library(taxadb)
library(tidyverse)
library(dplyr)
library(kableExtra)
```

### {taxadb}

The `taxadb` package installs a taxonomic database of your choice *on your workstation*. This local databaseis installed from `taxadb`, which is periodically updated from the relevant online database APIs. For a thorough understanding of the data sources used by `taxadb`, I encourage you to read the documentation found at the [rOpenSci taxadb page](https://docs.ropensci.org/taxadb/articles/data-sources.html). The TL:DR is that you should not simply merge information from two different taxonomic data sources. The point is made in this paragraph:

> "**Please Note**: `taxadb` advises against uncritically combining data from multiple providers. The same name is frequently used by different providers to mean different things – some providers consider two names synonyms that other providers consider distinct species. It is crucial to recognize that taxonomic name providers represent independent taxonomic theories, and not merely additional observations of the same immutable reality (Franz & Sterner (2018)). You cannot just merge two databases of taxonomic names like you can two databases of, say, plant traits to get a bigger and more complete sample, because the former can contain meaningful contradictions."

The data sources used by `taxadb` are updated on an annual basis, and this can be checked using the `available_versions` function. The first element `"2019"` tells us the last time the data sources were updated by `taxadb`. The `"dwc"` indicates that all data sources are formatted according to the [Darwin Core standard](https://dwc.tdwg.org/).


```r
available_versions()
```

```
## [1] "2019" "dwc"
```

#### A quick comparison of herpetofaunal species in GISD and ITIS 

In a related project, I queried the GISD database to ascertain the names of all herpetofaunal species that have established non-native or invasive populations to date. Let's import the file downloaded from the GISD and compare it to the herpetofauna listed in the ITIS database. First we import the `.csv` datafile, do some data preparation.

Second we create a local ITIS database using td_create. Third we extract all amphibian and reptile species from the ITIS database. Lastly we join the information in the two tibbles using a `left_join` where we tell the function that `Species` in GISD is the same variable as `scientificName` in ITIS. After joining the two tibbles, I decided to select just the variables that I am interested in.


```r
GISD_query <- read_delim("amrep_gisd.csv", trim_ws = TRUE, delim = ";") %>%
              .[,-8] %>% 
              separate(Species, c("Genus", 
                                  "Specific_Epithet", 
                                  "Infraspecific_Epithet"), 
                       sep = " ", remove = FALSE) 

td_create("itis")
database <- filter_rank(c("Amphibia", "Reptilia"), "class")

db_check <- GISD_query %>% 
                left_join(database, by = c("Species" = "scientificName")) %>%
                select(species_GISD = Species,
                       vernacularName_ITIS = vernacularName,
                       order_GISD = Order, order_ITIS = order,
                       family_GISD = Family, family_ITIS = family,
                       taxonomicStatus_ITIS = taxonomicStatus,
                       acceptedNameUsageID) 
                
db_check %>%  select(-vernacularName_ITIS) %>%
              .[c(1,5,6,16,20,23,28,30,31,43),] %>% 
              kable() %>% 
              kable_styling(bootstrap_options = c("striped", "hover")) %>% 
              column_spec(column = 1, italic = TRUE) %>% 
              row_spec(row = c(5,9), background = "Dodgerblue", color = "white")
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> species_GISD </th>
   <th style="text-align:left;"> order_GISD </th>
   <th style="text-align:left;"> order_ITIS </th>
   <th style="text-align:left;"> family_GISD </th>
   <th style="text-align:left;"> family_ITIS </th>
   <th style="text-align:left;"> taxonomicStatus_ITIS </th>
   <th style="text-align:left;"> acceptedNameUsageID </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Anolis aeneus </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Polychrotidae </td>
   <td style="text-align:left;"> Dactyloidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:1056079 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Anolis equestris </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Polychrotidae </td>
   <td style="text-align:left;"> Dactyloidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:173891 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Anolis extremus </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Polychrotidae </td>
   <td style="text-align:left;"> Dactyloidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:1056181 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Boiga irregularis </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Colubridae </td>
   <td style="text-align:left;"> Colubridae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:174206 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;color: white !important;background-color: Dodgerblue !important;"> Elaphe guttata </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Squamata </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Squamata </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Colubridae </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Colubridae </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> synonym </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> ITIS:1081818 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Eleutherodactylus planirostris </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Leptodactylidae </td>
   <td style="text-align:left;"> Eleutherodactylidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:173568 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Lithobates catesbeianus </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Ranidae </td>
   <td style="text-align:left;"> Ranidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:775084 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Natrix maura </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> Colubridae </td>
   <td style="text-align:left;"> Colubridae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:700797 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;color: white !important;background-color: Dodgerblue !important;"> Norops grahami </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Squamata </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> NA </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> Polychrotidae </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> NA </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> NA </td>
   <td style="text-align:left;color: white !important;background-color: Dodgerblue !important;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Xenopus laevis </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Anura </td>
   <td style="text-align:left;"> Pipidae </td>
   <td style="text-align:left;"> Pipidae </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:173549 </td>
  </tr>
</tbody>
</table>

Unfortunately, taxonomic data and their dataframes don't make for very pretty data visualisations. I apologise for the 'wall of text' feeling in this post, but I hope that you find this worked example of `taxadb` worth the eye-strain.

The table above shows just 10 of the 43 species but gives you a feel for the information I have extracted from ITIS. The two rows highlighted blue indicate examples of species that need additional consideration. *Elaphe guttata* is listed as a synonym, so we need to find the new, accepted name for the species, and *Norops grahami* is the species I mentioned earlier and appears to be missing from the ITIS database.

We are interested in the species in the GISD that have no match in the ITIS database. Species that have no match will have missing values for all the *_ITIS variables, so I chose `taxonomicStatus_ITIS`. The output shows us that there are four species in the GISD that have no match in the ITIS database.


```r
db_check[which(is.na(db_check$taxonomicStatus_ITIS) == TRUE),] %>% 
  select(-vernacularName_ITIS, -acceptedNameUsageID) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(column = 1, italic = TRUE, width = "6cm")
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> species_GISD </th>
   <th style="text-align:left;"> order_GISD </th>
   <th style="text-align:left;"> order_ITIS </th>
   <th style="text-align:left;"> family_GISD </th>
   <th style="text-align:left;"> family_ITIS </th>
   <th style="text-align:left;"> taxonomicStatus_ITIS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 6cm; font-style: italic;"> Anolis wattsi </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Polychrotidae </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 6cm; font-style: italic;"> Boa constrictor imperator </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Boidae </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 6cm; font-style: italic;"> Norops grahami </td>
   <td style="text-align:left;"> Squamata </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Polychrotidae </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 6cm; font-style: italic;"> Trachemys scripta elegans </td>
   <td style="text-align:left;"> Testudines </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Emydidae </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

I am also interested in names that are synonyms and not currently the accepted scientific name for the species. Any species identified by a synonym in the GISD merits further investigation to determine if the most recent taxonomic assignment gave due consideration to the status of the alien/invasive population.

Using the code below, I compare the name from our GISD list with the accepted name in ITIS database. The code allows us to determine the level at which the reassignment occurred. A reassignment at genus level might mean that less attention is needed as compared to a change in the specific epithet. The work doesn't end when you identify differences using this comparison but it does get a useful pointer.


```r
db_check[which(db_check$taxonomicStatus_ITIS == "synonym"),] %>% 
  select(-vernacularName_ITIS) %>% 
  left_join(.,database, by = "acceptedNameUsageID") %>% 
  filter(taxonomicStatus == "accepted") %>% 
  select(species_GISD,
         acceptedName_ITIS = scientificName,
         acceptedNameUsageID
         ) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(column = 1, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> species_GISD </th>
   <th style="text-align:left;"> acceptedName_ITIS </th>
   <th style="text-align:left;"> acceptedNameUsageID </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Chamaeleo jacksonii </td>
   <td style="text-align:left;"> Trioceros jacksonii </td>
   <td style="text-align:left;"> ITIS:1055685 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Elaphe guttata </td>
   <td style="text-align:left;"> Pantherophis guttatus </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Litoria aurea </td>
   <td style="text-align:left;"> Ranoidea aurea </td>
   <td style="text-align:left;"> ITIS:1099285 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Norops sagrei </td>
   <td style="text-align:left;"> Anolis sagrei </td>
   <td style="text-align:left;"> ITIS:173903 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Ramphotyphlops braminus </td>
   <td style="text-align:left;"> Indotyphlops braminus </td>
   <td style="text-align:left;"> ITIS:1116297 </td>
  </tr>
</tbody>
</table>

The `acceptedNameUsageID` number is a way for us to backreference the spcies to the ITIS database to extract all the synonyms for a single species. Below I demonstrate the process for *Elaphe guttata* (Eastern Corn Snake), which shows us that the ITIS database recognises *Pantherophis guttatus* as the accepted name and the three other classifications as synonyms. You can also see that the `acceptedNameUsageID`is the same for all names of this species while the `taxonID` is different for each.


```r
filter(database, acceptedNameUsageID == "ITIS:1081818") %>% 
  select(scientificName,
         taxonRank,
         taxonomicStatus,
         acceptedNameUsageID,
         taxonID) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(column = 1, italic = TRUE, width = "5cm")
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> scientificName </th>
   <th style="text-align:left;"> taxonRank </th>
   <th style="text-align:left;"> taxonomicStatus </th>
   <th style="text-align:left;"> acceptedNameUsageID </th>
   <th style="text-align:left;"> taxonID </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;width: 5cm; font-style: italic;"> Elaphe guttata </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> synonym </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
   <td style="text-align:left;"> ITIS:174175 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; font-style: italic;"> Elaphe guttata guttata </td>
   <td style="text-align:left;"> subspecies </td>
   <td style="text-align:left;"> synonym </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
   <td style="text-align:left;"> ITIS:174176 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; font-style: italic;"> Coluber guttatus </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> synonym </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
   <td style="text-align:left;"> ITIS:209204 </td>
  </tr>
  <tr>
   <td style="text-align:left;width: 5cm; font-style: italic;"> Pantherophis guttatus </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
   <td style="text-align:left;"> ITIS:1081818 </td>
  </tr>
</tbody>
</table>

# Out with the old...

The last demo I want to do with `taxadb` is compare a list of species names from a reptile survey in 2006 with the current taxonomy of the species. I know for a fact that several names have changed over the past 14 years, so let's see how easy it is to determine the new accepted species names.


```r
data_2006 <- read_csv("rep_survey.csv")

species_2006 <- filter_name(data_2006[[1]]) %>% 
                select(reptiles_2006 = input,
                       acceptedNameUsageID,
                       Genus_ITIS = genus,
                       specificEpithet_ITIS = specificEpithet,
                       taxonomicStatus_ITIS = taxonomicStatus,
                       vernacularName_ITIS = vernacularName)
```

The output of our query provides food for thought. Five of the 20 species find no matches in the ITIS database. Eleven names are still the accepted name for the species concerned and two names are synonyms. We could use our code from above to get the accepted name, but we see that the `filter_name` function returns a `genus` and `specificEpithet` variable from the ITIS database. In the case of synonyms, these two variables hold the accepted name for the species.


```r
species_2006 %>% 
  filter(taxonomicStatus_ITIS == "synonym") %>% 
  select(-vernacularName_ITIS) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(column = 1, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> reptiles_2006 </th>
   <th style="text-align:left;"> acceptedNameUsageID </th>
   <th style="text-align:left;"> Genus_ITIS </th>
   <th style="text-align:left;"> specificEpithet_ITIS </th>
   <th style="text-align:left;"> taxonomicStatus_ITIS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-style: italic;"> Lamprophis capensis </td>
   <td style="text-align:left;"> ITIS:1082810 </td>
   <td style="text-align:left;"> Boaedon </td>
   <td style="text-align:left;"> capensis </td>
   <td style="text-align:left;"> synonym </td>
  </tr>
  <tr>
   <td style="text-align:left;font-style: italic;"> Typhlops bibronii </td>
   <td style="text-align:left;"> ITIS:1116090 </td>
   <td style="text-align:left;"> Afrotyphlops </td>
   <td style="text-align:left;"> bibronii </td>
   <td style="text-align:left;"> synonym </td>
  </tr>
</tbody>
</table>

Two of the seven species for which we have no genus or specific epithet matches do have ITIS ID numbers but they have missing taxonomic hierarchy information. While we could use their ID numbers to get further information, here I will use the `fuzzy_filter` function to search for matching names at different hierarchies. For *Agama aculeata distanti*, *Agama atra atra*, *Bitis arietans arietans*, we see that taxonomic information is not missing when we search at the species level but is missing for each subspecies level. For *Agama atra*, the ITIS databse recognises no subspecies, so our trinomial designation finds no ID match.


```r
input <- species_2006 %>%  
          filter(is.na(Genus_ITIS) == TRUE) %>% 
          separate(reptiles_2006, c("Genus", "Specific_Epithet", "Subspecific_Epithet"),
                               sep = " ", 
                               remove = FALSE) %>% 
          unite(c(Genus, Specific_Epithet), 
                col = "binomial",
                sep = " ",
                remove = FALSE)

fuzzy_filter(c(input$binomial), match = "contains") %>% 
  select(taxonID,
         scientificName,
         taxonRank,
         taxonomicStatus,
         class) %>%
  mutate(
    class = if_else(class == "Reptilia",
                  cell_spec(class, "html", 
                            background = "Dodgerblue", 
                            color = "white", 
                            bold = T),
                  class),
    taxonRank = if_else(taxonRank == "subspecies",
                  cell_spec(taxonRank, "html", 
                            background = "green", 
                            color = "white", 
                            bold = T),
                  taxonRank)
    ) %>% 
  arrange(by_group = scientificName) %>% 
  kable("html", escape = F) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(column = 2, italic = TRUE)
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> taxonID </th>
   <th style="text-align:left;"> scientificName </th>
   <th style="text-align:left;"> taxonRank </th>
   <th style="text-align:left;"> taxonomicStatus </th>
   <th style="text-align:left;"> class </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ITIS:1055456 </td>
   <td style="text-align:left;font-style: italic;"> Agama aculeata </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: Dodgerblue !important;">Reptilia</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:1056979 </td>
   <td style="text-align:left;font-style: italic;"> Agama aculeata aculeata </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: green !important;">subspecies</span> </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:1056978 </td>
   <td style="text-align:left;font-style: italic;"> Agama aculeata distanti </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: green !important;">subspecies</span> </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:1055460 </td>
   <td style="text-align:left;font-style: italic;"> Agama atra </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: Dodgerblue !important;">Reptilia</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:634949 </td>
   <td style="text-align:left;font-style: italic;"> Bitis arietans </td>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: Dodgerblue !important;">Reptilia</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:635232 </td>
   <td style="text-align:left;font-style: italic;"> Bitis arietans arietans </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: green !important;">subspecies</span> </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITIS:635233 </td>
   <td style="text-align:left;font-style: italic;"> Bitis arietans somalica </td>
   <td style="text-align:left;"> <span style=" font-weight: bold;    color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: green !important;">subspecies</span> </td>
   <td style="text-align:left;"> accepted </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Using the `input` object created above we could search for fuzzy matches of the genus name or specific epithet to get a more detailed understanding of the taxonomic situation in each case. It is interesting to see the outputs, just change `input$binomial` to `input$Genus` etc. You will see that matches from any class are returned, so you can improve the returned table using a call such as `filter(class == "Reptilia)`, but remember that you will also lose all "NA" columns this way.

# Taxonomic issues in data science projects

Taxonomic assignment within the 'Tree of Life' is a neverending process of hypothesis generation and revision. The technology for genomic sequencing and analysis has been available for more than 40 years, and yet phylogenetic revisions of reptiles and amphibians are being published annually. Processing these revisions places a heavy burden on database managers and they do an often thankless task with great dedication. The nett result is that each database varies from others in unpredictable ways. If this post achieves anything, I hope it gives you a deep appreciation for the fact that we are learning new facts about the interrelatedness of all organisms with every phylogenetic analysis conducted. Secondly, I want to highlight the incredible work being done by all taxonomic database managers in their efforts to curate the relevant taxonomic changes (read: taxonomic hypotheses) on an ongoing basis. Their work makes my biological research infinitely easier. A huge thank you to you all!

The last thank you goes to the developers of the `taxadb` package - Carl Boettiger (Author, maintainer); Kari Norman (Author); Jorrit Poelen (Author); Scott Chamberlain (Author); Noam Ross (Contributor). I am always blown away by the R community and its collaborative, opensource practices. The openSci project is a brilliant example of this philosophy. Thank you so much.
