# AppendIDs

The **AppendIDs** package provides researchers using cross-sectional and cross-sectional time-series (CSTS) country data with tools to standardize and merge datasets from sources using differing country-coding schemes. Researchers in international relations and comparative politics confront merging problems when combining datasets which rely on differing country-coding methods, such as Gleditch-Ward numbers, Correlates of War (COW) country codes, and International Financial Statistics (IFS) codes. Variations in naming and coding conventions across datasets, particularly for countries which may be coded under different names in the same year, can result in duplicated or missing observations which can affect empirical analyses.

The package's core function, `append_ids()`, can be used to apply standardized Gleditsch-Ward country names and identifiers to datasets with country-year and dyadic country-year observations. These new identifiers, along with their corresponding values in the Correlates of War coding conventions, are appended to the provided dataset automatically. 

The function also informs the user of duplicated country-year observations. The **AppendIDs** function can be used along with the countrycode package to label and merge datasets without uniform coding schemes, or to merge datasets from differing coding conventions. 

We rely on Gleditch-Ward naming conventions to provide a standardized coding criteria for country-year and dyadic country-year observations, enabling researchers to easily merge datasets from different sources using alternative coding conventions such as COW and IFS codes. Gleditch-Ward (Gleditsch and Ward 1999) conventions are selected as their coding criteria provide temporal continuity over country name changes, such as between observations of the "German Federal Republic" prior to 1990 and "Germany" in the post-1990 period.

The **AppendIDs** package is used to maintain the International Political Economy (IPE) Data Resource (Graham and Tucker 2016), which provides researchers in international relations and comparative politics with a unified dataset of important political and economic variables from 97 of the most commonly used datasets in the IPE literature.

## Installation

This package can be installed via the `devtools` package. To install the `devtools` package, run the following code:

```R
install.packages("devtools")
```

Then, after, you can install the `AppendIDs` package by running the following codes:

```R
devtools::install_github("alcocer-jj/AppendIDs")
```

or 
    
```R
library(devtools)
install_github("alcocer-jj/AppendIDs")
```

## Usage

The **AppendIDs** package provides several functions: `append_ids()`, `append_suffix()`, `cy_append_ids()`, `cy_append_suffix()`, `dyad_append_ids()`, and `dyad_append_suffix()`.

### append_ids()

The `append_ids()` function standardizes a dataframe with country-year observations by creating new country identifer columns based on Gleditsch-Ward (GW) country-year observations.

A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.

In addition to GW country names and country codes (gwno), adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.

#### Example

```R
new_data <- append_ids(data, breaks = F) 
```

### append_suffix()

The `append_suffix()` function appends a designated character string ("suffix") to all variables in a given _monadic_ or _dyadic_ country-year dataframe except for identifying column variables. 

Identifying column variables include country names, identifiers, and abbreviations, and year.
This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.

For example, "COW" could be appended as a suffix to variables from the Correlates of War dataset.

#### Example

```R
new_data <- append_suffix(data, suffix = "_COW")
```

### cy_append_ids()

The `cy_append_ids()` function standardizes a dataframe with _monadic_ country-year observations by creating a "country" column based on Gleditsch-Ward (GW) country-year observations.

A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.

In addition to GW country names and country codes (gwno), the function adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.

The supplied dataframe must include columns designating each observation's country identifier and the year of observation.
The Dataframe must also include single observations for each country-year without duplicated observations.

#### Example

```R
data_cy <- cy_append_ids(data, breaks=T)
```

### cy_append_suffix()

The `cy_append_suffix()` function appends a designated character string ("suffix") to all variables in a given _monadic_ country-year dataframe except for identifying column variables.

The identifying column variables include country names, identifiers, and abbreviations, and year.
This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.

#### Example

```R
cy_append_suffix("IR of Afghanistan", "Afghanistan")
```

### dyad_append_ids()

The `dyad_append_ids()` function standardizes a dataframe with _dyadic_ country-year observations by creating "repcountry" and "parcountry" columns based on Gleditsch-Ward (GW) country-year observations.

A master dataframe of Gleditch-Ward country-year observations is used to standardize country-year identifiers across observations, allowing for merging of datasets from sources using different country name coding schemes.

In addition to GW country names and country codes (gwno), adds additional columns with other potential country-year identifiers including Correlates of War (COW) and International Finanical Statistics (IFS) codes.

The supplied dataframe must include columns designating observations for each dyad member's country identifier and the year of observation.
The Dataframe must also include single observations for each repcountry-parcountry-year without duplicated observations.

#### Example

```R
data_dyad <- dyad_append_ids(data, breaks = F)
```

### dyad_append_suffix()

The `dyad_append_suffix()` function appends a designated character string ("suffix") to all variables in a given _dyadic_ dataframe except for identifying column variables. Identifying column variables include country names, identifiers, and abbreviations, and year.

This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.

#### Example

```R
dyad_append_suffix("IR of Afghanistan", "Afghanistan")
```

## Author Contributions: 

Jacob R. Tucker built the initial version of the `AppendIDs` function. Miriam Barnum made significant improvements to the structure and performance of this function, including enabling it to be more efficiently updated and extending it to work more smoothly with dyad-year data. Therese Anders also contributed  revisions to the code at a variety of junctures. Jose J. Alcocer, Stephen Schick, and Gabriel Solis completed the work to enable the `AppendIDs` function to be released as a publicly available R package. Benjamin A.T. Graham initiated the creation of the `AppendIDs` function and guided its development throughout. Jose J. Alcocer (alcocerj29@gmail.com) is the corresponding author for this package. 


## Acknowledgements
We would like to thank many years of research assistants in the USC Security and Political Economy (SPEC) Lab for their contributions to the `AppendIDs` function, especially the adding of additional usual country spellings and abbreviations. Particular thanks to Anisha Chinwalla, Min Haeng Cho, Taylor Dufour, Robert Huang, Kaitlyn King, Xinru Ma, Rohit Madan, Kartik Mathur, Suzie Mulesky, Jihyun Shin, Patrick Vossler, Xueyuan (Sherry) Wang, Grace Xu, and Madeline Zheng.

## References

Gleditsch, Kristian Skrede, and Michael D. Ward. 1999. "Interstate System Membership: A Revised List of the Independent States since 1816." International Interactions 25(4): 393-413.

Graham, Benjamin A.T., and Jacob R. Tucker. 2016. "The International Political Economy Data Resource." _International Studies Perspectives_ 17(3): 255-270.

## License

This package is released under the MIT License. Please see the LICENSE file for more information.

