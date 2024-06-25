
# TO DOs:

- [ ] Renaming of paths with spaces " "
- [ ] Rework of folder structure? Does it all make sense?
- [ ] All links working correctly?
- [ ] Make consistent throughout repe: 10714 vs 10,714
- [ ] Delete outdated or irrelevant files
- [ ] All sub-README.md's mentioned in main README.md?


# Do monetary policy shocks affect output, employment, and prices? Meta-analyses on the effects of conventional monetary policy

This repository contains all raw and transformed data for our [pre-registered meta-study on the macroeconomic effects of conventional monetary policy](https://osf.io/cduq4). Below, we refer to the documentation of our data collection, management and transformation, as well as scripts for our data analyses.

> [!IMPORTANT]
> The pre-registration and the pre-analysis plan for our meta-study can be found at [https://osf.io/cduq4](https://osf.io/cduq4).

---

## Data collection

### Literature data base search

As a first step, we conducted a comprehensive search for literature that estimates effects of monetary policy shocks on output, (un)employment or the price level.
We detailed our search strategy for potentially relevant literature in [Sec. 3.3 and 3.4 of our pre-registration](https://osf.io/4jxk3). We used the [EconLit](https://www.aeaweb.org/econlit/) and the [Google Scholar](https://scholar.google.com/) databases for our search of primary studies. Due to differences in their search behaviour[^1], we decided to use one comprehensive query for EconLit and multiple simpler search queries for Google Scholar.

[^1]: See Sec. 3.3 of the pre-registration for details.

- The raw results and procedural details of our EconLit search can be accessed [here](data/study_search/database_search/raw/EconLit_search).
- The raw results and procedural details of our Google Scholar search can be accessed [here](data/study_search/database_search/raw/Google_Scholar_search).

This yielded 7455 bibliographic entries from our EconLit search and 10810 bibliographic entries from our Google scholar search. After de-duplication, checks for the availability of abstracts and the inclusion of additional studies from related existing meta-studies (see [here](data/study_search/database_search/processed
/preparation_for_abstract_screening) for detailed documentation of these steps and related files), our consolidated dataset of primary studies with available abstracts totaled at 10714 entries.

- [x] outline steps to get to our dataset
- [x] refer to folder and files
- [x] refer to more detailed documentation therin
- [ ] check if renaming of the folders without spaces is possible without causing any problems (should be a single commit). 
- [ ] check if something important in quarto docs

### Abstract screening

Our dataset of 10714 studies then entered into the artificial intelligence-supported abstract screening to excluded clearly ineligible studies according to our eligibility criteria as defined in [Sec. 3.5 of our pre-registration](https://osf.io/4jxk3). The title and abstract screening was conducted independently by two researchers using [ASReview](https://asreview.nl/). [Sec. 3.6.1 of our pre-registration](https://osf.io/4jxk3) presents the details of the abstract screening process. [Sec. 3.7.1 of our pre-registration](https://osf.io/4jxk3) defines the stopping rules for the abstract screening phase. See [here](data/study_search/database_search/processed/abstract_screening) for further documentation and related files of the abstract screening.

- [x] Store final ASReview files
- [x] Refer to AS rules and criteria in pre-reg
- [ ] outline steps to get to our dataset
- [ ] refer to folder and files
- [ ] refer to more detailed documentation therin
- [ ] check if renaming of the folders without spaces is possible without causing any problems (should be a single commit). 
- [ ] check if something important in quarto docs

### Full text screening

...

### Snowballing

### External data

...

---

## Data processing and transformation

...

- refer also to the package 

---

## Data analyses


> [!IMPORTANT]
> At the moment, this repo also contains our analysis scripts and results. But we should think about creating a separate repo for this within our organization.


...
