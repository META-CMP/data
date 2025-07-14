> [!NOTE]
>
> # Do monetary policy shocks affect output, employment, and prices? Meta-analyses on the effects of conventional monetary policy
>
> ## About:
>  
> This repository contains replication files and documentation for our **meta-study on the macroeconomic effects of conventional monetary policy**. Below, we provide guidance on our research process and the documentation of our data collection, data processing and transformation, and the scripts for the data analyses.
> 
> ## Project:
> 
> This repository is part of the research project [Monetary Policy and Energy Prices](https://www.tu-chemnitz.de/wirtschaft/vwl4/Makrooekonomie/MORPEP.php.en) funded by the [European Macro Policy Network (EMPN)](https://empn.eu/).
> 
> ## Pre-registration:
>
> The pre-registration and pre-analysis plan for our meta-study can be found at [https://osf.io/cduq4](https://osf.io/cduq4).
>
> ## Research papers:
> 
> Enzinger, M., Gechert, S., Heimberger, P., Prante, F., & Romero, D. F. (n.d.). **The overstated effects of conventional monetary policy on output and prices**. **[OSF Preprint]** [**[Replication files]**](https://github.com/META-CMP/data/tree/main/analysis/working_paper_1) 
>

---

## Data collection

### Literature data base search

We conducted a comprehensive search for literature that econometrically estimates effects of conventional monetary policy shocks on output, (un)employment or the price level.
We detailed our search strategy for relevant literature in [Sec. 3.3 and 3.4 of our pre-registration](https://osf.io/4jxk3). We used the [EconLit](https://www.aeaweb.org/econlit/) and the [Google Scholar](https://scholar.google.com/) databases for our search of primary studies. Due to differences in their search behaviour[^1], we decided to use one comprehensive query for EconLit and multiple simpler search queries for Google Scholar.

[^1]: Check [Sec. 3.3 of the pre-registration](https://osf.io/4jxk3) for details.

- The raw results and procedural details of our EconLit search can be accessed [here](data/study_search/database_search/raw/EconLit_search).
- The raw results and procedural details of our Google Scholar search can be accessed [here](data/study_search/database_search/raw/Google_Scholar_search).

This yielded 7455 bibliographic entries from our EconLit search and 10810 bibliographic entries from our Google scholar search. After de-duplication, checks for the availability of abstracts and the inclusion of additional studies from related existing meta-studies (see [here](data/study_search/database_search/processed/preparation_for_abstract_screening) for detailed documentation of these steps and related files), our consolidated dataset of primary studies with available abstracts totaled at 10714 entries.

### Abstract screening

This dataset of 10714 studies then entered into the artificial intelligence-supported abstract screening to exclude clearly ineligible studies according to our eligibility criteria as defined in [Sec. 3.5 of our pre-registration](https://osf.io/4jxk3). The title and abstract screening was conducted independently by two researchers using [ASReview](https://asreview.nl/). [Sec. 3.6.1 of our pre-registration](https://osf.io/4jxk3) presents the details of the abstract screening process. [Sec. 3.7.1 of our pre-registration](https://osf.io/4jxk3) defines the stopping rules for the abstract screening phase. See [here](data/study_search/database_search/processed/abstract_screening) for further documentation and related files of the abstract screening.

After the abstract screening we conducted some validity tests as well as agreement and overlap analysis on the merged abstract screening data of both screeners. We then randomized the order of the potentially relevant studies and prepared files to assist and document the full text download. See [here](data/study_search/database_search/processed/post_AS/testing_merging_dowload_prep.pdf) for the documentation and [here](data/study_search/database_search/processed/post_AS/testing_and_merging_of_AS_data.R) for the R code for these steps. 

### Full text screening

#### PDF retrieval 

Following the abstract screening, we proceeded to download full texts of the potentially relevant studies. The full text download was conducted by research assistants using standardized procedures as documented [here](data/study_search/database_search/processed/post_AS/full_text_download_HIWI_sheet.pdf). The studies were divided into [26 Excel files]() about 100 bibliographic entries each. For each entry, research assistants attempted to access PDFs via the provided URLs or DOIs, or by searching Google Scholar when direct links were unavailable or not working. All PDFs were saved using their BibtexKey as filename in a centralized folder. During the download process, assistants verified that each PDF matched the bibliographic information, checked for the most recent version of working papers, and documented various attributes including availability status, whether the paper was retracted, duplicates, and whether it was a master's/bachelor's thesis (which we defined as non-eligible). Special attention was paid to accessing the most current versions of working papers that may have been subsequently published in journals.

#### Full text assesment, coding and data extraction

...



### Snowballing



### PRISMA Diagramm



### External data


---

## Data processing and transformation

...



---

## Data analyses


...
