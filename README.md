## Package

``` r
devtools::install_github("jaytimm/gbr")
```

### Metadata

``` r
subs <- gbr::gb_subset(x = 10,
                       min_birth_year = 1850,
                       max_birth_year = 1900,
                       subj = 'Science fiction',
                       lang = 'en',
                       filepath = path_to_out_folder) 
```

### Build corpus

``` r
subs |> 
  gbr::gb_build_corpus() |>
  dplyr::sample_n(5) |> 
  knitr::kable()
```

| doc_id       | text                                                                                                                                                                                                                               |
|:----|:------------------------------------------------------------------|
| PG44404.641  | Withdrawing as far as the limits of the steel structure would allow, they put their heads together and held a brief but animated conversation in tones so low that the professor and I could not overhear.                         |
| PG69394.1088 | “A ‘cert’ was a horse that was certain to win and never did.                                                                                                                                                                       |
| PG25439.2762 | After going to my room that night, I sat up late to read a romance of Berrian, handed me by Dr. Leete, the plot of which turned on a situation suggested by his last words, concerning the modern view of parental responsibility. |
| PG1164.1573  | And they were unconscious that they had made this sound.                                                                                                                                                                           |
| PG69394.5651 | Every day since the departure of Hetty I had been feeling more and more sure that this at least was not going to happen.                                                                                                           |

### Simple regex search

``` r
gbr::gb_subset(x = 5000) |>
  gbr::gb_search(pattern = '^I remembered', cores = 12) |>
  gbr::gb_examples(n = 1) |>
  knitr::kable()
```

| authoryearofbirth | text                                                   |
|------------------:|:----------------------------------------------------|
|              1799 | I remembered the dressmaker’s smile!                   |
|              1805 | I remembered it first.                                 |
|              1808 | I remembered its striking features so well.            |
|              1811 | I remembered the dim melodies of the Lady of the Lake. |
|              1818 | I remembered the lazo.                                 |
|              1819 | I remembered my vow to you.                            |
|              1821 | I remembered thee on the cross.                        |
|              1826 | I remembered.                                          |
|              1828 | I remembered it all then.                              |
|              1835 | I remembered my first lessons.                         |
|              1839 | I remembered more after that.                          |
|              1842 | I remembered no barrier.                               |
|              1850 | I remembered Forister’s parting sentence.              |
|              1855 | I remembered how rich he was.                          |
|              1857 | I remembered how we had parted on board the *Thames*.  |
|              1860 | I remembered the coolness that was there.              |
|              1861 | I remembered now.                                      |
|              1863 | I remembered her face.                                 |
|              1864 | I remembered them now, and discerned his meaning.      |
|              1865 | I remembered that you are joining your regiment.       |
|              1866 | I remembered her simply as a Normandy.                 |
|              1868 | I remembered his drawn face at my question.            |
|              1869 | I remembered the piece of good news I had for Raoul.   |
|              1870 | I remembered seeing barges in Bensersiel harbour.      |
|              1871 | I remembered the lad that was Jimmie Tool.             |
|              1872 | I remembered Smithers–well.                            |
|              1873 | I remembered but one conversation during that wait.    |
|              1875 | I remembered work that had to be done.                 |
|              1876 | I remembered the Cockney’s way of putting it.          |
|              1880 | I remembered my first day in the city.                 |
|              1881 | I remembered this with forebodings.                    |
|              1882 | I remembered how you looked after the funeral!         |
|              1886 | I remembered something I had forgotten to tell him.    |
|              1887 | I remembered that my boat was to sail on Friday.       |
|              1918 | I remembered it.                                       |
|              1923 | I remembered the library book and almost went back.    |
