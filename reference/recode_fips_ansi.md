# Recode FIPS by ANSI

Replaces state ansi with state fips

## Usage

``` r
recode_fips_ansi(.data, .ansi)
```

## Arguments

- .data:

  data.frame or tibble

- .ansi:

  column with state ansi

## Value

.data with column .ansi replaced with state fips

## Examples

``` r
data('stata')
stata |> recode_fips_ansi(ansi)
#>    fips abb                        name ansi    region           division
#> 1    01  AL                     Alabama   01     South East South Central
#> 2    02  AK                      Alaska   02      West            Pacific
#> 3    04  AZ                     Arizona   04      West           Mountain
#> 4    05  AR                    Arkansas   05     South West South Central
#> 5    06  CA                  California   06      West            Pacific
#> 6    08  CO                    Colorado   08      West           Mountain
#> 7    09  CT                 Connecticut   09 Northeast        New England
#> 8    10  DE                    Delaware   10     South     South Atlantic
#> 9    11  DC        District of Columbia   11     South     South Atlantic
#> 10   12  FL                     Florida   12     South     South Atlantic
#> 11   13  GA                     Georgia   13     South     South Atlantic
#> 12   15  HI                      Hawaii   15      West            Pacific
#> 13   16  ID                       Idaho   16      West           Mountain
#> 14   17  IL                    Illinois   17   Midwest East North Central
#> 15   18  IN                     Indiana   18   Midwest East North Central
#> 16   19  IA                        Iowa   19   Midwest West North Central
#> 17   20  KS                      Kansas   20   Midwest West North Central
#> 18   21  KY                    Kentucky   21     South East South Central
#> 19   22  LA                   Louisiana   22     South West South Central
#> 20   23  ME                       Maine   23 Northeast        New England
#> 21   24  MD                    Maryland   24     South     South Atlantic
#> 22   25  MA               Massachusetts   25 Northeast        New England
#> 23   26  MI                    Michigan   26   Midwest East North Central
#> 24   27  MN                   Minnesota   27   Midwest West North Central
#> 25   28  MS                 Mississippi   28     South East South Central
#> 26   29  MO                    Missouri   29   Midwest West North Central
#> 27   30  MT                     Montana   30      West           Mountain
#> 28   31  NE                    Nebraska   31   Midwest West North Central
#> 29   32  NV                      Nevada   32      West           Mountain
#> 30   33  NH               New Hampshire   33 Northeast        New England
#> 31   34  NJ                  New Jersey   34 Northeast       Mid-Atlantic
#> 32   35  NM                  New Mexico   35      West           Mountain
#> 33   36  NY                    New York   36 Northeast       Mid-Atlantic
#> 34   37  NC              North Carolina   37     South     South Atlantic
#> 35   38  ND                North Dakota   38   Midwest West North Central
#> 36   39  OH                        Ohio   39   Midwest East North Central
#> 37   40  OK                    Oklahoma   40     South West South Central
#> 38   41  OR                      Oregon   41      West            Pacific
#> 39   42  PA                Pennsylvania   42 Northeast       Mid-Atlantic
#> 40   44  RI                Rhode Island   44 Northeast        New England
#> 41   45  SC              South Carolina   45     South     South Atlantic
#> 42   46  SD                South Dakota   46   Midwest West North Central
#> 43   47  TN                   Tennessee   47     South East South Central
#> 44   48  TX                       Texas   48     South West South Central
#> 45   49  UT                        Utah   49      West           Mountain
#> 46   50  VT                     Vermont   50 Northeast        New England
#> 47   51  VA                    Virginia   51     South     South Atlantic
#> 48   53  WA                  Washington   53      West            Pacific
#> 49   54  WV               West Virginia   54     South     South Atlantic
#> 50   55  WI                   Wisconsin   55   Midwest East North Central
#> 51   56  WY                     Wyoming   56      West           Mountain
#> 52   60  AS              American Samoa   60      <NA>               <NA>
#> 53   66  GU                        Guam   66      <NA>               <NA>
#> 54   69  MP    Northern Mariana Islands   69      <NA>               <NA>
#> 55   72  PR                 Puerto Rico   72      <NA>               <NA>
#> 56   74  UM U.S. Minor Outlying Islands   74      <NA>               <NA>
#> 57   78  VI         U.S. Virgin Islands   78      <NA>               <NA>
```
