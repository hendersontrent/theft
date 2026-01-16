# Calculate interval summaries with a measure of central tendency of classification results

Calculate interval summaries with a measure of central tendency of
classification results

## Usage

``` r
calculate_interval(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  type = c("sd", "qt", "quantile"),
  interval = NULL,
  model_type = c("main", "null")
)
```

## Arguments

- data:

  `list` object containing the classification outputs produce by
  `tsfeature_classifier`

- metric:

  `character` denoting the classification performance metric to
  calculate intervals for. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- by_set:

  `Boolean` specifying whether to compute intervals for each feature
  set. Defaults to `TRUE`. If `FALSE`, the function will instead
  calculate intervals for each feature

- type:

  `character` denoting whether to calculate a +/- SD interval with
  `"sd"`, confidence interval based off the t-distribution with `"qt"`,
  or based on a quantile with `"quantile"`. Defaults to `"sd"`

- interval:

  `numeric` scalar denoting the width of the interval to calculate.
  Defaults to `1` if `type = "sd"` to produce a +/- 1 SD interval.
  Defaults to `0.95` if `type = "qt"` or `type = "quantile"` for a 95
  per cent interval

- model_type:

  `character` denoting whether to calculate intervals for main models
  with `"main"` or null models with `"null"` if the `use_null` argument
  when using `tsfeature_classifier` was `use_null = TRUE`. Defaults to
  `"main"`

## Value

`data.frame` containing the results

## Author

Trent Henderson

## Examples

``` r
# \donttest{
featMat <- calculate_features(data = simData, 
  id_var = "id", 
  time_var = "timepoint", 
  values_var = "values", 
  group_var = "process", 
  feature_set = "catch22",
  seed = 123)
#> No IDs removed. All value vectors good for feature extraction.
#> Running computations for catch22...
#> 
#> Calculations completed for catch22.
  
classifiers <- tsfeature_classifier(featMat,
  by_set = FALSE)
#> Only one set of 'catch22', 'feasts', 'tsfeatures', or 'Kats' with potential duplicates is in your feature data. Exiting and returning original input data.
#> Fitting model 1/660
#> Fitting model 2/660
#> Fitting model 3/660
#> Fitting model 4/660
#> Fitting model 5/660
#> Fitting model 6/660
#> Fitting model 7/660
#> Fitting model 8/660
#> Fitting model 9/660
#> Fitting model 10/660
#> Fitting model 11/660
#> Fitting model 12/660
#> Fitting model 13/660
#> Fitting model 14/660
#> Fitting model 15/660
#> Fitting model 16/660
#> Fitting model 17/660
#> Fitting model 18/660
#> Fitting model 19/660
#> Fitting model 20/660
#> Fitting model 21/660
#> Fitting model 22/660
#> Fitting model 23/660
#> Fitting model 24/660
#> Fitting model 25/660
#> Fitting model 26/660
#> Fitting model 27/660
#> Fitting model 28/660
#> Fitting model 29/660
#> Fitting model 30/660
#> Fitting model 31/660
#> Fitting model 32/660
#> Fitting model 33/660
#> Fitting model 34/660
#> Fitting model 35/660
#> Fitting model 36/660
#> Fitting model 37/660
#> Fitting model 38/660
#> Fitting model 39/660
#> Fitting model 40/660
#> Fitting model 41/660
#> Fitting model 42/660
#> Fitting model 43/660
#> Fitting model 44/660
#> Fitting model 45/660
#> Fitting model 46/660
#> Fitting model 47/660
#> Fitting model 48/660
#> Fitting model 49/660
#> Fitting model 50/660
#> Fitting model 51/660
#> Fitting model 52/660
#> Fitting model 53/660
#> Fitting model 54/660
#> Fitting model 55/660
#> Fitting model 56/660
#> Fitting model 57/660
#> Fitting model 58/660
#> Fitting model 59/660
#> Fitting model 60/660
#> Fitting model 61/660
#> Fitting model 62/660
#> Fitting model 63/660
#> Fitting model 64/660
#> Fitting model 65/660
#> Fitting model 66/660
#> Fitting model 67/660
#> Fitting model 68/660
#> Fitting model 69/660
#> Fitting model 70/660
#> Fitting model 71/660
#> Fitting model 72/660
#> Fitting model 73/660
#> Fitting model 74/660
#> Fitting model 75/660
#> Fitting model 76/660
#> Fitting model 77/660
#> Fitting model 78/660
#> Fitting model 79/660
#> Fitting model 80/660
#> Fitting model 81/660
#> Fitting model 82/660
#> Fitting model 83/660
#> Fitting model 84/660
#> Fitting model 85/660
#> Fitting model 86/660
#> Fitting model 87/660
#> Fitting model 88/660
#> Fitting model 89/660
#> Fitting model 90/660
#> Fitting model 91/660
#> Fitting model 92/660
#> Fitting model 93/660
#> Fitting model 94/660
#> Fitting model 95/660
#> Fitting model 96/660
#> Fitting model 97/660
#> Fitting model 98/660
#> Fitting model 99/660
#> Fitting model 100/660
#> Fitting model 101/660
#> Fitting model 102/660
#> Fitting model 103/660
#> Fitting model 104/660
#> Fitting model 105/660
#> Fitting model 106/660
#> Fitting model 107/660
#> Fitting model 108/660
#> Fitting model 109/660
#> Fitting model 110/660
#> Fitting model 111/660
#> Fitting model 112/660
#> Fitting model 113/660
#> Fitting model 114/660
#> Fitting model 115/660
#> Fitting model 116/660
#> Fitting model 117/660
#> Fitting model 118/660
#> Fitting model 119/660
#> Fitting model 120/660
#> Fitting model 121/660
#> Fitting model 122/660
#> Fitting model 123/660
#> Fitting model 124/660
#> Fitting model 125/660
#> Fitting model 126/660
#> Fitting model 127/660
#> Fitting model 128/660
#> Fitting model 129/660
#> Fitting model 130/660
#> Fitting model 131/660
#> Fitting model 132/660
#> Fitting model 133/660
#> Fitting model 134/660
#> Fitting model 135/660
#> Fitting model 136/660
#> Fitting model 137/660
#> Fitting model 138/660
#> Fitting model 139/660
#> Fitting model 140/660
#> Fitting model 141/660
#> Fitting model 142/660
#> Fitting model 143/660
#> Fitting model 144/660
#> Fitting model 145/660
#> Fitting model 146/660
#> Fitting model 147/660
#> Fitting model 148/660
#> Fitting model 149/660
#> Fitting model 150/660
#> Fitting model 151/660
#> Fitting model 152/660
#> Fitting model 153/660
#> Fitting model 154/660
#> Fitting model 155/660
#> Fitting model 156/660
#> Fitting model 157/660
#> Fitting model 158/660
#> Fitting model 159/660
#> Fitting model 160/660
#> Fitting model 161/660
#> Fitting model 162/660
#> Fitting model 163/660
#> Fitting model 164/660
#> Fitting model 165/660
#> Fitting model 166/660
#> Fitting model 167/660
#> Fitting model 168/660
#> Fitting model 169/660
#> Fitting model 170/660
#> Fitting model 171/660
#> Fitting model 172/660
#> Fitting model 173/660
#> Fitting model 174/660
#> Fitting model 175/660
#> Fitting model 176/660
#> Fitting model 177/660
#> Fitting model 178/660
#> Fitting model 179/660
#> Fitting model 180/660
#> Fitting model 181/660
#> Fitting model 182/660
#> Fitting model 183/660
#> Fitting model 184/660
#> Fitting model 185/660
#> Fitting model 186/660
#> Fitting model 187/660
#> Fitting model 188/660
#> Fitting model 189/660
#> Fitting model 190/660
#> Fitting model 191/660
#> Fitting model 192/660
#> Fitting model 193/660
#> Fitting model 194/660
#> Fitting model 195/660
#> Fitting model 196/660
#> Fitting model 197/660
#> Fitting model 198/660
#> Fitting model 199/660
#> Fitting model 200/660
#> Fitting model 201/660
#> Fitting model 202/660
#> Fitting model 203/660
#> Fitting model 204/660
#> Fitting model 205/660
#> Fitting model 206/660
#> Fitting model 207/660
#> Fitting model 208/660
#> Fitting model 209/660
#> Fitting model 210/660
#> Fitting model 211/660
#> Fitting model 212/660
#> Fitting model 213/660
#> Fitting model 214/660
#> Fitting model 215/660
#> Fitting model 216/660
#> Fitting model 217/660
#> Fitting model 218/660
#> Fitting model 219/660
#> Fitting model 220/660
#> Fitting model 221/660
#> Fitting model 222/660
#> Fitting model 223/660
#> Fitting model 224/660
#> Fitting model 225/660
#> Fitting model 226/660
#> Fitting model 227/660
#> Fitting model 228/660
#> Fitting model 229/660
#> Fitting model 230/660
#> Fitting model 231/660
#> Fitting model 232/660
#> Fitting model 233/660
#> Fitting model 234/660
#> Fitting model 235/660
#> Fitting model 236/660
#> Fitting model 237/660
#> Fitting model 238/660
#> Fitting model 239/660
#> Fitting model 240/660
#> Fitting model 241/660
#> Fitting model 242/660
#> Fitting model 243/660
#> Fitting model 244/660
#> Fitting model 245/660
#> Fitting model 246/660
#> Fitting model 247/660
#> Fitting model 248/660
#> Fitting model 249/660
#> Fitting model 250/660
#> Fitting model 251/660
#> Fitting model 252/660
#> Fitting model 253/660
#> Fitting model 254/660
#> Fitting model 255/660
#> Fitting model 256/660
#> Fitting model 257/660
#> Fitting model 258/660
#> Fitting model 259/660
#> Fitting model 260/660
#> Fitting model 261/660
#> Fitting model 262/660
#> Fitting model 263/660
#> Fitting model 264/660
#> Fitting model 265/660
#> Fitting model 266/660
#> Fitting model 267/660
#> Fitting model 268/660
#> Fitting model 269/660
#> Fitting model 270/660
#> Fitting model 271/660
#> Fitting model 272/660
#> Fitting model 273/660
#> Fitting model 274/660
#> Fitting model 275/660
#> Fitting model 276/660
#> Fitting model 277/660
#> Fitting model 278/660
#> Fitting model 279/660
#> Fitting model 280/660
#> Fitting model 281/660
#> Fitting model 282/660
#> Fitting model 283/660
#> Fitting model 284/660
#> Fitting model 285/660
#> Fitting model 286/660
#> Fitting model 287/660
#> Fitting model 288/660
#> Fitting model 289/660
#> Fitting model 290/660
#> Fitting model 291/660
#> Fitting model 292/660
#> Fitting model 293/660
#> Fitting model 294/660
#> Fitting model 295/660
#> Fitting model 296/660
#> Fitting model 297/660
#> Fitting model 298/660
#> Fitting model 299/660
#> Fitting model 300/660
#> Fitting model 301/660
#> Fitting model 302/660
#> Fitting model 303/660
#> Fitting model 304/660
#> Fitting model 305/660
#> Fitting model 306/660
#> Fitting model 307/660
#> Fitting model 308/660
#> Fitting model 309/660
#> Fitting model 310/660
#> Fitting model 311/660
#> Fitting model 312/660
#> Fitting model 313/660
#> Fitting model 314/660
#> Fitting model 315/660
#> Fitting model 316/660
#> Fitting model 317/660
#> Fitting model 318/660
#> Fitting model 319/660
#> Fitting model 320/660
#> Fitting model 321/660
#> Fitting model 322/660
#> Fitting model 323/660
#> Fitting model 324/660
#> Fitting model 325/660
#> Fitting model 326/660
#> Fitting model 327/660
#> Fitting model 328/660
#> Fitting model 329/660
#> Fitting model 330/660
#> Fitting model 331/660
#> Fitting model 332/660
#> Fitting model 333/660
#> Fitting model 334/660
#> Fitting model 335/660
#> Fitting model 336/660
#> Fitting model 337/660
#> Fitting model 338/660
#> Fitting model 339/660
#> Fitting model 340/660
#> Fitting model 341/660
#> Fitting model 342/660
#> Fitting model 343/660
#> Fitting model 344/660
#> Fitting model 345/660
#> Fitting model 346/660
#> Fitting model 347/660
#> Fitting model 348/660
#> Fitting model 349/660
#> Fitting model 350/660
#> Fitting model 351/660
#> Fitting model 352/660
#> Fitting model 353/660
#> Fitting model 354/660
#> Fitting model 355/660
#> Fitting model 356/660
#> Fitting model 357/660
#> Fitting model 358/660
#> Fitting model 359/660
#> Fitting model 360/660
#> Fitting model 361/660
#> Fitting model 362/660
#> Fitting model 363/660
#> Fitting model 364/660
#> Fitting model 365/660
#> Fitting model 366/660
#> Fitting model 367/660
#> Fitting model 368/660
#> Fitting model 369/660
#> Fitting model 370/660
#> Fitting model 371/660
#> Fitting model 372/660
#> Fitting model 373/660
#> Fitting model 374/660
#> Fitting model 375/660
#> Fitting model 376/660
#> Fitting model 377/660
#> Fitting model 378/660
#> Fitting model 379/660
#> Fitting model 380/660
#> Fitting model 381/660
#> Fitting model 382/660
#> Fitting model 383/660
#> Fitting model 384/660
#> Fitting model 385/660
#> Fitting model 386/660
#> Fitting model 387/660
#> Fitting model 388/660
#> Fitting model 389/660
#> Fitting model 390/660
#> Fitting model 391/660
#> Fitting model 392/660
#> Fitting model 393/660
#> Fitting model 394/660
#> Fitting model 395/660
#> Fitting model 396/660
#> Fitting model 397/660
#> Fitting model 398/660
#> Fitting model 399/660
#> Fitting model 400/660
#> Fitting model 401/660
#> Fitting model 402/660
#> Fitting model 403/660
#> Fitting model 404/660
#> Fitting model 405/660
#> Fitting model 406/660
#> Fitting model 407/660
#> Fitting model 408/660
#> Fitting model 409/660
#> Fitting model 410/660
#> Fitting model 411/660
#> Fitting model 412/660
#> Fitting model 413/660
#> Fitting model 414/660
#> Fitting model 415/660
#> Fitting model 416/660
#> Fitting model 417/660
#> Fitting model 418/660
#> Fitting model 419/660
#> Fitting model 420/660
#> Fitting model 421/660
#> Fitting model 422/660
#> Fitting model 423/660
#> Fitting model 424/660
#> Fitting model 425/660
#> Fitting model 426/660
#> Fitting model 427/660
#> Fitting model 428/660
#> Fitting model 429/660
#> Fitting model 430/660
#> Fitting model 431/660
#> Fitting model 432/660
#> Fitting model 433/660
#> Fitting model 434/660
#> Fitting model 435/660
#> Fitting model 436/660
#> Fitting model 437/660
#> Fitting model 438/660
#> Fitting model 439/660
#> Fitting model 440/660
#> Fitting model 441/660
#> Fitting model 442/660
#> Fitting model 443/660
#> Fitting model 444/660
#> Fitting model 445/660
#> Fitting model 446/660
#> Fitting model 447/660
#> Fitting model 448/660
#> Fitting model 449/660
#> Fitting model 450/660
#> Fitting model 451/660
#> Fitting model 452/660
#> Fitting model 453/660
#> Fitting model 454/660
#> Fitting model 455/660
#> Fitting model 456/660
#> Fitting model 457/660
#> Fitting model 458/660
#> Fitting model 459/660
#> Fitting model 460/660
#> Fitting model 461/660
#> Fitting model 462/660
#> Fitting model 463/660
#> Fitting model 464/660
#> Fitting model 465/660
#> Fitting model 466/660
#> Fitting model 467/660
#> Fitting model 468/660
#> Fitting model 469/660
#> Fitting model 470/660
#> Fitting model 471/660
#> Fitting model 472/660
#> Fitting model 473/660
#> Fitting model 474/660
#> Fitting model 475/660
#> Fitting model 476/660
#> Fitting model 477/660
#> Fitting model 478/660
#> Fitting model 479/660
#> Fitting model 480/660
#> Fitting model 481/660
#> Fitting model 482/660
#> Fitting model 483/660
#> Fitting model 484/660
#> Fitting model 485/660
#> Fitting model 486/660
#> Fitting model 487/660
#> Fitting model 488/660
#> Fitting model 489/660
#> Fitting model 490/660
#> Fitting model 491/660
#> Fitting model 492/660
#> Fitting model 493/660
#> Fitting model 494/660
#> Fitting model 495/660
#> Fitting model 496/660
#> Fitting model 497/660
#> Fitting model 498/660
#> Fitting model 499/660
#> Fitting model 500/660
#> Fitting model 501/660
#> Fitting model 502/660
#> Fitting model 503/660
#> Fitting model 504/660
#> Fitting model 505/660
#> Fitting model 506/660
#> Fitting model 507/660
#> Fitting model 508/660
#> Fitting model 509/660
#> Fitting model 510/660
#> Fitting model 511/660
#> Fitting model 512/660
#> Fitting model 513/660
#> Fitting model 514/660
#> Fitting model 515/660
#> Fitting model 516/660
#> Fitting model 517/660
#> Fitting model 518/660
#> Fitting model 519/660
#> Fitting model 520/660
#> Fitting model 521/660
#> Fitting model 522/660
#> Fitting model 523/660
#> Fitting model 524/660
#> Fitting model 525/660
#> Fitting model 526/660
#> Fitting model 527/660
#> Fitting model 528/660
#> Fitting model 529/660
#> Fitting model 530/660
#> Fitting model 531/660
#> Fitting model 532/660
#> Fitting model 533/660
#> Fitting model 534/660
#> Fitting model 535/660
#> Fitting model 536/660
#> Fitting model 537/660
#> Fitting model 538/660
#> Fitting model 539/660
#> Fitting model 540/660
#> Fitting model 541/660
#> Fitting model 542/660
#> Fitting model 543/660
#> Fitting model 544/660
#> Fitting model 545/660
#> Fitting model 546/660
#> Fitting model 547/660
#> Fitting model 548/660
#> Fitting model 549/660
#> Fitting model 550/660
#> Fitting model 551/660
#> Fitting model 552/660
#> Fitting model 553/660
#> Fitting model 554/660
#> Fitting model 555/660
#> Fitting model 556/660
#> Fitting model 557/660
#> Fitting model 558/660
#> Fitting model 559/660
#> Fitting model 560/660
#> Fitting model 561/660
#> Fitting model 562/660
#> Fitting model 563/660
#> Fitting model 564/660
#> Fitting model 565/660
#> Fitting model 566/660
#> Fitting model 567/660
#> Fitting model 568/660
#> Fitting model 569/660
#> Fitting model 570/660
#> Fitting model 571/660
#> Fitting model 572/660
#> Fitting model 573/660
#> Fitting model 574/660
#> Fitting model 575/660
#> Fitting model 576/660
#> Fitting model 577/660
#> Fitting model 578/660
#> Fitting model 579/660
#> Fitting model 580/660
#> Fitting model 581/660
#> Fitting model 582/660
#> Fitting model 583/660
#> Fitting model 584/660
#> Fitting model 585/660
#> Fitting model 586/660
#> Fitting model 587/660
#> Fitting model 588/660
#> Fitting model 589/660
#> Fitting model 590/660
#> Fitting model 591/660
#> Fitting model 592/660
#> Fitting model 593/660
#> Fitting model 594/660
#> Fitting model 595/660
#> Fitting model 596/660
#> Fitting model 597/660
#> Fitting model 598/660
#> Fitting model 599/660
#> Fitting model 600/660
#> Fitting model 601/660
#> Fitting model 602/660
#> Fitting model 603/660
#> Fitting model 604/660
#> Fitting model 605/660
#> Fitting model 606/660
#> Fitting model 607/660
#> Fitting model 608/660
#> Fitting model 609/660
#> Fitting model 610/660
#> Fitting model 611/660
#> Fitting model 612/660
#> Fitting model 613/660
#> Fitting model 614/660
#> Fitting model 615/660
#> Fitting model 616/660
#> Fitting model 617/660
#> Fitting model 618/660
#> Fitting model 619/660
#> Fitting model 620/660
#> Fitting model 621/660
#> Fitting model 622/660
#> Fitting model 623/660
#> Fitting model 624/660
#> Fitting model 625/660
#> Fitting model 626/660
#> Fitting model 627/660
#> Fitting model 628/660
#> Fitting model 629/660
#> Fitting model 630/660
#> Fitting model 631/660
#> Fitting model 632/660
#> Fitting model 633/660
#> Fitting model 634/660
#> Fitting model 635/660
#> Fitting model 636/660
#> Fitting model 637/660
#> Fitting model 638/660
#> Fitting model 639/660
#> Fitting model 640/660
#> Fitting model 641/660
#> Fitting model 642/660
#> Fitting model 643/660
#> Fitting model 644/660
#> Fitting model 645/660
#> Fitting model 646/660
#> Fitting model 647/660
#> Fitting model 648/660
#> Fitting model 649/660
#> Fitting model 650/660
#> Fitting model 651/660
#> Fitting model 652/660
#> Fitting model 653/660
#> Fitting model 654/660
#> Fitting model 655/660
#> Fitting model 656/660
#> Fitting model 657/660
#> Fitting model 658/660
#> Fitting model 659/660
#> Fitting model 660/660
  
calculate_interval(classifiers,
  by_set = FALSE,
  type = "sd",
  interval = 1)
#> # A tibble: 22 × 4
#>    names                                         .mean .lower .upper
#>    <chr>                                         <dbl>  <dbl>  <dbl>
#>  1 catch22_CO_Embed2_Dist_tau_d_expfit_meandiff 0.327  0.304  0.349 
#>  2 catch22_CO_FirstMin_ac                       0.318  0.294  0.342 
#>  3 catch22_CO_HistogramAMI_even_2_5             0.358  0.308  0.408 
#>  4 catch22_CO_f1ecac                            0.344  0.332  0.357 
#>  5 catch22_CO_trev_1_num                        0.108  0.0780 0.138 
#>  6 catch22_DN_HistogramMode_10                  0.0644 0.0448 0.0841
#>  7 catch22_DN_HistogramMode_5                   0.0689 0.0582 0.0796
#>  8 catch22_DN_OutlierInclude_n_001_mdrmd        0.0637 0.0475 0.0799
#>  9 catch22_DN_OutlierInclude_p_001_mdrmd        0.08   0.0610 0.0990
#> 10 catch22_FC_LocalSimple_mean1_tauresrat       0.38   0.342  0.418 
#> # … with 12 more rows
# }
```
