# Conduct statistical testing on time-series feature classification performance to identify top features or compare entire sets

Conduct statistical testing on time-series feature classification
performance to identify top features or compare entire sets

## Usage

``` r
compare_features(
  data,
  metric = c("accuracy", "precision", "recall", "f1"),
  by_set = TRUE,
  hypothesis = c("null", "pairwise"),
  p_adj = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr")
)
```

## Arguments

- data:

  `list` object containing the classification outputs produce by
  `tsfeature_classifier`

- metric:

  `character` denoting the classification performance metric to use in
  statistical testing. Can be one of `"accuracy"`, `"precision"`,
  `"recall"`, `"f1"`. Defaults to `"accuracy"`

- by_set:

  `Boolean` specifying whether you want to compare feature sets (if
  `TRUE`) or individual features (if `FALSE`). Defaults to `TRUE` but
  this is contingent on whether you computed by set or not in
  `tsfeature_classifier`

- hypothesis:

  `character` denoting whether p-values should be calculated for each
  feature set or feature (depending on `by_set` argument) individually
  relative to the null if `use_null = TRUE` in `tsfeature_classifier`
  through `"null"`, or whether pairwise comparisons between each set or
  feature should be conducted on main model fits only through
  `"pairwise"`. Defaults to `"null"`

- p_adj:

  `character` denoting the adjustment made to p-values for multiple
  comparisons. Should be a valid argument to
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Defaults
  to `"none"` for no adjustment. `"holm"` is recommended as a starting
  point for adjustments

## Value

`data.frame` containing the results

## References

Henderson, T., Bryant, A. G., and Fulcher, B. D. Never a Dull Moment:
Distributional Properties as a Baseline for Time-Series Classification.
27th Pacific-Asia Conference on Knowledge Discovery and Data Mining,
(2023).

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
  
compare_features(classifiers,
  by_set = FALSE,
  hypothesis = "pairwise") 
#> Calculating comparison 1/231
#> Calculating comparison 2/231
#> Calculating comparison 3/231
#> Calculating comparison 4/231
#> Calculating comparison 5/231
#> Calculating comparison 6/231
#> Calculating comparison 7/231
#> Calculating comparison 8/231
#> Calculating comparison 9/231
#> Calculating comparison 10/231
#> Calculating comparison 11/231
#> Calculating comparison 12/231
#> Calculating comparison 13/231
#> Calculating comparison 14/231
#> Calculating comparison 15/231
#> Calculating comparison 16/231
#> Calculating comparison 17/231
#> Calculating comparison 18/231
#> Calculating comparison 19/231
#> Calculating comparison 20/231
#> Calculating comparison 21/231
#> Calculating comparison 22/231
#> Calculating comparison 23/231
#> Calculating comparison 24/231
#> Calculating comparison 25/231
#> Calculating comparison 26/231
#> Calculating comparison 27/231
#> Calculating comparison 28/231
#> Calculating comparison 29/231
#> Calculating comparison 30/231
#> Calculating comparison 31/231
#> Calculating comparison 32/231
#> Calculating comparison 33/231
#> Calculating comparison 34/231
#> Calculating comparison 35/231
#> Calculating comparison 36/231
#> Calculating comparison 37/231
#> Calculating comparison 38/231
#> Calculating comparison 39/231
#> Calculating comparison 40/231
#> Calculating comparison 41/231
#> Calculating comparison 42/231
#> Calculating comparison 43/231
#> Calculating comparison 44/231
#> Calculating comparison 45/231
#> Calculating comparison 46/231
#> Calculating comparison 47/231
#> Calculating comparison 48/231
#> Calculating comparison 49/231
#> Calculating comparison 50/231
#> Calculating comparison 51/231
#> Calculating comparison 52/231
#> Calculating comparison 53/231
#> Calculating comparison 54/231
#> Calculating comparison 55/231
#> Calculating comparison 56/231
#> Calculating comparison 57/231
#> Calculating comparison 58/231
#> Calculating comparison 59/231
#> Calculating comparison 60/231
#> Calculating comparison 61/231
#> Calculating comparison 62/231
#> Calculating comparison 63/231
#> Calculating comparison 64/231
#> Calculating comparison 65/231
#> Calculating comparison 66/231
#> Calculating comparison 67/231
#> Calculating comparison 68/231
#> Calculating comparison 69/231
#> Calculating comparison 70/231
#> Calculating comparison 71/231
#> Calculating comparison 72/231
#> Calculating comparison 73/231
#> Calculating comparison 74/231
#> Calculating comparison 75/231
#> Calculating comparison 76/231
#> Calculating comparison 77/231
#> Calculating comparison 78/231
#> Calculating comparison 79/231
#> Calculating comparison 80/231
#> Calculating comparison 81/231
#> Calculating comparison 82/231
#> Calculating comparison 83/231
#> Calculating comparison 84/231
#> Calculating comparison 85/231
#> Calculating comparison 86/231
#> Calculating comparison 87/231
#> Calculating comparison 88/231
#> Calculating comparison 89/231
#> Calculating comparison 90/231
#> Calculating comparison 91/231
#> Calculating comparison 92/231
#> Calculating comparison 93/231
#> Calculating comparison 94/231
#> Calculating comparison 95/231
#> Calculating comparison 96/231
#> Calculating comparison 97/231
#> Calculating comparison 98/231
#> Calculating comparison 99/231
#> Calculating comparison 100/231
#> Calculating comparison 101/231
#> Calculating comparison 102/231
#> Calculating comparison 103/231
#> Calculating comparison 104/231
#> Calculating comparison 105/231
#> Calculating comparison 106/231
#> Calculating comparison 107/231
#> Calculating comparison 108/231
#> Calculating comparison 109/231
#> Calculating comparison 110/231
#> Calculating comparison 111/231
#> Calculating comparison 112/231
#> Calculating comparison 113/231
#> Calculating comparison 114/231
#> Calculating comparison 115/231
#> Calculating comparison 116/231
#> Calculating comparison 117/231
#> Calculating comparison 118/231
#> Calculating comparison 119/231
#> Calculating comparison 120/231
#> Calculating comparison 121/231
#> Calculating comparison 122/231
#> Calculating comparison 123/231
#> Calculating comparison 124/231
#> Calculating comparison 125/231
#> Calculating comparison 126/231
#> Calculating comparison 127/231
#> Calculating comparison 128/231
#> Calculating comparison 129/231
#> Calculating comparison 130/231
#> Calculating comparison 131/231
#> Calculating comparison 132/231
#> Calculating comparison 133/231
#> Calculating comparison 134/231
#> Calculating comparison 135/231
#> Calculating comparison 136/231
#> Calculating comparison 137/231
#> Calculating comparison 138/231
#> Calculating comparison 139/231
#> Calculating comparison 140/231
#> Calculating comparison 141/231
#> Calculating comparison 142/231
#> Calculating comparison 143/231
#> Calculating comparison 144/231
#> Calculating comparison 145/231
#> Calculating comparison 146/231
#> Calculating comparison 147/231
#> Calculating comparison 148/231
#> Calculating comparison 149/231
#> Calculating comparison 150/231
#> Calculating comparison 151/231
#> Calculating comparison 152/231
#> Calculating comparison 153/231
#> Calculating comparison 154/231
#> Calculating comparison 155/231
#> Calculating comparison 156/231
#> Calculating comparison 157/231
#> Calculating comparison 158/231
#> Calculating comparison 159/231
#> Calculating comparison 160/231
#> Calculating comparison 161/231
#> Calculating comparison 162/231
#> Calculating comparison 163/231
#> Calculating comparison 164/231
#> Calculating comparison 165/231
#> Calculating comparison 166/231
#> Calculating comparison 167/231
#> Calculating comparison 168/231
#> Calculating comparison 169/231
#> Calculating comparison 170/231
#> Calculating comparison 171/231
#> Calculating comparison 172/231
#> Calculating comparison 173/231
#> Calculating comparison 174/231
#> Calculating comparison 175/231
#> Calculating comparison 176/231
#> Calculating comparison 177/231
#> Calculating comparison 178/231
#> Calculating comparison 179/231
#> Calculating comparison 180/231
#> Calculating comparison 181/231
#> Calculating comparison 182/231
#> Calculating comparison 183/231
#> Calculating comparison 184/231
#> Calculating comparison 185/231
#> Calculating comparison 186/231
#> Calculating comparison 187/231
#> Calculating comparison 188/231
#> Calculating comparison 189/231
#> Calculating comparison 190/231
#> Calculating comparison 191/231
#> Calculating comparison 192/231
#> Calculating comparison 193/231
#> Calculating comparison 194/231
#> Calculating comparison 195/231
#> Calculating comparison 196/231
#> Calculating comparison 197/231
#> Calculating comparison 198/231
#> Calculating comparison 199/231
#> Calculating comparison 200/231
#> Calculating comparison 201/231
#> Calculating comparison 202/231
#> Calculating comparison 203/231
#> Calculating comparison 204/231
#> Calculating comparison 205/231
#> Calculating comparison 206/231
#> Calculating comparison 207/231
#> Calculating comparison 208/231
#> Calculating comparison 209/231
#> Calculating comparison 210/231
#> Calculating comparison 211/231
#> Calculating comparison 212/231
#> Calculating comparison 213/231
#> Calculating comparison 214/231
#> Calculating comparison 215/231
#> Calculating comparison 216/231
#> Calculating comparison 217/231
#> Calculating comparison 218/231
#> Calculating comparison 219/231
#> Calculating comparison 220/231
#> Calculating comparison 221/231
#> Calculating comparison 222/231
#> Calculating comparison 223/231
#> Calculating comparison 224/231
#> Calculating comparison 225/231
#> Calculating comparison 226/231
#> Calculating comparison 227/231
#> Calculating comparison 228/231
#> Calculating comparison 229/231
#> Calculating comparison 230/231
#> Calculating comparison 231/231
#>                                                                                                 hypothesis
#> 1                                   catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_CO_FirstMin_ac
#> 2                         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_CO_HistogramAMI_even_2_5
#> 3                                        catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_CO_f1ecac
#> 4                                    catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_CO_trev_1_num
#> 5                              catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_DN_HistogramMode_10
#> 6                               catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_DN_HistogramMode_5
#> 7                    catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_DN_OutlierInclude_n_001_mdrmd
#> 8                    catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_DN_OutlierInclude_p_001_mdrmd
#> 9                   catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_FC_LocalSimple_mean1_tauresrat
#> 10                     catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_FC_LocalSimple_mean3_stderr
#> 11         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 12                            catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_MD_hrv_classic_pnn40
#> 13                       catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_PD_PeriodicityWang_th0_01
#> 14                catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SB_BinaryStats_diff_longstretch0
#> 15                catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SB_BinaryStats_mean_longstretch1
#> 16                       catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SB_MotifThree_quantile_hh
#> 17              catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 18          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 19     catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 20                catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SP_Summaries_welch_rect_area_5_1
#> 21                catch22_CO_Embed2_Dist_tau_d_expfit_meandiff != catch22_SP_Summaries_welch_rect_centroid
#> 22                                              catch22_CO_FirstMin_ac != catch22_CO_HistogramAMI_even_2_5
#> 23                                                             catch22_CO_FirstMin_ac != catch22_CO_f1ecac
#> 24                                                         catch22_CO_FirstMin_ac != catch22_CO_trev_1_num
#> 25                                                   catch22_CO_FirstMin_ac != catch22_DN_HistogramMode_10
#> 26                                                    catch22_CO_FirstMin_ac != catch22_DN_HistogramMode_5
#> 27                                         catch22_CO_FirstMin_ac != catch22_DN_OutlierInclude_n_001_mdrmd
#> 28                                         catch22_CO_FirstMin_ac != catch22_DN_OutlierInclude_p_001_mdrmd
#> 29                                        catch22_CO_FirstMin_ac != catch22_FC_LocalSimple_mean1_tauresrat
#> 30                                           catch22_CO_FirstMin_ac != catch22_FC_LocalSimple_mean3_stderr
#> 31                               catch22_CO_FirstMin_ac != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 32                                                  catch22_CO_FirstMin_ac != catch22_MD_hrv_classic_pnn40
#> 33                                             catch22_CO_FirstMin_ac != catch22_PD_PeriodicityWang_th0_01
#> 34                                      catch22_CO_FirstMin_ac != catch22_SB_BinaryStats_diff_longstretch0
#> 35                                      catch22_CO_FirstMin_ac != catch22_SB_BinaryStats_mean_longstretch1
#> 36                                             catch22_CO_FirstMin_ac != catch22_SB_MotifThree_quantile_hh
#> 37                                    catch22_CO_FirstMin_ac != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 38                                catch22_CO_FirstMin_ac != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 39                           catch22_CO_FirstMin_ac != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 40                                      catch22_CO_FirstMin_ac != catch22_SP_Summaries_welch_rect_area_5_1
#> 41                                      catch22_CO_FirstMin_ac != catch22_SP_Summaries_welch_rect_centroid
#> 42                                                   catch22_CO_HistogramAMI_even_2_5 != catch22_CO_f1ecac
#> 43                                               catch22_CO_HistogramAMI_even_2_5 != catch22_CO_trev_1_num
#> 44                                         catch22_CO_HistogramAMI_even_2_5 != catch22_DN_HistogramMode_10
#> 45                                          catch22_CO_HistogramAMI_even_2_5 != catch22_DN_HistogramMode_5
#> 46                               catch22_CO_HistogramAMI_even_2_5 != catch22_DN_OutlierInclude_n_001_mdrmd
#> 47                               catch22_CO_HistogramAMI_even_2_5 != catch22_DN_OutlierInclude_p_001_mdrmd
#> 48                              catch22_CO_HistogramAMI_even_2_5 != catch22_FC_LocalSimple_mean1_tauresrat
#> 49                                 catch22_CO_HistogramAMI_even_2_5 != catch22_FC_LocalSimple_mean3_stderr
#> 50                     catch22_CO_HistogramAMI_even_2_5 != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 51                                        catch22_CO_HistogramAMI_even_2_5 != catch22_MD_hrv_classic_pnn40
#> 52                                   catch22_CO_HistogramAMI_even_2_5 != catch22_PD_PeriodicityWang_th0_01
#> 53                            catch22_CO_HistogramAMI_even_2_5 != catch22_SB_BinaryStats_diff_longstretch0
#> 54                            catch22_CO_HistogramAMI_even_2_5 != catch22_SB_BinaryStats_mean_longstretch1
#> 55                                   catch22_CO_HistogramAMI_even_2_5 != catch22_SB_MotifThree_quantile_hh
#> 56                          catch22_CO_HistogramAMI_even_2_5 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 57                      catch22_CO_HistogramAMI_even_2_5 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 58                 catch22_CO_HistogramAMI_even_2_5 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 59                            catch22_CO_HistogramAMI_even_2_5 != catch22_SP_Summaries_welch_rect_area_5_1
#> 60                            catch22_CO_HistogramAMI_even_2_5 != catch22_SP_Summaries_welch_rect_centroid
#> 61                                                              catch22_CO_f1ecac != catch22_CO_trev_1_num
#> 62                                                        catch22_CO_f1ecac != catch22_DN_HistogramMode_10
#> 63                                                         catch22_CO_f1ecac != catch22_DN_HistogramMode_5
#> 64                                              catch22_CO_f1ecac != catch22_DN_OutlierInclude_n_001_mdrmd
#> 65                                              catch22_CO_f1ecac != catch22_DN_OutlierInclude_p_001_mdrmd
#> 66                                             catch22_CO_f1ecac != catch22_FC_LocalSimple_mean1_tauresrat
#> 67                                                catch22_CO_f1ecac != catch22_FC_LocalSimple_mean3_stderr
#> 68                                    catch22_CO_f1ecac != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 69                                                       catch22_CO_f1ecac != catch22_MD_hrv_classic_pnn40
#> 70                                                  catch22_CO_f1ecac != catch22_PD_PeriodicityWang_th0_01
#> 71                                           catch22_CO_f1ecac != catch22_SB_BinaryStats_diff_longstretch0
#> 72                                           catch22_CO_f1ecac != catch22_SB_BinaryStats_mean_longstretch1
#> 73                                                  catch22_CO_f1ecac != catch22_SB_MotifThree_quantile_hh
#> 74                                         catch22_CO_f1ecac != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 75                                     catch22_CO_f1ecac != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 76                                catch22_CO_f1ecac != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 77                                           catch22_CO_f1ecac != catch22_SP_Summaries_welch_rect_area_5_1
#> 78                                           catch22_CO_f1ecac != catch22_SP_Summaries_welch_rect_centroid
#> 79                                                    catch22_CO_trev_1_num != catch22_DN_HistogramMode_10
#> 80                                                     catch22_CO_trev_1_num != catch22_DN_HistogramMode_5
#> 81                                          catch22_CO_trev_1_num != catch22_DN_OutlierInclude_n_001_mdrmd
#> 82                                          catch22_CO_trev_1_num != catch22_DN_OutlierInclude_p_001_mdrmd
#> 83                                         catch22_CO_trev_1_num != catch22_FC_LocalSimple_mean1_tauresrat
#> 84                                            catch22_CO_trev_1_num != catch22_FC_LocalSimple_mean3_stderr
#> 85                                catch22_CO_trev_1_num != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 86                                                   catch22_CO_trev_1_num != catch22_MD_hrv_classic_pnn40
#> 87                                              catch22_CO_trev_1_num != catch22_PD_PeriodicityWang_th0_01
#> 88                                       catch22_CO_trev_1_num != catch22_SB_BinaryStats_diff_longstretch0
#> 89                                       catch22_CO_trev_1_num != catch22_SB_BinaryStats_mean_longstretch1
#> 90                                              catch22_CO_trev_1_num != catch22_SB_MotifThree_quantile_hh
#> 91                                     catch22_CO_trev_1_num != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 92                                 catch22_CO_trev_1_num != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 93                            catch22_CO_trev_1_num != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 94                                       catch22_CO_trev_1_num != catch22_SP_Summaries_welch_rect_area_5_1
#> 95                                       catch22_CO_trev_1_num != catch22_SP_Summaries_welch_rect_centroid
#> 96                                               catch22_DN_HistogramMode_10 != catch22_DN_HistogramMode_5
#> 97                                    catch22_DN_HistogramMode_10 != catch22_DN_OutlierInclude_n_001_mdrmd
#> 98                                    catch22_DN_HistogramMode_10 != catch22_DN_OutlierInclude_p_001_mdrmd
#> 99                                   catch22_DN_HistogramMode_10 != catch22_FC_LocalSimple_mean1_tauresrat
#> 100                                     catch22_DN_HistogramMode_10 != catch22_FC_LocalSimple_mean3_stderr
#> 101                         catch22_DN_HistogramMode_10 != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 102                                            catch22_DN_HistogramMode_10 != catch22_MD_hrv_classic_pnn40
#> 103                                       catch22_DN_HistogramMode_10 != catch22_PD_PeriodicityWang_th0_01
#> 104                                catch22_DN_HistogramMode_10 != catch22_SB_BinaryStats_diff_longstretch0
#> 105                                catch22_DN_HistogramMode_10 != catch22_SB_BinaryStats_mean_longstretch1
#> 106                                       catch22_DN_HistogramMode_10 != catch22_SB_MotifThree_quantile_hh
#> 107                              catch22_DN_HistogramMode_10 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 108                          catch22_DN_HistogramMode_10 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 109                     catch22_DN_HistogramMode_10 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 110                                catch22_DN_HistogramMode_10 != catch22_SP_Summaries_welch_rect_area_5_1
#> 111                                catch22_DN_HistogramMode_10 != catch22_SP_Summaries_welch_rect_centroid
#> 112                                    catch22_DN_HistogramMode_5 != catch22_DN_OutlierInclude_n_001_mdrmd
#> 113                                    catch22_DN_HistogramMode_5 != catch22_DN_OutlierInclude_p_001_mdrmd
#> 114                                   catch22_DN_HistogramMode_5 != catch22_FC_LocalSimple_mean1_tauresrat
#> 115                                      catch22_DN_HistogramMode_5 != catch22_FC_LocalSimple_mean3_stderr
#> 116                          catch22_DN_HistogramMode_5 != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 117                                             catch22_DN_HistogramMode_5 != catch22_MD_hrv_classic_pnn40
#> 118                                        catch22_DN_HistogramMode_5 != catch22_PD_PeriodicityWang_th0_01
#> 119                                 catch22_DN_HistogramMode_5 != catch22_SB_BinaryStats_diff_longstretch0
#> 120                                 catch22_DN_HistogramMode_5 != catch22_SB_BinaryStats_mean_longstretch1
#> 121                                        catch22_DN_HistogramMode_5 != catch22_SB_MotifThree_quantile_hh
#> 122                               catch22_DN_HistogramMode_5 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 123                           catch22_DN_HistogramMode_5 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 124                      catch22_DN_HistogramMode_5 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 125                                 catch22_DN_HistogramMode_5 != catch22_SP_Summaries_welch_rect_area_5_1
#> 126                                 catch22_DN_HistogramMode_5 != catch22_SP_Summaries_welch_rect_centroid
#> 127                         catch22_DN_OutlierInclude_n_001_mdrmd != catch22_DN_OutlierInclude_p_001_mdrmd
#> 128                        catch22_DN_OutlierInclude_n_001_mdrmd != catch22_FC_LocalSimple_mean1_tauresrat
#> 129                           catch22_DN_OutlierInclude_n_001_mdrmd != catch22_FC_LocalSimple_mean3_stderr
#> 130               catch22_DN_OutlierInclude_n_001_mdrmd != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 131                                  catch22_DN_OutlierInclude_n_001_mdrmd != catch22_MD_hrv_classic_pnn40
#> 132                             catch22_DN_OutlierInclude_n_001_mdrmd != catch22_PD_PeriodicityWang_th0_01
#> 133                      catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SB_BinaryStats_diff_longstretch0
#> 134                      catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SB_BinaryStats_mean_longstretch1
#> 135                             catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SB_MotifThree_quantile_hh
#> 136                    catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 137                catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 138           catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 139                      catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SP_Summaries_welch_rect_area_5_1
#> 140                      catch22_DN_OutlierInclude_n_001_mdrmd != catch22_SP_Summaries_welch_rect_centroid
#> 141                        catch22_DN_OutlierInclude_p_001_mdrmd != catch22_FC_LocalSimple_mean1_tauresrat
#> 142                           catch22_DN_OutlierInclude_p_001_mdrmd != catch22_FC_LocalSimple_mean3_stderr
#> 143               catch22_DN_OutlierInclude_p_001_mdrmd != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 144                                  catch22_DN_OutlierInclude_p_001_mdrmd != catch22_MD_hrv_classic_pnn40
#> 145                             catch22_DN_OutlierInclude_p_001_mdrmd != catch22_PD_PeriodicityWang_th0_01
#> 146                      catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SB_BinaryStats_diff_longstretch0
#> 147                      catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SB_BinaryStats_mean_longstretch1
#> 148                             catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SB_MotifThree_quantile_hh
#> 149                    catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 150                catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 151           catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 152                      catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SP_Summaries_welch_rect_area_5_1
#> 153                      catch22_DN_OutlierInclude_p_001_mdrmd != catch22_SP_Summaries_welch_rect_centroid
#> 154                          catch22_FC_LocalSimple_mean1_tauresrat != catch22_FC_LocalSimple_mean3_stderr
#> 155              catch22_FC_LocalSimple_mean1_tauresrat != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 156                                 catch22_FC_LocalSimple_mean1_tauresrat != catch22_MD_hrv_classic_pnn40
#> 157                            catch22_FC_LocalSimple_mean1_tauresrat != catch22_PD_PeriodicityWang_th0_01
#> 158                     catch22_FC_LocalSimple_mean1_tauresrat != catch22_SB_BinaryStats_diff_longstretch0
#> 159                     catch22_FC_LocalSimple_mean1_tauresrat != catch22_SB_BinaryStats_mean_longstretch1
#> 160                            catch22_FC_LocalSimple_mean1_tauresrat != catch22_SB_MotifThree_quantile_hh
#> 161                   catch22_FC_LocalSimple_mean1_tauresrat != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 162               catch22_FC_LocalSimple_mean1_tauresrat != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 163          catch22_FC_LocalSimple_mean1_tauresrat != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 164                     catch22_FC_LocalSimple_mean1_tauresrat != catch22_SP_Summaries_welch_rect_area_5_1
#> 165                     catch22_FC_LocalSimple_mean1_tauresrat != catch22_SP_Summaries_welch_rect_centroid
#> 166                 catch22_FC_LocalSimple_mean3_stderr != catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 167                                    catch22_FC_LocalSimple_mean3_stderr != catch22_MD_hrv_classic_pnn40
#> 168                               catch22_FC_LocalSimple_mean3_stderr != catch22_PD_PeriodicityWang_th0_01
#> 169                        catch22_FC_LocalSimple_mean3_stderr != catch22_SB_BinaryStats_diff_longstretch0
#> 170                        catch22_FC_LocalSimple_mean3_stderr != catch22_SB_BinaryStats_mean_longstretch1
#> 171                               catch22_FC_LocalSimple_mean3_stderr != catch22_SB_MotifThree_quantile_hh
#> 172                      catch22_FC_LocalSimple_mean3_stderr != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 173                  catch22_FC_LocalSimple_mean3_stderr != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 174             catch22_FC_LocalSimple_mean3_stderr != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 175                        catch22_FC_LocalSimple_mean3_stderr != catch22_SP_Summaries_welch_rect_area_5_1
#> 176                        catch22_FC_LocalSimple_mean3_stderr != catch22_SP_Summaries_welch_rect_centroid
#> 177                        catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_MD_hrv_classic_pnn40
#> 178                   catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_PD_PeriodicityWang_th0_01
#> 179            catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SB_BinaryStats_diff_longstretch0
#> 180            catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SB_BinaryStats_mean_longstretch1
#> 181                   catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SB_MotifThree_quantile_hh
#> 182          catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 183      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 184 catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 185            catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SP_Summaries_welch_rect_area_5_1
#> 186            catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi != catch22_SP_Summaries_welch_rect_centroid
#> 187                                      catch22_MD_hrv_classic_pnn40 != catch22_PD_PeriodicityWang_th0_01
#> 188                               catch22_MD_hrv_classic_pnn40 != catch22_SB_BinaryStats_diff_longstretch0
#> 189                               catch22_MD_hrv_classic_pnn40 != catch22_SB_BinaryStats_mean_longstretch1
#> 190                                      catch22_MD_hrv_classic_pnn40 != catch22_SB_MotifThree_quantile_hh
#> 191                             catch22_MD_hrv_classic_pnn40 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 192                         catch22_MD_hrv_classic_pnn40 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 193                    catch22_MD_hrv_classic_pnn40 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 194                               catch22_MD_hrv_classic_pnn40 != catch22_SP_Summaries_welch_rect_area_5_1
#> 195                               catch22_MD_hrv_classic_pnn40 != catch22_SP_Summaries_welch_rect_centroid
#> 196                          catch22_PD_PeriodicityWang_th0_01 != catch22_SB_BinaryStats_diff_longstretch0
#> 197                          catch22_PD_PeriodicityWang_th0_01 != catch22_SB_BinaryStats_mean_longstretch1
#> 198                                 catch22_PD_PeriodicityWang_th0_01 != catch22_SB_MotifThree_quantile_hh
#> 199                        catch22_PD_PeriodicityWang_th0_01 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 200                    catch22_PD_PeriodicityWang_th0_01 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 201               catch22_PD_PeriodicityWang_th0_01 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 202                          catch22_PD_PeriodicityWang_th0_01 != catch22_SP_Summaries_welch_rect_area_5_1
#> 203                          catch22_PD_PeriodicityWang_th0_01 != catch22_SP_Summaries_welch_rect_centroid
#> 204                   catch22_SB_BinaryStats_diff_longstretch0 != catch22_SB_BinaryStats_mean_longstretch1
#> 205                          catch22_SB_BinaryStats_diff_longstretch0 != catch22_SB_MotifThree_quantile_hh
#> 206                 catch22_SB_BinaryStats_diff_longstretch0 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 207             catch22_SB_BinaryStats_diff_longstretch0 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 208        catch22_SB_BinaryStats_diff_longstretch0 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 209                   catch22_SB_BinaryStats_diff_longstretch0 != catch22_SP_Summaries_welch_rect_area_5_1
#> 210                   catch22_SB_BinaryStats_diff_longstretch0 != catch22_SP_Summaries_welch_rect_centroid
#> 211                          catch22_SB_BinaryStats_mean_longstretch1 != catch22_SB_MotifThree_quantile_hh
#> 212                 catch22_SB_BinaryStats_mean_longstretch1 != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 213             catch22_SB_BinaryStats_mean_longstretch1 != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 214        catch22_SB_BinaryStats_mean_longstretch1 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 215                   catch22_SB_BinaryStats_mean_longstretch1 != catch22_SP_Summaries_welch_rect_area_5_1
#> 216                   catch22_SB_BinaryStats_mean_longstretch1 != catch22_SP_Summaries_welch_rect_centroid
#> 217                        catch22_SB_MotifThree_quantile_hh != catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 218                    catch22_SB_MotifThree_quantile_hh != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 219               catch22_SB_MotifThree_quantile_hh != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 220                          catch22_SB_MotifThree_quantile_hh != catch22_SP_Summaries_welch_rect_area_5_1
#> 221                          catch22_SB_MotifThree_quantile_hh != catch22_SP_Summaries_welch_rect_centroid
#> 222           catch22_SB_TransitionMatrix_3ac_sumdiagcov != catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 223      catch22_SB_TransitionMatrix_3ac_sumdiagcov != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 224                 catch22_SB_TransitionMatrix_3ac_sumdiagcov != catch22_SP_Summaries_welch_rect_area_5_1
#> 225                 catch22_SB_TransitionMatrix_3ac_sumdiagcov != catch22_SP_Summaries_welch_rect_centroid
#> 226  catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 != catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 227             catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 != catch22_SP_Summaries_welch_rect_area_5_1
#> 228             catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 != catch22_SP_Summaries_welch_rect_centroid
#> 229        catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 != catch22_SP_Summaries_welch_rect_area_5_1
#> 230        catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 != catch22_SP_Summaries_welch_rect_centroid
#> 231                   catch22_SP_Summaries_welch_rect_area_5_1 != catch22_SP_Summaries_welch_rect_centroid
#>                                                 names_a
#> 1          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 2          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 3          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 4          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 5          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 6          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 7          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 8          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 9          catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 10         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 11         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 12         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 13         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 14         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 15         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 16         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 17         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 18         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 19         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 20         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 21         catch22_CO_Embed2_Dist_tau_d_expfit_meandiff
#> 22                               catch22_CO_FirstMin_ac
#> 23                               catch22_CO_FirstMin_ac
#> 24                               catch22_CO_FirstMin_ac
#> 25                               catch22_CO_FirstMin_ac
#> 26                               catch22_CO_FirstMin_ac
#> 27                               catch22_CO_FirstMin_ac
#> 28                               catch22_CO_FirstMin_ac
#> 29                               catch22_CO_FirstMin_ac
#> 30                               catch22_CO_FirstMin_ac
#> 31                               catch22_CO_FirstMin_ac
#> 32                               catch22_CO_FirstMin_ac
#> 33                               catch22_CO_FirstMin_ac
#> 34                               catch22_CO_FirstMin_ac
#> 35                               catch22_CO_FirstMin_ac
#> 36                               catch22_CO_FirstMin_ac
#> 37                               catch22_CO_FirstMin_ac
#> 38                               catch22_CO_FirstMin_ac
#> 39                               catch22_CO_FirstMin_ac
#> 40                               catch22_CO_FirstMin_ac
#> 41                               catch22_CO_FirstMin_ac
#> 42                     catch22_CO_HistogramAMI_even_2_5
#> 43                     catch22_CO_HistogramAMI_even_2_5
#> 44                     catch22_CO_HistogramAMI_even_2_5
#> 45                     catch22_CO_HistogramAMI_even_2_5
#> 46                     catch22_CO_HistogramAMI_even_2_5
#> 47                     catch22_CO_HistogramAMI_even_2_5
#> 48                     catch22_CO_HistogramAMI_even_2_5
#> 49                     catch22_CO_HistogramAMI_even_2_5
#> 50                     catch22_CO_HistogramAMI_even_2_5
#> 51                     catch22_CO_HistogramAMI_even_2_5
#> 52                     catch22_CO_HistogramAMI_even_2_5
#> 53                     catch22_CO_HistogramAMI_even_2_5
#> 54                     catch22_CO_HistogramAMI_even_2_5
#> 55                     catch22_CO_HistogramAMI_even_2_5
#> 56                     catch22_CO_HistogramAMI_even_2_5
#> 57                     catch22_CO_HistogramAMI_even_2_5
#> 58                     catch22_CO_HistogramAMI_even_2_5
#> 59                     catch22_CO_HistogramAMI_even_2_5
#> 60                     catch22_CO_HistogramAMI_even_2_5
#> 61                                    catch22_CO_f1ecac
#> 62                                    catch22_CO_f1ecac
#> 63                                    catch22_CO_f1ecac
#> 64                                    catch22_CO_f1ecac
#> 65                                    catch22_CO_f1ecac
#> 66                                    catch22_CO_f1ecac
#> 67                                    catch22_CO_f1ecac
#> 68                                    catch22_CO_f1ecac
#> 69                                    catch22_CO_f1ecac
#> 70                                    catch22_CO_f1ecac
#> 71                                    catch22_CO_f1ecac
#> 72                                    catch22_CO_f1ecac
#> 73                                    catch22_CO_f1ecac
#> 74                                    catch22_CO_f1ecac
#> 75                                    catch22_CO_f1ecac
#> 76                                    catch22_CO_f1ecac
#> 77                                    catch22_CO_f1ecac
#> 78                                    catch22_CO_f1ecac
#> 79                                catch22_CO_trev_1_num
#> 80                                catch22_CO_trev_1_num
#> 81                                catch22_CO_trev_1_num
#> 82                                catch22_CO_trev_1_num
#> 83                                catch22_CO_trev_1_num
#> 84                                catch22_CO_trev_1_num
#> 85                                catch22_CO_trev_1_num
#> 86                                catch22_CO_trev_1_num
#> 87                                catch22_CO_trev_1_num
#> 88                                catch22_CO_trev_1_num
#> 89                                catch22_CO_trev_1_num
#> 90                                catch22_CO_trev_1_num
#> 91                                catch22_CO_trev_1_num
#> 92                                catch22_CO_trev_1_num
#> 93                                catch22_CO_trev_1_num
#> 94                                catch22_CO_trev_1_num
#> 95                                catch22_CO_trev_1_num
#> 96                          catch22_DN_HistogramMode_10
#> 97                          catch22_DN_HistogramMode_10
#> 98                          catch22_DN_HistogramMode_10
#> 99                          catch22_DN_HistogramMode_10
#> 100                         catch22_DN_HistogramMode_10
#> 101                         catch22_DN_HistogramMode_10
#> 102                         catch22_DN_HistogramMode_10
#> 103                         catch22_DN_HistogramMode_10
#> 104                         catch22_DN_HistogramMode_10
#> 105                         catch22_DN_HistogramMode_10
#> 106                         catch22_DN_HistogramMode_10
#> 107                         catch22_DN_HistogramMode_10
#> 108                         catch22_DN_HistogramMode_10
#> 109                         catch22_DN_HistogramMode_10
#> 110                         catch22_DN_HistogramMode_10
#> 111                         catch22_DN_HistogramMode_10
#> 112                          catch22_DN_HistogramMode_5
#> 113                          catch22_DN_HistogramMode_5
#> 114                          catch22_DN_HistogramMode_5
#> 115                          catch22_DN_HistogramMode_5
#> 116                          catch22_DN_HistogramMode_5
#> 117                          catch22_DN_HistogramMode_5
#> 118                          catch22_DN_HistogramMode_5
#> 119                          catch22_DN_HistogramMode_5
#> 120                          catch22_DN_HistogramMode_5
#> 121                          catch22_DN_HistogramMode_5
#> 122                          catch22_DN_HistogramMode_5
#> 123                          catch22_DN_HistogramMode_5
#> 124                          catch22_DN_HistogramMode_5
#> 125                          catch22_DN_HistogramMode_5
#> 126                          catch22_DN_HistogramMode_5
#> 127               catch22_DN_OutlierInclude_n_001_mdrmd
#> 128               catch22_DN_OutlierInclude_n_001_mdrmd
#> 129               catch22_DN_OutlierInclude_n_001_mdrmd
#> 130               catch22_DN_OutlierInclude_n_001_mdrmd
#> 131               catch22_DN_OutlierInclude_n_001_mdrmd
#> 132               catch22_DN_OutlierInclude_n_001_mdrmd
#> 133               catch22_DN_OutlierInclude_n_001_mdrmd
#> 134               catch22_DN_OutlierInclude_n_001_mdrmd
#> 135               catch22_DN_OutlierInclude_n_001_mdrmd
#> 136               catch22_DN_OutlierInclude_n_001_mdrmd
#> 137               catch22_DN_OutlierInclude_n_001_mdrmd
#> 138               catch22_DN_OutlierInclude_n_001_mdrmd
#> 139               catch22_DN_OutlierInclude_n_001_mdrmd
#> 140               catch22_DN_OutlierInclude_n_001_mdrmd
#> 141               catch22_DN_OutlierInclude_p_001_mdrmd
#> 142               catch22_DN_OutlierInclude_p_001_mdrmd
#> 143               catch22_DN_OutlierInclude_p_001_mdrmd
#> 144               catch22_DN_OutlierInclude_p_001_mdrmd
#> 145               catch22_DN_OutlierInclude_p_001_mdrmd
#> 146               catch22_DN_OutlierInclude_p_001_mdrmd
#> 147               catch22_DN_OutlierInclude_p_001_mdrmd
#> 148               catch22_DN_OutlierInclude_p_001_mdrmd
#> 149               catch22_DN_OutlierInclude_p_001_mdrmd
#> 150               catch22_DN_OutlierInclude_p_001_mdrmd
#> 151               catch22_DN_OutlierInclude_p_001_mdrmd
#> 152               catch22_DN_OutlierInclude_p_001_mdrmd
#> 153               catch22_DN_OutlierInclude_p_001_mdrmd
#> 154              catch22_FC_LocalSimple_mean1_tauresrat
#> 155              catch22_FC_LocalSimple_mean1_tauresrat
#> 156              catch22_FC_LocalSimple_mean1_tauresrat
#> 157              catch22_FC_LocalSimple_mean1_tauresrat
#> 158              catch22_FC_LocalSimple_mean1_tauresrat
#> 159              catch22_FC_LocalSimple_mean1_tauresrat
#> 160              catch22_FC_LocalSimple_mean1_tauresrat
#> 161              catch22_FC_LocalSimple_mean1_tauresrat
#> 162              catch22_FC_LocalSimple_mean1_tauresrat
#> 163              catch22_FC_LocalSimple_mean1_tauresrat
#> 164              catch22_FC_LocalSimple_mean1_tauresrat
#> 165              catch22_FC_LocalSimple_mean1_tauresrat
#> 166                 catch22_FC_LocalSimple_mean3_stderr
#> 167                 catch22_FC_LocalSimple_mean3_stderr
#> 168                 catch22_FC_LocalSimple_mean3_stderr
#> 169                 catch22_FC_LocalSimple_mean3_stderr
#> 170                 catch22_FC_LocalSimple_mean3_stderr
#> 171                 catch22_FC_LocalSimple_mean3_stderr
#> 172                 catch22_FC_LocalSimple_mean3_stderr
#> 173                 catch22_FC_LocalSimple_mean3_stderr
#> 174                 catch22_FC_LocalSimple_mean3_stderr
#> 175                 catch22_FC_LocalSimple_mean3_stderr
#> 176                 catch22_FC_LocalSimple_mean3_stderr
#> 177     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 178     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 179     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 180     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 181     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 182     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 183     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 184     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 185     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 186     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi
#> 187                        catch22_MD_hrv_classic_pnn40
#> 188                        catch22_MD_hrv_classic_pnn40
#> 189                        catch22_MD_hrv_classic_pnn40
#> 190                        catch22_MD_hrv_classic_pnn40
#> 191                        catch22_MD_hrv_classic_pnn40
#> 192                        catch22_MD_hrv_classic_pnn40
#> 193                        catch22_MD_hrv_classic_pnn40
#> 194                        catch22_MD_hrv_classic_pnn40
#> 195                        catch22_MD_hrv_classic_pnn40
#> 196                   catch22_PD_PeriodicityWang_th0_01
#> 197                   catch22_PD_PeriodicityWang_th0_01
#> 198                   catch22_PD_PeriodicityWang_th0_01
#> 199                   catch22_PD_PeriodicityWang_th0_01
#> 200                   catch22_PD_PeriodicityWang_th0_01
#> 201                   catch22_PD_PeriodicityWang_th0_01
#> 202                   catch22_PD_PeriodicityWang_th0_01
#> 203                   catch22_PD_PeriodicityWang_th0_01
#> 204            catch22_SB_BinaryStats_diff_longstretch0
#> 205            catch22_SB_BinaryStats_diff_longstretch0
#> 206            catch22_SB_BinaryStats_diff_longstretch0
#> 207            catch22_SB_BinaryStats_diff_longstretch0
#> 208            catch22_SB_BinaryStats_diff_longstretch0
#> 209            catch22_SB_BinaryStats_diff_longstretch0
#> 210            catch22_SB_BinaryStats_diff_longstretch0
#> 211            catch22_SB_BinaryStats_mean_longstretch1
#> 212            catch22_SB_BinaryStats_mean_longstretch1
#> 213            catch22_SB_BinaryStats_mean_longstretch1
#> 214            catch22_SB_BinaryStats_mean_longstretch1
#> 215            catch22_SB_BinaryStats_mean_longstretch1
#> 216            catch22_SB_BinaryStats_mean_longstretch1
#> 217                   catch22_SB_MotifThree_quantile_hh
#> 218                   catch22_SB_MotifThree_quantile_hh
#> 219                   catch22_SB_MotifThree_quantile_hh
#> 220                   catch22_SB_MotifThree_quantile_hh
#> 221                   catch22_SB_MotifThree_quantile_hh
#> 222          catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 223          catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 224          catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 225          catch22_SB_TransitionMatrix_3ac_sumdiagcov
#> 226      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 227      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 228      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1
#> 229 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 230 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1
#> 231            catch22_SP_Summaries_welch_rect_area_5_1
#>                                                 names_b   metric names_a_mean
#> 1                                catch22_CO_FirstMin_ac accuracy   0.32666667
#> 2                      catch22_CO_HistogramAMI_even_2_5 accuracy   0.32666667
#> 3                                     catch22_CO_f1ecac accuracy   0.32666667
#> 4                                 catch22_CO_trev_1_num accuracy   0.32666667
#> 5                           catch22_DN_HistogramMode_10 accuracy   0.32666667
#> 6                            catch22_DN_HistogramMode_5 accuracy   0.32666667
#> 7                 catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.32666667
#> 8                 catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.32666667
#> 9                catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.32666667
#> 10                  catch22_FC_LocalSimple_mean3_stderr accuracy   0.32666667
#> 11      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.32666667
#> 12                         catch22_MD_hrv_classic_pnn40 accuracy   0.32666667
#> 13                    catch22_PD_PeriodicityWang_th0_01 accuracy   0.32666667
#> 14             catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.32666667
#> 15             catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.32666667
#> 16                    catch22_SB_MotifThree_quantile_hh accuracy   0.32666667
#> 17           catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.32666667
#> 18       catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.32666667
#> 19  catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.32666667
#> 20             catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.32666667
#> 21             catch22_SP_Summaries_welch_rect_centroid accuracy   0.32666667
#> 22                     catch22_CO_HistogramAMI_even_2_5 accuracy   0.31777778
#> 23                                    catch22_CO_f1ecac accuracy   0.31777778
#> 24                                catch22_CO_trev_1_num accuracy   0.31777778
#> 25                          catch22_DN_HistogramMode_10 accuracy   0.31777778
#> 26                           catch22_DN_HistogramMode_5 accuracy   0.31777778
#> 27                catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.31777778
#> 28                catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.31777778
#> 29               catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.31777778
#> 30                  catch22_FC_LocalSimple_mean3_stderr accuracy   0.31777778
#> 31      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.31777778
#> 32                         catch22_MD_hrv_classic_pnn40 accuracy   0.31777778
#> 33                    catch22_PD_PeriodicityWang_th0_01 accuracy   0.31777778
#> 34             catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.31777778
#> 35             catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.31777778
#> 36                    catch22_SB_MotifThree_quantile_hh accuracy   0.31777778
#> 37           catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.31777778
#> 38       catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.31777778
#> 39  catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.31777778
#> 40             catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.31777778
#> 41             catch22_SP_Summaries_welch_rect_centroid accuracy   0.31777778
#> 42                                    catch22_CO_f1ecac accuracy   0.35777778
#> 43                                catch22_CO_trev_1_num accuracy   0.35777778
#> 44                          catch22_DN_HistogramMode_10 accuracy   0.35777778
#> 45                           catch22_DN_HistogramMode_5 accuracy   0.35777778
#> 46                catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.35777778
#> 47                catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.35777778
#> 48               catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.35777778
#> 49                  catch22_FC_LocalSimple_mean3_stderr accuracy   0.35777778
#> 50      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.35777778
#> 51                         catch22_MD_hrv_classic_pnn40 accuracy   0.35777778
#> 52                    catch22_PD_PeriodicityWang_th0_01 accuracy   0.35777778
#> 53             catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.35777778
#> 54             catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.35777778
#> 55                    catch22_SB_MotifThree_quantile_hh accuracy   0.35777778
#> 56           catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.35777778
#> 57       catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.35777778
#> 58  catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.35777778
#> 59             catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.35777778
#> 60             catch22_SP_Summaries_welch_rect_centroid accuracy   0.35777778
#> 61                                catch22_CO_trev_1_num accuracy   0.34444444
#> 62                          catch22_DN_HistogramMode_10 accuracy   0.34444444
#> 63                           catch22_DN_HistogramMode_5 accuracy   0.34444444
#> 64                catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.34444444
#> 65                catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.34444444
#> 66               catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.34444444
#> 67                  catch22_FC_LocalSimple_mean3_stderr accuracy   0.34444444
#> 68      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.34444444
#> 69                         catch22_MD_hrv_classic_pnn40 accuracy   0.34444444
#> 70                    catch22_PD_PeriodicityWang_th0_01 accuracy   0.34444444
#> 71             catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.34444444
#> 72             catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.34444444
#> 73                    catch22_SB_MotifThree_quantile_hh accuracy   0.34444444
#> 74           catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.34444444
#> 75       catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.34444444
#> 76  catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.34444444
#> 77             catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.34444444
#> 78             catch22_SP_Summaries_welch_rect_centroid accuracy   0.34444444
#> 79                          catch22_DN_HistogramMode_10 accuracy   0.10814815
#> 80                           catch22_DN_HistogramMode_5 accuracy   0.10814815
#> 81                catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.10814815
#> 82                catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.10814815
#> 83               catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.10814815
#> 84                  catch22_FC_LocalSimple_mean3_stderr accuracy   0.10814815
#> 85      catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.10814815
#> 86                         catch22_MD_hrv_classic_pnn40 accuracy   0.10814815
#> 87                    catch22_PD_PeriodicityWang_th0_01 accuracy   0.10814815
#> 88             catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.10814815
#> 89             catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.10814815
#> 90                    catch22_SB_MotifThree_quantile_hh accuracy   0.10814815
#> 91           catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.10814815
#> 92       catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.10814815
#> 93  catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.10814815
#> 94             catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.10814815
#> 95             catch22_SP_Summaries_welch_rect_centroid accuracy   0.10814815
#> 96                           catch22_DN_HistogramMode_5 accuracy   0.06444444
#> 97                catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.06444444
#> 98                catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.06444444
#> 99               catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.06444444
#> 100                 catch22_FC_LocalSimple_mean3_stderr accuracy   0.06444444
#> 101     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.06444444
#> 102                        catch22_MD_hrv_classic_pnn40 accuracy   0.06444444
#> 103                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.06444444
#> 104            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.06444444
#> 105            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.06444444
#> 106                   catch22_SB_MotifThree_quantile_hh accuracy   0.06444444
#> 107          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.06444444
#> 108      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.06444444
#> 109 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.06444444
#> 110            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.06444444
#> 111            catch22_SP_Summaries_welch_rect_centroid accuracy   0.06444444
#> 112               catch22_DN_OutlierInclude_n_001_mdrmd accuracy   0.06888889
#> 113               catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.06888889
#> 114              catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.06888889
#> 115                 catch22_FC_LocalSimple_mean3_stderr accuracy   0.06888889
#> 116     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.06888889
#> 117                        catch22_MD_hrv_classic_pnn40 accuracy   0.06888889
#> 118                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.06888889
#> 119            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.06888889
#> 120            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.06888889
#> 121                   catch22_SB_MotifThree_quantile_hh accuracy   0.06888889
#> 122          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.06888889
#> 123      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.06888889
#> 124 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.06888889
#> 125            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.06888889
#> 126            catch22_SP_Summaries_welch_rect_centroid accuracy   0.06888889
#> 127               catch22_DN_OutlierInclude_p_001_mdrmd accuracy   0.06370370
#> 128              catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.06370370
#> 129                 catch22_FC_LocalSimple_mean3_stderr accuracy   0.06370370
#> 130     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.06370370
#> 131                        catch22_MD_hrv_classic_pnn40 accuracy   0.06370370
#> 132                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.06370370
#> 133            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.06370370
#> 134            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.06370370
#> 135                   catch22_SB_MotifThree_quantile_hh accuracy   0.06370370
#> 136          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.06370370
#> 137      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.06370370
#> 138 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.06370370
#> 139            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.06370370
#> 140            catch22_SP_Summaries_welch_rect_centroid accuracy   0.06370370
#> 141              catch22_FC_LocalSimple_mean1_tauresrat accuracy   0.08000000
#> 142                 catch22_FC_LocalSimple_mean3_stderr accuracy   0.08000000
#> 143     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.08000000
#> 144                        catch22_MD_hrv_classic_pnn40 accuracy   0.08000000
#> 145                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.08000000
#> 146            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.08000000
#> 147            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.08000000
#> 148                   catch22_SB_MotifThree_quantile_hh accuracy   0.08000000
#> 149          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.08000000
#> 150      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.08000000
#> 151 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.08000000
#> 152            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.08000000
#> 153            catch22_SP_Summaries_welch_rect_centroid accuracy   0.08000000
#> 154                 catch22_FC_LocalSimple_mean3_stderr accuracy   0.38000000
#> 155     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.38000000
#> 156                        catch22_MD_hrv_classic_pnn40 accuracy   0.38000000
#> 157                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.38000000
#> 158            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.38000000
#> 159            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.38000000
#> 160                   catch22_SB_MotifThree_quantile_hh accuracy   0.38000000
#> 161          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.38000000
#> 162      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.38000000
#> 163 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.38000000
#> 164            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.38000000
#> 165            catch22_SP_Summaries_welch_rect_centroid accuracy   0.38000000
#> 166     catch22_IN_AutoMutualInfoStats_40_gaussian_fmmi accuracy   0.56296296
#> 167                        catch22_MD_hrv_classic_pnn40 accuracy   0.56296296
#> 168                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.56296296
#> 169            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.56296296
#> 170            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.56296296
#> 171                   catch22_SB_MotifThree_quantile_hh accuracy   0.56296296
#> 172          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.56296296
#> 173      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.56296296
#> 174 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.56296296
#> 175            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.56296296
#> 176            catch22_SP_Summaries_welch_rect_centroid accuracy   0.56296296
#> 177                        catch22_MD_hrv_classic_pnn40 accuracy   0.23481481
#> 178                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.23481481
#> 179            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.23481481
#> 180            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.23481481
#> 181                   catch22_SB_MotifThree_quantile_hh accuracy   0.23481481
#> 182          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.23481481
#> 183      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.23481481
#> 184 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.23481481
#> 185            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.23481481
#> 186            catch22_SP_Summaries_welch_rect_centroid accuracy   0.23481481
#> 187                   catch22_PD_PeriodicityWang_th0_01 accuracy   0.20370370
#> 188            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.20370370
#> 189            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.20370370
#> 190                   catch22_SB_MotifThree_quantile_hh accuracy   0.20370370
#> 191          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.20370370
#> 192      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.20370370
#> 193 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.20370370
#> 194            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.20370370
#> 195            catch22_SP_Summaries_welch_rect_centroid accuracy   0.20370370
#> 196            catch22_SB_BinaryStats_diff_longstretch0 accuracy   0.23407407
#> 197            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.23407407
#> 198                   catch22_SB_MotifThree_quantile_hh accuracy   0.23407407
#> 199          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.23407407
#> 200      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.23407407
#> 201 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.23407407
#> 202            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.23407407
#> 203            catch22_SP_Summaries_welch_rect_centroid accuracy   0.23407407
#> 204            catch22_SB_BinaryStats_mean_longstretch1 accuracy   0.20222222
#> 205                   catch22_SB_MotifThree_quantile_hh accuracy   0.20222222
#> 206          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.20222222
#> 207      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.20222222
#> 208 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.20222222
#> 209            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.20222222
#> 210            catch22_SP_Summaries_welch_rect_centroid accuracy   0.20222222
#> 211                   catch22_SB_MotifThree_quantile_hh accuracy   0.46518519
#> 212          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.46518519
#> 213      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.46518519
#> 214 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.46518519
#> 215            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.46518519
#> 216            catch22_SP_Summaries_welch_rect_centroid accuracy   0.46518519
#> 217          catch22_SB_TransitionMatrix_3ac_sumdiagcov accuracy   0.34888889
#> 218      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.34888889
#> 219 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.34888889
#> 220            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.34888889
#> 221            catch22_SP_Summaries_welch_rect_centroid accuracy   0.34888889
#> 222      catch22_SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1 accuracy   0.24888889
#> 223 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.24888889
#> 224            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.24888889
#> 225            catch22_SP_Summaries_welch_rect_centroid accuracy   0.24888889
#> 226 catch22_SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1 accuracy   0.08370370
#> 227            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.08370370
#> 228            catch22_SP_Summaries_welch_rect_centroid accuracy   0.08370370
#> 229            catch22_SP_Summaries_welch_rect_area_5_1 accuracy   0.28296296
#> 230            catch22_SP_Summaries_welch_rect_centroid accuracy   0.28296296
#> 231            catch22_SP_Summaries_welch_rect_centroid accuracy   0.63851852
#>     names_b_mean   t_statistic      p.value
#> 1     0.31777778   0.280182339 3.906633e-01
#> 2     0.35777778  -0.500940956 3.100969e-01
#> 3     0.34444444  -0.887567401 1.910379e-01
#> 4     0.10814815   5.547883463 2.770485e-06
#> 5     0.06444444  11.648353522 9.298260e-13
#> 6     0.06888889   9.573883988 8.779501e-11
#> 7     0.06370370   9.432755248 1.220350e-10
#> 8     0.08000000   6.896592297 7.041388e-08
#> 9     0.38000000  -1.069972640 1.467284e-01
#> 10    0.56296296  -6.783105780 9.531318e-08
#> 11    0.23481481   1.228938483 1.144871e-01
#> 12    0.20370370   2.350969361 1.286715e-02
#> 13    0.23407407   2.324580179 1.365039e-02
#> 14    0.20222222   3.421037904 9.373813e-04
#> 15    0.46518519  -1.708957884 4.907081e-02
#> 16    0.34888889  -0.505105895 3.086509e-01
#> 17    0.24888889   2.291005721 1.470937e-02
#> 18    0.08370370   6.739180953 1.072026e-07
#> 19    0.28296296   1.553689350 6.555257e-02
#> 20    0.63851852  -7.256278078 2.722401e-08
#> 21    0.45333333  -2.776267204 4.765550e-03
#> 22    0.35777778  -0.683214080 2.499470e-01
#> 23    0.34444444  -1.047939272 1.516612e-01
#> 24    0.10814815   5.064526047 1.060052e-05
#> 25    0.06444444   7.058754705 4.579432e-08
#> 26    0.06888889  10.360002977 1.470288e-11
#> 27    0.06370370   8.291185010 1.928158e-09
#> 28    0.08000000   6.961547534 5.924685e-08
#> 29    0.38000000  -1.422074546 8.283582e-02
#> 30    0.56296296  -7.365973750 2.043467e-08
#> 31    0.23481481   1.085659734 1.432861e-01
#> 32    0.20370370   2.140113495 2.044337e-02
#> 33    0.23407407   2.087574954 2.286579e-02
#> 34    0.20222222   3.064618667 2.338624e-03
#> 35    0.46518519  -1.681072511 5.174492e-02
#> 36    0.34888889  -0.679311063 2.511636e-01
#> 37    0.24888889   1.670041395 5.283590e-02
#> 38    0.08370370   5.899483338 1.049650e-06
#> 39    0.28296296   1.115401422 1.369182e-01
#> 40    0.63851852  -6.775128389 9.736847e-08
#> 41    0.45333333  -3.738938429 4.043965e-04
#> 42    0.34444444   0.245816988 4.037777e-01
#> 43    0.10814815   4.235947047 1.050469e-04
#> 44    0.06444444   5.116397460 9.177089e-06
#> 45    0.06888889   5.623109001 2.249781e-06
#> 46    0.06370370   5.553276362 2.729420e-06
#> 47    0.08000000   5.056578226 1.083733e-05
#> 48    0.38000000  -0.438432108 3.321610e-01
#> 49    0.56296296  -3.535933980 6.934300e-04
#> 50    0.23481481   1.559162499 6.490278e-02
#> 51    0.20370370   2.287300666 1.483068e-02
#> 52    0.23407407   1.824101062 3.922873e-02
#> 53    0.20222222   2.704567935 5.662815e-03
#> 54    0.46518519  -1.204137981 1.191364e-01
#> 55    0.34888889   0.147927900 4.417117e-01
#> 56    0.24888889   1.666249267 5.321536e-02
#> 57    0.08370370   4.834255465 2.010259e-05
#> 58    0.28296296   1.423558047 8.262258e-02
#> 59    0.63851852  -4.384800532 6.976727e-05
#> 60    0.45333333  -1.341605759 9.506787e-02
#> 61    0.10814815   7.098929385 4.118294e-08
#> 62    0.06444444  13.296858712 3.599927e-14
#> 63    0.06888889  15.839309358 4.089875e-16
#> 64    0.06370370  12.435048222 1.897907e-13
#> 65    0.08000000  10.406212943 1.326950e-11
#> 66    0.38000000  -0.918288118 1.830214e-01
#> 67    0.56296296  -7.078047704 4.351771e-08
#> 68    0.23481481   1.535041432 6.780645e-02
#> 69    0.20370370   2.869986307 3.792858e-03
#> 70    0.23407407   2.618513298 6.947556e-03
#> 71    0.20222222   4.075732816 1.627891e-04
#> 72    0.46518519  -1.539942113 6.720811e-02
#> 73    0.34888889  -0.107688939 4.574920e-01
#> 74    0.24888889   2.607864161 7.124153e-03
#> 75    0.08370370   8.802276745 5.486343e-10
#> 76    0.28296296   2.704632673 5.661938e-03
#> 77    0.63851852  -7.500239788 1.441198e-08
#> 78    0.45333333  -2.445191635 1.039306e-02
#> 79    0.06444444   1.130277926 1.338105e-01
#> 80    0.06888889   1.037552621 1.540263e-01
#> 81    0.06370370   1.441822821 8.003265e-02
#> 82    0.08000000   0.882118415 1.924831e-01
#> 83    0.38000000  -5.009439150 1.235470e-05
#> 84    0.56296296 -12.162795300 3.263280e-13
#> 85    0.23481481  -1.823226948 3.929657e-02
#> 86    0.20370370  -1.745580618 4.573615e-02
#> 87    0.23407407  -2.398380831 1.156196e-02
#> 88    0.20222222  -1.822829809 3.932742e-02
#> 89    0.46518519  -4.474353294 5.449500e-05
#> 90    0.34888889  -4.479178765 5.377351e-05
#> 91    0.24888889  -3.079971898 2.249954e-03
#> 92    0.08370370   0.539431169 2.968537e-01
#> 93    0.28296296  -5.595688033 2.427088e-06
#> 94    0.63851852  -9.950138069 3.695760e-11
#> 95    0.45333333  -6.010096050 7.746418e-07
#> 96    0.06888889  -0.185000053 4.272584e-01
#> 97    0.06370370   0.026333718 4.895858e-01
#> 98    0.08000000  -0.485671414 3.154248e-01
#> 99    0.38000000  -6.901744907 6.945478e-08
#> 100   0.56296296 -16.268126856 2.033378e-16
#> 101   0.23481481  -2.459627478 1.005506e-02
#> 102   0.20370370  -2.935869195 3.224624e-03
#> 103   0.23407407  -3.739055833 4.042695e-04
#> 104   0.20222222  -3.747916088 3.947964e-04
#> 105   0.46518519  -5.488546720 3.265480e-06
#> 106   0.34888889  -6.687937282 1.229908e-07
#> 107   0.24888889  -4.480677932 5.355129e-05
#> 108   0.08370370  -0.627898316 2.674921e-01
#> 109   0.28296296  -8.432132546 1.358699e-09
#> 110   0.63851852 -15.891132429 3.755645e-16
#> 111   0.45333333  -8.210351221 2.359568e-09
#> 112   0.06370370   0.245427862 4.039269e-01
#> 113   0.08000000  -0.505105895 3.086509e-01
#> 114   0.38000000  -8.190301493 2.481074e-09
#> 115   0.56296296 -16.433976688 1.558010e-16
#> 116   0.23481481  -2.374652880 1.219926e-02
#> 117   0.20370370  -2.911296555 3.426441e-03
#> 118   0.23407407  -3.990774942 2.051050e-04
#> 119   0.20222222  -4.103499687 1.509188e-04
#> 120   0.46518519  -5.137429893 8.655954e-06
#> 121   0.34888889  -6.507908746 1.997072e-07
#> 122   0.24888889  -4.363656550 7.395154e-05
#> 123   0.08370370  -0.516992366 3.045412e-01
#> 124   0.28296296  -9.170819332 2.264287e-10
#> 125   0.63851852 -13.650211894 1.862210e-14
#> 126   0.45333333  -8.355877585 1.641454e-09
#> 127   0.08000000  -0.735932011 2.338417e-01
#> 128   0.38000000  -7.345710004 2.154445e-08
#> 129   0.56296296 -14.939792877 1.863042e-15
#> 130   0.23481481  -2.462955702 9.978583e-03
#> 131   0.20370370  -3.177836673 1.755749e-03
#> 132   0.23407407  -3.792859600 3.499834e-04
#> 133   0.20222222  -3.616620275 5.601907e-04
#> 134   0.46518519  -5.253817643 6.263984e-06
#> 135   0.34888889  -7.593138843 1.133327e-08
#> 136   0.24888889  -4.977395596 1.350564e-05
#> 137   0.08370370  -0.575231844 2.847856e-01
#> 138   0.28296296  -9.318024885 1.598028e-10
#> 139   0.63851852 -12.643343099 1.260661e-13
#> 140   0.45333333  -8.227065489 2.262927e-09
#> 141   0.38000000  -6.772907030 9.794874e-08
#> 142   0.56296296 -12.282677383 2.567826e-13
#> 143   0.23481481  -2.339016085 1.321672e-02
#> 144   0.20370370  -2.480278719 9.589068e-03
#> 145   0.23407407  -3.412255977 9.591116e-04
#> 146   0.20222222  -2.892067651 3.592633e-03
#> 147   0.46518519  -4.718808249 2.769679e-05
#> 148   0.34888889  -5.834592302 1.254907e-06
#> 149   0.24888889  -3.925405275 2.448533e-04
#> 150   0.08370370  -0.101168440 4.600565e-01
#> 151   0.28296296  -8.625468981 8.441789e-10
#> 152   0.63851852 -11.612374803 1.001622e-12
#> 153   0.45333333  -7.247486088 2.785890e-08
#> 154   0.56296296  -4.420275384 6.326715e-05
#> 155   0.23481481   1.723097157 4.775989e-02
#> 156   0.20370370   3.034133716 2.524641e-03
#> 157   0.23407407   2.404196616 1.141047e-02
#> 158   0.20222222   3.717038764 4.287797e-04
#> 159   0.46518519  -1.131397105 1.335788e-01
#> 160   0.34888889   0.709733812 2.417687e-01
#> 161   0.24888889   2.392291132 1.172256e-02
#> 162   0.08370370   5.872638644 1.130106e-06
#> 163   0.28296296   2.257891753 1.582609e-02
#> 164   0.63851852  -4.950668035 1.454716e-05
#> 165   0.45333333  -1.455323539 7.816010e-02
#> 166   0.23481481   4.476622124 5.415459e-05
#> 167   0.20370370   6.897695558 7.020739e-08
#> 168   0.23407407   5.930206010 9.646309e-07
#> 169   0.20222222   8.541593578 1.037149e-09
#> 170   0.46518519   1.390028797 8.754928e-02
#> 171   0.34888889   4.493829904 5.164044e-05
#> 172   0.24888889   6.908218394 6.826859e-08
#> 173   0.08370370  12.989273534 6.456990e-14
#> 174   0.28296296   8.532447278 1.060754e-09
#> 175   0.63851852  -1.756102360 4.481429e-02
#> 176   0.45333333   2.470978072 9.796425e-03
#> 177   0.20370370   0.362239811 3.598997e-01
#> 178   0.23407407   0.008606641 4.965960e-01
#> 179   0.20222222   0.452018235 3.273094e-01
#> 180   0.46518519  -2.042484534 2.514424e-02
#> 181   0.34888889  -1.318394781 9.884498e-02
#> 182   0.24888889  -0.170558885 4.328775e-01
#> 183   0.08370370   1.917888716 3.251032e-02
#> 184   0.28296296  -0.673825002 2.528791e-01
#> 185   0.63851852  -5.649763474 2.089924e-06
#> 186   0.45333333  -2.752561037 5.046327e-03
#> 187   0.23407407  -0.503258570 3.092919e-01
#> 188   0.20222222   0.025630859 4.898637e-01
#> 189   0.46518519  -3.058555809 2.374548e-03
#> 190   0.34888889  -2.658712959 6.316921e-03
#> 191   0.24888889  -0.625076983 2.684042e-01
#> 192   0.08370370   2.642461895 6.565159e-03
#> 193   0.28296296  -1.674223413 5.242006e-02
#> 194   0.63851852  -6.411660620 2.591164e-07
#> 195   0.45333333  -4.093258537 1.551943e-04
#> 196   0.20222222   0.566963514 2.875508e-01
#> 197   0.46518519  -2.480850448 9.576454e-03
#> 198   0.34888889  -1.888023105 3.453258e-02
#> 199   0.24888889  -0.304338899 3.815215e-01
#> 200   0.08370370   2.993846081 2.792127e-03
#> 201   0.28296296  -1.197008658 1.204985e-01
#> 202   0.63851852  -6.112585957 5.850365e-07
#> 203   0.45333333  -4.139179663 1.369095e-04
#> 204   0.46518519  -2.848973664 3.993123e-03
#> 205   0.34888889  -3.315662529 1.232569e-03
#> 206   0.24888889  -0.888717180 1.907339e-01
#> 207   0.08370370   2.710374699 5.584648e-03
#> 208   0.28296296  -2.063990103 2.403367e-02
#> 209   0.63851852  -8.355138891 1.644469e-09
#> 210   0.45333333  -4.757915946 2.484863e-05
#> 211   0.34888889   1.408627147 8.478871e-02
#> 212   0.24888889   2.565791554 7.863361e-03
#> 213   0.08370370   4.832378147 2.020768e-05
#> 214   0.28296296   2.367045414 1.241027e-02
#> 215   0.63851852  -2.292795770 1.465108e-02
#> 216   0.45333333   0.132973366 4.475664e-01
#> 217   0.24888889   2.111233171 2.174509e-02
#> 218   0.08370370   5.234347146 6.612156e-06
#> 219   0.28296296   1.440903462 8.016145e-02
#> 220   0.63851852  -5.734561380 1.653478e-06
#> 221   0.45333333  -2.004442831 2.721986e-02
#> 222   0.08370370   3.311988948 1.244327e-03
#> 223   0.28296296  -0.889116810 1.906282e-01
#> 224   0.63851852  -8.292377963 1.922433e-09
#> 225   0.45333333  -3.796540202 3.465419e-04
#> 226   0.28296296  -5.804709147 1.362606e-06
#> 227   0.63851852 -10.424887261 1.273167e-11
#> 228   0.45333333  -6.278583909 3.719369e-07
#> 229   0.63851852  -7.869910029 5.573885e-09
#> 230   0.45333333  -3.526179790 7.114845e-04
#> 231   0.45333333   3.461371789 8.435391e-04
# }
```
