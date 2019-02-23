Data Wrangling Excercise 1
================
Afif

Introduction and Preparation
----------------------------

Purpose of this exercise is to work with a toy data set showing product purchases from an electronics store.While the data set is small and simple, it still illustrates many of the challenges you have to address in real-world data wrangling.

First step of solving this excercise is to load the necessary libraries, and the dataset.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
refine <- tbl_df(read.csv("refine.csv"))
```

------------------------------------------------------------------------

Clean up brand names
--------------------

Next is to clean up the 'company' column so all of the misspellings of the brand names are standardized.

Unique brand names currently registered in the data frame are as follows:

``` r
refine %>%
  select(company) %>%
  distinct()
```

    ## # A tibble: 19 x 1
    ##    company   
    ##    <fct>     
    ##  1 Phillips  
    ##  2 phillips  
    ##  3 philips   
    ##  4 phllips   
    ##  5 phillps   
    ##  6 phillipS  
    ##  7 akzo      
    ##  8 Akzo      
    ##  9 AKZO      
    ## 10 akz0      
    ## 11 ak zo     
    ## 12 fillips   
    ## 13 phlips    
    ## 14 Van Houten
    ## 15 van Houten
    ## 16 van houten
    ## 17 unilver   
    ## 18 unilever  
    ## 19 Unilever

From the data above, we know that there should be only four correct company names : phillips, akzo, van houten, and unilever. To correct these mispelling names, the following coding is used.

``` r
correct.company <- data_frame(initial=c('ph','fi','ak','va','un'),correct_name= c('phillips','phillips','akzo','van houten','unilever'))

buffer <- refine %>%
            mutate(initial=tolower(substr(company,1,2)))
buffer1<-left_join(correct.company, buffer)
```

    ## Joining, by = "initial"

``` r
buffer1$company<- buffer1$correct_name
refine <- buffer1 %>% select(-correct_name, -initial)
```

As the result of above data wrangling process we get these following unique company names in the dataset:

``` r
refine %>%
  distinct(company)
```

    ## # A tibble: 4 x 1
    ##   company   
    ##   <chr>     
    ## 1 phillips  
    ## 2 akzo      
    ## 3 van houten
    ## 4 unilever

Which in this case, we can conclude that company names have been successfully standarized.

------------------------------------------------------------------------

Separate Product code and number
--------------------------------

Next is to separate the product code and number from Product.code...number attribute into two separates variables.

``` r
refine <- refine %>% mutate(product_code=sub("-.*","",Product.code...number),product_number=sub(".*-","",Product.code...number))%>% 
          select(-Product.code...number)
refine%>%
  select(company, product_number,product_code)%>%
  sample_n(10)
```

    ## # A tibble: 10 x 3
    ##    company    product_number product_code
    ##    <chr>      <chr>          <chr>       
    ##  1 phillips   21             v           
    ##  2 phillips   5              p           
    ##  3 phillips   34             x           
    ##  4 akzo       43             v           
    ##  5 van houten 65             v           
    ##  6 van houten 56             v           
    ##  7 akzo       8              x           
    ##  8 akzo       5              q           
    ##  9 phillips   56             p           
    ## 10 unilever   6              q

------------------------------------------------------------------------

Add Product Categories
----------------------

In order to make data more readable, product codes will be changed to the following product categories:

``` r
product.categories <- data_frame(code=c("p","v","x","q"),category=c("Smartphone","TV","Laptop","Tablet"))
product.categories
```

    ## # A tibble: 4 x 2
    ##   code  category  
    ##   <chr> <chr>     
    ## 1 p     Smartphone
    ## 2 v     TV        
    ## 3 x     Laptop    
    ## 4 q     Tablet

``` r
buffer4 <- data_frame(category=refine$product_code)
buffer4[]<-product.categories$category[match(refine$product_code, product.categories$code)]
refine$category <- buffer4$category
refine%>%
  select(company,product_code,category)%>%
  sample_n(10)
```

    ## # A tibble: 10 x 3
    ##    company    product_code category  
    ##    <chr>      <chr>        <chr>     
    ##  1 phillips   x            Laptop    
    ##  2 van houten v            TV        
    ##  3 phillips   p            Smartphone
    ##  4 akzo       q            Tablet    
    ##  5 van houten x            Laptop    
    ##  6 phillips   x            Laptop    
    ##  7 unilever   q            Tablet    
    ##  8 van houten x            Laptop    
    ##  9 phillips   x            Laptop    
    ## 10 akzo       q            Tablet

------------------------------------------------------------------------

Add Full Address for Geocoding
------------------------------

To view the customer information on a map, the addresses need to be in a form that can be easily geocoded. Create a new column full\_address that concatenates the three address fields (address, city, country), separated by commas.

``` r
refine%>%
  mutate(full_address=paste(address,city,country,sep=","))%>%
  select(company,address,city,country,full_address)%>%
  sample_n(10)
```

    ## # A tibble: 10 x 5
    ##    company   address        city   country     full_address                
    ##    <chr>     <fct>          <fct>  <fct>       <chr>                       
    ##  1 phillips  Delfzijlstraa~ arnhem the nether~ Delfzijlstraat 56,arnhem,th~
    ##  2 unilever  Jourestraat 25 arnhem the nether~ Jourestraat 25,arnhem,the n~
    ##  3 van hout~ Delfzijlstraa~ arnhem the nether~ Delfzijlstraat 60,arnhem,th~
    ##  4 akzo      Leeuwardenweg~ arnhem the nether~ Leeuwardenweg 180,arnhem,th~
    ##  5 van hout~ Delfzijlstraa~ arnhem the nether~ Delfzijlstraat 59,arnhem,th~
    ##  6 phillips  Groningensing~ arnhem the nether~ Groningensingel 150,arnhem,~
    ##  7 phillips  Delfzijlstraa~ arnhem the nether~ Delfzijlstraat 55,arnhem,th~
    ##  8 phillips  Groningensing~ arnhem the nether~ Groningensingel 152,arnhem,~
    ##  9 unilever  Jourestraat 23 arnhem the nether~ Jourestraat 23,arnhem,the n~
    ## 10 unilever  Jourestraat 26 arnhem the nether~ Jourestraat 26,arnhem,the n~

------------------------------------------------------------------------

Create dummy variables for company and product category
-------------------------------------------------------

Next is to create binary variables for each of product and company for further analysis

``` r
refine<- refine%>% 
         mutate(company_phillips=match(company,"phillips"))%>%
         mutate(company_akzo=match(company,"akzo"))%>%
         mutate(company_unilever=match(company,"unilever"))%>%
         mutate(company_vanhouten=match(company,"van houten"))%>%
         mutate(product_smartphone=match(category,"Smartphone"))%>%
         mutate(product_tv=match(category,"TV"))%>%
         mutate(product_laptop=match(category,"Laptop"))%>%
         mutate(product_tablet=match(category,"Tablet"))
 
refine[is.na(refine)]<-0
```

Result for companies are as follows:

``` r
refine%>%
  select(company,company_phillips,company_akzo,company_unilever,company_vanhouten)%>%
  sample_n(10)
```

    ## # A tibble: 10 x 5
    ##    company  company_phillips company_akzo company_unilever company_vanhout~
    ##    <chr>               <dbl>        <dbl>            <dbl>            <dbl>
    ##  1 phillips                1            0                0                0
    ##  2 van hou~                0            0                0                1
    ##  3 akzo                    0            1                0                0
    ##  4 phillips                1            0                0                0
    ##  5 akzo                    0            1                0                0
    ##  6 van hou~                0            0                0                1
    ##  7 unilever                0            0                1                0
    ##  8 phillips                1            0                0                0
    ##  9 phillips                1            0                0                0
    ## 10 van hou~                0            0                0                1

Result for products are as follows:

``` r
refine%>%
  select(category,product_laptop,product_tv,product_smartphone,product_tablet)%>%
  sample_n(10)
```

    ## # A tibble: 10 x 5
    ##    category   product_laptop product_tv product_smartphone product_tablet
    ##    <chr>               <dbl>      <dbl>              <dbl>          <dbl>
    ##  1 TV                      0          1                  0              0
    ##  2 Tablet                  0          0                  0              1
    ##  3 Laptop                  1          0                  0              0
    ##  4 Laptop                  1          0                  0              0
    ##  5 Smartphone              0          0                  1              0
    ##  6 Laptop                  1          0                  0              0
    ##  7 Laptop                  1          0                  0              0
    ##  8 TV                      0          1                  0              0
    ##  9 Smartphone              0          0                  1              0
    ## 10 TV                      0          1                  0              0
