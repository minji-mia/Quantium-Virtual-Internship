# Quantium Data Analytics Virtual Internship

Quantium provides this program.

## [Certificate of Completion](https://insidesherpa.s3.amazonaws.com/completion-certificates/Quantium/NkaC7knWtjSbi6aYv_Quantium_zEdLKmMSBsBPSazdf_completion_certificate.pdf) 
![image](https://user-images.githubusercontent.com/52568892/97493639-46654300-1933-11eb-85d6-b2a93f0ca064.png)

## Who is Quantium?

Quantium is leading data science and AI Firm, founded in Australia in 2002 and offers a 17-year track record of innovation in data science.
Quantium combines the best of human and artificial intelligence to power possibilities for individuals, organisations, and society. 

## Project Overview

This project consists of 3 tasks.

+ Task 1: Data preparation and customer analytics

Analyze client's transaction dataset and identify customer purchasing behaviors to generate insights and provide commercial recommendations.

+ Task 2: Experimentation and uplift testing

Extend your analysis from Task 1 to help you identify benchmark stores that allow you to test the trial store layouts' impact on customer sales.

+ Task 3: Analytics and commercial application

Use analytics and insights from Task 1 and 2 to prepare a report for your client, the Category Manager.

## Installation

+ Download R Software: https://cran.r-project.org/

+ Install required libraries:

```r 
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("readr")
``` 

## Task 1

+ Analyse the data to understand the current purchasing trends and behaviors.
+ The client is particularly interested in customer segments and their chip purchasing behavior. 
+ LIFESTAGE: Customer attribute that identifies whether a customer has a family or not and what point in life they are at, e.g., are their children in pre-school/primary/secondary school. 
+ PREMIUM_CUSTOMER: Customer segmentation is used to differentiate shoppers by the price point of products they buy and the types of products. It is used to identify whether customers may spend more for quality or brand or whether they will purchase the cheapest options. 

### Insights

1. Sales have mainly been due to Budget - older families, Mainstream - young singles/couples, and Mainstream  - retirees shoppers. 
2. The high spend on chips for mainstream young singles/couples and retirees is due to more of them than other buyers. 
3. Mainstream, mid-age, and young singles and couples are also more likely to pay more per packet of chips. This is indicative of impulse buying behavior.
4. Mainstream young singles and couples are 23% more likely to purchase Tyrrells chips compared to the rest of the population. 

**Thus, the Category Manager may want to increase the category’s performance by off-locating some Tyrrells and smaller packs of chips in discretionary space near segments where young singles and couples frequent more often to increase visibility and impulse behavior.**

## Task 2

+ Create a measure to compare different control stores to each of the trial stores to do this, write a function to reduce having to re-do the analysis for each trial store. 
+ Consider using Pearson correlations or a metric such as a magnitude distance e.g., 1- (Observed distance – minimum distance)/(Maximum distance – minimum distance) as a measure.  + Compare each trial and control pair during the trial period. 

### Insights

We’ve found control stores 233, 155, 237 for trial stores 77, 86, and 88 respectively.

1. The results for trial stores 77 and 88 during the trial period show a significant difference in at least two of the three trial months, but this is not the case for trial store 86. 
2. We can check with the client if the implementation of the trial was different in trial store 86, but overall, the trial shows a significant increase in sales. 

## Reference
https://www.theforage.com/virtual-internships/prototype/NkaC7knWtjSbi6aYv/Data%20Analytics%20Virtual%20Experience%20Program

## License

[MIT](https://github.com/minji-mia/Quantium-Virtual-Internship/blob/main/LICENSE)
