# Swashbuckling Violence and Nonstop Ninja Action : 
# A Dive into MPAA Rating Reasons

## Table of Contents
* [Motivation](#Motivation)   
* [Data Question](#Data-Question)
* [Data Sources and Tools](#Data-Sources-and-Tools)
* [The Process](#The-Process)
* [Link to the App](#Link-to-the-App)
* [Insights Gained](#Insights-Gained)

## Motivation
<details>
  <summary>You had me at macabre and suggestive humor</summary>      
  This project began as my midcourse capstone for Nashville Software School's Data Science Bootcamp. As I started to explore potential datasets, I came upon a list of bizarre and funny reasons given to justify MPAA ratings assigned to movies. (One of which, from the 2019 animated version of The Addams Family, is noted above!)    

  This piqued my curiosity.    

  To satisfy my curious nature, I will complete a text analysis of these MPAA rating reasons to see what trends there are to discover. This is my first time performing text analysis, and part of my motivation is the opportunity to learn more about tools and methods for effectively uncovering the insights held within text-based information.
</details>

## Data Questions
What trends will a text analysis of MPAA movie rating reasons reveal? Have content concerns changed over time? Is the language similar or distinctive between different ratings? What are the the modifying words and phrases connected to frequently occurring words? Are there associations among words such that they often appear in the same ratings together?

## Data Sources and Tools  
#### Data Source
* [filmratings.com](https://filmratings.com)   
  Film names, release years, MPAA ratings, rating reasons, and distributors

#### Technology Tools
* Python
  * [Jupyter Notebooks](https://jupyter.org/)
  * [pandas](https://pandas.pydata.org/)
  * [requests](https://requests.readthedocs.io/en/latest/)
  * [Selenium WebDriver](https://www.selenium.dev/documentation/webdriver/)
* R
  * [RStudio](https://posit.co/products/open-source/rstudio/)
  * [tidyverse](https://www.tidyverse.org/)
  * [tidytext](https://www.rdocumentation.org/packages/tidytext/versions/0.4.1)
  * [tm](https://www.rdocumentation.org/packages/tm/versions/0.7-10)
  * [wordcloud](https://www.rdocumentation.org/packages/wordcloud/versions/2.6)
* [Shiny](https://shiny.rstudio.com/)

## The Process  
<details>
  <summary>Acquiring the data</summary>
I scraped the rating reasons and associated data directly from filmratings.com for all movies rated from 1992 through 2022. This was done using Python with a combination of the requests package and Selenium WebDriver.

</details>  

<details>
  <summary>Cleaning and organizing the data</summary>
Initial cleaning was completed using Python, primarily using pandas and also pandasprofiling. These first steps included separating combined variables into separate columns, identifying and addressing duplicates, filtering out older movies that were rereleased/rerated during this time, and filtering out rerealeses to home video.

</details>

<details>
  <summary>Moving data into R, building the interactive web app in Shiny</summary>
Moving into R, it was time to deploy tidytext and tm to tokenize and perform EDA on the text of the rating reasons. I looked at unigrams, bigrams, correlations between words, used regular expressions to extract modifying words/phrases connected to a selection of frequently used words, and played around with a variety of visualization option.

From there, I began to build the Shiny app. The number one, ongoing challenge was deciding what NOT to include because there were so many fun ways to explore this data, all leading to fun tidbits of information. I ultimately decided to focus my presentation on 3 elements of the text analysis:

- Top content concerns and how they have or have not changed over time as well as similarities/differences in these content concerns accross different ratings

- Comparing and contrasting words used for different ratings

- Exploring the modifying words/phrases that are used in connection with the most commonly occuring words as well as identifying words that often appear in the same ratings together.

Building was a fun challenge. Shiny provides a lot of the structure to lean on for folks like me who don't have web development experience, but I did learn a smidge of HTML along the way as I worked through getting all the components placed where and looking how I wanted them to be. There is still so much more to explore and show with this data, my intent is to add a couple of additional tabs to explore the words that only occur one time in the data set, and also a tab to specifically explore the many different words found in the rating reasons to reference sexual content.


</details>

## Link to the App
Please [take a look at my app](https://fzq6a6-monica0weiss0sharp.shinyapps.io/mpaa_rating_reasons_app/) and adjust the filters to hone in on the timeframe and movie ratings that most interest you!

## Insights Gained

Across the 30+ years of movie rating reasons explored, language is consistently a top content concern for PG, PG-13, and R rate movies. With only 51 NC-17 movies from this time frame, it's more difficult to draw clear conclusions about them, but strong sexuality and violence lead the way in content concerns for that particular subset of movies. Many of the most frequently seen content concerns often appear without any modifying words or phrases. The most common modifying words are those that soften or strengthen the intensity of these words such as "some violence" or "strong language."

More broadly, I learned that I actually really enjoy working with text! It was great to learn some new tools and techniques, and also to satisfy my curiosity about what there was to discover in the language used to explain the reasons behind movie ratings.