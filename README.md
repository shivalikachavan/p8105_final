# p8105_final

The House Always Wins? Exploring the Societal Costs of Legal Sports Gambling

## EDA
1. Sports handle across states
   * [ ] stratify online vs retail (J)
   * [ ] spikes in handle -> what corresponding sports season (S)
   * [ ] chloropleth -> states total handle by year (J)

> [!IMPORTANT]
> $ values are not inflation adjusted
  
2. BRFSS
   * [ ] confirm outcome variables available across years (S)
   * [ ] correlation matix -> identify colinearity -> stratigy by sex (A)
   * [ ] demographics -> age/sex -> do they match national demographics, is a scaling factor necessary? <br>
         * urban/rural, marital status (children in household), education status, income level, employment status <br>
         * insurance coverage -> financial strain from medical bills and/or utilities <br>
         * mental health / physical health

## ANALYSES / MODELS
1. Does the legalization of sports betting correlate with poor mental health outcomes?
   * [ ] prop.test by state -> do rates of ____ differ?
      * [ ] 2017 vs. 2024
      * [ ] pre-legalization / post
   * [ ] regression
      * [ ] recreate Couture
      * [ ] expand time frame
      * [ ] expand to other outcomes variables
        
2. Male loneliness epidemic

> [!WARNING]
> We will probably need to account for COVID by looking at data before 2020 (and potentially after 2021?)

## Questions for TA:
1. Is our data pipeline ok?
   * download zipped xpt files -> unzip in directory -> concatenate across desired variables -> save raw csv as zip -> load csv with factors in R
