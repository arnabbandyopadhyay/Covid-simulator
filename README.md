**Covid-19 Simulator**

Covid simulator is an online tool designed for generating various possible pandemic scenarios. This is based upon age stratified infection epidemic model guided by the social mixing pattern. For background on infection epidemic model that is implemented in the current framework, see the paper by Khalaie and Mitra et al. (https://doi.org/10.1186/s12916-020-01884-4) The state-of-the-art study on social contact patterns is the POLYMOD study, a survey based on self-reported contacts, is the first large scale study with 7290 participants across eight European countries. For detailed information on age-specific mixing matrices and what data inform them , see the paper on POLYMOD by Mossong et al (https://doi.org/10.1371/journal.pmed.0050074). POLYMOD data is part of the 'socialmixr' R package (https://cran.r-project.org/web/packages/socialmixr/vignettes/socialmixr.html), which we used to derive contact matrices. Further information is available at https://socialcontactdata.org/. We also refer to the Rshiny App on social contact rates (SOCRATES, https://doi.org/10.1186/s13104-020-05136-9 , https://socialcontactdata.org/socrates/) which we integrated into our mathematical model to simulate pandemic scenarios.


/https://arnabbandyopadhyay.shinyapps.io/Covid-situation-simulator/

Press Simulate to play

![alt text](https://github.com/arnabbandyopadhyay/Covid-simulator/blob/main/contents.png)
