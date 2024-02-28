# RAND Algorithmic Equity Tool

## Introduction

In recent years, there has been a growing awareness that Machine Learning (ML) algorithms can reinforce or exacerbate human biases. The RAND Algorithmic Equity Tool was developed to help assess and correct biases in algorithms that assist in decision-making processes. In particular, the tool helps users visualize tradeoffs between different types of fairness and overall model performance. It also provides tools to mitigate bias through post-processing or pre-processing.

This tool was originally produced as part of a research effort for RAND, with the goal of assisting the Department of Defense (DoD) as they invest in the development of ML algorithms for a growing number of applications. The tool has been extended to address the issue of using proxy measures for group labels, which is common in healthcare settings where information on race and ethnicity is often missing or imputed. The two companion reports further discusses this tool, its creation, and its development.

While ML algorithms are deployed in a wide variety of applications, this tool is specifically designed for algorithms that assist in decision-making processes. In particular, this tool is useful when algorithmic output is used to influence binary decisions about individuals. Hypothetical examples within this framework are (1) an algorithm that produces individual-level employee performance scores which are subsequently considered in promotional decisions or (2) an algorithm that produces recommendations for follow-up treatment from medical diagnostic testing.

## References

The following report further discusses this tool and its original creation: 
**[Advancing Equitable Decisionmaking for the Department of Defense Through Fairness in Machine Learning](https://www.rand.org/pubs/research_reports/RRA1542-1.html)**

The following paper provides the methodological innovations utilized in the tool to provide estimates with noisy group measurements: 
**[De-Biasing the Bias: Methods for Improving Disparity Assessments with Noisy Group Measurements](https://arxiv.org/abs/2402.13391)**

## Usage

Download the code from this repository to run this application locally. This application requires [R be installed](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-can-R-be-installed_003f) to run. It is likely that running the application locally will be preferred when the required data cannot be exported, for instance, due to privacy considerations.

Individuals who would like to quickly evaluate the tool with datasets that are not private can use this version posted to shinyapps.io:

**[RAND Algorithmic Equity Tool - Public Version](https://rand.shinyapps.io/rand-ml-equity-tool/)**


## Contact
Reach out to [Joshua Snoke](https://www.rand.org/about/people/s/snoke_joshua.html) for questions related to this repository.

## License
Copyright (C) 2023 by The RAND Corporation. This repository is released as open-source software under a GPL-2.0 license. See the LICENSE file.
