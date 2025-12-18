# Strangers-to-the-Algorithm

This repository provides scripts for the replication of my Honors Thesis **Strangers to the Algorithm: The Effects of Algorithmic Decision Making on Due Process Rights in Removal Proceedings**

## Goals of the Research

*The primary question investigated by this paper* surrounds the causal effect automated decision-making aids or "algorithm-in-the-loop" processes have on the exercise of discretion or independent judgment on the part of the human decision makers in said process. Answers to this question are of increased importance, especially when the performance of these novel tools is assessed based on their alignment with key legal principles and protections, like those of Due Process.

## Approach

To assess these tools on that metric, I chose to look at the **Discretionary Behavior** of U.S. Immigration and Customs Enforcement Officers (I.C.E Officers) when making decisions on whether to (1) Release, (2) Detain with Bond, or (3) Detain without Bond an individual arrested by I.C.E.

To study this discretionary behavior, I constructed a measure that compared data on **Final Decisions** in custody determinations (i.e, whether someone was detained w/o bond or released) to the **Recommendations** given by I.C.E's Risk Classification Algorithm (RCA). By comparing these two data points, I was able to study how often officers were disagreeing with the RCA's recommendations and how those trends in discretion were changing over time. Specifically, my focus was on looking at how updates to I.C.E.'s RCA (one around Jan. 2014 and another around March 2015) affected the discretionary behavior of the I.C.E. Officers. These updates ended up serving as natural interventions or treatments that allowed for the effects of the changes on discretionary behavior to be studied robustly.

## Getting Started

### Prerequisites

To replicate this analysis, you will need R (version 4.0+) and the following packages:

-   'tidyverse' (data wrangling and visualization)
-   'lubridate' (date-time processing)

### Replication Workflow

This repository is designed for a "single-point-of-entry" replication. Once the raw data is in place, you only need to execute the master script.

### Script Structure

The master script executes the following pipeline in order:

-   01_setup.R: Ingests raw data on final decisions and risk rates; performs initial cleaning and formatting.

-   02_rec_rates.R: Applies logic-based rules described in paper to get data for RCA Recommendations.

-   03_discretion_rates.R: Merges Final Decisions data with RCA Recommendations data to calculate Net and Signed Discretion rates.

-   04_plots.R: Generates the final visualizations used in the paper.
