# EAST sleep deprivation study

Data and code accompanying **“Sleep deprivation disrupts error awareness and subsequent behavioural regulation.”** The study compares well-rested (WR) and total sleep-deprivation (TSD) conditions in 30 participants.

## Contents

- `data/`: participant-level performance, sleepiness, and time-on-task summaries.
- `A00_descriptive_summary.m`: descriptive statistics and manuscript Figure 2; uses `print2pdf.m` and saves results to `outputs/`.
- `EAST_bLMM_Overview.ipynb`: Bayesian analyses of participant-level summaries.
- `single_trial_GLMs/data/data_clean.csv`: prepared trial-level data.
- `single_trial_GLMs/Linear_models/Behav_analysis_main.R`: Bayesian RT and accuracy model fitting, including the correct-current-trial RT analysis.

## How to run

Start in the repository's main folder and keep the folder structure intact.

**MATLAB:** Run `A00_descriptive_summary.m`.

**Python/Jupyter:** Run `EAST_bLMM_Overview.ipynb`. Requires Bambi, PyMC, ArviZ, pandas, NumPy, Matplotlib, and statsmodels.

**R:** Requires brms, dplyr, and a working Stan setup. Run:

```r
source('single_trial_GLMs/Linear_models/Behav_analysis_main.R')
setwd('single_trial_GLMs/data')
EAST_linear_models()
```

R model fits are saved in the current working directory. Fitting all models can take substantial time.

The analysis code is available under the MIT License.