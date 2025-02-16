# On the robustness and provenance of the gambler's fallacy

Data, materials, and analysis scripts for: On the robustness and provenance of the gambler's fallacy.

### Preregistrations: 

Experiments 1a and 1b: https://aspredicted.org/2frb-zshx.pdf

Experiments 2a and 2b: https://aspredicted.org/prws-hqh3.pdf

Experiment 3: https://aspredicted.org/khp6-hkz8.pdf

### Files:

In the `Code` folder, `plot_figures.R` generates plots. `helper.R` contains helper functions for plotting. `statistical_analysis.R` contains all the statistical tests reported in the manuscript.

In the `Data` folder, `IID_dat.csv` contains results from Experiments 1a, 1b, 2a, and 2b, which used IID stimuli. `RH_replication_dat.csv` contains results from Experiment 3, which used RH sequences. In both files: `experiment` column indicates the corresponding experiment. `subject` indicates anonymized participant ID. `response` indicates whether probability judgments ("probability") or point predictions ("point") were elicited. `ground_truth` indicates the ground truth probability of the ball color at the end the sequence; for example, "60" means 60% objective probability of the terminal streak repeating. `trial` indicates the trial number. `sequence` indicates the 8-ball sequence the participant saw on each trial, where 0 indicates blue ball and 1 indicates red ball. `streak` indicates the terminal streak length. `repetition` is participants’ response regarding how likely or whether the streak will repeat, recorded as a percentage. `RH2023_study2A.csv` was retrieved from https://osf.io/98sve?view_only=f11ee708bfc4416a9da4e8a5ef71ad31.

In the `Experiment` folder,`Exp1a.html`, `Exp1b.html`, `Exp2a.html`, `Exp2b.html`, `Exp3.html` are the experiment scripts. `consent_bingo50.html` is the consent form we used. `save_data.php` is a file for writing data to server. We coded the experiments using jsPsych 7.3.4 (https://www.jspsych.org/7.3/). To run the experiments locally, make sure to comment out `timeline.push(consent);`.

The `Figures` folder contains figures generated using `Code/plot_figures.R`.
