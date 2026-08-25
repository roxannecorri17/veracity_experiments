# Veracity: Human-Controlled Evaluation Experiments

This repository contains the data and R scripts used for the **human-controlled evaluation of Veracity**, a deployed AI fact-checking system. The evaluation examines the behavioural robustness of Veracity across English and French and identifies potential failure modes related to evidence retrieval and claim formulation.

The repository accompanies the paper **“Veracity: Evaluating AI Fact-Checking in Practice.”**

## Overview

We conducted three complementary experiments using the deployed Veracity interface between July and August 2026:

1. **Cross-language performance and retrieval quality**
2. **Evidence–verdict alignment**
3. **Wording sensitivity**

The repository contains the datasets, preprocessing scripts, statistical analyses, and figures associated with these experiments.

## Repository Structure

```text
veracity_experiments/
│
├── 1. Cross-language performance and retrieval quality/
│   ├── data/
│   ├── figures/
│   └── scripts/
│
├── 2. Evidence-verdict_alignment/
│   ├── data/
│   ├── figures/
│   └── scripts/
│
├── 3. Wording_sensitivity/
│   ├── data/
│   ├── figures/
│   └── scripts/
│
└── README.md
```

All scripts use paths relative to the **repository root**. Analyses should therefore be run with the repository root as the R working directory.

---

## Experiment 1 — Cross-Language Performance and Retrieval Quality

The first experiment evaluates whether Veracity performs consistently across English and French.

The experiment uses **200 claims (100 true and 100 false)**, each tested in semantically equivalent English and French versions, yielding **400 responses**.

The analysis evaluates:

* binary classification accuracy;
* truth-aligned score quality;
* English–French score consistency;
* response consistency;
* cross-language source overlap;
* retrieval quality;
* source relevance;
* source-type distributions; and
* associations between retrieval quality and Veracity's scores.

### Retrieval evaluation

Retrieved source domains were classified along two dimensions:

**Source type**

* Journalistic source
* International organization
* Government/public institution
* Local organization
* Scientific source
* Intermediate media
* App
* Social media
* Unclear website

**Source relevance**

* Relevant
* Partially relevant
* Outdated
* Out of context
* Cannot determine

Source relevance was evaluated relative to the specific claim, including its temporal, geographic, population, and contextual characteristics.

---

## Experiment 2 — Evidence–Verdict Alignment

The second experiment evaluates whether Veracity's final verdict is supported by the evidence retrieved by the system.

The experiment contains **100 claims** tested in semantically equivalent English and French versions, yielding **200 responses**.

The analysis examines:

* whether retrieved evidence supports the final verdict (`Yes`, `Partially`, or `No`);
* whether the response relies on retrieved evidence (`Yes`, `Mixed`, `No`, or `Unclear`);
* whether Veracity flags uncertainty in the claim;
* whether Veracity flags limitations or uncertainty in the retrieved evidence; and
* English–French score consistency.

Results are summarized using frequency distributions and cross-tabulations by language.

---

## Experiment 3 — Wording Sensitivity

The third experiment evaluates whether Veracity's outputs are robust to semantically related changes in claim formulation.

The experiment uses **100 original claims** (75 English-origin and 25 French-origin) and their translations. Each claim is evaluated in four forms:

* Original
* Passive
* Synonym substitution
* Negation

This produces **800 responses**.

Each reformulation is compared with its corresponding original claim along three dimensions:

### Response coherence

* `2` — same or highly similar arguments
* `1` — partially similar arguments
* `0` — dissimilar arguments

### Score coherence

For passive and synonym reformulations, coherence is calculated from the absolute difference between the reformulated and original Veracity scores.

For negated claims, the comparison is instead made against the inverse of the original score:

```text
Expected negated score = 100 − original score
```

Score coherence is coded as:

* `2` — difference < 10
* `1` — difference between 10 and 30
* `0` — difference > 30

### Source similarity

Source overlap is calculated from retrieved domain names:

* `2` — at least 5 domains in common
* `1` — 1–4 domains in common
* `0` — no domains in common

English–French differences are evaluated separately for each reformulation and metric.

---

## Common Classification Rule

To maintain consistency with the benchmark evaluation reported in the paper, Veracity's reliability scores are converted into binary predictions using the following threshold:

```text
Score > 50  → SUPPORTS
Score ≤ 50  → REFUTES
```

A prediction is considered accurate when the resulting classification matches the ground-truth label.

In addition to binary accuracy, Experiment 1 uses **truth-aligned score quality** to capture the magnitude of scoring errors. Scores are oriented toward the correct endpoint so that higher values indicate better performance.

---

## Statistical Analysis

Depending on the experiment and outcome, analyses include:

* descriptive statistics and frequency distributions;
* chi-square tests;
* Wilcoxon rank-sum tests;
* Kruskal–Wallis tests;
* pairwise comparisons; and
* Spearman rank correlations.

Missing observations are excluded from the corresponding analyses.

---

## Software

Analyses were conducted in **R**.

The scripts primarily rely on packages including:

```r
library(tidyverse)
library(readxl)
library(janitor)
```

Individual scripts may load additional packages when required.

## Reproducing the Analyses

1. Clone or download this repository.
2. Open the repository as an R project or set the repository root as the working directory.
3. Install the required R packages.
4. Run the scripts within each experiment's `scripts/` directory in numerical order where applicable.
5. Generated figures are saved in the corresponding experiment's `figures/` directory.

Raw data should not be manually modified to reproduce the analyses. Cleaning, recoding, and derived variables are handled by the corresponding R scripts.

## Citation

If you use this repository, please cite:

> Sallami, D., Eldifrawi, I., Corriveau, R., Godbout, J.-F., & Rabbany, R. *Veracity: Evaluating AI Fact-Checking in Practice.*

Full publication information will be added upon publication.
