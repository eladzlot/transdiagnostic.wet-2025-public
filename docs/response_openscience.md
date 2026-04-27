# Response to Open Science Advisor (CPX-25-0117.R1)

Dear Dr. Urry,

Thank you for your careful review and for supporting our badge applications. We respond point-by-point below, indicating what was done and where it can be found. The manuscript, the GitHub repository, and the linked OSF project ([https://osf.io/dfzgt/](https://osf.io/dfzgt/)) have all been updated; the OSF project will be registered to produce the time-stamped, immutable snapshot once all other revisions are in place, immediately before resubmission.

## A correction we found while auditing the analyses

While implementing the WAIC/PSIS diagnostic across all models (Item 2), we noticed that one of the multivariate mechanism models — the correlation between change in distress tolerance (DTS) and change in state anxiety to the core-threat script (BSAM) — was fit on control-group rather than treatment-group data, despite its internal label ("dts.bsam.wet") and the surrounding narrative referring to the WET condition. We refit the model on the correct subset. The new estimate is weakly positive where the original was null. The Results passage (Response to Core Threats subsection) and Discussion (Distress Tolerance subsection) have been revised accordingly. The correction was found through our own audit rather than a reviewer query; we mention it here so the change is on the record.

---

## Preregistration

### 1. Stopping boundary (ROPE 90%, two-point OASIS, increments of 30)

> *"The preregistration specified a stopping boundary (ROPE of 90% for a two-point OASIS difference) and the plan to recruit in increments of 30 but these details are not reflected in the manuscript. Please update the manuscript to note these details or specify how you deviated from this plan."*

**Response:** The stopping rule was applied as preregistered. We have added the full specification to the manuscript and clarified the temporal relationship between the stopping rule and the follow-up analyses.

The Transparency and Openness "Reporting" subsection now notes that recruitment was halted before the 148-participant target under a preregistered Bayesian stopping rule, with specifics in Methods. The Methods (Participants) subsection gives those specifics: recruitment in increments of 30; after each increment, we evaluated whether 90% of the posterior distribution for the WET–NIW difference in end-of-treatment OASIS fell within a region of practical equivalence of ±2 points; recruitment stopped when this criterion was met.

The Results section's Growth subsection now opens with a paragraph clarifying that the stopping rule was defined on the end-of-treatment OASIS contrast, which is what triggered termination. The follow-up assessments were completed afterwards. We flag this ordering for readers and note, citing Sloan et al. (2022), that continued post-treatment change is consistent with the WET literature.

**Location:** Methods — Transparency and Openness (Reporting); Methods — Participants; Results — Growth (opening paragraph).

---

### 2. Outlier handling (WAIC penalty > 1 and PSIS Pareto k > .5)

> *"The preregistration specified the plan to identify measurement-level outliers based on WAIC penalty (>1) and PSIS Pareto k (> .5) with potential exclusion if it was not possible to model them or there were few of them, but these details are not reflected in the manuscript. Please update the manuscript to note these details or specify how you deviated from this plan."*

**Response:** We have implemented the preregistered outlier rule across all 27 Bayesian models and added a brief account to the Methods. Joint flags (WAIC pointwise penalty > 1 and PSIS Pareto k > .5) were rare across all models (under 1% of measurements overall, and under 1% in any single model) and dispersed across measures, participants, items, and conditions. Because the flagged observations were too few to warrant exclusion under the preregistered rule, we retained all observations.

The Methods now documents this assessment and points to a new supplementary file with the per-model diagnostics: the rule, the per-model summary table, the full list of flagged observations, clustering analyses, and distributional plots. The supplement also notes that the Thought-Action Fusion Likelihood subscale produced more flags than other measures, attributable to floor effects in the response distribution interacting with the graded-response IRT specification. This does not affect the substantive conclusions, since TAF-L was not identified as a mechanism of change.

**Location:** Methods — Statistical Analysis (Missing Data and Estimation); Supplementary file `diagnostics_outliers.pdf`.

---

### 3. Thought-action fusion: instrument and schedule

> *"The preregistration specified the plan to administer the Thought Fusion Inventory but the manuscript instead noted administration of the Thought-Action Fusion Scale Revised. In addition, the preregistration indicated that you would measure thought-action fusion at each writing session but it seems instead to have been administered at pre- and post-intervention timepoints. Please update the manuscript to note these deviations from the preregistration."*

**Response:** Both deviations were made before data collection. We adopted the TAF-R because it more directly indexes the form of thought-action fusion most relevant to the present intervention, and we restricted administration to pre- and post-intervention to manage participant burden. The Transparency and Openness "Preregistration" paragraph now notes both changes explicitly.

**Location:** Methods — Transparency and Openness (Deviations from preregistration).

---

### 4. Measures not in or not from the preregistration (BSAM, CTSI, Credibility, MCQ-30, iBAT)

> *"The preregistration did not specify inclusion of several specific measures: Brief State Anxiety Measure, Core Threat Structured Interview, Treatment Credibility Questionnaire, and Metacognitive Beliefs Questionnaire. In addition, the Imaginal Behavior Approach Test was noted in the preregistration but does not appear in the manuscript. Please update the manuscript to note these deviations from the preregistration."*

**Response:** We have restructured the Transparency and Openness section to include a "Deviations from preregistration" subsection that lists every departure from the preregistered plan. As we worked through the items you raise, however, we concluded that several of them are not analytic deviations but design decisions about *how* the preregistered protocol was implemented, and that listing them as deviations would dilute the more substantive departures (the waitlist arm, the follow-up timing, the instrument substitution, the piecewise model). Specifically:

- **MCQ-30**: was preregistered. It appears in the preregistration's mechanism list as "Metacognition Questionnaire (Wells & Cartwright-Hatton, 2004)" (preregistration p. 5).
- **BSAM**: state-anxiety measurement was preregistered as a construct, but no specific instrument was named. Choosing the BSAM is a clarification, not a departure, so we did not list it.
- **CTSI**: was used as an intervention component (the structured procedure for eliciting each participant's core threat, which then served as the focus of the writing). It is not a measured outcome and plays no role in any analysis; it is part of the intervention itself, so we did not treat it as an analytic deviation.
- **Treatment Credibility Questionnaire**: was administered once, as a manipulation check on the WET vs. NIW rationale. It is not a hypothesised mechanism or a measured outcome, and adding a manipulation check did not change any preregistered analysis.
- **iBAT**: administered as preregistered, but not analyzed in this report. Listed in the Deviations subsection because the preregistration committed to analyzing it.

If you would prefer that we list the BSAM, CTSI, and CEQ in the Deviations subsection regardless of this reasoning, we are happy to add them back; please let us know.

**Location:** Methods — Transparency and Openness (Deviations from preregistration).

---

### 5. Zero-based time encoding and contrast coding for categorical variables

> *"The preregistration specified the plan to apply zero-based encoding of time and contrast coding for categorical variables but the manuscript does not specify these details. Please update the manuscript accordingly or note how you deviated from the preregistration."*

**Response:** Both decisions were implemented as preregistered. The analysis code in the public repository applies zero-based encoding of time and contrast coding of categorical variables throughout. We have not added these details to the manuscript because they are implementation details with no bearing on interpretation: the model equations in Methods, together with the public code, make the parameterization unambiguous. We can add a sentence to the Statistical Analysis section if you prefer; happy to do so on request.

**Location:** No manuscript change pending your response.

---

### 6. Piecewise vs linear time model, and WAIC/PSIS-based model selection

> *"The measurement model and Bayesian framework align with the preregistration but you switched from a linear time model to a piecewise growth specification. Also, I don't see details regarding the preregistered plan to use WAIC/PSIS-based model selection. Please note in the manuscript the deviation from the preregistered analysis plan and whether or not you followed the model selection plan."*

**Response:** Two parts, addressed separately.

*Linear vs. piecewise.* The shift from a single linear slope to a piecewise specification was a theory-driven choice made before data analysis. Change during the four-session intervention, the one-week follow-up window, and the two-month follow-up window are three qualitatively different regimes, and we did not expect them to share a single rate. The Methods section's existing discussion of the piecewise specification frames the choice on these grounds; the Deviations subsection now names the change as a departure from the preregistered linear form.

*Model-selection criterion.* The preregistration specified WAIC/PSIS for two distinct purposes. First, for deciding whether to include gender as a predictor: we applied the criterion as planned, and gender was retained on that basis. Second, for a more general model-selection plan (relative weight of .9). We did not run a broad candidate-model competition of that kind; the piecewise specification was chosen on theoretical grounds rather than competitive model fitting. WAIC and PSIS were also used for the per-observation fit diagnostics described in Item 2 and the supplementary material.

**Location:** Methods — Transparency and Openness (Deviations from preregistration).

---

## Open Data

### 7. Data and analysis scripts

> *"The necessary data and analysis scripts are present. Nice!"*

**Response:** Thank you. No action required.

---

### 8. Variable key or codebook

> *"Please create and upload a variable key or codebook for each of the data files."*

**Response:** We have created `data/CODEBOOK.md`, alongside the data files it documents. It covers every variable in each shipped data file:

- **`data_long.csv`** — long-format questionnaire responses. Each measure is documented with its full name, the exact `question`-number set within its parent scale, response range, and wave coverage. Sign conventions are stated explicitly.
- **`demographics.data.csv`** — per-participant demographics, condition, completion status, and wave-1 baseline totals. Includes column types, value sets for categorical columns, and notes on how `oasis` and `wsas` totals are computed.
- **`pid.condition.csv`** — id-to-condition crosswalk. Brief; notes that the identifier is a serial number, not the real Prolific ID.
- **`screening.csv`** — pre-randomization screening pool. Columns are grouped by purpose (identifier, metadata, eligibility criteria by construct, descriptive probes, Qualtrics internal). Attention-check handling, conditional items, and redundant Qualtrics-generated scoring variables are noted.
- **`diagnostics/outlier_summary.csv`** and **`outliers_flagged.csv`** — outputs of the preregistered diagnostic from Item 2.

**Location:** `data/CODEBOOK.md`.

---

### 9. OSF registration

> *"Please use the Add-ons feature in your OSF project to connect the GitHub repository to OSF, then register the OSF project. This way, contents are time-stamped and immutable."*

**Response:** The GitHub repository is now connected to the OSF project ([https://osf.io/dfzgt/](https://osf.io/dfzgt/)) via the OSF Add-ons feature. We will register the OSF project — taking a time-stamped snapshot of the linked repository — once all other revisions are in place, immediately before resubmission.

**Location:** [https://osf.io/dfzgt/](https://osf.io/dfzgt/).

---

### 10. Add the OSF project URL to the manuscript

> *"Please update the manuscript to provide a link to the OSF project (in addition to the link to the GitHub repository) so that readers can access the time-stamped, immutable version."*

**Response:** The Transparency and Openness section now lists both the GitHub repository and the OSF project. The link to the registered (time-stamped) snapshot will replace the project link once registration is complete.

**Location:** Methods — Transparency and Openness (Data, materials, code, and online resources).

---

## Open Materials

### 11. Participant-facing materials

> *"Urry will check whether all materials used as manipulated or measured variables, including demographic information, information about eligibility, and all self-report questionnaires, are available for evaluation."*

**Response:** We have created `supplementary/materials/MATERIALS.md`, a navigation document with two parts: (a) a single cross-reference table showing what participants saw at each timepoint, and (b) for each measured variable, the exact instruction stem participants read, the full response anchors, and a citation to the source where the item wording is published. The folder `supplementary/materials/` now consolidates all participant-facing materials, including the new `MATERIALS.md` and `consent.md` (which reproduces the consent text in full), together with the three intervention documents that were already in the repository (CTSI protocol, WET psychoeducation and writing instructions, NIW psychoeducation and writing instructions; relocated from `supplementary/instructions/`).

Items from published scales are not reproduced verbatim. For each, MATERIALS.md provides instructions, anchors, subscale-to-item mappings, and a citation to the source publication, where the full item wording can be obtained. Study-designed items (consent text, commitment-to-return item, treatment-credibility stem, post-writing reflection questions, fear-script evaluation items, debrief text, qualitative feedback prompts, and a description of the comprehension check) are reproduced in full within MATERIALS.md.

Demographic information was collected by Prolific at the participant-account level and returned to us as metadata; we did not administer a study-designed demographic instrument. This is documented in MATERIALS.md and in `data/CODEBOOK.md`.

A few measures were administered but are not analysed in this report (iBAT, TIII, SAIQ, Insight-SR, a 5-item fear-script evaluation matrix, and several probes from the repeat writing sessions). They are listed in a dedicated section of MATERIALS.md with a brief explanation of why they are not further documented. A separate section documents three open-ended qualitative feedback prompts (one at session 4, two at the two-month follow-up) that are analysed qualitatively in the manuscript.

**Location:** `supplementary/materials/` (consolidated folder).

---

### 12. Update the manuscript to note materials availability

> *"Please update the manuscript to specify in the Transparency and Openness section that participant-facing materials are available."*

**Response:** Done. The Transparency and Openness section now explicitly states that participant-facing materials are available in `supplementary/materials/`, and notes their content.

**Location:** Methods — Transparency and Openness (Data, materials, code, and online resources).

---

## Optional items

### 13. License the OSF project

> *"Please consider applying a license to your OSF project."*

**Response:** The repository is released under the MIT License (see `LICENSE` at the repository root); the same license will apply to the OSF project at registration. The Transparency and Openness section now states the license alongside the repository link.

**Location:** Methods — Transparency and Openness (Data, materials, code, and online resources); `LICENSE` at the repository root.

---

### 14. Upload Qualtrics survey files

> *"Please consider uploading your Qualtrics survey files to OSF."*

**Response:** We have chosen to provide a structured materials document (`supplementary/materials/MATERIALS.md`) rather than the raw Qualtrics `.qsf` files. The materials document covers all instructions, anchors, and study-designed items addressed in Item 11.

---
