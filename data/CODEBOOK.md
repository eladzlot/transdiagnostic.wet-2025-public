# Codebook

Variable-level documentation for the data files in this repository.

---

## `data_long.csv`

Long-format dataset of all questionnaire responses, one row per item response.
35,108 rows × 6 columns.

| Variable | Type | Description |
|---|---|---|
| `id` | integer | Participant identifier. A sequential serial number assigned to protect participant privacy, *not* the real Prolific ID. Matches `id` in `demographics.data.csv` and `prolific_pid` in `pid.condition.csv` and the diagnostics files. |
| `condition` | character | Experimental condition. One of `treatment` (WET), `control` (NIW), `waitlist`. |
| `wave` | integer | Measurement wave. See wave coding below. |
| `measure` | character | Measure label. See measure coding below. |
| `question` | integer | Item number within the measure. 1-indexed. |
| `value` | integer | Response value on the item's original scale. Range depends on the measure (see measure coding below). |

### Wave coding

| `wave` | Timepoint |
|---|---|
| 0 | Screening (pre-randomization; OASIS and WSAS only) |
| 1 | Writing session 1 |
| 2 | Writing session 2 |
| 3 | Writing session 3 |
| 4 | Writing session 4 (end of intervention) |
| 5 | One-week follow-up |
| 6 | Two-month follow-up |

The waitlist group was assessed at timepoints equivalent to waves 1, 4, and 5 of the active conditions.

### Measure coding

Each measure below lists its full name, the number of items, the item identifiers as they appear in the `question` column, the response scale, and the waves at which it was administered.

For composite instruments whose subscales share a 1–N item numbering (MCQ-30, TAF-R, BCSS-24), the `question` values identify the original item position within the full parent scale. This lets an independent analyst reconstruct the parent-scale layout without ambiguity.

| `measure` | Scale | `question` values | Response range | Waves |
|---|---|---|---|---|
| `oasis` | Overall Anxiety Severity and Impairment Scale | 1–5 (5 items) | 0–4 | 0–6 (every wave) |
| `wsas` | Work and Social Adjustment Scale | 1–5 (5 items) | 0–8 | 0, 1, 4, 5, 6 |
| `dts` | Distress Tolerance Scale | 1–14 (14 items; originally 15-item, item 6 dropped due to a data-collection error that duplicated item 7) | 1–5 | 1, 4 |
| `taf_m` | Thought-Action Fusion Scale Revised — Moral subscale | 1, 3, 4, 6, 8, 10, 11, 13, 15, 17, 18, 19 (12 items, positions within the 19-item TAF-R) | 0–4 | 1, 4 |
| `taf_l` | Thought-Action Fusion Scale Revised — Likelihood subscale | 2, 5, 7, 9, 12, 14, 16 (7 items, positions within the 19-item TAF-R) | 0–4 | 1, 4 |
| `bcss_self_neg` | Brief Core Schema Scale — Self-negative subscale | 1–6 (6 items, positions within the 24-item BCSS) | 0–4 | 1, 4 |
| `bcss_self_pos` | Brief Core Schema Scale — Self-positive subscale | 7–12 (6 items, positions within the 24-item BCSS) | 0–4 | 1, 4 |
| `bcss_other_neg` | Brief Core Schema Scale — Other-negative subscale | 13–18 (6 items, positions within the 24-item BCSS) | 0–4 | 1, 4 |
| `bcss_other_pos` | Brief Core Schema Scale — Other-positive subscale | 19–24 (6 items, positions within the 24-item BCSS) | 0–4 | 1, 4 |
| `ifs` | Impact of Future Events Scale | 1–24 (24 items) | 1–5 | 1, 4 |
| `bsam.pre` | Brief State Anxiety Measure — before writing session, while imagining core-threat script | 1–6 (6 items) | 1–7 | 1–6 (administered pre-session at waves 1–4 and at both follow-ups) |
| `bsam.post` | Brief State Anxiety Measure — after writing session | 1–6 (6 items) | 1–7 | 1–4 (writing sessions only) |
| `mcq_confidence` | MCQ-30 — Cognitive confidence subscale | 8, 14, 17, 24, 26, 29 (6 items, positions within the 30-item MCQ) | 1–4 | 1, 4 |
| `mcq_worry` | MCQ-30 — Positive beliefs about worry subscale | 1, 7, 10, 19, 23, 28 (6 items, positions within the 30-item MCQ) | 1–4 | 1, 4 |
| `mcq_danger` | MCQ-30 — Negative beliefs about uncontrollability and danger subscale | 2, 4, 9, 11, 15, 21 (6 items, positions within the 30-item MCQ) | 1–4 | 1, 4 |
| `mcq_self` | MCQ-30 — Cognitive self-consciousness subscale | 3, 5, 12, 16, 18, 30 (6 items, positions within the 30-item MCQ) | 1–4 | 1, 4 |
| `mcq_control` | MCQ-30 — Beliefs about the need to control thoughts subscale | 6, 13, 20, 22, 25, 27 (6 items, positions within the 30-item MCQ) | 1–4 | 1, 4 |
| `scs` | Self-Compassion Scale — Short Form | 1–12 (12 items) | 1–5 | 1, 4 |
| `commitment` | Single-item commitment to complete the next session | 1 (1 item) | 1–5 | 1, 2, 3, 4 |
| `credibility.think` | Treatment Credibility Questionnaire — cognitive subscale | 4 items | 1–9 | 1 (post-rationale, pre-intervention) |
| `credibility.feel` | Treatment Credibility Questionnaire — affective subscale | 2 items | 1–9 | 1 (post-rationale, pre-intervention) |
| `tiii` | Experimental measure, not analyzed in this report | 10 items | 1–5 | 1, 4, 6 |

### Sign conventions for change

For cross-measure interpretation: higher `value` represents more of the construct named in the measure. Specifically:

- For `oasis`, `wsas`, and BSAM (`bsam.pre`, `bsam.post`): higher = more symptoms / anxiety / impairment (reduction is the desired outcome).
- For `dts`: higher = greater distress tolerance (increase is the desired outcome).
- For `scs`: higher = greater self-compassion (increase is the desired outcome).
- For BCSS: higher on each subscale = stronger endorsement of that schema type.
- For MCQ, TAF, IFS: higher = stronger endorsement of the belief / experience.

This matters for reading the covariance analyses: for example, a positive correlation between change in DTS and change in BSAM indicates that gains in distress tolerance tracked *increases* in state anxiety, opposite to the direction predicted by the hypothesized mechanism.

### Notes

- Reverse-scored items on BSAM (items 1, 2, 4 — "relaxed", "steady", "comfortable") are already reversed in `value`. See `scripts/tidy.R` for the transformation.
- The `tiii` measure is an experimental measure unrelated to the hypotheses of this study and is not analyzed or reported. It remains in the long-format file because it was collected in the same survey.
- Missing responses are not represented as rows (the file is long-format, so a missing item-wave-participant triple simply does not appear).

---

## `demographics.data.csv`

One row per randomized participant, with demographic information, condition assignment, completion status, and baseline scores on the primary and secondary symptom measures. 162 rows × 10 columns.

| Variable | Type | Description |
|---|---|---|
| `id` | integer | Participant identifier. Serial number, not the real Prolific ID (see `data_long.csv`). |
| `Age` | integer | Participant age in years at time of participation. Range 19–79; one participant declined to report (`NA`). |
| `Sex` | character | Self-reported sex as collected by Prolific. Values in this sample: `Female`, `Male`. Prolific's response options include additional categories which did not occur in this sample. |
| `Ethnicity` | character | Prolific's "Ethnicity simplified" category. Values in this sample: `White`, `Asian`, `Black`. |
| `Country` | character | Country of residence as reported to Prolific. Six distinct countries in this sample, predominantly `United Kingdom` (144 of 162). |
| `Employment` | character | Employment status as reported to Prolific. Values include `Full-Time`, `Part-Time`, `Unemployed (and job seeking)`, `Not in paid work (e.g. homemaker', 'retired or disabled)`, `Other`, and `DATA_EXPIRED`. `DATA_EXPIRED` is a Prolific-internal marker indicating that the participant's demographic record has been purged from Prolific's systems since data collection; it is not a substantive employment category. |
| `condition` | character | Experimental condition. One of `treatment` (WET), `control` (NIW), `waitlist`. Matches `condition` in `pid.condition.csv`. |
| `completed` | logical | `TRUE` if the participant completed all assessments (the four writing sessions and both follow-ups for the active arms; the equivalent schedule for the waitlist arm); `FALSE` otherwise. |
| `oasis` | integer | Total OASIS score at wave 1 (pre-intervention, before the first writing session). Sum of the five OASIS items; range in this sample 2–14. For the waitlist arm, wave 1 is the equivalent pre-baseline assessment from the prior study. |
| `wsas` | integer | Total WSAS score at wave 1. Sum of the five WSAS items; range in this sample 1–31. |

### Notes

- The 162-participant total in this file matches `pid.condition.csv` and reflects all randomized participants across the three conditions.
- `oasis` and `wsas` are provided as pre-computed baseline totals for convenience. They can be reconstructed from `data_long.csv` by summing the relevant items at wave 1 per participant.

---

---

## `pid.condition.csv`

One row per randomized participant, linking participant identifier to experimental condition. 162 rows × 2 columns.

| Variable | Type | Description |
|---|---|---|
| `prolific_pid` | integer | Participant identifier. A sequential serial number assigned to protect participant privacy, *not* the real Prolific ID. Corresponds to `id` in `data_long.csv` and `demographics.data.csv`. |
| `condition` | character | Experimental condition. One of `treatment` (WET), `control` (NIW), `waitlist` (passive waitlist group drawn from a prior study with identical screening and procedural criteria). |

---

## `screening.csv`

One row per individual who completed the screening survey. 375 rows × 46 columns. This file is the pre-randomization pool; the 162 participants who passed eligibility and were randomized are documented in `pid.condition.csv` and `demographics.data.csv`. The file is organized by Qualtrics export convention and includes a mix of raw item responses, computed totals, and Qualtrics-internal scoring variables.

The columns group by purpose as follows.

| Purpose | Columns | Description |
|---|---|---|
| Identifier | `prolific_pid` | Participant identifier. Serial number, not the real Prolific ID. Links to `id` in `data_long.csv` / `demographics.data.csv` for the subset who were randomized. |
| Metadata | `startdate`, `enddate` | Qualtrics start and end timestamps for the screening session. |
| Anxiety (inclusion criterion) | `oasis`, `oasis_1`–`oasis_5` | OASIS total (`oasis`) and five individual items. Inclusion criterion: `oasis` > 4. |
| Functioning (inclusion criterion) | `wsas_1`, `wsas_2`, `wsas_3`, `wsas_5`, `wsas_6` | Five WSAS items (work, home management, social leisure, private leisure, family/relationships). Inclusion criterion: at least one WSAS item > 2. |
| Attention check (exclusion) | `wsas_4`, `phq_3`, `attention_check` | `wsas_4` and `phq_3` are instruction items embedded inside the WSAS and PHQ-9 respectively ("respond *not at all*" / "respond *nearly every day*"). These are excluded from the WSAS and PHQ totals. `attention_check` is the count of failed instruction items; participants who failed either were excluded. |
| Depression (exclusion criterion) | `phq`, `phq_1`, `phq_2`, `phq_4`–`phq_9` | PHQ-9 total (`phq`; excludes the `phq_3` instruction item) and its eight scored items. Exclusion criterion: `phq` > 14. |
| Trauma (exclusion criterion) | `a`, `b_1`–`b_4`, `sc3` | Gateway item `a` asks whether the participant has experienced a qualifying trauma; `b_1`–`b_4` are brief PCL-5 items administered only to those who endorsed `a`, so these columns are `NA` for most of the sample. `sc3` is the PCL-5 sum. Exclusion criterion: `sc3` > 6. |
| Psychosis (exclusion criterion) | `delusion`, `hallucination` | DIAMOND screener items 19 and 20 (self-report of delusional and hallucinatory experiences). Exclusion if either is endorsed. |
| Diagnostic screener (descriptive only) | `screener_1`–`screener_7` | DIAMOND-style yes/no items covering OCD, social anxiety, panic, agoraphobia, GAD, and specific phobia. Collected for descriptive purposes only; not used in the eligibility decision. All values are `NA` in this data, as the items were not populated in this collection round. |
| Descriptive | `magic` | Follow-up probe: "if you could make only one of them disappear — anxiety or depressed mood — which would you choose?" Not used in analysis. |
| Descriptive | `current_treatment` | Self-report (0/1) of currently being in mental-health therapy. Collected for descriptive purposes; not used in the eligibility decision. |
| Qualtrics internal | `sc0`, `sc1`, `sc2` | Qualtrics-computed scoring variables, fully redundant with `attention_check`, `oasis`, and `phq` respectively. Retained as exported. |

### Notes

- **Attention-check items are excluded from the scored totals.** `wsas_4` is not included in the WSAS scoring used for the inclusion decision; `phq_3` is not included in `phq`. The raw item values are retained in the file for transparency.
- **`b_1`–`b_4` are conditional.** Only participants who endorsed `a` (gateway trauma question) were asked the PCL-5 items. `NA` in these columns for participants with `a = 0` is expected and not a data-quality issue.
- **`screener_1`–`screener_7` are all `NA`** in this data. These fields were reserved in the Qualtrics form but not populated in this collection round.
- **Qualtrics-computed `sc*` variables are redundant** with the corresponding computed totals (`attention_check`, `oasis`, `phq`, and the sum of `b_1`–`b_4`). They are retained as part of the raw Qualtrics export but add no information beyond the named total columns.
- **Full item wording.** The original Qualtrics questionnaire, including exact item wording, response options, and instruction items, is provided in the Open Materials document accompanying this repository.

---

---

## `diagnostics/outlier_summary.csv`

One row per fitted Bayesian model. Summarizes pointwise WAIC and PSIS
diagnostics for that model. Produced by `scripts/diagnostics_outliers.Rmd`.

| Variable | Type | Description |
|---|---|---|
| `model` | character | Model identifier. Format: `{family}.{measure}` (e.g., `growth.oasis`, `change.dts`). For mechanism models, the suffix is the outcome whose pointwise log-likelihood is available from the fit — typically `{measure}.oasis` (see note below). |
| `n_obs` | integer | Number of observations in the model fit. For mechanism models, this reflects the x-outcome only (see note below). |
| `n_participants` | integer | Number of unique participants contributing observations to the model. |
| `max_p_waic` | numeric | Largest pointwise WAIC penalty observed in the model. Larger values indicate observations the model finds harder to predict. |
| `max_pareto_k` | numeric | Largest pointwise PSIS Pareto *k* observed in the model. |
| `n_waic_gt1` | integer | Number of observations with WAIC pointwise penalty > 1. |
| `n_k_gt.5` | integer | Number of observations with PSIS Pareto *k* > 0.5. |
| `n_k_gt.7` | integer | Number of observations with PSIS Pareto *k* > 0.7. |
| `n_both` | integer | Number of observations meeting the preregistered joint flag (WAIC penalty > 1 **and** PSIS Pareto *k* > 0.5). |
| `pct_both` | numeric | `n_both` as a percentage of `n_obs`, rounded to three decimal places. |

**Note on mechanism models.** Multivariate mechanism models fit two outcomes
(OASIS on the x-side and a candidate process on the m-side). `ulam`'s
`log_lik = TRUE` option generates pointwise log-likelihoods only for the first
declared likelihood statement, so diagnostics for these rows cover the x-side
outcome only. Pointwise diagnostics for each process outcome are available in
the corresponding univariate `change.*` model.

---

## `diagnostics/outliers_flagged.csv`

One row per observation meeting the preregistered joint flag (WAIC penalty > 1
**and** PSIS Pareto *k* > 0.5) across all models. Produced by
`scripts/diagnostics_outliers.Rmd`.

| Variable | Type | Description |
|---|---|---|
| `id` | integer | Participant identifier. Matches `id` in `data_long.csv` (a sequential serial number, not the real Prolific ID). |
| `condition` | character | Condition label at model-fit time: `treatment` (WET), `control` (NIW), or `waitlist`. May be `NA` for observations drawn from a data frame that was not joined with demographics (e.g., OASIS observations in mechanism-model splits). |
| `wave` | integer | Measurement wave. Matches `wave` in `data_long.csv`. |
| `measure` | character | Measure label. Matches `measure` in `data_long.csv`. |
| `question` | integer | Item number within the measure. Matches `question` in `data_long.csv`. |
| `value` | integer | Response on the item's original scale. Matches `value` in `data_long.csv`. |
| `model` | character | Identifier of the model in which this observation was flagged. Same format as in `outlier_summary.csv`. |
| `row_id` | integer | Position of this observation within the model's input data frame. Internal bookkeeping; useful only for reproducing the diagnostic. |
| `p_waic` | numeric | Pointwise WAIC penalty for this observation in this model. |
| `pareto_k` | numeric | Pointwise PSIS Pareto *k* for this observation in this model. |
| `flag_waic` | logical | `TRUE` if `p_waic` > 1. Always `TRUE` in this file by construction. |
| `flag_psis` | logical | `TRUE` if `pareto_k` > 0.5. Always `TRUE` in this file by construction. |
| `flag_both` | logical | `TRUE` if both flags are set. Always `TRUE` in this file by construction. |
| `flag_psis_severe` | logical | `TRUE` if `pareto_k` > 0.7. Indicates the stricter PSIS threshold. |

Because this file is filtered to the joint flag, `flag_waic`, `flag_psis`, and
`flag_both` are all `TRUE` for every row. They are retained so that the file
has the same schema as `outlier_diagnostics_all.rds` (the unfiltered
per-observation dataset saved alongside it).

An observation may appear in this file multiple times if it was flagged in
more than one model (e.g., an OASIS response that meets the flag in the
primary growth model and in several mechanism models, which all include the
same OASIS observation set).

---
