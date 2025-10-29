# Coding Manual – Treatment impact

Each participant is scored on **three separate binary (0/1) variables**:

1. **No Effect (NE)** – whether the participant explicitly said the intervention made *no difference*.
2. **Change in Life Quality (CQ)** – whether they described *feeling better* (e.g., calmer, less anxious, more confident) regardless of *how* it happened.
3. **Used Strategy (US)** – whether they described *actively using* a specific skill, technique, or coping strategy learned in the program.

> A participant can receive **1 (present)** or **0 (absent)** for **each** dimension.
> These codes are **not mutually exclusive** – a response may have multiple 1’s or all zeros.

---

## 1. **No Effect (NE)**

**Code 1:** Participant clearly states there was **no noticeable benefit** or no change.

* Phrases: *“No change,” “Didn’t affect me,” “No added benefit,” “Not sure it helped.”*

**Code 0:** Any other response (including feeling better or using strategies).

> **Examples – Code 1:**
> • “No change.”
> • “It hasn’t affected my life.”
> • “No meaningful changes, anxiety still the same.”

---

## 2. **Change in Life Quality (CQ)**

**Code 1:** Participant reports an **improvement in wellbeing, emotions, confidence, or functioning**, even if they do **not mention any technique**.

* Indicators: *calmer, less anxious, more relaxed, more confident, less worried, improved mood, more sociable, more optimistic.*

**Code 0:** No mention of improved wellbeing or quality of life.

> **Examples – Code 1:**
> • “I am calmer now.”
> • “I feel slightly less anxious in social situations.”
> • “I have been able to become a better person.”
> • “A couple of family members say I seem happier and friendlier.”

---

## 3. **Used Strategy (US)**

**Code 1:** Participant describes **actively applying a concrete strategy or coping skill**—especially one learned in the intervention (e.g., journaling, exposure practice, rational questioning, visualization).

* Indicators: *writing about fears, imagining scenarios repeatedly, confronting a trigger, using a spider-catching tool, grounding, reframing thoughts.*

**Code 0:** No description of active strategy use.

> **Examples – Code 1:**
> • “Writing down my fears helped me enormously.”
> • “I faced a fear of speaking up to request a pay rise.”
> • “I visualised any anxiety as explained in the study.”
> • “I rehearsed the scenario in my head before meetings.”

---

## **Coding Rules**

1. Read the full response.
2. For each dimension (**NE, CQ, US**), assign **1 = present** or **0 = absent**.
3. If a response contains evidence for both *No Effect* and *Used Strategy* (e.g., “I tried writing but it didn’t help”), code both: **NE = 1, US = 1, CQ = 0**.
4. If the participant simply wrote “NA” or left it blank → all **0** unless they explicitly said *no effect* → then **NE = 1**.
5. Do not infer benefits or strategy use that are not stated.

---

## **Data Structure Example**

| ID | Raw Response                             | NE | CQ | US | Notes                         |
| -- | ---------------------------------------- | -- | -- | -- | ----------------------------- |
| 01 | “No change.”                             | 1  | 0  | 0  | Explicit no-effect            |
| 02 | “I feel calmer now.”                     | 0  | 1  | 0  | Feeling better only           |
| 03 | “I wrote down my fears and felt calmer.” | 0  | 1  | 1  | Both benefit & strategy       |
| 04 | “I tried writing but it didn’t help.”    | 1  | 0  | 1  | Tried strategy but no benefit |

---

## **Quality Assurance**

* Double-code ~20% of responses to calculate **Cohen’s κ** for each binary code.
* Resolve discrepancies by discussion.
* Maintain an **examples log** of tricky cases.

---

Would you like me to generate an **Excel/CSV template** with these three binary columns plus notes for judges to use directly?
