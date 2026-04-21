# PRD: Open Science Badge Compliance (CPX-25-0117)

## Goal

Achieve approval for:
- Preregistered Badge (with TC if needed)
- Open Data Badge
- Open Materials Badge

By ensuring:
1. Manuscript transparently reflects preregistration and deviations
2. Repository supports reproducibility
3. OSF contains an immutable registered snapshot
4. A strict, point-by-point response document is built alongside fixes

---

# Core Strategy

- Prioritize transparency over perfection
- Treat deviations as:
  - explicit
  - minimal
  - non-defensive
- Build response document incrementally during implementation

---

# Workstreams

## 1. Preregistration Alignment (Manuscript)

### 1.1 Deviation Audit (Core Artifact)
Goal: Create full mapping between preregistration and manuscript

Output:
- Table:
  - preregistered plan
  - actual implementation
  - manuscript status
  - required action

Time: 2–3h

---

### 1.2 Stopping Rule
Facts:
- ROPE + increments of 30 → used
- Stopped based on rule
- End-of-treatment → null
- Follow-up → strong effect

Action:
- Add:
  - preregistered stopping rule
  - confirmation it was applied
  - explicit note: follow-up ≠ preregistered endpoint

Time: ~30m

---

### 1.3 Outlier Handling (WAIC / PSIS)
Decision: Proper implementation now (post hoc, labeled)

Action:
- Run diagnostics (WAIC, PSIS)
- Report:
  - preregistered plan
  - post hoc implementation
  - results

Time: ~1–2h

---

### 1.4 Measure Discrepancies

Corrections:
- MCQ → preregistered
- BSAM → preregistered

Additions:
- Credibility → manipulation check
- CTSI → intervention component

Omission:
- iBAT → collected but not analyzed (must be mentioned)

Time: ~1–2h

---

### 1.5 Coding Details
Status: reporting omission

Action:
- Add minimal sentence:
  - zero-based time
  - contrast coding

Time: ~20–30m

---

### 1.6 Model Deviation
Facts:
- Piecewise model decided pre-results
- Extension of preregistered model
- WAIC/PSIS used diagnostically

Action:
- State prereg vs implementation
- Justify time structure
- Clarify WAIC/PSIS usage

Time: ~1h

---

## 2. Open Data (Repository)

### 2.1 Codebooks
Files:
- data_long
- demographics
- screening
- pid.condition

Each includes:
- variable name
- description
- coding

Time: ~2–4h

---

### 2.2 README
Status: exists

Action:
- Light audit for clarity

Time: ~30m

---

## 3. Open Materials

### 3.1 Materials Document
Includes:
- instructions
- survey flow
- randomization
- timing
- measures (full or referenced)
- response anchors

Time: ~2–3h

---

## 4. OSF Compliance

### 4.1 Sync GitHub → OSF
Time: ~20–30m

### 4.2 Register OSF Project
Time: ~15m

### 4.3 Update T&O Section
Include:
- OSF link
- data/code/materials description
- note on copyrighted content

Time: ~20–30m

---

## 5. Response Document

Format:

[Quoted comment]

Response:
<what was done>

Location:
<where to find it>

Workflow:
- Build during implementation

Time: ~1–1.5h

---

# Execution Order

1. Deviation audit
2. Manuscript fixes
3. Codebooks + Materials
4. OSF sync
5. OSF registration
6. T&O update
7. Finalize response

---

# Risk Areas

- Follow-up vs prereg endpoint
- iBAT omission
- Outlier handling transparency

---

# Estimated Effort

- Focused: ~10–14 hours
- Comfortable: ~15–20 hours

---

# Success Criteria

- All prereg mismatches addressed
- Codebooks complete
- Materials reproducible
- OSF registered
- Response document complete
