# Feature Relationship Network — Principles & Algorithm

## Overview

The feature network engine detects relationships among LC-MS features (peaks) and groups them into **empirical compounds** for downstream metabolomics analysis. It is plant-metabolomics focused, with dictionaries tailored to plant secondary metabolites.

A **feature** here is a chromatographic peak characterized by (m/z, retention time, abundance). The engine detects three relationship classes:

1. **Isotope edges** — same compound, different isotopic composition (e.g., [M] and [M+1])
2. **Adduct edges** — same compound, different adduct forms (e.g., [M+H]+ and [M+Na]+)
3. **ISF edges** (In-Source Fragments) — fragments of the same compound produced during ionization (e.g., neutral loss of H₂O, CO₂, sugar moieties)

---

## Algorithm Pipeline

### Step 1 — Candidate search via sliding mass-difference window

Features are sorted by RT, then m/z. For each feature `i`:

1. **RT window**: `findInterval()` (O(log n)) selects candidates within `rt[i] ± rt_tolerance`.
2. **Mass filter**: only features with `m/z > m/z[i]` are kept.
3. **Dictionary match**: for each mass difference in the dictionary (isotope / adduct / ISF), the observed delta `m/z[high] − m/z[low]` is tested against the expected delta ± ppm tolerance.

This yields candidate pairs `(low_mz_feature, high_mz_feature)`.

### Step 2 — Abundance correlation filter

For each candidate pair, the Pearson correlation of abundance profiles across all samples is computed:

- **Fast path**: if no missing values exist, uses matrix multiplication `source_row %*% t(candidate_rows)` after row-centering and scaling — O(1) per pair after cache.
- **Fallback**: `stats::cor(use = "pairwise.complete.obs")` for data with NAs.

Pairs with correlation below `cor_cutoff` (default 0.7) are discarded.

### Step 3 — Edge confidence scoring

A composite score (0–1) from three components:

```
confidence = 0.45 × mass_score + 0.20 × rt_score + 0.35 × cor_score
```

- `mass_score = max(0, 1 − mz_error_ppm / ppm_tolerance)`
- `rt_score = max(0, 1 − rt_diff / rt_tolerance)`
- `cor_score = max(0, abundance_correlation)`

Weights prioritize mass accuracy (45%) and abundance correlation (35%) over RT matching (20%), since RT in LC-MS is less reproducible than mass accuracy.

### Step 4 — QC ratio stability annotation

For each edge, the ratio `abundance[high] / abundance[low]` is computed in QC samples. The RSD (relative standard deviation) of this ratio across QC injections measures consistency. A stable ratio suggests a genuine physicochemical relationship rather than a spurious correlation.

### Step 5 — Isotope-specific intensity filter

Isotope edges are additionally filtered: the mean abundance ratio `[M+1] / [M]` must be ≤ `isotope_intensity_ratio_max` (default 0.8). This prevents mislabeling of near-equal-abundance features as isotopes.

---

## Empirical Compound Collapse (`collapse_to_pseudo_area`)

After edge detection, connected components in the network graph represent putative **empirical compounds** (a parent metabolite + its isotopologues + adducts + fragments). For each sub-network:

1. The feature with the **highest mean abundance** is selected as the **base feature**.
2. **PC1 pseudo-area**: expression data of all features in the sub-network are standardized (center + scale) and projected onto the first principal component. This captures the shared abundance pattern while suppressing noise from individual features. The PC1 scores become the **pseudo-area** of the empirical compound.

This produces a single abundance vector per empirical compound, suitable for downstream statistical analysis.

---

## Plant-Specific Dictionaries

Two built-in dictionaries support plant metabolomics:

- **Neutral loss table**: H₂O, CO₂, NH₃, CH₂O₂, hexose (−162 Da), pentose (−132 Da), malonyl (−86 Da), SO₃, H₂SO₄, H₃PO₄, etc.
- **Fragment ion table**: diagnostic fragments for flavonoids (quercetin, kaempferol, apigenin aglycones), phenolic acids (caffeic, ferulic, coumaric, gallic acid fragments), phospholipids, fatty acids, amino acid immonium ions.

Both can be overridden with custom tables via `adduct_table` and `neutral_loss_table` parameters.

---

## Implementation Notes

- **Storage**: The network edge table is stored in `object@other_files$feature_network`, keeping compatibility with the `massdataset` S4 class without modifying its slot definition.
- **Graph conversion**: `as_feature_igraph()` converts the edge table to an igraph directed graph, with vertex attributes from `variable_info`.
- **Performance**: The RT window search uses `findInterval()` (O(log n)) rather than scanning all features. The Pearson fast path avoids the overhead of `stats::cor()` per pair (which would be O(n²) calls without the cache).

---

## References

- The ISF detection strategy is adapted from the MetMiner v1 pipeline (Wang et al., 2024, JIPB).
- Neutral loss and fragment ion dictionaries are curated from plant metabolomics literature and MS/MS spectral libraries.
