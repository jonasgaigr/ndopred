# Refined generation lengths

Proposed replacement for the 1-year placeholder of AOPK's decision of 2026-10-15
(`inst/extdata/generation_lengths.csv`). Built by `build.R` into two tables:

| File | Rows | Content |
|---|---|---|
| `inst/extdata/generation_lengths_species.csv` | 698 | every vertebrate (590) and every Habitats Directive taxon (108 non-vertebrates), species-specific |
| `inst/extdata/generation_lengths_clades.csv` | 620 | everything else: one `group` default per NDOP group, plus `family` and `genus` rows where the value differs (genus rows: trees in mixed families, invertebrate genera above 3.3 years, vertebrate genus medians) |
| `inst/extdata/generation_lengths_genus_family.csv` | 3,362 | genus to family: AOPK checklist for plants, fungi, lichens and bryophytes; the species table for vertebrates |

Used through `generation_length_for(..., set = "refined")`. It is opt-in: the 1-year placeholder stays the default until AOPK adopts the proposal (see `?generation_lengths_refined`).

## What matters

A generation length of up to 3.3 years gives the same periods as 1 year (criterion A: 10 years; C1: 3, 5, 10 years).
Only rows above 3.3 change an assessment. That is 444 of the 698 species rows, mostly vertebrates, and in the clade table fungi, lichens, bryophytes, perennial and woody plants, large bivalves, crayfish, ants and a few insect families with multi-year larvae.

## Sources and method

Definitions follow Red List Guidelines §4.4 (v16).

- **Birds (389).** BirdLife International (2025) *Generation lengths of the world's birds*, v3.1 (August 2025), the method of Bird et al. (2020). The species set is the ČSO checklist (`cso_birds.txt`, fkcso.cz) together with the bird names in the NDOP snapshot (`ndop_bird_names.txt`). Rows use the NDOP name where NDOP uses one, otherwise BirdLife's. Other names go in `synonyms`. *Corvus cornix*, *Acanthis cabaret*, *A. hornemanni* and *Saxicola rubicola* are lumped by BirdLife and carry the value of the species they are lumped into.
- **Other vertebrates (201).** Option 3: GL = AFR + z × (longevity − AFR), with z = 0.29 (Pacifici et al. 2013). The parameters are in `vertebrates_life_history.csv`:
  - AFR is the age at first reproduction in the IUCN age convention: 0 when the species first breeds by 12 months, rounded up in borderline cases.
  - Longevity is the maximum recorded in the wild, not in captivity.
  - The values were cross-checked against the Amniote, AnAge and AmphiBIO databases, whose captive maxima often inflate GL (e.g. *Emys* 120 years → 43).
  - Lampreys and the eel are semelparous, so GL is the age at spawning. The eel takes 13 years from its IUCN assessment (Pike, Crook & Gollock 2020).
  - Pacifici's own mammal table could not be downloaded: Dryad refuses automated access.
- **HD non-vertebrates (108).** The list comes from three sources: AOPK's EVD code list (`host_naturecz/Data/Input/cis_evd_perioda.csv`, 131 taxa, which holds only Annex II taxa), the Czech Article 17 species in the EEA service, and Annex IV/V taxa occurring in Czechia. Methods (`hd_taxa_life_history.csv`):
  - iteroparous taxa use option 3;
  - semelparous insects use their development time;
  - ephemerals with seed or egg banks use option 6;
  - bryophytes and a few others use a stated life-strategy estimate.
- **Fungi.** Dahlberg & Mueller (2011), as quoted in the fungal red-list guidelines. Their periods are three generations: ectomycorrhizal and grassland fungi 50 years (GL 17), wood-inhabiting 20–50 years (GL 7–17 by wood durability), litter fungi 20 years (GL 7).
- **Lichens.** 7 years for epigeic or asexually dispersed species, 17 for sexually dispersed epilithic ones (Estonian red-list practice). The default is 10.
- **Vertebrate clades.** These are medians of the species table, by family and by NDOP group, for taxa not in it.

## Confidence and review

- The BirdLife values, the eel's 13 years and the fungal conventions come from published sources.
- The other vertebrates use one formula on curated parameters, so they are consistent but only approximate. z = 0.29 was fitted for mammals and is applied to every class.
- Expert estimates for review are flagged in the `method` column. They are the bryophytes, *Trichomanes*, the long-lived plants (Cypripedium 24.5, Pulsatilla, Ligularia, Lycopodiaceae) and *Margaritifera* (36.8).
- Bergamini et al. (2019) set out bryophyte generation-length guidance but could not be read; the bryophyte values should be checked against it.
- Family values for plants and invertebrates describe the typical life form. Long-lived taxa inside short-lived families need their own rows.

## Matching

The lookup searches within the taxon's NDOP group, which keeps homonyms apart (*Prunella* the bird vs *Prunella* the plant). The order is:

1. Species table: the name, a synonym, or the binomial of an infraspecific name.
2. Genus row.
3. Family of the genus, then the family row. NDOP exports carry no family, so the family comes from `generation_lengths_genus_family.csv`. Invertebrate families are not looked up; where one differs from its group default, its genera have rows.
4. The NDOP group default.

A value given for one taxon (argument, app field, evidence file) still wins.

## Time depth

The derived comparison window does not start before `time_depth_start` (2000, a convention in `ndopred_config()`). This is the start of the effort layers and of AOPK's D5 windows.

- For reference year 2026, every generation length over 3.3 years keeps 2000–2016. The generation length then changes the C1 periods but not the trend window.
- The trend issue says that the change covers fewer years than the criterion A period. A steady decline measured that way is understated.
- Lowering the limit (e.g. `time_depth_start = 1990`) lets long-lived taxa use older records, where a group's pre-2000 coverage allows.
