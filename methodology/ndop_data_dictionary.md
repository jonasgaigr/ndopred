# NDOP export: data dictionary for ndopred

Phase 0 deliverable, 2026-10-15. It describes what an `rndop::ndop_download()` export actually contains, what ndopred does with each field today, and what the field could support under the IUCN criteria.

## How this was built, and its limits

For most of the audit `login.nature.cz` accepted TCP connections but never answered (2026-10-14 23:50 to 2026-10-15 00:05, repeated attempts). The dictionary is therefore built mainly from three cached exports already on this machine, and then checked against one live export (below):

| Label | Source | Records | Route |
|---|---|---|---|
| **S** | Scarabaeidae, cached 2026-09-03 | 46,144 | `rndop::ndop_download()` table |
| **A** | Annex II amphibians, cached 2026-10-03 | 60,331 + 32,342 localisation geometries | `rndop::ndop_download(locations = 2)` |
| **P** | *Phengaris teleius* 2019–2024, *P. nausithous* 2012–2018 | 2,190 / 3,334 | AOPK internal CSV export (not rndop) |

The column-name mapping between the AOPK export and rndop was cross-checked against an existing inventory (`amphibian-condition-czechia/inventory/12_column_mapping_export_vs_rndop.csv`).

**Live confirmation.** The login came back at 00:05, and one live export was pulled: *Onthophagus medius*, 25 records, downloaded 2026-10-15 00:06 with `locations = 2`. It has exactly the same 45 columns, no `ROK` or `CELED`, integer `DATUM_OD`, unique `ID_NALEZ`, and a geometry layer of 10 points and 6 polygons. 3 of its 25 records have precision worse than 2 km and 2 are date ranges; `VEROH = 0` throughout, with `GARANCE` blank or "Návrh validace". The larger live downloads were stopped once the schema was confirmed.

Meanings marked *unverified* are inferred from the data and need confirming against an AOPK code list.

The cached files contain observer names and exact coordinates. They must not be copied into this repository.

## Fields in the rndop export (45 columns, identical in S and A)

Fill rate is the share of non-blank values. "Used now" describes the working tree on branch `bulk-assesment`.

### Identity and taxonomy

| Field | Fill S / A | Meaning | Used now | IUCN relevance / planned use |
|---|---|---|---|---|
| `ID_NALEZ` | 100 / 100 | NDOP record ID; unique in both exports | no | **Key for the evidence layer**: record-level flags (misidentification, vagrant, duplicate) and the species card's list of records used |
| `PORADI` | 100 / 100 | Row order of the export | no | None. Not stable between downloads, so never use it as a key |
| `DRUH` | 100 / 100 | Latin name as recorded | no | **Taxonomic concept.** The rndop query is an unanchored `grep`, so `"Bombina bombina"` also returns `"Bombina bombina × variegata"` (229 records in A). A also contains `"Triturus cristatus s. l."` (116). Needs an accepted-name mapping and flags (§2.1.1; Regional Guidelines II.6) |
| `CESKE_JMENO` | 79 / 100 | Czech name | no | Display only |
| `KAT_TAX` | 100 / 100 | Taxonomic group ("Brouci", "Obojživelníci", …) | yes: picks whether C/D are evaluated (app.R) | Group-level defaults: "current" window, generation length |
| `TAX_POZN` | 7 / 5 | Free-text taxonomic note | no | Expert review of identification |

### Time

| Field | Fill S / A | Meaning | Used now | IUCN relevance / planned use |
|---|---|---|---|---|
| `DATUM_OD` | 100 / 100 | Start date, integer `YYYYMMDD` | yes: year = first 4 digits | Windows. There is **no `ROK` column**, so the code paths that read `ROK` never fire on real data |
| `DATUM_DO` | 100 / 100 | End date, integer `YYYYMMDD` | **no** | **Date precision.** In S, 21.5 % of records have `DATUM_DO ≠ DATUM_OD`; in 414 the end year is at least two years after the start year, and in 33 more than ten. These are filed under the start year. Needs `year_min`/`year_max` and a window-assignment rule |
| `CAS` | 2 / 2 | Time of day | no | Detection covariate (T2 models) |
| `DATI_INSERT`, `DATI_UPDATE`, `UPDATOR` | 100 / 17 / 17 (S) | Database insert/update date and updater ID | no | **Reproducibility.** `DATI_INSERT` allows filtering a later download back to a snapshot date, but edits after insert cannot be undone that way |

### Space

| Field | Fill S / A | Meaning | Used now | IUCN relevance / planned use |
|---|---|---|---|---|
| `X`, `Y` | 100 / 100 | S-JTSK / Krovak East North (EPSG:5514). Set to `NA` by rndop for hidden localities ("Skrytá lokalizace") | yes: the only geometry used | EOO/AOO. For lines and polygons this is a single representative point (how it is derived is *unverified*) |
| `CXPRESNOST` | 100 / 100 | Positional precision in metres. **0 means an exact position** (confirmed by AOPK, 2026-10-15). For polygon records it reflects the polygon's size (341–3,220 m in the live sample) | yes (0.4.0): `precision_m`, AOO/EOO bounds | **AOO uncertainty** (§3.2, §4.10.8). In S, 6.2 % of records are worse than 2 km and 96 are worse than 5 km. 0 occurs in 19 % (S) and 32 % (A) of records |
| `ID_LOKAL` | 100 / 100 | Localisation ID. **Equals `idx_nd_lok` in the rndop geometry layer** (all 25 live records matched) | yes (0.4.0): joins record polygons and lines; also the fallback locality key in `calculate_pop_metrics()` | Precision-aware AOO from the real geometry |
| `NAZ_LOKAL` | 100 / 100 | Locality name | yes: used as **subpopulation** in C2a | A site name, not a subpopulation (§4.2) |
| `KATASTR` | 99.7 / 99.8 | Cadastral area name | yes: blank → record deleted | Blank probably means outside Czechia (*unverified*). Replace with an explicit spatial in-region test and flag |
| `SITMAP` | 100 / 100 | KFME mapping-grid square | no | Coarse-scale fallback; the scale-correction method of §4.10.5 if precision is square-level |
| `CXLOKAL_Z` | 15 / 31 | Altitude (m) | no | Habitat masks and covariates |
| `CXLOKAL_POZN`, `UMIST_NAL`, `POP_BIOT` | 5–26 | Locality note, microhabitat, habitat description | no | Expert context for habitat decline B(b)(iii); not machine-usable as is |

**Geometry layer** (A, `locations = 2`): 32,342 localisations: 22,641 points, 9,504 polygons or multipolygons, 197 lines. Polygon median 0.013 km², 90th percentile 2.0 km², maximum 137 km². **668 polygons are larger than one 4 km² AOO cell.** ndopred never requests this layer and reduces every record to a point.

### Detection, counts and life stage

| Field | Fill S / A | Meaning | Used now | IUCN relevance / planned use |
|---|---|---|---|---|
| `NEGATIV` | 100 / 100 | 1 = negative finding (searched for, not found) | yes: **record deleted** | **Non-detections for trend and occupancy models** and for RE/CR(PE) survey evidence (§11.3). 168 in S (0.4 %), 10,993 in A (18 %), 797 in P *teleius* (36 %) |
| `POCET` | 56 / 54 | Count | yes, as mature individuals | C/D only with a valid census (review Table 5, T3) |
| `ODHAD` | 99.8 / 100 | 1 = count is an estimate | no | Data-quality qualifier (Table 3.1) |
| `POCITANO` | 71 / 73 | What was counted: "jedinci", "imaga", "samci", "larvy", "trus larev", "osídlené stromy", "mrtví jedinci", … | yes: `get_mature_terms()` filter | Mature individuals (§4.3); traces and dead individuals are presence evidence only |
| `REL_POC` | 16 / 19 | Relative abundance class | no | Abundance index candidate (T3 only) |
| `POKRYVN` | 0 / 0 | Cover (plants) | no | Empty for these groups |
| `KODHN` | 0 / 0 | *Unverified* (possibly a breeding code) | no | Empty for these groups |

### Provenance and verification

| Field | Fill S / A | Meaning | Used now | IUCN relevance / planned use |
|---|---|---|---|---|
| `VEROH` | 100 / 100 | Credibility after the guarantor's review (code list confirmed by AOPK, 2026-10-15): **0** not yet validated, **1** credible, **3** less credible, **6** erroneous, to be deleted (deleted once guaranteed), **9** erroneous, to be corrected. After the guarantee closes only 1 or 3 remain; while it is open the value is 0 even if a validation mark has been proposed. Seen: 0, 1, 3. Non-zero exactly when `GARANCE` = "Garantováno" (S: 13,486 of 13,486; A: 59,870 of 59,870) | yes (0.4.0): 6 and 9 excluded; 3 excluded unless more than 10 % of the taxon's records carry it, then kept and flagged; 0 and 1 used | ndopred ≤ 0.3 deleted 3 and 9, which matches the code list. The `Dung_beetle_expansion` reading (1 = doubtful) is wrong |
| `GARANCE` | 59 / 99 | Guarantee status: "Garantováno", "Návrh validace", blank | no | Verification flag |
| `VALIDACE` | 30 / 0.04 | Validation verdict: "věrohodný záznam", "záznam k opravě" (47), "méně věrohodný záznam" (6), "chybný záznam" (1) (counts from S) | **no** | **"chybný záznam" (erroneous record) and "méně věrohodný záznam" (less credible) are used as if valid.** Should become flags |
| `VAL_POZN`, `GAR_POZN` | <1 | Validation/guarantee notes | no | Reason text for flags |
| `AUTOR`, `ZAPSAL` | 100 / 100 | Observer(s), person who entered the record | no | Personal data. Detection covariate (observer) in T2 models; keep out of published exports |
| `ZDROJ` | 100 / 100 | Source: publication citation, dataset or "Vlastní nálezy" (own records) | no | Duplicate detection (literature and collection records re-entered); survey grouping |
| `DAT_SADA` | 100 / 100 | Dataset ("Nálezová data ochrany přírody", "ND – Databáze BioLog", "iNaturalist – data ČR", …) | no | Source-type flag (citizen science, import, monitoring) |
| `PROJEKT` | 100 / 100 | Project | no | Identifies monitoring programmes that may support T2/T3 trends |
| `STRUKT_POZN` | 0.6 / 25 | Structured programme-specific note ("key: value, …"). For some monitoring it records site threats and prospects, e.g. "Negativní vlivy na lokalitě: …", "Perspektiva lokality: …" | no | **Possible evidence source** for threat-defined locations (§4.11) and habitat decline B(b)(iii), with expert confirmation only |

### Taxon-level attributes repeated on every record

| Field | Fill S / A | Meaning | Used now | Planned use |
|---|---|---|---|---|
| `REDLIST` | 32 / 100 | Current national Red List category (CR … RE) | no | Previous assessment for transfer rules (§2.2.1) and change-reason coding |
| `VYHLASKA` | 34 / 99 | Legal protection category (KO/SO/O) | no | Display only (not a Red List input, §2.4) |
| `EVD` | 8 / 99.6 | Habitats Directive annex (HD II, HD IV) | no | Display only |

## Fields that exist only in other export routes

These are **not** in the rndop export:

| Field | Seen in | Fill | Note |
|---|---|---|---|
| `KOMPLETNI_SEZNAM` | AOPK CSV (P) | 0 % non-zero | Complete-species-list flag. It would support the events layer, but it is unused in the samples seen |
| `METOD_SB` | AOPK CSV (P) | 2 % | Sampling method ("pozorování", "síť", "foto", …) |
| `ETOLOGIE`, `FENOFAZE`, `PATROVITOST` | AOPK CSV (P) | not profiled | Behaviour, phenophase, vegetation layer |
| `IDX_ND_AKCE` | AOPK export in the mapping inventory | not profiled | **Event (visit) ID.** It has no counterpart in rndop (mapping share 0), so records cannot be linked to visits through rndop |
| `PRESNOST`, `EVL`, `PROC_EVL`, `KOD_LOKALITY`, `IDX_ND_LOKALIZACE` | same | not profiled | Precision (portal form), Natura 2000 site, … |

## Consequences for the plan

1. **An events layer cannot be built from rndop alone.** There is no visit ID, and the complete-list flag is unused in the samples seen. T2 occupancy/detection models (review §7.3) therefore need either the AOPK export route with `IDX_ND_AKCE`, or external monitoring datasets identified by `PROJEKT`/`DAT_SADA`. AOPK decided (D4, 2026-10-15) that ndopred uses rndop only, so visits have to be rebuilt from `DATUM_OD`/`DATUM_DO`, `ID_LOKAL` and `AUTOR`, with `ZDROJ`, `DAT_SADA` and `PROJEKT` separating monitoring from opportunistic records.
2. **Precision-aware AOO is feasible**, and since 0.4.0 implemented: `CXPRESNOST` is fully populated (0 = exact), and the geometry layer comes with `locations = 2`.
3. **Verification follows the AOPK code list** (see `VEROH`). `VALIDACE` holds a *proposed* mark while the guarantee is open (`VEROH` = 0); it is flagged, not acted on.
4. **Negative findings are common in monitored groups** (18–36 %) and are the most valuable input for trends and extinction-probability evidence. They must be retained and flagged, not deleted.
5. **Taxon queries need anchoring.** Since 0.4.0 records whose `DRUH` is not exactly the queried name are flagged and excluded (decision D13); rndop's substring match is no longer trusted.
