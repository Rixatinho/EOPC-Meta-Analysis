# Search Strategies

**Manuscript**: The Early-Onset Pancreatic Cancer Paradox: A Systematic Review and Meta-Analysis (JNCI MS# JNCI-26-0532)
**PROSPERO**: CRD420261346604

Five databases were searched from inception through January 20, 2026, with no start
date restriction: PubMed, Embase, Web of Science, Cochrane Library, and Scopus.
Searches were limited to English-language records; non-PDAC pancreatic tumors
(neuroendocrine, solid pseudopapillary, IPMN, ampullary) were excluded via NOT blocks.
Reference lists of included studies and relevant reviews were hand-searched for
additional records. These strategies are identical to those presented on pages 2-4 of
the Supplementary Material.

## PubMed

```
#1 Pancreatic cancer (MeSH + Free text)
"Pancreatic Neoplasms"[MeSH Terms]
OR "pancreatic cancer"[tiab]
OR "pancreatic ductal adenocarcinoma"[tiab]
OR "pancreatic adenocarcinoma"[tiab]
OR "PDAC"[tiab]
OR "pancreas cancer"[tiab]
OR "cancer of the pancreas"[tiab]

#2 Early-onset / Young
"early-onset"[tiab]
OR "early onset"[tiab]
OR "young-onset"[tiab]
OR "young onset"[tiab]
OR "young adults"[tiab]
OR "young patients"[tiab]
OR "adolescent and young adult"[tiab]
OR "AYA"[tiab]
OR "under 50"[tiab]
OR "younger than 50"[tiab]
OR "below 50"[tiab]
OR "under 45"[tiab]
OR "under 40"[tiab]
OR "younger than 40"[tiab]

#3 Clinical / Treatment / Outcomes
"Survival"[MeSH]
OR "Prognosis"[MeSH]
OR "Treatment Outcome"[MeSH]
OR "survival"[tiab]
OR "prognosis"[tiab]
OR "mortality"[tiab]
OR "clinicopathological"[tiab]
OR "clinicopathologic"[tiab]
OR "clinical characteristics"[tiab]
OR "clinical features"[tiab]
OR "treatment pattern"[tiab]
OR "surgical resection"[tiab]
OR "chemotherapy"[tiab]
OR "overall survival"[tiab]

#4 NOT -- exclude non-PDAC tumors
"neuroendocrine"[tiab]
OR "islet cell"[tiab]
OR "solid pseudopapillary"[tiab]
OR "IPMN"[tiab]
OR "intraductal papillary"[tiab]
OR "ampullary"[tiab]
OR "ampulla of Vater"[tiab]

#5 Final: #1 AND #2 AND #3 NOT #4

Filters: Humans; English
```

## Embase

```
#1 Pancreatic cancer (Emtree + Free text)
'pancreas cancer'/exp
OR 'pancreatic ductal adenocarcinoma'/exp
OR 'pancreatic cancer':ti,ab
OR 'pancreatic ductal adenocarcinoma':ti,ab
OR 'pancreatic adenocarcinoma':ti,ab
OR 'PDAC':ti,ab
OR 'pancreas cancer':ti,ab

#2 Early-onset / Young
'early-onset':ti,ab
OR 'early onset':ti,ab
OR 'young-onset':ti,ab
OR 'young onset':ti,ab
OR 'young adult'/exp
OR 'young adults':ti,ab
OR 'young patients':ti,ab
OR 'adolescent and young adult':ti,ab
OR 'AYA':ti,ab
OR 'under 50':ti,ab
OR 'younger than 50':ti,ab
OR 'below 50':ti,ab
OR 'under 45':ti,ab
OR 'under 40':ti,ab
OR 'younger than 40':ti,ab

#3 Clinical / Treatment / Outcomes
'survival'/exp
OR 'prognosis'/exp
OR 'treatment outcome'/exp
OR 'survival':ti,ab
OR 'prognosis':ti,ab
OR 'mortality':ti,ab
OR 'clinicopathological':ti,ab
OR 'clinicopathologic':ti,ab
OR 'clinical characteristics':ti,ab
OR 'clinical features':ti,ab
OR 'treatment pattern':ti,ab
OR 'surgical resection':ti,ab
OR 'chemotherapy':ti,ab
OR 'overall survival':ti,ab

#4 NOT -- exclude non-PDAC
'neuroendocrine tumor'/exp
OR 'neuroendocrine':ti,ab
OR 'islet cell':ti,ab
OR 'solid pseudopapillary':ti,ab
OR 'IPMN':ti,ab
OR 'intraductal papillary':ti,ab
OR 'ampullary':ti,ab
OR 'ampulla of Vater':ti,ab

#5 Final: #1 AND #2 AND #3 NOT #4

Limits: [humans]/lim AND [english]/lim
NOT ('conference abstract'/it OR 'conference paper'/it)
```

## Web of Science

```
#1 Pancreatic cancer
TS=("pancreatic cancer" OR "pancreatic ductal adenocarcinoma"
OR "pancreatic adenocarcinoma" OR "PDAC"
OR "pancreas cancer" OR "pancreatic neoplasm*")

#2 Early-onset / Young
TS=("early-onset" OR "early onset" OR "young-onset" OR "young onset"
OR "young adults" OR "young patients"
OR "adolescent and young adult" OR "AYA"
OR "under 50" OR "younger than 50" OR "below 50"
OR "under 45" OR "under 40" OR "younger than 40")

#3 Clinical / Treatment / Outcomes
TS=("survival" OR "prognosis" OR "mortality"
OR "clinicopatholog*" OR "clinical characteristics"
OR "clinical features" OR "treatment pattern*"
OR "surgical resection" OR "chemotherapy" OR "overall survival")

#4 NOT -- exclude non-PDAC
TS=("neuroendocrine" OR "islet cell" OR "solid pseudopapillary"
OR "IPMN" OR "intraductal papillary" OR "ampullary")

#5 Final: #1 AND #2 AND #3 NOT #4

Refined by: Languages = English
Document Types = Article
```

## Cochrane Library

```
#1 Pancreatic cancer
MeSH descriptor: [Pancreatic Neoplasms] explode all trees
OR ("pancreatic cancer" OR "pancreatic ductal adenocarcinoma"
OR "pancreatic adenocarcinoma" OR "PDAC"):ti,ab,kw

#2 Early-onset / Young
("early-onset" OR "early onset" OR "young-onset" OR "young onset"
OR "young adults" OR "young patients"
OR "adolescent and young adult"
OR "under 50" OR "younger than 50" OR "under 45"
OR "under 40" OR "younger than 40"):ti,ab,kw

#3 Clinical / Treatment / Outcomes
MeSH descriptor: [Survival] explode all trees
OR MeSH descriptor: [Prognosis] explode all trees
OR MeSH descriptor: [Treatment Outcome] explode all trees
OR ("survival" OR "prognosis" OR "mortality"
OR "clinicopatholog*" OR "clinical characteristics"
OR "treatment pattern" OR "overall survival"):ti,ab,kw

#4 NOT -- exclude non-PDAC
("neuroendocrine" OR "islet cell" OR "solid pseudopapillary"
OR "IPMN" OR "intraductal papillary" OR "ampullary"):ti,ab,kw

#5 Final: #1 AND #2 AND #3 NOT #4
```

## Scopus

```
TITLE-ABS-KEY(
  ("pancreatic cancer" OR "pancreatic ductal adenocarcinoma"
   OR "pancreatic adenocarcinoma" OR "PDAC" OR "pancreas cancer")
  AND
  ("early-onset" OR "early onset" OR "young-onset" OR "young onset"
   OR "young adults" OR "young patients"
   OR "adolescent and young adult" OR "AYA"
   OR "under 50" OR "younger than 50" OR "under 45" OR "under 40"
   OR "younger than 40")
  AND
  ("survival" OR "prognosis" OR "mortality"
   OR "clinicopatholog*" OR "clinical characteristics"
   OR "clinical features" OR "treatment pattern*"
   OR "surgical resection" OR "chemotherapy" OR "overall survival")
  AND NOT
  ("neuroendocrine" OR "islet cell" OR "solid pseudopapillary"
   OR "IPMN" OR "intraductal papillary" OR "ampullary")
)
AND LANGUAGE(english)
AND DOCTYPE(ar)
```
