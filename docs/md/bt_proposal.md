---
geometry: "margin=2.5cm"
lang: cs
header-includes:
  - \pagenumbering{gobble}
---

# Návrh zadání bakalářské práce

## Metody generování kódu a jejich využití

Primárním cílem práce je zmapovat a porovnat různé metody a technologie, které jsou v současnosti používané pro generování kódu zejména malými překladači, a na základě těchto srovnání navrhnout a implementovat vlastní malý překladačový backend provádějící základní optimalizace. Výsledný backend bude koncipován s ohledem na rovnováhu mezi praktickou využitelností jeho výstupů a komplexností celého systému tak, aby bylo možné ho využít například pro vzdělávací účely. Vzhledem k cíli využít projekt pro účely výuky vyvstávají i další požadavky na jeho architekturu v podobě snadné rozšiřitelnosti a integrovatelnosti do jiných projektů tak, aby bylo možné ho použít při tvorbě vlastního překladače například v rámci kurzů, jako jsou principy překladačů. Oba tyto architektonické požadavky budou v práci demonstrovány, a to implementací generování kódu pro dvě různé cílové platformy v podobě RISC-V a x86 a vytvořením jednoduchého překladače integrací s frontendem, který bude svým rozsahem přibližně odpovídat zkoumaným malým překladačům či univerzitním projektům. Implementační část práce pak bude navazovat na ročníkový projekt, v jehož rámci vznikl jednoduchý jednoprůchodový generátor kódu pro vlastní stack-based reprezentaci.
