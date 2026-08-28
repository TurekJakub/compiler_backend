---
geometry: "margin=2.5cm"
lang: cs
header-includes:
  - \pagenumbering{gobble}
---

# Návrh zadání bakalářské práce

## Metody generování kódu a jejich využití

Primárním cílem práce je zmapovat a porovnat různé metody a technologie, které jsou v současnosti používané pro generování kódu zejména malými překladači, a na základě těchto srovnání navrhnout a implementovat vlastní malý překladačový backend provádějící základní optimalizace. Výsledný backend bude koncipován s ohledem na rovnováhu mezi praktickou využitelností jeho výstupů a co nejnižší komplexností celého systému tak, aby bylo možné ho využít například pro vzdělávací účely. Vzhledem k cíli využít projekt pro účely výuky vyvstávají i další požadavky na jeho architekturu v podobě snadné rozšiřitelnosti a integrovatelnosti do jiných projektů tak, aby bylo možné ho použít při tvorbě vlastního překladače například v rámci kurzů, jako jsou principy překladačů. Oba tyto architektonické požadavky budou v práci demonstrovány, a to implementací generování kódu pro dvě různé cílové platformy v podobě RISC-V a x86 a vytvořením jednoduchého překladače integrací s frontendem, který bude svým rozsahem přibližně odpovídat zkoumaným malým překladačům či univerzitním projektům. Implementační část práce pak bude navazovat na ročníkový projekt, v jehož rámci vznikl jednoduchý jednoprůchodový generátor kódu pro vlastní stack-based reprezentaci.

## Didactically simple compiler object-code generator

LLVM has emerged as a de-facto standard for implementing back-end code generators in compilers. While versatile, the sheer complexity of LLVM system prevents its use in many areas; for example, the library is too big for running on small microcontrollers, and the imposed learning gap prevents its use in education.

The aim of this thesis is to explore LLVM alternatives present in current compilers, and produce a review of code-generation techniques that are used contemporarily, mainly from the perspective of the input language, optimization possibilities, and, for thesis purposes, code size. The thesis will gather some of the gained insight to produce a minimal, didactic replacement for LLVM that can be used in small projects, such as the toy compilers as used in compiler-related courses at the faculty.

The thesis should demonstrate that the resulting code generator posesses the desired didactic properties, such as addition of new optimization passes, portability to new instruction sets, and extensions of the input language with new supported functionality. It is expected that the feature set of the delivered solution will be minimized to keep the code reasonably small (and repurposable) while still delivering practically applicable results.
