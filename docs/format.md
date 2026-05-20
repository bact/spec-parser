---
SPDX-FileCopyrightText: 2024-2025 SPDX Project
SPDX-License-Identifier: Community-Spec-1.0
---

# Markdown for SPDX 3 model specification

The SPDX 3 model is written in a constrained subset of Markdown, with
predefined headings and a YAML frontmatter block.

This document provides guidelines for writing model files in the specific
syntax used by spec-parser.

Following these guidelines ensures consistency and avoids rendering issues when
using spec-parser and MkDocs to generate model ontologies and model textual
specifications.

## Table of contents

- [Overview](#overview)
- [Directory organisation](#directory-organisation)
- [Naming convention](#naming-convention)
- [File content structure and formatting](#file-content-structure-and-formatting)
  - [Model file example](#model-file-example)
- [Syntax](#syntax)
  - [Common optional metadata fields](#common-optional-metadata-fields)
  - [Classes](#classes)
  - [Datatypes](#datatypes)
  - [Individuals](#individuals)
  - [Properties](#properties)
  - [Vocabularies](#vocabularies)
- [Verbal forms for expressions of provisions](#verbal-forms-for-expressions-of-provisions)
- [Writing style](#writing-style)
- [Translation](#translation)
- [Migrating from the old format](#migrating-from-the-old-format)
- [Checking if everything is ok](#checking-if-everything-is-ok)

## Overview

Each model element (class, datatype, individual, property, and vocabulary)
is defined in a distinct file.

Specific headings and formatting are used to provide information for the
generation of a machine-readable specification, in
[Resource Description Framework (RDF)](https://en.wikipedia.org/wiki/Resource_Description_Framework)
data model, by the [spec-parser][].

For instance, a summary listed under the "Summary" heading will be represented
as a `rdfs:comment` in the RDF file. Likewise, a value specified for the
`minCount` of a property under the "Properties" heading will be
translated into a `sh:minCount` in the RDF file.
See [an example](#model-file-example).

Descriptions provided under the "Description" heading are intended for human
reference and will not be incorporated into the RDF file.

The same Markdown files are used to generate the HTML files for
[the website](https://spdx.github.io/spdx-spec/).

## Directory organisation

Apart from the content in each individual file, the file itself has to be
placed in a specific location, as spec-parser implies some model semantic
from the location of the file:

- Each element (class, datatype, individual, property, and vocabulary)
  is defined in a distinct file.
- Model file names are case-sensitive and shall be identical to the element they
  represent.
- All model files shall be located within the `model/` directory.
- Profiles should be organised into subdirectories (e.g., `Core/`, `Dataset/`).
- Elements should be categorised by their type in subdirectories (e.g.,
  `Classes/`, `Datatypes/`, `Individuals/`, `Properties/`, `Vocabularies/`).

File and directory organisation:

```text
.
├── model
:   :
│   ├── Core
│   │   ├── Classes
:   :   :   :
│   │   │   └── Tool.md
│   │   ├── Datatypes
:   :   :   :
│   │   │   └── SemVer.md
│   │   ├── Individuals
:   :   :   :
│   │   │   └── NoneElement.md
│   │   ├── Properties
:   :   :   :
│   │   │   └── verifiedUsing.md
│   │   └── Vocabularies
:   :       :
│   │       └── SupportType.md
│   ├── Dataset
:   :
```

The living repository at
<https://github.com/spdx/spdx-3-model/tree/main/model>
is the best reference.

## Naming convention

- Use the singular form (e.g., use `import` and not `imports`).
- Use `UpperCamelCase` format for the names of classes, datatypes, individuals,
  and vocabularies.
- Use `lowerCamelCase` format for the names of properties and
  vocabulary entries.
- Metadata keys use `lowerCamelCase` format (e.g., `subclassOf`, `sinceVersion`).

## File content structure and formatting

Each model file shall adhere to a strict content structure:

- All files shall be encoded in UTF-8.
- Each file shall start with a YAML frontmatter block containing the SPDX
  license identifier:

  ```markdown
  ---
  SPDX-License-Identifier: Community-Spec-1.0
  ---
  ```

- The content immediately after the frontmatter shall begin with an H1 heading
  containing the element's name.
- Each element type has a predefined set of [allowed H2 headings](#syntax) and
  labeled lists that shall be used to structure its content.
- Headings shall be in sentence case (i.e., only the first letter of the first
  word is capitalized).

Additionally, since MkDocs uses a strict
[Python-Markdown](https://python-markdown.github.io/#differences),
each model file shall adhere to specific formatting guidelines:

- Blank lines:
  - There shall be a blank line before and after a heading.
  - There shall be a blank line before and after a list.
- Indentation:
  - Use spaces instead of tabs.
  - When a list item consists of multiple paragraphs, each subsequent paragraph
    in a list item shall be indented by 4 spaces.

### Model file example

This example shows a `SimpleLicensingText` class.
Its name and location in the repository is
`model/SimpleLicensing/Classes/SimpleLicensingText.md`.

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# SimpleLicensingText

## Summary

A license or addition that is not listed on the SPDX License List.

## Description

A SimpleLicensingText represents a License or Addition that is not listed on
the [SPDX License List](https://spdx.org/licenses),
and is therefore defined by an SPDX data creator.

## Metadata

- name: SimpleLicensingText
- subclassOf: /Core/Element

## Properties

- licenseText:
  - minCount: 1
  - maxCount: 1
```

will give this RDF graph
(in [Turtle syntax](https://en.wikipedia.org/wiki/Turtle_(syntax))):

```ttl
<https://spdx.org/rdf/3.1/terms/SimpleLicensing/SimpleLicensingText> a owl:Class,
        sh:NodeShape ;
    rdfs:comment "A license or addition that is not listed on the SPDX License List."@en ;
    rdfs:subClassOf <https://spdx.org/rdf/3.1/terms/Core/Element> ;
    sh:nodeKind sh:IRI ;
    sh:property [ sh:datatype xsd:string ;
            sh:maxCount 1 ;
            sh:minCount 1 ;
            sh:nodeKind sh:Literal ;
            sh:path <https://spdx.org/rdf/3.1/terms/SimpleLicensing/licenseText> ] .
```

## Syntax

### Common optional metadata fields

The following metadata fields are available on all element types (class,
datatype, individual, property, and vocabulary) and on vocabulary entries:

| Field | Type | Description | RDF mapping |
|---|---|---|---|
| `sinceVersion` | string | The version of the specification in which this element was introduced. | `owl:versionInfo "Since version X"@en` |
| `deprecated` | boolean (`true`) | Marks this element as deprecated. | `owl:deprecated true` |
| `deprecatedVersion` | string | The version in which deprecation was announced. | `owl:versionInfo "Deprecated since version X"@en` |
| `isReplacedBy` | string | The fully-qualified name or IRI of the recommended replacement. | `dcterms:isReplacedBy` |

Example:

```markdown
## Metadata

- name: startTime
- nature: DataProperty
- range: DateTime
- sinceVersion: 3.0
- deprecated: true
- deprecatedVersion: 3.1
- isReplacedBy: /Core/creationInfo
```

### Classes

The name of a class shall be in `UpperCamelCase` format.

Allowed headings:

- Summary
- Description
- Metadata
  - `name`: \<class_name\>
  - `subclassOf`: \<class_name\> OR `/Namespace/ClassName` OR `none`
    *(Required unless class has no parent)*
  - `abstract`: `true` *(Omit for concrete classes; default is concrete)*
  - `sinceVersion`: \<version_string\> *(Optional)*
  - `deprecated`: `true` *(Optional)*
  - `deprecatedVersion`: \<version_string\> *(Optional)*
  - `isReplacedBy`: \<fqname_or_IRI\> *(Optional)*
- Properties *(Optional)*
  - `propName:` *(colon after name; property type is looked up from the property definition)*
    - `minCount`: \<number\> *(Optional)*
    - `maxCount`: \<number\> *(Optional)*
  - ...
- External properties restrictions *(Optional)*
  - `/Namespace/propName:` *(colon after fully-qualified name)*
    - `minCount`: \<number\> *(Optional)*
    - `maxCount`: \<number\> *(Optional)*
  - ...

`minCount` and `maxCount` indicate the minimum and maximum number of times
a property may appear in a class (cardinality):

- The minimum possible occurrence is zero (`0`).
- An unbounded maximum occurrence is represented by a star/asterisk (`*`).
- If `minCount` is omitted, it defaults to `0`.
- If `maxCount` is omitted, it defaults to `*`.

> **Note:** The `type:` line that appeared in the old format under each
> property in the Properties section is no longer used. The type is derived
> automatically from the property definition's `range` field.

See details in the [Conformance section][conformance] of the specification.

[conformance]: https://spdx.github.io/spdx-spec/v3.0.1/conformance/

#### Class example

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# Bot

## Summary

An automated agent.

## Description

A class example.

## Metadata

- name: Bot
- subclassOf: Agent
- sinceVersion: 3.1

## Properties

- prohibitedTask:
  - minCount: 1

## External properties restrictions

- /Core/Element/name:
  - minCount: 1
```

#### Abstract class example

```markdown
## Metadata

- name: Agent
- subclassOf: Element
- abstract: true
```

Output label in generated documents: **Instantiability: Abstract**

### Datatypes

The name of a datatype shall be in `UpperCamelCase` format.

Allowed headings:

- Summary
- Description
- Metadata
  - `name`: \<datatype_name\>
  - `subclassOf`: \<xsd_type\> OR \<class_name\>
  - `sinceVersion`, `deprecated`, `deprecatedVersion`, `isReplacedBy` *(Optional)*
- Format
  - `pattern`: \<regular_expression\>

The pattern value shall be wrapped in backticks to protect special characters
(such as `*` and `?`) from being interpreted as Markdown syntax:

```markdown
## Format

- pattern: `^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$`
```

#### Datatype example

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# DateTime

## Summary

A string representing a specific date and time.

## Description

A DateTime is a string representation of a specific date and time.

## Metadata

- name: DateTime
- subclassOf: xsd:dateTimeStamp

## Format

- pattern: `^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$`
```

### Individuals

The name of an individual shall be in `UpperCamelCase` format.

Allowed headings:

- Summary
- Description
- Metadata
  - `name`: \<individual_name\>
  - `type`: \<class_name\>
  - `iri`: \<IRI\> *(Optional; overrides the default IRI derived from namespace and name)*
  - `sameAs`: \<IRI\> *(Optional; declares an `owl:sameAs` equivalence to an external IRI)*
  - `sinceVersion`, `deprecated`, `deprecatedVersion`, `isReplacedBy` *(Optional)*
- Property Values
  - \<property_name\>: \<property_value\>
  - ...

#### Individual example

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# NoneElement

## Summary

A named individual example.

## Description

A named individual of Element class representing none.

## Metadata

- name: NoneElement
- type: IndividualElement

## Property Values

- name: "NONE"
```

### Properties

The name of a property shall be in `lowerCamelCase` format.

Allowed headings:

- Summary
- Description
- Metadata
  - `name`: \<property_name\>
  - `nature`: `DataProperty` OR `ObjectProperty`
  - `range`: \<xsd_type\> OR \<class_name\> OR `/Namespace/ClassName`
  - `sinceVersion`, `deprecated`, `deprecatedVersion`, `isReplacedBy` *(Optional)*

#### Property example

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# sensor

## Summary

Describes a sensor used for collecting the data.

## Description

Describes a sensor that was used for collecting the data
and its calibration value as a key-value pair.

## Metadata

- name: sensor
- nature: ObjectProperty
- range: /Core/DictionaryEntry
```

### Vocabularies

The name of a vocabulary shall be in `UpperCamelCase` format.

Allowed headings:

- Summary
- Description
- Metadata
  - `name`: \<vocabulary_name\>
  - `sinceVersion`, `deprecated`, `deprecatedVersion`, `isReplacedBy` *(Optional)*
- Entries *(see formats below)*

#### Simple entry format

For general enumerations where entries are plain named values:

```markdown
## Entries

- entryName: A plain description of this entry.
```

Entry formatting:

- Entry names shall be in `lowerCamelCase` format.
- Each entry shall be written on a single line.
- Entries should be listed alphabetically whenever possible.

#### Structured entry format

For relationship-type vocabularies where entries carry semantic constraints,
a structured format may be used:

```markdown
## Entries

- entryName:
  - description: A description of this entry.
  - from: SourceClass, AnotherClass
  - to: TargetClass
  - relationshipClass: ConstraintRelationship
  - sinceVersion: 3.0
  - deprecated: true
  - deprecatedVersion: 3.1
  - isReplacedBy: /Core/replacementEntry
```

Structured entry fields:

| Field | Type | Default | Description |
|---|---|---|---|
| `description` | string | `""` | Human-readable description of this entry. |
| `from` | comma-separated names | `Element` | Class(es) that may be the `from` element of a relationship using this entry. |
| `to` | comma-separated names | `Element` | Class(es) that may be the `to` element. |
| `relationshipClass` | string | `Relationship` | The relationship class or subclass constrained to use this entry. |
| `sinceVersion` | string | — | Version in which this entry was introduced. |
| `deprecated` | `true` | — | Marks this entry as deprecated. |
| `deprecatedVersion` | string | — | Version in which this entry was deprecated. |
| `isReplacedBy` | string | — | Recommended replacement entry name or FQ name. |

A mix of simple and structured entries within the same vocabulary is allowed.

#### Vocabulary example (simple)

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# RpsType

## Summary

Specifies the hand shapes used in Rock-Paper-Scissors.

## Description

RpsType specifies the type of a hand shape.

## Metadata

- name: RpsType

## Entries

- paper: A flat, open hand.
- rock: A closed fist.
- scissors: Two fingers extended, forming a V shape.
```

#### Vocabulary example (structured, relationship type)

```markdown
---
SPDX-License-Identifier: Community-Spec-1.0
---

# RelationshipType

## Summary

Information about the relationship between two Elements.

## Description

Provides information about the relationship between two Elements.

## Metadata

- name: RelationshipType

## Entries

- hasContactPoint:
  - description: The `from` Artifact has each `to` Agent as a contact point.
  - from: Artifact
  - to: Agent
  - relationshipClass: ContactPointRelationship
- affects:
  - description: The `from` Vulnerability, Action or DefinedProcess affects each `to` Element.
  - from: Vulnerability, Action, DefinedProcess
  - to: Element
```

## Verbal forms for expressions of provisions

When formulating a provision, the following verbal forms shall be used to
express the necessary level of normativity, as defined in
*ISO/IEC Directives, Part 2 -- Principles and rules for the structure and
drafting of ISO and IEC documents*:

| Verbal forms          | Type of provision                                                                   |
|-----------------------|-------------------------------------------------------------------------------------|
| shall / shall not     | Requirement                                                                         |
| should / should not   | Recommendation                                                                      |
| may                   | Permission (do not use "can", "might", "possible", or "impossible" in this context) |
| can / cannot          | Possibility and capability                                                          |
| must                  | External constraint                                                                 |

Equivalent phrases or expressions may be used in certain cases,
provided they maintain the exact meaning and level of normativity established
by these verbal forms.

## Writing style

To ensure clear and consistent documentation generation, follow these
recommendations when writing paragraph text and incorporating links.

- **Avoid overly long paragraphs.**

    Breaking up text into smaller paragraphs, using bullet points, or creating
    numbered lists can significantly improve readability and comprehension.

- **Maintain a consistent style.**

  - Use a uniform writing style, particularly when presenting similar
    information.
  - Use parallel sentence structures for related entries.

  Here's an example of a **consistent** writing style:

  ```markdown
  ## Entries

  - crl: Certificate Revocation List, or CRL, is a list of ...
  - ocsp: Online Certificate Status Protocol, or OCSP, is a common ...
  - tls: Transport Layer Security, or TLS, is a widely ...
  ```

- **Avoid bare URLs.**

    Always provide a descriptive label for each link.

    Recommended:

    ```markdown
    [SPDX Project](https://spdx.dev/)
    ```

    The `iri` field in the Metadata section of an Individual is an exception
    where a bare URL is appropriate:

    ```markdown
    ## Metadata

    - name: SpdxOrganization
    - type: Organization
    - iri: https://spdx.org/
    ```

- **Code highlighting.**

    When using code blocks, specify the appropriate language code
    (e.g., `json`, `yaml`, `text`) to enable syntax highlighting.

## Translation

Model summaries, descriptions, and vocabulary entry descriptions can be
translated into other languages. Translations are added to the same file as
the source content, using H2 headings with a BCP 47 language tag:

```markdown
## Summary @ja

日本語の要約。

## Entries @ja

- entryName: 日本語の説明。
```

Only `Summary`, `Description`, and `Entries` sections can be translated.
`Metadata` and `Properties` sections cannot be translated.

Translated `Entries` sections shall use the simple `- name: description`
format regardless of whether the base `Entries` section uses structured format.
Entry names shall not be translated.

Please see [translation.md](./translation.md) for full details.

## Migrating from the old format

The `scripts/migrate_format.py` script converts model files from the old format
(bare `SPDX-License-Identifier:` line, `SubclassOf`/`Nature`/`Range`/`IRI`
keys, `Instantiability: Abstract/Concrete`, `type:` lines in Properties) to
the new format described in this document.

```shell
python scripts/migrate_format.py <model-directory>
```

The inverse operation (new → old) is performed by `scripts/revert_format.py`.

Key differences between old and new format:

| Element | Old format | New format |
|---|---|---|
| License header | `SPDX-License-Identifier: X` (bare line) | YAML frontmatter `--- ... ---` block |
| Class parent | `- SubclassOf: ClassName` | `- subclassOf: ClassName` |
| Abstractness | `- Instantiability: Abstract` | `- abstract: true` |
| Concreteness | `- Instantiability: Concrete` | *(omit; false is the default)* |
| Property nature | `- Nature: ObjectProperty` | `- nature: ObjectProperty` |
| Property range | `- Range: ClassName` | `- range: ClassName` |
| Individual IRI | `- IRI: https://...` | `- iri: https://...` |
| Datatype parent | `- SubclassOf: xsd:string` | `- subclassOf: xsd:string` |
| Property in class | `- propName` (plain, then `- type: ...`) | `- propName:` (colon; no `type:` line) |
| Format pattern | `- pattern: regex` | `- pattern: \`regex\`` (backtick-wrapped) |
| Deprecation notice | `**DEPRECATED in SPDX X.**` in Description | `- deprecated: true` + `- deprecatedVersion: X` in Metadata |

## Checking if everything is ok

To check if your additions or edits are correctly formatted,
according to the latest [spec-parser][], use the following commands.

Install spec-parser:

```shell
git clone --branch main --depth 1 https://github.com/spdx/spec-parser.git
```

If you already have the spec-parser, update it:

```shell
git pull origin main
```

Check format:

```shell
python3 spec-parser/main.py --no-output <MODEL_MARKDOWN_DIR>
```

Generate RDF, so you can check the RDF outputs:

```shell
python spec-parser/main.py --generate-rdf --output-rdf <OUTPUT_RDF_DIR> <MODEL_MARKDOWN_DIR>
```

More spec-parser options:

```shell
python spec-parser/main.py --help
```

[spec-parser]: https://github.com/spdx/spec-parser/
