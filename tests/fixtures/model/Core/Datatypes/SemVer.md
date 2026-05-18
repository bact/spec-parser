---
SPDX-License-Identifier: Community-Spec-1.0
---

# SemVer

## Summary

A semantic version string following the semver.org specification.

## Description

SemVer strings follow the format MAJOR.MINOR.PATCH with optional pre-release and build metadata.

## Metadata

- name: SemVer
- subclassOf: xsd:string

## Format

- pattern: `^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*).*$`
