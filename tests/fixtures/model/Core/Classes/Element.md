---
SPDX-License-Identifier: Community-Spec-1.0
---

# Element

## Summary

An abstract base class for all elements.

## Description

Element is the root of the class hierarchy. All concrete elements extend Element.

## Metadata

- name: Element
- abstract: true

## Properties

- spdxId:
  - minCount: 1
  - maxCount: 1
- name:
  - minCount: 0
  - maxCount: 1
