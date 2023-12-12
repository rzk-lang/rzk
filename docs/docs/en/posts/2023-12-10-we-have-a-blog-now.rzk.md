---
authors:
  - fizruk
categories:
  - Announcements
date: 2023-12-10
---

# We have a blog now!

This weekend I have spent some time to make some updates to the Rzk website.
In particular, we now have multi-lingual support (with some significant portions translated to Russian)
as well as a blog system, where we plan to regularly post about changes and improvements
to Rzk, tooling, and related formalization projects.

The blog supports literate Rzk files:

```rzk
#lang rzk-1

#define hello-world
  : U
  := (hello : U) → hello
```
