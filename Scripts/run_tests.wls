#!/usr/bin/env wolframscript
(* ::Package:: *)

$packageDirectory = ParentDirectory @ DirectoryName @ $InputFileName;

Print["Loading MathTools."];
Get @ FileNameJoin[{$packageDirectory, "Kernel", "init.m"}];

Print["Running tests."];
RunTests[];