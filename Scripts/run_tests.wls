#!/usr/bin/env wolframscript
(* ::Package:: *)

$packageDirectory = ParentDirectory @ DirectoryName @ $InputFileName;

Print["Loading QuiverGeometry."];
Get @ FileNameJoin[{$packageDirectory, "Kernel", "init.m"}];

Print["Running tests."];
RunTests[];