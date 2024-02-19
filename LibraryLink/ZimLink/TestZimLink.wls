#!/usr/bin/env wolframscript

Get["/Users/tali/git/qg/Kernel/init.m"];

$a = OpenZimArchive["~/Downloads/wikipedia_en_mathematics_nopic_2024-01.zim"];

Print @ ZimSearch[$a, "category theory"];