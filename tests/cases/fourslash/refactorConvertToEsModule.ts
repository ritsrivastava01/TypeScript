/// <reference path='fourslash.ts' />

// @allowJs: true

// @Filename: /a.js
////const x = /*a*/require/*b*/("x");

goTo.select("a", "b");
edit.applyRefactor({
    refactorName: "Convert to default import",
    actionName: "Convert to default import",
    actionDescription: "Convert to default import",
    newContent: 'import x from "x";',
});
