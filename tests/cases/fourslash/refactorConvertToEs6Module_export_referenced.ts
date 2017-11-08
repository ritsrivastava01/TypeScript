/// <reference path='fourslash.ts' />

// Test that we leave it alone if the name is a keyword.

// @allowJs: true

// @Filename: /a.js
////const x = 0;
/////*a*/exports/*b*/.x = 1;

goTo.select("a", "b");
edit.applyRefactor({
    refactorName: "Convert to ES6 module",
    actionName: "Convert to ES6 module",
    actionDescription: "Convert to ES6 module",
    newContent: `const x = 0;
const _x = 1;
export { _x as x };
`,
});
