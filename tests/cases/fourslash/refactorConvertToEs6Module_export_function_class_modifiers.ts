/// <reference path='fourslash.ts' />

// @allowJs: true

// @Filename: /a.js
/////*a*/exports/*b*/.f = @foo async function* f(p) {}
////exports.C = @foo class C extends D { m() {} }

goTo.select("a", "b");
edit.applyRefactor({
    refactorName: "Convert to ES6 module",
    actionName: "Convert to ES6 module",
    actionDescription: "Convert to ES6 module",
    newContent: `function f() {}
export default { f };`,
});
