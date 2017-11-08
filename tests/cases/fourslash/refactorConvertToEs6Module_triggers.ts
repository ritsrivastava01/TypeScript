/// <reference path='fourslash.ts' />

// @allowJs: true

// @Filename: /a.js
////c[|o|]nst [|a|]lias [|=|] [|m|]odule[|.|]export[|s|];
////[|a|]lias[|.|][|x|] = 0;

goTo.eachRange(() => verify.refactorAvailable("Convert to ES6 module"));

