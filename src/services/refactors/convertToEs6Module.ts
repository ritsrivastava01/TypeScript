/* @internal */
namespace ts.refactor {
    const actionName = "Convert to ES6 module";

    const convertToEs6Module: Refactor = {
        name: actionName,
        description: getLocaleSpecificMessage(Diagnostics.Convert_to_ES6_module),
        getEditsForAction,
        getAvailableActions,
    };

    registerRefactor(convertToEs6Module);

    function getAvailableActions(context: RefactorContext): ApplicableRefactorInfo[] | undefined {
        const { file, startPosition } = context;
        if (!isSourceFileJavaScript(file) || !file.commonJsModuleIndicator) {
            return undefined;
        }

        const node = getTokenAtPosition(file, startPosition, /*includeJsDocComment*/ false);
        return !isAtTriggerLocation(file, node) ? undefined : [
            {
                name: convertToEs6Module.name,
                description: convertToEs6Module.description,
                actions: [
                    {
                        description: convertToEs6Module.description,
                        name: actionName,
                    },
                ],
            },
        ];
    }

    //TODO: probably activate at more places.
    function isAtTriggerLocation(sourceFile: SourceFile, node: Node) {
        if (isExpression(node) && isExportsOrModuleExportsOrAlias(sourceFile, node)) {
            return true;
        }
        const { parent } = node;
        switch (parent.kind) {
            case SyntaxKind.CallExpression:
                return isAtTopLevelRequire(parent as CallExpression);
            case SyntaxKind.PropertyAccessExpression:
                return isExportsOrModuleExportsOrAlias(sourceFile, parent as PropertyAccessExpression)
                    || isExportsOrModuleExportsOrAlias(sourceFile, (parent as PropertyAccessExpression).expression);
            case SyntaxKind.VariableDeclarationList:
                switch (node.kind) {
                    case SyntaxKind.ConstKeyword:
                    case SyntaxKind.LetKeyword:
                    case SyntaxKind.VarKeyword:
                        const decl = (parent as VariableDeclarationList).declarations[0];
                        return isExportsOrModuleExportsOrAlias(sourceFile, decl.initializer);
                    default:
                        return false;
                }
            case SyntaxKind.VariableDeclaration:
                return isExportsOrModuleExportsOrAlias(sourceFile, (parent as VariableDeclaration).initializer);
            default:
                return false;
        }
    }

    function isAtTopLevelRequire(call: CallExpression): boolean {
        if (!isRequireCall(call, /*checkArgumentIsStringLiteral*/ true)) {
            return false;
        }
        const { parent: propAccess } = call;
        const varDecl = isPropertyAccessExpression(propAccess) ? propAccess.parent : propAccess;
        if (!isVariableDeclaration(varDecl)) {
            return false;
        }
        const { parent: varDeclList } = varDecl;
        if (varDeclList.kind !== SyntaxKind.VariableDeclarationList) {
            return false;
        }
        const { parent: varStatement } = varDeclList;
        return varStatement.kind === SyntaxKind.VariableStatement && varStatement.parent.kind === SyntaxKind.SourceFile;
    }

    function getEditsForAction(context: RefactorContext, _actionName: string): RefactorEditInfo | undefined {
        Debug.assertEqual(actionName, _actionName);
        const { file, program } = context;
        Debug.assert(isSourceFileJavaScript(file));
        const edits = textChanges.ChangeTracker.with(context, changes => {
            const moduleExportsChangedToDefault = convertFileToEs6Module(file, program.getTypeChecker(), changes);
            if (moduleExportsChangedToDefault) {
                for (const importingFile of program.getSourceFiles()) {
                    fixImportOfModuleExports(importingFile, file, changes);
                }
            }
        });
        return { edits, renameFilename: undefined, renameLocation: undefined }
    }

    function fixImportOfModuleExports(importingFile: ts.SourceFile, exportingFile: ts.SourceFile, changes: textChanges.ChangeTracker) {
        for (const moduleSpecifier of importingFile.imports) {
            const imported = getResolvedModule(importingFile, moduleSpecifier.text);
            if (!imported || imported.resolvedFileName !== exportingFile.fileName) {
                continue;
            }

            const { parent } = moduleSpecifier;
            switch (parent.kind) {
                case SyntaxKind.ExternalModuleReference: {
                    const importEq = (parent as ExternalModuleReference).parent;
                    changes.replaceNode(importingFile, importEq, makeImport(importEq.name, /*namedImports*/ undefined, moduleSpecifier));
                    break;
                }
                case SyntaxKind.CallExpression: {
                    const call = parent as CallExpression;
                    if (isRequireCall(call, /*checkArgumentIsStringLiteral*/ false)) {
                        changes.replaceNode(importingFile, parent, createPropertyAccess(getSynthesizedDeepClone(call), "default"));
                    }
                    break;
                }
            }
        }
    }

    /** @returns Whether we converted a `module.exports =` to a default export. */
    function convertFileToEs6Module(sourceFile: SourceFile, checker: TypeChecker, changes: textChanges.ChangeTracker): boolean {
        const identifiers: Identifiers = { original: collectFreeIdentifiers(sourceFile), additional: createMap<true>() };
        let moduleExportsChangedToDefault = false;
        for (const statement of sourceFile.statements) {
            switch (statement.kind) {
                case SyntaxKind.CallExpression:
                    if (isRequireCall(statement, /*checkArgumentIsStringLiteral*/ true)) {
                        // For side-effecting require() call, just make a side-effecting import.
                        changes.replaceNode(sourceFile, statement, makeImport(/*name*/ undefined, /*namedImports*/ undefined, statement.expression));
                    }
                    break;
                case SyntaxKind.VariableStatement:
                    doVariableStatement(sourceFile, statement as VariableStatement, changes, checker, identifiers);
                    break;
                case SyntaxKind.ExpressionStatement: {
                    const { expression } = statement as ExpressionStatement
                    if (isBinaryExpression(expression) && expression.operatorToken.kind === SyntaxKind.EqualsToken) {
                        const x = doAssignment(sourceFile, statement as ExpressionStatement, expression, changes, identifiers); //name
                        moduleExportsChangedToDefault = moduleExportsChangedToDefault || x;
                    }
                    break;
                }
            }
        }
        return moduleExportsChangedToDefault;
    }

    function doVariableStatement(sourceFile: SourceFile, statement: VariableStatement, changes: textChanges.ChangeTracker, checker: TypeChecker, identifiers: Identifiers): void {
        const { declarations } = (statement as VariableStatement).declarationList;
        let didChange = false;
        const newNodes = flatMap(declarations, decl => {
            const { name, initializer } = decl;
            if (isExportsOrModuleExportsOrAlias(sourceFile, initializer)) {
                // `const alias = module.exports;` can be removed.
                didChange = true;
                return [];
            }
            if (isRequireCall(initializer, /*checkArgumentIsStringLiteral*/ true)) {
                didChange = true;
                //todo: pass module specifier as string, not node
                return doSingleImport(sourceFile, name, getSynthesizedDeepClone(initializer.arguments[0]), changes, checker, identifiers);
            }
            else if (isPropertyAccessExpression(initializer) && isRequireCall(initializer.expression, /*checkArgumentIsStringLiteral*/ true)) {
                didChange = true;
                return doPropertyAccessImport(name, initializer.name.text, createLiteral(initializer.expression.arguments[0]), identifiers);
            }
            else {
                //test -- some unrelated declaration should be left alone.
                return createVariableStatement(/*modifiers*/ undefined, [decl]);
            }
        });
        if (didChange) { //else leave it alone
            // useNonAdjustedEndPosition to ensure we don't eat the newline after the statement.
            changes.replaceNodeWithNodes(sourceFile, statement, newNodes, { nodeSeparator: "\n", useNonAdjustedEndPosition: true });
        }
    }

    //name
    function doAssignment(sourceFile: SourceFile, statement: ExpressionStatement, expression: BinaryExpression, changes: textChanges.ChangeTracker, identifiers: Identifiers): boolean {
        const { left, right } = expression;
        if (!isPropertyAccessExpression(left)) {
            return false;
        }

        if (isExportsOrModuleExportsOrAlias(sourceFile, left)) {
            if (isExportsOrModuleExportsOrAlias(sourceFile, right)) {
                //`const alias = module.exports;` or `module.exports = alias;` can be removed.
                changes.deleteNode(sourceFile, statement);
            }
            else {
                let x: ReadonlyArray<Statement> | undefined;
                if (isObjectLiteralExpression(right)) {
                    x = doModuleExportsObject(right); //name
                    if (x) {
                        changes.replaceNodeWithNodes(sourceFile, statement, x, { nodeSeparator: "\n", useNonAdjustedEndPosition: true });
                    }
                }
                if (!x) { //neater
                    changes.replaceNode(sourceFile, statement, doModuleExports(right), { useNonAdjustedEndPosition: true });
                    return true;
                }
            }
        }
        else if (isExportsOrModuleExportsOrAlias(sourceFile, left.expression)) {
            fooNamedExport(sourceFile, statement, left.name, right, changes, identifiers);
        }

        return false;
    }

    function fooNamedExport(sourceFile: SourceFile, statement: Statement, propertyName: Identifier, right: Expression, changes: textChanges.ChangeTracker, identifiers: Identifiers) { //name
        // If "originalKeywordKind" was set, this is e.g. `exports.
        const { originalKeywordKind, text } = propertyName;
        if (originalKeywordKind !== undefined && isKeyword(originalKeywordKind) && !isContextualKeyword(originalKeywordKind)) {
            /*
            const _class = 0;
            export { _class as class };
            */
            const tmp = makeUniqueName(`_${text}`, identifiers); // e.g. _class
            const newNodes = [
                makeConst(/*modifiers*/ undefined, tmp, right),
                createExportDeclaration(
                    /*decorators*/ undefined,
                    /*modifiers*/ undefined,
                    createNamedExports([createExportSpecifier(tmp, text)])),
            ];
            changes.replaceNodeWithNodes(sourceFile, statement, newNodes, { nodeSeparator: "\n", useNonAdjustedEndPosition: true });
        }
        else {
            changes.replaceNode(sourceFile, statement, doExportsDotXEquals(text, right), { useNonAdjustedEndPosition: true });
        }
    }

    //mv
    function doPropertyAccessImport(name: BindingName, propertyName: string, moduleSpecifier: StringLiteral, identifiers: Identifiers): ReadonlyArray<Node> {
        switch (name.kind) {
            case SyntaxKind.ObjectBindingPattern:
            case SyntaxKind.ArrayBindingPattern: {
                // `const [a, b] = require("c").d` --> `import { d } from "c"; const [a, b] = d;`
                const tmp  = makeUniqueName(propertyName, identifiers);
                return [
                    makeMeAnImport(tmp, propertyName, moduleSpecifier),
                    makeConst(/*modifiers*/ undefined, name, createIdentifier(tmp)),
                ];
            }
            case SyntaxKind.Identifier:
                // `const a = require("b").c` --> `import { c as a } from "./b";
                return [makeMeAnImport(name.text, propertyName, moduleSpecifier)];
            default:
                Debug.assertNever(name);
        }
    }

    //!
    function makeMeAnImport(name: string, propertyName: string, moduleSpecifier: StringLiteral): ImportDeclaration {
        return propertyName === "default"
            ? makeImport(createIdentifier(name), /*namedImports*/ undefined, moduleSpecifier)
            : makeImport(/*name*/ undefined, [makeImportSpecifier(propertyName, name)], moduleSpecifier);
    }

    //!
    function doModuleExportsObject(object: ObjectLiteralExpression): ReadonlyArray<Statement> | undefined {
        return mapAllOrFail(object.properties, prop => {
            switch (prop.kind) {
                case SyntaxKind.GetAccessor:
                case SyntaxKind.SetAccessor:
                case SyntaxKind.ShorthandPropertyAssignment: //TODO: handle?
                case SyntaxKind.SpreadAssignment:
                    return undefined;
                case SyntaxKind.PropertyAssignment: {
                    const { name, initializer } = prop as PropertyAssignment;
                    return !isIdentifier(name) ? undefined : doExportsDotXEquals(name.text, initializer);
                }
                case SyntaxKind.MethodDeclaration: {
                    const m = prop as MethodDeclaration; //name
                    return !isIdentifier(m.name) ? undefined : makeFunctionDeclaration(m.name.text, [createToken(SyntaxKind.ExportKeyword)], m);
                }
                default:
                    Debug.assertNever(prop);
            }
        });
    }

    function doModuleExports(exported: Expression): Statement {
        const modifiers = [createToken(SyntaxKind.ExportKeyword), createToken(SyntaxKind.DefaultKeyword)];
        switch (exported.kind) {
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.ArrowFunction: {
                // `module.exports = function f() {}` --> `export default function f() {}`
                const fn = exported as FunctionExpression | ArrowFunction;
                return makeFunctionDeclaration(fn.name && fn.name.text, modifiers, fn);
            }
            case SyntaxKind.ClassExpression: {
                // `module.exports = class C {}` --> `export default class C {}`
                const cls = exported as ClassExpression;
                return makeClassDeclaration(cls.name && cls.name.text, modifiers, cls);
            }
            case SyntaxKind.CallExpression:
                // `module.exports = require("x");` ==> `export * from "x";`
                if (isRequireCall(exported, /*checkArguementIsStringLiteral*/ true)) {
                    return createExportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, /*exportClause*/ undefined, /*moduleSpecifier*/ createLiteral(exported.arguments[0]));
                }
                // falls through
            default:
                // `module.exports = 0;` --> `export default 0;`
                return createExportAssignment(/*decorators*/ undefined, /*modifiers*/ undefined, /*isExportEquals*/ false, exported);
        }
    }

    function doExportsDotXEquals(name: string | undefined, exported: Expression): Statement {
        const modifiers = [createToken(SyntaxKind.ExportKeyword)];
        switch (exported.kind) {
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.ArrowFunction:
                // `exports.f = function() {}` --> `export function f() {}`
                return makeFunctionDeclaration(name, modifiers, exported as FunctionExpression | ArrowFunction);
            case SyntaxKind.ClassExpression:
                // `exports.C = class {}` --> `export class C {}`
                return makeClassDeclaration(name, modifiers, exported as ClassExpression);
            default:
                // `exports.x = 0;` --> `export const x = 0;`
                return makeConst(modifiers, createIdentifier(name), exported);
        }
    }

    function makeFunctionDeclaration(name: string | undefined, additionalModifiers: ReadonlyArray<Modifier>, fn: FunctionExpression | ArrowFunction | MethodDeclaration): FunctionDeclaration {
        //Test that we're correctly copying over e.g. `async function* f()`
        return createFunctionDeclaration(
            getSynthesizedDeepClones(fn.decorators),
            concatenate(additionalModifiers, getSynthesizedDeepClones(fn.modifiers)),
            getSynthesizedDeepClone(fn.asteriskToken),
            name,
            getSynthesizedDeepClones(fn.typeParameters),
            getSynthesizedDeepClones(fn.parameters),
            getSynthesizedDeepClone(fn.type),
            convertToFunctionBody(getSynthesizedDeepClone(fn.body)));
    }

    function makeClassDeclaration(name: string | undefined, additionalModifiers: ReadonlyArray<Modifier>, cls: ClassExpression): ClassDeclaration {
        return createClassDeclaration(
            getSynthesizedDeepClones(cls.decorators),
            concatenate(additionalModifiers, getSynthesizedDeepClones(cls.modifiers)),
            name,
            getSynthesizedDeepClones(cls.typeParameters),
            getSynthesizedDeepClones(cls.heritageClauses),
            getSynthesizedDeepClones(cls.members));
    }

    /**
     * Returns nodes that will replace the variable declaration for the commonjs import.
     * May also make use `changes` to remove qualifiers at the use sites of imports, to change `mod.x` to `x`.
     */
    function doSingleImport(
        file: SourceFile,
        name: BindingName,
        moduleSpecifier: StringLiteralLike,
        changes: textChanges.ChangeTracker,
        checker: TypeChecker,
        identifiers: Identifiers,
    ): ReadonlyArray<Node> {
        switch (name.kind) {
            case SyntaxKind.ObjectBindingPattern: {
                const importSpecifiers = mapAllOrFail(name.elements, e =>
                    e.dotDotDotToken || e.initializer || e.propertyName && !isIdentifier(e.propertyName) || !isIdentifier(e.name)
                        ? undefined
                        : makeImportSpecifier(e.propertyName && (e.propertyName as Identifier).text, e.name.text));
                if (importSpecifiers) {
                    return [makeImport(/*name*/ undefined, importSpecifiers, moduleSpecifier)];
                }
                // falls through -- object destructuring has an interesting pattern and must be a variable declaration
            }
            case SyntaxKind.ArrayBindingPattern: {
                /*
                import x from "x";
                const [a, b, c] = x;
                */
                const tmp = nameFromModuleSpecifier(moduleSpecifier, identifiers); //name
                return [
                    makeImport(createIdentifier(tmp), /*namedImports*/ undefined, moduleSpecifier),
                    makeConst(/*modifiers*/ undefined, getSynthesizedDeepClone(name), createIdentifier(tmp)),
                ];
            }
            case SyntaxKind.Identifier:
                return doSingleIdentifierImport(file, name, moduleSpecifier, changes, checker, identifiers);
            default:
                Debug.assertNever(name);
        }
    }

    //mv
    function makeImportSpecifier(propertyName: string | undefined, name: string): ImportSpecifier {
        return createImportSpecifier(propertyName !== undefined && propertyName !== name ? createIdentifier(propertyName) : undefined, createIdentifier(name));
    }

    //move
    function makeConst(modifiers: ReadonlyArray<Modifier> | undefined, name: string | BindingName, init: Expression): VariableStatement {
        return createVariableStatement(
            modifiers,
            createVariableDeclarationList([createVariableDeclaration(name, /*type*/ undefined, init)], NodeFlags.Const));
    }

    function doSingleIdentifierImport(file: SourceFile, name: Identifier, moduleSpecifier: StringLiteralLike, changes: textChanges.ChangeTracker, checker: TypeChecker, identifiers: Identifiers): ReadonlyArray<Node> {
        //At each use:
        //If it's `name.foo`: use a named binding to "foo"
        //If it's anything else (e.g. `name()`): use a default import
        //  (todo l8r: special case for re-export of module)

        //Find all occurrences of the identifier.
        const nameSymbol = checker.getSymbolAtLocation(name);
        //Maps from module property name to name used.
        const namedBindingsNames = createMap<string>();
        let needDefaultImport = false;

        for (const use of identifiers.original.get(name.text)) {
            //Check that it actually resolves to it
            if (checker.getSymbolAtLocation(use) !== nameSymbol || use === name) {
                continue;
            }

            const { parent } = use;
            switch (parent.kind) {
                case SyntaxKind.PropertyAccessExpression: {
                    const { expression, name: { text: propertyName } } = parent as PropertyAccessExpression;
                    Debug.assert(expression === use); //Else shouldn't have been in `collectIdentifiers`
                    let idName = namedBindingsNames.get(propertyName);
                    if (idName === undefined) {
                        idName = makeUniqueName(propertyName, identifiers);
                        namedBindingsNames.set(propertyName, idName);
                    }
                    changes.replaceNode(file, parent, createIdentifier(idName));
                    break;
                }
                default:
                    needDefaultImport = true;
                    break;
            }
        }

        const namedBindings = namedBindingsNames.size === 0 ? undefined : mapIter(namedBindingsNames.entries(), ([propertyName, idName]) =>
            createImportSpecifier(propertyName === idName ? undefined : createIdentifier(propertyName), createIdentifier(idName)));
        if (!namedBindings) {
            // If it was unused, ensure that we at least import *something*.
            needDefaultImport = true;
        }
        return [makeImport(needDefaultImport ? getSynthesizedDeepClone(name) : undefined, namedBindings, createLiteral(moduleSpecifier))];
    }

    function nameFromModuleSpecifier(moduleSpecifier: StringLiteralLike, identifiers: Identifiers): string {
        return makeUniqueName(moduleNameToValidIdentifier(moduleSpecifier.text), identifiers);
    }

    function moduleNameToValidIdentifier(moduleSpecifier: string): string {
        if (moduleSpecifier.length === 0) {
            return "_";
        }

        //Convert to alphanumeric
        let chars = "";
        let lastWasValid = true;
        const firstCharCode = moduleSpecifier.charCodeAt(0);
        if (isIdentifierStart(firstCharCode, ScriptTarget.ESNext)) {
            chars += String.fromCharCode(firstCharCode);
        }
        else {
            lastWasValid = false;
        }

        for (let i = 1; i < moduleSpecifier.length; i++) {
            const ch = moduleSpecifier.charCodeAt(i);
            if (isIdentifierPart(ch, ScriptTarget.ESNext)) {
                let char = String.fromCharCode(ch);
                if (!lastWasValid) {
                    char = char.toUpperCase();
                }
                chars += char;
                lastWasValid = true;
            }
            else {
                lastWasValid = false;
            }
        }

        return chars || "_";
    }

    function makeUniqueName(name: string, identifiers: Identifiers): string {
        while (identifiers.original.has(name) || identifiers.additional.has(name)) {
            name = `_${name}`;
        }
        identifiers.additional.set(name, true);
        return name;
    }

    interface Identifiers {
        readonly original: FreeIdentifiers;
        // Additional identifiers we've added. Mutable!
        readonly additional: Map<true>;
    }

    type FreeIdentifiers = ReadonlyMap<ReadonlyArray<Identifier>>;
    function collectFreeIdentifiers(file: SourceFile): FreeIdentifiers {
        const map = createMultiMap<Identifier>();
        file.forEachChild(function recur(node) {
            if (isIdentifier(node) && isFreeIdentifier(node)) {
                map.add(node.text, node);
            }
            node.forEachChild(recur);
        });
        return map;
    }

    function isFreeIdentifier(node: Identifier): boolean {
        const { parent } = node;
        switch (parent.kind) {
            case SyntaxKind.PropertyAccessExpression:
                return (parent as PropertyAccessExpression).name !== node;
            case SyntaxKind.BindingElement:
                return (parent as BindingElement).propertyName !== node;
            default:
                return true;
        }
    }

    function makeImport(name: Identifier | undefined, namedImports: ReadonlyArray<ImportSpecifier>, moduleSpecifier: Expression): ImportDeclaration {
        const importClause = (name || namedImports) && createImportClause(name, namedImports && createNamedImports(namedImports));
        return createImportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, importClause, moduleSpecifier);
    }
}
