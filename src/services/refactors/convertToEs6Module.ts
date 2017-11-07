/* @internal */
namespace ts.refactor.installTypesForPackage {
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
        if (!isSourceFileJavaScript(file)) {
            return undefined;
        }

        //TODO: probably activate at more places.
        const node = getTokenAtPosition(file, startPosition, /*includeJsDocComment*/ false);
        const { parent: call } = node;
        if (!isIdentifier(node) || node.text !== "require" || !isCallExpression(call) || call.expression !== node) {
            return undefined;
        }
        const { parent: varDecl } = call;
        if (!isVariableDeclaration(varDecl)) {
            return undefined;
        }
        const { parent: varDeclList } = varDecl;
        if (varDeclList.kind !== SyntaxKind.VariableDeclarationList) {
            return undefined;
        }
        const { parent: varStatement } = varDeclList;
        if (varStatement.kind !== SyntaxKind.VariableStatement) {
            return undefined;
        }
        const { parent: sourceFile } = varStatement;
        if (!isSourceFile(sourceFile)) {
            return undefined;
        }
        Debug.assert(sourceFile === file);

        return [
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

    function getEditsForAction(context: RefactorContext, _actionName: string): RefactorEditInfo | undefined {
        Debug.assertEqual(actionName, _actionName);
        const { file, program } = context;
        Debug.assert(isSourceFileJavaScript(file));

        const checker = program.getTypeChecker();
        const identifiers: Identifiers = { original: collectFreeIdentifiers(file), additional: createMap<true>() };
        const edits = textChanges.ChangeTracker.with(context, changes => {
            for (const statement of file.statements) {
                switch (statement.kind) {
                    case SyntaxKind.CallExpression:
                        if (isRequireCall(statement, /*checkArgumentIsStringLiteral*/ true)) {
                            // For side-effecting require() call, just make a side-effecting import.
                            changes.replaceNode(file, statement, makeImport(/*name*/ undefined, /*namedImports*/ undefined, statement.expression));
                        }
                        break;
                    case SyntaxKind.VariableStatement:
                        const { declarations } = (statement as VariableStatement).declarationList;
                        let didFindRequire = false;
                        const newNodes = flatMap(declarations, decl => {
                            if (isRequireCall(decl.initializer, /*checkArgumentIsStringLiteral*/ true)) {
                                didFindRequire = true;
                                return doSingleImport(file, decl, getSynthesizedDeepClone(decl.initializer.arguments[0] as StringLiteral), changes, checker, identifiers);
                            }
                            else {
                                //test -- some unrelated declaration should basically be left alone.
                                return createVariableStatement(undefined, [decl]);
                            }
                        });
                        if (didFindRequire) { //else leave it alone
                            // useNonAdjustedEndPosition to ensure we don't eat the newline after the statement.
                            changes.replaceNodeWithNodes(file, statement, newNodes, { nodeSeparator: "\n", useNonAdjustedEndPosition: true });
                        }
                        break;
                }
            }
        });
        return { edits, renameFilename: undefined, renameLocation: undefined }
    }

    /**
     * Returns nodes that will replace the variable declaration for the commonjs import.
     * May also make use `changes` to remove qualifiers at the use sites of imports, to change `mod.x` to `x`.
     */
    function doSingleImport(
        file: SourceFile,
        vd: VariableDeclaration,
        moduleSpecifier: StringLiteral,
        changes: textChanges.ChangeTracker,
        checker: TypeChecker,
        identifiers: Identifiers,
    ): ReadonlyArray<Node> {
        switch (vd.name.kind) {
            case SyntaxKind.ObjectBindingPattern: {
                const importSpecifiers = mapAllOrFail(vd.name.elements, e =>
                    e.dotDotDotToken || e.initializer || e.propertyName && !isIdentifier(e.propertyName) || !isIdentifier(e.name)
                        ? undefined
                        : createImportSpecifier(getSynthesizedDeepClone(e.propertyName as Identifier), getSynthesizedDeepClone(e.name)));
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
                const name = nameFromModuleSpecifier(moduleSpecifier, identifiers);
                return [
                    makeImport(createIdentifier(name), /*namedImports*/ undefined, moduleSpecifier),
                    createVariableStatement(/*modifiers*/ undefined,
                        createVariableDeclarationList(
                            [createVariableDeclaration(getSynthesizedDeepClone(vd.name), /*type*/ undefined, createIdentifier(name))],
                            NodeFlags.Const)),
                ];
            }
            case SyntaxKind.Identifier:
                return doSingleIdentifierImport(file, vd.name, moduleSpecifier, changes, checker, identifiers);
            default:
                Debug.assertNever(vd.name);
        }
    }

    function doSingleIdentifierImport(file: SourceFile, name: Identifier, moduleSpecifier: StringLiteral, changes: textChanges.ChangeTracker, checker: TypeChecker, identifiers: Identifiers): Node[] {
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
        return [makeImport(needDefaultImport ? getSynthesizedDeepClone(name) : undefined, namedBindings, moduleSpecifier)];
    }

    function nameFromModuleSpecifier(moduleSpecifier: StringLiteral, identifiers: Identifiers) {
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
