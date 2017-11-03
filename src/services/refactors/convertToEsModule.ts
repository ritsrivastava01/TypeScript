/* @internal */
namespace ts.refactor.installTypesForPackage {
    const actionName = "Convert to default import";

    const convertToEs6Module: Refactor = {
        name: actionName,
        description: getLocaleSpecificMessage(Diagnostics.Convert_to_ES_module),
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
        if (!isIdentifier(node)) {
            return undefined;
        }

        if (node.text !== "require") {
            return undefined;
        }

        //Must appear in base scope.
        const { parent: call } = node;
        if (!isCallExpression(call) || call.expression !== node) {
            return undefined;
        }

        const { parent: v } = call; //name
        if (!isVariableDeclaration(v)) {
            return undefined;
        }

        const { parent: vdl } = v; //name
        if (vdl.kind !== SyntaxKind.VariableDeclarationList) {
            return undefined;
        }

        const { parent: sf } = vdl;
        if (!isSourceFile(sf)) {
            return undefined;
        }

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
        const identifiers = collectIdentifiers(file);
        const edits = textChanges.ChangeTracker.with(context, changes => {
            for (const statement of file.statements) {
                switch (statement.kind) {
                    case SyntaxKind.CallExpression:
                        if (isRequireCall(statement, /*checkArgumentIsStringLiteral*/ true)) {
                            // For side-effecting require() call, just make a side-effecting import.
                            const im = createImportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, /*importClause*/ undefined, /*moduleSpecifier*/ statement.expression);
                            changes.replaceNode(file, statement, im);
                        }
                        break;
                    case SyntaxKind.VariableStatement:
                        const { declarations } = (statement as VariableStatement).declarationList;
                        let didFindRequire = false;
                        const newNodes = flatMap(declarations, decl => {
                            if (isRequireCall(decl.initializer, /*checkArgumentIsStringLiteral*/ true)) {
                                didFindRequire = true;
                                return doit(file, decl, decl.initializer.expression, changes, checker, identifiers);
                            }
                            else {
                                //test -- some unrelated declaration should basically be left alone.
                                return createVariableStatement(undefined, [decl]);
                            }
                        });
                        if (didFindRequire) { //else leave it alone
                            changes.replaceNodeWithNodes(file, statement, newNodes, { nodeSeparator: ";\n" });
                        }
                        break;
                }
            }
        });
        return { edits, renameFilename: undefined, renameLocation: undefined }
    }

    //Returns the node[] to replace the variable declaration. May also make additional text changes at use sites of the imports (to remove a qualifier or rename)
    function doit(file: ts.SourceFile, vd: VariableDeclaration, moduleSpecifier: Expression, changes: textChanges.ChangeTracker, checker: TypeChecker, identifiers: Identifiers): Node[] {
        switch (vd.name.kind) {
            case SyntaxKind.ObjectBindingPattern:
                const x = trySimple(vd.name);
                if (x) {
                    return [ts.createImportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, x, moduleSpecifier)];
                }
                // falls through -- object destructuring has an interesting pattern and must be a variable declaration
                //(test)
            case SyntaxKind.ArrayBindingPattern:
                //Default or namespace import, then const [x, y, z] = that
                throw new Error("TODO");
            case SyntaxKind.Identifier:
                return doitIdentifier(file, vd.name, moduleSpecifier, changes, checker, identifiers);
            default:
                Debug.assertNever(vd.name);
        }

    }

    function doitIdentifier(file: ts.SourceFile, name: Identifier, moduleSpecifier: Expression, changes: textChanges.ChangeTracker, checker: TypeChecker, identifiers: Identifiers): Node[] {
        //At each use:
        //If it's `name.foo`: use a named binding to "foo"
        //If it's anything else (e.g. `name()`): use a default import
        //  (todo l8r: special case for re-export of module)

        //Find all occurrences of the identifier.
        const nameSymbol = checker.getSymbolAtLocation(name);
        const namedBindingsNames = createMap<true>();
        let needDefaultImport = false;

        for (const use of identifiers.get(name.text)) {
            //Check that it actually resolves to it
            if (checker.getSymbolAtLocation(use) !== nameSymbol || use === name) {
                continue;
            }

            const { parent } = use;
            switch (parent.kind) {
                case SyntaxKind.PropertyAccessExpression: {
                    const { expression, name } = parent as PropertyAccessExpression;
                    Debug.assert(expression === use); //Else shouldn't have been in `collectIdentifiers`
                    namedBindingsNames.set(name.text, true);
                    changes.replaceNode(file, parent, name);
                    break;
                }
                default:
                    needDefaultImport = true;
                    break;
            }
        }

        const namedBindings = namedBindingsNames.size === 0 ? undefined : ts.mapDefinedIter(namedBindingsNames.keys(), b => {
            //TODO: worry about shadowing!
            //If any occurrence *would* resolve to something non-global at this location, must rename.
            return ts.createImportSpecifier(/*propertyName*/ undefined, createIdentifier(b));
        });
        const importClause = createImportClause(needDefaultImport ? name : undefined, createNamedImports(namedBindings));
        ts.createImportDeclaration(/*decorators*/ undefined, /*modifiers*/ undefined, importClause, moduleSpecifier);
    }

    type Identifiers = ReadonlyMap<ReadonlyArray<Identifier>>;
    function collectIdentifiers(file: ts.SourceFile): Identifiers {
        const m = createMultiMap<Identifier>();
        //and iterate!
        file.forEachChild(function recur(node) {
            if (isIdentifier(node) && !(isPropertyAccessExpression(node.parent) && node.parent.name === node)) {
                m.add(node.text, node);
            }
        });
        return m;
    }

    /*enum Kind { //name
        ES6,
        CommonJsDefault,
    }

    function getKind(moduleSymbol: Symbol) {
        //reuse code
        const ex =  moduleSymbol.exports.get("export=" as __String);
        if (!ex) {
            return Kind.ES6; // Doesn't use `export =`, must be an es6 module
        }

        return Kind.CommonJsDefault; //TODO: smarter -- if not a fn or class then still OK
    }*/


    function trySimple(o: ObjectBindingPattern): ImportClause | undefined { //name
        const elements = [];
        for (const e of o.elements) {
            if (e.dotDotDotToken || e.initializer || !isIdentifier(e.propertyName) || !isIdentifier(e.name)) {
                return undefined;
            }
            elements.push(createImportSpecifier(e.propertyName, e.name));
        };
        return createImportClause(/*name*/ undefined, ts.createNamedImports(elements));
    }

}
