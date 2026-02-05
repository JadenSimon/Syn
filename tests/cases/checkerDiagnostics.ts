// Contains fixtures that produce type checker related diagnostic codes

// === TS2200: The types of '{0}' are incompatible between these types. ===
// (chain diagnostic — appears nested under TS2322)
interface I2200A { prop: { x: number } }
interface I2200B { prop: { x: string } }
{ const v: I2200A = {} as I2200B; }

// === TS2201: The types returned by '{0}' are incompatible between these types. ===
// (chain diagnostic — appears nested under TS2322)
interface I2201A { fn(): number }
interface I2201B { fn(): string }
{ const v: I2201A = {} as I2201B; }

// === TS2202: Call signature return types '{0}' and '{1}' are incompatible. ===
// (chain diagnostic)
interface I2202A { (): number }
interface I2202B { (): string }
{ const v: I2202A = {} as I2202B; }

// === TS2203: Construct signature return types '{0}' and '{1}' are incompatible. ===
// (chain diagnostic)
interface I2203A { new(): { a: number } }
interface I2203B { new(): { a: string } }
{ const v: I2203A = {} as I2203B; }

// === TS2204: Call signatures with no arguments have incompatible return types '{0}' and '{1}'. ===
// legacy

// === TS2205: Construct signatures with no arguments have incompatible return types '{0}' and '{1}'. ===
// legacy

// === TS2206 — Skip: requires 'import type { type X }' from an actual module ===
// === TS2207 — Skip: requires 'export type { type X }' with module context ===

// === TS2208: This type parameter might need an `extends {0}` constraint. ===
// (chain diagnostic — appears as a suggestion alongside other errors)
// Skip: this is a suggestion/related diagnostic, hard to trigger standalone

// === TS2209 — Skip: requires ambiguous project root with export map entries ===
// === TS2210 — Skip: requires ambiguous project root with import map entries ===

// === TS2300: Duplicate identifier '{0}'. ===
interface I2300 { x: number; x: string; }

// === TS2301 — Skip: requires specific instance member variable referencing constructor identifier ===
// Very difficult to trigger in modern TS

// === TS2302: Static members cannot reference class type parameters. ===
class C2302<T> { static x: T; }

// === TS2303 — Skip: requires circular import alias chain across files ===

// === TS2304: Cannot find name '{0}'. ===
{ const v = nonexistentName2304; }

// === TS2305 — Skip: requires importing a non-existent member from an actual module ===
// === TS2306 — Skip: requires importing from a file that exists but is not a module ===

// === TS2307: Cannot find module '{0}' or its corresponding type declarations. ===
// Skip: triggers TS2307 but makes file a module — uncomment if running as module
// import { x as x2307 } from "nonexistent-module-2307";

// === TS2308 — Skip: requires re-exporting conflicting names from modules ===
// === TS2309 — Skip: requires export assignment with other exported elements ===

// === TS2310: Type '{0}' recursively references itself as a base type. ===
interface I2310A extends I2310B {}
interface I2310B extends I2310A {}

// === TS2311 — Skip: requires using 'await' outside async function with specific wording ===
// The "Did you mean to write this in an async function?" variant

// === TS2312: An interface can only extend an object type or intersection of object types with statically known members. ===
type T2312 = { a: number } | { b: string };
interface I2312 extends T2312 {}

// === TS2313: Type parameter '{0}' has a circular constraint. ===
type T2313<A extends A> = A;

// === TS2314: Generic type '{0}' requires {1} type argument(s). ===
type T2314<A, B> = A & B;
type U2314 = T2314<number>;

// === TS2315: Type '{0}' is not generic. ===
type NotGeneric2315 = number;
type V2315 = NotGeneric2315<string>;

// === TS2316 — Skip: requires global type to not be a class/interface — needs --noLib or custom global setup ===
// === TS2317 — Skip: requires global type with wrong number of type params — needs --noLib ===
// === TS2318 — Skip: requires missing global type — needs --noLib ===

// === TS2319: Named property '{0}' of types '{1}' and '{2}' are not identical. ===
// (chain diagnostic — appears nested under TS2320)
interface I2319A { x: number }
interface I2319B { x: string }
interface I2319C extends I2319A, I2319B {}

// === TS2320: Interface '{0}' cannot simultaneously extend types '{1}' and '{2}'. ===
// (produced by the same case as 2319 — chain diagnostic)

// === TS2321 — Skip: requires excessively deep recursive type comparison ===
// Difficult to trigger reliably without risking OOM

// === TS2322: Type '{0}' is not assignable to type '{1}'. ===
{ const v: string = 42; }

// === TS2323: Cannot redeclare exported variable '{0}'. ===
// Skip: file must be a module with duplicate exported vars

// === TS2324: Property '{0}' is missing in type '{1}'. ===
// Skip: older variant — modern TS uses TS2741 instead

// === TS2325: Property '{0}' is private in type '{1}' but not in type '{2}'. ===
// (chain diagnostic — appears nested under TS2322)
class C2325A { private x: number = 1; }
class C2325B { x: number = 1; }
{ const v: C2325A = new C2325B(); }

// === TS2326: Types of property '{0}' are incompatible. ===
// (chain diagnostic — appears alongside TS2322)
interface I2326A { x: { y: number } }
interface I2326B { x: { y: string } }
{ const v: I2326A = {} as I2326B; }

// === TS2327: Property '{0}' is optional in type '{1}' but required in type '{2}'. ===
// (chain diagnostic — appears nested under TS2322)
interface I2327A { x?: number }
interface I2327B { x: number }
{ const v: I2327B = {} as I2327A; }

// === TS2328: Types of parameters '{0}' and '{1}' are incompatible. ===
// (chain diagnostic — appears alongside TS2322)
interface I2328A { fn(x: number): void }
interface I2328B { fn(x: string): void }
{ const v: I2328A = {} as I2328B; }

// === TS2329: Index signature for type '{0}' is missing in type '{1}'. ===
// (chain diagnostic)
interface I2329A { [k: string]: number }
{ const v: I2329A = {} as { x: number }; }

// === TS2330: '{0}' and '{1}' index signatures are incompatible. ===
// (chain diagnostic)
interface I2330A { [k: string]: number }
interface I2330B { [k: string]: string }
{ const v: I2330A = {} as I2330B; }

// === TS2331: 'this' cannot be referenced in a module or namespace body. ===
namespace NS2331 { const v = this; }

// === TS2332: 'this' cannot be referenced in current location. ===
// Skip: hard to distinguish from 2331/2334/2683 in TS 5.9 — most contexts use a more specific code

// === TS2334: 'this' cannot be referenced in a static property initializer. ===
// Skip: `this` in static property initializers is valid in TS 5.9+ (refers to the class constructor)

// === TS2335: 'super' can only be referenced in a derived class. ===
class C2335 { method() { super.toString(); } }

// === TS2336: 'super' cannot be referenced in constructor arguments. ===
class C2336Base {}
class C2336 extends C2336Base {
    constructor(x = super.toString()) { super(); }
}

// === TS2337: Super calls are not permitted outside constructors or in nested functions inside constructors. ===
class C2337Base {}
class C2337 extends C2337Base {
    method() { super(); }
}

// === TS2338: 'super' property access is permitted only in a constructor, member function, or member accessor of a derived class. ===
// Skip: superseded by TS2660 in TS 5.9+ ('super' can only be referenced in members of derived classes or object literal expressions)

// === TS2339: Property '{0}' does not exist on type '{1}'. ===
{ const v = ({} as { a: number }).b; }

// === TS2340: Only public and protected methods of the base class are accessible via the 'super' keyword. ===
class C2340Base { private method() {} }
class C2340 extends C2340Base { fn() { super.method(); } }

// === TS2341: Property '{0}' is private and only accessible within class '{1}'. ===
class C2341 { private x = 1; }
{ const v = new C2341().x; }

// === TS2343 — Skip: requires missing imported helper (tslib) ===
// === TS2354 — Skip: requires missing helper module ===

// === TS2344: Type '{0}' does not satisfy the constraint '{1}'. ===
type T2344<A extends string> = A;
type U2344 = T2344<number>;

// === TS2345: Argument of type '{0}' is not assignable to parameter of type '{1}'. ===
function f2345(x: string): void {}
f2345(42);

// === TS2346: Call target does not contain any signatures. ===
// Skip: superseded by TS2351/TS7009 in TS 5.9+

// === TS2347: Untyped function calls may not accept type arguments. ===
{ const v: Function = () => {}; v<number>(); }

// === TS2348: Value of type '{0}' is not callable. Did you mean to include 'new'? ===
class C2348 {}
{ C2348(); }

// === TS2349: This expression is not callable. ===
{ const v: number = 42; v(); }

// === TS2350: Only a void function can be called with the 'new' keyword. ===
// Skip: superseded by TS2351 in TS 5.9+

// === TS2351: This expression is not constructable. ===
{ const v: () => void = () => {}; new v(); }

// === TS2352: Conversion of type '{0}' to type '{1}' may be a mistake. ===
{ const v = "hello" as number; }

// === TS2353: Object literal may only specify known properties, and '{0}' does not exist in type '{1}'. ===
{ const v: { a: number } = { a: 1, b: 2 }; }

// === TS2355: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value. ===
function f2355(): string {}

// === TS2356: An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type. ===
// Skip: in TS 5.9+, unary minus on string produces TS2362 instead

// === TS2357: The operand of an increment or decrement operator must be a variable or a property access. ===
{ let v = 1; (v + 1)++; }

// === TS2358: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter. ===
{ const v = "hello" instanceof String; }

// === TS2359: The right-hand side of an 'instanceof' expression must be either of type 'any', ... ===
{ const v = ({}) instanceof (42 as any as number); }

// === TS2362: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type. ===
{ const v = ("hello" as string) + 1; }

// === TS2363: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type. ===
{ const v = 1 + ("hello" as string); }

// === TS2364: The left-hand side of an assignment expression must be a variable or a property access. ===
{ 42 = 1; }

// === TS2365: Operator '{0}' cannot be applied to types '{1}' and '{2}'. ===
{ const v = true - false; }

// === TS2366: Function lacks ending return statement and return type does not include 'undefined'. ===
function f2366(): number { if (Math.random()) return 1; }

// === TS2367: This comparison appears to be unintentional because the types '{0}' and '{1}' have no overlap. ===
{ const v: string = "hi"; if (v === 42 as any as number) {} }

// === TS2368: Type parameter name cannot be '{0}'. ===
// Skip: reserved type parameter names — unclear which names trigger this in modern TS

// === TS2369: A parameter property is only allowed in a constructor implementation. ===
class C2369 { constructor(public x: number);
    constructor(x: any) {} }

// === TS2370: A rest parameter must be of an array type. ===
function f2370(...args: number): void {}

// === TS2371: A parameter initializer is only allowed in a function or constructor implementation. ===
declare function f2371(x?: number): void;
// (this doesn't trigger 2371 — the error is for overloads with initializers)
class C2371 { method(x: number = 1): void;
    method(x: any) {} }

// === TS2372: Parameter '{0}' cannot reference itself. ===
function f2372(x: number = x) {}

// === TS2373: Parameter '{0}' cannot reference identifier '{1}' declared after it. ===
function f2373(x: number = y, y: number = 1) {}

// === TS2374: Duplicate index signature for type '{0}'. ===
interface I2374 { [k: string]: number; [k: string]: string; }

// === TS2375 — Skip: requires --exactOptionalPropertyTypes ===

// === TS2376: A 'super' call must be the first statement in the constructor ... ===
// Skip: superseded by TS2401 in TS 5.9+ (root-level statement variant)

// === TS2377: Constructors for derived classes must contain a 'super' call. ===
class C2377Base {}
class C2377 extends C2377Base { constructor() {} }

// === TS2378: A 'get' accessor must return a value. ===
class C2378 { get x(): number {} }

// === TS2379 — Skip: requires --exactOptionalPropertyTypes ===

// === TS2383: Overload signatures must all be exported or non-exported. ===
// Skip: requires module context with mixed export/non-export overloads

// === TS2384: Overload signatures must all be ambient or non-ambient. ===
// Skip: requires mixing declare and non-declare overloads — hard to set up

// === TS2385: Overload signatures must all be public, private or protected. ===
class C2385 {
    public method(x: number): void;
    private method(x: string): void;
    method(x: any) {}
}

// === TS2386: Overload signatures must all be optional or required. ===
class C2386 {
    method(x: number): void;
    method?(x: string): void;
    method(x: any) {}
}

// === TS2387: Function overload must be static. ===
class C2387 {
    method(x: number): void;
    static method(x: string): void;
    static method(x: any) {}
}

// === TS2388: Function overload must not be static. ===
class C2388 {
    static method(x: number): void;
    method(x: string): void;
    method(x: any) {}
}

// === TS2389: Function implementation name must be '{0}'. ===
// Skip: requires overload name mismatch with implementation — hard to trigger without syntax errors

// === TS2390: Constructor implementation is missing. ===
class C2390 { constructor(x: number); constructor(x: string); }

// === TS2391: Function implementation is missing or not immediately following the declaration. ===
function f2391(x: number): void;

// === TS2392: Multiple constructor implementations are not allowed. ===
class C2392 { constructor() {} constructor(x: number) {} }

// === TS2393: Duplicate function implementation. ===
function f2393() {} function f2393() {}

// === TS2394: This overload signature is not compatible with its implementation signature. ===
function f2394(x: number): string;
function f2394(x: any): number { return x; }

// === TS2395: Individual declarations in merged declaration '{0}' must be all exported or all local. ===
// Skip: requires module context with mixed export

// === TS2396: Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters. ===
// legacy

// === TS2397: Declaration name conflicts with built-in global identifier '{0}'. ===
// Skip: hard to trigger — reserved global identifiers

// === TS2398: 'constructor' cannot be used as a parameter property name. ===
class C2398 { constructor(public constructor: number) {} }

// === TS2399: Duplicate identifier '_this'. Compiler uses variable declaration '_this' to capture 'this' reference. ===
// legacy

// === TS2400: Expression resolves to variable declaration '_this' ... ===
// legacy

// === TS2401: A 'super' call must be a root-level statement within a constructor ... ===
// Skip: needs `useDefineForClassFields` set to false to trigger the error
class C2401Base {}
class C2401 extends C2401Base {
    x = 1;
    constructor() { if (true) { super(); } }
}

// === TS2402: Expression resolves to '_super' ... ===
// legacy

// === TS2403: Subsequent variable declarations must have the same type. ===
// legacy (we do not support var)

// === TS2404: The left-hand side of a 'for...in' statement cannot use a type annotation. ===
// Skip: treated as a syntax error (TS1012) in TS 5.9+, not a checker diagnostic

// === TS2405: The left-hand side of a 'for...in' statement must be of type 'string' or 'any'. ===
// legacy, requires LHS assignment 

// === TS2406: The left-hand side of a 'for...in' statement must be a variable or a property access. ===
// Skip: typically caught by parser, not checker

// === TS2407: The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter. ===
{ for (const x in 42) {} }

// === TS2408: Setters cannot return a value. ===
class C2408 { set x(v: number) { return v; } }

// === TS2409: Return type of constructor signature must be assignable to the instance type of the class. ===
class C2409 { x = 1; constructor() { return 10 } }

// === TS2410: The 'with' statement is not supported. ===
// legacy

// === TS2411: Property '{0}' of type '{1}' is not assignable to '{2}' index type '{3}'. ===
interface I2411 { [k: string]: number; x: string; }

// === TS2412 — Skip: requires --exactOptionalPropertyTypes ===

// === TS2413: '{0}' index type '{1}' is not assignable to '{2}' index type '{3}'. ===
interface I2413 { [k: string]: number; [k: number]: string; }

// === TS2414: Class name cannot be '{0}'. ===
class any {}

// === TS2415: Class '{0}' incorrectly extends base class '{1}'. ===
// legacy?

// === TS2416: Property '{0}' in type '{1}' is not assignable to the same property in base type '{2}'. ===
class C2416Base { x: number = 1; }
class C2416 extends C2416Base { x: string = "hi"; }

// === TS2417: Class static side '{0}' incorrectly extends base class static side '{1}'. ===
class C2417Base { static x: number = 1; }
class C2417 extends C2417Base { static x: string = "hi"; }

// === TS2418: Type of computed property's value is '{0}', which is not assignable to type '{1}'. ===
const sym2418: unique symbol = Symbol();
interface I2418 { [sym2418]: number }
{ const v: I2418 = { [sym2418]: "hello" }; }

// === TS2419: Types of construct signatures are incompatible. ===
// (chain diagnostic — appears alongside 2322)

// === TS2420: Class '{0}' incorrectly implements interface '{1}'. ===
interface I2420 { x: number; y: string; }
class C2420 implements I2420 { x = 1; }

// === TS2422: A class can only implement an object type or intersection of object types with statically known members. ===
type T2422 = { a: number } | { b: string };
class C2422 implements T2422 {}

// === TS2423: Class '{0}' defines instance member function '{1}', but extended class '{2}' defines it as instance member accessor. ===
class C2423Base { method() {} }
class C2423 extends C2423Base { get method() { return () => {}; } }

// === TS2425: Class '{0}' defines instance member property '{1}', but extended class '{2}' defines it as instance member function. ===
class C2425Base { x: number = 1; }
class C2425 extends C2425Base { x() { return 1; } }

// === TS2426: Class '{0}' defines instance member accessor '{1}', but extended class '{2}' defines it as instance member function. ===
class C2426Base { get x() { return 1; } }
class C2426 extends C2426Base { x() { return 1; } }

// === TS2427: Interface name cannot be '{0}'. ===
interface string {}

// === TS2428: All declarations of '{0}' must have identical type parameters. ===
interface I2428A<T> {}
interface I2428A<T, U> {}

// === TS2430: Interface '{0}' incorrectly extends interface '{1}'. ===
interface I2430Base { x: number }
interface I2430 extends I2430Base { x: string }

// === TS2431: Enum name cannot be '{0}'. ===
enum number {}

// === TS2432: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element. ===
// Skip: very rare
enum E2432 { A }
enum E2432 { B }

// === TS2433 — Skip: requires namespace declaration in a different file from merged class/function ===
// === TS2434 — Skip: requires namespace declaration located prior to merged class/function ===
// === TS2435 — Skip: requires nested ambient modules ===
// === TS2436 — Skip: requires ambient module with relative module name ===
// === TS2437 — Skip: requires module hidden by local declaration ===
// === TS2438 — Skip: requires import name being a reserved word ===
// === TS2439 — Skip: requires import/export in ambient module with relative name ===
// === TS2440 — Skip: requires import declaration conflicting with local declaration ===
// === TS2441 — Skip: requires compiler-reserved name in top-level module scope ===

// === TS2442: Types have separate declarations of a private property '{0}'. ===
// (chain diagnostic — appears nested under TS2322)
class C2442A { private x: number = 1; }
class C2442B { private x: number = 1; }
{ const v: C2442A = new C2442B(); }

// === TS2443: Property '{0}' is protected but type '{1}' is not a class derived from '{2}'. ===
// (chain diagnostic — appears nested under TS2322)
class C2443A { protected x: number = 1; }
class C2443B { protected x: number = 1; }
{ const v: C2443A = new C2443B(); }

// === TS2444: Property '{0}' is protected in type '{1}' but public in type '{2}'. ===
// (chain diagnostic — appears nested under TS2322)
class C2444A { protected x: number = 1; }
class C2444B { x: number = 1; }
{ const v: C2444A = new C2444B(); }

// === TS2445: Property '{0}' is protected and only accessible within class '{1}' and its subclasses. ===
class C2445 { protected x = 1; }
{ const v = new C2445().x; }

// === TS2446: Property '{0}' is protected and only accessible through an instance of class '{1}'. ===
class C2446Base { protected x = 1; }
class C2446 extends C2446Base { fn(other: C2446Base) { other.x; } }

// === TS2447: The '{0}' operator is not allowed for boolean types. Consider using '{1}' instead. ===
{ const a: boolean = true; const b: boolean = false; const r = a | b; }

// === TS2448: Block-scoped variable '{0}' used before its declaration. ===
{ const v = x2448; let x2448 = 1; } // binder

// === TS2449: Class '{0}' used before its declaration. ===
{ const v = C2449; class C2449 {} } // binder

// === TS2450: Enum '{0}' used before its declaration. ===
{ const v = E2450; enum E2450 { A } } // binder

// === TS2451: Cannot redeclare block-scoped variable '{0}'. ===
{ let v2451 = 1; let v2451 = 2; } // binder

// === TS2452: An enum member cannot have a numeric name. ===
enum E2452 { "0" = 0 }

// === TS2454: Variable '{0}' is used before being assigned. ===
{ let v: number; console.log(v); }

// === TS2456: Type alias '{0}' circularly references itself. ===
type T2456 = T2456;

// === TS2457: Type alias name cannot be '{0}'. ===
type boolean = 1

// === TS2458 — Skip: AMD module — requires --module AMD ===
// === TS2459 — Skip: requires module declaring local but not exporting ===
// === TS2460 — Skip: same as above ===

// === TS2461: Type '{0}' is not an array type. ===
{ const [...v] = 42 as number; }

// === TS2462: A rest element must be last in a destructuring pattern. ===
// Skip: treated as a syntax error (TS1014) in TS 5.9+, not a checker diagnostic

// === TS2463: A binding pattern parameter cannot be optional in an implementation signature. ===
function f2463({ x }?: { x: number }) {}

// === TS2464: A computed property name must be of type 'string', 'number', 'symbol', or 'any'. ===
{ const v = { [true as any as boolean]: 1 }; }

// === TS2465: 'this' cannot be referenced in a computed property name. ===
class C2465 { [this.toString()] = 1; }

// === TS2466: 'super' cannot be referenced in a computed property name. ===
class C2466Base {}
class C2466 extends C2466Base { [super.toString()] = 1; }

// === TS2467: A computed property name cannot reference a type parameter from its containing type. ===
// Skip: requires computed property referencing type parameter — hard to trigger with valid syntax

// === TS2468: Cannot find global value '{0}'. ===
// Skip: requires missing global like Symbol/Promise with specific --target

// === TS2469: The '{0}' operator cannot be applied to type 'symbol'. ===
{ const s: symbol = Symbol(); const v = +s; }

// === TS2472  ===
// Skip: requires --target ES5 or lower with spread in new expression

// === TS2473: Enum declarations must all be const or non-const. ===
// Skip: produces TS2567 instead in TS 5.9+ (enum can only merge with namespace or other enum)

// === TS2474: const enum member initializers must be constant expressions. ===
const enum E2474 { A = Math.random() }

// === TS2475: 'const' enums can only be used in property or index access expressions ... ===
const enum E2475 { A, B }
{ const v = E2475; }

// === TS2476: A const enum member can only be accessed using a string literal. ===
const enum E2476 { A, B }
{ const k: string = "A"; const v = E2476[k]; }

// === TS2477: 'const' enum member initializer was evaluated to a non-finite value. ===
const enum E2477 { A = 1/0 }

// === TS2478: 'const' enum member initializer was evaluated to disallowed value 'NaN'. ===
const enum E2478 { A = NaN }

// === TS2480: 'let' is not allowed to be used as a name in 'let' or 'const' declarations. ===
// Skip: treated as a syntax error (TS1212) in TS 5.9+

// === TS2481: Cannot initialize outer scoped variable '{0}' in the same scope as block scoped declaration '{1}'. ===
// Skip: requires var in outer scope conflicting with let in inner scope — tricky

// === TS2483: The left-hand side of a 'for...of' statement cannot use a type annotation. ===
// Skip: typically a syntax error, not a checker error

// === TS2484: Export declaration conflicts with exported declaration of '{0}'. ===
// Skip: requires module context

// === TS2487: The left-hand side of a 'for...of' statement must be a variable or a property access. ===
// Skip: typically a syntax error, not a checker error

// === TS2488: Type '{0}' must have a '[Symbol.iterator]()' method that returns an iterator. ===
{ for (const x of 42 as number) {} }

// === TS2489: An iterator must have a 'next()' method. ===
{ for (const x of { [Symbol.iterator]() { return {}; } }) {} }

// === TS2490: The type returned by the '{0}()' method of an iterator must have a 'value' property. ===
{ for (const x of { [Symbol.iterator]() { return { next() { return {}; } }; } }) {} }

// === TS2491: The left-hand side of a 'for...in' statement cannot be a destructuring pattern. ===
{ for (const { x } in {}) {} } // parser

// === TS2492: Cannot redeclare identifier '{0}' in catch clause. ===
// Skip: requires 'var' in catch clause that conflicts — complex scoping issue

// === TS2493: Tuple type '{0}' of length '{1}' has no element at index '{2}'. ===
{ const v: [number, string] = [1, "hi"]; const x = v[5]; }

// === TS2494 — Skip: requires for-of on string with --target ES3 ===
// === TS2495 — Skip: similar target restrictions ===
// === TS2496 — Skip: requires arrow function referencing arguments with --target ES5 ===

// === TS2497 — Skip: requires module that uses 'export =' and --esModuleInterop ===
// === TS2498 — Skip: requires module that uses 'export =' with 'export *' ===

// === TS2499: An interface can only extend an identifier/qualified-name with optional type arguments. ===
// Skip: extending a complex type expression — typically a syntax error

// === TS2500: A class can only implement an identifier/qualified-name with optional type arguments. ===
// Skip: implementing a complex type expression — typically a syntax error

// === TS2501: A rest element cannot contain a binding pattern. ===
// Skip: treated as a syntax error in TS 5.9+

// === TS2502: '{0}' is referenced directly or indirectly in its own type annotation. ===
{ const v: typeof v = 1; }

// === TS2503: Cannot find namespace '{0}'. ===
{ const v: NonExistentNamespace2503.Type = 1; }

// === TS2504: Type '{0}' must have a '[Symbol.asyncIterator]()' method that returns an async iterator. ===
async function* f2504() { yield* 42; }

// === TS2505: A generator cannot have a 'void' type annotation. ===
// Skip: specific to return type annotation on generators

// === TS2506: '{0}' is referenced directly or indirectly in its own base expression. ===
// Skip: requires complex circular base expression — hard to trigger cleanly

// === TS2507: Type '{0}' is not a constructor function type. ===
{ const v = "hello"; class C extends v {} }

// === TS2508: No base constructor has the specified number of type arguments. ===
// Skip: TS 5.9+ produces TS2314 instead (Generic type requires N type arguments)

// === TS2509: Base constructor return type '{0}' is not an object type or intersection of object types with statically known members. ===
// Skip: requires dynamic base class returning non-object type

// === TS2510: Base constructors must all have the same return type. ===
// Skip: requires mixin pattern with incompatible constructors

// === TS2511: Cannot create an instance of an abstract class. ===
abstract class C2511 {}
{ new C2511(); }

// === TS2512: Overload signatures must all be abstract or non-abstract. ===
abstract class C2512 {
    abstract method(x: number): void;
    method(x: string): void;
    method(x: any) {}
}

// === TS2513: Abstract method '{0}' in class '{1}' cannot be accessed via super expression. ===
abstract class C2513Base { abstract method(): void; }
class C2513 extends C2513Base { method() { super.method(); } }

// === TS2514: A tuple type cannot be indexed with a negative value. ===
{ const v: [number] = [1]; const x = v[-1]; }

// === TS2515: Non-abstract class '{0}' does not implement inherited abstract member {1} from class '{2}'. ===
abstract class C2515Base { abstract method(): void; }
class C2515 extends C2515Base {}

// === TS2516: All declarations of an abstract method must be consecutive. ===
abstract class C2516 {
    abstract method(x: number): void;
    x = 1;
    abstract method(x: string): void;
}

// === TS2517: Cannot assign an abstract constructor type to a non-abstract constructor type. ===
// (chain diagnostic — appears nested under TS2322)
abstract class C2517 {}
{ const v: new () => C2517 = C2517; }

// === TS2518: A 'this'-based type guard is not compatible with a parameter-based type guard. ===
// Skip: complex type guard scenario

// === TS2519: An async iterator must have a 'next()' method. ===
// Skip: requires async iterable without next — complex setup

// === TS2520 — Skip: compiler reserves name for async — requires --target ES5 ===
// === TS2522 — Skip: 'arguments' in async with --target ES5 ===
// === TS2523 — Skip: 'yield' in parameter initializer ===

// === TS2524: 'await' expressions cannot be used in a parameter initializer. ===
async function f2524(x = await Promise.resolve(1)) {}

// === TS2526: A 'this' type is available only in a non-static member of a class or interface. ===
function f2526(this: this) {}

// === TS2527: The inferred type of '{0}' references an inaccessible '{1}' type. A type annotation is necessary. ===
// Skip: requires complex setup with private types across files

// === TS2528: A module cannot have multiple default exports. ===
// Skip: requires module context

// === TS2529 — Skip: compiler reserves name in module with async — requires --target ES5 ===

// === TS2530: Property '{0}' is incompatible with index signature. ===
// (chain diagnostic — appears alongside other assignability errors)

// === TS2531 — Skip: replaced by TS18047 in TS 5.9+ (Object is possibly 'null') ===
// === TS2532 — Skip: replaced by TS18048 in TS 5.9+ (Object is possibly 'undefined') ===
// === TS2533 — Skip: replaced by TS18047/TS18048 in TS 5.9+ (Object is possibly 'null' or 'undefined') ===

// === TS2534: A function returning 'never' cannot have a reachable end point. ===
function f2534(): never { console.log("oops"); }

// === TS2536: Type '{0}' cannot be used to index type '{1}'. ===
// TODO: produces TS2537 instead — may be deprecated in favor of TS2537/TS2339
{ type T = { a: number }; const k: string = "b"; const v: T[typeof k] = 1; }

// === TS2537: Type '{0}' has no matching index signature for type '{1}'. ===
{ type T = { a: number }; type V = T[string]; }

// === TS2538: Type '{0}' cannot be used as an index type. ===
{ const obj: any = {}; const k: boolean = true; obj[k]; }

// === TS2539: Cannot assign to '{0}' because it is not a variable. ===
// Skip: superseded by TS2630 in TS 5.9+ (Cannot assign to '{0}' because it is a function)

// === TS2540: Cannot assign to '{0}' because it is a read-only property. ===
{ const v: { readonly x: number } = { x: 1 }; v.x = 2; }

// === TS2542: Index signature in type '{0}' only permits reading. ===
{ const v: { readonly [k: string]: number } = {}; v["x"] = 1; }

// === TS2543 — Skip: compiler reserves '_newTarget' — requires --target ES5 ===
// === TS2544 — Skip: expression resolves to '_newTarget' — requires --target ES5 ===

// === TS2545: A mixin class must have a constructor with a single rest parameter of type 'any[]'. ===
// Skip: requires specific mixin class pattern with type parameter constraint

// === TS2547: The type returned by the '{0}()' method of an async iterator must be a promise for a type with a 'value' property. ===
// Skip: requires complex async iterator setup

// === TS2548: Type '{0}' is not an array type or does not have a '[Symbol.iterator]()' method that returns an iterator. ===
// legacy?
{ const [...v] = 42 as number; }

// === TS2549: Type '{0}' is not an array type or a string type or does not have a '[Symbol.iterator]()' method. ===
// Skip: variant of TS2548 for for-of — same target restrictions apply

// === TS2550 — Skip: property does not exist, needs different --lib target ===

// === TS2551: Property '{0}' does not exist on type '{1}'. Did you mean '{2}'? ===
{ const v = { myProperty: 1 }; v.myPropert; }

// === TS2552: Cannot find name '{0}'. Did you mean '{1}'? ===
{ const myVariable = 1; const v = myVariabel; }

// === TS2553: Computed values are not permitted in an enum with string valued members. ===
// TODO: produces TS1061 (syntax error) in TS 5.9+ instead of TS2553
enum E2553 { A = "hello", B }

// === TS2554: Expected {0} arguments, but got {1}. ===
function f2554(a: number, b: number) {}
f2554(1);

// === TS2555: Expected at least {0} arguments, but got {1}. ===
function f2555(a: number, b: number, ...c: number[]) {}
f2555();

// === TS2556: A spread argument must either have a tuple type or be passed to a rest parameter. ===
function f2556(a: number, b: string) {}
{ const args: number[] = [1, 2]; f2556(...args); }

// === TS2558: Expected {0} type arguments, but got {1}. ===
function f2558<A, B>() {}
f2558<number>();

// === TS2559: Type '{0}' has no properties in common with type '{1}'. ===
type WeakType2559 = { a?: number; b?: string };
{ const v: WeakType2559 = 42 as any as { c: number }; }

// === TS2560: Value of type '{0}' has no properties in common with type '{1}'. Did you mean to call it? ===
// TODO: produces TS2322 in TS 5.9+ — may be deprecated in favor of TS2322
function f2560(): { x: number } { return { x: 1 }; }
{ const v: { x: number } = f2560; }

// === TS2561: Object literal may only specify known properties, but '{0}' does not exist in type '{1}'. Did you mean to write '{2}'? ===
// TODO: produces TS2353 in TS 5.9+ — "Did you mean" variant may be deprecated
{ const v: { name: string } = { naem: "hi" }; }

// === TS2562: Base class expressions cannot reference class type parameters. ===
// Skip: requires class expression in extends clause referencing type param

// === TS2563: The containing function or module body is too large for control flow analysis. ===
// Skip: requires extremely large function body

// === TS2564: Property '{0}' has no initializer and is not definitely assigned in the constructor. ===
// Requires --strictPropertyInitialization (included in --strict)
class C2564 { x: number; }

// === TS2565: Property '{0}' is used before being assigned. ===
class C2565 { x: number;
    constructor() { console.log(this.x); this.x = 1; } }

// === TS2566: A rest element cannot have a property name. ===
// Skip: syntax-level error in destructuring

// === TS2567: Enum declarations can only merge with namespace or other enum declarations. ===
class C2567 {}
enum C2567 { A }

// === TS2568: Property '{0}' may not exist on type '{1}'. Did you mean '{2}'? ===
// Skip: variant of 2551 for optional properties — hard to trigger distinctly

// === TS2570: Could not find name '{0}'. Did you mean '{1}'? ===
// Skip: variant of 2552 for specific "could not find" cases

// === TS2571 — Skip: replaced by TS18046 in TS 5.9+ (Object is of type 'unknown') ===

// === TS2574: A rest element type must be an array type. ===
type T2574 = [...number] // becomes any[]

// === TS2575: No overload expects {0} arguments, but overloads do exist that expect either {1} or {2} arguments. ===
function f2575(x: number): void;
function f2575(x: number, y: number, z: number): void;
function f2575(...args: any[]) {}
f2575(1, 2);

// === TS2576: Property '{0}' does not exist on type '{1}'. Did you mean to access the static member '{2}' instead? ===
class C2576 { static x = 1; method() { this.x; } }

// === TS2577: Return type annotation circularly references itself. ===
// Skip: requires circular return type — hard to produce without type alias

// === TS2578: Unused '@ts-expect-error' directive. ===
// @ts-expect-error
const v2578_ok: number = 1;

// === TS2580 — Skip: requires missing type definitions for node ===
// === TS2581 — Skip: requires missing type definitions for jQuery ===
// === TS2582 — Skip: requires missing type definitions for test runner ===
// === TS2583 — Skip: requires changing target library ===
// === TS2584 — Skip: requires missing 'dom' library ===
// === TS2585 — Skip: requires type used as value with missing lib ===

// === TS2588: Cannot assign to '{0}' because it is a constant. ===
{ const v = 1; v = 2; }

// === TS2589: Type instantiation is excessively deep and possibly infinite. ===
type DeepRecurse<T, N extends number, Acc extends any[] = []> =
    Acc['length'] extends N ? T : DeepRecurse<T[], N, [...Acc, 0]>;
type TooDeep2589 = DeepRecurse<number, 1000>;

// === TS2590: Expression produces a union type that is too complex to represent. ===
// Skip: requires extremely large union type — hard to produce concisely

// === TS2591 — Skip: requires missing @types/node with node types field ===
// === TS2592 — Skip: requires missing @types/jquery with types field ===
// === TS2593 — Skip: requires missing @types/jest or @types/mocha with types field ===
// === TS2594 — Skip: requires module with 'export =' and specific flags ===
// === TS2595 — Skip: requires default import of specific module kind ===
// === TS2596 — Skip: requires --esModuleInterop for default import ===
// === TS2597 — Skip: requires 'require' call for module import ===
// === TS2598 — Skip: requires --esModuleInterop with require ===

// === TS2602 — Skip: requires JSX with missing JSX.Element global type ===
// === TS2603 — Skip: requires JSX property assignability error ===
// === TS2604 — Skip: requires JSX element without construct/call signatures ===
// === TS2606 — Skip: requires JSX spread attribute assignability ===
// === TS2607 — Skip: requires JSX element class without attributes property ===
// === TS2608 — Skip: requires JSX global type with multiple properties ===
// === TS2609 — Skip: requires JSX spread child ===

// === TS2610: '{0}' is defined as an accessor in class '{1}', but is overridden here in '{2}' as an instance property. ===
class C2610Base { get x() { return 1; } set x(v: number) {} }
class C2610 extends C2610Base { x = 1; }

// === TS2611: '{0}' is defined as a property in class '{1}', but is overridden here in '{2}' as an accessor. ===
class C2611Base { x = 1; }
class C2611 extends C2611Base { get x() { return 1; } }

// === TS2612: Property '{0}' will overwrite the base property in '{1}'. ===
// Skip: specific scenario with declaration emit — hard to trigger standalone

// === TS2613 — Skip: module has no default export — requires module context ===
// === TS2614 — Skip: module has no exported member — requires module context ===

// === TS2615: Type of property '{0}' circularly references itself in mapped type '{1}'. ===
// TODO: produces TS2456/TS2313 instead — circular mapped type triggers more general circularity errors
type T2615 = { [K in keyof T2615]: T2615[K] };

// === TS2616 — Skip: requires import with 'require' syntax ===
// === TS2617 — Skip: requires import with 'require' syntax and --esModuleInterop ===

// === TS2618: Source has {0} element(s) but target requires {1}. ===
// (chain diagnostic — tuple assignability)
{ const v: [number, string, boolean] = [1, "hi"] as [number, string]; }

// === TS2619: Source has {0} element(s) but target allows only {1}. ===
// (chain diagnostic — tuple length mismatch)
{ const v: [number] = [1, 2] as [number, number]; }

// === TS2620: Target requires {0} element(s) but source may have fewer. ===
// (chain diagnostic)

// === TS2621: Target allows only {0} element(s) but source may have more. ===
// (chain diagnostic)

// === TS2623: Source provides no match for required element at position {0} in target. ===
// (chain diagnostic — already covered by tuple assignments above)

// === TS2624: Source provides no match for variadic element at position {0} in target. ===
// (chain diagnostic — variadic tuple)

// === TS2625: Variadic element at position {0} in source does not match element at position {1} in target. ===
// (chain diagnostic)

// === TS2626: Type at position {0} in source is not compatible with type at position {1} in target. ===
// (chain diagnostic)

// === TS2627: Type at positions {0} through {1} in source is not compatible with type at position {2} in target. ===
// (chain diagnostic)

// === TS2628: Cannot assign to '{0}' because it is an enum. ===
enum E2628 { A }
E2628 = 1

// === TS2629: Cannot assign to '{0}' because it is a class. ===
class C2629 {}
C2629 = 1

// === TS2630: Cannot assign to '{0}' because it is a function. ===
function f2630() {}
f2630 = 1

// === TS2631: Cannot assign to '{0}' because it is a namespace. ===
namespace NS2631 {}
NS2631 = 1

// === TS2632: Cannot assign to '{0}' because it is an import. ===
// Skip: requires import context

// === TS2633 — Skip: JSX property access with namespace names ===

// === TS2634: '{0}' index signatures are incompatible. ===
// (chain diagnostic — appears alongside 2322)

// === TS2635: Type '{0}' has no signatures for which the type argument list is applicable. ===
interface I2635 { (x: string): string; <T extends string, U>(x: T): T }
{ const v: I2635 = null!; typeof v<number> } // consolidate with overloads?

// === TS2636: Type '{0}' is not assignable to type '{1}' as implied by variance annotation. ===
// Skip: requires variance annotations (in/out) with incorrect variance

// === TS2637: Variance annotations are only supported in type aliases for object, function, constructor, and mapped types. ===
type T2637<out T> = T;

// === TS2638: Type '{0}' may represent a primitive value, which is not permitted as the right operand of the 'in' operator. ===
// TODO: produces TS2322 in TS 5.9+ — assignability error emitted instead of specific 'in' operator diagnostic
function f2638<T extends string | number>(k: string, obj: T) { k in obj; }

// === TS2639 — Skip: requires React JSX namespace names ===

// === TS2649 — Skip: requires augmenting module with value exports ===

// === TS2650: Non-abstract class expression is missing implementations for '{0}': {1} and {2} more. ===
abstract class C2650Base { abstract a(): void; abstract b(): void; abstract c(): void; abstract d(): void; abstract e(): void; abstract f(): void; }
{ const v = class extends C2650Base {}; }

// === TS2651: A member initializer in a enum declaration cannot reference members declared after it. ===
enum E2651 { A = B, B = 1 }

// === TS2652 — Skip: merged declaration with default export — requires module context ===

// === TS2653: Non-abstract class expression does not implement inherited abstract member '{0}' from class '{1}'. ===
// (variant of 2650 for single missing member)
abstract class C2653Base { abstract method(): void; }
{ const v = class extends C2653Base {}; }

// === TS2654: Non-abstract class '{0}' is missing implementations for the following members of '{1}': {2}. ===
// (variant of 2515 for multiple missing members — listed)
abstract class C2654Base { abstract a(): void; abstract b(): void; }
class C2654 extends C2654Base {}

// === TS2655: Non-abstract class '{0}' is missing implementations for ... {2} and {3} more. ===
// (variant of 2654 when there are many missing members)
abstract class C2655Base { abstract a(): void; abstract b(): void; abstract c(): void; abstract d(): void; abstract e(): void; abstract f(): void; abstract g(): void; abstract h(): void; }
class C2655 extends C2655Base {}

// === TS2656: Non-abstract class expression is missing implementations for '{0}': {1}. ===
// (chain diagnostic — appears alongside TS2650 for the shorter list variant)

// === TS2657: JSX expressions must have one parent element. ===
// legacy?

// === TS2658: Type '{0}' provides no match for the signature '{1}'. ===
// (chain diagnostic)

// === TS2659: 'super' is only allowed in members of object literal expressions when option 'target' is 'ES2015' or higher. ===
// Skip: requires --target lower than ES2015

// === TS2660: 'super' can only be referenced in members of derived classes or object literal expressions. ===
function f2660() { super.x; }

// === TS2661: Cannot export '{0}'. Only local declarations can be exported from a module. ===
// Skip: requires module context

// === TS2662: Cannot find name '{0}'. Did you mean the static member '{1}.{0}'? ===
class C2662 { static myMethod() {}; method() { myMethod(); } }

// === TS2663: Cannot find name '{0}'. Did you mean the instance member 'this.{0}'? ===
class C2663 { myProp = 1; method() { myProp; } }

// === TS2664 — Skip: requires invalid module name in augmentation ===
// === TS2665 — Skip: requires augmenting untyped module ===
// === TS2666 — Skip: requires exports in module augmentation ===
// === TS2667 — Skip: requires imports in module augmentation ===
// === TS2668 — Skip: requires export modifier on ambient module ===
// === TS2669 — Skip: requires global scope augmentation outside external module ===
// === TS2670 — Skip: requires global scope augmentation without declare ===
// === TS2671 — Skip: requires augmenting non-module entity ===

// === TS2672: Cannot assign a '{0}' constructor type to a '{1}' constructor type. ===
// Skip: requires abstract/non-abstract constructor assignment

// === TS2673: Constructor of class '{0}' is private and only accessible within the class declaration. ===
class C2673 { private constructor() {} }
{ new C2673(); }

// === TS2674: Constructor of class '{0}' is protected and only accessible within the class declaration. ===
class C2674 { protected constructor() {} }
{ new C2674(); }

// === TS2675: Cannot extend a class '{0}'. Class constructor is marked as private. ===
class C2675 { private constructor() {} }
class C2675Child extends C2675 {}

// === TS2676: Accessors must both be abstract or non-abstract. ===
abstract class C2676 {
    abstract get x(): number;
    set x(v: number) {}
}

// === TS2677: A type predicate's type must be assignable to its parameter's type. ===
function f2677(x: string): x is number { return true; }

// === TS2678: Type '{0}' is not comparable to type '{1}'. ===
{ const v: string = "hi"; switch (v) { case 42 as any as number: break; } }

// === TS2679: A function that is called with the 'new' keyword cannot have a 'this' type that is 'void'. ===
// Skip: requires specific this-type in constructor signature

// === TS2680: A '{0}' parameter must be the first parameter. ===
function f2680(x: number, this: string) {}

// === TS2681: A constructor cannot have a 'this' parameter. ===
class C2681 { constructor(this: string) {} }

// === TS2683: 'this' implicitly has type 'any' because it does not have a type annotation. ===
// Requires --noImplicitThis (included in --strict)
function f2683() { return this; }

// === TS2684: The 'this' context of type '{0}' is not assignable to method's 'this' of type '{1}'. ===
class C2684 { method(this: C2684) {} }
{ const fn = new C2684().method; fn(); }

// === TS2685: The 'this' types of each signature are incompatible. ===
// (chain diagnostic)

// === TS2686 — Skip: requires UMD global reference from module ===

// === TS2687: All declarations of '{0}' must have identical modifiers. ===
interface I2687 { readonly x: number; }
interface I2687 { x: number; }

// === TS2688 — Skip: requires missing type definition file ===

// === TS2689: Cannot extend an interface '{0}'. Did you mean 'implements'? ===
interface I2689 {}
class C2689 extends I2689 {}

// === TS2690: '{0}' only refers to a type, but is being used as a value here. Did you mean to use '{1} in {0}'? ===
// Skip: specific scenario with type-only imports

// === TS2692: '{0}' is a primitive, but '{1}' is a wrapper object. Prefer using '{0}' when possible. ===
// (chain diagnostic)

// === TS2693: '{0}' only refers to a type, but is being used as a value here. ===
type T2693 = string;
{ const v = T2693; }

// === TS2694: Namespace '{0}' has no exported member '{1}'. ===
namespace NS2694 { const x = 1; }
{ const v: NS2694.x = 1; }

// === TS2695: Left side of comma operator is unused and has no side effects. ===
{ const v = (1, 2); }

// === TS2696: The 'Object' type is assignable to very few other types. ===
// (chain diagnostic)

// === TS2697 — Skip: requires async function without Promise lib ===
// === TS2698 — Skip: requires spread from non-object type — difficult ===

// === TS2699: Static property '{0}' conflicts with built-in property 'Function.{0}' of constructor function '{1}'. ===
class C2699 { static name = "hello"; }

// === TS2700: Rest types may only be created from object types. ===
function f2700<T extends string>(x: T) { const { length, ...rest } = x; }

// === TS2701: The target of an object rest assignment must be a variable or a property access. ===
// Skip: typically a syntax error

// === TS2702: '{0}' only refers to a type, but is being used as a namespace here. ===
// Skip: superseded by TS2713 in TS 5.9+ (Cannot access 'T.x' because 'T' is a type, but not a namespace)

// === TS2703: The operand of a 'delete' operator must be a property reference. ===
{ delete 42; }

// === TS2704: The operand of a 'delete' operator cannot be a read-only property. ===
{ const v: { readonly x: number } = { x: 1 }; delete v.x; }

// === TS2705 — Skip: requires async/ES5 Promise constructor ===

// === TS2706: Required type parameters may not follow optional type parameters. ===
type T2706<A = string, B> = A & B;

// === TS2707: Generic type '{0}' requires between {1} and {2} type arguments. ===
type T2707<A, B = string> = A & B;
type U2707 = T2707;

// === TS2708: Cannot use namespace '{0}' as a value. ===
namespace NS2708 { export type T = number; }
{ const v = NS2708; }

// === TS2709: Cannot use namespace '{0}' as a type. ===
namespace NS2709 { export const x = 1; }
{ const v: NS2709 = 1; }

// === TS2710: '{0}' are specified twice. The attribute named '{0}' will be overwritten. ===
// Skip: requires JSX attributes

// === TS2711 — Skip: requires dynamic import without Promise lib ===
// === TS2712 — Skip: requires dynamic import/ES5 without Promise constructor ===

// === TS2713: Cannot access '{0}.{1}' because '{0}' is a type, but not a namespace. ===
class C2713 { private x = 1; }
type T2713 = C2713["x"];
{ const v: C2713.x = 1; }

// === TS2714: The expression of an export assignment must be an identifier or qualified name in an ambient context. ===
// Skip: requires ambient module with export assignment

// === TS2715: Abstract property '{0}' in class '{1}' cannot be accessed in the constructor. ===
abstract class C2715 { abstract x: number; constructor() { console.log(this.x); } }

// === TS2716: Type parameter '{0}' has a circular default. ===
// TODO: produces TS2744 in TS 5.9+ — forward reference error emitted before circularity is detected
type T2716<A = B, B = A> = A & B;

// === TS2717: Subsequent property declarations must have the same type. ===
interface I2717 { x: number; }
interface I2717 { x: string; }

// === TS2718: Duplicate property '{0}'. ===
// TODO: produces TS1117 (syntax error) in strict mode — may need non-strict context
{ const v = { x: 1, x: 2 }; }

// === TS2719: Type '{0}' is not assignable to type '{1}'. Two different types with this name exist, but they are unrelated. ===
// Skip: requires types with same name from different declarations

// === TS2720: Class '{0}' incorrectly implements class '{1}'. Did you mean to extend '{1}' and inherit its members as a subclass? ===
class C2720A { x = 1; y = 2; }
class C2720B implements C2720A { x = 1; }

// === TS2721: Cannot invoke an object which is possibly 'null'. ===
{ const v: (() => void) | null = null; v(); }

// === TS2722: Cannot invoke an object which is possibly 'undefined'. ===
{ const v: (() => void) | undefined = undefined; v(); }

// === TS2723: Cannot invoke an object which is possibly 'null' or 'undefined'. ===
{ const v = null as (() => void) | null | undefined; v(); }

// === TS2724: '{0}' has no exported member named '{1}'. Did you mean '{2}'? ===
// Skip: requires module context

// === TS2725 — Skip: requires class name 'Object' with --target ES5+ and module ===
// === TS2726 — Skip: requires missing lib definition ===
// === TS2727 — Skip: requires missing lib definition (did you mean variant) ===

// === TS2729: Property '{0}' is used before its initialization. ===
class C2729 { x = this.y; y = 1; }

// === TS2730: An arrow function cannot have a 'this' parameter. ===
{ const fn = (this: string) => {}; }

// === TS2731: Implicit conversion of a 'symbol' to a 'string' will fail at runtime. ===
{ const s: symbol = Symbol(); const v = `${s}`; }

// === TS2732 — Skip: requires --resolveJsonModule for .json imports ===

// === TS2733: Property '{0}' was also declared here. ===
// (related/informational diagnostic — appears alongside other errors)

// === TS2734: Are you missing a semicolon? ===
// (related/informational diagnostic)

// === TS2735: Did you mean for '{0}' to be constrained to type 'new (...args: any[]) => {1}'? ===
// (related/informational diagnostic)

// === TS2736: Operator '{0}' cannot be applied to type '{1}'. ===
// TODO: produces TS2365 in TS 5.9+ — binary operator error emitted instead of unary-style message
{ const s: symbol = Symbol(); const v = s + 1; }

// === TS2737: BigInt literals are not available when targeting lower than ES2020. ===
// Skip: requires --target lower than ES2020

// === TS2739: Type '{0}' is missing the following properties from type '{1}': {2} ===
interface I2739 { a: number; b: string; c: boolean; }
{ const v: I2739 = {} as {}; }

// === TS2740: Type '{0}' is missing the following properties from type '{1}': {2}, and {3} more. ===
interface I2740 { a: number; b: string; c: boolean; d: number; e: string; f: boolean; }
{ const v: I2740 = {} as {}; }

// === TS2741: Property '{0}' is missing in type '{1}' but required in type '{2}'. ===
interface I2741 { required: number }
{ const v: I2741 = {}; }

// === TS2742 — Skip: requires inferred type with non-portable reference across projects ===

// === TS2743: No overload expects {0} type arguments, but overloads do exist that expect either {1} or {2} type arguments. ===
function f2743<A>(x: A): void;
function f2743<A, B, C>(x: A, y: B, z: C): void;
function f2743(...args: any[]) {}
f2743<number, string>(1, "hi");

// === TS2744: Type parameter defaults can only reference previously declared type parameters. ===
type T2744<A = B, B = string> = A & B;

// === TS2745 — Skip: requires JSX children type mismatch ===
// === TS2746 — Skip: requires JSX single/multiple children mismatch ===
// === TS2747 — Skip: requires JSX text child type mismatch ===

// === TS2748 — Skip: requires --isolatedModules with ambient const enum ===

// === TS2749: '{0}' refers to a value, but is being used as a type here. Did you mean 'typeof {0}'? ===
const val2749 = { x: 1 };
{ const v: val2749 = { x: 1 }; }

// === TS2750: The implementation signature is declared here. ===
// (related/informational — appears alongside TS2394)

// === TS2751: Circularity originates in type at this location. ===
// (related/informational)

// === TS2752: The first export default is here. ===
// (related/informational — requires module context)

// === TS2753: Another export default is here. ===
// (related/informational — requires module context)

// === TS2754: 'super' may not use type arguments. ===
class C2754Base {}
class C2754 extends C2754Base { constructor() { super<number>(); } }

// === TS2755: No constituent of type '{0}' is callable. ===
// TODO: produces TS2349 in TS 5.9+ — general "not callable" error emitted instead
{ const v: string | number = "hi" as string | number; v(); }

// === TS2756: Not all constituents of type '{0}' are callable. ===
// TODO: produces TS2349 in TS 5.9+ — general "not callable" error emitted instead
{ const v: string | (() => void) = "hi" as string | (() => void); v(); }

// === TS2757: Type '{0}' has no call signatures. ===
// (chain diagnostic)

// === TS2758: Each member of the union type '{0}' has signatures, but none of those signatures are compatible with each other. ===
// TODO: produces TS2349 in TS 5.9+ — general "not callable" error emitted instead
{ const v: ((x: number) => void) | ((x: string) => void) = null!; v(true); }

// === TS2759: No constituent of type '{0}' is constructable. ===
// TODO: produces TS2351 in TS 5.9+ — general "not constructable" error emitted instead
{ const v: string | number = "hi" as string | number; new v(); }

// === TS2760: Not all constituents of type '{0}' are constructable. ===
// Skip: requires union with mixed constructable types — hard to trigger distinctly from TS2351

// === TS2761: Type '{0}' has no construct signatures. ===
// (chain diagnostic)

// === TS2762: Each member of the union type '{0}' has construct signatures, but none of those signatures are compatible with each other. ===
// Skip: requires union of incompatible constructable types

// === TS2763-2768 — Skip: complex iterator protocol errors, hard to trigger distinctly ===

// === TS2769: No overload matches this call. ===
function f2769(x: number): void;
function f2769(x: string): void;
function f2769(x: any) {}
f2769(true);

// === TS2770: The last overload gave the following error. ===
// (related diagnostic — appears alongside TS2769)

// === TS2771: The last overload is declared here. ===
// (related diagnostic — appears alongside TS2769)

// === TS2772: Overload {0} of {1}, '{2}', gave the following error. ===
// (related diagnostic — appears alongside TS2769)

// === TS2773: Did you forget to use 'await'? ===
// legacy - await by default largely removes this problem

// === TS2774: This condition will always return true since this function is always defined. ===
function f2774() {}
{ if (f2774) {} }

// === TS2775: Assertions require every name in the call target to be declared with an explicit type annotation. ===
const f2775 = (x: unknown): asserts x is string => {};
f2775("hi");

// === TS2776: Assertions require the call target to be an identifier or qualified name. ===
{ const obj = { check: (x: unknown): asserts x is string => {} };
  [obj.check][0]("hi"); }

// === TS2777: The operand of an increment or decrement operator may not be an optional property access. ===
{ const v: { x?: number } = {}; v?.x++; }

// === TS2778: The target of an object rest assignment may not be an optional property access. ===
// Skip: requires optional property access as rest target

// === TS2779: The left-hand side of an assignment expression may not be an optional property access. ===
{ const v: { x?: number } = {}; v?.x = 1; }

// === TS2780: The left-hand side of a 'for...in' statement may not be an optional property access. ===
// legacy

// === TS2781: The left-hand side of a 'for...of' statement may not be an optional property access. ===
// legacy

// === TS2783: '{0}' is specified more than once, so this usage will be overwritten. ===
{ const v: { x: number } = { x: 1, ...{ x: 2 } }; }

// === TS2784: 'get' and 'set' accessors cannot declare 'this' parameters. ===
class C2784 { get x(this: C2784) { return 1; } }

// === TS2785: This spread always overwrites this property. ===
// chained with TS2783

// === TS2786 — Skip: requires JSX component type mismatch ===
// === TS2787 — Skip: requires JSX return type not valid element ===
// === TS2788 — Skip: requires JSX instance type not valid element ===
// === TS2789 — Skip: requires JSX element type not valid element ===

// === TS2790: The operand of a 'delete' operator must be optional. ===
{ const v: { x: number } = { x: 1 }; delete v.x; }

// === TS2791: Exponentiation cannot be performed on 'bigint' values unless the 'target' option is set to 'es2016' or later. ===
// Skip: requires --target lower than ES2016

// === TS2792 — Skip: requires --moduleResolution nodenext suggestion ===

// === TS2793: The call would have succeeded against this implementation, but implementation signatures of overloads are not externally visible. ===
// (related diagnostic — appears alongside TS2769)

// === TS2794: Expected {0} arguments, but got {1}. Did you forget to include 'void' in your type argument to 'Promise'? ===
// Skip: requires specific Promise type argument scenario

// === TS2795: The 'intrinsic' keyword can only be used to declare compiler provided intrinsic types. ===
type T2795 = intrinsic;

// === TS2796: It is likely that you are missing a comma to separate these two template expressions. ===
// Skip: requires specific template literal expression scenario

// === TS2797: A mixin class that extends from a type variable containing an abstract construct signature must also be declared 'abstract'. ===
// Skip: requires complex mixin pattern

// === TS2798: The declaration was marked as deprecated here. ===
// (related/informational diagnostic)

// === TS2799: Type produces a tuple type that is too large to represent. ===
// Skip: requires type producing tuple with >10000 elements

// === TS2800: Expression produces a tuple type that is too large to represent. ===
// Skip: same as above

// === TS2801: This condition will always return true since this '{0}' is always defined. ===
// TODO: produces TS2774 in TS 5.9+ — "function is always defined" emitted instead
class C2801 { method() {} fn() { if (this.method) {} } }

// === TS2802 — Skip: requires --downlevelIteration or lower --target ===

// === TS2803: Cannot assign to private method '{0}'. Private methods are not writable. ===
// TODO: requires --target ES2015+ for private identifiers — produces TS18028 with default target
class C2803 { #method() {} fn() { this.#method = () => {}; } }

// === TS2804: Duplicate identifier '{0}'. Static and instance elements cannot share the same private name. ===
class C2804 { #x = 1; static #x = 2; }

// === TS2806: Private accessor was defined without a getter. ===
class C2806 { set #x(v: number) {} fn() { console.log(this.#x); } }

// === TS2807 — Skip: requires imported helper with wrong parameter count ===

// === TS2808: A get accessor must be at least as accessible as the setter. ===
class C2808 { private get x() { return 1; } set x(v: number) {} }

// === TS2809: Declaration or statement expected. This '=' follows a block of statements ... ===
// Skip: typically a parser error, not a checker error
// {} =

// === TS2810 — Skip: requires new Promise() JSDoc hint scenario ===

// === TS2811: Initializer for property '{0}' ===
// (related/informational diagnostic)

// === TS2812 — Skip: requires property access missing 'dom' lib ===

// === TS2813: Class declaration cannot implement overload list for '{0}'. ===
interface I2813 { method(x: number): void; method(x: string): void; }
class C2813 implements I2813 { method(x: number): void; method(x: string): void; method(x: any) {} }

// === TS2814: Function with bodies can only merge with classes that are ambient. ===
// legacy - prototype-based classes
// function f2814() {}
// class f2814 {}

// === TS2815: 'arguments' cannot be referenced in property initializers or class static initialization blocks. ===
// legacy?
class C2815 { static { arguments } }

// === TS2816: Cannot use 'this' in a static property initializer of a decorated class. ===
// Skip: requires decorators with specific setup

// === TS2817: Property '{0}' has no initializer and is not definitely assigned in a class static block. ===
// TODO: does not trigger in TS 5.9+ — static property initialization check may not apply with empty static blocks
class C2817 { static x: number; static {} }

// === TS2818 — Skip: requires compiler-reserved name with 'super' in static initializers ===

// === TS2819: Namespace name cannot be '{0}'. ===
// Skip: reserved namespace names — hard to trigger

// === TS2820: Type '{0}' is not assignable to type '{1}'. Did you mean '{2}'? ===
// (variant of 2322 with suggestion — triggered by close match)
{ const v: "hello" | "world" = "helo" as string as "helo"; }

// === TS2821 — Skip: requires import assertions with specific --module option ===
// === TS2822 — Skip: requires import assertions with type-only import ===
// === TS2823 — Skip: requires import attributes with specific --module option ===

// === TS2833: Cannot find namespace '{0}'. Did you mean '{1}'? ===
namespace MyNamespace2833 { export type T = number; }
{ const v: MyNamespce2833.T = 1; }

// === TS2834 — Skip: requires --moduleResolution node16/nodenext with relative import ===
// === TS2835 — Skip: same as above ===
// === TS2836 — Skip: requires import assertions compiling to CommonJS ===
// === TS2837 — Skip: requires import assertion values ===

// === TS2838: All declarations of '{0}' must have identical constraints. ===
// TODO: produces TS2428 in TS 5.9+ — "identical type parameters" emitted instead of "identical constraints"
interface I2838<T extends string> {}
interface I2838<T extends number> {}

// === TS2839: This condition will always return '{0}' since JavaScript compares objects by reference, not value. ===
{ if ([] === []) {} }

// === TS2840: An interface cannot extend a primitive type like '{0}'. ===
interface I2840 extends string {}

// === TS2842: '{0}' is an unused renaming of '{1}'. Did you intend to use it as a type annotation? ===
// Skip: requires destructuring with unused rename that looks like type annotation
// { const { x: string } = { x: 1 } }

// === TS2843: We can only write a type for '{0}' by adding a type for the entire parameter here. ===
// (related/informational diagnostic)

// === TS2844: Type of instance member variable '{0}' cannot reference identifier '{1}' declared in the constructor. ===
// Skip: very specific scenario — class member initializer referencing constructor param

// === TS2845: This condition will always return '{0}'. ===
{ if (NaN !== NaN) {} }

// === TS2846 — Skip: requires importing .d.ts file without 'import type' ===

// === TS2848: The right-hand side of an 'instanceof' expression must not be an instantiation expression. ===
class C2848<T> {}
{ const v = {} instanceof C2848<number>; }

// === TS2849: Target signature provides too few arguments. Expected {0} or more, but got {1}. ===
// Skip: requires specific overload target with fewer params

// === TS2850: The initializer of a 'using' declaration must be either an object with a '[Symbol.dispose]()' method ... ===
{ using v = 42 as any as number; }

// === TS2851: The initializer of an 'await using' declaration must be either an object with a '[Symbol.asyncDispose]()' or '[Symbol.dispose]()' method ... ===
async function f2851() { await using v = 42 as any as number; }

// === TS2852: 'await using' statements are only allowed within async functions ... ===
// TODO: produces TS2853/TS2854 instead in script context — requires module context with non-async function
{ await using v = null; }

// === TS2853 — Skip: requires 'await using' at top level without module context ===
// === TS2854 — Skip: requires specific --module and --target options ===

// === TS2855: Class field '{0}' defined by the parent class is not accessible in the child class via super. ===
// TODO: produces TS2340 in TS 5.9+ — "Only public and protected methods" error emitted instead
class C2855Base { x = 1; }
class C2855 extends C2855Base { fn() { super.x; } }

// === TS2856 — Skip: requires import attributes compiling to CommonJS ===
// === TS2857 — Skip: requires import attributes with type-only import ===
// === TS2858 — Skip: requires import attribute values ===

// === TS2859: Excessive complexity comparing types '{0}' and '{1}'. ===
// Skip: requires extremely complex type comparison — hard to produce concisely

// === TS2860: The left-hand side of an 'instanceof' expression must be assignable to the first argument of the right-hand side's '[Symbol.hasInstance]' method. ===
// Skip: requires custom [Symbol.hasInstance] method

// === TS2861: An object's '[Symbol.hasInstance]' method must return a boolean value for it to be used on the right-hand side of an 'instanceof' expression. ===
// Skip: requires custom [Symbol.hasInstance] with non-boolean return

// === TS2862: Type '{0}' is generic and can only be indexed for reading. ===
function f2862<T extends Record<string, number>>(obj: T) { obj["x"] = 1; }

// === TS2863: A class cannot extend a primitive type like '{0}'. ===
// Skip: extends requires a constructor — primitives are caught differently (2507)

// === TS2864: A class cannot implement a primitive type like '{0}'. ===
class C2864 implements string {}

// === TS2865 — Skip: requires --isolatedModules with conflicting import ===
// === TS2866 — Skip: requires --isolatedModules with global value conflict ===
// === TS2867 — Skip: requires missing @types/bun ===
// === TS2868 — Skip: requires missing @types/bun with types field ===

// === TS2869: Right operand of ?? is unreachable because the left operand is never nullish. ===
{ const v = 1 ?? 2; }

// === TS2870: This binary expression is never nullish. Are you missing parentheses? ===
// No longer used

// === TS2871: This expression is always nullish. ===
{ (true ? undefined : null) ?? "always reached"; }

// === TS2872: This kind of expression is always truthy. ===
{ if (class {}) {} }

// === TS2873: This kind of expression is always falsy. ===
{ if (void 0) {} }

// === TS2874 — Skip: requires JSX tag with missing scope ===
// === TS2875 — Skip: requires JSX tag with missing module path ===
// === TS2876 — Skip: requires relative import path rewrite unsafe ===
// === TS2877 — Skip: requires import path with TS extension rewrite ===
// === TS2878 — Skip: requires import path rewrite across projects ===
// === TS2879 — Skip: requires JSX fragment factory ===

// === TS2880: Import assertions have been replaced by import attributes. Use 'with' instead of 'assert'. ===
// Skip: requires import assertion syntax — needs --module esnext

// === TS2881: This expression is never nullish. ===
{ this ?? 1 ?? 2; }
