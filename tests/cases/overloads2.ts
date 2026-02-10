// @filename: http.ts
/// <reference lib="es2020" />
/// <reference lib="dom" />

type HttpMethod = "GET" | "POST" | "PUT" | "HEAD" | "DELETE" | "PATCH" | string;
type TrimRoute<T extends string> = T extends `${infer U}${"+" | "*"}` ? U : T;
type ExtractPattern<T extends string> = T extends `${infer P}{${infer U}}${infer S}` ? TrimRoute<U> | ExtractPattern<P | S> : never;
export type CapturedPattern<T extends string> = string extends T ? Record<string, string> : {
    [P in ExtractPattern<T>]: string;
};
type SplitRoute<T extends string> = T extends `${infer M extends HttpMethod} ${infer P}` ? [
    M,
    P
] : [
    string,
    T
];
export type PathArgs<T extends string> = T extends `${infer P}{${infer U}}${infer S}` ? [
    ...PathArgs<P>,
    string,
    ...PathArgs<S>
] : [];
export type PathArgsWithBody<T extends string, U> = [
    ...PathArgs<T>,
    ...(U extends undefined ? [
        body?: U
    ] : unknown extends U ? [
        body?: any
    ] : [
        body: U
    ])
];
export interface HttpRequest<T extends string = string> {
    readonly path: string;
    readonly method: SplitRoute<T>[0];
    readonly headers: Request["headers"];
    readonly cookies?: string[];
    readonly context?: any;
    readonly queryString?: string;
    readonly pathParameters: CapturedPattern<SplitRoute<T>[1]>;
}
export interface TypedRequest<T extends string = string> extends Request {
    readonly method: SplitRoute<T>[0];
    readonly pathParameters: CapturedPattern<SplitRoute<T>[1]>;
    readonly body: (Request["body"] & AsyncIterable<Uint8Array>) | null;
    readonly context?: any;
}
export type RequestHandler<T extends string = string, R = unknown> = (request: TypedRequest<T>) => Promise<HandlerResponse<R>> | HandlerResponse<R>;
export type RequestHandlerWithBody<T extends string = string, U = any, R = unknown> = (request: TypedRequest<T>, body: U) => Promise<HandlerResponse<R>> | HandlerResponse<R>;
type HandlerResponse<R> = Response | R | void;
type HandlerArgs<T extends string, U> = U extends undefined ? [
    request: HttpRequest<T>
] : [
    request: HttpRequest<T>,
    body: U
];
export type HttpHandler<T extends string = string, U = any, R = unknown, C = void> = (this: C, ...args: HandlerArgs<T, U>) => Promise<HandlerResponse<R>> | HandlerResponse<R>;
export type HttpFetcher<T extends string = string, U = any, R = unknown> = (...args: [
    ...PathArgs<T>,
    ...(U extends undefined ? [
    ] : [
        body: U
    ])
]) => Promise<R>;
interface BindingBase {
    from: string;
    to: string;
}
interface PathBinding extends BindingBase {
    type: "path";
}
interface QueryBinding extends BindingBase {
    type: "query";
}
interface HeaderBinding extends BindingBase {
    type: "header";
}
interface BodyBinding extends BindingBase {
    type: "body";
}
type HttpBinding = PathBinding | QueryBinding | HeaderBinding | BodyBinding;
export interface HttpRoute<T extends any[] = any[], R = any> {
    readonly host: string;
    readonly port?: number;
    readonly method: string;
    readonly path: string;
    readonly query?: string;
    readonly body?: any;
    readonly bindings: {
        readonly request: HttpBinding[];
        readonly response: HttpBinding[];
    };
    readonly _args?: T;
    readonly _body?: R;
}
export interface HttpResponse {
    body?: any;
    statusCode?: number;
    headers?: Record<string, string>;
}
export type SubstituteRoute<T extends string, Root = true> = T extends `${infer L}{${infer U}}${infer R}` ? Root extends true ? (`${SubstituteRoute<L, false>}${string}${SubstituteRoute<R, false>}` | `${SubstituteRoute<L, false>}${string}${SubstituteRoute<R, false>}?${string}`) : `${SubstituteRoute<L, false>}${string}${SubstituteRoute<R, false>}` : Root extends true ? (`${T}?${string}` | T) : T;
export type RequestArgs<T> = T extends HttpHandler<infer _, infer U> ? U extends undefined ? [
] : any extends U ? [
    body?: any
] : [
    body: U
] : never;

// @filename: compute.ts
import { HttpRoute, PathArgs, RequestHandler, RequestHandlerWithBody, PathArgsWithBody } from "./http";
type HttpBodyMethod = "POST" | "PUT" | "DELETE" | "OPTIONS" | "PATCH";
export declare class HttpService {
    route<P extends string = string, R = unknown>(method: "GET", path: P, handler: RequestHandler<`GET ${P}`, R>): HttpRoute<PathArgs<P>, R>;
    route<P extends string = string, U = unknown, R = unknown>(method: "ANY", path: P, handler: RequestHandlerWithBody<`${string} ${P}`, U, R>): HttpRoute<PathArgsWithBody<P, U>, R>;
    route<P extends string = string, U = unknown, R = unknown, M extends HttpBodyMethod = HttpBodyMethod>(method: HttpBodyMethod, path: P, handler: RequestHandlerWithBody<`${HttpBodyMethod} ${P}`, U, R>): HttpRoute<PathArgsWithBody<P, U>, R>;
    route<P extends string = string, U = unknown, R = unknown>(method: string, path: P, handler: RequestHandlerWithBody<`${string} ${P}`, U, R>): HttpRoute<PathArgsWithBody<P, U>, R>;
}

// @filename: main.ts
import { HttpService } from './compute'

const s = new HttpService()

export const hello = s.route('GET', '/{foo}/{bar}/{fizz}', () => {
    return 'hello, world!'
})
