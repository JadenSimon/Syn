
// esbuild plugin compat layer
export type Loader = 'base64' | 'binary' | 'copy' | 'css' | 'dataurl' | 'default' | 'empty' | 'file' | 'js' | 'json' | 'jsx' | 'local-css' | 'text' | 'ts' | 'tsx'

export interface Plugin {
    name: string
    setup: (build: PluginBuild) => (void | Promise<void>)
}

export interface PluginBuild {
    // initialOptions: BuildOptions

    // resolve(path: string, options?: ResolveOptions): Promise<ResolveResult>

    //   onStart(callback: () =>
    //     (OnStartResult | null | void | Promise<OnStartResult | null | void>)): void

    //   onEnd(callback: (result: BuildResult) =>
    //     (OnEndResult | null | void | Promise<OnEndResult | null | void>)): void

    /** Documentation: https://esbuild.github.io/plugins/#on-resolve */
    onResolve(options: OnResolveOptions, callback: (args: OnResolveArgs) =>
        (OnResolveResult | null | undefined | Promise<OnResolveResult | null | undefined>)): void

    /** Documentation: https://esbuild.github.io/plugins/#on-load */
    onLoad(options: OnLoadOptions, callback: (args: OnLoadArgs) =>
        (OnLoadResult | null | undefined | Promise<OnLoadResult | null | undefined>)): void

    //   /** Documentation: https://esbuild.github.io/plugins/#on-dispose */
    //   onDispose(callback: () => void): void
}

/** Documentation: https://esbuild.github.io/plugins/#resolve-options */
export interface ResolveOptions {
    pluginName?: string
    importer?: string
    namespace?: string
    resolveDir?: string
    kind?: ImportKind
    pluginData?: any
}

export interface OnStartResult {
    errors?: PartialMessage[]
    warnings?: PartialMessage[]
}

export interface OnEndResult {
    errors?: PartialMessage[]
    warnings?: PartialMessage[]
}

/** Documentation: https://esbuild.github.io/plugins/#on-resolve-options */
export interface OnResolveOptions {
    filter: RegExp
    namespace?: string
}

/** Documentation: https://esbuild.github.io/plugins/#on-resolve-arguments */
export interface OnResolveArgs {
    path: string
    importer: string
    namespace: string
    resolveDir: string
    kind: ImportKind
    pluginData: any
}

export type ImportKind =
    | 'entry-point'

    // JS
    | 'import-statement'
    | 'require-call'
    | 'dynamic-import'
    | 'require-resolve'

    // CSS
    | 'import-rule'
    | 'composes-from'
    | 'url-token'

/** Documentation: https://esbuild.github.io/plugins/#on-resolve-results */
export interface OnResolveResult {
    pluginName?: string

    errors?: PartialMessage[]
    warnings?: PartialMessage[]

    path?: string
    external?: boolean
    sideEffects?: boolean
    namespace?: string
    suffix?: string
    pluginData?: any

    watchFiles?: string[]
    watchDirs?: string[]
}

/** Documentation: https://esbuild.github.io/plugins/#on-load-options */
export interface OnLoadOptions {
    filter: RegExp
    namespace?: string
}

/** Documentation: https://esbuild.github.io/plugins/#on-load-arguments */
export interface OnLoadArgs {
    path: string
    namespace: string
    suffix: string
    pluginData: any
    with: Record<string, string>
}

/** Documentation: https://esbuild.github.io/plugins/#on-load-results */
export interface OnLoadResult {
    pluginName?: string

    errors?: PartialMessage[]
    warnings?: PartialMessage[]

    contents?: string | Uint8Array
    resolveDir?: string
    loader?: Loader
    pluginData?: any

    watchFiles?: string[]
    watchDirs?: string[]
}

export interface PartialMessage {
    id?: string
    pluginName?: string
    text?: string
    location?: Partial<Location> | null
    notes?: PartialNote[]
    detail?: any
}

export interface PartialNote {
    text?: string
    location?: Partial<Location> | null
}