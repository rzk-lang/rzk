import { WASI, File, OpenFile, ConsoleStdout } from '@bjorn3/browser_wasi_shim'

export type Result = { status: string, result: string }

// The rzk logic core is compiled to WebAssembly (see ../../rzk-js). The reactor
// module exports a single async function `rzkTypecheck(source) => Promise<json>`
// over the GHC wasm JSFFI; `public/rzk-js.wasm` and its JSFFI glue
// `public/rzk-js.ghc_wasm_jsffi.js` are produced by rzk-js/build-wasm.sh.

type RzkExports = { rzkTypecheck: (source: string) => Promise<string> }

let core: RzkExports | null = null
let loading: Promise<void> | null = null

async function load(): Promise<void> {
  const base = import.meta.env.BASE_URL
  const jsffiModule = await import(/* @vite-ignore */ base + 'rzk-js.ghc_wasm_jsffi.js')
  const ghc_wasm_jsffi = jsffiModule.default

  // A reactor needs stdin and (unused) stdout/stderr file descriptors.
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(() => { }),
    ConsoleStdout.lineBuffered(() => { }),
  ]
  const wasi = new WASI(['rzk-js.wasm'], [], fds)

  const module = await WebAssembly.compileStreaming(fetch(base + 'rzk-js.wasm'))
  // The JSFFI glue and the instance are mutually recursive: the glue's imports
  // read the instance's exports, so pass a reference object and fill it after.
  const exportsRef: Record<string, unknown> = {}
  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(exportsRef),
  })
  Object.assign(exportsRef, instance.exports)
  wasi.initialize(instance as unknown as {
    exports: { memory: WebAssembly.Memory, _initialize: () => unknown }
  })

  core = instance.exports as unknown as RzkExports
}

// Load and initialise the wasm core (idempotent). Resolves once `typecheck` is
// ready to call.
export function initRzk(): Promise<void> {
  if (!loading) loading = load()
  return loading
}

export async function typecheck(input: string): Promise<Result> {
  await initRzk()
  const json = await core!.rzkTypecheck(input)
  return JSON.parse(json) as Result
}
