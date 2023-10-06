export type Result = { status: string, result: string }

declare function rzkTypecheck_(tmp: { input: string }): void;

export function typecheck(input: string) {
    const tmp = { input }
    rzkTypecheck_(tmp)
    const result: Result = tmp as unknown as Result
    return result
}