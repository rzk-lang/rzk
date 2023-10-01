import * as wrapper from "./rzk-wrapper"

export type Result = { status: string, result: string }

export function typecheck(input: string) {
    let result: Result = wrapper.rzkTypecheck(input) as any
    return result
}