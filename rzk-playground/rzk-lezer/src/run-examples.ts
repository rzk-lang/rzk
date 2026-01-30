import { parser } from "./parser"
import { printTree } from "@lezer-unofficial/printer"
import { readFileSync, readdirSync, writeFileSync, mkdirSync, existsSync } from "fs"

const src = "examples/src"
const exampleNames: string[] = readdirSync(src)

const tree = "examples/tree"
if (!existsSync(tree)) mkdirSync(tree, { recursive: true })

exampleNames.map(name => {
  const doc = readFileSync(`${src}/${name}`).toString()
  writeFileSync(`${tree}/${name}`, printTree(parser.parse(doc), doc))
})