import { parser } from "./parser"
import { printTree } from "printer"
import { readFileSync, readdirSync, writeFileSync, mkdirSync, existsSync } from "fs"

const src = "examples/src"
const exampleNames = readdirSync(src)

const tree = "examples/tree"
if (!existsSync(tree)) mkdirSync(tree, { recursive: true })

exampleNames.map(name => {
  const doc = readFileSync(`${src}/${name}`).toString()
  writeFileSync(`${tree}/${name}`, printTree(parser.parse(doc), doc))
})