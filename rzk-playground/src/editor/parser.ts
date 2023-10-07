import { parser } from 'rzk-lezer/src/parser'
import { styleTags, tags as t } from '@lezer/highlight'
import { LRLanguage } from '@codemirror/language'

const parserWithMetadata = parser.configure({
    props: [
        styleTags({
            "LineComment!": t.lineComment,
            "BlockComment!": t.blockComment,
            "String!": t.string,
            "#lang": t.moduleKeyword,
            "Rzk1!": t.namespace,
            "#assume #check #compute-nf \
            #compute-whnf #compute #def #define \
            #end #postulate #section #set-option \
            #unset-option #variable #variables": t.definitionKeyword,
            "→ ↦ ∑ × < <= -> =_{ = === > |-> | ∧ ∨ ≡ ≤": t.operatorKeyword,
            ":": t.definitionOperator,
            "π₁ π₂ Σ ⊤ ⊥ 0_2 0₂ 1_2 1 1₂ 2 \
            as BOT Cube first idJ recBot recOR \
            refl_{ refl second Sigma TOP \
            TOPE U unit Unit uses": t.keyword,
            "{ }": t.brace,
            "( )": t.paren,
            VarIdent: t.variableName
        })
    ]
})

export const language = LRLanguage.define({
    parser: parserWithMetadata
})