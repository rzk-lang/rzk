import { EditorView } from "@codemirror/view"
import { Extension } from "@codemirror/state"
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language"
import { tags as t } from "@lezer/highlight"

// Using https://github.com/codemirror/theme-one-dark as reference for the styling

// Using https://github.com/codemirror/codemirror5/blob/master/theme/gruvbox-dark.css as a reference for colors


const chalky = "#e5c07b",
    coral = "#e06c75",
    cyan = "#56b6c2",
    invalid = "#ffffff",
    ivory = "#bdae93",
    stone = "#7c6f64", // Brightened compared to original to increase contrast
    gutterColor = "#8d8e88",
    malibu = "#61afef",
    sage = "#98c379",
    whiskey = "#d19a66",
    violet = "#c678dd",
    darkBackground = "#21252b",
    highlightBackground = "#2c313a",
    background = "#272822",
    tooltipBackground = "#353a42",
    cursor = "#ebdbb2",
    activeLine = "#3e3d32",
    selection = "#49483e",
    gutterBackground = "#2f3129",
    textColor = "#ffffff",
    parenColor = "#eeff00",
    commentColor = "#909570"

/// The colors used in the theme, as CSS color strings.
export const color = {
    chalky,
    coral,
    cyan,
    invalid,
    ivory,
    stone,
    malibu,
    sage,
    whiskey,
    violet,
    darkBackground,
    highlightBackground,
    background,
    tooltipBackground,
    selection,
    cursor,
    gutterBackground,
    activeLine
}

/// The editor theme styles for One Dark.
export const oneDarkTheme = EditorView.theme({
    "&": {
        color: textColor,
        backgroundColor: background,
        fontSize: "16px",
        fontFamily: "Inconsolata"
    },

    ".cm-content": {
        caretColor: cursor
    },

    ".cm-scroller": {
        lineHeight: "1.2"
    },

    ".cm-cursor, .cm-dropCursor": { borderLeftColor: cursor },
    "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection": { backgroundColor: selection },

    ".cm-panels": { backgroundColor: darkBackground, color: ivory },
    ".cm-panels.cm-panels-top": { borderBottom: "2px solid black" },
    ".cm-panels.cm-panels-bottom": { borderTop: "2px solid black" },


    ".cm-activeLine": { backgroundColor: activeLine },

    ".cm-searchMatch": {
        backgroundColor: "#72a1ff59",
        outline: "1px solid #457dff"
    },
    ".cm-searchMatch.cm-searchMatch-selected": {
        backgroundColor: "#6199ff2f"
    },

    ".cm-selectionMatch": { backgroundColor: "#aafe661a" },

    "&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket": {
        backgroundColor: "#bad0f847"
    },

    ".cm-gutters": {
        backgroundColor: gutterBackground,
        color: gutterColor,
        border: "none",
        fontWeight: "500"
    },

    ".cm-activeLineGutter": {
        backgroundColor: gutterBackground,
        color: "#d0d2c0"
    },

    ".cm-foldPlaceholder": {
        backgroundColor: "transparent",
        border: "none",
        color: "#ddd"
    },

    ".cm-tooltip": {
        border: "none",
        backgroundColor: tooltipBackground
    },
    ".cm-tooltip .cm-tooltip-arrow:before": {
        borderTopColor: "transparent",
        borderBottomColor: "transparent"
    },
    ".cm-tooltip .cm-tooltip-arrow:after": {
        borderTopColor: tooltipBackground,
        borderBottomColor: tooltipBackground
    },
    ".cm-tooltip-autocomplete": {
        "& > ul > li[aria-selected]": {
            backgroundColor: highlightBackground,
            color: ivory
        }
    }
}, { dark: true })

/// The highlighting style for code in the One Dark theme.
export const oneDarkHighlightStyle = HighlightStyle.define([
    {
        tag: t.keyword,
        color: violet
    },
    {
        tag: [t.name, t.deleted, t.character, t.propertyName, t.macroName],
        color: coral
    },
    {
        tag: [t.function(t.variableName), t.labelName],
        color: malibu
    },
    {
        tag: [t.color, t.constant(t.name), t.standard(t.name)],
        color: whiskey
    },
    {
        tag: [t.definition(t.name), t.separator],
        color: ivory
    },
    {
        tag: [t.typeName, t.className, t.number, t.changed, t.annotation, t.modifier, t.self, t.namespace],
        color: chalky
    },
    {
        tag: [t.operator, t.operatorKeyword, t.url, t.escape, t.regexp, t.link, t.special(t.string)],
        color: cyan
    },
    {
        tag: [t.meta, t.comment],
        color: stone
    },
    {
        tag: t.strong,
        fontWeight: "bold"
    },
    {
        tag: t.emphasis,
        fontStyle: "italic"
    },
    {
        tag: t.strikethrough,
        textDecoration: "line-through"
    },
    {
        tag: t.link,
        color: stone,
        textDecoration: "underline"
    },
    {
        tag: t.heading,
        fontWeight: "bold",
        color: coral
    },
    {
        tag: t.moduleKeyword,
        fontWeight: "bold",
        color: "#178b8b"
    },
    {
        tag: t.namespace,
        fontWeight: "bold",
        color: "#fe2f96"
    },
    {
        tag: t.definitionKeyword,
        fontWeight: "bold",
        color: "#861ffb"
    },
    {
        tag: t.definitionOperator,
        fontWeight: "bold",
        color: "#1fcbfb"
    },
    {
        tag: t.lineComment,
        color: commentColor
    },
    {
        tag: t.blockComment,
        color: commentColor
    },
    {
        tag: [t.atom, t.bool, t.special(t.variableName)],
        color: whiskey
    },
    {
        tag: [t.processingInstruction, t.string, t.inserted],
        color: sage
    },
    {
        tag: t.brace,
        color: cyan
    },
    {
        tag: t.paren,
        color: parenColor
    },
    {
        tag: t.invalid,
        color: invalid
    },
    {
        tag: t.macroName,
        color: whiskey,
    }
])

/// Extension to enable the One Dark theme (both the editor theme and
/// the highlight style).
export const oneDark: Extension = [oneDarkTheme, syntaxHighlighting(oneDarkHighlightStyle)]