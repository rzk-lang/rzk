import CodeMirror from '@uiw/react-codemirror';
import { EditorView, keymap } from '@codemirror/view'
import { scrollPastEnd } from '@codemirror/view';
import { example } from './example';
import { color, oneDark } from './theme';
import { Dispatch, SetStateAction, useState } from 'react';
import { centerCursor } from './cursor-height';
import { language } from './parser';

async function setInitialText(setText: Dispatch<SetStateAction<string>>) {
    const params = new URLSearchParams(window.location.search);
    if (params.has("snippet")) {
        setText(params.get("snippet")!);
    } else if (params.has("code")) {
        setText(params.get("code")!);
    } else if (params.has("snippet_url")) {
        const url = params.get("snippet_url")!;
        const response = await fetch(url, {
            method: 'GET',
            headers: {
                Accept: 'text/plain;charset=UTF-8'
            }
        })

        if (!response.ok) {
            console.error(`Could not get code from ${url}`)
        }

        setText(await response.text())
    }
    else {
        setText(example)
    }
}

export function Editor({
    textState,
    setNeedTypecheck,
    editorHeight
}: {
    textState: [string, Dispatch<SetStateAction<string>>],
    setNeedTypecheck: React.Dispatch<React.SetStateAction<boolean>>,
    editorHeight: number
}) {
    const [existsSelection, setExistsSelection] = useState(false)
    const [text, setText] = textState

    return <CodeMirror
        value={text}
        height={`100vh`}
        width={`100vw`}
        onCreateEditor={async (view) => {
            await setInitialText(setText)

            view.dispatch({ effects: EditorView.scrollIntoView(0) })
        }}
        onUpdate={(update) => {
            if (update.selectionSet) {
                const ranges = update.state.selection.ranges.filter(v => { return v.from != v.to })
                setExistsSelection(ranges.length > 0)
            }
        }}
        onChange={(value) => {
            setText(value)
        }}
        theme={oneDark}
        extensions={[
            keymap.of([
                {
                    key: "Shift-Enter", run: () => {
                        setNeedTypecheck(true)
                        return true
                    }
                }
            ]),
            scrollPastEnd(),
            centerCursor(editorHeight),

            // dynamic parts of the theme
            EditorView.theme({
                "& .cm-scroller": {
                    maxHeight: `${editorHeight}px !important`
                },
                "& .cm-activeLine": {
                    backgroundColor: `${existsSelection ? "transparent" : color.activeLine} !important`
                },
            }),
            language
        ]}
    />;
}
