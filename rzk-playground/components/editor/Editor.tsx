'use client';

import CodeMirror from '@uiw/react-codemirror';
import { EditorView, keymap } from '@codemirror/view'
import { scrollPastEnd } from '@codemirror/view';
import { example } from './example';
import { oneDark } from './theme';
import { Dispatch, SetStateAction } from 'react';
import { centerCursor } from './cursor-height';

export default function Editor({
    setText,
    setNeedTypecheck,
    outputHeight
}: {
    setText: Dispatch<SetStateAction<string>>,
    setNeedTypecheck: React.Dispatch<React.SetStateAction<boolean>>,
    outputHeight: number
}) {
    return <CodeMirror
        value={example}
        height={`100vh`}
        width={`100vw`}
        onCreateEditor={(view) => {
            setText(example)
            view.dispatch({ effects: EditorView.scrollIntoView(0) })
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
            centerCursor(outputHeight),
            // scrollPastEndCustom(outputHeight),
        ]}
    />;
}