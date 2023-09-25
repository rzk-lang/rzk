'use client';

import CodeMirror, { keymap } from '@uiw/react-codemirror';
import { example } from './example';
import { oneDark } from './theme';
import { Dispatch, SetStateAction } from 'react';

export default function Editor({
    setText,
    setNeedTypecheck,
    height,
}: {
    setText: Dispatch<SetStateAction<string>>,
    setNeedTypecheck: React.Dispatch<React.SetStateAction<boolean>>,
    height: number,
}) {
    return <CodeMirror
        value={example}
        height={`${height}px`}
        width={`100vw`}
        onCreateEditor={() => {
            setText(example)
        }}
        onChange={(value) => {
            setText(value)
        }}
        theme={oneDark}
        extensions={[keymap.of([
            {
                key: "Shift-Enter", run: () => {
                    setNeedTypecheck(true)
                    return true
                }
            }
        ])]}
    />;
}