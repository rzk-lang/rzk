'use client';

import { useCallback, useEffect, useState } from 'react';
import Editor from '../components/editor/Editor';
import Script from 'next/script';
import { Resizable } from 're-resizable';
import React from 'react';
import * as rzk from '../src/rzk';
import { KeyBindProvider, ShortcutType } from 'react-keybinds';
import dynamic from 'next/dynamic';

var typecheckedOnStart = false

declare var window: Window & typeof globalThis;

function HomeNoSSR() {
  const [message, setMessage] = useState("Starting...");

  const [windowHeight, setWindowHeight] = useState(window.innerHeight)
  const [needTypecheck, setNeedTypecheck] = useState(false)

  useEffect(() => {
    const handleWindowResize = () => {
      setWindowHeight(window.innerHeight)
    };

    window.addEventListener('resize', handleWindowResize);

    return () => {
      window.removeEventListener('resize', handleWindowResize);
    };
  }, []);

  const [text, setText] = useState("")

  const typecheck = useCallback(() => {
    const result = rzk.typecheck(text)
    if (result.status == 'ok') {
      setMessage(`Everything is OK!`)
    } else {
      setMessage(result.result)
    }
  }, [text])

  useEffect(() => {
    function checkFlag() {
      if ((!((window as any).rzkTypecheck_) || (text === "")) && !typecheckedOnStart) {
        console.warn("something bad")
        console.warn((window as any).rzkTypecheck_)
        window.setTimeout(checkFlag, 100); /* this checks the flag every 100 milliseconds*/
      } else if (!typecheckedOnStart) {
        typecheck()
        typecheckedOnStart = true
      }
    }

    if (!typecheckedOnStart) {
      checkFlag();
    }
  }, [typecheck, text])

  useEffect(() => {
    if (needTypecheck) {
      typecheck()
      setNeedTypecheck(false)
    }
  }, [setNeedTypecheck, needTypecheck, typecheck])

  const KEYBINDINGS: ShortcutType[] =
    [
      {
        keys: {
          Mac: ['Shift', 'Enter'],
          Windows: ['Shift', 'Enter'],
          Linux: ['Shift', 'Enter'],
        },
        label: 'Test command',
        callback: () => {
          setNeedTypecheck(true)
        },
      },
    ]
    ;

  const [outputHeight, setOutputHeight] = useState(windowHeight * 30 / 100)
  const [editorHeight, setEditorHeight] = useState(windowHeight * 70 / 100)

  return (
    <main>
      <div style={{ height: '100vh', width: '100vw', backgroundColor: '#202028' }}>
        <Script src='rzk.js' />
        <KeyBindProvider shortcuts={KEYBINDINGS}></KeyBindProvider>
        <div style={{ height: '100vh' }}>
          <Editor
            setText={setText}
            setNeedTypecheck={setNeedTypecheck}
            editorHeight={editorHeight}
          />
        </div>
        <div
          id={'message'}
          style={{
            position: 'fixed',
            bottom: 0,
            overflow: 'auto',
          }}
        >
          <Resizable
            size={{ width: '100vw', height: outputHeight }}
            minHeight={100}
            maxHeight={windowHeight - 100}
            enable={{ top: true }}
            onResizeStop={(_e, _direction, _ref, d) => {
              setOutputHeight(outputHeight + d.height)
            }}
            onResize={(_e, _direction, _ref, d) => {
              setEditorHeight(window.innerHeight - (outputHeight + d.height))
            }}
            style={{ padding: '20px' }}
          >
            <button id='btnTypecheck' onClick={typecheck}>
              TYPECHECK (SHIFT + ENTER)
            </button>
            <pre id='message' style={{ fontSize: 16 }}>{message}</pre>
          </Resizable>
        </div>
      </div>
    </main>
  )
}

const Home = dynamic(() => Promise.resolve(HomeNoSSR), {
  ssr: false,
})

export default Home