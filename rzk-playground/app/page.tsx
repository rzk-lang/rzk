'use client';

import { useCallback, useEffect, useState } from 'react';
import Editor from '../components/editor/Editor';
import Script from 'next/script';
import { Resizable } from 're-resizable';
import React from 'react';
import { color } from '../components/editor/theme';
import * as rzk from '../src/rzk';
import { KeyBindProvider, ShortcutType } from 'react-keybinds';
import dynamic from 'next/dynamic';

const style = {
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  background: color.background
}

var typecheckedOnStart = false

declare var window: Window & typeof globalThis;

function HomeNoSSR() {
  const [message, setMessage] = useState("Starting...");

  const [windowHeight, setWindowHeight] = useState(window.innerHeight)
  const [editorHeight, setHeight] = React.useState(windowHeight * 70 / 100)
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

  return (
    <main>
      <div style={{ height: '100vh', width: '100vw', backgroundColor: '#202028' }}>
        <Script src='rzk.js' />
        <KeyBindProvider shortcuts={KEYBINDINGS}></KeyBindProvider>
        <Resizable
          style={style}
          size={{ width: '100vw', height: editorHeight }}
          maxHeight={windowHeight - 100}
          minHeight={100}
          enable={{ bottom: true }}
          onResize={(_e, _direction, _ref, d) => {
            setHeight(_ref.offsetHeight)
          }}
        >
          <div style={{ height: editorHeight }}>
            <Editor setText={setText} setNeedTypecheck={setNeedTypecheck} height={editorHeight} />
          </div>
        </Resizable>
        <div className='column' style={{
          height: `${windowHeight - editorHeight}px`,
          position: 'fixed',
          bottom: 0,
          overflow: 'auto'
        }}>
          <div id='__app__'>
            <div>
              <button id='btnTypecheck' onClick={typecheck}>
                TYPECHECK (SHIFT + ENTER)
              </button>
              <pre id='message' style={{ fontSize: 16 }}>{message}</pre>
            </div>
          </div>
        </div>
      </div>
    </main>
  )
}

const Home = dynamic(() => Promise.resolve(HomeNoSSR), {
  ssr: false,
})

export default Home