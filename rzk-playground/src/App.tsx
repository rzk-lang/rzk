import { useCallback, useEffect, useState } from 'react';
import Editor from './editor/Editor';
import { Resizable } from 're-resizable';
import * as rzk from './rzk';
import { KeyBindProvider, ShortcutType } from 'react-keybinds';

let typecheckedOnStart = false

declare let window: Window & typeof globalThis & { rzkTypecheck_: (input: {input: string}) => void };

function App() {
  const [windowHeight, setWindowHeight] = useState(window.innerHeight)

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
  const [output, setOutput] = useState("Starting...");

  const typecheck = useCallback(() => {
    const result = rzk.typecheck(text)
    if (result.status == 'ok') {
      setOutput(`Everything is OK!`)
    } else {
      setOutput(result.result)
    }
  }, [text])

  useEffect(() => {
    function checkFlag() {
      if ((!((window).rzkTypecheck_) || (text === "")) && !typecheckedOnStart) {
        console.warn("something bad")
        console.warn((window).rzkTypecheck_)
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

  const [needTypecheck, setNeedTypecheck] = useState(false)
  
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

  const [outputPanelHeight, setOutputHeight] = useState(windowHeight * 30 / 100)
  const [editorHeight, setEditorHeight] = useState(windowHeight * 70 / 100)

  return (
    <main>
      <div style={{ height: '100vh', width: '100vw', backgroundColor: '#202028' }}>
        <KeyBindProvider shortcuts={KEYBINDINGS}></KeyBindProvider>
        <div style={{ height: '100vh' }}>
          <Editor
            setText={setText}
            setNeedTypecheck={setNeedTypecheck}
            editorHeight={editorHeight}
          />
        </div>
        <div
          id={'output'}
          style={{
            position: 'fixed',
            bottom: 0,
            overflow: 'auto',
          }}
        >
          <Resizable
            size={{ width: '100vw', height: outputPanelHeight }}
            minHeight={100}
            maxHeight={windowHeight - 100}
            enable={{ top: true }}
            onResizeStop={(_e, _direction, _ref, d) => {
              setOutputHeight(outputPanelHeight + d.height)
            }}
            onResize={(_e, _direction, _ref, d) => {
              setEditorHeight(window.innerHeight - (outputPanelHeight + d.height))
            }}
            style={{ padding: '20px' }}
          >
            <button id='btnTypecheck' onClick={typecheck}>
              TYPECHECK (SHIFT + ENTER)
            </button>
            <pre id='message' style={{ fontSize: 16 }}>{output}</pre>
          </Resizable>
        </div>
      </div>
    </main>
  )
}

export default App