import { useCallback, useEffect, useState } from 'react';
import { Editor } from './editor/Editor';
import { Resizable } from 're-resizable';
import * as rzk from './rzk';
import { KeyBindProvider, ShortcutType } from 'react-keybinds';

declare let window: Window & typeof globalThis & {
  rzkTypecheck_: (input: { input: string }) => void
};

function Root({ typecheckedOnStartState }:
  {
    typecheckedOnStartState: [boolean, React.Dispatch<React.SetStateAction<boolean>>]
  }) {
  const [typecheckedOnStart, setTypecheckedOnStart] = typecheckedOnStartState
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

  const [rzkTypeCheckAvailable, setRzkTypeCheckAvailable] = useState(false)

  useEffect(() => {
    function checkRzkTypecheckAvailable() {
      if (!window.rzkTypecheck_) {
        window.setTimeout(checkRzkTypecheckAvailable, 100)
      } else {
        setRzkTypeCheckAvailable(true)
      }
    }

    checkRzkTypecheckAvailable()
  }, [text])

  // Typecheck when function and text are ready
  useEffect(() => {
    function checkFlag() {
      if (!typecheckedOnStart && (!rzkTypeCheckAvailable || text === '')) {
        console.warn("Can't typecheck!")
      } else if (!typecheckedOnStart) {
        typecheck()
        setTypecheckedOnStart(true)
      }
    }

    if (!typecheckedOnStart) {
      checkFlag();
    }
  }, [typecheck, rzkTypeCheckAvailable, text, setTypecheckedOnStart, typecheckedOnStart])

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


  // height of the output panel
  const [outputPanelHeight, setOutputHeight] = useState(windowHeight * 30 / 100)

  // visible height of the editor
  const [editorHeight, setEditorHeight] = useState(windowHeight * 70 / 100)

  const
    minHeight = 100,
    maxHeight = windowHeight - 100,
    width = '100vw',
    height = '100vh',
    backgroundColor = '#202028'

  return (
    <main>
      <div style={{ height, width, backgroundColor }}>
        <KeyBindProvider shortcuts={KEYBINDINGS}></KeyBindProvider>
        <div style={{ height }}>
          <Editor
            textState={[text, setText]}
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
            size={{ width, height: outputPanelHeight }}
            minHeight={minHeight}
            maxHeight={maxHeight}
            enable={{ top: true }}
            onResizeStop={(_event, _direction, _elementRef, delta) => {
              setOutputHeight(outputPanelHeight + delta.height)
            }}
            onResize={(_event, _direction, _elementRef, delta) => {
              setEditorHeight(window.innerHeight - (outputPanelHeight + delta.height))
            }}
            style={{ padding: '20px' }}
          >
            <button id='btnTypecheck' onClick={typecheck}>
              TYPECHECK (SHIFT + ENTER)
            </button>
            <pre style={{ fontSize: 16 }}>{output}</pre>
          </Resizable>
        </div>
      </div>
    </main>
  )
}


function App() {
  return <Root typecheckedOnStartState={useState(false)} />
}

export default App