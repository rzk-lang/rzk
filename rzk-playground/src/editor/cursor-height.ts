import { ViewPlugin, ViewUpdate } from "@uiw/react-codemirror"


// https://discuss.codemirror.net/t/cm6-scroll-to-middle/2924/4
export const centerCursor = (editorHeight: number) => ViewPlugin.fromClass(class {
    update(update: ViewUpdate) {
        if (update.transactions.some(tr => tr.scrollIntoView)) {
            const view = update.view
            // (Sync with other DOM read/write phases for efficiency)
            view.requestMeasure({
                read() {
                    return {
                        cursor: view.coordsAtPos(view.state.selection.main.head),
                    }
                },
                write({ cursor }) {
                    if (cursor) {
                        const cursorHeight = cursor.bottom - cursor.top
                        const outputTop = editorHeight
                        if (cursor.bottom + cursorHeight + 5 > outputTop)
                            view.scrollDOM.scrollTop += ((cursor.bottom + 5) - outputTop)
                    }
                }
            })
        }
    }
})