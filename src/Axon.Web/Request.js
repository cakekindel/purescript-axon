/// <reference lib="dom" />
/// <reference lib="dom.iterable" />

import Stream from 'stream'

/** @type {(r: Request) => () => ReadableStream<Uint8Array> | null} */
export const body = r => () => r.body

/** @type {(r: Request) => () => boolean} */
export const bodyUsed = r => () => r.bodyUsed

/** @type {(r: Request) => () => string} */
export const method = r => () => r.method

/** @type {(r: Request) => () => string} */
export const url = r => () => r.url

/** @type {(r: Request) => () => Headers} */
export const headers = r => () => r.headers

/** @type {(r: ReadableStream<Uint8Array>) => () => Stream.Readable} */
export const readableFromWeb = r => () => {
  const reader = r.getReader()
  return new Stream.Readable({
    read: function () {
      ;(async () => {
        /** @type {ReadableStreamReadResult<Uint8Array> | undefined} */
        let res = undefined
        try {
          res = await reader.read()
        } catch (e) {
          if (typeof e === 'undefined' || e instanceof Error) {
            this.destroy(e)
            return
          } else {
            throw e
          }
        }

        if (res.value) this.push(res.value)
        if (res.done) this.push(null)
      })()
    },
  })
}
