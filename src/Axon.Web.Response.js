// foreign import response :: {body :: WebResponseBody, status :: Int} -> WebResponse

/** @typedef {string | null | ArrayBuffer | ReadableStream} Body */

/** @type {(_: {body: Body, status: number, headers: Record<string, string>}) => () => Response} */
export const make =
  ({ body, status, headers }) =>
  () =>
    new Response(body, { status, headers })

/** @type {Body} */
export const bodyEmpty = null
