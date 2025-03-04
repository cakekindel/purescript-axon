import { IncomingMessage, ServerResponse, Server } from 'node:http'
import * as HTTP from 'node:http'
import { Readable } from 'node:stream'

/**
type CreateServer =
  { keepAliveTimeout :: Nullable Number
  , fetch :: IncomingMessage -> ServerResponse -> Effect Unit
  }

foreign import createServer :: CreateServer -> Effect Server

  @type {(o: {keepAliveTimeout: number | null, fetch: (req: IncomingMessage) => (rep: ServerResponse) => () => void}) => () => Server}
*/
export const createServer = o => () =>
  HTTP.createServer(
    {
      keepAliveTimeout:
        o.keepAliveTimeout === null ? undefined : o.keepAliveTimeout,
    },
    (req, res) => o.fetch(req)(res)(),
  )

/**
 * foreign import serverClose :: Effect Unit -> Server -> Effect Unit
 * @type {(onClose: () => void) => (s: Server) => () => void}
 */
export const serverClose = f => s => () => s.close(f)

/**
 * foreign import serverListen :: String -> Int -> Server -> Effect Unit
 * @type {(onListening: () => void) => (addr: string ) => (port: number ) => (s: Server) => () => void}
 */
export const serverListen = f => hostname => port => s => () => {
  s.listen(port, hostname, f)
}

/**
 * requestBody :: IncomingMessage -> Readable ()
 * @type {(req: IncomingMessage) => Readable}
 */
export const requestBody = req => req

/**
 * requestHeaders :: IncomingMessage -> Effect (Object String)
 * @type {(req: IncomingMessage) => () => Record<string, string[]>}
 */
export const requestHeaders = req => () => {
  /** @type {Record<string, string[]>} */
  const o = {}

  for (const [k, v] of Object.entries(req.headers)) {
    if (!o[k]) {
      o[k] = []
    }

    if (v instanceof Array) {
      o[k].push(...v)
    } else if (v !== undefined) {
      o[k].push(v)
    }
  }

  return o
}

/**
 * requestMethod :: IncomingMessage -> Effect String
 * @type {(req: IncomingMessage) => () => string | undefined}
 */
export const requestMethod = req => () => req.method

/**
 * requestURL :: IncomingMessage -> Effect String
 * @type {(req: IncomingMessage) => () => string | undefined}
 */
export const requestURL = req => () => req.url

/**
 * foreign import requestAddr ::
 *   { ipv4 :: String -> Int -> SocketAddress
 *   , ipv6 :: String -> Int -> SocketAddress
 *   } ->
 *   IncomingMessage ->
 *   Effect SocketAddress
 * @type {(
 *   (addr: {ipv4: (a: string) => (b: number) => unknown, ipv6: (a: string) =>
 *           (b: number) => unknown}) =>
 *   (req: IncomingMessage) =>
 *   () =>
 *   unknown
 * )}
 */
export const requestAddr =
  ({ ipv4, ipv6 }) =>
  req =>
  () => {
    const addr = req.socket.address()
    if (!('family' in addr)) throw new Error('Request has no socket')

    if (addr.family.toLowerCase() === 'ipv4') {
      return ipv4(addr.address)(addr.port)
    } else {
      return ipv6(addr.address)(addr.port)
    }
  }

/**
 * responseEnd :: ServerResponse -> Effect Unit
 * @type {(req: ServerResponse) => () => void}
 */
export const responseEnd = rep => () => rep.end()

/**
 * responseWriteHead :: Int -> Object (Array String) -> ServerResponse -> Effect Unit
 * @type {(status: number) => (o: Record<string, string[]>) => (req: ServerResponse) => () => void}
 */
export const responseWriteHead = status => headers => rep => () =>
  rep.writeHead(status, headers)

/**
 * responseWriteString :: String -> ServerResponse -> Effect Unit
 * @type {(body: string) => (req: ServerResponse) => () => void}
 */
export const responseWriteString = body => rep => () => rep.write(body)

/**
 * responseWriteBuffer :: Buffer -> ServerResponse -> Effect Unit
 * @type {(body: Buffer) => (req: ServerResponse) => () => void}
 */
export const responseWriteBuffer = body => rep => () => rep.write(body)

/**
 * responseWriteStream :: Readable () -> ServerResponse -> Effect Unit
 * @type {(body: Readable) => (req: ServerResponse) => () => void}
 */
export const responseWriteStream = body => rep => () =>
  body.pipe(rep, { end: false })
