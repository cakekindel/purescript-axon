import Bun from 'bun'
import * as Net from 'node:net'

/*
type Serve =
  { port :: Int
  , hostname :: String
  , idleTimeout :: Number
  , fetch :: WebRequest -> Bun -> Effect (Promise WebResponse)
  }

foreign import serve :: Serve -> Effect Bun
foreign import stop :: Bun -> Promise Unit
foreign import ref :: Bun -> Effect Unit
foreign import unref :: Bun -> Effect Unit
foreign import requestAddr ::
  {left :: forall a b. a -> Either a b, right :: forall a b. b -> Either a b}
  -> WebRequest
  -> Bun
  -> Effect (Either (SocketAddress IPv4) (SocketAddress IPv6))
*/

/** @typedef {{port: number | null, hostname: string | null, idleTimeout: number | null, fetch: (req: Request) => (bun: Bun.Server) => () => Promise<Response>}} ServeOptions */

/**
 * @typedef {(addr: string) => (port: number) => unknown} FFISocketAddress
 */

/** @type {(s: ServeOptions) => () => Bun.Server} */
export const serve = opts => () =>
  Bun.serve({
    development: true,
    port: opts.port === null ? undefined : opts.port,
    hostname: opts.hostname === null ? undefined : opts.hostname,
    idleTimeout: opts.idleTimeout === null ? undefined : opts.idleTimeout,
    fetch: (req, server) => opts.fetch(req)(server)(),
  })

/** @type {(s: Bun.Server) => () => void} */
export const ref = s => () => s.ref()

/** @type {(s: Bun.Server) => () => void} */
export const unref = s => () => s.unref()

/** @type {(s: Bun.Server) => () => Promise<void>} */
export const stop = s => () => s.stop()

/** @type {(_: {ipv4: FFISocketAddress, ipv6: FFISocketAddress}) => (req: Request) => (s: Bun.Server) => () => unknown} */
export const requestAddr =
  ({ ipv4, ipv6 }) =>
  req =>
  s =>
  () => {
    const ip = s.requestIP(req)
    if (!ip) throw new Error('Request closed')
    return (ip.family === 'IPv4' ? ipv4 : ipv6)(ip.address)(ip.port)
  }
